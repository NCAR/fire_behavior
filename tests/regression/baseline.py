#!/usr/bin/env python3
# Copyright 2026      Research Applications Laboratory (RAL),
#                     National Center for Atmospheric Research (NCAR),
#                     University Corporation for Atmospheric Research (UCAR)
#
#--------------------------------------------------------------------------------
# Created by Maria Frediani (frediani@ucar.edu) on 2026-09-12
#--------------------------------------------------------------------------------
# run /glade/work/frediani/casper/anaconda3/envs/py314/bin/python tests/regression/regression.py baseline-create --help
#
"""Create, validate, select, and accept immutable CFBM baseline sets."""

from __future__ import annotations

#--------------------------------------------------------------------------------
# Python modules
#--------------------------------------------------------------------------------

import json
import shutil
import subprocess
from pathlib import Path
from typing import Any

import yaml

from generate_inputs import sha256_file


def git_identity(repository: Path) -> dict[str, Any]:
    """Return exact commit and cleanliness required for candidate production."""
    commit = subprocess.run(
        ["git", "-C", str(repository), "rev-parse", "HEAD"], check=True,
        capture_output=True, text=True,
    ).stdout.strip()
    status = subprocess.run(
        ["git", "-C", str(repository), "status", "--porcelain", "--untracked-files=all"],
        check=True, capture_output=True, text=True,
    ).stdout
    return {"repository": str(repository.resolve()), "commit": commit, "clean": not bool(status), "status": status.splitlines()}


def verify_baseline(root: Path) -> dict[str, Any]:
    """Validate an immutable baseline manifest and every recorded checksum."""
    manifest_path = root / "manifest.yaml"
    if not manifest_path.is_file():
        raise FileNotFoundError(f"Missing baseline manifest: {manifest_path}")
    manifest = yaml.safe_load(manifest_path.read_text(encoding="utf-8"))
    required = {
        "schema_version", "identifier", "approved", "validation_status",
        "model_source", "harness_source", "mappings", "checksums", "run_evidence",
        "candidate_validation_summary_sha256", "approval",
    }
    if not isinstance(manifest, dict) or set(manifest) != required:
        raise ValueError(f"Baseline manifest keys differ from the approved schema: {root}")
    if manifest["schema_version"] != 1 or manifest["identifier"] != root.name:
        raise ValueError(f"Baseline schema or identifier is invalid: {root}")
    if not manifest.get("approved", False):
        raise ValueError(f"Baseline is not approved: {root}")
    for relative, expected in manifest["checksums"].items():
        path = root / relative
        if not path.resolve().is_relative_to(root.resolve()):
            raise ValueError(f"Baseline checksum path escapes its root: {relative}")
        if not path.is_file() or sha256_file(path) != expected:
            raise ValueError(f"Baseline checksum mismatch: {path}")
    for key, relative in manifest["mappings"].items():
        if len(key.split("|")) != 5:
            raise ValueError(f"Baseline mapping key is invalid: {key}")
        directory = root / relative
        if not directory.resolve().is_relative_to(root.resolve()) or not directory.is_dir():
            raise ValueError(f"Baseline mapping directory is invalid: {directory}")
        files = sorted(directory.glob("fire_output_*.nc"))
        if not files or any(str(path.relative_to(root)) not in manifest["checksums"] for path in files):
            raise ValueError(f"Baseline mapping is empty or has unrecorded payloads: {directory}")
    if set(manifest["run_evidence"]) != set(manifest["mappings"]):
        raise ValueError(f"Baseline run evidence differs from its mappings: {root}")
    return manifest


def resolve_baseline(
    cli_root: Path | None, environment_root: str | None, configured_id: str | None,
    configured_parent: Path,
) -> Path:
    """Resolve baseline selection by CLI, environment, then approved configuration."""
    if cli_root is not None:
        root = cli_root
    elif environment_root:
        root = Path(environment_root)
    elif configured_id:
        root = configured_parent / configured_id
    else:
        approved = configured_parent / "approved.yaml"
        if not approved.is_file():
            raise FileNotFoundError("No approved baseline is configured")
        selection = yaml.safe_load(approved.read_text(encoding="utf-8"))
        if not isinstance(selection, dict) or set(selection) != {"approved_id", "manifest_sha256"}:
            raise ValueError(f"Approved baseline selection is invalid: {approved}")
        root = configured_parent / selection["approved_id"]
    verify_baseline(root)
    if cli_root is None and not environment_root and not configured_id:
        if sha256_file(root / "manifest.yaml") != selection["manifest_sha256"]:
            raise ValueError(f"Approved baseline manifest identity changed: {root}")
    return root.resolve()


def create_candidate(
    candidate_root: Path, identifier: str, work_root: Path, model_repository: Path,
    harness_repository: Path,
) -> Path:
    """Publish completed validated runs into a previously nonexistent candidate set."""
    summary_path = work_root / "summary.json"
    if not summary_path.is_file():
        raise FileNotFoundError(f"Missing candidate validation summary: {summary_path}")
    summary = json.loads(summary_path.read_text(encoding="utf-8"))
    if not summary.get("candidate_validation_pass", False):
        raise ValueError("A failed or incomplete candidate validation cannot create a baseline set")
    model = git_identity(model_repository)
    harness = git_identity(harness_repository)
    if not model["clean"] or not harness["clean"]:
        raise ValueError("Model source and harness repositories must be clean committed states")
    if model["commit"][:7] not in identifier:
        raise ValueError("Candidate identifier must contain the model-source commit abbreviation")
    if summary.get("model_source", {}).get("commit") != model["commit"]:
        raise ValueError("Candidate validation model commit differs from the requested model repository")
    if summary.get("harness_source", {}).get("commit") != harness["commit"]:
        raise ValueError("Candidate validation harness commit differs from the requested harness repository")
    destination = candidate_root / identifier
    if destination.exists():
        raise FileExistsError(f"Candidate identifier already exists: {destination}")
    completed_manifests = []
    for manifest_path in sorted(work_root.glob("runs/*/*/*/*/*/*/run_manifest.json")):
        run_manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
        if run_manifest.get("status") == "completed":
            local_entries = [run_manifest[name] for name in ("namelist", "resolved")]
            local_entries.extend(run_manifest["inputs"])
            external_entries = [run_manifest[name] for name in ("configuration", "template", "generator", "executable")]
            for entry in local_entries:
                source = manifest_path.parent / Path(entry["path"]).name
                if not source.is_file() or sha256_file(source) != entry["sha256"]:
                    raise ValueError(f"Completed run evidence changed: {source}")
            for entry in external_entries:
                source = Path(entry["path"])
                if not source.is_file() or sha256_file(source) != entry["sha256"]:
                    raise ValueError(f"Completed run provenance changed: {source}")
            for output in run_manifest.get("outputs", []):
                source = manifest_path.parent / output["name"]
                if not source.is_file() or sha256_file(source) != output["sha256"]:
                    raise ValueError(f"Completed result identity changed: {source}")
            completed_manifests.append((manifest_path, run_manifest))
    if not completed_manifests:
        raise ValueError(f"No completed runs found beneath {work_root}")
    destination.mkdir(parents=True, exist_ok=False)
    mappings: dict[str, str] = {}
    checksums: dict[str, str] = {}
    run_evidence: dict[str, Any] = {}
    summary_target = destination / "evidence" / "summary.json"
    summary_target.parent.mkdir(parents=True, exist_ok=False)
    shutil.copy2(summary_path, summary_target)
    checksums[str(summary_target.relative_to(destination))] = sha256_file(summary_target)
    for manifest_path, run_manifest in completed_manifests:
        identity = run_manifest["spec"]["identity"]
        key = "|".join(identity[name] for name in ("case", "suite", "method", "feature", "execution"))
        if key in mappings:
            raise ValueError(f"Candidate contains duplicate result identity: {key}")
        relative_dir = Path("references") / identity["execution"] / identity["case"] / identity["suite"] / identity["method"] / identity["feature"]
        target_dir = destination / relative_dir
        target_dir.mkdir(parents=True, exist_ok=False)
        evidence_dir = Path("evidence") / "runs" / identity["execution"] / identity["case"] / identity["suite"] / identity["method"] / identity["feature"]
        evidence_target = destination / evidence_dir / "run_manifest.json"
        evidence_target.parent.mkdir(parents=True, exist_ok=False)
        shutil.copy2(manifest_path, evidence_target)
        checksums[str(evidence_target.relative_to(destination))] = sha256_file(evidence_target)
        for output in run_manifest["outputs"]:
            source = manifest_path.parent / output["name"]
            target = target_dir / output["name"]
            shutil.copy2(source, target)
            relative = str(target.relative_to(destination))
            checksums[relative] = sha256_file(target)
        mappings[key] = str(relative_dir)
        run_evidence[key] = {
            "configuration": run_manifest["configuration"],
            "template": run_manifest["template"],
            "generator": run_manifest["generator"],
            "namelist": run_manifest["namelist"],
            "resolved": run_manifest["resolved"],
            "inputs": run_manifest["inputs"],
            "executable": run_manifest["executable"],
            "outputs": run_manifest["outputs"],
            "run_manifest_sha256": sha256_file(manifest_path),
            "candidate_run_manifest": str(evidence_target.relative_to(destination)),
        }
    manifest = {
        "schema_version": 1, "identifier": identifier, "approved": False,
        "validation_status": "candidate validation passed", "model_source": model,
        "harness_source": harness, "mappings": mappings, "checksums": checksums,
        "run_evidence": run_evidence,
        "candidate_validation_summary_sha256": sha256_file(summary_path),
    }
    (destination / "manifest.yaml").write_text(yaml.safe_dump(manifest, sort_keys=False), encoding="utf-8")
    return destination


def accept_candidate(candidate: Path, approver: str, decision: str, approved_file: Path) -> None:
    """Record an explicit decision and select a validated candidate without changing payloads."""
    manifest_path = candidate / "manifest.yaml"
    manifest = yaml.safe_load(manifest_path.read_text(encoding="utf-8"))
    if not approver.strip() or not decision.strip():
        raise ValueError("Baseline approval requires nonempty approver and decision text")
    expected_keys = {
        "schema_version", "identifier", "approved", "validation_status",
        "model_source", "harness_source", "mappings", "checksums", "run_evidence",
        "candidate_validation_summary_sha256",
    }
    if not isinstance(manifest, dict) or set(manifest) != expected_keys:
        raise ValueError(f"Candidate manifest keys differ from the expected schema: {candidate}")
    if manifest["schema_version"] != 1 or manifest["identifier"] != candidate.name:
        raise ValueError(f"Candidate schema or identifier is invalid: {candidate}")
    for relative, expected in manifest["checksums"].items():
        path = candidate / relative
        if not path.resolve().is_relative_to(candidate.resolve()):
            raise ValueError(f"Candidate checksum path escapes its root: {relative}")
        if not path.is_file() or sha256_file(path) != expected:
            raise ValueError(f"Candidate checksum mismatch: {path}")
    if set(manifest["run_evidence"]) != set(manifest["mappings"]):
        raise ValueError("Candidate run evidence differs from its mappings")
    if manifest.get("validation_status") != "candidate validation passed":
        raise ValueError("Only a passing candidate can be accepted")
    if manifest.get("approved"):
        raise FileExistsError(f"Candidate is already approved: {candidate}")
    if approved_file.exists():
        raise FileExistsError(f"Approval selection already exists: {approved_file}")
    manifest["approved"] = True
    manifest["approval"] = {"approver": approver, "decision": decision}
    manifest_path.write_text(yaml.safe_dump(manifest, sort_keys=False), encoding="utf-8")
    approved_file.parent.mkdir(parents=True, exist_ok=True)
    approved_file.write_text(yaml.safe_dump({"approved_id": candidate.name, "manifest_sha256": sha256_file(manifest_path)}, sort_keys=False), encoding="utf-8")
