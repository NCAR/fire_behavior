#!/usr/bin/env python3
# Created on 2026-10-02. Developed by the CFBM development team.
# run python -B tests/regression/regression.py reference-create --help
"""Create and verify explicit references without selecting or approving one."""
from __future__ import annotations

import json
from pathlib import Path
import shutil
from typing import Any

from compare_outputs import compare_directories
from generate_inputs import sha256_file
from reports import write_result

#--------------------------------------------------------------------------------
# Reference integrity and creation
#--------------------------------------------------------------------------------


def scientific_settings(spec: dict[str, Any]) -> dict[str, Any]:
    """Exclude execution bookkeeping when checking experiment equivalence."""
    return {
        name: value
        for name, value in spec.items()
        if name not in ("execution", "identity")
    }


def verify_reference(root: Path) -> dict[str, Any]:
    """Require every recorded reference payload to retain its SHA256 checksum."""
    manifest = json.loads((root / "reference.json").read_text())
    for relative, checksum in manifest["checksums"].items():
        path = (root / relative).resolve()
        if not path.is_relative_to(root.resolve()):
            raise ValueError(
                f"Reference path escapes its directory: {relative}")
        if sha256_file(path) != checksum:
            raise ValueError(f"Reference checksum differs: {relative}")
    return manifest


def compare_reference(root: Path, result: dict[str, Any],
                      document: dict[str, Any]) -> dict[str, Any]:
    """Compare against the explicitly supplied matching reference experiment."""
    manifest = verify_reference(root)
    if manifest["approval"] != "approved":
        raise ValueError("Reference requires recorded team approval before use")
    reference = manifest["cases"][result["name"]]
    settings = json.loads((root / reference["settings"]).read_text())
    if settings != scientific_settings(result["spec"]):
        raise ValueError("Reference scientific settings differ from this case")
    comparison = compare_directories(root / reference["directory"],
                                     Path(result["directory"]),
                                     result["expected_outputs"],
                                     set(document["static_fields"]))
    comparison["approval"] = manifest["approval"]
    return comparison


def create_reference(suite_root: Path, destination: Path) -> None:
    """Copy a passing clean-source suite into a new, unapproved reference.

    This operation neither changes configuration nor activates the reference.
    Team approval remains a separate recorded scientific review decision.
    """
    from run_case import source_identity
    identity = source_identity()
    if identity["changes"]:
        raise ValueError("Reference creation requires a clean source worktree")
    summary = json.loads((suite_root / "summary.json").read_text())
    if not summary["pass"] or not summary["cases"]:
        raise ValueError("Reference creation requires a passing nonempty suite")
    results = [
        json.loads((Path(case["directory"]) / "result.json").read_text())
        for case in summary["cases"]
    ]
    if any(not item["pass"] or item["source"] != identity for item in results):
        raise ValueError(
            "All reference runs must use the current clean revision")
    destination.mkdir(parents=True, exist_ok=False)
    manifest = {
        "source": identity,
        "approval": "unapproved",
        "cases": {},
        "checksums": {}
    }
    for result in results:
        name = result["name"]
        directory = destination / name
        directory.mkdir()
        write_result(directory / "settings.json",
                     scientific_settings(result["spec"]))
        for filename in result["expected_outputs"]:
            shutil.copy2(
                Path(result["directory"]) / filename, directory / filename)
        for path in directory.iterdir():
            manifest["checksums"][str(
                path.relative_to(destination))] = sha256_file(path)
        manifest["cases"][name] = {
            "directory": name,
            "settings": f"{name}/settings.json"
        }
    write_result(destination / "reference.json", manifest)
    verify_reference(destination)
    print(f"Unapproved reference created: {destination}")
