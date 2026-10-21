#!/usr/bin/env python3
# Created on 2026-10-02. Developed by the CFBM development team.
# run python -B tests/regression/regression.py case --help
"""Prepare and run one scientific case with an existing executable.

Each invocation creates a fresh directory and retains inputs, namelists,
outputs, process logs, and one result. Building belongs to CMake and CI/PBS.
"""
from __future__ import annotations

import datetime as dt
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import time
from typing import Any

import yaml

from check_outputs import expected_output_names, validate_outputs
from config import load_yaml, resolve_spec
from generate_inputs import generate_inputs, sha256_file
from render_namelist import render_namelist
from reports import print_result, write_result

SCRIPT_DIR = Path(__file__).resolve().parent
SOURCE_ROOT = SCRIPT_DIR.parents[1]
FATAL_PREFIXES = ("STOP", "ERROR:", "Error:", "MPI_ABORT")

#--------------------------------------------------------------------------------
# Case preparation and execution
#--------------------------------------------------------------------------------


def source_identity(source_root: Path = SOURCE_ROOT) -> dict[str, Any]:
    """Record the revision and local edits; ordinary testing allows both."""

    def git(*args: str) -> str:
        """Read identity from the source worktree without changing it."""
        return subprocess.check_output(
            ["git", "-C", str(source_root), *args], text=True).strip()

    return {
        "revision": git("rev-parse", "HEAD"),
        "changes": git("status", "--porcelain", "--untracked-files=all")
    }


def write_esmx_config(path: Path, spec: dict[str, Any]) -> None:
    """Use the same forcing clock and ranks for both coupled components."""
    clock = spec
    component = {"petList": list(range(spec["execution"]["ranks"]))}
    config = {
        "ESMX": {
            "App": {
                "fieldDictionary": "./fd_fire.yaml",
                "logKindFlag": "ESMF_LOGKIND_Multi",
                "startTime": clock["start"].replace("_", "T"),
                "stopTime": clock["end"].replace("_", "T")
            },
            "Driver": {
                "componentList": ["FIRE", "WRF"],
                "runSequence":
                    (f"@{spec['namelist']['atm']['interval_atm']}\n"
                     "  WRF -> FIRE\n  FIRE -> WRF\n  FIRE\n  WRF\n@")
            }
        },
        "FIRE": {
            "model": "fire_behavior",
            **component
        },
        "WRF": {
            "model": "wrfdata",
            **component
        }
    }
    path.write_text(yaml.safe_dump(config, sort_keys=False), encoding="utf-8")


def run_model(command: list[str], directory: Path, environment: dict[str, str],
              timeout_seconds: int, driver: str) -> dict[str, Any]:
    """Launch one integration and retain process and fatal-log evidence."""
    result = {
        "reasons": [],
        "command": command,
        "started_utc": dt.datetime.now(dt.timezone.utc).isoformat()
    }
    with (directory / "model.log").open("w") as log:
        process = subprocess.run(command,
                                 cwd=directory,
                                 env=environment,
                                 stdout=log,
                                 stderr=subprocess.STDOUT,
                                 timeout=timeout_seconds,
                                 check=False)
    result["exit_status"] = process.returncode
    if process.returncode:
        result["reasons"].append(
            f"Model exited with status {process.returncode}")
    logs = [
        directory / "model.log", *directory.glob("PET*.ESMF_LogFile"),
        *directory.glob("rsl.error.*")
    ]
    for log in logs:
        lines = log.read_text(errors="replace").splitlines()
        result["reasons"].extend(
            line.strip()
            for line in lines
            if line.strip().startswith(FATAL_PREFIXES) or " ERROR " in line)
    if driver != "standalone":
        marker = {
            "nuopc": "esmApp FINISHED",
            "esmx": "ESMX (Earth System Model eXecutable) FINISHED",
            "wrf": "SUCCESS COMPLETE WRF",
            "wrf_ideal": "SUCCESS COMPLETE IDEAL INIT"
        }[driver]
        completed = any(
            marker in log.read_text(errors="replace") for log in logs)
        result["coupled_finalize_observed"] = completed
        if not completed:
            result["reasons"].append("Coupled finalization was not observed")
    return result


def run_case(args: Any) -> dict[str, Any]:
    """Stage a case, launch the model, and reject incomplete integrations."""
    if args.driver == "wrf":
        if not args.wrf_ideal or not args.wrf_source:
            raise ValueError("WRF requires --wrf-ideal and --wrf-source")
        from wrf_coupled import run_wrf_case
        return run_wrf_case(args)
    if Path("/glade").exists() and not os.environ.get("PBS_JOBID"):
        raise RuntimeError(
            "On NCAR HPC, run model cases inside a PBS allocation")
    document = load_yaml(args.config)
    spec = resolve_spec(document, args.case, args.scale, args.configuration,
                        args.execution)
    if args.driver not in document["cases"][args.case]["drivers"]:
        raise ValueError(f"No coupled configuration for {args.case}")
    if spec.get("restart_splits") and args.driver != "standalone":
        raise ValueError("Restart experiments currently require standalone")
    executable = args.executable.resolve(strict=True)
    root = Path(os.environ.get("CFBM_RUN_ROOT", str(args.run_root))).resolve()
    if root.is_relative_to(SOURCE_ROOT) and not root.is_relative_to(
            SOURCE_ROOT / "build"):
        raise ValueError(
            "Keep generated runs in build/ or outside the checkout")
    root.mkdir(parents=True, exist_ok=True)
    name = (f"{args.driver}_{args.case}_{spec['identity']['configuration']}_"
            f"{args.scale}_{args.execution}")
    directory = Path(tempfile.mkdtemp(prefix=name + "-", dir=root))
    result = {
        "name": name,
        "directory": str(directory),
        "driver": args.driver,
        "pass": False,
        "reasons": [],
        "source": source_identity(),
        "executable": str(executable),
        "executable_sha256": sha256_file(executable),
        "environment": {
            "python": sys.executable,
            "PBS_JOBID": os.environ.get("PBS_JOBID"),
            "LOADEDMODULES": os.environ.get("LOADEDMODULES")
        }
    }
    started = time.monotonic()
    try:
        spec, _ = generate_inputs(spec, directory)
        result["spec"] = spec
        result["expected_outputs"] = expected_output_names(spec)
        result["expected_fields"] = document["expected_output_fields"]
        (directory / "namelist.fire").write_text(render_namelist(spec))
        command = [str(executable)]
        if args.driver != "standalone":
            for filename in ("esmfRun.config", "fd_fire.yaml"):
                shutil.copy2(SCRIPT_DIR / "templates" / filename,
                             directory / filename)
        if args.driver == "esmx":
            write_esmx_config(directory / "esmxRun.yaml", spec)
            command.append("esmxRun.yaml")
        execution = spec["execution"]
        if execution["build"] in ("mpi", "hybrid"):
            command = [
                args.launcher, *args.launcher_arg, args.process_flag,
                str(execution["ranks"]), command[0], *args.launcher_post_arg,
                *command[1:]
            ]
        environment = dict(os.environ,
                           OMP_NUM_THREADS=str(execution["threads"]),
                           OMP_DYNAMIC="FALSE")
        result.update(command=command,
                      started_utc=dt.datetime.now(dt.timezone.utc).isoformat(),
                      threads=execution["threads"])
        result.update(
            run_model(command, directory, environment,
                      execution["timeout_seconds"], args.driver))
        result["validation"] = validate_outputs(directory, result)
        result["reasons"].extend(result["validation"]["reasons"])
        if spec.get("restart_splits"):
            from restart import run_restarts
            result["restart"] = run_restarts(spec, directory, command,
                                             environment,
                                             set(document["static_fields"]))
            result["reference_policy"] = "same-revision continuous/restarted"
            if not result["restart"]["pass"]:
                result["reasons"].append("Restart continuity checks failed")
        if spec["identity"]["configuration"] == "restart_prescribed":
            from scheduling import check_legacy_ideal
            result["legacy_clock"] = check_legacy_ideal(spec, directory,
                                                        command, environment)
            if not result["legacy_clock"]["pass"]:
                result["reasons"].append(
                    "Legacy idealized clock compatibility checks failed")
        if spec["identity"]["configuration"] == "schedule_03_restart":
            from scheduling import check_schedule_limits
            result["scheduling_limits"] = check_schedule_limits(
                spec, directory, command, environment)
            if not result["scheduling_limits"]["pass"]:
                result["reasons"].append(
                    "Scheduling rejection or final-time checks failed")
        if args.reference and not spec.get("restart_splits"):
            from reference import compare_reference
            result["reference"] = compare_reference(args.reference, result,
                                                    document)
            if not result["reference"]["pass"]:
                result["reasons"].append("Reference comparison failed")
    except (OSError, ValueError, KeyError, RuntimeError,
            subprocess.SubprocessError) as error:
        result["reasons"].append(f"{type(error).__name__}: {error}")
    result["elapsed_seconds"] = time.monotonic() - started
    result["pass"] = not result["reasons"]
    path = directory / "result.json"
    write_result(path, result)
    print_result(result, path)
    return result
