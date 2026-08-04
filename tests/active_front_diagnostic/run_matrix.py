#!/usr/bin/env python3
# Copyright 2026      Research Applications Laboratory (RAL),
#                     National Center for Atmospheric Research (NCAR),
#                     University Corporation for Atmospheric Research (UCAR)
#
#--------------------------------------------------------------------------------
# Created by Maria Frediani (frediani@ucar.edu) on 2026-08-03
#--------------------------------------------------------------------------------
# run model cases through a PBS command documented in README.md
#
"""Stage and run the manual active-front numerical-invariance matrix."""

from __future__ import annotations

#--------------------------------------------------------------------------------
# Python modules
#--------------------------------------------------------------------------------

import argparse
from dataclasses import dataclass, replace
import hashlib
import os
from pathlib import Path
import shlex
import shutil
import subprocess
from typing import Iterable


#--------------------------------------------------------------------------------
# Paths and test layout
#--------------------------------------------------------------------------------

REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
TEST_ROOT = Path(__file__).resolve().parent
DEFAULT_INPUT_ROOT = REPOSITORY_ROOT / "tests" / "test7"
FINAL_TEMPLATE = TEST_ROOT / "namelist.fire.in"
PRE_REFACTOR_TEMPLATE = TEST_ROOT / "namelist.pre_refactor.in"
REQUIRED_INPUTS = ("geo_em.d01.nc", "wrf.nc")
SCRATCH_ROOT = Path("/glade/derecho/scratch/frediani")
PRE_REFACTOR_REVISION = "ca9c8109b2f4e80e40d96fc8492c1b0a2475d50a"


#--------------------------------------------------------------------------------
# Parameters
#--------------------------------------------------------------------------------


@dataclass(frozen=True)
class CaseConfig:
    """Define one deterministic fire-model configuration."""

    fire_upwinding: int
    fire_upwinding_reinit: int
    use_active_front: bool = False
    reinit_use_rs: bool = False
    godunov_sign_branch: bool = False
    fire_lsm_reinit: bool = True
    active_front_band_ngp: int = 4
    devel_opt: int = 1
    output_level: int = 1
    fire_print_msg: int = 2


MATRIX_CONFIGS = {
    "p2_r4": CaseConfig(2, 4),
    "p4_r4": CaseConfig(4, 4),
    "p4_r5_rs_off": CaseConfig(4, 5),
    "p4_r5_rs_on": CaseConfig(4, 5, reinit_use_rs=True, godunov_sign_branch=True),
    "p8_r4": CaseConfig(8, 4),
    "p9_r4": CaseConfig(9, 4),
    "p10_r4": CaseConfig(10, 4),
}

SPECIAL_CONFIGS = {
    "band_width1": ("active_false", replace(MATRIX_CONFIGS["p4_r4"], active_front_band_ngp=1)),
    "callcount_true_noreinit": (
        "active_true",
        replace(MATRIX_CONFIGS["p4_r4"], use_active_front=True, fire_lsm_reinit=False),
    ),
    "gate_devel0": (
        "active_false",
        replace(MATRIX_CONFIGS["p4_r4"], devel_opt=0, output_level=1),
    ),
    "gate_output0": (
        "active_false",
        replace(MATRIX_CONFIGS["p4_r4"], output_level=0),
    ),
}

PRE_REFACTOR_CASES = {
    name: config.fire_upwinding
    for name, config in MATRIX_CONFIGS.items()
    if config.fire_upwinding in (8, 9, 10)
}


#--------------------------------------------------------------------------------
# Command-line interface
#--------------------------------------------------------------------------------


def parse_args() -> argparse.Namespace:
    """Parse runner paths, execution mode, and optional case selection."""

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--mode", choices=("serial", "mpi", "pre-refactor"), required=True)
    parser.add_argument("--executable", type=Path, required=True)
    parser.add_argument("--run-root", type=Path, required=True)
    parser.add_argument("--input-root", type=Path, default=DEFAULT_INPUT_ROOT)
    parser.add_argument(
        "--case",
        action="append",
        dest="cases",
        help="Run only this matrix or special case; repeat to select multiple cases.",
    )
    parser.add_argument("--mpi-ranks", type=int, default=4)
    return parser.parse_args()


#--------------------------------------------------------------------------------
# Input construction
#--------------------------------------------------------------------------------


def fortran_logical(value: bool) -> str:
    """Format a Python logical for a Fortran namelist."""

    return ".true." if value else ".false."


def render_final_namelist(config: CaseConfig) -> str:
    """Render one final-implementation namelist from the maintained template."""

    replacements = {
        "@FIRE_PRINT_MSG@": str(config.fire_print_msg),
        "@FIRE_UPWINDING@": str(config.fire_upwinding),
        "@FIRE_LSM_REINIT@": fortran_logical(config.fire_lsm_reinit),
        "@FIRE_UPWINDING_REINIT@": str(config.fire_upwinding_reinit),
        "@REINIT_USE_RS@": fortran_logical(config.reinit_use_rs),
        "@GODUNOV_SIGN_BRANCH@": fortran_logical(config.godunov_sign_branch),
        "@ACTIVE_FRONT_BAND_NGP@": str(config.active_front_band_ngp),
        "@DEVEL_OPT@": str(config.devel_opt),
        "@OUTPUT_LEVEL@": str(config.output_level),
        "@USE_ACTIVE_FRONT@": fortran_logical(config.use_active_front),
    }
    text = FINAL_TEMPLATE.read_text(encoding="utf-8")
    for key, value in replacements.items():
        text = text.replace(key, value)
    if "@" in text:
        raise ValueError(f"Unresolved template marker in {FINAL_TEMPLATE}")
    return text


def render_pre_refactor_namelist(fire_upwinding: int) -> str:
    """Render the option-8/9/10 input accepted by the pre-refactor executable."""

    text = PRE_REFACTOR_TEMPLATE.read_text(encoding="utf-8")
    text = text.replace("@FIRE_UPWINDING@", str(fire_upwinding))
    if "@" in text:
        raise ValueError(f"Unresolved template marker in {PRE_REFACTOR_TEMPLATE}")
    return text


def selected(names: Iterable[str], requested: list[str] | None) -> list[str]:
    """Return requested cases while rejecting misspelled identifiers."""

    available = list(names)
    if requested is None:
        return available
    unknown = sorted(set(requested) - set(available))
    if unknown:
        raise ValueError(f"Unknown case(s): {', '.join(unknown)}; available: {', '.join(available)}")
    return [name for name in available if name in requested]


def final_case_entries(requested: list[str] | None) -> list[tuple[str, str, CaseConfig]]:
    """Build paired matrix cases plus serial-only diagnostic checks."""

    available = list(MATRIX_CONFIGS) + list(SPECIAL_CONFIGS)
    chosen = selected(available, requested)
    entries: list[tuple[str, str, CaseConfig]] = []
    for name in chosen:
        if name in MATRIX_CONFIGS:
            base = MATRIX_CONFIGS[name]
            entries.append((name, "active_false", replace(base, use_active_front=False)))
            entries.append((name, "active_true", replace(base, use_active_front=True)))
        else:
            variant, config = SPECIAL_CONFIGS[name]
            entries.append((name, variant, config))
    return entries


#--------------------------------------------------------------------------------
# Case execution
#--------------------------------------------------------------------------------


def file_digest(path: Path) -> str:
    """Calculate a stable file identifier for run provenance."""

    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def repository_revision() -> str:
    """Require and record the clean revision providing the maintained runner."""

    status = subprocess.run(
        ["git", "status", "--porcelain=v1"],
        cwd=REPOSITORY_ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    if status.stdout:
        raise RuntimeError(f"Regression runner requires a clean worktree: {REPOSITORY_ROOT}")

    result = subprocess.run(
        ["git", "rev-parse", "HEAD"],
        cwd=REPOSITORY_ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout.strip()


def stage_and_run(
    case_dir: Path,
    namelist_text: str,
    input_root: Path,
    command: list[str],
    executable_sha256: str,
    source_revision: str,
    runner_revision: str,
    execution_mode: str,
    mpi_ranks: int,
) -> None:
    """Stage one immutable case directory and execute the configured model command."""

    if case_dir.exists():
        raise FileExistsError(f"Case directory already exists; choose a new run root: {case_dir}")
    case_dir.mkdir(parents=True)

    input_hashes: dict[str, str] = {}
    for name in REQUIRED_INPUTS:
        source = input_root / name
        if not source.is_file():
            raise FileNotFoundError(f"Required input is missing: {source}")
        input_hashes[name] = file_digest(source)
        shutil.copy2(source, case_dir / name)

    (case_dir / "namelist.fire").write_text(namelist_text, encoding="utf-8")
    command_text = shlex.join(command)
    namelist_sha256 = hashlib.sha256(namelist_text.encode("utf-8")).hexdigest()
    provenance_lines = (
        f"command={command_text}",
        f"source_revision={source_revision}",
        f"runner_revision={runner_revision}",
        f"executable_sha256={executable_sha256}",
        f"namelist_sha256={namelist_sha256}",
        *(f"{name}_sha256={input_hashes[name]}" for name in REQUIRED_INPUTS),
        f"pbs_job_id={os.environ['PBS_JOBID']}",
        f"execution_mode={execution_mode}",
        f"mpi_ranks={mpi_ranks}",
    )
    provenance = "\n".join(provenance_lines) + "\n"
    (case_dir / "command.txt").write_text(provenance, encoding="utf-8")

    print(f"Running {case_dir}")
    with (case_dir / "run.log").open("w", encoding="utf-8") as log:
        log.write(provenance)
        log.flush()
        subprocess.run(command, cwd=case_dir, stdout=log, stderr=subprocess.STDOUT, check=True)


def main() -> None:
    """Run the requested matrix without overwriting an earlier validation result."""

    args = parse_args()
    executable = args.executable.expanduser().resolve()
    if not executable.is_file() or not os.access(executable, os.X_OK):
        raise FileNotFoundError(f"Executable is missing or is not executable: {executable}")
    if args.mpi_ranks < 1:
        raise ValueError("--mpi-ranks must be positive")
    if "PBS_JOBID" not in os.environ:
        raise RuntimeError("Model cases must run inside a PBS allocation; use a qcmd command from README.md")

    run_root = args.run_root.expanduser().resolve()
    if not run_root.is_relative_to(SCRATCH_ROOT):
        raise ValueError(f"--run-root must be under {SCRATCH_ROOT}: {run_root}")

    executable_sha256 = file_digest(executable)
    runner_revision = repository_revision()
    source_revision = PRE_REFACTOR_REVISION if args.mode == "pre-refactor" else runner_revision
    mode_root = run_root / args.mode.replace("-", "_")

    if args.mode == "pre-refactor":
        for name in selected(PRE_REFACTOR_CASES, args.cases):
            case_dir = mode_root / name / "baseline_false"
            namelist = render_pre_refactor_namelist(PRE_REFACTOR_CASES[name])
            stage_and_run(
                case_dir,
                namelist,
                args.input_root,
                [str(executable)],
                executable_sha256,
                source_revision,
                runner_revision,
                args.mode,
                1,
            )
    else:
        command = [str(executable)]
        if args.mode == "mpi":
            command = ["mpiexec", "-n", str(args.mpi_ranks), str(executable)]
        for name, variant, config in final_case_entries(args.cases):
            case_dir = mode_root / name / variant
            stage_and_run(
                case_dir,
                render_final_namelist(config),
                args.input_root,
                command,
                executable_sha256,
                source_revision,
                runner_revision,
                args.mode,
                args.mpi_ranks if args.mode == "mpi" else 1,
            )

    print(f"Completed cases under {mode_root}")


if __name__ == "__main__":
    main()
