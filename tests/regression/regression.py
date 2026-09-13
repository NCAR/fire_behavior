#!/usr/bin/env python3
# Copyright 2026      Research Applications Laboratory (RAL),
#                     National Center for Atmospheric Research (NCAR),
#                     University Corporation for Atmospheric Research (UCAR)
#
#--------------------------------------------------------------------------------
# Created by Maria Frediani (frediani@ucar.edu) on 2026-09-12
#--------------------------------------------------------------------------------
# run /glade/work/frediani/casper/anaconda3/envs/py314/bin/python tests/regression/regression.py all --suite quick --variants serial,omp,mpi --work-root /glade/derecho/scratch/frediani/cfbm-regression/quick-v1
#
"""Prepare, execute, compare, and manage standalone CFBM regression cases."""

from __future__ import annotations

#--------------------------------------------------------------------------------
# Python modules
#--------------------------------------------------------------------------------

import argparse
import datetime as dt
import json
import os
import platform
import subprocess
import sys
import time
import xml.etree.ElementTree as ET
from pathlib import Path
from typing import Any

import netCDF4
import numpy as np
import yaml

SCRIPT_DIR = Path(__file__).resolve().parent
SOURCE_ROOT = SCRIPT_DIR.parents[1]
sys.path.insert(0, str(SCRIPT_DIR))

from baseline import accept_candidate, create_candidate, git_identity, resolve_baseline, verify_baseline
from compare_outputs import compare_directories, write_reports
from config import enumerate_matrix, load_platform, load_yaml, resolve_spec
from generate_inputs import generate_inputs, sha256_file
from render_namelist import namelist_values, render_template


#--------------------------------------------------------------------------------
# Paths and model-output contract
#--------------------------------------------------------------------------------

DEFAULT_CONFIG = SCRIPT_DIR / "cases.yaml"
DEFAULT_TEMPLATE = SCRIPT_DIR / "templates" / "namelist.fire.in"
DEFAULT_BASELINES = SCRIPT_DIR / "baselines"
FATAL_PREFIXES = ("STOP:", "ERROR: mpi_", "ERROR: ideal_opt option not supported")
OUTPUT_METADATA = {
    "lats": ("degrees_north", "fire-grid cell-center latitude"),
    "lons": ("degrees_east", "fire-grid cell-center longitude"),
    "fgrnhfx": ("W m-2", "ground fire sensible heat flux"),
    "fgrnqfx": ("W m-2", "ground fire latent heat flux"),
    "fire_area": ("1", "fire-area fraction within cell"),
    "fuel_frac_burnt_dt": ("1", "fuel fraction burned during current fire timestep"),
    "fuel_frac": ("1", "remaining fuel fraction"),
    "emis_smoke": ("kg m-2", "fire particulate emissions per cell area during current timestep"),
    "fire_t2": ("K", "air temperature at 2 m"),
    "fire_q2": ("kg kg-1", "water-vapor mixing ratio at 2 m (legacy variable name)"),
    "fire_psfc": ("Pa", "surface air pressure"),
    "fire_rain": (None, "standalone accumulated precipitation; coupled-driver units unresolved"),
    "fz0": ("m", "surface roughness length"),
    "fmc_g": ("kg kg-1", "ground fuel moisture content"),
    "uf": ("m s-1", "eastward wind used by fire spread"),
    "vf": ("m s-1", "northward wind used by fire spread"),
    "zsf": ("m", "fire-grid terrain height"),
    "lfn": ("m", "signed level-set distance to fire perimeter"),
    "nfuel_cat": ("1", "fuel category identifier"),
    "grad_norm_ls": ("1", "level-set gradient norm used during propagation"),
    "grad_norm_reinit": ("1", "level-set gradient norm used during reinitialization"),
}
ATMOSPHERIC_FIELDS = {"fire_t2", "fire_q2", "fire_psfc", "fire_rain", "fz0"}
FORCING_CHECK_RTOL = 8.0 * np.finfo(np.float32).eps


def _json_write(path: Path, value: dict[str, Any]) -> None:
    """Write deterministic strict JSON through an atomic same-directory replacement."""
    temporary = path.with_suffix(path.suffix + ".new")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + "\n", encoding="utf-8")
    temporary.replace(path)


def _new_directory(path: Path) -> None:
    """Create an exclusive directory and refuse existing invocation targets."""
    path.mkdir(parents=True, exist_ok=False)


def _attempt_directory(parent: Path) -> Path:
    """Allocate a concurrency-safe monotonically named CTest attempt directory."""
    parent.mkdir(parents=True, exist_ok=True)
    for index in range(1, 1_000_000):
        candidate = parent / f"attempt-{index:06d}"
        try:
            candidate.mkdir()
            return candidate
        except FileExistsError:
            continue
    raise RuntimeError(f"Unable to allocate an attempt directory beneath {parent}")


def expected_output_names(spec: dict[str, Any]) -> list[str]:
    """Enumerate initialization and scheduled output names from configured time semantics."""
    time_cfg = spec["time"]
    start = dt.datetime.strptime(time_cfg["start"], "%Y-%m-%d_%H:%M:%S")
    seconds = [0] + list(range(int(time_cfg["output_interval_seconds"]), int(time_cfg["duration_seconds"]) + 1, int(time_cfg["output_interval_seconds"])))
    return [f"fire_output_{(start + dt.timedelta(seconds=value)).strftime('%Y-%m-%d_%H:%M:%S')}.nc" for value in seconds]


def _environment_record() -> dict[str, Any]:
    """Record the Python numerical environment used for generation and comparison."""
    return {
        "host": platform.node(), "python": sys.version.split()[0], "executable": sys.executable,
        "numpy": np.__version__, "netCDF4": netCDF4.__version__, "PyYAML": yaml.__version__,
    }


def _write_stage_report(run_dir: Path, stage: str, passed: bool, reasons: list[str]) -> None:
    """Write strict JSON, text, and JUnit evidence for a non-comparison stage."""
    result = {"stage": stage, "pass": passed, "reasons": reasons}
    _json_write(run_dir / f"{stage}_report.json", result)
    (run_dir / f"{stage}_report.txt").write_text(
        f"stage={stage}\npass={str(passed).lower()}\n" + "".join(f"reason={reason}\n" for reason in reasons),
        encoding="utf-8",
    )
    suite = ET.Element("testsuite", name=f"CFBM {stage}", tests="1", failures="0" if passed else "1")
    case = ET.SubElement(suite, "testcase", classname="cfbm.regression", name=stage)
    if not passed:
        failure = ET.SubElement(case, "failure", message=f"CFBM {stage} failed")
        failure.text = "\n".join(reasons)
    ET.ElementTree(suite).write(run_dir / f"{stage}_report.xml", encoding="utf-8", xml_declaration=True)


def prepare_case(
    config_path: Path, template_path: Path, case: str, suite: str, method: str | None,
    feature: str | None, execution: str, run_dir: Path,
) -> dict[str, Any]:
    """Resolve, validate, and stage one new case directory with deterministic inputs."""
    document = load_yaml(config_path)
    spec = resolve_spec(document, case, suite, method, feature, execution)
    _new_directory(run_dir)
    spec, inputs = generate_inputs(spec, run_dir)
    namelist = render_template(template_path, namelist_values(spec))
    namelist_path = run_dir / "namelist.fire"
    namelist_path.write_text(namelist, encoding="utf-8")
    resolved_path = run_dir / "resolved.yaml"
    resolved_path.write_text(yaml.safe_dump(spec, sort_keys=False), encoding="utf-8")
    manifest = {
        "schema_version": 1, "stage": "prepare", "status": "prepared", "spec": spec,
        "configuration": {"path": str(config_path.resolve()), "sha256": sha256_file(config_path)},
        "template": {"path": str(template_path.resolve()), "sha256": sha256_file(template_path)},
        "generator": {"path": str((SCRIPT_DIR / "generate_inputs.py").resolve()), "sha256": sha256_file(SCRIPT_DIR / "generate_inputs.py")},
        "namelist": {"path": "namelist.fire", "sha256": sha256_file(namelist_path)},
        "resolved": {"path": "resolved.yaml", "sha256": sha256_file(resolved_path)},
        "inputs": inputs, "expected_outputs": expected_output_names(spec), "environment": _environment_record(),
    }
    _json_write(run_dir / "run_manifest.json", manifest)
    _write_stage_report(run_dir, "prepare", True, [])
    return manifest


def prepare_attempt(
    parent: Path, config_path: Path, template_path: Path, case: str, suite: str,
    method: str | None, feature: str | None, execution: str,
) -> tuple[Path, dict[str, Any]]:
    """Allocate and prepare a unique case attempt without deleting a reservation."""
    parent.mkdir(parents=True, exist_ok=True)
    for index in range(1, 1_000_000):
        run_dir = parent / f"attempt-{index:06d}"
        try:
            manifest = prepare_case(config_path, template_path, case, suite, method, feature, execution, run_dir)
            return run_dir, manifest
        except FileExistsError:
            continue
    raise RuntimeError(f"Unable to allocate a case attempt beneath {parent}")


def _verify_prepared(run_dir: Path, manifest: dict[str, Any], executable: Path) -> None:
    """Refuse stale inputs, namelists, executables, completed runs, and output collisions."""
    if manifest.get("status") != "prepared":
        raise ValueError(f"Run directory is not in prepared state: {run_dir}")
    for entry in (manifest["namelist"], manifest["resolved"], *manifest["inputs"]):
        path = run_dir / Path(entry["path"]).name
        if not path.is_file() or sha256_file(path) != entry["sha256"]:
            raise ValueError(f"Prepared file identity changed: {path}")
    for entry in (manifest["configuration"], manifest["template"], manifest["generator"]):
        path = Path(entry["path"])
        if not path.is_file() or sha256_file(path) != entry["sha256"]:
            raise ValueError(f"Prepared provenance identity changed: {path}")
    if not executable.is_absolute() or not executable.is_file() or not os.access(executable, os.X_OK):
        raise ValueError(f"Executable must be an absolute executable file: {executable}")
    stale = list(run_dir.glob("fire_output_*.nc"))
    if stale:
        raise FileExistsError(f"Prepared directory contains stale model outputs: {stale}")


def _launcher_argv(
    spec: dict[str, Any], executable: Path, launcher: list[str], process_flag: str,
    postflags: list[str] | None = None,
) -> list[str]:
    """Construct an argument-list launch command without shell evaluation."""
    ranks = spec["execution"]["ranks"]
    if spec["execution"]["variant"] == "mpi":
        if not launcher:
            raise ValueError("MPI execution requires explicit launcher arguments")
        return [*launcher, process_flag, str(ranks), str(executable), *(postflags or [])]
    return [str(executable)]


def _validate_outputs(run_dir: Path, manifest: dict[str, Any]) -> dict[str, Any]:
    """Require the output schema and evidence that each configured scientific path ran."""
    expected = set(manifest["expected_outputs"])
    actual = {path.name for path in run_dir.glob("fire_output_*.nc")}
    validation: dict[str, Any] = {"pass": True, "reasons": [], "files": []}
    if actual != expected:
        validation["reasons"].append(f"output inventory differs: expected={sorted(expected)}, actual={sorted(actual)}")
    expected_fields = set(load_yaml(Path(manifest["configuration"]["path"]))["expected_output_fields"])
    for name in sorted(expected & actual):
        path = run_dir / name
        with netCDF4.Dataset(path) as dataset:
            fields = set(dataset.variables)
            if fields != expected_fields:
                validation["reasons"].append(f"{name} fields differ: expected={sorted(expected_fields)}, actual={sorted(fields)}")
            nonfinite = []
            for variable_name, variable in dataset.variables.items():
                if variable.dtype.kind != "f":
                    continue
                values = np.ma.asarray(variable[:])
                valid = values.compressed()
                if valid.size and not np.isfinite(valid).all():
                    nonfinite.append(variable_name)
                expected_units, expected_long_name = OUTPUT_METADATA[variable_name]
                attributes = set(variable.ncattrs())
                required_attributes = {"_FillValue", "long_name"}
                if expected_units is not None:
                    required_attributes.add("units")
                if not required_attributes <= attributes:
                    validation["reasons"].append(
                        f"{name} {variable_name} lacks metadata {sorted(required_attributes - attributes)}"
                    )
                if expected_units is None and "units" in attributes:
                    validation["reasons"].append(f"{name} {variable_name} has unresolved units but declares {variable.units!r}")
                if expected_units is not None and getattr(variable, "units", None) != expected_units:
                    validation["reasons"].append(f"{name} {variable_name} units differ from the output contract")
                if getattr(variable, "long_name", None) != expected_long_name:
                    validation["reasons"].append(f"{name} {variable_name} long_name differs from the output contract")
            if nonfinite:
                validation["reasons"].append(f"{name} contains nonfinite fields: {sorted(nonfinite)}")
            validation["files"].append({"name": name, "sha256": sha256_file(path), "size": path.stat().st_size, "fields": sorted(fields)})
    spec = manifest["spec"]
    if expected <= actual and len(expected) >= 2:
        first = run_dir / sorted(expected)[0]
        last = run_dir / sorted(expected)[-1]
        with netCDF4.Dataset(first) as first_ds, netCDF4.Dataset(last) as last_ds:
            lfn_changed = not np.array_equal(first_ds.variables["lfn"][:], last_ds.variables["lfn"][:])
            fuel_consumed = bool(np.any(last_ds.variables["fuel_frac"][:] < first_ds.variables["fuel_frac"][:]))
            active_flux = bool(
                np.any(last_ds.variables["fgrnhfx"][:] > 0.0)
                or np.any(last_ds.variables["fgrnqfx"][:] > 0.0)
            )
            validation.update({
                "level_set_changed": lfn_changed,
                "fuel_consumption_observed": fuel_consumed,
                "positive_fire_flux_observed": active_flux,
            })
            if not lfn_changed:
                validation["reasons"].append("lfn did not change between initialization and final output")
            if not fuel_consumed:
                validation["reasons"].append("fuel_frac did not decrease between initialization and final output")
            if not active_flux:
                validation["reasons"].append("no positive sensible or latent fire flux was produced")
            if spec["moisture"]["run"]:
                moisture_changed = bool(np.any(first_ds.variables["fmc_g"][:] != last_ds.variables["fmc_g"][:]))
                validation["moisture_update_observed"] = moisture_changed
                if not moisture_changed:
                    validation["reasons"].append("fmc_g did not change between initialization and final output")

            if spec["model"]["ideal_opt"] == 1:
                unmasked = [name for name in ATMOSPHERIC_FIELDS if np.ma.count(last_ds.variables[name][:])]
                if unmasked:
                    validation["reasons"].append(f"ideal output contains applicable values in {sorted(unmasked)}")
            else:
                forcing = spec["forcing"]
                expected_forcing = {
                    "fire_t2": forcing["temperature_end_k"],
                    "fire_q2": forcing["mixing_ratio_end_kg_kg"],
                    "fire_psfc": forcing["surface_pressure_pa"],
                    "fire_rain": forcing["accumulated_rain_end_mm"],
                }
                for field, target in expected_forcing.items():
                    values = np.ma.asarray(last_ds.variables[field][:]).compressed()
                    if not values.size or not np.allclose(values, target, rtol=FORCING_CHECK_RTOL, atol=0.0):
                        validation["reasons"].append(
                            f"final {field} does not contain the forcing record valid at the output timestamp"
                        )
                z0 = np.ma.asarray(last_ds.variables["fz0"][:]).compressed()
                if not z0.size or float(np.ptp(z0)) <= 0.0:
                    validation["reasons"].append("fz0 does not retain the spatially varying WRF ZNT field")

            if spec["feature"]["real_perimeter"]:
                geo_path = run_dir / "geo_em.d01.nc"
                with netCDF4.Dataset(geo_path) as geo:
                    supplied = np.asarray(geo.variables["lfn_init"][:])
                initial = np.asarray(first_ds.variables["lfn"][:])
                matches = initial.shape == supplied.shape and np.array_equal(initial, supplied)
                if not matches and initial.shape == supplied.T.shape:
                    matches = np.array_equal(initial, supplied.T)
                activation_time = float(spec["ignition"]["start_time_s"])
                validation["observed_perimeter_activation_time_s"] = activation_time
                if activation_time <= 0.0:
                    validation["observed_perimeter_installed_at_initial_time"] = matches
                    if not matches:
                        validation["reasons"].append("zero-time perimeter is absent from the initial lfn")
                else:
                    inactive = not matches and bool(np.all(initial > 0.0))
                    validation["observed_perimeter_inactive_at_initial_time"] = inactive
                    if not inactive:
                        validation["reasons"].append("delayed perimeter is active before its scheduled time")
    validation["pass"] = not validation["reasons"]
    return validation


def run_case(
    run_dir: Path, executable: Path, launcher: list[str], process_flag: str,
    postflags: list[str] | None = None,
) -> dict[str, Any]:
    """Execute one prepared case once and preserve complete process evidence."""
    manifest_path = run_dir / "run_manifest.json"
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    _verify_prepared(run_dir, manifest, executable)
    if not os.environ.get("PBS_JOBID") and not os.environ.get("GITHUB_ACTIONS") and os.environ.get("CFBM_ALLOW_LOGIN_MODEL") != "1":
        raise RuntimeError("Model integrations may run only on a PBS compute node or GitHub Actions runner")
    argv = _launcher_argv(manifest["spec"], executable, launcher, process_flag, postflags)
    executable_sha256 = sha256_file(executable)
    environment = os.environ.copy()
    environment.update(manifest["spec"]["execution"]["environment"])
    started_utc = dt.datetime.now(dt.timezone.utc).isoformat()
    started = time.monotonic()
    timed_out = False
    try:
        process = subprocess.run(
            argv, cwd=run_dir, env=environment, capture_output=True, text=True,
            timeout=manifest["spec"]["execution"]["timeout_seconds"], check=False,
        )
        exit_status = process.returncode
        stdout, stderr = process.stdout, process.stderr
    except subprocess.TimeoutExpired as error:
        timed_out = True
        exit_status = None
        stdout = error.stdout or ""
        stderr = error.stderr or ""
    (run_dir / "stdout.log").write_text(stdout, encoding="utf-8")
    (run_dir / "stderr.log").write_text(stderr, encoding="utf-8")
    fatal_lines = [line for line in (stdout + "\n" + stderr).splitlines() if line.strip().startswith(FATAL_PREFIXES)]
    validation = _validate_outputs(run_dir, manifest)
    passed = exit_status == 0 and not timed_out and not fatal_lines and validation["pass"]
    if sha256_file(executable) != executable_sha256:
        validation["reasons"].append("executable changed during model execution")
        validation["pass"] = False
        passed = False
    manifest.update({
        "stage": "run", "status": "completed" if passed else "failed", "argv": argv,
        "executable": {"path": str(executable), "sha256": executable_sha256},
        "exit_status": exit_status, "timed_out": timed_out, "fatal_lines": fatal_lines,
        "started_utc": started_utc,
        "finished_utc": dt.datetime.now(dt.timezone.utc).isoformat(),
        "elapsed_seconds": time.monotonic() - started, "output_validation": validation,
        "outputs": validation["files"],
    })
    _json_write(manifest_path, manifest)
    run_reasons = []
    if exit_status != 0:
        run_reasons.append(f"model exit status was {exit_status}")
    if timed_out:
        run_reasons.append("model execution timed out")
    run_reasons.extend(fatal_lines)
    run_reasons.extend(validation["reasons"])
    _write_stage_report(run_dir, "run", passed, run_reasons)
    return manifest


def compare_case(
    result_dir: Path, baseline_root: Path, report_parent: Path,
    diagnostic_reference: Path | None = None,
) -> dict[str, Any]:
    """Compare one completed result with an approved mapping or diagnostic result."""
    report_dir = _attempt_directory(report_parent)
    try:
        manifest = json.loads((result_dir / "run_manifest.json").read_text(encoding="utf-8"))
        if diagnostic_reference is not None:
            reference_dir = diagnostic_reference
            baseline_identity = "diagnostic"
        else:
            baseline_manifest = verify_baseline(baseline_root)
            identity = manifest["spec"]["identity"]
            key = "|".join(identity[name] for name in ("case", "suite", "method", "feature", "execution"))
            if key not in baseline_manifest["mappings"]:
                raise KeyError(f"Baseline has no explicit mapping for {key}")
            reference_dir = baseline_root / baseline_manifest["mappings"][key]
            baseline_identity = baseline_manifest["identifier"]
        document = load_yaml(Path(manifest["configuration"]["path"]))
        result = compare_directories(
            reference_dir, result_dir, manifest["expected_outputs"], set(document["static_fields"]),
            set(document["metadata_policy"]["volatile_global_attributes"]),
        )
        result.update({"baseline_identity": baseline_identity, "execution": manifest["spec"]["identity"]["execution"], "comparison_direction": "test-minus-reference"})
    except Exception as error:
        result = {
            "stage": "compare", "pass": False, "reasons": [str(error)],
            "test": str(result_dir), "baseline_identity": None,
            "comparison_direction": "test-minus-reference",
        }
    result["reports"] = write_reports(result, report_dir)
    return result


def _blocked_comparison(report_parent: Path, reason: str) -> dict[str, Any]:
    """Write all report formats for a comparison blocked by an earlier stage."""
    result = {
        "stage": "compare", "pass": False, "reasons": [reason],
        "baseline_identity": None, "comparison_direction": "test-minus-reference",
    }
    report_dir = _attempt_directory(report_parent)
    result["reports"] = write_reports(result, report_dir)
    return result


def _repository_inventory(repository: Path) -> dict[str, Any]:
    """Record tracked, untracked, and ignored file identities without changing the tree."""
    groups: dict[str, list[str]] = {}
    commands = {
        "tracked": ["git", "-C", str(repository), "ls-files", "--cached"],
        "untracked": ["git", "-C", str(repository), "ls-files", "--others", "--exclude-standard"],
        "ignored": ["git", "-C", str(repository), "ls-files", "--others", "--ignored", "--exclude-standard"],
    }
    for name, argv in commands.items():
        output = subprocess.run(argv, check=True, capture_output=True, text=True).stdout
        groups[name] = sorted(line for line in output.splitlines() if line)
    files = sorted({relative for values in groups.values() for relative in values})
    hashes = {
        relative: sha256_file(repository / relative)
        for relative in files
        if (repository / relative).is_file()
    }
    return {**groups, "sha256": hashes}


def _directory_inventory(root: Path) -> dict[str, str]:
    """Record every regular file below a baseline root in stable relative-path order."""
    if not root.exists():
        return {}
    return {
        str(path.relative_to(root)): sha256_file(path)
        for path in sorted(root.rglob("*"))
        if path.is_file()
    }


def _cmake_cache_values(cache_path: Path) -> dict[str, str]:
    """Read the standalone build switches required for executable provenance."""
    required = {"CMAKE_BUILD_TYPE", "DM_PARALLEL", "OPENMP", "NUOPC", "ESMX"}
    values: dict[str, str] = {}
    for line in cache_path.read_text(encoding="utf-8").splitlines():
        if ":" not in line or "=" not in line:
            continue
        name = line.split(":", 1)[0]
        if name in required:
            values[name] = line.split("=", 1)[1]
    missing = required - values.keys()
    if missing:
        raise ValueError(f"CMake cache lacks required settings: {sorted(missing)}")
    return values


def _build_variant(
    source_root: Path, work_root: Path, variant: str, platform_cfg: dict[str, Any],
) -> dict[str, Any]:
    """Build and install one isolated serial, OpenMP, or MPI executable."""
    build_dir = work_root / "build" / variant
    install_dir = work_root / "install" / variant
    argv = [str(source_root / "compile.sh"), f"--build-dir={build_dir}", f"--prefix={install_dir}", "--build-type=release"]
    env_file = platform_cfg.get("build_environment")
    if env_file:
        argv.append(f"--env-file={source_root / env_file}")
    if variant == "serial":
        argv.append("--mpi-off")
    elif variant == "omp":
        argv.extend(["--mpi-off", "--openmp-on"])
    elif variant != "mpi":
        raise ValueError(f"Unknown build variant {variant}")
    build_environment = os.environ.copy()
    build_environment["PATH"] = f"{Path(sys.executable).parent}:{build_environment.get('PATH', '')}"
    subprocess.run(argv, cwd=source_root, env=build_environment, check=True)
    executable = (install_dir / "bin" / "fire_behavior.exe").resolve()
    if not executable.is_file():
        raise FileNotFoundError(f"Installed executable is missing: {executable}")
    cache = _cmake_cache_values(build_dir / "CMakeCache.txt")
    expected = {
        "serial": {"DM_PARALLEL": "OFF", "OPENMP": "OFF"},
        "omp": {"DM_PARALLEL": "OFF", "OPENMP": "ON"},
        "mpi": {"DM_PARALLEL": "ON", "OPENMP": "OFF"},
    }[variant]
    mismatches = {
        name: {"expected": value, "actual": cache[name]}
        for name, value in {**expected, "NUOPC": "OFF", "ESMX": "OFF"}.items()
        if cache[name].upper() != value
    }
    if mismatches:
        raise ValueError(f"Unexpected CMake settings for {variant}: {mismatches}")
    return {
        "variant": variant, "argv": argv, "build_directory": str(build_dir),
        "install_directory": str(install_dir), "cmake_cache": cache,
        "executable": str(executable), "executable_sha256": sha256_file(executable),
    }


def run_all(
    config_path: Path, suite: str, variants: set[str], work_root: Path,
    platform_path: Path, baseline_root: Path | None, source_root: Path = SOURCE_ROOT,
    selected_cases: set[str] | None = None,
) -> dict[str, Any]:
    """Build requested variants, run the required matrix, and aggregate comparisons."""
    document = load_yaml(config_path)
    platform_cfg = load_platform(platform_path)
    unknown_variants = variants - {"serial", "omp", "mpi"}
    if unknown_variants:
        raise ValueError(f"Unknown variants: {sorted(unknown_variants)}")
    if selected_cases is not None:
        unknown_cases = selected_cases - document["cases"].keys()
        if unknown_cases:
            raise ValueError(f"Unknown selected cases: {sorted(unknown_cases)}")
    if work_root.resolve().is_relative_to(source_root.resolve()):
        raise ValueError("The outer work root must be outside the model source repository")
    complete_rows = enumerate_matrix(document, suite)
    rows = [
        row for row in complete_rows
        if document["executions"][row["execution"]]["variant"] in variants
        and (selected_cases is None or row["case"] in selected_cases)
    ]
    source_identity = git_identity(source_root)
    harness_identity = git_identity(SOURCE_ROOT)
    if not source_identity["clean"] or not harness_identity["clean"]:
        raise ValueError("Candidate validation requires clean committed model and harness repositories")
    source_before = _repository_inventory(source_root)
    harness_before = _repository_inventory(SOURCE_ROOT)
    baseline_before = _directory_inventory(baseline_root) if baseline_root is not None else None
    _new_directory(work_root)
    builds = {variant: _build_variant(source_root, work_root, variant, platform_cfg) for variant in sorted(variants)}
    executables = {variant: Path(record["executable"]) for variant, record in builds.items()}
    results: list[dict[str, Any]] = []
    for row in rows:
        execution_cfg = document["executions"][row["execution"]]
        run_parent = work_root / "runs" / row["execution"] / row["case"] / suite / row["method"] / row["feature"]
        run_dir = run_parent
        try:
            run_dir, _ = prepare_attempt(
                run_parent, config_path, DEFAULT_TEMPLATE, row["case"], suite,
                row["method"], row["feature"], row["execution"],
            )
            launcher = [*platform_cfg["mpi_launcher"], *platform_cfg["mpi_preflags"]]
            manifest = run_case(
                run_dir, executables[execution_cfg["variant"]], launcher,
                platform_cfg["mpi_process_flag"][0], platform_cfg["mpi_postflags"],
            )
            result = {"identity": row, "run_dir": str(run_dir), "run_pass": manifest["status"] == "completed"}
            if result["run_pass"] and baseline_root is not None:
                comparison = compare_case(run_dir, baseline_root, work_root / "reports" / "baseline" / row["execution"] / row["case"])
                result["baseline_pass"] = comparison["pass"]
            elif baseline_root is None:
                result["baseline_pass"] = False
                result["baseline_blocker"] = "No approved baseline is configured"
            results.append(result)
        except Exception as error:
            results.append({"identity": row, "run_dir": str(run_dir), "run_pass": False, "error": str(error)})
    by_key = {(item["identity"]["case"], item["identity"]["method"], item["identity"]["feature"], item["identity"]["execution"]): item for item in results}
    cross: list[dict[str, Any]] = []
    for item in results:
        identity = item["identity"]
        if identity["execution"] == "serial" or not item.get("run_pass"):
            continue
        serial = by_key.get((identity["case"], identity["method"], identity["feature"], "serial"))
        if serial and serial.get("run_pass"):
            comparison = compare_case(Path(item["run_dir"]), Path(item["run_dir"]), work_root / "reports" / "cross" / identity["execution"] / identity["case"], Path(serial["run_dir"]))
            cross.append({
                "pair": f"serial->{identity['execution']}",
                "case": identity["case"],
                "pass": comparison["pass"],
                "comparison": comparison,
            })
    if suite in {"pr", "full"}:
        direct_pairs = [("omp1", "omp4"), ("mpi1", "mpi4")]
        if suite == "full":
            direct_pairs.append(("mpi1", "mpi8"))
        scientific_keys = {(row["case"], row["method"], row["feature"]) for row in rows}
        for case_name, method_name, feature_name in sorted(scientific_keys):
            for reference_execution, test_execution in direct_pairs:
                reference = by_key.get((case_name, method_name, feature_name, reference_execution))
                test = by_key.get((case_name, method_name, feature_name, test_execution))
                if reference and test and reference.get("run_pass") and test.get("run_pass"):
                    comparison = compare_case(
                        Path(test["run_dir"]), Path(test["run_dir"]),
                        work_root / "reports" / "cross" / f"{reference_execution}-to-{test_execution}" / case_name,
                        Path(reference["run_dir"]),
                    )
                    cross.append({
                        "pair": f"{reference_execution}->{test_execution}",
                        "case": case_name,
                        "pass": comparison["pass"],
                        "comparison": comparison,
                    })
    complete_variants = variants == {"serial", "omp", "mpi"} and len(rows) == len(complete_rows)
    source_after = _repository_inventory(source_root)
    harness_after = _repository_inventory(SOURCE_ROOT)
    baseline_after = _directory_inventory(baseline_root) if baseline_root is not None else None
    integrity = {
        "source_unchanged": source_before == source_after,
        "harness_unchanged": harness_before == harness_after,
        "baseline_unchanged": baseline_before == baseline_after,
        "source_before": source_before, "source_after": source_after,
        "harness_before": harness_before, "harness_after": harness_after,
        "baseline_before": baseline_before, "baseline_after": baseline_after,
    }
    summary = {
        "suite": suite, "variants": sorted(variants), "complete_matrix": complete_variants,
        "model_source": source_identity, "harness_source": harness_identity,
        "builds": builds, "results": results,
        "cross_execution": cross, "integrity": integrity,
    }
    integrity_pass = (
        integrity["source_unchanged"]
        and integrity["harness_unchanged"]
        and integrity["baseline_unchanged"]
    )
    summary["candidate_validation_pass"] = complete_variants and integrity_pass and all(item.get("run_pass") for item in results) and all(item["pass"] for item in cross)
    summary["pass"] = complete_variants and integrity_pass and all(item.get("run_pass") and item.get("baseline_pass") for item in results) and all(item["pass"] for item in cross)
    _json_write(work_root / "summary.json", summary)
    return summary


def _add_selection(parser: argparse.ArgumentParser, include_run_dir: bool = True) -> None:
    """Add shared case-selection arguments to one CLI stage."""
    parser.add_argument("--config", type=Path, default=DEFAULT_CONFIG)
    parser.add_argument("--case", required=True, choices=sorted(("circle_nowind", "fuel_strip_wind", "terrain_fuel_fmc_wind")))
    parser.add_argument("--profile", required=True, choices=("quick", "pr", "full"))
    parser.add_argument("--method")
    parser.add_argument("--feature")
    parser.add_argument("--execution", required=True)
    if include_run_dir:
        parser.add_argument("--run-dir", type=Path, required=True)


def build_parser() -> argparse.ArgumentParser:
    """Build the single public command-line interface for all harness stages."""
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)
    prepare = subparsers.add_parser("prepare", help="validate and stage one new case directory")
    _add_selection(prepare)
    prepare.add_argument("--template", type=Path, default=DEFAULT_TEMPLATE)
    run = subparsers.add_parser("run", help="execute one existing prepared directory")
    run.add_argument("--run-dir", type=Path, required=True)
    run.add_argument("--executable", type=Path, required=True)
    run.add_argument("--launcher-arg", action="append", default=[])
    run.add_argument("--launcher-post-arg", action="append", default=[])
    run.add_argument("--process-count-flag", default="-n")
    compare = subparsers.add_parser("compare", help="compare completed outputs without running the model")
    compare.add_argument("--result-dir", type=Path, required=True)
    compare.add_argument("--baseline-root", type=Path)
    compare.add_argument("--reference-result", type=Path)
    compare.add_argument("--report-parent", type=Path, required=True)
    case = subparsers.add_parser("case", help="prepare, run, validate, and compare one case")
    _add_selection(case, include_run_dir=False)
    case_target = case.add_mutually_exclusive_group(required=True)
    case_target.add_argument("--run-dir", type=Path)
    case_target.add_argument("--attempt-parent", type=Path)
    case.add_argument("--executable", type=Path, required=True)
    case.add_argument("--baseline-root", type=Path)
    case.add_argument("--launcher-arg", action="append", default=[])
    case.add_argument("--launcher-post-arg", action="append", default=[])
    case.add_argument("--process-count-flag", default="-n")
    all_parser = subparsers.add_parser("all", help="build and run the selected outer matrix")
    all_parser.add_argument("--config", type=Path, default=DEFAULT_CONFIG)
    all_parser.add_argument("--suite", required=True, choices=("quick", "pr", "full"))
    all_parser.add_argument("--variants", required=True, help="comma-separated serial,omp,mpi variants")
    all_parser.add_argument("--work-root", type=Path, required=True)
    all_parser.add_argument("--platform", type=Path, default=SCRIPT_DIR / "platforms" / "derecho.yaml")
    all_parser.add_argument("--baseline-root", type=Path)
    all_parser.add_argument("--source-root", type=Path, default=SOURCE_ROOT)
    all_parser.add_argument("--cases", default="all", help="all or a comma-separated case subset for diagnostic dispatch")
    create = subparsers.add_parser("baseline-create", help="create an immutable candidate from a validated work root")
    create.add_argument("--candidate-root", type=Path, required=True)
    create.add_argument("--identifier", required=True)
    create.add_argument("--work-root", type=Path, required=True)
    create.add_argument("--model-repository", type=Path, required=True)
    accept = subparsers.add_parser("baseline-accept", help="record explicit approval for an exact candidate")
    accept.add_argument("--candidate", type=Path, required=True)
    accept.add_argument("--approver", required=True)
    accept.add_argument("--decision", required=True)
    accept.add_argument("--approved-file", type=Path, default=DEFAULT_BASELINES / "approved.yaml")
    return parser


def main(argv: list[str] | None = None) -> int:
    """Dispatch one harness stage and return a process status suitable for CTest."""
    args = build_parser().parse_args(argv)
    if args.command == "prepare":
        manifest = prepare_case(args.config, args.template, args.case, args.profile, args.method, args.feature, args.execution, args.run_dir)
        print(args.run_dir / "run_manifest.json")
        return 0 if manifest["status"] == "prepared" else 1
    if args.command == "run":
        manifest = run_case(
            args.run_dir, args.executable.resolve(), args.launcher_arg,
            args.process_count_flag, args.launcher_post_arg,
        )
        return 0 if manifest["status"] == "completed" else 1
    if args.command == "compare":
        if bool(args.baseline_root) == bool(args.reference_result):
            raise ValueError("Specify exactly one of --baseline-root or --reference-result")
        result = compare_case(args.result_dir, args.baseline_root or args.result_dir, args.report_parent, args.reference_result)
        if not result["pass"]:
            print("CFBM comparison failed: " + "; ".join(result["reasons"]), file=sys.stderr)
        return 0 if result["pass"] else 1
    if args.command == "case":
        run_dir = args.run_dir
        if args.attempt_parent:
            run_dir, _ = prepare_attempt(
                args.attempt_parent, args.config, DEFAULT_TEMPLATE, args.case,
                args.profile, args.method, args.feature, args.execution,
            )
        else:
            prepare_case(args.config, DEFAULT_TEMPLATE, args.case, args.profile, args.method, args.feature, args.execution, run_dir)
        manifest = run_case(
            run_dir, args.executable.resolve(), args.launcher_arg,
            args.process_count_flag, args.launcher_post_arg,
        )
        if manifest["status"] != "completed":
            reason = "Comparison blocked because model execution failed"
            _blocked_comparison(run_dir / "reports", reason)
            print(f"CFBM {reason.lower()}", file=sys.stderr)
            return 1
        document = load_yaml(args.config)
        try:
            baseline_root = resolve_baseline(args.baseline_root, os.environ.get("CFBM_BASELINE_ROOT"), document["baseline"]["approved_id"], DEFAULT_BASELINES)
        except Exception as error:
            reason = f"Comparison blocked: {error}"
            _blocked_comparison(run_dir / "reports", reason)
            print(f"CFBM {reason.lower()}", file=sys.stderr)
            return 1
        result = compare_case(run_dir, baseline_root, run_dir / "reports")
        if not result["pass"]:
            print("CFBM comparison failed: " + "; ".join(result["reasons"]), file=sys.stderr)
        return 0 if result["pass"] else 1
    if args.command == "all":
        document = load_yaml(args.config)
        baseline_root = resolve_baseline(args.baseline_root, os.environ.get("CFBM_BASELINE_ROOT"), document["baseline"]["approved_id"], DEFAULT_BASELINES) if args.baseline_root or os.environ.get("CFBM_BASELINE_ROOT") or document["baseline"]["approved_id"] or (DEFAULT_BASELINES / "approved.yaml").exists() else None
        selected_cases = None if args.cases == "all" else set(args.cases.split(","))
        summary = run_all(
            args.config, args.suite, set(args.variants.split(",")), args.work_root,
            args.platform, baseline_root, args.source_root.resolve(), selected_cases,
        )
        summary_path = args.work_root / "summary.json"
        if baseline_root is None:
            accepted = summary["candidate_validation_pass"]
            print(f"candidate_validation_pass={str(accepted).lower()} summary={summary_path}")
        else:
            accepted = summary["pass"]
            print(f"regression_pass={str(accepted).lower()} summary={summary_path}")
        for comparison in summary["cross_execution"]:
            if not comparison["pass"]:
                print(
                    f"CFBM cross-execution comparison failed: {comparison['pair']} "
                    f"case={comparison['case']}", file=sys.stderr,
                )
        return 0 if accepted else 1
    if args.command == "baseline-create":
        candidate = create_candidate(args.candidate_root, args.identifier, args.work_root, args.model_repository, SOURCE_ROOT)
        print(candidate)
        return 0
    if args.command == "baseline-accept":
        accept_candidate(args.candidate, args.approver, args.decision, args.approved_file)
        print(args.approved_file)
        return 0
    raise AssertionError(args.command)


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"CFBM regression error: {error}", file=sys.stderr)
        raise SystemExit(2)
