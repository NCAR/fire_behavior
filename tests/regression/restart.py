#!/usr/bin/env python3
# Copyright 2026 Research Applications Laboratory (RAL),
#                National Center for Atmospheric Research (NCAR),
#                University Corporation for Atmospheric Research (UCAR)
#--------------------------------------------------------------------------------
# Created by Maria Frediani (frediani@ucar.edu) on 2026-10-20
# run python -B tests/regression/regression.py suite --suite restart --help
"""Check same-executable restart chains against an uninterrupted integration."""
from __future__ import annotations

#--------------------------------------------------------------------------------
# Python modules and checkpoint contract
#--------------------------------------------------------------------------------
import copy
import datetime as dt
from pathlib import Path
import shutil
import subprocess
from typing import Any

import netCDF4
import numpy as np

from compare_outputs import compare_file
from config import resolved_clock
from generate_inputs import DATE_FORMAT, sha256_file
from render_namelist import render_namelist
from reports import write_result

# Compare persistent physical state. Method work arrays lfn_0/1/2 and lfn_s0/1/2/3,
# and unused crown fluxes fcanhfx/fcanqfx, are written by PR #39 but are not all
# initialized for the selected methods. Their names remain in the schema check.
STATE_FIELDS = {
    "lfn", "lfn_out", "tign_g", "fuel_frac", "fuel_frac_burnt_dt", "fire_area",
    "fgrnhfx", "fgrnqfx", "flame_length", "ros", "ros_front", "emis_smoke",
    "fmc_g", "fuel_load_g", "fuel_time", "zsf", "dzdxf", "dzdyf", "nfuel_cat",
    "uf", "vf"
}
FORCING_FIELDS = {"fire_t2", "fire_q2", "fire_psfc", "fire_rain"}
REAL64_FIELDS = {"fuel_frac", "fuel_frac_burnt_dt"}
MOISTURE_TIMES = ("fmc_fmoist_lasttime", "fmc_fmoist_nexttime")

#--------------------------------------------------------------------------------
# Original-origin schedules and checkpoint validation
#--------------------------------------------------------------------------------


def timestamp(spec: dict[str, Any], seconds: int) -> str:
    """Translate elapsed seconds from the original origin into a file timestamp."""
    return (dt.datetime.strptime(spec["start"], DATE_FORMAT) +
            dt.timedelta(seconds=seconds)).strftime(DATE_FORMAT)


def segment_names(spec: dict[str, Any],
                  begin: int,
                  end: int,
                  checkpoint: bool = False) -> list[str]:
    """Enumerate writes independently of a resumed segment's local start time."""
    key = "restart_interval" if checkpoint else "interval_output"
    interval = int(spec["namelist"]["time"][key])
    seconds = list(range((begin // interval + 1) * interval, end + 1, interval))
    if begin == 0 and not checkpoint:
        seconds.insert(0, 0)
    prefix = "fire_restart" if checkpoint else "fire_output"
    return [f"{prefix}_{timestamp(spec, second)}.nc" for second in seconds]


def checkpoint_fields(spec: dict[str, Any]) -> set[str]:
    """Select physical fields whose owned cells must survive a restart exactly."""
    fields = set(STATE_FIELDS)
    fire = spec["namelist"]["fire"]
    # Init_fire_perimeter alone initializes lfn_hist; ideal runs leave fz0
    # unavailable. Neither is consumed by the corresponding excluded path.
    if fire["fire_is_real_perim"]:
        fields.add("lfn_hist")
    if fire["ideal_opt"] == 0:
        fields |= FORCING_FIELDS | {"fz0"}
    if fire["fmoist_run"]:
        fields |= {"fmc_gc"} | {f"{name}_old" for name in FORCING_FIELDS}
    return fields


def check_checkpoint(path: Path, spec: dict[str, Any],
                     seconds: int) -> dict[str, Any]:
    """Require valid clock, perimeter, class state, and double-precision fuel data."""
    result = {"path": str(path), "pass": False, "reasons": []}
    if not path.is_file():
        result["reasons"].append("Checkpoint is missing")
        return result
    reasons = result["reasons"]
    fire = spec["namelist"]["fire"]
    with netCDF4.Dataset(path) as dataset:
        required = checkpoint_fields(spec)
        missing = required - set(dataset.variables)
        if missing:
            reasons.append(f"Checkpoint fields missing: {sorted(missing)}")
        for name in required & set(dataset.variables):
            variable = dataset[name]
            values = np.ma.asarray(variable[:])
            if (not values.size or np.ma.count_masked(values) or
                    not np.isfinite(values).all()):
                reasons.append(
                    f"Unavailable or nonfinite checkpoint state: {name}")
            if name in REAL64_FIELDS and variable.dtype != np.dtype("float64"):
                reasons.append(f"{name} must retain REAL64 storage")
            if (name in REAL64_FIELDS and
                    seconds == spec["duration_seconds"] and not np.any(
                        values != values.astype("float32").astype("float64"))):
                reasons.append(f"{name} lacks a non-REAL32 precision signal")
        # Check dates and step count independently of the comparison file.
        for prefix, date in (("start", spec["start"]),
                             ("restart", timestamp(spec, seconds))):
            parsed = dt.datetime.strptime(date, DATE_FORMAT)
            for part in ("year", "month", "day", "hour", "minute", "second"):
                name = f"{prefix}_{part}"
                if getattr(dataset, name, None) != getattr(parsed, part):
                    reasons.append(f"Incorrect checkpoint clock: {name}")
        clock_seconds, clock_steps = resolved_clock(spec)
        if getattr(dataset, "itimestep",
                   None) != seconds * clock_steps // clock_seconds:
            reasons.append("Incorrect checkpoint itimestep")
        ignited = int(fire["fire_is_real_perim"] and
                      seconds >= fire["fire_ignition_start_time1"])
        if getattr(dataset, "fire_perimeter_ignited", None) != ignited:
            reasons.append("Incorrect observed-perimeter activation state")
        if fire["fmoist_run"]:
            for name in MOISTURE_TIMES:
                value = getattr(dataset, name, None)
                if value is None or not np.isfinite(value) or value < 0:
                    reasons.append(f"Missing or invalid moisture time: {name}")
            if "fmc_gc" in dataset.variables:
                variable = dataset["fmc_gc"]
                if (variable.dimensions != ("ny", "moisture_class", "nx") or
                        variable.shape[1] != 5):
                    reasons.append("Incorrect five-class moisture dimensions")
                # Positive, finite moisture is required for these nonzero initial
                # values and dry forcing. Zero-filled uninitialized storage fails.
                if np.any(variable[:] <= 0) or np.any(variable[:] >= 1):
                    reasons.append(
                        "Class moisture is outside this case's physical range (0,1)"
                    )
    result["pass"] = not reasons
    return result


def compare_checkpoint(reference: Path, test: Path,
                       spec: dict[str, Any]) -> dict[str, Any]:
    """Require bitwise physical state and identical metadata for the same layout."""
    fields = checkpoint_fields(spec)
    result = compare_file(reference, test, fields, required_fields=fields)
    result["comparison_kind"] = "restart_checkpoint"
    return result


#--------------------------------------------------------------------------------
# Segmented integrations using the existing case launcher
#--------------------------------------------------------------------------------


def run_restarts(spec: dict[str, Any], directory: Path, command: list[str],
                 environment: dict[str, str],
                 static_fields: set[str]) -> dict[str, Any]:
    """Run a cold first segment and chained restarts against unchanged forcing."""
    from run_case import run_model

    result = {
        "pass": False,
        "reasons": [],
        "segments": [],
        "comparisons": [],
        "checkpoint_checks": []
    }
    duration = int(spec["duration_seconds"])
    splits = [0, *spec["restart_splits"], duration]
    inputs = [
        path for name in ("wrf.nc", "geo_em.d01.nc")
        if (path := directory / name).is_file()
    ]
    result["input_sha256"] = {path.name: sha256_file(path) for path in inputs}
    expected = set(segment_names(spec, 0, duration, checkpoint=True))
    if {path.name for path in directory.glob("fire_restart_*.nc")} != expected:
        result["reasons"].append("Continuous checkpoint inventory differs")
    # Validate every continuous checkpoint, including times before first ignition.
    for seconds in range(spec["namelist"]["time"]["restart_interval"],
                         duration + 1,
                         spec["namelist"]["time"]["restart_interval"]):
        result["checkpoint_checks"].append(
            check_checkpoint(
                directory / f"fire_restart_{timestamp(spec, seconds)}.nc", spec,
                seconds))
    previous = None
    for index, (begin, end) in enumerate(zip(splits, splits[1:])):
        segment = directory / f"segment-{index:02d}"
        segment.mkdir()
        segment_spec = copy.deepcopy(spec)
        segment_spec.update(start=timestamp(spec, begin),
                            end=timestamp(spec, end),
                            duration_seconds=end - begin)
        segment_spec["namelist"]["time"]["restart"] = begin > 0
        for path in inputs:
            shutil.copy2(path, segment / path.name)
        incoming = None
        if begin:
            incoming = f"fire_restart_{timestamp(spec, begin)}.nc"
            source = previous / incoming
            if not source.is_file():
                result["reasons"].append(f"Cannot resume: missing {source}")
                break
            shutil.copy2(source, segment / incoming)
        (segment / "namelist.fire").write_text(render_namelist(segment_spec))
        write_result(
            segment / "settings.json", {
                "spec":
                    segment_spec,
                "original_spec":
                    spec,
                "input_sha256":
                    result["input_sha256"],
                "restart_source":
                    str(previous / incoming) if incoming else None,
                "restart_sha256":
                    sha256_file(segment / incoming) if incoming else None
            })
        try:
            record = run_model(command, segment, environment,
                               spec["execution"]["timeout_seconds"],
                               "standalone")
        except (OSError, RuntimeError, subprocess.SubprocessError) as error:
            record = {"reasons": [f"{type(error).__name__}: {error}"]}
        record.update(directory=str(segment),
                      begin_seconds=begin,
                      end_seconds=end)
        for checkpoint in (False, True):
            expected = segment_names(spec, begin, end, checkpoint)
            prefix = "fire_restart" if checkpoint else "fire_output"
            actual = {path.name for path in segment.glob(f"{prefix}_*.nc")}
            inventory = set(expected) | ({incoming}
                                         if checkpoint and incoming else set())
            if actual != inventory:
                record["reasons"].append(
                    f"{prefix} inventory differs: expected={sorted(inventory)}, actual={sorted(actual)}"
                )
            for name in expected:
                if checkpoint:
                    comparison = compare_checkpoint(directory / name,
                                                    segment / name, spec)
                    seconds = int(
                        (dt.datetime.strptime(name[13:-3], DATE_FORMAT) -
                         dt.datetime.strptime(spec["start"],
                                              DATE_FORMAT)).total_seconds())
                    result["checkpoint_checks"].append(
                        check_checkpoint(segment / name, spec, seconds))
                else:
                    comparison = compare_file(directory / name, segment / name,
                                              static_fields)
                    comparison["comparison_kind"] = "restart_output"
                result["comparisons"].append(comparison)
        record["pass"] = not record["reasons"]
        write_result(segment / "execution.json", record)
        result["segments"].append(record)
        previous = segment
    if len(result["segments"]) != len(splits) - 1 or any(
            not record["pass"] for record in result["segments"]):
        result["reasons"].append(
            "One or more restart segments did not complete")
    if any(not item["pass"] for item in result["checkpoint_checks"]):
        result["reasons"].append("Checkpoint contract failed")
    if any(not item["pass"] for item in result["comparisons"]):
        result["reasons"].append("Continuous/restarted comparison failed")
    result["pass"] = not result["reasons"]
    write_result(directory / "restart.json", result)
    return result
