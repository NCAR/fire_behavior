#!/usr/bin/env python3
# Copyright 2026 Research Applications Laboratory (RAL),
#                National Center for Atmospheric Research (NCAR),
#                University Corporation for Atmospheric Research (UCAR)
#--------------------------------------------------------------------------------
# Created by Maria Frediani (frediani@ucar.edu) on 2026-10-20
# run python -B tests/regression/regression.py suite --suite restart --help
"""Exercise invalid schedules and checkpoint metadata with the case launcher."""
from __future__ import annotations

#--------------------------------------------------------------------------------
# Python modules and rejected configurations
#--------------------------------------------------------------------------------
import copy
from pathlib import Path
import re
import shutil
from typing import Any

import netCDF4

from render_namelist import render_namelist
from reports import write_result
from restart import timestamp

REJECTIONS = {
    "unaligned_output": "Time intervals must be integer multiples of dt",
    "unaligned_end": "Final time must align with a fire timestep",
    "forcing_gap": "Atmospheric record spacing differs from interval_atm",
    "clock_mismatch": "Restart metadata mismatch: clock_steps",
    "partial_clock": "Incomplete restart clock metadata",
    "cadence_mismatch": "Restart metadata mismatch: interval_atm",
    "legacy_forcing": "Legacy restart lacks interval_atm metadata",
}

#--------------------------------------------------------------------------------
# Independent model launches, retaining each rejected input and log
#--------------------------------------------------------------------------------


def check_schedule_limits(spec: dict[str,
                                     Any], directory: Path, command: list[str],
                          environment: dict[str, str]) -> dict[str, Any]:
    """Require specific rejections, then stop normally between event boundaries."""
    from run_case import run_model

    checks = []
    for scenario, diagnostic in REJECTIONS.items():
        target = directory / scenario
        target.mkdir()
        settings = copy.deepcopy(spec)
        for name in ("wrf.nc", "geo_em.d01.nc"):
            shutil.copy2(directory / name, target / name)
        if scenario == "unaligned_output":
            settings["namelist"]["time"]["interval_output"] = 13
        elif scenario == "unaligned_end":
            settings["end"] = timestamp(spec, 71)
        elif scenario == "forcing_gap":
            with netCDF4.Dataset(target / "wrf.nc", "a") as dataset:
                dataset["Times"][1, :] = dataset["Times"][0, :]
        else:
            settings["start"] = timestamp(spec, 9)
            settings["namelist"]["time"]["restart"] = True
            name = f"fire_restart_{settings['start']}.nc"
            shutil.copy2(directory / name, target / name)
            with netCDF4.Dataset(target / name, "a") as dataset:
                if scenario == "clock_mismatch":
                    dataset.clock_steps = dataset.clock_steps + 1
                elif scenario == "partial_clock":
                    dataset.delncattr("clock_steps")
                elif scenario == "cadence_mismatch":
                    dataset.interval_atm = 27
                else:
                    # Only the copy loses P04 metadata, reproducing the PR #39
                    # scheduling schema while preserving all physical state.
                    for attribute in ("clock_seconds", "clock_steps",
                                      "interval_atm"):
                        dataset.delncattr(attribute)
        (target / "namelist.fire").write_text(render_namelist(settings))
        result = run_model(command, target, environment,
                           spec["execution"]["timeout_seconds"], "standalone")
        log = (target / "model.log").read_text(errors="replace")
        passed = (diagnostic in log and
                  not list(target.glob("fire_output_*.nc")))
        checks.append({
            "scenario": scenario,
            "expected_diagnostic": diagnostic,
            "pass": passed,
            "execution": result,
            "directory": str(target)
        })

    target = directory / "end_between_events"
    target.mkdir()
    settings = copy.deepcopy(spec)
    settings["end"] = timestamp(spec, 69)
    for name in ("wrf.nc", "geo_em.d01.nc"):
        shutil.copy2(directory / name, target / name)
    (target / "namelist.fire").write_text(render_namelist(settings))
    result = run_model(command, target, environment,
                       spec["execution"]["timeout_seconds"], "standalone")
    log = (target / "model.log").read_text(errors="replace")
    dates = re.findall(r"(2020-01-01_\d\d:\d\d:\d\d)\s+([0-9.E+-]+)", log)
    expected_files = {
        f"fire_output_{timestamp(spec, second)}.nc"
        for second in range(0, 70, 12)
    }
    passed = (not result["reasons"] and bool(dates) and
              dates[-1][0] == timestamp(spec, 69) and
              float(dates[-1][1]) == 0 and
              {path.name for path in target.glob("fire_output_*.nc")
              } == expected_files)
    checks.append({
        "scenario": "end_between_events",
        "pass": passed,
        "execution": result,
        "directory": str(target)
    })
    result = {"pass": all(check["pass"] for check in checks), "checks": checks}
    write_result(directory / "scheduling-limits.json", result)
    return result


def check_legacy_ideal(spec: dict[str,
                                  Any], directory: Path, command: list[str],
                       environment: dict[str, str]) -> dict[str, Any]:
    """Read the known PR #39 idealized schema and retain original event phase."""
    from restart import compare_checkpoint, segment_names
    from run_case import run_model

    target = directory / "legacy_clock"
    target.mkdir()
    begin = spec["restart_splits"][0]
    settings = copy.deepcopy(spec)
    settings["start"] = timestamp(spec, begin)
    settings["namelist"]["time"]["restart"] = True
    name = f"fire_restart_{settings['start']}.nc"
    shutil.copy2(directory / name, target / name)
    with netCDF4.Dataset(target / name, "a") as dataset:
        for attribute in ("clock_seconds", "clock_steps", "interval_atm"):
            dataset.delncattr(attribute)
    (target / "namelist.fire").write_text(render_namelist(settings))
    execution = run_model(command, target, environment,
                          spec["execution"]["timeout_seconds"], "standalone")
    comparisons = []
    end = int(spec["duration_seconds"])
    for name in segment_names(spec, begin, end, checkpoint=True):
        comparisons.append(
            compare_checkpoint(directory / name, target / name, spec))
    inventory = ({path.name for path in target.glob("fire_output_*.nc")
                 } == set(segment_names(spec, begin, end)))
    result = {
        "pass":
            not execution["reasons"] and inventory
            and all(comparison["pass"] for comparison in comparisons),
        "execution":
            execution,
        "comparisons":
            comparisons,
        "output_inventory_pass":
            inventory
    }
    write_result(target / "result.json", result)
    return result
