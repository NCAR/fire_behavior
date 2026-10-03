#!/usr/bin/env python3
#
#--------------------------------------------------------------------------------
# Created on 2026-09-12 by the CFBM development team assisted by GPT-6-Astra.
#--------------------------------------------------------------------------------
# run python -B tests/regression/regression.py prepare --help
#
"""Render one CFBM Fortran namelist from a resolved run specification.

Format scalar values and substitute the shared template, rejecting missing or
unused substitutions. Return namelist text to regression.py for staging.
"""

from __future__ import annotations

#--------------------------------------------------------------------------------
# Python modules
#--------------------------------------------------------------------------------

from string import Formatter
from pathlib import Path
from typing import Any

#--------------------------------------------------------------------------------
# Template syntax
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# Fortran values and namelist settings
#--------------------------------------------------------------------------------


def fortran_value(value: Any) -> str:
    """Format a scalar as a portable Fortran namelist literal."""
    if isinstance(value, bool):
        return ".true." if value else ".false."
    if isinstance(value, int):
        return str(value)
    if isinstance(value, float):
        return f"{value:.15g}" if any(
            c in f"{value:.15g}" for c in ".eE") else f"{value:.15g}.0"
    if isinstance(value, str):
        return "'" + value.replace("'", "''") + "'"
    raise TypeError(f"Unsupported namelist value type: {type(value).__name__}")


def namelist_values(spec: dict[str, Any]) -> dict[str, Any]:
    """Map the resolved scientific specification to all template substitutions."""
    start = spec["time"]["start"].replace("_", "-").replace(":", "-").split("-")
    end = spec["time"]["end"].replace("_", "-").replace(":", "-").split("-")
    ignition = spec["ignition"]
    return {
        "START_YEAR": int(start[0]),
        "START_MONTH": int(start[1]),
        "START_DAY": int(start[2]),
        "START_HOUR": int(start[3]),
        "START_MINUTE": int(start[4]),
        "START_SECOND": int(start[5]),
        "END_YEAR": int(end[0]),
        "END_MONTH": int(end[1]),
        "END_DAY": int(end[2]),
        "END_HOUR": int(end[3]),
        "END_MINUTE": int(end[4]),
        "END_SECOND": int(end[5]),
        "DT": spec["time"]["dt_seconds"],
        "INTERVAL_OUTPUT": spec["time"]["output_interval_seconds"],
        "NUM_TILES": spec["model"]["num_tiles"],
        "TILE_STRATEGY": spec["model"]["tile_strategy"],
        "INTERVAL_ATM": spec["time"]["atmosphere_interval_seconds"],
        "KDE": spec["forcing"]["vertical_levels_stag"],
        "FIRE_NUM_IGNITIONS": ignition["count"],
        "IGNITION_ROS": ignition["ros_m_s"],
        "IGNITION_START_LAT": ignition["start_lat"],
        "IGNITION_START_LON": ignition["start_lon"],
        "IGNITION_END_LAT": ignition["end_lat"],
        "IGNITION_END_LON": ignition["end_lon"],
        "IGNITION_RADIUS": ignition["radius_m"],
        "IGNITION_START_TIME": ignition["start_time_s"],
        "IGNITION_END_TIME": ignition["end_time_s"],
        "FIRE_IS_REAL_PERIM": (spec["ignition"]["kind"] == "perimeter"),
        "FIRE_UPWINDING": spec["method"]["fire_upwinding"],
        "FIRE_UPWINDING_REINIT": spec["method"]["fire_upwinding_reinit"],
        "FIRE_LSM_REINIT": spec["model"]["fire_lsm_reinit"],
        "REINIT_COEF": spec["model"]["reinit_pseudot_coef"],
        "FIRE_WIND_HEIGHT": spec["interpolation"]["fire_wind_height_m"],
        "WIND_VINTERP_OPT": spec["interpolation"]["vertical"],
        "HINTERP_OPT": spec["interpolation"]["horizontal"],
        "FMOIST_RUN": spec["moisture"]["run"],
        "FMOIST_FREQ": spec["moisture"]["frequency_timesteps"],
        "FMOIST_DT": spec["moisture"]["dt_seconds"],
        "FUELMC_G": spec["moisture"]["initial_dead"],
        "FUELMC_G_LIVE": spec["moisture"]["initial_live"],
        "IDEAL_OPT": spec["model"]["ideal_opt"],
        "DEVEL_OPT": spec["model"]["devel_opt"],
        "FUEL_OPT": spec["fuel"]["family_id"],
        "FMC_OPT": spec["moisture"]["model_id"],
        "DX": spec["grid"]["dx_m"],
        "DY": spec["grid"]["dy_m"],
        "NX": spec["grid"]["nx"],
        "NY": spec["grid"]["ny"],
        "ZONAL_WIND": spec["forcing"]["u10_m_s"],
        "MERIDIONAL_WIND": spec["forcing"]["v10_m_s"],
        "FUEL_CAT": spec["fuel"]["uniform_category"],
        "DZ_DX": spec["terrain"]["ideal_dz_dx"],
        "DZ_DY": spec["terrain"]["ideal_dz_dy"],
        "ELEVATION": spec["terrain"]["base_elevation_m"],
        "CEN_LAT": spec["projection"]["cen_lat"],
        "CEN_LON": spec["projection"]["cen_lon"],
        "STAND_LON": spec["projection"]["stand_lon"],
        "TRUE_LAT_1": spec["projection"]["true_lat_1"],
        "TRUE_LAT_2": spec["projection"]["true_lat_2"],
        "OUTPUT_LEVEL": spec["output"]["level"],
        "CHECK_ISOLATED": spec["output"]["check_isolated_neg_lfn"],
    }


#--------------------------------------------------------------------------------
# Strict template rendering
#--------------------------------------------------------------------------------


def render_template(template_path: Path, values: dict[str, Any]) -> str:
    """Substitute named Fortran values using Python's standard formatter."""
    text = template_path.read_text(encoding="utf-8")
    names = {name for _, name, _, _ in Formatter().parse(text) if name}
    if names != set(values):
        raise ValueError(f"Template fields differ: {names ^ set(values)}")
    return text.format_map({
        name: fortran_value(value) for name, value in values.items()
    })
