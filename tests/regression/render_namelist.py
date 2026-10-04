#!/usr/bin/env python3
"""Write exact namelist options, deriving only dates and generated geometry."""
from __future__ import annotations

import copy
import datetime as dt
from typing import Any

#--------------------------------------------------------------------------------
# Fortran values and namelist blocks
#--------------------------------------------------------------------------------


def fortran_value(value: Any) -> str:
    """Format a scalar as a portable Fortran namelist literal."""
    if isinstance(value, bool):
        return ".true." if value else ".false."
    if isinstance(value, int):
        return str(value)
    if isinstance(value, float):
        text = f"{value:.15g}"
        return text if any(c in text for c in ".eE") else text + ".0"
    if isinstance(value, str):
        return "'" + value.replace("'", "''") + "'"
    raise TypeError(f"Unsupported namelist value type: {type(value).__name__}")


def render_namelist(spec: dict[str, Any]) -> str:
    """Render selected options without maintaining a second option-name mapping.

    Clock endpoints, vertical dimension, and the idealized grid describe the
    generated inputs. Ignition coordinates are supplied by generate_inputs.
    """
    blocks = copy.deepcopy(spec["namelist"])
    for endpoint in ("start", "end"):
        date = dt.datetime.strptime(spec[endpoint], "%Y-%m-%d_%H:%M:%S")
        for part in ("year", "month", "day", "hour", "minute", "second"):
            blocks["time"][f"{endpoint}_{part}"] = getattr(date, part)
    inputs = spec["inputs"]
    grid, terrain = inputs["grid"], inputs["terrain"]
    atmosphere = inputs["atmosphere"]
    blocks["atm"]["kde"] = atmosphere["vertical_levels_stag"]
    blocks["ideal"] = {
        "nx": grid["nx"],
        "ny": grid["ny"],
        "dx": grid["dx_m"],
        "dy": grid["dy_m"],
        "zonal_wind": atmosphere["u10_m_s"],
        "meridional_wind": atmosphere["v10_m_s"],
        "fuel_cat": inputs["fuel"]["uniform_category"],
        "dz_dx": terrain["ideal_dz_dx"],
        "dz_dy": terrain["ideal_dz_dy"],
        "elevation": terrain["base_elevation_m"],
        **{
            k: v for k, v in inputs["projection"].items() if k != "map_proj"
        },
    }
    lines = []
    for block in ("time", "atm", "fire", "ideal", "devel"):
        lines.append(f"&{block}")
        lines.extend(f"    {name}={fortran_value(value)},"
                     for name, value in blocks[block].items())
        lines.extend(["/", ""])
    return "\n".join(lines)
