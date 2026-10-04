#!/usr/bin/env python3
"""Resolve explicit experiments using exact Fortran namelist option names."""
from __future__ import annotations

import copy
import datetime as dt
import math
from pathlib import Path
from typing import Any

import yaml

#--------------------------------------------------------------------------------
# Supported interface
#--------------------------------------------------------------------------------
# These types describe options exercised by this harness, not every option the
# model can read. Extend this table and its scientific checks with new coverage.
NAMELIST_TYPES = {
    "time": {
        "dt": float,
        "interval_output": float,
        "num_tiles": int,
        "tile_strategy": int
    },
    "atm": {
        "interval_atm": float
    },
    "fire": {
        "fire_num_ignitions": int,
        "fire_ignition_ros1": float,
        "fire_ignition_radius1": float,
        "fire_ignition_start_time1": float,
        "fire_ignition_end_time1": float,
        "fire_is_real_perim": bool,
        "fire_upwinding": int,
        "fire_upwinding_reinit": int,
        "fire_lsm_reinit": bool,
        "fire_lsm_reinit_iter": int,
        "reinit_pseudot_coef": float,
        "fire_wind_height": float,
        "wind_vinterp_opt": int,
        "hinterp_opt": int,
        "fmoist_run": bool,
        "fmoist_freq": int,
        "fmoist_dt": float,
        "fuelmc_g": float,
        "fuelmc_g_live": float,
        "ideal_opt": int,
        "devel_opt": int,
        "fuel_opt": int,
        "fmc_opt": int,
    },
    "devel": {
        "check_isolated_neg_lfn": int,
        "output_level": int
    },
}
INPUT_KEYS = {
    "grid": {"nx", "ny", "dx_m", "dy_m", "sr_x", "sr_y", "axis_order"},
    "projection": {
        "cen_lat", "cen_lon", "stand_lon", "true_lat_1", "true_lat_2",
        "map_proj"
    },
    "terrain": {
        "kind", "base_elevation_m", "amplitude_m", "wavelength_x_m",
        "wavelength_y_m", "ideal_dz_dx", "ideal_dz_dy"
    },
    "fuel": {"family", "uniform_category", "categories", "strip_axis"},
    "ignition": {
        "kind", "center_x_fraction", "center_y_fraction", "line_x_fraction",
        "line_y_start_fraction", "line_y_end_fraction"
    },
    "atmosphere": {
        "wind_terrain_gradient_per_m",
        "u10_m_s",
        "v10_m_s",
        "temperature_start_k",
        "temperature_end_k",
        "mixing_ratio_start_kg_kg",
        "mixing_ratio_end_kg_kg",
        "surface_pressure_pa",
        "accumulated_rain_start_mm",
        "accumulated_rain_end_mm",
        "vertical_levels_stag",
        "roughness_length_min_m",
        "roughness_length_max_m",
        "height_interfaces_m",
        "u_profile_m_s",
        "v_profile_m_s",
    },
}
SCIENCE_KEYS = {"start", "duration_seconds", "namelist", "inputs"}
DRIVERS = {"standalone", "nuopc", "esmx"}
BUILDS = {"serial", "omp", "mpi", "hybrid"}
ALLOWED_METHOD_PAIRS = {(9, 4), (2, 4)}

#--------------------------------------------------------------------------------
# YAML loading and validation helpers
#--------------------------------------------------------------------------------


class UniqueKeyLoader(yaml.SafeLoader):
    """Load safe YAML while rejecting duplicate mapping keys."""


def _construct_mapping(loader: UniqueKeyLoader,
                       node: yaml.MappingNode,
                       deep: bool = False) -> dict[str, Any]:
    """Reject repeated settings before a later value can hide the first."""
    mapping = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node, deep=deep)
        if key in mapping:
            raise ValueError(f"Duplicate YAML key: {key}")
        mapping[key] = loader.construct_object(value_node, deep=deep)
    return mapping


UniqueKeyLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG,
                                _construct_mapping)


def load_yaml(path: Path) -> dict[str, Any]:
    """Read a mapping without interpreting arbitrary Python objects."""
    with path.open(encoding="utf-8") as stream:
        value = yaml.load(stream, Loader=UniqueKeyLoader)
    if not isinstance(value, dict):
        raise TypeError(f"Top-level YAML value must be a mapping: {path}")
    return value


def deep_merge(base: dict[str, Any], update: dict[str, Any]) -> dict[str, Any]:
    """Merge nested mappings; replace lists and scalars without changing inputs."""
    result = copy.deepcopy(base)
    for key, value in update.items():
        if isinstance(value, dict) and isinstance(result.get(key), dict):
            result[key] = deep_merge(result[key], value)
        else:
            result[key] = copy.deepcopy(value)
    return result


def _expect_keys(mapping: dict[str, Any], allowed: set[str],
                 context: str) -> None:
    """Reject misspelled settings with their location in the configuration."""
    if not isinstance(mapping, dict):
        raise ValueError(f"{context} must be a mapping")
    unknown = set(mapping) - allowed
    if unknown:
        raise ValueError(f"Unknown {context} keys: {sorted(unknown)}")


def _require_number(mapping: dict[str, Any], key: str, minimum: float,
                    context: str) -> float:
    """Require a finite value within the scientific lower bound."""
    value = mapping.get(key)
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{context}.{key} must be numeric")
    if not math.isfinite(value) or value < minimum:
        raise ValueError(f"{context}.{key} must be >= {minimum}")
    return float(value)


def _validate_override(values: dict[str, Any], context: str) -> None:
    """Check even overridden settings, so a later layer cannot hide a typo."""
    _expect_keys(values, SCIENCE_KEYS, context)
    nml = values.get("namelist", {})
    _expect_keys(nml, set(NAMELIST_TYPES), f"{context}.namelist")
    for block, options in nml.items():
        _expect_keys(options, set(NAMELIST_TYPES[block]), f"{context}.{block}")
        for name, value in options.items():
            kind = NAMELIST_TYPES[block][name]
            valid = (type(value) in (int, float) and math.isfinite(value)
                     if kind is float else type(value) is kind)
            if not valid:
                raise ValueError(
                    f"{context}.{block}.{name} must be {kind.__name__}")
    inputs = values.get("inputs", {})
    _expect_keys(inputs, set(INPUT_KEYS), f"{context}.inputs")
    for section, options in inputs.items():
        _expect_keys(options, INPUT_KEYS[section],
                     f"{context}.inputs.{section}")


def validate_document(document: dict[str, Any]) -> None:
    """Validate all named selections before CMake registers any experiments."""
    _expect_keys(
        document, {
            "defaults", "cases", "scales", "executions", "suites",
            "static_fields", "expected_output_fields"
        }, "document")
    _validate_override(document["defaults"], "defaults")
    for name, case in document["cases"].items():
        _expect_keys(
            case, SCIENCE_KEYS | {"description", "drivers", "configurations"},
            name)
        _validate_override({
            k: v for k, v in case.items() if k in SCIENCE_KEYS
        }, name)
        if not case["configurations"] or not case["drivers"] or set(
                case["drivers"]) - DRIVERS:
            raise ValueError(
                f"{name} needs configurations and supported drivers")
        for config, values in case["configurations"].items():
            _validate_override(values, f"{name}.{config}")
    for name, values in document["scales"].items():
        _validate_override(values, f"scales.{name}")
    for name, layout in document["executions"].items():
        _expect_keys(layout, {"build", "ranks", "threads", "timeout_seconds"},
                     name)
        if layout["build"] not in BUILDS:
            raise ValueError(f"Unsupported build in {name}")
        for key in ("ranks", "threads", "timeout_seconds"):
            if type(layout[key]) is not int or layout[key] < 1:
                raise ValueError(f"{name}.{key} must be a positive integer")
        if layout["build"] in {"serial", "omp"} and layout["ranks"] != 1:
            raise ValueError(f"{name}: a non-MPI build requires one rank")
        if layout["build"] in {"serial", "mpi"} and layout["threads"] != 1:
            raise ValueError(f"{name}: a non-OpenMP build requires one thread")
    for suite, selection in document["suites"].items():
        _expect_keys(selection, {"scale", "cases"}, suite)
        if selection["scale"] not in document["scales"]:
            raise ValueError(f"Unknown scale in {suite}")
        for case, choices in selection["cases"].items():
            _expect_keys(choices, {"configurations", "executions"},
                         f"{suite}.{case}")
            if case not in document["cases"]:
                raise ValueError(f"Unknown case {case} in {suite}")
            for key, available in (("configurations",
                                    document["cases"][case]["configurations"]),
                                   ("executions", document["executions"])):
                selected = choices[key]
                if not isinstance(selected, list) or not selected or len(
                        set(selected)) != len(
                            selected) or set(selected) - set(available):
                    raise ValueError(
                        f"Unknown, empty, or repeated {key} in {suite}.{case}")


#--------------------------------------------------------------------------------
# Scientific resolution and CTest registrations
#--------------------------------------------------------------------------------


def resolve_spec(document: dict[str, Any],
                 case: str,
                 scale: str = "small",
                 configuration: str | None = None,
                 execution: str = "serial") -> dict[str, Any]:
    """Merge defaults, case, named configuration, then scale; record execution.

    When omitted, configuration selects the first entry listed for that case.
    The resolved identity always records its explicit name.
    """
    validate_document(document)
    definition = document["cases"][case]
    if configuration is None:
        configuration = next(iter(definition["configurations"]))
    spec = deep_merge(document["defaults"], {
        k: v for k, v in definition.items() if k in SCIENCE_KEYS
    })
    spec = deep_merge(spec, definition["configurations"][configuration])
    spec = deep_merge(spec, document["scales"][scale])
    spec["execution"] = copy.deepcopy(document["executions"][execution])
    spec["identity"] = dict(case=case,
                            scale=scale,
                            configuration=configuration,
                            execution=execution)
    validate_spec(spec)
    return spec


def registrations(document: dict[str, Any], variant: str,
                  drivers: list[str]) -> list[dict[str, Any]]:
    """Expand only explicitly selected configurations, executions, and drivers."""
    validate_document(document)
    records = {}
    for suite, selection in document["suites"].items():
        scale = selection["scale"]
        for case, choices in selection["cases"].items():
            for configuration in choices["configurations"]:
                for execution in choices["executions"]:
                    layout = document["executions"][execution]
                    if layout["build"] != variant:
                        continue
                    # Validate scientific combinations while configuring CMake.
                    resolve_spec(document, case, scale, configuration,
                                 execution)
                    for driver in drivers:
                        if driver not in document["cases"][case]["drivers"]:
                            continue
                        name = f"{driver}_{case}_{configuration}_{scale}_{execution}"
                        record = records.setdefault(
                            name,
                            dict(name=name,
                                 case=case,
                                 scale=scale,
                                 configuration=configuration,
                                 execution=execution,
                                 driver=driver,
                                 labels=[],
                                 processors=layout["ranks"] *
                                 layout["threads"]))
                        if suite not in record["labels"]:
                            record["labels"].append(suite)
    return list(records.values())


def validate_spec(spec: dict[str, Any]) -> None:
    """Reject unsupported scientific combinations and misaligned schedules."""
    nml, inputs = spec["namelist"], spec["inputs"]
    time, fire, grid = nml["time"], nml["fire"], inputs["grid"]
    duration = _require_number(spec, "duration_seconds", 1, "spec")
    step = _require_number(time, "dt", 1, "time")
    output = _require_number(time, "interval_output", step, "time")
    forcing_interval = _require_number(nml["atm"], "interval_atm", step, "atm")
    for value in (duration, step, output, forcing_interval):
        if value != int(value):
            raise ValueError("Harness timestamps require integer seconds")
        if abs(value / step - round(value / step)) > 1.0e-10:
            raise ValueError(
                "Schedule must be an integer number of model timesteps")
    for interval in (output, forcing_interval):
        if abs(duration / interval - round(duration / interval)) > 1.0e-10:
            raise ValueError(
                "Integration end must align with output and forcing records")
    start = dt.datetime.strptime(spec["start"], "%Y-%m-%d_%H:%M:%S")
    spec["end"] = (start +
                   dt.timedelta(seconds=duration)).strftime("%Y-%m-%d_%H:%M:%S")
    for name in ("nx", "ny", "sr_x", "sr_y"):
        if type(grid[name]) is not int or grid[name] < 1:
            raise ValueError(f"grid.{name} must be a positive integer")
    for name in ("dx_m", "dy_m"):
        _require_number(grid, name, 1.0e-12, "grid")
    terrain, forcing = inputs["terrain"], inputs["atmosphere"]
    if terrain["kind"] not in {"flat", "sinusoidal"}:
        raise ValueError("Unsupported terrain kind")
    amplitude = _require_number(terrain, "amplitude_m", 0, "terrain")
    for name in ("wavelength_x_m", "wavelength_y_m"):
        _require_number(terrain, name, 1.0e-12, "terrain")
    gradient = _require_number(forcing, "wind_terrain_gradient_per_m", 0,
                               "atmosphere")
    if gradient * amplitude >= 1:
        raise ValueError("Terrain wind scaling must remain positive")
    for name in ("mixing_ratio_start_kg_kg", "mixing_ratio_end_kg_kg"):
        _require_number(forcing, name, 0, "atmosphere")
    z0_min = _require_number(forcing, "roughness_length_min_m", 1.0e-12,
                             "atmosphere")
    z0_max = _require_number(forcing, "roughness_length_max_m", z0_min,
                             "atmosphere")
    if z0_max <= z0_min:
        raise ValueError("Roughness bounds must define spatial variation")
    heights = forcing["height_interfaces_m"]
    if (len(heights) != forcing["vertical_levels_stag"] or len(heights) < 4 or
            heights[0] != 0 or not all(math.isfinite(z) for z in heights) or
            any(b <= a for a, b in zip(heights, heights[1:]))):
        raise ValueError(
            "WRF height interfaces must start at zero and increase")
    for component in ("u_profile_m_s", "v_profile_m_s"):
        if len(forcing[component]) != len(heights) - 1 or not all(
                math.isfinite(v) for v in forcing[component]):
            raise ValueError(f"{component} needs a finite value per mass level")
    pair = (fire["fire_upwinding"], fire["fire_upwinding_reinit"])
    if pair not in ALLOWED_METHOD_PAIRS:
        raise ValueError(f"Unsupported resolved method pair {pair}")
    if fire["ideal_opt"] not in (0, 1) or fire["hinterp_opt"] not in (
            1, 2) or fire["wind_vinterp_opt"] not in (0, 1):
        raise ValueError(
            "Unsupported ideal_opt, hinterp_opt, or wind_vinterp_opt")
    _require_number(fire, "fire_wind_height", 1.0e-12, "fire")
    if fire["ideal_opt"] == 1 and fire["fmoist_run"]:
        raise ValueError(
            "Idealized cases do not support the fuel-moisture model")
    fuel = inputs["fuel"]
    if fire["fuel_opt"] != 1 or fuel["family"] != "Anderson13":
        raise ValueError("Only Anderson fuel_opt=1 is currently covered")
    categories = fuel["categories"]
    if (not isinstance(categories, list) or not categories or any(
            type(v) is not int or not 1 <= v <= 13
            for v in [*categories, fuel["uniform_category"]]) or
            fuel["strip_axis"] != "south_north"):
        raise ValueError(
            "Anderson fuel categories must lie in 1..13; strips use south_north"
        )
    ignition = inputs["ignition"]
    if ignition["kind"] not in {"point", "line", "perimeter"}:
        raise ValueError("Unsupported ignition kind")
    perimeter = ignition["kind"] == "perimeter"
    if perimeter != fire["fire_is_real_perim"]:
        raise ValueError("Ignition geometry must agree with fire_is_real_perim")
    if perimeter and fire["ideal_opt"] != 0:
        raise ValueError(
            "Observed perimeter requires a generated real-case grid")
    # The generator currently describes exactly one point, line, or perimeter.
    if fire["fire_num_ignitions"] != 1:
        raise ValueError("Generated cases require one ignition record")
    for name in ("center_x_fraction", "center_y_fraction", "line_x_fraction",
                 "line_y_start_fraction", "line_y_end_fraction"):
        if _require_number(ignition, name, 0, "ignition") > 1:
            raise ValueError(f"ignition.{name} must be <= 1")
    if ignition["line_y_end_fraction"] <= ignition["line_y_start_fraction"]:
        raise ValueError("Line ignition end must exceed its start fraction")
    ignition_time = _require_number(fire, "fire_ignition_start_time1", 0,
                                    "fire")
    if perimeter and abs(ignition_time / step -
                         round(ignition_time / step)) > 1.0e-10:
        raise ValueError(
            "Perimeter ignition time must align with a fire timestep")
    if time["num_tiles"] < 4:
        raise ValueError(
            "Regression cases require at least four computational tiles")
