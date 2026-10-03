#!/usr/bin/env python3
# Created on 2026-10-02.
# Developed by the CFBM development team.
# run python -B tests/regression/regression.py --help
"""Read scientific cases and keep rank/thread choices separate."""
from __future__ import annotations

import copy
import datetime as dt
import math
from pathlib import Path
from typing import Any
import yaml

SCIENTIFIC_KEYS = {
    "time", "grid", "projection", "forcing", "fuel", "terrain", "ignition",
    "moisture", "interpolation", "method", "model", "output"
}
ALLOWED_METHOD_PAIRS = {(9, 4), (2, 4)}

#--------------------------------------------------------------------------------
# Scientific configuration
#--------------------------------------------------------------------------------


def resolve_spec(document: dict[str, Any],
                 case: str,
                 scale: str = "standard",
                 method: str = "ref94",
                 execution: str = "serial") -> dict[str, Any]:
    """Combine shared defaults, one case, and the requested domain size.

    Execution settings are recorded separately and never change the physics.
    The full scale changes only grid dimensions and integration intervals.
    """
    spec = deep_merge(document["defaults"], document["cases"][case])
    spec = deep_merge(spec, document["scales"][scale])
    spec["method"] = copy.deepcopy(document["methods"][method])
    spec["execution"] = copy.deepcopy(document["executions"][execution])
    _expect_keys(spec["execution"],
                 {"variant", "ranks", "threads", "timeout_seconds"},
                 "execution")
    for name in ("ranks", "threads", "timeout_seconds"):
        value = spec["execution"][name]
        if type(value) is not int or value < 1:
            raise ValueError(f"execution.{name} must be a positive integer")
    spec["identity"] = dict(case=case,
                            scale=scale,
                            method=method,
                            execution=execution)
    # Catch misspelled scientific options before generating any files.
    for section, values in spec.items():
        if section in document["defaults"]:
            _expect_keys(values, set(document["defaults"][section]), section)
    validate_spec(spec)
    forcing = spec["forcing"]
    heights = forcing["height_interfaces_m"]
    if (len(heights) != forcing["vertical_levels_stag"] or len(heights) < 4 or
            heights[0] != 0 or
            any(b <= a for a, b in zip(heights, heights[1:]))):
        raise ValueError(
            "WRF height interfaces must start at zero and increase")
    for component in ("u_profile_m_s", "v_profile_m_s"):
        if len(forcing[component]) != len(heights) - 1:
            raise ValueError(f"{component} needs one value per mass level")
    return spec


def registrations(document: dict[str, Any], variant: str,
                  drivers: list[str]) -> list[dict[str, Any]]:
    """Return named CTest registrations from the explicit suite tables."""
    records = {}
    for suite, cases in document["suites"].items():
        scale = "full" if suite == "full" else "standard"
        methods = list(document["methods"]) if suite == "full" else ["ref94"]
        for case, executions in cases.items():
            for execution in executions:
                layout = document["executions"][execution]
                if layout["variant"] != variant:
                    continue
                for driver in drivers:
                    if driver != "standalone" and case not in document[
                            "coupled_cases"]:
                        continue
                    for method in methods:
                        name = f"{driver}_{case}_{scale}_{method}_{execution}"
                        record = records.setdefault(
                            name,
                            dict(name=name,
                                 case=case,
                                 scale=scale,
                                 method=method,
                                 execution=execution,
                                 driver=driver,
                                 labels=[],
                                 processors=layout["ranks"] *
                                 layout["threads"]))
                        record["labels"].append(suite)
    return list(records.values())


class UniqueKeyLoader(yaml.SafeLoader):
    """Load safe YAML while rejecting duplicate mapping keys."""


def _construct_mapping(loader: UniqueKeyLoader,
                       node: yaml.MappingNode,
                       deep: bool = False) -> dict[str, Any]:
    """Construct one mapping and reject duplicate keys before conversion."""
    mapping: dict[str, Any] = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node, deep=deep)
        if key in mapping:
            raise ValueError(f"Duplicate YAML key: {key}")
        mapping[key] = loader.construct_object(value_node, deep=deep)
    return mapping


UniqueKeyLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG,
                                _construct_mapping)


def load_yaml(path: Path) -> dict[str, Any]:
    """Load a YAML mapping with safe scalar construction and unique keys."""
    with path.open("r", encoding="utf-8") as stream:
        value = yaml.load(stream, Loader=UniqueKeyLoader)
    if not isinstance(value, dict):
        raise TypeError(f"Top-level YAML value must be a mapping: {path}")
    return value


def deep_merge(base: dict[str, Any], update: dict[str, Any]) -> dict[str, Any]:
    """Deep-merge mappings while replacing lists and scalar values."""
    result = copy.deepcopy(base)
    for key, value in update.items():
        if isinstance(value, dict) and isinstance(result.get(key), dict):
            result[key] = deep_merge(result[key], value)
        else:
            result[key] = copy.deepcopy(value)
    return result


def _expect_keys(mapping: dict[str, Any], allowed: set[str],
                 context: str) -> None:
    """Reject unknown mapping keys with their configuration context."""
    unknown = set(mapping) - allowed
    if unknown:
        raise ValueError(f"Unknown {context} keys: {sorted(unknown)}")


def _require_number(mapping: dict[str, Any], key: str, minimum: float,
                    context: str) -> float:
    """Return a finite numeric configuration value within its lower bound."""
    value = mapping.get(key)
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise TypeError(f"{context}.{key} must be numeric")
    if not math.isfinite(value) or value < minimum:
        raise ValueError(f"{context}.{key} must be >= {minimum}")
    return float(value)


def validate_spec(spec: dict[str, Any]) -> None:
    """Validate scientific ranges, combinations, and exact scheduling alignment."""
    _expect_keys(spec, SCIENTIFIC_KEYS | {"execution", "identity"},
                 "resolved specification")
    time = spec["time"]
    grid = spec["grid"]
    _expect_keys(
        time, {
            "start", "duration_seconds", "dt_seconds",
            "output_interval_seconds", "atmosphere_interval_seconds", "end"
        }, "time")
    _expect_keys(grid,
                 {"nx", "ny", "dx_m", "dy_m", "sr_x", "sr_y", "axis_order"},
                 "grid")
    duration = _require_number(time, "duration_seconds", 1.0e-12, "time")
    step = _require_number(time, "dt_seconds", 1.0e-12, "time")
    output_interval = _require_number(time, "output_interval_seconds", step,
                                      "time")
    atmosphere_interval = _require_number(time, "atmosphere_interval_seconds",
                                          step, "time")
    for value, name in ((duration, "duration"), (output_interval,
                                                 "output interval"),
                        (atmosphere_interval, "atmosphere interval")):
        quotient = value / step
        if abs(quotient - round(quotient)) > 1.0e-10:
            raise ValueError(
                f"{name} must be an integer number of model timesteps")
    for interval in (output_interval, atmosphere_interval):
        if abs(duration / interval - round(duration / interval)) > 1.0e-10:
            raise ValueError(
                "Integration end must align with output and forcing records")
    start = dt.datetime.strptime(time["start"], "%Y-%m-%d_%H:%M:%S")
    spec["time"]["end"] = (
        start + dt.timedelta(seconds=duration)).strftime("%Y-%m-%d_%H:%M:%S")
    for name in ("nx", "ny", "sr_x", "sr_y"):
        if not isinstance(grid[name], int) or grid[name] < 1:
            raise ValueError(f"grid.{name} must be a positive integer")
    for name in ("dx_m", "dy_m"):
        _require_number(grid, name, 1.0e-12, "grid")
    forcing = spec["forcing"]
    for name in ("mixing_ratio_start_kg_kg", "mixing_ratio_end_kg_kg"):
        _require_number(forcing, name, 0.0, "forcing")
    z0_min = _require_number(forcing, "roughness_length_min_m", 0.0, "forcing")
    z0_max = _require_number(forcing, "roughness_length_max_m", z0_min,
                             "forcing")
    if z0_max <= z0_min:
        raise ValueError(
            "forcing roughness-length bounds must define spatial variation")
    pair = (spec["method"]["fire_upwinding"],
            spec["method"]["fire_upwinding_reinit"])
    if pair not in ALLOWED_METHOD_PAIRS:
        raise ValueError(f"Unsupported resolved method pair {pair}")
    ideal = spec["model"]["ideal_opt"]
    if ideal not in (0, 1):
        raise ValueError("model.ideal_opt must be 0 or 1")
    if ideal == 1 and spec["moisture"]["run"]:
        raise ValueError(
            "Idealized cases do not support the fuel-moisture model")
    if (spec["ignition"]["kind"] == "perimeter") and ideal != 0:
        raise ValueError(
            "Observed-perimeter initialization requires a generated real-case grid"
        )
    if spec["interpolation"]["horizontal"] not in (
            1, 2) or spec["interpolation"]["vertical"] not in (0, 1):
        raise ValueError(
            "Unsupported horizontal or vertical interpolation option")
    categories = spec["fuel"]["categories"]
    if not isinstance(categories, list) or not categories or any(
            not isinstance(value, int) or value < 1 or value > 13
            for value in categories):
        raise ValueError(
            "Anderson fuel categories must be a nonempty list within 1..13")
    if spec["ignition"]["kind"] not in {"point", "line", "perimeter"}:
        raise ValueError("Unsupported ignition kind")
    ignition = spec["ignition"]
    for name in ("center_x_fraction", "center_y_fraction", "line_x_fraction",
                 "line_y_start_fraction", "line_y_end_fraction"):
        value = _require_number(ignition, name, 0.0, "ignition")
        if value > 1.0:
            raise ValueError(f"ignition.{name} must be <= 1")
    if ignition["line_y_end_fraction"] <= ignition["line_y_start_fraction"]:
        raise ValueError(
            "line ignition end fraction must exceed its start fraction")
    if (spec["ignition"]["kind"]
            == "perimeter") and spec["ignition"]["count"] != 1:
        raise ValueError(
            "Observed-perimeter cases require one ignition record for the activation time"
        )
    if (spec["ignition"]["kind"] == "perimeter"):
        ignition_time = _require_number(ignition, "start_time_s", 0.0,
                                        "ignition")
        # Harness schedules are exact boundaries; the model separately admits
        # single-precision roundoff and records any normalization of input time.
        if abs(ignition_time / step - round(ignition_time / step)) > 1.0e-10:
            raise ValueError(
                "Perimeter ignition time must align with a fire timestep")
    if not (spec["ignition"]["kind"]
            == "perimeter") and spec["ignition"]["count"] < 1:
        raise ValueError("Point and line cases require at least one ignition")
    if spec["model"]["num_tiles"] < 4:
        raise ValueError(
            "Regression cases require at least four computational tiles")
    if not isinstance(spec["moisture"]["run"], bool):
        raise TypeError("moisture.run must be logical")
