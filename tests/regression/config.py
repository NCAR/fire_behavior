#!/usr/bin/env python3
# Copyright 2026      Research Applications Laboratory (RAL),
#                     National Center for Atmospheric Research (NCAR),
#                     University Corporation for Atmospheric Research (UCAR)
#
#--------------------------------------------------------------------------------
# Created by Maria Frediani (frediani@ucar.edu) on 2026-09-12
#--------------------------------------------------------------------------------
# run /glade/work/frediani/casper/anaconda3/envs/py314/bin/python -m pytest tests/regression/tests
#
"""Load, merge, and validate CFBM regression case specifications."""

from __future__ import annotations

#--------------------------------------------------------------------------------
# Python modules
#--------------------------------------------------------------------------------

import copy
import datetime as dt
from pathlib import Path
from typing import Any

import yaml


#--------------------------------------------------------------------------------
# Configuration schema
#--------------------------------------------------------------------------------

TOP_KEYS = {
    "schema_version", "defaults", "cases", "suites", "methods", "features",
    "executions", "static_fields", "metadata_policy", "baseline",
    "expected_output_fields", "provenance",
}
SCIENTIFIC_KEYS = {
    "time", "grid", "projection", "forcing", "fuel", "terrain", "ignition",
    "moisture", "interpolation", "method", "model", "output",
}
NESTED_KEYS = {
    "time": {"start", "duration_seconds", "dt_seconds", "output_interval_seconds", "atmosphere_interval_seconds"},
    "grid": {"nx", "ny", "dx_m", "dy_m", "sr_x", "sr_y", "axis_order"},
    "projection": {"cen_lat", "cen_lon", "stand_lon", "true_lat_1", "true_lat_2", "map_proj"},
    "forcing": {"u10_m_s", "v10_m_s", "temperature_start_k", "temperature_end_k", "specific_humidity_start_kg_kg", "specific_humidity_end_kg_kg", "surface_pressure_pa", "accumulated_rain_start_mm", "accumulated_rain_end_mm", "vertical_levels_stag"},
    "fuel": {"family", "family_id", "uniform_category", "categories", "strip_axis", "background_category", "patch_category", "patch_radius_m"},
    "terrain": {"kind", "base_elevation_m", "amplitude_m", "wavelength_x_m", "wavelength_y_m", "ideal_dz_dx", "ideal_dz_dy"},
    "ignition": {"kind", "count", "center_x_fraction", "center_y_fraction", "line_x_fraction", "radius_m", "ros_m_s", "start_time_s", "end_time_s", "start_lat", "start_lon", "end_lat", "end_lon"},
    "moisture": {"run", "frequency_timesteps", "dt_seconds", "initial_dead", "initial_live", "model_id"},
    "interpolation": {"horizontal", "vertical"},
    "model": {"ideal_opt", "devel_opt", "fire_lsm_reinit", "reinit_pseudot_coef", "num_tiles", "tile_strategy"},
    "output": {"level", "check_isolated_neg_lfn"},
    "feature": {"real_perimeter"},
}
EXECUTION_KEYS = {"variant", "ranks", "threads", "timeout_seconds", "environment"}
FORBIDDEN_EXECUTION_KEYS = SCIENTIFIC_KEYS | {
    "dx", "dy", "nx", "ny", "duration_seconds", "dt_seconds", "fuel_category",
}
ALLOWED_CASES = {"circle_nowind", "fuel_strip_wind", "terrain_fuel_fmc_wind"}
ALLOWED_METHOD_PAIRS = {(9, 4), (2, 4)}


class UniqueKeyLoader(yaml.SafeLoader):
    """Load safe YAML while rejecting duplicate mapping keys."""


def _construct_mapping(loader: UniqueKeyLoader, node: yaml.MappingNode, deep: bool = False) -> dict[str, Any]:
    """Construct one mapping and reject duplicate keys before conversion."""
    mapping: dict[str, Any] = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node, deep=deep)
        if key in mapping:
            raise ValueError(f"Duplicate YAML key: {key}")
        mapping[key] = loader.construct_object(value_node, deep=deep)
    return mapping


UniqueKeyLoader.add_constructor(
    yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, _construct_mapping
)


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


def _expect_keys(mapping: dict[str, Any], allowed: set[str], context: str) -> None:
    """Reject unknown mapping keys with their configuration context."""
    unknown = set(mapping) - allowed
    if unknown:
        raise ValueError(f"Unknown {context} keys: {sorted(unknown)}")


def _validate_partial_science(mapping: dict[str, Any], context: str) -> None:
    """Reject unknown keys in partial scientific configuration mappings."""
    allowed = SCIENTIFIC_KEYS | {"feature"}
    _expect_keys(mapping, allowed, context)
    for name, value in mapping.items():
        if name in {"method", "feature"} and isinstance(value, str):
            continue
        if name not in NESTED_KEYS:
            continue
        if not isinstance(value, dict):
            raise TypeError(f"{context}.{name} must be a mapping")
        _expect_keys(value, NESTED_KEYS[name], f"{context}.{name}")


def _require_number(mapping: dict[str, Any], key: str, minimum: float, context: str) -> float:
    """Return a finite numeric configuration value within its lower bound."""
    value = mapping.get(key)
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise TypeError(f"{context}.{key} must be numeric")
    if value < minimum:
        raise ValueError(f"{context}.{key} must be >= {minimum}")
    return float(value)


def validate_document(document: dict[str, Any]) -> None:
    """Validate the complete configuration document and named matrices."""
    _expect_keys(document, TOP_KEYS, "top-level")
    if document.get("schema_version") != 1:
        raise ValueError("schema_version must equal 1")
    if set(document.get("cases", {})) != ALLOWED_CASES:
        raise ValueError(f"cases must be exactly {sorted(ALLOWED_CASES)}")
    for section in ("defaults", "cases", "suites", "methods", "features", "executions"):
        if not isinstance(document.get(section), dict):
            raise TypeError(f"{section} must be a mapping")
    _validate_partial_science(document["defaults"], "defaults")
    for name, case in document["cases"].items():
        _validate_partial_science(case, f"case {name}")
    for name, suite in document["suites"].items():
        if not isinstance(suite, dict):
            raise TypeError(f"suite {name} must be a mapping")
        _expect_keys(suite, {"time", "grid", "matrix_methods", "matrix_executions"}, f"suite {name}")
        _validate_partial_science({key: value for key, value in suite.items() if key in {"time", "grid"}}, f"suite {name}")
        if not isinstance(suite.get("matrix_methods"), list) or not isinstance(suite.get("matrix_executions"), list):
            raise TypeError(f"suite {name} matrix entries must be lists")
    for name, method in document["methods"].items():
        _expect_keys(method, {"fire_upwinding", "fire_upwinding_reinit"}, f"method {name}")
        pair = (method.get("fire_upwinding"), method.get("fire_upwinding_reinit"))
        if pair == (4, 5):
            raise ValueError("Method pair (4,5) is deferred until after PR 39")
        if pair not in ALLOWED_METHOD_PAIRS:
            raise ValueError(f"Unsupported method pair {pair}")
    for name, execution in document["executions"].items():
        if not isinstance(execution, dict):
            raise TypeError(f"execution {name} must be a mapping")
        _expect_keys(execution, EXECUTION_KEYS, f"execution {name}")
        forbidden = set(execution) & FORBIDDEN_EXECUTION_KEYS
        if forbidden:
            raise ValueError(f"Execution {name} changes scientific settings: {sorted(forbidden)}")
        ranks = execution.get("ranks")
        threads = execution.get("threads")
        if not isinstance(ranks, int) or ranks < 1 or not isinstance(threads, int) or threads < 1:
            raise ValueError(f"Execution {name} ranks and threads must be positive integers")
    for name, feature in document["features"].items():
        _validate_partial_science(feature, f"feature {name}")
    if not isinstance(document["static_fields"], list) or not isinstance(document["expected_output_fields"], list):
        raise TypeError("static_fields and expected_output_fields must be lists")
    _expect_keys(document["metadata_policy"], {"compare_global_attributes", "compare_variable_attributes", "volatile_global_attributes", "rationale"}, "metadata_policy")
    _expect_keys(document["baseline"], {"approved_id", "relative_root"}, "baseline")
    _expect_keys(document["provenance"], {"reference_scripts", "rationale"}, "provenance")
    _expect_keys(document["provenance"]["reference_scripts"], {"create_cases_circle_sha256", "create_cases_strips_sha256", "make_strip_ladder_inputs_sha256"}, "provenance.reference_scripts")
    for name, case in document["cases"].items():
        if case.get("method") not in document["methods"] or case.get("feature") not in document["features"]:
            raise ValueError(f"Case {name} references an unknown method or feature")
    for name, suite in document["suites"].items():
        if not set(suite["matrix_methods"]) <= set(document["methods"]):
            raise ValueError(f"Suite {name} references an unknown method")
        if not set(suite["matrix_executions"]) <= set(document["executions"]):
            raise ValueError(f"Suite {name} references an unknown execution")
    if not set(document["static_fields"]) <= set(document["expected_output_fields"]):
        raise ValueError("Every static field must be in expected_output_fields")


def resolve_spec(
    document: dict[str, Any], case: str, suite: str, method: str | None = None,
    feature: str | None = None, execution: str = "serial",
) -> dict[str, Any]:
    """Resolve one run specification using the contract precedence order."""
    validate_document(document)
    for section, name in (("cases", case), ("suites", suite), ("executions", execution)):
        if name not in document[section]:
            raise KeyError(f"Unknown {section[:-1]} {name}")
    case_cfg = document["cases"][case]
    method_name = method or case_cfg["method"]
    feature_name = feature or case_cfg["feature"]
    if method_name not in document["methods"]:
        raise KeyError(f"Unknown method {method_name}")
    if feature_name not in document["features"]:
        raise KeyError(f"Unknown feature {feature_name}")

    suite_settings = copy.deepcopy(document["suites"][suite])
    suite_settings.pop("matrix_methods", None)
    suite_settings.pop("matrix_executions", None)
    spec = deep_merge(document["defaults"], case_cfg)
    spec = deep_merge(spec, suite_settings)
    spec = deep_merge(spec, {"method": document["methods"][method_name]})
    spec = deep_merge(spec, document["features"][feature_name])
    spec = deep_merge(spec, {"execution": document["executions"][execution]})
    spec["identity"] = {
        "case": case, "suite": suite, "method": method_name,
        "feature": feature_name, "execution": execution,
    }
    validate_spec(spec)
    return spec


def validate_spec(spec: dict[str, Any]) -> None:
    """Validate scientific ranges, combinations, and exact scheduling alignment."""
    _expect_keys(spec, SCIENTIFIC_KEYS | {"feature", "execution", "identity"}, "resolved specification")
    time = spec["time"]
    grid = spec["grid"]
    _expect_keys(time, {"start", "duration_seconds", "dt_seconds", "output_interval_seconds", "atmosphere_interval_seconds"}, "time")
    _expect_keys(grid, {"nx", "ny", "dx_m", "dy_m", "sr_x", "sr_y", "axis_order"}, "grid")
    duration = _require_number(time, "duration_seconds", 0.0, "time")
    step = _require_number(time, "dt_seconds", 1.0e-12, "time")
    output_interval = _require_number(time, "output_interval_seconds", step, "time")
    atmosphere_interval = _require_number(time, "atmosphere_interval_seconds", step, "time")
    for value, name in ((duration, "duration"), (output_interval, "output interval"), (atmosphere_interval, "atmosphere interval")):
        quotient = value / step
        if abs(quotient - round(quotient)) > 1.0e-10:
            raise ValueError(f"{name} must be an integer number of model timesteps")
    start = dt.datetime.strptime(time["start"], "%Y-%m-%d_%H:%M:%S")
    spec["time"]["end"] = (start + dt.timedelta(seconds=duration)).strftime("%Y-%m-%d_%H:%M:%S")
    for name in ("nx", "ny", "sr_x", "sr_y"):
        if not isinstance(grid[name], int) or grid[name] < 1:
            raise ValueError(f"grid.{name} must be a positive integer")
    for name in ("dx_m", "dy_m"):
        _require_number(grid, name, 1.0e-12, "grid")
    pair = (spec["method"]["fire_upwinding"], spec["method"]["fire_upwinding_reinit"])
    if pair not in ALLOWED_METHOD_PAIRS:
        raise ValueError(f"Unsupported resolved method pair {pair}")
    ideal = spec["model"]["ideal_opt"]
    if ideal not in (0, 1):
        raise ValueError("model.ideal_opt must be 0 or 1")
    if ideal == 1 and spec["moisture"]["run"]:
        raise ValueError("Idealized cases do not support the fuel-moisture model")
    if spec["feature"]["real_perimeter"] and ideal != 0:
        raise ValueError("Observed-perimeter initialization requires a generated real-case grid")
    if spec["interpolation"]["horizontal"] not in (1, 2) or spec["interpolation"]["vertical"] not in (0, 1):
        raise ValueError("Unsupported horizontal or vertical interpolation option")
    categories = spec["fuel"]["categories"]
    if not isinstance(categories, list) or not categories or any(not isinstance(value, int) or value < 1 or value > 13 for value in categories):
        raise ValueError("Anderson fuel categories must be a nonempty list within 1..13")
    if spec["ignition"]["kind"] not in {"point", "line", "perimeter"}:
        raise ValueError("Unsupported ignition kind")
    if not isinstance(spec["moisture"]["run"], bool):
        raise TypeError("moisture.run must be logical")


def enumerate_matrix(document: dict[str, Any], suite: str) -> list[dict[str, str]]:
    """Enumerate the required matrix without duplicating shared quick and PR members."""
    validate_document(document)
    if suite not in document["suites"]:
        raise KeyError(f"Unknown suite {suite}")
    suite_cfg = document["suites"][suite]
    rows: list[dict[str, str]] = []
    methods = suite_cfg["matrix_methods"]
    for case in sorted(document["cases"]):
        executions = suite_cfg["matrix_executions"]
        if suite == "quick" and case != "terrain_fuel_fmc_wind":
            executions = ["serial"]
        for method in methods:
            for execution in executions:
                rows.append({
                    "case": case,
                    "suite": suite,
                    "method": method,
                    "feature": document["cases"][case]["feature"],
                    "execution": execution,
                })
    return rows
