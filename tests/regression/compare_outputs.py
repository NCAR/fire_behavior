#!/usr/bin/env python3
#
#--------------------------------------------------------------------------------
# Created on 2026-09-12 by the CFBM development team assisted by GPT-6-Astra.
#--------------------------------------------------------------------------------
# run python -B tests/regression/regression.py compare --help
#
"""Compare CFBM NetCDF output inventories using the fixed acceptance policy.

Read reference and test fields, check schema and numerical differences, and
return structured numerical evidence to the case and suite runners.
"""

from __future__ import annotations

#--------------------------------------------------------------------------------
# Python modules
#--------------------------------------------------------------------------------

import math
from pathlib import Path
from typing import Any

import netCDF4
import numpy as np

#--------------------------------------------------------------------------------
# Comparison parameters
#--------------------------------------------------------------------------------

RELATIVE_TOLERANCE = 1.0e-4

#--------------------------------------------------------------------------------
# Metadata, storage bits, and decoded values
#--------------------------------------------------------------------------------


def _attribute_value(value: Any) -> Any:
    """Convert a NetCDF attribute into an exactly comparable Python value."""
    if isinstance(value, np.ndarray):
        return (str(value.dtype), value.shape, value.tobytes())
    if isinstance(value, np.generic):
        return (str(value.dtype), value.tobytes())
    return value


def _attributes(target: Any,
                excluded: set[str] | None = None) -> dict[str, Any]:
    """Return exact NetCDF attributes after explicit volatile exclusions."""
    excluded = excluded or set()
    return {
        name: _attribute_value(target.getncattr(name))
        for name in target.ncattrs()
        if name not in excluded
    }


def _raw_bits(values: np.ndarray) -> np.ndarray:
    """Return element storage bytes in a common little-endian representation."""
    array = np.ascontiguousarray(values)
    if array.dtype.byteorder == ">" or (array.dtype.byteorder == "=" and
                                        not np.little_endian):
        array = array.byteswap().view(array.dtype.newbyteorder("<"))
    return array.view(np.dtype((np.void, array.dtype.itemsize)))


def _mask(variable: netCDF4.Variable, raw: np.ndarray) -> np.ndarray:
    """Construct a mask explicitly from fill and missing-value attributes."""
    mask = np.zeros(raw.shape, dtype=bool)
    for attribute in ("_FillValue", "missing_value"):
        if attribute in variable.ncattrs():
            values = np.atleast_1d(variable.getncattr(attribute)).astype(
                raw.dtype, copy=False)
            for value in values:
                mask |= raw == value
    return mask


def _decoded(variable: netCDF4.Variable, raw: np.ndarray) -> np.ndarray:
    """Apply NetCDF scale and offset attributes in float64 arithmetic."""
    values = raw.astype(np.float64)
    scale = float(variable.getncattr(
        "scale_factor")) if "scale_factor" in variable.ncattrs() else 1.0
    offset = float(variable.getncattr(
        "add_offset")) if "add_offset" in variable.ncattrs() else 0.0
    return values * scale + offset


def _json_number(value: float | None) -> tuple[float | None, str | None]:
    """Represent finite metrics in strict JSON and explain undefined values."""
    if value is None:
        return None, "no valid cells"
    if not math.isfinite(value):
        return None, "metric is infinite or nonfinite"
    return value, None


#--------------------------------------------------------------------------------
# Field comparison
#--------------------------------------------------------------------------------


def compare_variable(
    reference: netCDF4.Variable,
    test: netCDF4.Variable,
    static: bool,
) -> dict[str, Any]:
    """Compare one variable including schema, masks, raw bits, and numerical values."""
    result: dict[str, Any] = {
        "variable":
            reference.name,
        "pass":
            True,
        "acceptance_rule":
            "bitwise-static" if static else
            f"abs(test-reference) <= {RELATIVE_TOLERANCE}*abs(reference)",
        "reasons": [],
    }
    if reference.dimensions != test.dimensions:
        result["reasons"].append(
            f"dimension order differs: {reference.dimensions} != {test.dimensions}"
        )
    if reference.shape != test.shape:
        result["reasons"].append(
            f"shape differs: {reference.shape} != {test.shape}")
    if reference.dtype != test.dtype:
        result["reasons"].append(
            f"dtype differs: {reference.dtype} != {test.dtype}")
    if _attributes(reference) != _attributes(test):
        result["reasons"].append("variable metadata differs")
    if result["reasons"]:
        result["pass"] = False
        return result

    reference.set_auto_maskandscale(False)
    test.set_auto_maskandscale(False)
    ref_raw = np.asarray(reference[:])
    test_raw = np.asarray(test[:])
    raw_equal = _raw_bits(ref_raw) == _raw_bits(test_raw)
    result["bitwise_equal"] = bool(np.all(raw_equal))
    result["bitwise_difference_count"] = int(np.count_nonzero(~raw_equal))
    ref_mask = _mask(reference, ref_raw)
    test_mask = _mask(test, test_raw)
    mask_difference = ref_mask != test_mask
    result["mask_difference_count"] = int(np.count_nonzero(mask_difference))
    result["masked_payload_difference_count"] = int(
        np.count_nonzero(ref_mask & test_mask & ~raw_equal))

    if ref_raw.dtype.kind not in "fiu":
        result["differing_cell_count"] = int(
            np.count_nonzero(ref_raw != test_raw))
        result["tolerance_violation_count"] = result["differing_cell_count"]
        result["pass"] = result["pass"] and result[
            "differing_cell_count"] == 0 and result["mask_difference_count"] == 0
        return result

    ref_values = _decoded(reference, ref_raw)
    test_values = _decoded(test, test_raw)
    ref_nan = np.isnan(ref_values)
    test_nan = np.isnan(test_values)
    ref_inf = np.isinf(ref_values)
    test_inf = np.isinf(test_values)
    nonfinite_pattern_difference = (ref_nan != test_nan) | (
        ref_inf
        != test_inf) | (ref_inf & test_inf &
                        (np.signbit(ref_values) != np.signbit(test_values)))
    valid = ~(ref_mask | test_mask | ref_nan | test_nan | ref_inf | test_inf)
    with np.errstate(invalid="ignore"):
        difference = np.abs(test_values - ref_values)
    differing = valid & (test_values != ref_values)
    threshold = RELATIVE_TOLERANCE * np.abs(ref_values)
    violations = valid & (difference > threshold)
    valid_count = int(np.count_nonzero(valid))
    differing_count = int(np.count_nonzero(differing))
    result.update({
        "valid_cell_count":
            valid_count,
        "differing_cell_count":
            differing_count,
        "differing_cell_fraction":
            differing_count / valid_count if valid_count else None,
        "tolerance_violation_count":
            int(np.count_nonzero(violations)),
        "nan_count":
            int(np.count_nonzero(ref_nan & test_nan)),
        "infinity_count":
            int(
                np.count_nonzero(ref_inf & test_inf &
                                 ~nonfinite_pattern_difference)),
        "nonfinite_pattern_difference_count":
            int(np.count_nonzero(nonfinite_pattern_difference)),
    })
    if valid_count:
        valid_difference = difference[valid]
        max_abs = float(np.max(valid_difference))
        with np.errstate(divide="ignore", invalid="ignore"):
            percent = np.where(
                ref_values[valid] == 0.0,
                np.where(test_values[valid] == 0.0, 0.0, np.inf),
                100.0 * valid_difference / np.abs(ref_values[valid]))
        max_percent = float(np.max(percent))
        rms = float(
            np.sqrt(
                np.mean(np.square(valid_difference, dtype=np.float64),
                        dtype=np.float64)))
        first_mask = violations if np.any(violations) else differing
        if np.any(first_mask):
            first_index = tuple(
                int(value) for value in np.argwhere(first_mask)[0])
            result["first_differing_index"] = list(first_index)
            result["first_reference_value"] = float(ref_values[first_index])
            result["first_test_value"] = float(test_values[first_index])
    else:
        max_abs = max_percent = rms = None
    for name, value in (("maximum_absolute_error", max_abs),
                        ("maximum_percentage_error", max_percent), ("rms_error",
                                                                    rms)):
        result[name], result[f"{name}_status"] = _json_number(value)

    if static:
        accepted = result["bitwise_equal"]
    elif ref_raw.dtype.kind in "iu":
        accepted = differing_count == 0
    else:
        accepted = result["tolerance_violation_count"] == 0
    if result["mask_difference_count"] or result[
            "nonfinite_pattern_difference_count"] or result["infinity_count"]:
        accepted = False
    result["pass"] = result["pass"] and accepted
    if not result["pass"] and not result["reasons"]:
        result["reasons"].append("field values violate the acceptance rule")
    return result


#--------------------------------------------------------------------------------
# File and timestamp inventory comparison
#--------------------------------------------------------------------------------


def compare_file(
    reference_path: Path,
    test_path: Path,
    static_fields: set[str],
    volatile_global_attributes: set[str] | None = None,
) -> dict[str, Any]:
    """Compare two NetCDF files and every field under the fixed policy."""
    result: dict[str, Any] = {
        "reference":
            str(reference_path),
        "test":
            str(test_path),
        "pass":
            True,
        "fields": [],
        "reasons": [],
        "excluded_global_attributes":
            sorted(volatile_global_attributes or set()),
    }
    if not reference_path.is_file() or not test_path.is_file():
        result["pass"] = False
        result["reasons"].append("reference or test file is missing")
        return result
    with netCDF4.Dataset(reference_path) as reference, netCDF4.Dataset(
            test_path) as test:
        ref_dims = {
            name: (len(value), value.isunlimited())
            for name, value in reference.dimensions.items()
        }
        test_dims = {
            name: (len(value), value.isunlimited())
            for name, value in test.dimensions.items()
        }
        if ref_dims != test_dims:
            result["reasons"].append(
                f"dimensions differ: {ref_dims} != {test_dims}")
        if set(reference.variables) != set(test.variables):
            result["reasons"].append(
                f"variable sets differ: {sorted(reference.variables)} != {sorted(test.variables)}"
            )
        if _attributes(reference, volatile_global_attributes) != _attributes(
                test, volatile_global_attributes):
            result["reasons"].append("global metadata differs")
        for name in sorted(set(reference.variables) & set(test.variables)):
            result["fields"].append(
                compare_variable(reference.variables[name],
                                 test.variables[name], name in static_fields))
    result["pass"] = not result["reasons"] and all(
        field["pass"] for field in result["fields"])
    return result


def compare_directories(
    reference_dir: Path,
    test_dir: Path,
    expected_files: list[str],
    static_fields: set[str],
    volatile_global_attributes: set[str] | None = None,
) -> dict[str, Any]:
    """Compare an independently expected output inventory in two directories."""
    expected = set(expected_files)
    ref_actual = {path.name for path in reference_dir.glob("fire_output_*.nc")}
    test_actual = {path.name for path in test_dir.glob("fire_output_*.nc")}
    result: dict[str, Any] = {
        "stage": "compare",
        "reference": str(reference_dir),
        "test": str(test_dir),
        "expected_files": sorted(expected),
        "pass": True,
        "reasons": [],
        "files": [],
    }
    if ref_actual != expected:
        result["reasons"].append(
            f"reference inventory differs: expected={sorted(expected)}, actual={sorted(ref_actual)}"
        )
    if test_actual != expected:
        result["reasons"].append(
            f"test inventory differs: expected={sorted(expected)}, actual={sorted(test_actual)}"
        )
    for name in sorted(expected & ref_actual & test_actual):
        result["files"].append(
            compare_file(reference_dir / name, test_dir / name, static_fields,
                         volatile_global_attributes))
    result["pass"] = not result["reasons"] and all(
        item["pass"] for item in result["files"])
    return result
