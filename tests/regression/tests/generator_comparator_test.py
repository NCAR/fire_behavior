#!/usr/bin/env python3
#
#--------------------------------------------------------------------------------
# Created on 2026-09-12 by the CFBM development team assisted by GPT-6-Astra.
#--------------------------------------------------------------------------------
# run python -B -m unittest discover -s tests/regression/tests -p generator_comparator_test.py -v
#
"""Verify generated NetCDF inputs and output-comparison edge cases.

Create small scratch datasets to check deterministic fields, reader schemas,
fixed tolerances, storage bits, masks, and report serialization. Run with
unittest discovery; no model executable is launched.
"""

from __future__ import annotations

#--------------------------------------------------------------------------------
# Python modules
#--------------------------------------------------------------------------------

import json
import os
import sys
import unittest
import uuid
from pathlib import Path

import netCDF4
import numpy as np

#--------------------------------------------------------------------------------
# Local module lookup
#--------------------------------------------------------------------------------

REGRESSION_DIR = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REGRESSION_DIR))

#--------------------------------------------------------------------------------
# Harness modules under test
#--------------------------------------------------------------------------------

from compare_outputs import compare_directories, compare_file
from run_suite import compare_pair, compare_executions
from reports import write_result
from config import load_yaml, resolve_spec
from generate_inputs import GEO_VARIABLES, sha256_file, generate_inputs, validate_input_file

#--------------------------------------------------------------------------------
# Paths and test layout
#--------------------------------------------------------------------------------

# CTest sets this under the build tree; direct invocations supply it explicitly.
SCRATCH_ROOT = os.environ.get("CFBM_TEST_TMP")

#--------------------------------------------------------------------------------
# Synthetic NetCDF fixtures
#--------------------------------------------------------------------------------


def write_netcdf(path: Path,
                 values: np.ndarray,
                 fill_value: float | None = None) -> None:
    """Write one small synthetic field with explicit dimensions and metadata."""
    with netCDF4.Dataset(path, "w", format="NETCDF4_CLASSIC") as dataset:
        dataset.createDimension("y", values.shape[0])
        dataset.createDimension("x", values.shape[1])
        kwargs = {"fill_value": fill_value} if fill_value is not None else {}
        variable = dataset.createVariable("field", values.dtype, ("y", "x"),
                                          **kwargs)
        variable.units = "1"
        variable[:] = values


#--------------------------------------------------------------------------------
# Input generation tests
#--------------------------------------------------------------------------------


class GeneratorTests(unittest.TestCase):
    """Verify deterministic schema and scientifically selected fields."""

    def setUp(self) -> None:
        """Allocate a persistent scratch location for one validation test."""
        if not SCRATCH_ROOT:
            raise RuntimeError(
                "Set CFBM_TEST_TMP to an artifact directory outside the source tree"
            )
        root = Path(SCRATCH_ROOT).expanduser().resolve()
        if root.is_relative_to(REGRESSION_DIR.parents[1]):
            raise ValueError(
                "Keep Python test artifacts outside the source tree")
        self.root = root / str(uuid.uuid4())
        self.root.mkdir(parents=True)

    def test_repeat_generation_has_equal_file_hashes(self) -> None:
        """Require byte-identical files in the pinned Python environment."""
        document = load_yaml(REGRESSION_DIR / "cases.yaml")
        spec = resolve_spec(document, "fuel_strip_wind", "standard", "ref94",
                            "serial")
        first = self.root / "first"
        second = self.root / "second"
        first.mkdir()
        second.mkdir()
        _, first_info = generate_inputs(spec, first)
        _, second_info = generate_inputs(spec, second)
        self.assertEqual([
            sha256_file(first / name) for name in ("wrf.nc", "geo_em.d01.nc")
        ], [sha256_file(second / name) for name in ("wrf.nc", "geo_em.d01.nc")])
        with netCDF4.Dataset(first / "geo_em.d01.nc") as dataset:
            cats = set(np.unique(dataset.variables["NFUEL_CAT"][:]).astype(int))
        self.assertEqual(cats, {1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13})

    def test_complex_inputs_include_perimeter_and_forcing_records(self) -> None:
        """Require shared fuel strips, observed perimeter, and forcing through final time."""
        document = load_yaml(REGRESSION_DIR / "cases.yaml")
        spec = resolve_spec(document, "terrain_u10m", "standard", "ref94",
                            "serial")
        strip_spec = resolve_spec(document, "fuel_strip_wind", "standard",
                                  "ref94", "serial")
        complex_root = self.root / "complex"
        strip_root = self.root / "strips"
        complex_root.mkdir()
        strip_root.mkdir()
        _, _ = generate_inputs(spec, complex_root)
        _, _ = generate_inputs(strip_spec, strip_root)
        with (
                netCDF4.Dataset(complex_root / "geo_em.d01.nc") as geo,
                netCDF4.Dataset(strip_root / "geo_em.d01.nc") as strips,
        ):
            self.assertIn("lfn_init", geo.variables)
            self.assertGreater(float(np.ptp(geo.variables["ZSF"][:])), 0.0)
            np.testing.assert_array_equal(geo.variables["NFUEL_CAT"][:],
                                          strips.variables["NFUEL_CAT"][:])
        with netCDF4.Dataset(complex_root / "wrf.nc") as wrf:
            self.assertEqual(len(wrf.dimensions["Time"]), 16)
            self.assertEqual(wrf.variables["Q2"].units, "kg kg-1")
            self.assertAlmostEqual(float(wrf.variables["Q2"][0, 0, 0]),
                                   0.008,
                                   places=6)
            self.assertAlmostEqual(float(wrf.variables["Q2"][-1, 0, 0]),
                                   0.004,
                                   places=6)
            self.assertGreater(float(np.ptp(wrf.variables["ZNT"][:])), 0.0)
            self.assertAlmostEqual(float(np.min(wrf.variables["ZNT"][:])),
                                   0.05,
                                   places=6)
            self.assertAlmostEqual(float(np.max(wrf.variables["ZNT"][:])),
                                   0.25,
                                   places=6)

    def test_atmospheric_coordinates_preserve_double_precision(self) -> None:
        """Keep geographic rounding below the precision used by ESMF mapping."""
        spec = resolve_spec(load_yaml(REGRESSION_DIR / "cases.yaml"),
                            "terrain_u10m")
        generate_inputs(spec, self.root)
        with netCDF4.Dataset(self.root / "wrf.nc") as dataset:
            for name in ("XLAT", "XLONG"):
                values = np.asarray(dataset[name][:])
                self.assertEqual(values.dtype, np.dtype("float64"))
                self.assertTrue(np.any(values != values.astype("float32")))
            self.assertEqual(dataset["ZNT"].dtype, np.dtype("float32"))

    def test_reader_schema_validation_rejects_missing_field(self) -> None:
        """Reject a generated geogrid file that omits a field required by the reader."""
        path = self.root / "incomplete_geo.nc"
        with netCDF4.Dataset(path, "w", format="NETCDF4_CLASSIC") as dataset:
            dataset.createDimension("Time", 1)
            dataset.createDimension("DateStrLen", 19)
            dataset.createVariable("Times", "S1", ("Time", "DateStrLen"))
        with self.assertRaises(ValueError):
            validate_input_file(path, GEO_VARIABLES)


#--------------------------------------------------------------------------------
# Output comparison and report tests
#--------------------------------------------------------------------------------


class ComparatorTests(unittest.TestCase):
    """Exercise tolerance, zero-reference, masks, NaNs, infinities, and raw bits."""

    def setUp(self) -> None:
        """Allocate a persistent scratch location for synthetic comparisons."""
        if not SCRATCH_ROOT:
            raise RuntimeError(
                "Set CFBM_TEST_TMP to an artifact directory outside the source tree"
            )
        root = Path(SCRATCH_ROOT).expanduser().resolve()
        if root.is_relative_to(REGRESSION_DIR.parents[1]):
            raise ValueError(
                "Keep Python test artifacts outside the source tree")
        self.root = root / str(uuid.uuid4())
        self.root.mkdir(parents=True)

    def compare(self,
                reference: np.ndarray,
                test: np.ndarray,
                static: bool = False) -> dict[str, object]:
        """Write and compare one synthetic reference/test array pair."""
        ref_path = self.root / "reference.nc"
        test_path = self.root / "test.nc"
        write_netcdf(ref_path, reference)
        write_netcdf(test_path, test)
        return compare_file(ref_path, test_path, {"field"} if static else set())

    def test_relative_rule_and_zero_reference(self) -> None:
        """Accept a sub-boundary relative change and reject any nonzero at zero reference."""
        accepted = self.compare(np.array([[2.0]], dtype="f8"),
                                np.array([[2.00019]], dtype="f8"))
        rejected = self.compare(
            np.array([[0.0]], dtype="f8"),
            np.array([[np.nextafter(0.0, 1.0)]], dtype="f8"))
        self.assertTrue(accepted["pass"])
        self.assertFalse(rejected["pass"])

    def test_exact_tolerance_boundary_passes(self) -> None:
        """Accept a binary-exact tolerance-boundary value."""
        reference = np.array([[10000.0]], dtype="f8")
        test = np.array([[10001.0]], dtype="f8")
        self.assertTrue(self.compare(reference, test)["pass"])

    def test_static_signed_zero_is_bitwise(self) -> None:
        """Reject a static signed-zero storage-bit difference."""
        result = self.compare(np.array([[0.0]], dtype="f4"),
                              np.array([[-0.0]], dtype="f4"),
                              static=True)
        self.assertFalse(result["pass"])
        self.assertEqual(result["fields"][0]["bitwise_difference_count"], 1)

    def test_dynamic_nan_payload_location_and_infinity(self) -> None:
        """Accept agreed NaN locations but reject unexpected matching infinities."""
        nan_result = self.compare(np.array([[np.nan]], dtype="f8"),
                                  np.array([[np.nan]], dtype="f8"))
        inf_result = self.compare(np.array([[np.inf]], dtype="f8"),
                                  np.array([[np.inf]], dtype="f8"))
        self.assertTrue(nan_result["pass"])
        self.assertFalse(inf_result["pass"])

    def test_schema_dtype_and_metadata_mismatches_fail(self) -> None:
        """Reject dimension order, dtype, and variable metadata mismatches."""
        ref_path = self.root / "reference.nc"
        test_path = self.root / "test.nc"
        write_netcdf(ref_path, np.ones((2, 2), dtype="f4"))
        write_netcdf(test_path, np.ones((2, 2), dtype="f8"))
        self.assertFalse(compare_file(ref_path, test_path, set())["pass"])
        with netCDF4.Dataset(test_path, "a") as dataset:
            dataset.variables["field"].units = "m"
        self.assertFalse(compare_file(ref_path, test_path, set())["pass"])

    def test_mask_mismatch_fails(self) -> None:
        """Reject differing missing-value locations independent of valid-cell metrics."""
        ref_path = self.root / "reference.nc"
        test_path = self.root / "test.nc"
        write_netcdf(ref_path,
                     np.array([[-999.0, 1.0]], dtype="f4"),
                     fill_value=-999.0)
        write_netcdf(test_path,
                     np.array([[1.0, 1.0]], dtype="f4"),
                     fill_value=-999.0)
        result = compare_file(ref_path, test_path, set())
        self.assertFalse(result["pass"])
        self.assertEqual(result["fields"][0]["mask_difference_count"], 1)

    def test_empty_valid_set_has_undefined_metrics(self) -> None:
        """Report undefined reductions when every element is agreed missing data."""
        ref_path = self.root / "reference.nc"
        test_path = self.root / "test.nc"
        values = np.array([[-999.0]], dtype="f4")
        write_netcdf(ref_path, values, fill_value=-999.0)
        write_netcdf(test_path, values, fill_value=-999.0)
        result = compare_file(ref_path, test_path, set())
        field = result["fields"][0]
        self.assertTrue(result["pass"])
        self.assertEqual(field["valid_cell_count"], 0)
        self.assertIsNone(field["maximum_absolute_error"])

    def test_expected_inventory_rejects_missing_and_extra_files(self) -> None:
        """Reject output directories that do not match the independent timestamp inventory."""
        reference_dir = self.root / "reference"
        test_dir = self.root / "test"
        reference_dir.mkdir()
        test_dir.mkdir()
        name = "fire_output_2020-01-01_00:00:00.nc"
        extra = "fire_output_2020-01-01_00:01:00.nc"
        write_netcdf(reference_dir / name, np.ones((1, 1), dtype="f4"))
        write_netcdf(test_dir / name, np.ones((1, 1), dtype="f4"))
        write_netcdf(test_dir / extra, np.ones((1, 1), dtype="f4"))
        result = compare_directories(reference_dir, test_dir, [name], set())
        self.assertFalse(result["pass"])

    def run_fixture(self,
                    name: str,
                    driver: str,
                    execution: str,
                    roughness: float,
                    fuel: float = 1.0) -> dict:
        """Write a small completed run with independently adjustable static fields."""
        directory = self.root / name
        directory.mkdir()
        filename = "fire_output_2020-01-01_00:00:00.nc"
        with netCDF4.Dataset(directory / filename, "w") as dataset:
            dataset.createDimension("x", 1)
            for field, value in (("fz0", roughness), ("nfuel_cat", fuel)):
                dataset.createVariable(field, "f8", ("x",))[:] = value
        spec = resolve_spec(load_yaml(REGRESSION_DIR / "cases.yaml"),
                            "terrain_u10m",
                            execution=execution)
        return {
            "directory": str(directory),
            "driver": driver,
            "spec": spec,
            "expected_outputs": [filename],
            "pass": True
        }

    def test_cross_driver_roughness_uses_existing_tolerance(self) -> None:
        """Accept remapping roundoff only across standalone and coupled drivers."""
        standalone = self.run_fixture("serial", "standalone", "serial", 0.1)
        for driver in ("nuopc", "esmx"):
            coupled = self.run_fixture(driver, driver, "mpi4", 0.10000001)
            result = compare_pair(standalone, coupled, {"fz0", "nfuel_cat"})
            self.assertTrue(result["pass"])
            self.assertEqual(result["comparison_kind"], "cross_driver")
            self.assertFalse(result["files"][0]["fields"][0]["bitwise_equal"])
        changed = self.run_fixture("mpi", "standalone", "mpi4", 0.10000001)
        self.assertFalse(compare_pair(standalone, changed, {"fz0"})["pass"])

    def test_cross_driver_still_rejects_large_errors_and_categories(
            self) -> None:
        """Reject roughness outside the tolerance and any changed fuel category."""
        reference = self.run_fixture("serial", "standalone", "serial", 0.1)
        large = self.run_fixture("large", "nuopc", "mpi4", 0.10002)
        category = self.run_fixture("category", "esmx", "mpi4", 0.1, 1.000001)
        for test in (large, category):
            self.assertFalse(
                compare_pair(reference, test, {"fz0", "nfuel_cat"})["pass"])

    def test_suite_keeps_layout_and_cross_driver_comparisons(self) -> None:
        """Require both coupled decomposition agreement and standalone comparisons."""
        results = [
            self.run_fixture("serial", "standalone", "serial", 0.1),
            self.run_fixture("nuopc1", "nuopc", "mpi1", 0.10000001),
            self.run_fixture("nuopc4", "nuopc", "mpi4", 0.10000001),
            self.run_fixture("esmx4", "esmx", "mpi4", 0.10000001)
        ]
        comparisons = compare_executions(results, {"fz0", "nfuel_cat"})
        self.assertEqual(len(comparisons), 4)
        self.assertTrue(all(item["pass"] for item in comparisons))
        self.assertEqual(
            sum(item["comparison_kind"] == "cross_driver"
                for item in comparisons), 3)
        results[1]["spec"]["grid"]["dx_m"] = 200.0
        self.assertTrue(
            any(not item["pass"]
                for item in compare_executions(results, {"fz0", "nfuel_cat"})))

    def test_reports_are_strict_json(self) -> None:
        """Serialize undefined metrics as null in the result report."""
        result = self.compare(np.array([[np.nan]], dtype="f8"),
                              np.array([[np.nan]], dtype="f8"))
        path = self.root / "result.json"
        write_result(path, result)
        self.assertEqual(json.loads(path.read_text())["pass"], result["pass"])


#--------------------------------------------------------------------------------
# Main
#--------------------------------------------------------------------------------

if __name__ == "__main__":
    unittest.main()
