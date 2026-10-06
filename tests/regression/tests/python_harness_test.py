#!/usr/bin/env python3
# Created on 2026-10-02. Developed by the CFBM development team.
# run python -B -m unittest discover -s tests/regression/tests -p '*_test.py' -v
"""This file tests the Python harness itself.

Its checks include rejecting misspelled settings, generating correctly
staggered winds, selecting exact test names, rejecting damaged references,
and detecting missing model output.

These tests create small input/output files without running the Fortran model.
Each test_* method checks one behavior. setUp gives every test a fresh case
configuration and artifact directory; assert* calls describe the expected result.
"""
from __future__ import annotations

import json
import os
from pathlib import Path
import sys
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

import netCDF4
import numpy as np

MODULE_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MODULE_ROOT))

from check_outputs import check_surface_wind, check_wind_profile, expected_output_names
from config import load_yaml, registrations, resolve_spec
from generate_inputs import generate_inputs
from reference import compare_reference, verify_reference
from render_namelist import render_namelist
from run_case import run_case, write_esmx_config
from run_suite import select_tests

# CTest supplies a directory under the build tree. Direct Python invocations
# must supply CFBM_TEST_TMP, so artifacts never go to a developer's home/scratch.
SCRATCH = os.environ.get("CFBM_TEST_TMP")


class PythonHarnessTests(unittest.TestCase):
    """Exercise real input files with synthetic process and output evidence."""

    def setUp(self) -> None:
        """Retain each test's artifacts in a unique scratch directory."""
        if not SCRATCH:
            raise RuntimeError(
                "Set CFBM_TEST_TMP to an artifact directory outside the source tree"
            )
        root = Path(SCRATCH).expanduser().resolve()
        if root.is_relative_to(MODULE_ROOT.parents[1]):
            raise ValueError(
                "Keep Python test artifacts outside the source tree")
        root.mkdir(parents=True, exist_ok=True)
        self.root = Path(
            tempfile.mkdtemp(prefix=self._testMethodName + "-", dir=root))
        self.document = load_yaml(MODULE_ROOT / "cases.yaml")

    #------------------------------------------------------------------------
    # Scientific configuration
    #------------------------------------------------------------------------

    def test_execution_cannot_change_science(self) -> None:
        """Reject scientific overrides hidden in a rank/thread definition."""
        self.document["executions"]["mpi4"]["inputs"] = {"grid": {"nx": 8}}
        with self.assertRaises(ValueError):
            resolve_spec(self.document, "terrain", execution="mpi4")

    def test_case_configuration_and_large_scale_precedence(self) -> None:
        """Retain case physics when changing domain size and simulation length."""
        spec = resolve_spec(self.document, "terrain", "large", "godunov3d",
                            "mpi8")
        self.assertEqual(spec["inputs"]["grid"]["nx"], 320)
        self.assertEqual(spec["namelist"]["fire"]["wind_vinterp_opt"], 0)
        self.assertEqual(spec["namelist"]["fire"]["fire_upwinding"], 2)
        self.assertEqual(len(expected_output_names(spec)), 5)

    def test_misspelled_option_is_rejected(self) -> None:
        """Reject an option that would otherwise silently use its default."""
        self.document["cases"]["terrain"]["namelist"]["fire"][
            "wind_vinter_opt"] = 0
        with self.assertRaises(ValueError):
            resolve_spec(self.document, "terrain")

    def test_output_schedule_is_aligned(self) -> None:
        """Reject output times that cannot coincide with the 4 s timestep."""
        self.document["defaults"]["namelist"]["time"]["interval_output"] = 7
        with self.assertRaises(ValueError):
            resolve_spec(self.document, "circle")

    def test_fractional_model_timestep_remains_supported(self) -> None:
        """Allow subsecond integration steps when saved times align exactly."""
        self.document["cases"]["circle"]["namelist"] = {"time": {"dt": 0.5}}
        spec = resolve_spec(self.document, "circle")
        self.assertEqual(spec["namelist"]["time"]["dt"], 0.5)
        self.assertEqual(len(expected_output_names(spec)), 2)
        self.assertIn("dt=0.5", render_namelist(spec))

    def test_deferred_method_is_rejected(self) -> None:
        """Keep method (4,5) outside the currently reviewed experiments."""
        self.document["cases"]["circle"]["configurations"]["base"] = {
            "namelist": {
                "fire": {
                    "fire_upwinding": 4,
                    "fire_upwinding_reinit": 5
                }
            }
        }
        with self.assertRaises(ValueError):
            resolve_spec(self.document, "circle")

    def test_duplicate_yaml_key_is_rejected(self) -> None:
        """Do not silently overwrite a repeated scientific setting."""
        path = self.root / "duplicate.yaml"
        path.write_text("grid: 1\ngrid: 2\n")
        with self.assertRaises(ValueError):
            load_yaml(path)

    #------------------------------------------------------------------------
    # Generated inputs and output checks
    #------------------------------------------------------------------------

    def test_plain_namelist_and_esmx_clock(self) -> None:
        """Use one resolved clock for both namelist and ESMX configuration."""
        spec = resolve_spec(self.document,
                            "terrain",
                            configuration="u3d",
                            execution="mpi4")
        text = render_namelist(spec)
        self.assertIn("wind_vinterp_opt=0", text)
        self.assertNotIn("{", text)
        path = self.root / "esmxRun.yaml"
        write_esmx_config(path, spec)
        config = load_yaml(path)
        self.assertEqual(config["FIRE"]["petList"], [0, 1, 2, 3])
        self.assertEqual(config["ESMX"]["App"]["stopTime"],
                         "2020-01-01T00:01:00")

    def test_generated_wind_staggering_and_heights(self) -> None:
        """Confirm staggered U/V and the prescribed terrain-relative interfaces."""
        spec = resolve_spec(self.document, "terrain", configuration="u3d")
        spec["inputs"]["atmosphere"]["wind_terrain_gradient_per_m"] = 0.0
        generate_inputs(spec, self.root)
        with netCDF4.Dataset(self.root / "wrf.nc") as dataset:
            # WRF U/V stagger on different horizontal axes. Heights are
            # interfaces above terrain, derived with the reader's gravity.
            self.assertEqual(dataset["U"].shape, (16, 4, 75, 76))
            self.assertEqual(dataset["V"].shape, (16, 4, 76, 75))
            np.testing.assert_array_equal(dataset["U"][0, :, 0, 0],
                                          [10, 14, 18, 22])
            z = (dataset["PH"][0, :, 30, 30] +
                 dataset["PHB"][0, :, 30, 30]) / 9.81
            np.testing.assert_allclose(z - z[0], [0, 20, 60, 120, 200],
                                       atol=0.0002)

    def test_wind_check_rejects_10m_values_in_3d_run(self) -> None:
        """A successful process must not pass with the wrong wind representation."""
        spec = resolve_spec(self.document, "terrain", configuration="u3d")
        spec["inputs"]["atmosphere"]["wind_terrain_gradient_per_m"] = 0.0
        spec["namelist"]["fire"]["fire_wind_height"] = 20.0
        path = self.root / "wind.nc"
        with netCDF4.Dataset(path, "w") as dataset:
            dataset.createDimension("x", 2)
            dataset.createVariable("fz0", "f4", ("x",))[:] = [0.05, 0.25]
            for name, value in (("uf", 12.), ("vf", 8.)):
                dataset.createVariable(name, "f4", ("x",))[:] = value
        self.assertTrue(check_wind_profile(path, spec)["pass"])
        # Replace the expected interpolated U=12 m/s with the 10 m input.
        # This must fail even though the file remains valid NetCDF.
        with netCDF4.Dataset(path, "a") as dataset:
            dataset["uf"][:] = 10.
        self.assertFalse(check_wind_profile(path, spec)["pass"])

    def test_surface_log_profile_uses_cell_roughness(self) -> None:
        """Distinguish 6.096 m winds from raw 10 m and 20 m controls."""
        spec = resolve_spec(self.document, "terrain", configuration="u3d")
        spec["inputs"]["atmosphere"]["wind_terrain_gradient_per_m"] = 0.0
        self.assertEqual(spec["namelist"]["fire"]["fire_wind_height"], 6.096)
        path = self.root / "surface_profile.nc"
        roughness = np.array([0.05, 0.10, 0.25])
        # This closed-form solution tests the surface branch independently
        # of the generator and uses deliberately different roughness cells.
        wind = 10 * np.log(6.096 / roughness) / np.log(10 / roughness)
        with netCDF4.Dataset(path, "w") as dataset:
            dataset.createDimension("x", 3)
            dataset.createVariable("fz0", "f4", ("x",))[:] = roughness
            for name in ("uf", "vf"):
                dataset.createVariable(name, "f4", ("x",))[:] = wind
        self.assertTrue(check_wind_profile(path, spec)["pass"])
        for incorrect in (10.0, 12.0, 4.0):
            with netCDF4.Dataset(path, "a") as dataset:
                dataset["uf"][:] = incorrect
            self.assertFalse(check_wind_profile(path, spec)["pass"])
        with netCDF4.Dataset(path, "a") as dataset:
            dataset["uf"][:] = wind
            dataset["fz0"][0] = 0.0
        self.assertFalse(check_wind_profile(path, spec)["pass"])

    def test_terrain_configurations_share_generated_winds(self) -> None:
        """Both wind choices read the same fixture containing U10/V10 and U/V."""
        contents = []
        for configuration in ("u10m", "u3d"):
            directory = self.root / configuration
            directory.mkdir()
            spec = resolve_spec(self.document,
                                "terrain",
                                configuration=configuration)
            generate_inputs(spec, directory)
            contents.append((directory / "wrf.nc").read_bytes())
        self.assertEqual(*contents)

    def test_explicit_suites_preserve_execution_coverage(self) -> None:
        """Keep all four builds and both coupled entry points in the matrices."""
        records = []
        for build in ("serial", "omp", "mpi", "hybrid"):
            drivers = ["standalone", "nuopc", "esmx"
                      ] if build == "mpi" else ["standalone"]
            records.extend(registrations(self.document, build, drivers))
        for suite, count in (("quick", 14), ("pr", 32), ("full", 80)):
            selected = [r for r in records if suite in r["labels"]]
            self.assertEqual(len(selected), count)
            self.assertEqual({r["case"] for r in selected},
                             {"circle", "fuels", "terrain"})
        args = SimpleNamespace(test=None,
                               suite="pr",
                               case="terrain",
                               execution=None,
                               driver=None,
                               configuration="u3d",
                               scale="small")
        tests = [{
            "name":
                "u10m",
            "properties": [{
                "name":
                    "LABELS",
                "value": [
                    "pr", "case:terrain", "configuration:u10m", "scale:small"
                ]
            }]
        }, {
            "name":
                "u3d",
            "properties": [{
                "name":
                    "LABELS",
                "value": [
                    "pr", "case:terrain", "configuration:u3d", "scale:small"
                ]
            }]
        }]
        self.assertEqual(select_tests(tests, args), [2])

    def test_invalid_configuration_and_option_types_are_rejected(self) -> None:
        """Fail on bad suite selections and exact-option misspellings or types."""
        self.document["suites"]["pr"]["cases"]["terrain"]["configurations"] = [
            "typo"
        ]
        with self.assertRaises(ValueError):
            registrations(self.document, "serial", ["standalone"])
        self.document = load_yaml(MODULE_ROOT / "cases.yaml")
        self.document["defaults"]["namelist"]["fire"]["wind_vinterp_opt"] = "0"
        with self.assertRaises(ValueError):
            resolve_spec(self.document, "terrain")
        self.document = load_yaml(MODULE_ROOT / "cases.yaml")
        self.document["defaults"]["namelist"]["fire"]["wind_vinter_opt"] = 0
        with self.assertRaises(ValueError):
            resolve_spec(self.document, "terrain")

    def test_terrain_winds_increase_at_each_field_location(self) -> None:
        """Detect face offsets using an independent sinusoidal terrain sample."""
        spec = resolve_spec(self.document, "terrain", configuration="u3d")
        generate_inputs(spec, self.root)
        with netCDF4.Dataset(self.root / "wrf.nc") as dataset:
            # Check an interior sample away from terrain extrema, where a
            # half-cell offset changes the expected component appreciably.
            j, i = 17, 23
            terrain = spec["inputs"]["terrain"]

            def height(x: float, y: float) -> float:
                """Evaluate the configured surface independently of generation."""
                return terrain["base_elevation_m"] + terrain["amplitude_m"] * (
                    np.sin(2 * np.pi * x / terrain["wavelength_x_m"]) *
                    np.sin(2 * np.pi * y / terrain["wavelength_y_m"]))

            x = (i + 0.5 - 2) * spec["inputs"]["grid"]["dx_m"]
            y = (j + 0.5 - 2) * spec["inputs"]["grid"]["dy_m"]
            dx, dy = spec["inputs"]["grid"]["dx_m"], spec["inputs"]["grid"][
                "dy_m"]
            z00, z10 = height(x, y), height(x + dx, y)
            z01, z11 = height(x, y + dy), height(x + dx, y + dy)
            samples = {
                "U": (z00 + z01) / 2,
                "V": (z00 + z10) / 2,
                "U10": (z00 + z10 + z01 + z11) / 4
            }
            for name, elevation in samples.items():
                field = dataset[name][0]
                if field.ndim == 3:
                    field = field[0]
                factor = 1 + 0.001 * (elevation - 1600)
                self.assertAlmostEqual(float(field[j, i]),
                                       10 * factor,
                                       places=5)
                self.assertGreater(float(np.ptp(field)), 2.0)

    def test_terrain_wind_check_rejects_uniform_or_excessive_winds(
            self) -> None:
        """Require variability without claiming one exact interpolation method."""
        spec = resolve_spec(self.document, "terrain", configuration="u3d")
        spec["namelist"]["fire"]["fire_wind_height"] = 20.0
        path = self.root / "variable_wind.nc"
        with netCDF4.Dataset(path, "w") as dataset:
            dataset.createDimension("x", 3)
            dataset.createVariable("fz0", "f4", ("x",))[:] = 0.1
            dataset.createVariable("uf", "f4", ("x",))[:] = [11., 12., 13.]
            dataset.createVariable("vf", "f4", ("x",))[:] = [7.5, 8., 8.5]
        self.assertTrue(check_wind_profile(path, spec)["pass"])
        with netCDF4.Dataset(path, "a") as dataset:
            dataset["uf"][:] = 12.
        self.assertFalse(check_wind_profile(path, spec)["pass"])
        with netCDF4.Dataset(path, "a") as dataset:
            dataset["uf"][:] = [11., 12., 20.]
        self.assertFalse(check_wind_profile(path, spec)["pass"])

    def test_terrain_scaling_cannot_reverse_winds(self) -> None:
        """Reject an excessive gradient before constructing synthetic forcing."""
        self.document["cases"]["terrain"]["inputs"]["atmosphere"][
            "wind_terrain_gradient_per_m"] = 0.01
        with self.assertRaises(ValueError):
            resolve_spec(self.document, "terrain", configuration="u3d")

    def test_surface_wind_cannot_vanish_in_a_patch(self) -> None:
        """Reject partial loss of a prescribed nonzero wind component."""
        spec = resolve_spec(self.document, "terrain")
        path = self.root / "surface.nc"
        with netCDF4.Dataset(path, "w") as dataset:
            dataset.createDimension("x", 4)
            dataset.createVariable("uf", "f4", ("x",))[:] = [4., 4., 0., 4.]
            dataset.createVariable("vf", "f4", ("x",))[:] = 4.
        result = check_surface_wind(path, spec)
        self.assertFalse(result["pass"])
        self.assertEqual(result["invalid_cells"]["uf"], 1)

    #------------------------------------------------------------------------
    # CTest selection and resource accounting
    #------------------------------------------------------------------------

    def test_hybrid_resources_cover_ranks_times_threads(self) -> None:
        """Reserve 16 CPUs for each four-rank, four-thread execution."""
        records = registrations(self.document, "hybrid",
                                ["standalone", "nuopc", "esmx"])
        self.assertTrue(all(record["processors"] == 16 for record in records))
        self.assertEqual({r["driver"] for r in records},
                         {"standalone", "nuopc", "esmx"})

    def test_selection_uses_exact_names(self) -> None:
        """Treat punctuation in a test name literally, without pattern matching."""
        # This is the CTest JSON structure read by the suite selector. Brackets
        # deliberately check that an exact name is not interpreted as a regex.
        tests = [{
            "name":
                "terrain[3d]",
            "properties": [{
                "name": "LABELS",
                "value": ["quick", "case:terrain"]
            }]
        }]
        args = SimpleNamespace(test="terrain[3d]",
                               suite="quick",
                               case="terrain",
                               execution=None,
                               driver=None,
                               configuration=None,
                               scale=None)
        self.assertEqual(select_tests(tests, args), [1])
        args.test = "terrain.*"
        self.assertEqual(select_tests(tests, args), [])

    #------------------------------------------------------------------------
    # Stored reference integrity and approval
    #------------------------------------------------------------------------

    def test_reference_corruption_is_rejected(self) -> None:
        """Require cryptographic integrity when reading stored reference data."""
        (self.root / "field.nc").write_bytes(b"changed")
        (self.root / "reference.json").write_text(
            json.dumps({"checksums": {
                "field.nc": "wrong"
            }}))
        with self.assertRaises(ValueError):
            verify_reference(self.root)

    def test_unapproved_reference_is_rejected(self) -> None:
        """An explicit directory does not replace recorded team approval."""
        (self.root / "reference.json").write_text(
            json.dumps({
                "checksums": {},
                "approval": "unapproved",
                "cases": {}
            }))
        with self.assertRaises(ValueError):
            compare_reference(self.root, {}, {})

    #------------------------------------------------------------------------
    # Incomplete model execution
    #------------------------------------------------------------------------

    def test_zero_exit_without_outputs_fails(self) -> None:
        """Catch Fortran STOP paths that return zero before producing results."""
        args = SimpleNamespace(config=MODULE_ROOT / "cases.yaml",
                               case="circle",
                               scale="small",
                               configuration="base",
                               execution="serial",
                               driver="standalone",
                               executable=Path(sys.executable),
                               run_root=self.root,
                               reference=None)
        # Temporarily substitute a process that reports success but writes no
        # model output. Nothing is launched, even though run_case is exercised.
        # The PBS marker permits this synthetic test on an HPC login node.
        environment = {
            "PBS_JOBID": "unit-fixture",
            "CFBM_RUN_ROOT": str(self.root)
        }
        successful_exit = SimpleNamespace(returncode=0)
        source = {"revision": "fixture", "changes": "local edits"}
        with (
                patch.dict(os.environ, environment),
                patch("run_case.subprocess.run", return_value=successful_exit),
                patch("run_case.source_identity", return_value=source),
        ):
            result = run_case(args)
        # Process success alone must never certify a completed integration.
        self.assertFalse(result["pass"])
        self.assertTrue(
            any("inventory" in reason for reason in result["reasons"]))


if __name__ == "__main__":
    unittest.main()
