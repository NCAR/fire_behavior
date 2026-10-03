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
from render_namelist import namelist_values, render_template
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
        self.document["executions"]["mpi4"]["grid"] = {"nx": 8}
        with self.assertRaises(ValueError):
            resolve_spec(self.document, "terrain_10m", execution="mpi4")

    def test_case_and_full_scale_precedence(self) -> None:
        """Retain case physics when changing domain size and simulation length."""
        spec = resolve_spec(self.document, "terrain_3d", "full", "ref24",
                            "mpi8")
        self.assertEqual(spec["grid"]["nx"], 320)
        self.assertEqual(spec["interpolation"]["vertical"], 0)
        self.assertEqual(spec["method"]["fire_upwinding"], 2)
        self.assertEqual(len(expected_output_names(spec)), 5)

    def test_misspelled_option_is_rejected(self) -> None:
        """Reject an option that would otherwise silently use its default."""
        self.document["cases"]["terrain_10m"]["interpolation"]["vertcal"] = 0
        with self.assertRaises(ValueError):
            resolve_spec(self.document, "terrain_10m")

    def test_output_schedule_is_aligned(self) -> None:
        """Reject output times that cannot coincide with the 4 s timestep."""
        self.document["defaults"]["time"]["output_interval_seconds"] = 7
        with self.assertRaises(ValueError):
            resolve_spec(self.document, "circle_nowind")

    def test_deferred_method_is_rejected(self) -> None:
        """Keep method (4,5) outside the currently reviewed experiments."""
        self.document["methods"]["ref94"] = {
            "fire_upwinding": 4,
            "fire_upwinding_reinit": 5
        }
        with self.assertRaises(ValueError):
            resolve_spec(self.document, "circle_nowind")

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
        spec = resolve_spec(self.document, "terrain_3d", execution="mpi4")
        text = render_template(MODULE_ROOT / "templates/namelist.fire.in",
                               namelist_values(spec))
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
        spec = resolve_spec(self.document, "terrain_3d")
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
        spec = resolve_spec(self.document, "terrain_3d")
        path = self.root / "wind.nc"
        with netCDF4.Dataset(path, "w") as dataset:
            dataset.createDimension("x", 2)
            for name, value in (("uf", 12.), ("vf", 8.)):
                dataset.createVariable(name, "f4", ("x",))[:] = value
        self.assertTrue(check_wind_profile(path, spec)["pass"])
        # Replace the expected interpolated U=12 m/s with the 10 m input.
        # This must fail even though the file remains valid NetCDF.
        with netCDF4.Dataset(path, "a") as dataset:
            dataset["uf"][:] = 10.
        self.assertFalse(check_wind_profile(path, spec)["pass"])

    def test_surface_wind_cannot_vanish_in_a_patch(self) -> None:
        """Reject partial loss of a prescribed nonzero wind component."""
        spec = resolve_spec(self.document, "terrain_10m")
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
                "value": ["quick", "case:terrain_3d"]
            }]
        }]
        args = SimpleNamespace(test="terrain[3d]",
                               suite="quick",
                               case="terrain_3d",
                               execution=None,
                               driver=None)
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
                               case="circle_nowind",
                               scale="standard",
                               method="ref94",
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
