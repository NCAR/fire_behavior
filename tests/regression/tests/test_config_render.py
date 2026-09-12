#!/usr/bin/env python3
# Copyright 2026      Research Applications Laboratory (RAL),
#                     National Center for Atmospheric Research (NCAR),
#                     University Corporation for Atmospheric Research (UCAR)
#
#--------------------------------------------------------------------------------
# Created by Maria Frediani (frediani@ucar.edu) on 2026-09-12
#--------------------------------------------------------------------------------
# run /glade/work/frediani/casper/anaconda3/envs/py314/bin/python -m unittest tests.regression.tests.test_config_render
#
"""Test strict YAML resolution and portable Fortran namelist rendering."""

from __future__ import annotations

import copy
import os
import sys
import unittest
import uuid
from pathlib import Path

REGRESSION_DIR = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REGRESSION_DIR))
SCRATCH_ROOT = Path(os.environ.get("CFBM_TEST_TMP", "/glade/derecho/scratch/frediani/tmp/cfbm-regression-unit"))

from config import enumerate_matrix, load_platform, load_yaml, resolve_spec, validate_document
from render_namelist import fortran_value, namelist_values, render_template


class ConfigurationTests(unittest.TestCase):
    """Exercise configuration precedence and rejected policy combinations."""

    @classmethod
    def setUpClass(cls) -> None:
        """Load the production case policy once for all focused tests."""
        cls.path = REGRESSION_DIR / "cases.yaml"
        cls.document = load_yaml(cls.path)

    def test_required_matrix_sizes(self) -> None:
        """Require exactly 5, 15, and 36 executions before extra features."""
        self.assertEqual(len(enumerate_matrix(self.document, "quick")), 5)
        self.assertEqual(len(enumerate_matrix(self.document, "pr")), 15)
        self.assertEqual(len(enumerate_matrix(self.document, "full")), 36)

    def test_precedence_and_complete_schedule(self) -> None:
        """Apply suite, method, feature, and execution settings in documented order."""
        spec = resolve_spec(self.document, "fuel_strip_wind", "full", "ref24", None, "mpi8")
        self.assertEqual(spec["grid"]["dx_m"], 25.0)
        self.assertEqual(spec["method"], {"fire_upwinding": 2, "fire_upwinding_reinit": 4})
        self.assertEqual(spec["execution"]["ranks"], 8)
        self.assertEqual(spec["time"]["end"], "2020-01-01_01:00:00")

    def test_unknown_and_disabled_method_rejected(self) -> None:
        """Reject unknown top-level keys and deferred method pair (4,5)."""
        bad = copy.deepcopy(self.document)
        bad["unexpected"] = True
        with self.assertRaisesRegex(ValueError, "Unknown top-level"):
            validate_document(bad)
        bad = copy.deepcopy(self.document)
        bad["methods"]["deferred"] = {"fire_upwinding": 4, "fire_upwinding_reinit": 5}
        with self.assertRaisesRegex(ValueError, "deferred"):
            validate_document(bad)
        bad = copy.deepcopy(self.document)
        bad["cases"]["circle_nowind"]["grid"] = {"nx": 72, "unknown_axis": "x"}
        with self.assertRaisesRegex(ValueError, "unknown_axis"):
            validate_document(bad)

    def test_execution_cannot_change_science(self) -> None:
        """Reject scientific keys in an execution definition."""
        bad = copy.deepcopy(self.document)
        bad["executions"]["serial"]["grid"] = {"nx": 12}
        with self.assertRaises(ValueError):
            validate_document(bad)

    def test_duplicate_yaml_key_rejected(self) -> None:
        """Reject duplicate YAML keys before later values can hide earlier policy."""
        path = SCRATCH_ROOT / str(uuid.uuid4()) / "duplicate-key-test.yaml"
        path.parent.mkdir(parents=True)
        path.write_text("schema_version: 1\nschema_version: 1\n", encoding="utf-8")
        with self.assertRaisesRegex(ValueError, "Duplicate YAML key"):
            load_yaml(path)

    def test_unknown_platform_key_rejected(self) -> None:
        """Reject host profiles that mix launcher policy with unknown settings."""
        path = SCRATCH_ROOT / str(uuid.uuid4()) / "platform.yaml"
        path.parent.mkdir(parents=True)
        path.write_text(
            "schema_version: 1\nname: test\nbuild_environment: null\n"
            "mpi_launcher: [mpiexec]\nmpi_process_flag: [-n]\n"
            "mpi_preflags: []\nmpi_postflags: []\npbs: null\nscience: forbidden\n",
            encoding="utf-8",
        )
        with self.assertRaisesRegex(ValueError, "Unknown platform"):
            load_platform(path)


class NamelistTests(unittest.TestCase):
    """Exercise Fortran scalar syntax and strict template substitution."""

    def test_fortran_scalars(self) -> None:
        """Render logical, integer, real, and safely quoted string values."""
        self.assertEqual(fortran_value(True), ".true.")
        self.assertEqual(fortran_value(4), "4")
        self.assertEqual(fortran_value(4.0), "4.0")
        self.assertEqual(fortran_value("O'Brien"), "'O''Brien'")

    def test_production_template_has_exact_substitutions(self) -> None:
        """Resolve every production placeholder and leave no executable expression."""
        document = load_yaml(REGRESSION_DIR / "cases.yaml")
        spec = resolve_spec(document, "circle_nowind", "quick", None, None, "serial")
        rendered = render_template(REGRESSION_DIR / "templates" / "namelist.fire.in", namelist_values(spec))
        self.assertNotIn("{{", rendered)
        self.assertIn("ideal_opt=1", rendered)

    def test_production_template_has_one_assignment_per_line(self) -> None:
        """Keep every namelist option on a separate line for readable review."""
        lines = (REGRESSION_DIR / "templates" / "namelist.fire.in").read_text(
            encoding="utf-8",
        ).splitlines()
        assignment_lines = [
            line for line in lines
            if line.strip() and not line.lstrip().startswith(("&", "/"))
        ]
        self.assertTrue(assignment_lines)
        self.assertTrue(all(line.count("=") == 1 for line in assignment_lines))

    def test_missing_and_unused_substitutions_fail(self) -> None:
        """Reject both unresolved placeholders and values absent from the template."""
        template = SCRATCH_ROOT / str(uuid.uuid4()) / "template.in"
        template.parent.mkdir(parents=True)
        template.write_text("value={{VALUE}}\n", encoding="utf-8")
        with self.assertRaisesRegex(ValueError, "missing"):
            render_template(template, {})
        with self.assertRaisesRegex(ValueError, "unused"):
            render_template(template, {"VALUE": 1, "EXTRA": 2})


if __name__ == "__main__":
    unittest.main()
