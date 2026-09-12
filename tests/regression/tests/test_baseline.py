#!/usr/bin/env python3
# Copyright 2026      Research Applications Laboratory (RAL),
#                     National Center for Atmospheric Research (NCAR),
#                     University Corporation for Atmospheric Research (UCAR)
#
#--------------------------------------------------------------------------------
# Created by Maria Frediani (frediani@ucar.edu) on 2026-09-12
#--------------------------------------------------------------------------------
# run /glade/work/frediani/casper/anaconda3/envs/py314/bin/python -m unittest tests.regression.tests.test_baseline
#
"""Test immutable baseline provenance and rejection paths."""

from __future__ import annotations

#--------------------------------------------------------------------------------
# Python modules
#--------------------------------------------------------------------------------

import json
import os
import subprocess
import sys
import unittest
import uuid
from pathlib import Path

import netCDF4
import numpy as np

REGRESSION_DIR = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REGRESSION_DIR))

from baseline import create_candidate
from generate_inputs import sha256_file


SCRATCH_ROOT = Path(os.environ.get("CFBM_TEST_TMP", "/glade/derecho/scratch/frediani/tmp/cfbm-regression-unit"))


def initialize_repository(root: Path) -> str:
    """Create one clean committed scratch repository and return its exact commit."""
    root.mkdir(parents=True)
    (root / "source.txt").write_text("committed source\n", encoding="utf-8")
    commands = (
        ["git", "init", str(root)],
        ["git", "-C", str(root), "add", "source.txt"],
        ["git", "-C", str(root), "-c", "user.name=CFBM Tests", "-c", "user.email=cfbm@example.invalid", "commit", "-m", "Initial source"],
    )
    for argv in commands:
        subprocess.run(argv, check=True, capture_output=True, text=True)
    return subprocess.run(
        ["git", "-C", str(root), "rev-parse", "HEAD"], check=True,
        capture_output=True, text=True,
    ).stdout.strip()


def write_candidate_evidence(work_root: Path, commit: str) -> None:
    """Write the smallest completed result accepted by candidate publication."""
    run_dir = work_root / "runs" / "serial" / "circle_nowind" / "quick" / "ref94" / "point_bilinear_10m" / "attempt-000001"
    run_dir.mkdir(parents=True)
    output = run_dir / "fire_output_2020-01-01_00:00:00.nc"
    with netCDF4.Dataset(output, "w", format="NETCDF4_CLASSIC") as dataset:
        dataset.createDimension("cell", 1)
        dataset.createVariable("field", "f4", ("cell",))[:] = np.array([1.0], dtype="f4")
    manifest = {
        "status": "completed",
        "spec": {"identity": {
            "case": "circle_nowind", "suite": "quick", "method": "ref94",
            "feature": "point_bilinear_10m", "execution": "serial",
        }},
        "outputs": [{"name": output.name, "sha256": sha256_file(output)}],
    }
    (run_dir / "run_manifest.json").write_text(json.dumps(manifest), encoding="utf-8")
    summary = {"candidate_validation_pass": True, "model_source": {"commit": commit}}
    (work_root / "summary.json").write_text(json.dumps(summary), encoding="utf-8")


class BaselineTests(unittest.TestCase):
    """Verify clean-source and immutable-identifier requirements."""

    def setUp(self) -> None:
        """Allocate persistent scratch paths for baseline evidence."""
        self.root = SCRATCH_ROOT / str(uuid.uuid4())
        self.repository = self.root / "repository"
        self.commit = initialize_repository(self.repository)
        self.work_root = self.root / "work"
        write_candidate_evidence(self.work_root, self.commit)
        self.candidate_root = self.root / "candidates"

    def test_dirty_source_rejected(self) -> None:
        """Reject candidate production when either recorded source tree is dirty."""
        (self.repository / "source.txt").write_text("modified source\n", encoding="utf-8")
        identifier = f"pre-pr39-{self.commit[:7]}-dirty"
        with self.assertRaisesRegex(ValueError, "clean committed states"):
            create_candidate(self.candidate_root, identifier, self.work_root, self.repository, self.repository)

    def test_existing_identifier_rejected(self) -> None:
        """Create a candidate once and refuse reuse of its immutable identifier."""
        identifier = f"pre-pr39-{self.commit[:7]}-existing"
        created = create_candidate(self.candidate_root, identifier, self.work_root, self.repository, self.repository)
        self.assertTrue((created / "manifest.yaml").is_file())
        with self.assertRaisesRegex(FileExistsError, "already exists"):
            create_candidate(self.candidate_root, identifier, self.work_root, self.repository, self.repository)


if __name__ == "__main__":
    unittest.main()
