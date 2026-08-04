#!/usr/bin/env python3
# Copyright 2026      Research Applications Laboratory (RAL),
#                     National Center for Atmospheric Research (NCAR),
#                     University Corporation for Atmospheric Research (UCAR)
#
#--------------------------------------------------------------------------------
# Created by Maria Frediani (frediani@ucar.edu) on 2026-08-03
#--------------------------------------------------------------------------------
# run the comparator on a login node with the complete command in README.md
#
"""Apply bitwise and diagnostic-semantic checks to the active-front matrix."""

from __future__ import annotations

#--------------------------------------------------------------------------------
# Python modules
#--------------------------------------------------------------------------------

import argparse
import hashlib
from pathlib import Path
import re
from typing import Iterable

import netCDF4
import numpy as np


#--------------------------------------------------------------------------------
# Test definitions
#--------------------------------------------------------------------------------

MATRIX_CASES = (
    "p2_r4",
    "p4_r4",
    "p4_r5_rs_off",
    "p4_r5_rs_on",
    "p8_r4",
    "p9_r4",
    "p10_r4",
)
PRE_REFACTOR_CASES = ("p8_r4", "p9_r4", "p10_r4")
GODUNOV_REFERENCE_CASES = ("p4_r5_rs_off", "p4_r5_rs_on")
EXPECTED_OUTPUT_NAMES = tuple(
    f"fire_output_2012-06-25_18:00:{second:02d}.nc" for second in range(7)
)
EXPECTED_ADVANCES = len(EXPECTED_OUTPUT_NAMES) - 1
EXPECTED_PRE_REFACTOR_REVISION = "ca9c8109b2f4e80e40d96fc8492c1b0a2475d50a"
EXPECTED_GODUNOV_REVISION = "27cd977998ee23f14aa0b56c1cdca401ddca63ee"
REFERENCE_MANIFEST_NAME = "reference_manifest.txt"
PRE_REFACTOR_RECEIPT_NAME = "pre_refactor_receipt.txt"
SHA256_PATTERN = re.compile(r"[0-9a-f]{64}")
PBS_JOB_PATTERN = re.compile(r"[0-9]+\.[A-Za-z0-9-]+")
REQUIRED_PROVENANCE_KEYS = {
    "command",
    "source_revision",
    "runner_revision",
    "executable_sha256",
    "namelist_sha256",
    "geo_em.d01.nc_sha256",
    "wrf.nc_sha256",
    "pbs_job_id",
    "execution_mode",
    "mpi_ranks",
}

REQUIRED_NUMERICAL_FIELDS = (
    "lfn",
    "ros",
    "fuel_frac",
    "fire_area",
    "fuel_frac_burnt_dt",
)
# tign_g is not written by this driver. These two fluxes are retained as
# downstream, ignition-time-sensitive equivalents in addition to fire_area.
IGNITION_EQUIVALENT_FIELDS = ("fgrnhfx", "fgrnqfx")
REQUIRED_PAIR_FIELDS = REQUIRED_NUMERICAL_FIELDS + IGNITION_EQUIVALENT_FIELDS + ("fire_area_change_rate",)
ALLOWED_DIAGNOSTIC_DIFFERENCES = {
    "active_front_mask",
    "barrier_contact_front_mask",
    "band_mask",
    "ros_lfn_error_front",
}
SPECIALIZED_FIELDS = ALLOWED_DIAGNOSTIC_DIFFERENCES | {
    "fire_area_change_rate",
    "ros",
    "ros_front",
    "grad_norm_ls",
    "grad_norm_reinit",
    "lfn_tend_dbg",
    "lfn_adv_dbg",
    "lfn_visc_dbg",
    "lfn_pre_reinit_dbg",
    "lfn_post_reinit_dbg",
    "lfn_reinit_delta_dbg",
    "lfn_retreat_delta_dbg",
    "lfn_fastdist_delta_dbg",
    "lfn_laplacian_dbg",
    "rs_interface_mask",
    "rs_distance_dbg",
}
INITIAL_PREADVANCE_UNDEFINED_FIELDS = {
    "fgrnhfx",
    "fgrnqfx",
    "fuel_frac_burnt_dt",
    "ros",
    "ros_front",
    "grad_norm_ls",
    "grad_norm_reinit",
}
# The standalone test7 path uses roughness for wind interpolation but never
# assigns state_fire_t%fz0, so that allocated output field remains undefined.
UNDEFINED_OFFLINE_FIELDS = {"fz0"}
REFERENCE_FIELDS = REQUIRED_NUMERICAL_FIELDS + IGNITION_EQUIVALENT_FIELDS + (
    "grad_norm_ls",
    "grad_norm_reinit",
    "lfn_tend_dbg",
    "lfn_adv_dbg",
    "lfn_visc_dbg",
    "lfn_pre_reinit_dbg",
    "lfn_post_reinit_dbg",
    "lfn_reinit_delta_dbg",
    "rs_interface_mask",
    "rs_distance_dbg",
)
MODEL_DT = np.float32(1.0)
GRAD_NORM_MIN = np.float32(100.0 * np.finfo(np.float32).eps)
CALL_COUNT_PATTERN = re.compile(r"Active-front exact connectivity calls this timestep=(\d+)")


class ComparisonFailure(RuntimeError):
    """Identify the first failed bitwise or semantic comparison."""


#--------------------------------------------------------------------------------
# Command-line interface
#--------------------------------------------------------------------------------


def parse_args() -> argparse.Namespace:
    """Parse matrix roots and optional reference checks."""

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--run-root", type=Path, required=True)
    parser.add_argument("--case", action="append", dest="cases")
    parser.add_argument("--check-mpi", action="store_true")
    parser.add_argument("--pre-refactor-root", type=Path)
    parser.add_argument("--godunov-reference-root", type=Path)
    parser.add_argument(
        "--require-complete",
        action="store_true",
        help="Require the full serial matrix, both references, all special cases, and MPI coverage.",
    )
    return parser.parse_args()


#--------------------------------------------------------------------------------
# NetCDF and bitwise comparison helpers
#--------------------------------------------------------------------------------


def output_files(case_dir: Path) -> list[Path]:
    """Return the exact expected output sequence for one completed case."""

    files = sorted(case_dir.glob("fire_output_*.nc"))
    names = tuple(path.name for path in files)
    if names != EXPECTED_OUTPUT_NAMES:
        raise ComparisonFailure(
            f"Incomplete output sequence in {case_dir}: expected {EXPECTED_OUTPUT_NAMES}, found {names}"
        )
    return files


def check_run_log(case_dir: Path) -> None:
    """Reject missing logs and model STOP messages, including status-zero stops."""

    log_path = case_dir / "run.log"
    if not log_path.is_file():
        raise FileNotFoundError(f"Missing run log: {log_path}")
    if "STOP:" in log_path.read_text(encoding="utf-8"):
        raise ComparisonFailure(f"Model STOP message found in {log_path}")


def provenance(case_dir: Path) -> dict[str, str]:
    """Read the key-value provenance recorded by the maintained runner."""

    path = case_dir / "command.txt"
    if not path.is_file():
        raise FileNotFoundError(f"Missing provenance file: {path}")
    values: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        key, separator, value = line.partition("=")
        if not separator or not key or key in values:
            raise ComparisonFailure(f"Malformed provenance line in {path}: {line!r}")
        values[key] = value
    missing = sorted(REQUIRED_PROVENANCE_KEYS - set(values))
    if missing:
        raise ComparisonFailure(f"Missing provenance keys in {path}: {', '.join(missing)}")
    if not values["pbs_job_id"]:
        raise ComparisonFailure(f"Missing PBS job ID value in {path}")
    return values


def key_value_file(path: Path, description: str) -> dict[str, str]:
    """Read a strict key-value receipt without accepting duplicate entries."""

    if not path.is_file():
        raise FileNotFoundError(f"Missing {description}: {path}")
    values: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        key, separator, value = line.partition("=")
        if not separator or not key or key in values:
            raise ComparisonFailure(f"Malformed {description} line in {path}: {line!r}")
        values[key] = value
    return values


def file_sha256(path: Path) -> str:
    """Hash a preserved reference artifact without loading it into memory."""

    if not path.is_file():
        raise FileNotFoundError(f"Missing preserved reference artifact: {path}")
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def require_sha256(values: dict[str, str], key: str, path: Path) -> str:
    """Require a concrete lowercase SHA-256 value in a receipt."""

    value = values.get(key, "")
    if SHA256_PATTERN.fullmatch(value) is None:
        raise ComparisonFailure(f"Missing or invalid SHA-256 value {key} in {path}: {value!r}")
    return value


def require_pbs_jobs(values: dict[str, str], path: Path) -> set[str]:
    """Require one or more concrete PBS job identifiers in a receipt."""

    jobs = {value.strip() for value in values.get("pbs_job_ids", "").split(",") if value.strip()}
    if not jobs or any(PBS_JOB_PATTERN.fullmatch(job) is None for job in jobs):
        raise ComparisonFailure(f"Missing or invalid pbs_job_ids in {path}: {sorted(jobs)}")
    return jobs


def check_pre_refactor_receipt(root: Path, serial_root: Path) -> None:
    """Bind the declared pre-refactor revision to its preserved executable and runs."""

    path = root / PRE_REFACTOR_RECEIPT_NAME
    values = key_value_file(path, "pre-refactor receipt")
    if values.get("source_revision") != EXPECTED_PRE_REFACTOR_REVISION:
        raise ComparisonFailure(f"Unexpected source_revision in {path}: {values.get('source_revision')!r}")
    expected_executable = require_sha256(values, "executable_sha256", path)
    artifact = root / "reference_executable" / "fire_behavior.exe"
    if file_sha256(artifact) != expected_executable:
        raise ComparisonFailure(f"Preserved pre-refactor executable does not match {path}")
    matching_final_cases = [
        case
        for case in PRE_REFACTOR_CASES
        if provenance(serial_root / case / "active_false")["executable_sha256"]
        == expected_executable
    ]
    if matching_final_cases:
        raise ComparisonFailure(
            "Pre-refactor executable is identical to a maintained final executable for "
            f"{', '.join(matching_final_cases)}"
        )
    declared_jobs = require_pbs_jobs(values, path)
    for case in PRE_REFACTOR_CASES:
        case_values = provenance(root / case / "baseline_false")
        if case_values["executable_sha256"] != expected_executable:
            raise ComparisonFailure(f"Pre-refactor executable hash differs for {case}")
        if case_values["pbs_job_id"] not in declared_jobs:
            raise ComparisonFailure(f"Pre-refactor PBS job is absent from {path}: {case_values['pbs_job_id']}")


def check_reference_manifest(root: Path, serial_root: Path) -> None:
    """Bind the exact historical declaration to preserved inputs and artifacts."""

    path = root / REFERENCE_MANIFEST_NAME
    values = key_value_file(path, "exact-reference manifest")
    if values.get("source_revision") != EXPECTED_GODUNOV_REVISION:
        raise ComparisonFailure(f"Unexpected source_revision in {path}: {values.get('source_revision')!r}")
    if values.get("dropped_features_disabled") != "true":
        raise ComparisonFailure(f"The exact-reference manifest does not declare dropped features disabled: {path}")
    declared_jobs = require_pbs_jobs(values, path)
    expected_executable = require_sha256(values, "executable_sha256", path)
    expected_geo = require_sha256(values, "geo_em.d01.nc_sha256", path)
    expected_wrf = require_sha256(values, "wrf.nc_sha256", path)

    for case in GODUNOV_REFERENCE_CASES:
        case_dir = root / case / "baseline_false"
        check_run_log(case_dir)
        job_path = case_dir / "pbs_job_id.txt"
        if not job_path.is_file():
            raise FileNotFoundError(f"Missing exact-reference PBS receipt: {job_path}")
        job_id = job_path.read_text(encoding="utf-8").strip()
        if job_id not in declared_jobs:
            raise ComparisonFailure(f"Exact-reference PBS job is absent from {path}: {job_id!r}")
        for name, expected in (
            ("fire_behavior.exe", expected_executable),
            ("geo_em.d01.nc", expected_geo),
            ("wrf.nc", expected_wrf),
            ("namelist.fire", require_sha256(values, f"namelist_sha256_{case}", path)),
        ):
            if file_sha256(case_dir / name) != expected:
                raise ComparisonFailure(f"Exact-reference artifact hash differs: {case_dir / name}")
        final_values = provenance(serial_root / case / "active_false")
        if final_values["executable_sha256"] == expected_executable:
            raise ComparisonFailure(
                f"Exact-reference executable is identical to the maintained final executable for {case}"
            )
        if final_values["geo_em.d01.nc_sha256"] != expected_geo or final_values["wrf.nc_sha256"] != expected_wrf:
            raise ComparisonFailure(f"Exact-reference inputs differ from maintained inputs for {case}")


def array_from_variable(dataset: netCDF4.Dataset, field: str) -> np.ndarray:
    """Read one NetCDF variable as a contiguous native array."""

    if field not in dataset.variables:
        raise ComparisonFailure(f"Missing field {field} in {dataset.filepath()}")
    return np.ascontiguousarray(np.asarray(dataset.variables[field][:]))


def first_bitwise_difference(
    left: np.ndarray,
    right: np.ndarray,
) -> tuple[tuple[int, ...], object, object, str, str] | None:
    """Locate the first element whose stored bytes differ."""

    if left.shape != right.shape or left.dtype != right.dtype:
        raise ComparisonFailure(
            f"Array metadata differs: left shape/dtype={left.shape}/{left.dtype}, "
            f"right={right.shape}/{right.dtype}"
        )
    byte_shape = left.shape + (left.dtype.itemsize,)
    left_bytes = left.view(np.uint8).reshape(byte_shape)
    right_bytes = right.view(np.uint8).reshape(byte_shape)
    differing = np.any(left_bytes != right_bytes, axis=-1)
    if not np.any(differing):
        return None
    index = tuple(int(value) for value in np.argwhere(differing)[0])
    return (
        index,
        left[index].item(),
        right[index].item(),
        left_bytes[index].tobytes().hex(),
        right_bytes[index].tobytes().hex(),
    )


def require_fields(dataset: netCDF4.Dataset, fields: Iterable[str]) -> None:
    """Require all named regression fields in an output dataset."""

    missing = sorted(set(fields) - set(dataset.variables))
    if missing:
        raise ComparisonFailure(f"Missing required fields in {dataset.filepath()}: {', '.join(missing)}")


def compare_case_dirs(
    left_dir: Path,
    right_dir: Path,
    *,
    fields: Iterable[str] | None = None,
    ignored_fields: set[str] | None = None,
    skip_initial_fields: set[str] | None = None,
) -> None:
    """Compare matched output sequences and report the first differing element."""

    ignored = ignored_fields or set()
    left_files = output_files(left_dir)
    right_files = output_files(right_dir)
    left_names = [path.name for path in left_files]
    right_names = [path.name for path in right_files]
    if left_names != right_names:
        raise ComparisonFailure(f"Output file sequences differ: {left_dir} versus {right_dir}")

    initial_skip = skip_initial_fields or set()
    for file_index, (left_path, right_path) in enumerate(zip(left_files, right_files, strict=True)):
        with netCDF4.Dataset(left_path) as left_ds, netCDF4.Dataset(right_path) as right_ds:
            if fields is None:
                if set(left_ds.variables) != set(right_ds.variables):
                    raise ComparisonFailure(f"Variable sets differ: {left_path} versus {right_path}")
                comparison_fields = sorted(set(left_ds.variables) - ignored)
            else:
                comparison_fields = list(fields)
                require_fields(left_ds, comparison_fields)
                require_fields(right_ds, comparison_fields)
            if file_index == 0:
                comparison_fields = [field for field in comparison_fields if field not in initial_skip]

            for field in comparison_fields:
                left_array = array_from_variable(left_ds, field)
                right_array = array_from_variable(right_ds, field)
                try:
                    difference = first_bitwise_difference(left_array, right_array)
                except ComparisonFailure as error:
                    raise ComparisonFailure(
                        f"Metadata difference: file={left_path.name}, field={field}: {error}"
                    ) from error
                if difference is not None:
                    index, left_value, right_value, left_hex, right_hex = difference
                    raise ComparisonFailure(
                        f"First difference: file={left_path.name}, field={field}, index={index}, "
                        f"left={left_value!r} [{left_hex}], right={right_value!r} [{right_hex}]"
                    )


#--------------------------------------------------------------------------------
# Diagnostic semantic checks
#--------------------------------------------------------------------------------


def four_neighbor_growth(seed: np.ndarray, burnable: np.ndarray, width: int) -> np.ndarray:
    """Reconstruct the bounded false-mode diagnostic band without periodic wrapping."""

    band = seed.copy()
    frontier = seed.copy()
    for _ in range(max(0, width - 1)):
        neighbor = np.zeros(seed.shape, dtype=bool)
        neighbor[1:, :] |= frontier[:-1, :]
        neighbor[:-1, :] |= frontier[1:, :]
        neighbor[:, 1:] |= frontier[:, :-1]
        neighbor[:, :-1] |= frontier[:, 1:]
        frontier = neighbor & burnable & ~band
        band |= frontier
    return band


def four_neighbor_cells(mask: np.ndarray) -> np.ndarray:
    """Return cells that have a bounded four-neighbor in the input mask."""

    neighbors = np.zeros(mask.shape, dtype=bool)
    neighbors[1:, :] |= mask[:-1, :]
    neighbors[:-1, :] |= mask[1:, :]
    neighbors[:, 1:] |= mask[:, :-1]
    neighbors[:, :-1] |= mask[:, 1:]
    return neighbors


def eight_neighbor_cells(mask: np.ndarray) -> np.ndarray:
    """Return cells that have a bounded eight-neighbor in the input mask."""

    neighbors = np.zeros(mask.shape, dtype=bool)
    for axis0_shift in (-1, 0, 1):
        for axis1_shift in (-1, 0, 1):
            if axis0_shift == 0 and axis1_shift == 0:
                continue
            source0 = slice(max(0, -axis0_shift), mask.shape[0] - max(0, axis0_shift))
            source1 = slice(max(0, -axis1_shift), mask.shape[1] - max(0, axis1_shift))
            target0 = slice(max(0, axis0_shift), mask.shape[0] - max(0, -axis0_shift))
            target1 = slice(max(0, axis1_shift), mask.shape[1] - max(0, -axis1_shift))
            neighbors[target0, target1] |= mask[source0, source1]
    return neighbors


def exterior_connected(mask: np.ndarray) -> np.ndarray:
    """Flood a four-connected Boolean mask from all physical-domain boundaries."""

    exterior = np.zeros(mask.shape, dtype=bool)
    exterior[0, :] = mask[0, :]
    exterior[-1, :] = mask[-1, :]
    exterior[:, 0] |= mask[:, 0]
    exterior[:, -1] |= mask[:, -1]
    while True:
        updated = exterior | (four_neighbor_cells(exterior) & mask)
        if np.array_equal(updated, exterior):
            return exterior
        exterior = updated


def exact_masks_and_band(
    lfn: np.ndarray,
    fuel: np.ndarray,
    width: int,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Reconstruct exact masks and the two-sided diagnostic band."""

    burnable = fuel.astype(np.int64) != 14
    positive = (lfn >= 0.0) & burnable
    exterior = exterior_connected(positive)
    burned_burnable = (lfn < 0.0) & burnable
    has_positive = eight_neighbor_cells(positive)
    has_exterior = eight_neighbor_cells(exterior)
    active = burned_burnable & has_positive & has_exterior
    barrier = burned_burnable & has_positive & ~has_exterior

    positive_frontier = positive & exterior & eight_neighbor_cells(active)
    negative_frontier = active.copy()
    band = positive_frontier | negative_frontier
    for _ in range(1, max(1, width)):
        positive_frontier = four_neighbor_cells(positive_frontier) & positive & ~band
        band |= positive_frontier
        negative_frontier = (
            four_neighbor_cells(negative_frontier)
            & burned_burnable
            & ~band
            & ~barrier
        )
        band |= negative_frontier
    return active, barrier, band


def require_bitwise_equal(expected: np.ndarray, actual: np.ndarray, context: str) -> None:
    """Require bitwise equality and attach semantic context to the first mismatch."""

    try:
        difference = first_bitwise_difference(
            np.ascontiguousarray(expected),
            np.ascontiguousarray(actual),
        )
    except ComparisonFailure as error:
        raise ComparisonFailure(f"{context}: {error}") from error
    if difference is not None:
        index, expected_value, actual_value, expected_hex, actual_hex = difference
        raise ComparisonFailure(
            f"{context}: index={index}, expected={expected_value!r} [{expected_hex}], "
            f"actual={actual_value!r} [{actual_hex}]"
        )


def expected_ros_lfn_error(dataset: netCDF4.Dataset, support: np.ndarray) -> np.ndarray:
    """Evaluate the documented ROS-error formula on the supplied support mask."""

    gradient = array_from_variable(dataset, "grad_norm_ls")
    tendency = array_from_variable(dataset, "lfn_tend_dbg")
    ros = array_from_variable(dataset, "ros")
    expected = np.zeros(gradient.shape, dtype=gradient.dtype)
    valid = (support > 0.5) & (np.abs(gradient) > GRAD_NORM_MIN)
    expected[valid] = -tendency[valid] / gradient[valid] - ros[valid]
    return expected


def check_rate_and_false_band(case_dir: Path, width: int) -> tuple[int, int, int]:
    """Verify initial rate, cellwise rate evolution, false masks, and band geometry."""

    files = output_files(case_dir)
    previous_area: np.ndarray | None = None
    positive_count = 0
    negative_count = 0
    small_nonzero_count = 0

    for file_index, path in enumerate(files):
        with netCDF4.Dataset(path) as dataset:
            require_fields(dataset, REQUIRED_PAIR_FIELDS)
            rate = array_from_variable(dataset, "fire_area_change_rate")
            area = array_from_variable(dataset, "fire_area")
            active = array_from_variable(dataset, "active_front_mask")
            barrier = array_from_variable(dataset, "barrier_contact_front_mask")
            saved_band = array_from_variable(dataset, "band_mask")
            fuel = array_from_variable(dataset, "nfuel_cat")
            ros_error = array_from_variable(dataset, "ros_lfn_error_front")

            if file_index == 0:
                if np.any(rate != 0.0) or np.any(saved_band != 0.0):
                    raise ComparisonFailure(f"Initial rate or band is nonzero in {path}")
            else:
                assert previous_area is not None
                expected_rate = np.ascontiguousarray((area - previous_area) / MODEL_DT)
                difference = first_bitwise_difference(expected_rate, rate)
                if difference is not None:
                    index, expected, actual, expected_hex, actual_hex = difference
                    raise ComparisonFailure(
                        f"Area-rate identity failed: file={path.name}, index={index}, "
                        f"expected={expected!r} [{expected_hex}], actual={actual!r} [{actual_hex}]"
                    )

                seed = np.abs(rate) > 0.0
                burnable = fuel.astype(np.int64) != 14
                expected_band = four_neighbor_growth(seed, burnable, width).astype(saved_band.dtype)
                require_bitwise_equal(
                    expected_band,
                    saved_band,
                    f"False-mode band reconstruction failed: file={path.name}",
                )

            if np.any(active != 0.0) or np.any(barrier != 0.0):
                raise ComparisonFailure(f"False mode retained a nonzero exact mask in {path}")
            require_bitwise_equal(
                expected_ros_lfn_error(dataset, saved_band),
                ros_error,
                f"False-mode ROS-error formula failed: file={path.name}",
            )

            positive_count += int(np.count_nonzero(rate > 0.0))
            negative_count += int(np.count_nonzero(rate < 0.0))
            small_nonzero_count += int(np.count_nonzero((np.abs(rate) > 0.0) & (np.abs(rate) <= 1.0e-7)))
            previous_area = area

    return positive_count, negative_count, small_nonzero_count


def check_true_diagnostics(
    case_dir: Path,
    width: int,
    *,
    use_pre_reinit_support: bool = True,
) -> None:
    """Verify exact final masks, exact-band geometry, and pre-reinit ROS support."""

    for file_index, path in enumerate(output_files(case_dir)):
        with netCDF4.Dataset(path) as dataset:
            require_fields(dataset, SPECIALIZED_FIELDS)
            active = array_from_variable(dataset, "active_front_mask")
            barrier = array_from_variable(dataset, "barrier_contact_front_mask")
            saved_band = array_from_variable(dataset, "band_mask")
            ros_error = array_from_variable(dataset, "ros_lfn_error_front")

            if file_index == 0:
                for name, field in (
                    ("active_front_mask", active),
                    ("barrier_contact_front_mask", barrier),
                    ("band_mask", saved_band),
                    ("ros_lfn_error_front", ros_error),
                ):
                    if np.any(field != 0.0):
                        raise ComparisonFailure(f"Initial true-mode field {name} is nonzero in {path}")
                continue

            fuel = array_from_variable(dataset, "nfuel_cat")
            final_lfn = array_from_variable(dataset, "lfn")
            expected_active, expected_barrier, expected_band = exact_masks_and_band(
                final_lfn,
                fuel,
                width,
            )
            for name, expected, actual in (
                ("active_front_mask", expected_active.astype(active.dtype), active),
                ("barrier_contact_front_mask", expected_barrier.astype(barrier.dtype), barrier),
                ("band_mask", expected_band.astype(saved_band.dtype), saved_band),
            ):
                require_bitwise_equal(
                    expected,
                    actual,
                    f"True-mode exact diagnostic failed: file={path.name}, field={name}",
                )

            support = expected_active
            if use_pre_reinit_support:
                pre_lfn = array_from_variable(dataset, "lfn_pre_reinit_dbg")
                support, _, _ = exact_masks_and_band(pre_lfn, fuel, width)
            require_bitwise_equal(
                expected_ros_lfn_error(dataset, support),
                ros_error,
                f"True-mode ROS-error formula failed: file={path.name}",
            )


def check_call_count(case_dir: Path, expected: int) -> None:
    """Verify every reported exact-connectivity call count for one case."""

    log_path = case_dir / "run.log"
    if not log_path.is_file():
        raise FileNotFoundError(f"Missing run log: {log_path}")
    values = [int(value) for value in CALL_COUNT_PATTERN.findall(log_path.read_text(encoding="utf-8"))]
    if len(values) != EXPECTED_ADVANCES or set(values) != {expected}:
        raise ComparisonFailure(
            f"Expected {EXPECTED_ADVANCES} exact-call reports with value {expected} in "
            f"{log_path}; found {values}"
        )


def check_output_gate(case_dir: Path) -> None:
    """Verify the complete specialized field set is absent when output is disabled."""

    for path in output_files(case_dir):
        with netCDF4.Dataset(path) as dataset:
            unexpected = sorted(SPECIALIZED_FIELDS & set(dataset.variables))
            if unexpected:
                raise ComparisonFailure(f"Output gate failed in {path}: {', '.join(unexpected)}")


#--------------------------------------------------------------------------------
# Matrix checks
#--------------------------------------------------------------------------------


def selected_cases(requested: list[str] | None) -> list[str]:
    """Resolve optional matrix selection for comparison."""

    if requested is None:
        return list(MATRIX_CASES)
    unknown = sorted(set(requested) - set(MATRIX_CASES))
    if unknown:
        raise ValueError(f"Unknown matrix case(s): {', '.join(unknown)}")
    return [name for name in MATRIX_CASES if name in requested]


def check_complete_request(args: argparse.Namespace, cases: list[str], serial_root: Path) -> None:
    """Reject a final-validation request that omits a mandatory check."""

    if not args.require_complete:
        return
    if cases != list(MATRIX_CASES):
        raise ComparisonFailure("--require-complete requires the complete seven-case matrix")
    if not args.check_mpi:
        raise ComparisonFailure("--require-complete requires --check-mpi")
    if args.pre_refactor_root is None or args.godunov_reference_root is None:
        raise ComparisonFailure("--require-complete requires both historical reference roots")
    special_cases = ("band_width1", "callcount_true_noreinit", "gate_devel0", "gate_output0")
    missing_special = [name for name in special_cases if not (serial_root / name).is_dir()]
    if missing_special:
        raise ComparisonFailure(
            f"--require-complete is missing serial special cases: {', '.join(missing_special)}"
        )


def main() -> None:
    """Run paired, semantic, reference, and optional serial/MPI comparisons."""

    args = parse_args()
    root = args.run_root.expanduser().resolve()
    serial_root = root / "serial"
    cases = selected_cases(args.cases)
    check_complete_request(args, cases, serial_root)

    for case in cases:
        false_dir = serial_root / case / "active_false"
        true_dir = serial_root / case / "active_true"
        check_run_log(false_dir)
        check_run_log(true_dir)
        false_provenance = provenance(false_dir)
        true_provenance = provenance(true_dir)
        for key in ("source_revision", "executable_sha256", "geo_em.d01.nc_sha256", "wrf.nc_sha256"):
            if false_provenance.get(key) != true_provenance.get(key):
                raise ComparisonFailure(
                    f"Paired provenance differs for {case}, key={key}: "
                    f"false={false_provenance.get(key)!r}, true={true_provenance.get(key)!r}"
                )
        if false_provenance["execution_mode"] != "serial" or true_provenance["execution_mode"] != "serial":
            raise ComparisonFailure(f"Paired serial execution mode is incorrect for {case}")
        if false_provenance["mpi_ranks"] != "1" or true_provenance["mpi_ranks"] != "1":
            raise ComparisonFailure(f"Paired serial rank provenance is incorrect for {case}")
        with netCDF4.Dataset(output_files(false_dir)[0]) as dataset:
            require_fields(dataset, REQUIRED_PAIR_FIELDS)
        compare_case_dirs(
            false_dir,
            true_dir,
            ignored_fields=ALLOWED_DIAGNOSTIC_DIFFERENCES | UNDEFINED_OFFLINE_FIELDS,
            skip_initial_fields=INITIAL_PREADVANCE_UNDEFINED_FIELDS,
        )
        counts = check_rate_and_false_band(false_dir, width=4)
        check_true_diagnostics(true_dir, width=4)
        check_call_count(false_dir, expected=0)
        check_call_count(true_dir, expected=2)
        print(f"PASS paired {case}: bitwise numerical fields; rate counts positive/negative/small={counts}")

    if (serial_root / "band_width1").exists():
        special_dir = serial_root / "band_width1" / "active_false"
        check_run_log(special_dir)
        counts = check_rate_and_false_band(special_dir, width=1)
        print(f"PASS band_width1: exact seed geometry; rate counts positive/negative/small={counts}")
    else:
        print("SKIP band_width1: case directory not supplied")
    if (serial_root / "callcount_true_noreinit").exists():
        special_dir = serial_root / "callcount_true_noreinit" / "active_true"
        check_run_log(special_dir)
        check_true_diagnostics(special_dir, width=4, use_pre_reinit_support=False)
        check_call_count(special_dir, expected=1)
        print("PASS exact-call path: true mode without reinitialization reports one call")
    else:
        print("SKIP exact one-call path: case directory not supplied")
    for gate_case in ("gate_devel0", "gate_output0"):
        gate_dir = serial_root / gate_case / "active_false"
        if gate_dir.exists():
            check_run_log(gate_dir)
            check_output_gate(gate_dir)
            print(f"PASS output gate {gate_case}")
        else:
            print(f"SKIP output gate {gate_case}: case directory not supplied")

    if args.pre_refactor_root is not None:
        pre_root = args.pre_refactor_root.expanduser().resolve()
        check_pre_refactor_receipt(pre_root, serial_root)
        for case in PRE_REFACTOR_CASES:
            pre_dir = pre_root / case / "baseline_false"
            check_run_log(pre_dir)
            pre_provenance = provenance(pre_dir)
            if pre_provenance["source_revision"] != EXPECTED_PRE_REFACTOR_REVISION:
                raise ComparisonFailure(f"Unexpected pre-refactor source revision in {pre_dir}")
            serial_provenance = provenance(serial_root / case / "active_false")
            for key in ("geo_em.d01.nc_sha256", "wrf.nc_sha256"):
                if pre_provenance[key] != serial_provenance[key]:
                    raise ComparisonFailure(f"Pre-refactor input provenance differs for {case}, key={key}")
            compare_case_dirs(
                serial_root / case / "active_false",
                pre_dir,
                fields=REFERENCE_FIELDS,
                skip_initial_fields=INITIAL_PREADVANCE_UNDEFINED_FIELDS,
            )
            print(f"PASS pre-refactor reference {case}")
    else:
        print("SKIP pre-refactor references: --pre-refactor-root not supplied")

    if args.godunov_reference_root is not None:
        reference_root = args.godunov_reference_root.expanduser().resolve()
        check_reference_manifest(reference_root, serial_root)
        for case in GODUNOV_REFERENCE_CASES:
            compare_case_dirs(
                serial_root / case / "active_false",
                reference_root / case / "baseline_false",
                fields=REFERENCE_FIELDS,
                skip_initial_fields=INITIAL_PREADVANCE_UNDEFINED_FIELDS,
            )
            print(f"PASS 27cd977 Godunov-RS reference {case}")
    else:
        print("SKIP 27cd977 Godunov-RS references: --godunov-reference-root not supplied")

    if args.check_mpi:
        mpi_root = root / "mpi"
        compared_mpi_cases = 0
        for case in cases:
            case_root = mpi_root / case
            if not case_root.exists():
                continue
            variants = ("active_false", "active_true")
            missing_variants = [variant for variant in variants if not (case_root / variant).is_dir()]
            if missing_variants:
                raise ComparisonFailure(
                    f"Incomplete MPI pair for {case}: missing {', '.join(missing_variants)}"
                )
            for variant in variants:
                mpi_dir = mpi_root / case / variant
                check_run_log(mpi_dir)
                mpi_provenance = provenance(mpi_dir)
                if mpi_provenance.get("mpi_ranks") != "4":
                    raise ComparisonFailure(
                        f"Expected four MPI ranks in {mpi_dir}; found {mpi_provenance.get('mpi_ranks')!r}"
                    )
                if mpi_provenance["execution_mode"] != "mpi":
                    raise ComparisonFailure(f"Expected MPI execution mode in {mpi_dir}")
                serial_provenance = provenance(serial_root / case / variant)
                for key in (
                    "source_revision",
                    "namelist_sha256",
                    "geo_em.d01.nc_sha256",
                    "wrf.nc_sha256",
                ):
                    if mpi_provenance[key] != serial_provenance[key]:
                        raise ComparisonFailure(
                            f"Serial/MPI provenance differs for {case}/{variant}, key={key}"
                        )
                compare_case_dirs(
                    serial_root / case / variant,
                    mpi_dir,
                    ignored_fields=UNDEFINED_OFFLINE_FIELDS,
                    skip_initial_fields=INITIAL_PREADVANCE_UNDEFINED_FIELDS,
                )
                print(
                    f"PASS serial/MPI {case}/{variant}: all initialized fields except the "
                    "standalone-driver fz0 field are bitwise identical"
                )
            compared_mpi_cases += 1
        if compared_mpi_cases == 0:
            raise ComparisonFailure(f"--check-mpi found no complete MPI case pairs under {mpi_root}")
        print(f"PASS MPI coverage: {compared_mpi_cases} complete four-rank case pair(s)")
    else:
        print("SKIP serial/MPI comparison: --check-mpi not supplied")

    print("All requested active-front regression checks passed")


if __name__ == "__main__":
    main()
