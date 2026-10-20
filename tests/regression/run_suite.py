#!/usr/bin/env python3
# Created on 2026-10-02. Developed by the CFBM development team.
# run python -B tests/regression/regression.py suite --help
"""Select exact CTest labels and compare executions of the same experiment."""
from __future__ import annotations

import json
import os
from pathlib import Path
import subprocess
import tempfile
from typing import Any

from compare_outputs import compare_directories
from config import load_yaml
from reports import print_result, write_result
from reference import scientific_settings

#--------------------------------------------------------------------------------
# CTest selection and execution comparisons
#--------------------------------------------------------------------------------


def select_tests(tests: list[dict[str, Any]], args: Any) -> list[int]:
    """Select CTest indices with exact names; users supply no regular expressions."""
    indices = []
    for index, test in enumerate(tests, 1):
        labels = set(
            next((prop["value"]
                  for prop in test["properties"]
                  if prop["name"] == "LABELS"), []))
        if args.test and test["name"] != args.test:
            continue
        if not args.test and args.suite not in labels:
            continue
        if any(value and f"{key}:{value}" not in labels
               for key, value in (("case", args.case), ("driver", args.driver),
                                  ("execution", args.execution),
                                  ("configuration",
                                   args.configuration), ("scale", args.scale))):
            continue
        indices.append(index)
    return indices


def compare_pair(reference: dict[str, Any], test: dict[str, Any],
                 static_fields: set[str]) -> dict[str, Any]:
    """Compare equivalent runs, accounting for atmospheric remapping across drivers."""
    drivers = {reference["driver"], test["driver"]}
    cross_driver = drivers in ({"standalone", "nuopc"}, {"standalone", "esmx"})
    context = {
        "reference":
            reference["directory"],
        "test":
            test["directory"],
        "reference_driver":
            reference["driver"],
        "test_driver":
            test["driver"],
        "comparison_kind":
            "cross_driver" if cross_driver else "execution_layout",
    }
    if (scientific_settings(reference["spec"]) != scientific_settings(
            test["spec"]) or
            reference["expected_outputs"] != test["expected_outputs"]):
        return {
            **context, "pass":
                False,
            "reasons": [
                "Cannot compare runs with different scientific settings or output times"
            ]
        }

    # Roughness is static in time but interpolated differently by standalone
    # and ESMF. Apply the existing rtol=1e-4, atol=0 only for this mapped field.
    # Coordinates, terrain, fuel categories, and same-driver static fields
    # retain exact checks; layout comparisons do not receive this exception.
    exact_fields = static_fields - {"fz0"} if cross_driver else static_fields
    comparison = compare_directories(Path(reference["directory"]),
                                     Path(test["directory"]),
                                     reference["expected_outputs"],
                                     exact_fields)
    comparison.update(context)
    return comparison


def compare_executions(results: list[dict[str, Any]],
                       static_fields: set[str]) -> list[dict[str, Any]]:
    """Compare execution layouts and each coupled run with matching standalone output."""
    groups = {}
    comparisons = []
    for result in results:
        if not result["pass"]:
            continue
        spec = result["spec"]
        key = (result["driver"], spec["identity"]["case"],
               spec["identity"]["scale"], spec["identity"]["configuration"])
        groups.setdefault(key, []).append(result)
    for group in groups.values():
        # Prefer serial, then the smallest rank/thread layout available.
        group.sort(key=lambda item: (item["spec"]["execution"][
            "build"] != "serial", item["spec"]["execution"]["ranks"], item[
                "spec"]["execution"]["threads"]))
        comparisons.extend(
            compare_pair(group[0], test, static_fields) for test in group[1:])
    for key, group in groups.items():
        if key[0] not in ("nuopc", "esmx"):
            continue
        standalone = groups.get(("standalone", *key[1:]))
        if standalone:
            comparisons.extend(
                compare_pair(standalone[0], test, static_fields)
                for test in group)
    return comparisons


def run_suite(args: Any) -> dict[str, Any]:
    """Run selected CTests from existing build trees, then compare their results."""
    args.run_root.mkdir(parents=True, exist_ok=True)
    root = Path(tempfile.mkdtemp(prefix=args.suite + "-",
                                 dir=args.run_root)).resolve()
    summary = {"name": args.suite, "pass": False, "reasons": [], "builds": []}
    environment = dict(os.environ, CFBM_RUN_ROOT=str(root))
    selected_count = 0
    for build in args.build_dir:
        command = ["ctest", "--test-dir", str(build.resolve() / "tests")]
        listing = json.loads(
            subprocess.check_output([*command, "--show-only=json-v1"],
                                    text=True))
        indices = select_tests(listing["tests"], args)
        if not indices:
            continue
        selected_count += len(indices)
        # CTest's numeric list works with CMake 3.20 and avoids regex escaping.
        command += [
            "--output-on-failure", "--no-tests=error", "-I",
            "0,0,0," + ",".join(map(str, indices))
        ]
        if os.environ.get("GITHUB_ACTIONS"):
            command += [
                "--output-junit",
                str(root / f"ctest-{len(summary['builds'])}.xml")
            ]
        process = subprocess.run(command, env=environment, check=False)
        summary["builds"].append({
            "directory": str(build),
            "tests": len(indices),
            "exit_status": process.returncode
        })
        if process.returncode:
            summary["reasons"].append(f"CTest failed in {build}")
    if not selected_count:
        summary["reasons"].append("No CTest matched the requested names")
    results = [
        json.loads(path.read_text())
        for path in sorted(root.glob("*/result.json"))
    ]
    if args.suite in ("quick", "pr", "full") and not args.test and not results:
        summary["reasons"].append(
            "No generated cases ran; full requires CFBM_FULL_TESTS=ON")
    if any(not result["pass"] for result in results):
        summary["reasons"].append("One or more case checks failed")
    summary["cases"] = [{
        "name": result["name"],
        "pass": result["pass"],
        "directory": result["directory"]
    } for result in results]
    comparisons = compare_executions(
        results, set(load_yaml(args.config)["static_fields"]))
    summary["comparisons"] = comparisons
    if any(not comparison["pass"] for comparison in comparisons):
        summary["reasons"].append("Cross-execution comparison failed")
    summary["pass"] = not summary["reasons"]
    write_result(root / "summary.json", summary)
    print_result(summary, root / "summary.json")
    return summary
