#!/usr/bin/env python3
# Created on 2026-10-02. Developed by the CFBM development team.
# run python -B tests/regression/regression.py suite --help
"""Plain named interface to CFBM case execution, CTest, and references."""
from __future__ import annotations

import argparse
import json
from pathlib import Path

from config import load_yaml, registrations

DEFAULT_CONFIG = Path(__file__).with_name("cases.yaml")

#--------------------------------------------------------------------------------
# Command-line interface
#--------------------------------------------------------------------------------


def build_parser() -> argparse.ArgumentParser:
    """Describe the public commands without embedding workflow logic."""
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    case = commands.add_parser("case", help="Run one already-built executable")
    case.add_argument("--case", required=True)
    case.add_argument("--driver",
                      choices=("standalone", "nuopc", "esmx"),
                      default="standalone")
    case.add_argument("--execution", default="serial")
    case.add_argument("--scale", choices=("small", "large"), default="small")
    case.add_argument(
        "--configuration",
        help="Named configuration; defaults to the first entry for the case")
    case.add_argument("--executable", type=Path, required=True)
    case.add_argument("--run-root", type=Path, required=True)
    case.add_argument("--launcher", default="mpiexec")
    case.add_argument("--launcher-arg", action="append", default=[])
    case.add_argument("--launcher-post-arg", action="append", default=[])
    case.add_argument("--process-flag", default="-n")
    case.add_argument("--reference", type=Path)
    suite = commands.add_parser("suite",
                                help="Select and run registered CTests")
    suite.add_argument("--build-dir", type=Path, action="append", required=True)
    suite.add_argument("--suite",
                       choices=("quick", "pr", "full", "unit", "legacy"),
                       default="quick")
    suite.add_argument("--case")
    suite.add_argument("--driver", choices=("standalone", "nuopc", "esmx"))
    suite.add_argument("--execution")
    suite.add_argument("--configuration")
    suite.add_argument("--scale", choices=("small", "large"))
    suite.add_argument("--test", help="One exact CTest name")
    suite.add_argument("--run-root", type=Path, required=True)
    register = commands.add_parser("registrations",
                                   help="Emit CMake's test definitions")
    register.add_argument("--variant",
                          choices=("serial", "omp", "mpi", "hybrid"),
                          required=True)
    register.add_argument("--driver", action="append", default=["standalone"])
    register.add_argument("--include-full", action="store_true")
    reference = commands.add_parser(
        "reference-create",
        help="Save an unapproved reference from a passing suite")
    reference.add_argument("--suite-root", type=Path, required=True)
    reference.add_argument("--destination", type=Path, required=True)
    for command in (case, suite, register):
        command.add_argument("--config", type=Path, default=DEFAULT_CONFIG)
    return parser


def main() -> int:
    """Dispatch one command to the module responsible for that operation."""
    args = build_parser().parse_args()
    if args.command == "registrations":
        records = registrations(load_yaml(args.config), args.variant,
                                args.driver)
        if not args.include_full:
            records = [
                record for record in records if record["scale"] != "large"
            ]
        print(json.dumps(records))
        return 0
    if args.command == "case":
        from run_case import run_case
        return 0 if run_case(args)["pass"] else 1
    if args.command == "suite":
        from run_suite import run_suite
        return 0 if run_suite(args)["pass"] else 1
    from reference import create_reference
    create_reference(args.suite_root, args.destination)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
