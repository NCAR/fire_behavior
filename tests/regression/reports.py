#!/usr/bin/env python3
# Created on 2026-10-02. Developed by the CFBM development team.
# run python -B tests/regression/regression.py --help
"""Write one JSON result per case and a concise suite summary."""
from __future__ import annotations

import json
from pathlib import Path
from typing import Any

#--------------------------------------------------------------------------------
# Result reports
#--------------------------------------------------------------------------------


def write_result(path: Path, result: dict[str, Any]) -> None:
    """Save structured evidence without allowing nonstandard JSON numbers."""
    path.write_text(json.dumps(result, indent=2, allow_nan=False) + "\n",
                    encoding="utf-8")


def print_result(result: dict[str, Any], path: Path) -> None:
    """Print status and the location of the complete diagnostic evidence."""
    status = "PASS" if result["pass"] else "FAIL"
    print(f"{status}: {result.get('name', 'suite')}; result: {path}")
    for reason in result.get("reasons", []):
        print(f"  {reason}")
