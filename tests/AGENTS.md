# Instructions for tests

These instructions apply to all tests below this directory, including Fortran
unit tests in `unit/` and the Python regression harness in `regression/`.

## Unit-test comments

Each new or modified unit test must include an informative comment explaining
the behavior it checks, why the setup exercises that behavior, and the expected
result or rejection. For files covering several scenarios, summarize the shared
purpose near the top and explain distinct cases where they are defined.
Include relevant scientific assumptions, units, timing, or boundary conditions.
Explain what a failure would mean; avoid comments that merely repeat assertions
or depend on internal planning labels or PR numbers.

## User-facing documentation

Write user-facing test documentation so readers can understand it without
knowing the development history. Describe the scientific behavior, configuration,
or compatibility condition being tested. Do not use internal planning labels
such as P00/P02 or PR numbers as substitutes for those descriptions.

For example, use "restart-loading probes for moisture initialization" instead
of "probes for P02", and "legacy restart metadata" instead of "PR #39 metadata".
Explain which metadata is absent and why it matters when discussing legacy
compatibility; "restart metadata" alone does not identify that distinction.

Keep PR references, planning labels, revisions, and historical validation results
in validation records such as regression/doc/validation.md or session notes.
Preserve that provenance when revising user-facing guides.

## Regression harness

Read [regression/README.md](regression/README.md) before editing the harness.
Use the Google-based Python formatting in `regression/.style.yapf`, with
four-space indentation and an 80-column target. Run the pinned YAPF check
documented in that README. Keep the formatter out of runtime dependencies.

Use descriptive snake_case names, pathlib paths, and public-function type hints.
Separate logical operations with blank lines. Explain scientific choices and
synthetic test substitutions in comments. A formatter pass does not replace
review for readability.

Do not add personal filesystem paths or require a developer's Conda installation.
Use the module-provided environment documented in `regression/README.md` on NCAR
systems. `CFBM_TEST_TMP` is required for direct Python tests; CTest supplies a
build-tree artifact directory. Keep generated files in the repository's ignored
`build/` directory or outside the checkout, and retain failure evidence. Use PBS
for model/MPI runs and for the shared MPI-enabled Python environment.

Python test filenames end in `_test.py`. Use unittest discovery with
`-p '*_test.py'` and confirm that both Python test files are collected after any
rename. Keep the repository's `compile.sh` responsible for build options, CTest
responsible for test invocation, and `regression/cases.yaml` responsible for the
scientific cases and executions. Do not change scientific tolerances or legacy
references to conceal failures.

Documentation lives in `regression/doc/`. Keep `regression/README.md` as the entry
point. Update the case figures and comparison table when scientific settings or
outputs change. Read [contributing.md](regression/doc/contributing.md) and
[pr-checklist.md](regression/doc/pr-checklist.md) before extending the cases.
Keep historical fixtures in `legacy/` and preserve their reference data.

Use exact namelist names in `regression/cases.yaml`. Cases own explicit
configurations; do not add generic option-category expansion. Compare only
matching case, configuration, and scale. No creation or assistance header is
required.
