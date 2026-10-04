# Instructions for regression harness changes

Read README.md here before editing. Use the Google-based Python formatting in
.style.yapf, with four-space indentation and an 80-column target. Run the pinned
YAPF check documented in README.md. Keep the formatter out of runtime dependencies.

Use descriptive snake_case names, pathlib paths, and public-function type hints.
Separate logical operations with blank lines. Explain scientific choices and
synthetic test substitutions in comments. A formatter pass does not replace
review for readability.

Do not add personal filesystem paths or require a developer's Conda installation.
Use the module-provided environment documented in README.md on NCAR systems.
CFBM_TEST_TMP is required for direct Python tests; CTest supplies a build-tree
artifact directory. Keep generated files outside the source tree and retain
failure evidence. Use PBS for model/MPI runs and for the shared MPI-enabled
Python environment.

Python test filenames end in _test.py. Use unittest discovery with -p '*_test.py'
and confirm that both Python test files are collected after any rename.
Keep compile.sh responsible for build options, CTest responsible for test
invocation, and cases.yaml responsible for the scientific cases and executions.
Do not change scientific tolerances or legacy references to conceal failures.

Documentation lives in doc/. Keep README.md as the entry point. Update the
case figures and comparison table when scientific settings or outputs change.
Read doc/contributing.md and doc/pr-checklist.md before extending the cases.
Keep historical fixtures in ../legacy/ and preserve their reference data.
