# Legacy model tests

These fixtures and scripts preserve the original text comparisons. The
registered cases are `test7`, `test8`, `test7esmf`, `test8esmf`, `test7esmx`,
`test8esmx`, and `testx`, depending on build capabilities. Their original input
files and numerical reference text have not changed during this relocation.

From the repository root, inside a compute allocation:

```bash
TEST="/glade/derecho/scratch/$USER/tmp"
./compile_legacy.sh --esmx --test \
    --build-dir="$TEST/build/legacy" \
    --prefix="$TEST/install/legacy"
```

`run_legacy.sh` stages the historical `tests/` and `install/` relative layout
in a fresh `BUILD_DIR/legacy-runs/<test>-<unique>/` directory. It replaces
cleanup lines in the staged script with no-ops, preserving outputs even after
failure. CTest keeps its original test names. Select names from the parent
`BUILD_DIR/tests` directory, which discovers this subdirectory automatically.
Do not execute the historical `.s` scripts directly in the source tree.
The older `run_any_test.sh` and `run_esmx_tests.sh` are preserved for provenance
and are not the supported invocation route.

See [new and legacy coverage](../regression/doc/advantages.md) and
[known validation results](../regression/doc/validation.md). In particular,
`testx` exercises the ESMX_Data feedback fixture, which generated WRF-data
cases do not replace.
