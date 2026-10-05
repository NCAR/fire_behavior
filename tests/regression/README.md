# CFBM regression tests

Build with `compile.sh`, then run named suites through CTest. Python generates
inputs, checks the saved fields, and compares runs with the same scientific
settings. Standalone serial, OpenMP, MPI, hybrid MPI/OpenMP, NUOPC, and ESMX
use the same interface.

## Start here

Run these commands from the repository root, in a compute allocation on NCAR
systems. Select a Python interpreter with NumPy, netCDF4, and PyYAML. The
[run guide](doc/running.md) explains the shared NCAR environment and MPI setup.

```bash
TEST="/glade/derecho/scratch/$USER/tmp"

./compile.sh --mpi-off \
    --build-dir="$TEST/build/serial" \
    --prefix="$TEST/install/serial"

python -B tests/regression/regression.py suite --suite quick \
    --build-dir="$TEST/build/serial" \
    --run-root="$TEST/results"
```

Use fresh build/install directories for a new configuration. This example tests
only the serial build. For all four builds, including NUOPC and ESMX, use the
[Derecho PBS template](submit_derecho.pbs). Its run directory is
`$TEST/cfbm_<shortCommitHash>_<YYYYMMDD>`.

## Documentation

| Task | Guide |
| --- | --- |
| Build, submit a PBS job, select suites or exact names | [Running tests](doc/running.md) |
| Select exact namelist options and named configurations | [YAML configuration](doc/configuration.md) |
| Understand terrain, fuel, ignition, winds, and timing | [Case configurations and figures](doc/cases.md) |
| Understand which script does what | [Structure and execution diagrams](doc/structure.md) |
| Find every compared field and its acceptance rule | [Variable comparison table](doc/comparison.md) |
| Understand improvements and remaining legacy coverage | [New and legacy tests](doc/advantages.md) |
| Find outputs or prepare an unapproved reference | [Results and references](doc/references.md) |
| Add or extend a case, namelist option, or output | [Contributor guide](doc/contributing.md) |
| Prepare a model or regression change for review | [PR checklist](doc/pr-checklist.md) |
| Find postponed investigations and unsupported coverage | [Deferred work](doc/deferred.md) |
| Interpret existing passes, failures, and coverage limits | [Validation history](doc/validation.md) |

## Scope and acceptance

The three cases are `circle`, `fuels`, and `terrain`. Terrain has `u10m` and
`u3d` configurations, with both wind representations in its generated inputs.
The `small` scale integrates for 60 s on a 72 × 72 fire grid at 100 m spacing
with a 4 s timestep. Terrain also runs through NUOPC and ESMX. See the
[YAML configuration guide](doc/configuration.md) for exact namelist options,
configuration names, and the `large` scale.
The [comparison table](doc/comparison.md) separates bitwise checks from numerical
tolerance. Passing without an approved reference establishes the configured
behavior checks and agreement among selected executions, not historical
regression acceptance.

PR #57 aligns the standalone and coupled 3D wind-interpolation order. Its
60 s terrain comparisons pass at the requested 6.096 m height, but a separate
every-4-second diagnostic still exceeds tolerance in one level-set cell at
20 s. See the [validation record](doc/validation.md#wind-interpolation-order)
for the combined harness results and remaining limits.

The original tests are preserved in [tests/legacy](../legacy/README.md), including
`testx`, whose ESMX_Data feedback configuration is not replaced by the generated
cases. Use `compile_legacy.sh` with separate build/install directories.
Coverage limits and postponed investigations are recorded in
[deferred work](doc/deferred.md).

## Python style

Use Google-based YAPF formatting, four-space indentation, an 80-column target,
descriptive names, blank lines between logical operations, and comments explaining
scientific choices. YAPF is a development dependency, not a runtime dependency.
The [contributor guide](doc/contributing.md#style-and-python-checks) gives the
formatting and unit-test commands. Agents must also read [AGENTS.md](AGENTS.md).
