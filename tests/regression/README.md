# CFBM regression tests

Build with `compile.sh`, then run named suites through CTest. Python generates
inputs, checks the saved fields, and compares runs with the same scientific
settings. Standalone serial, OpenMP, MPI, hybrid MPI/OpenMP, NUOPC, and ESMX
use the same interface.

## Start here

Run these commands from the repository root, in a compute allocation on NCAR
systems. Select Python 3.11 or newer with NumPy, netCDF4, and PyYAML. The
[run guide](doc/running.md) explains the shared NCAR environment and MPI setup.
By default, `compile.sh` uses `build/` and `install/` inside the checkout;
both are ignored by Git. Explicit paths remain available for HPC campaigns.

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
[Derecho PBS template](submit_derecho.pbs). It loads the GNU environment, uses
Strict builds, and checks the approved GitHub baseline. Its run directory is
`$TEST/cfbm_<shortCommitHash>_<YYYYMMDD>`.
See the [execution overview](doc/structure.md#execution-overview) for the
difference between one `compile.sh` call and the complete four-build matrix.

## Documentation

| Task | Guide |
| --- | --- |
| Build, submit a PBS job, select suites or exact names | [Running tests](doc/running.md) |
| Select exact namelist options and named configurations | [YAML configuration](doc/configuration.md) |
| Understand terrain, fuel, ignition, winds, and timing | [Case configurations and figures](doc/cases.md) |
| Follow the build/run flow and connections between files | [Execution overview and file relationships](doc/structure.md) |
| Find every compared field and its acceptance rule | [Variable comparison table](doc/comparison.md) |
| Compare current coverage with the retired tests | [Current and retired tests](doc/advantages.md) |
| Find outputs or prepare an unapproved reference | [Results and references](doc/references.md) |
| Add or extend a case, namelist option, or output | [Contributor guide](doc/contributing.md) |
| Prepare a model or regression change for review | [PR checklist](doc/pr-checklist.md) |
| Find postponed investigations and unsupported coverage | [Deferred work](doc/deferred.md) |
| Interpret existing passes, failures, and coverage limits | [Validation history](doc/validation.md) |

## Scope and acceptance

The three cases are `circle`, `fuels`, and `terrain`. Terrain has `u10m` and
`u3d` configurations, with both wind representations in its generated inputs.
The `small` scale integrates for 60 s on a 72 × 72 fire grid at 100 m spacing
with a 4 s timestep. Terrain also runs through NUOPC and ESMX with MPI and
hybrid MPI/OpenMP. The complete PR matrix has 36 runs and 40 comparisons. See the
[YAML configuration guide](doc/configuration.md) for exact namelist options,
configuration names, and the `large` scale.
GNU `Strict` builds select the shared `github-pr` baseline by default.
Use `--reference` to override it or `--no-reference` to disable historical
comparison deliberately. Configuration warns when no reference is selected.
The [comparison table](doc/comparison.md) separates bitwise checks from numerical
tolerance. Passing without an approved reference establishes the configured
behavior checks and agreement among selected executions, not historical
regression acceptance.

The standalone and coupled pathways map 3D profiles horizontally before
sampling winds at 6.096 m. Standalone bilinear interpolation uses double
precision for weights and summation. The PR suite keeps `fire_upwinding=9`
and uses the 60 s endpoint for acceptance. Intermediate-time and longer-run
experiments are separate diagnostics, documented in the
[validation history](doc/validation.md), rather than additional PR-suite gates.

The legacy test scripts, fixtures, and build route have been retired. Their
source and reference data remain in [Git history](https://github.com/NCAR/fire_behavior/tree/26d57b64d9e5b9356f791d63e0c2461d5e67308a/tests/legacy).
The former `testx` ESMX_Data configuration is not covered by the generated
cases. Its replacement and numerical checks of received feedback fields are
tracked in [issue #58](https://github.com/NCAR/fire_behavior/issues/58).
Coverage limits and postponed investigations are recorded in
[deferred work](doc/deferred.md).

## Python style

Use Google-based YAPF formatting, four-space indentation, an 80-column target,
descriptive names, blank lines between logical operations, and comments explaining
scientific choices. YAPF is a development dependency, not a runtime dependency.
The [contributor guide](doc/contributing.md#style-and-python-checks) gives the
formatting and unit-test commands. Agents must also read [AGENTS.md](AGENTS.md).
