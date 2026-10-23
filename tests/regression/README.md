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
| Check continuous versus restarted integrations | [Restart continuity](doc/restart.md) |
| Run an evolving WRF atmosphere with generated terrain inputs | [WRF-host tests](doc/wrf.md) |
| Distinguish CTest invocations, model runs, and checkpoint checks | [How test counts are reported](#how-test-counts-are-reported) |
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
with a 4 s timestep. Terrain also runs through NUOPC and ESMX with MPI and
hybrid MPI/OpenMP. Fractional-step terrain configurations extend the complete
PR matrix to 72 runs and 85 execution comparisons. The original 36 cases retain
their approved-reference comparisons. See the
[YAML configuration guide](doc/configuration.md) for exact namelist options,
configuration names, and the `large` scale.
GNU `Strict` builds select the shared `github-pr` baseline by default.
Use `--reference` to override it or `--no-reference` to disable historical
comparison deliberately. Configuration warns when no reference is selected.

GitHub CI runs both `pr` and `restart` using the same four builds; either suite
failing makes the regression check fail. The restart suite compares uninterrupted
and segmented runs from the same revision and needs no stored baseline. Both
suites' reports and model files are retained in the test-evidence artifact.
The [comparison table](doc/comparison.md) separates bitwise checks from numerical
tolerance. Passing without an approved reference establishes the configured
behavior checks and agreement among selected executions, not historical
regression acceptance.

The standalone and coupled pathways map 3D profiles horizontally before
sampling winds at 6.096 m. Standalone bilinear interpolation uses double
precision for weights and summation. The PR suite keeps `fire_upwinding=9`
and checks the 60 s endpoint for reference cases. Fractional-step cases check
every scheduled output through 60 or 72 s. Longer-run experiments remain
separate diagnostics, documented in the
[validation history](doc/validation.md), rather than additional PR-suite gates.

The original tests are preserved in [tests/legacy](../legacy/README.md), including
`testx`, whose ESMX_Data feedback configuration is not replaced by the generated
cases. They do not run in CI or in the PBS template. For historical
investigations, use `compile_legacy.sh` with separate build/install directories.
Coverage limits and postponed investigations are recorded in
[deferred work](doc/deferred.md).

See [fractional event schedules](doc/scheduling.md) for continuous and restarted
cases selected by `pr` and `restart`.

## How test counts are reported

The complete four-build selection distinguishes CTest invocations, model
integrations, checkpoint-file checks, and comparisons between files. Selecting
fewer builds or filtering cases changes these totals. Reports retain counts
and errors at each level; an expected-rejection probe is not a failed case
when its diagnostic and absence of output match the specified condition.

### PR checks

The `pr` suite runs the original 36 reference cases and 36 fractional-step
cases. It performs 85 execution-layout/cross-driver comparisons and retains
all 36 approved-reference comparisons. The fractional cases use exact output
inventories, forcing-record checks, and same-revision execution comparisons;
they require no new stored baseline.

| Build | Fortran unit checks | Python harness invocation | Model runs | Total CTests |
| --- | ---: | ---: | ---: | ---: |
| Serial | 53 | 1 | 7 | 61 |
| OpenMP | 53 | 1 | 14 | 68 |
| MPI | 54 | 1 | 34 | 89 |
| Hybrid | 54 | 1 | 17 | 72 |
| **Total** | **214** | **4** | **72** | **290** |

Fortran checks cover timing, namelists, moisture initialization, perimeter
activation, tiling, interpolation, and numerical precision. The clock checks
include long-duration event counts and exchange-window accumulation/reset.

### Restart checks

The `restart` suite contains 30 case-level tests across five execution layouts,
48 moisture-initialization unit invocations (12 per build), and four Python
harness invocations: **82 CTests**. The unit and Python checks also run in `pr`.

Within the 30 cases there are **200 model invocations**: 30 uninterrupted runs,
95 chain segments, 30 moisture-loading probes, five idealized legacy-clock
resumes, 35 rejected schedule/clock probes, and five runs ending between events.
These probes retain separate diagnostics and reports.

The cases check **680 checkpoint files** against the expected field inventory,
finite state, REAL64 fuel storage, clock, perimeter state, and moisture lifecycle.
The original 15 cases check 450 files. The 15 fractional cases add 230: each
layout checks continuous and segmented sets of 8, 9, and 6 checkpoint files.
Final fuel state must contain a signal that would be lost with REAL32 storage.
Moisture bounds `(0,1)` describe these dry-forcing cases, not a general model bound.

The **525 continuity file comparisons** check 185 output files and 340
checkpoints. Five accepted moisture-legacy checkpoints and 40 idealized
legacy-clock checkpoints add 45 comparisons, for **570 comparisons** overall.
Checkpoint physical state is compared exactly; regular output retains its
existing field policies. Independent execution-layout comparisons total 24.
See [restart continuity](doc/restart.md) and [fractional event
schedules](doc/scheduling.md) for the schedules and schema assumptions.

Earlier counts for the integer-step configurations are preserved in the
[validation history](doc/validation.md); they describe their recorded revisions,
not the expanded selection above.

## Python style

Use Google-based YAPF formatting, four-space indentation, an 80-column target,
descriptive names, blank lines between logical operations, and comments explaining
scientific choices. YAPF is a development dependency, not a runtime dependency.
The [contributor guide](doc/contributing.md#style-and-python-checks) gives the
formatting and unit-test commands. Agents must also read [AGENTS.md](AGENTS.md).
