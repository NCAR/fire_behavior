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
hybrid MPI/OpenMP. The complete PR matrix has 36 runs and 40 comparisons. See the
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
and uses the 60 s endpoint for acceptance. Intermediate-time and longer-run
experiments are separate diagnostics, documented in the
[validation history](doc/validation.md), rather than additional PR-suite gates.

The original tests are preserved in [tests/legacy](../legacy/README.md), including
`testx`, whose ESMX_Data feedback configuration is not replaced by the generated
cases. They do not run in CI or in the PBS template. For historical
investigations, use `compile_legacy.sh` with separate build/install directories.
Coverage limits and postponed investigations are recorded in
[deferred work](doc/deferred.md).

## How test counts are reported

The counts below describe the complete four-build `pr` and `restart` selections
validated at `5de15ed` in [CI run 39970138677](https://github.com/NCAR/fire_behavior/actions/runs/39970138677).
They count different levels of work: CTest invocations, model integrations,
individual checkpoint-file checks, and comparisons between files. Selecting
fewer builds or filtering cases changes these totals. When registrations or
schedules change, update this breakdown from the suite summaries, per-build
CTest/JUnit reports, and each case's `restart.json`.

### 450 checkpoint-validity checks

These are checks of **450 checkpoint files** across 15 restart cases. Each case
writes checkpoints every 4 s, from 4 through 60 s:

- 15 files from the uninterrupted run.
- 15 files produced across the segmented run.
- Total: **15 cases × 30 files = 450 checkpoint-file checks**.

Each file is checked for required fields, finite/unmasked values, REAL64 fuel
storage, correct dates and timestep count, and the expected perimeter-activation
flag. The final checkpoint also checks for precision that would be lost if
fuel state were reduced to REAL32. Evolving-moisture cases additionally check
initialization status, update cadence and times, five-class dimensions, and
moisture bounds appropriate to these dry-forcing cases. The class-moisture
range `(0,1)` is specific to these cases, not a general bound on fuel moisture.
The checks are implemented in [check_checkpoint](restart.py).

These checks test each file against the expected checkpoint contract. The
separately reported **320 file comparisons** compare continuous and restarted
results: 90 regular-output comparisons, 225 checkpoint comparisons, and five
accepted-legacy checkpoint comparisons. Each checkpoint comparison requires
exact physical-state agreement. The 450 validity checks run inside the 15
restart cases; they are not 450 additional CTest invocations. See
[restart continuity](doc/restart.md) for field applicability and comparison rules.

### 238 PR CTest invocations

The `pr` suite executes the following registered CTests across four builds:

| Build | Fortran unit checks | Python harness invocation | Model runs | Total |
| --- | ---: | ---: | ---: | ---: |
| Serial | 49 | 1 | 4 | 54 |
| OpenMP | 49 | 1 | 8 | 58 |
| MPI | 50 | 1 | 16 | 67 |
| Hybrid | 50 | 1 | 8 | 59 |
| **Total** | **198** | **4** | **36** | **238** |

The [Fortran unit checks](../unit/CMakeLists.txt) cover namelist validation,
timing, tiles, perimeter activation, numerical precision, wind interpolation,
and moisture initialization. MPI and hybrid add the namelist-broadcast check.

Each `regression_python` invocation runs **45 Python tests**, but CTest counts
that invocation as one entry. Many unit checks are repeated across builds to
exercise the corresponding compiled configuration. Thus 238 is a count of
CTest invocations, not 238 distinct scientific scenarios. The 40 cross-execution
comparisons and 36 approved-reference comparisons are reported separately from
these invocation counts.

### 67 restart CTest invocations

The `restart` suite is separate from the 238 PR invocations. It contains 15
restart cases, 48 moisture-initialization unit invocations (12 in each build),
and four Python harness invocations: **15 + 48 + 4 = 67**. The unit and Python
checks overlap with `pr` and run again when both suites are selected.

Each restart case invokes the model several times. Together they run 15
uninterrupted integrations, 50 chain segments, and 30 restart-loading probes:
**95 model invocations** inside the 15 case-level CTests. The loading probes
include five accepted legacy files and 25 expected rejections of ambiguous or
damaged checkpoints. An expected rejection passes only when its diagnostic
and absence of regular output match the test requirements.

## Python style

Use Google-based YAPF formatting, four-space indentation, an 80-column target,
descriptive names, blank lines between logical operations, and comments explaining
scientific choices. YAPF is a development dependency, not a runtime dependency.
The [contributor guide](doc/contributing.md#style-and-python-checks) gives the
formatting and unit-test commands. Agents must also read [AGENTS.md](AGENTS.md).
