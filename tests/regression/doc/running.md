# Running regression tests

[Documentation index](../README.md)

## Run a suite

All commands below start at the repository root. Define the scratch root once:

```bash
TEST="/glade/derecho/scratch/$USER/tmp"
```

Use a Python environment with NumPy, netCDF4, and PyYAML. CI installs the
versions pinned in [requirements.txt](../requirements.txt). On NCAR HPC systems,
run builds and model integrations in a PBS allocation. Use fresh build and
installation directories outside the source checkout. For example:

```bash
./compile.sh --mpi-off --build-dir="$TEST/build/serial" \
    --prefix="$TEST/install/serial"

python -B tests/regression/regression.py suite --suite quick \
    --build-dir="$TEST/build/serial" \
    --run-root="$TEST/results"
```

On Casper, the validated module
combination is `ncarenv/25.10 intel/2025.2.1 openmpi/5.0.8 netcdf/4.9.3
esmf-mpi/8.9.1`. This is a tested environment, not a portability requirement.
For the complete Derecho build, including ESMX, use `ncarenv/25.10
intel/2025.2.1 ncarcompilers/1.1.0 cray-mpich/8.1.32 netcdf-mpi/4.9.3
esmf-mpi/8.9.1 cmake/3.31.8`. The parallel NetCDF module loads matching
parallel HDF5 libraries. Mixing serial NetCDF/HDF5 with this MPI ESMF can
cause unresolved HDF5 symbols when linking ESMX.

The build/launch separation is the same as in the legacy suite. `compile.sh`
chooses whether MPI and OpenMP are compiled in. The case runner launches the
resulting executable with the rank count and `OMP_NUM_THREADS` from the
`executions` section of `cases.yaml`. The model/coupler derives the MPI domain
layout from its communicator; `num_tiles` and `tile_strategy` in the rendered
namelist control the within-rank tiles. Compilation does not select rank counts
or domain decomposition. CI now builds four configurations explicitly so each
parallel mode is exercised.

### Build configurations

| Configuration | `compile.sh` options |
| --- | --- |
| Serial | `--mpi-off` |
| OpenMP | `--mpi-off --openmp-on` |
| MPI | Default |
| MPI and OpenMP | `--openmp-on` |
| NUOPC | `--nuopc` |
| ESMX and NUOPC | `--esmx` |

Pass `--build-dir` more than once to run a suite across those builds and compare
the resulting layouts. Python does not configure or build executables. A build
with NUOPC or ESMX also retains its standalone tests.

## Select tests by name

```bash
python -B tests/regression/regression.py suite --suite pr \
    --case terrain_u3d --driver nuopc --execution mpi4 \
    --build-dir="$TEST/build/nuopc" \
    --run-root="$TEST/results"
```

`--test` selects one exact CTest name. `--suite unit` runs focused tests. No
selection accepts regex syntax. Internally, Python reads CTest's JSON listing
and supplies a numeric list of selected tests to CTest.

`--suite` accepts **five values**. `--test` accepts **any exact CTest name registered in the selected build**, so its available names depend on the build configuration.

The suite options are defined in [regression.py](../regression.py):

| Option | Tests selected |
|---|---|
| `quick` | Default. Focused tests plus the four standard cases in serial, and both terrain cases with `omp4`, `mpi4`, or `hybrid4`, where supported. |
| `pr` | Focused tests plus all four standard cases across `serial`, `omp1`, `omp4`, `mpi1`, `mpi4`, and `hybrid4`, where supported. |
| `full` | Focused tests plus the larger experiments: 320 × 320 grids, 3600 s integrations, both method pairs `(9,4)` and `(2,4)`, and the additional `mpi8` execution. Requires `CFBM_FULL_TESTS=ON`. |
| `unit` | Focused Fortran tests and the Python harness tests. No generated fire-case integrations. |
| `legacy` | Original tests registered by the legacy build route. |

These selections follow [cases.yaml](../cases.yaml). Each build contributes only the executions it supports, such as serial or MPI. **`full` selects only the larger experiments.**

Enable their registration in an appropriately sized allocation:

```bash
cmake -S . -B "$TEST/build/mpi" -DCFBM_FULL_TESTS=ON
```
 They are absent from
ordinary registration. The one-hour PBS template is sized for standard cases,
not the full campaign.

For `--test`, list every available name in a particular build without running tests:

```bash
ctest --test-dir "$TEST/build/mpi/tests" -N
```

The valid names for `--test` fall into three groups:

### Generated model tests

Names follow this convention:

```text
<driver>_<case>_<scale>_<method>_<execution>
```

| Component | Values |
|---|---|
| Driver | `standalone`, `nuopc`, `esmx` |
| Case | `circle_nowind`, `fuel_strip_wind`, `terrain_u10m`, `terrain_u3d` |
| Scale and method | `standard_ref94`, `full_ref94`, `full_ref24` |
| Execution | `serial`, `omp1`, `omp4`, `mpi1`, `mpi4`, `mpi8`, `hybrid4` |

NUOPC and ESMX register only the two terrain cases. `mpi8` is available only for full experiments. Execution choices must match the build: serial, OpenMP, MPI, or hybrid. These rules are implemented in [config.py](../config.py).

Examples:

```text
standalone_circle_nowind_standard_ref94_serial
standalone_terrain_u3d_standard_ref94_omp4
nuopc_terrain_u10m_standard_ref94_mpi4
esmx_terrain_u3d_full_ref24_mpi8
```

### Focused tests

These are unit tests for an existing Fortran program or Python script. They have exact names, as registered in [unit/CMakeLists.txt](../../unit/CMakeLists.txt) and [regression/CMakeLists.txt](../CMakeLists.txt):

```text
regression_python
*_unit
namelist_broadcast_mpi
```

Here `*_unit` describes a naming pattern, not a literal selector. Copy the
complete name from the CTest listing when using `--test`.
`regression_python` tests **the Python regression harness**: input generation, configuration, output checks, comparisons, and reporting. It currently contains **34 Python tests** across two files and does not launch the Fortran model.

| Area | What it checks |
|---|---|
| Configuration | Rejects misspelled settings, duplicate YAML keys, unsupported method pairs, misaligned output times, and scientific overrides hidden in execution settings. |
| Namelist and ESMX configuration | Checks template substitution, wind interpolation selection, MPI PET assignments, and simulation stop time. |
| Generated inputs | Checks reproducibility, fuel categories, perimeter initialization, terrain, forcing records, required NetCDF fields, and double-precision atmospheric coordinates. |
| Wind fields | Checks WRF staggering and vertical heights, terrain-dependent wind scaling, and rejection of incorrect, uniform, excessive, or locally missing winds. |
| Test selection and resources | Checks exact-name selection and CPU accounting for ranks × threads. |
| Numerical comparisons | Exercises relative tolerance, zero references, tolerance boundaries, bitwise static-field checks, NaNs, infinities, missing-data masks, and empty valid-data sets. |
| Comparison policy | Checks cross-driver roughness tolerance, exact fuel categories, comparisons across execution layouts, and rejection of incompatible scientific settings. |
| Failure detection and reporting | Checks rejection of corrupted or unapproved references, extra output files, and a successful process exit with no outputs; also exercises JSON reporting. |

The implementation is in [python_harness_test.py](../tests/python_harness_test.py) (18 tests) and [generator_comparator_test.py](../tests/generator_comparator_test.py) (16 tests).

These tests use generated inputs and small synthetic output files. Passing them supports confidence in the testing infrastructure; validating model integration and scientific output requires the generated model tests.

`namelist_broadcast_mpi` requires an MPI build. `regression_python` is registered by the generated test system and runs both Python test files.

### Legacy tests

The original names are `test7`, `test8`, `test7esmf`, `test8esmf`, `test7esmx`, `test8esmx`, and `testx`, subject to the enabled coupling capabilities. They require the legacy test system. See [legacy instructions](../../legacy/README.md).

## Exact selection overrides the suite

**`--test` overrides the suite-label selection**. For example, `--test regression_python` selects that test even if `--suite` remains at its default, `quick`. Any supplied `--case`, `--driver`, or `--execution` filters still apply. See [run_suite.py](../run_suite.py).

## Shared Python on Casper and Derecho

Both systems provide the centrally maintained `npl-2026a` environment:

```bash
module load conda
conda activate npl-2026a
```

This supplies Python 3.13.11, NumPy 2.3.5, netCDF4 1.7.4, and PyYAML 6.0.3.
The three harness dependencies match the pinned CI versions. No personal Conda
installation or package installation into the shared environment is needed.

For Python-only checks, keep this environment active. For model builds and
integrations, capture its interpreter and deactivate it first: NPL also ships
MPI wrappers and a launcher, which must not replace the model's module-provided
MPI installation. With the model's compiler/MPI/NetCDF/ESMF modules loaded:

```bash
module load conda
conda activate npl-2026a
CFBM_PYTHON=$(command -v python)
conda deactivate

cmake -S . -B "$TEST/build/mpi" -DPython3_EXECUTABLE="$CFBM_PYTHON"
./compile.sh --nuopc --build-dir="$TEST/build/mpi" \
    --prefix="$TEST/install/mpi"
"$CFBM_PYTHON" -B tests/regression/regression.py suite --suite quick \
    --build-dir="$TEST/build/mpi" --run-root="$TEST/results"
```

Use fresh build directories when changing MPI installations. The CMake cache
must identify the model's MPI wrappers/libraries and launcher, with only
`Python3_EXECUTABLE` pointing into NPL. CTest records that Python path for its
generated cases and Python tests.

YAPF is not installed in this shared environment and is not needed for these
tests. The separate CI formatting job installs it from `requirements-style.txt`.

The shared netCDF4 package uses MPI-enabled libraries that can initialize MPI
on import. Run harness Python checks, CMake configuration, and model cases
inside a PBS allocation when using this environment. Do not combine the shared
Python libraries with a personal Conda `LD_LIBRARY_PATH`. The Python subprocess
exits before CTest launches each model process; retain the model's matching
compiler, MPI, NetCDF, and ESMF module stack for its executable.

Validated environments and numerical results are recorded separately in
[validation history](validation.md).

## Submit on Derecho

Use [submit_derecho.pbs](../submit_derecho.pbs) from a clean, committed checkout.
Set `PBS_ACCOUNT` to your authorized allocation before submission. The account
is supplied on the command line because PBS directive comments do not expand
shell variables.

```bash
# Run from the repository root, with PBS_ACCOUNT already exported.
: "${PBS_ACCOUNT:?Set PBS_ACCOUNT to your authorized allocation}"
qsub -A "$PBS_ACCOUNT" tests/regression/submit_derecho.pbs
```

The script uses `$USER` to define `TEST="/glade/derecho/scratch/$USER/tmp"` and
creates `$TEST/cfbm_<shortCommitHash>_<YYYYMMDD>`. The date is the job start date.
It refuses an existing directory, preserving earlier evidence. To rerun the
same commit on the same date, rename the previous directory without deleting
it, then resubmit. Avoid concurrent submissions of the same commit/date.

The job clones the local repository, checks out the submitted revision, builds
four configurations, runs `pr` (including focused checks and quick coverage),
then runs legacy tests. Settings are grouped near the top of the template.
`SUITES=(quick pr)` can explicitly run both when separate reports are needed.
`RUN_LEGACY=false` skips historical cases. The default keeps them enabled.

The requested queue is `develop`, which routes CPU jobs to `cpudev`. The
one-node, one-hour allocation covers the sequential standard-case workflow,
including four MPI ranks × four OpenMP threads. This template is for Derecho.
Casper needs its own queue/resource directives and OpenMPI module stack.

The scratch directory contains `validate.log`, build/configuration logs,
`suite-status.txt`, `results/<suite>-<unique>/summary.json`, individual case
outputs, and `build/mpi/legacy-runs/`. PBS also writes its usual joined output
file in the submission directory unless `qsub -o` selects another existing
location. Build failure stops the job. Test failure is collected and causes a
nonzero final job status. Existing legacy comparison failures are expected to
keep that status nonzero until separately resolved.

The final legacy step reconfigures the MPI build's CTest registrations. To
rerun generated cases afterward, restore `CFBM_TEST_SYSTEM=generated` through
`compile.sh` or use a separate generated build. Saved results are unchanged.
