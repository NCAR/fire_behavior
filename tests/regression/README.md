# CFBM regression tests

Build the model once, then select tests by name. CTest runs each selected case;
Python generates its inputs, checks the outputs, and compares executions with
the same scientific settings. Serial, OpenMP, MPI, combined MPI/OpenMP, NUOPC,
and ESMX builds use this interface.

## Run a suite

Use a Python environment with NumPy, netCDF4, and PyYAML. CI installs the
versions pinned in `requirements.txt`. On NCAR HPC systems,
run builds and model integrations in a PBS allocation. Use fresh build and
installation directories outside the source checkout. For example:

```bash
./compile.sh --mpi-off --build-dir=/path/to/scratch/build/serial \
  --prefix=/path/to/scratch/install/serial

python -B tests/regression/regression.py suite --suite quick \
  --build-dir=/path/to/scratch/build/serial \
  --run-root=/path/to/scratch/results
```

Replace the scratch paths with your own paths. On Casper, the validated module
combination is `ncarenv/25.10 intel/2025.2.1 openmpi/5.0.8 netcdf/4.9.3
esmf-mpi/8.9.1`. This is a tested environment, not a portability requirement.
For the complete Derecho build, including ESMX, use `ncarenv/25.10
intel/2025.2.1 ncarcompilers/1.2.0 cray-mpich/8.1.32 netcdf-mpi/4.9.3
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

### Build other configurations in separate directories:

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

### Select a smaller set with plain names:

```bash
python -B tests/regression/regression.py suite --suite pr \
  --case terrain_3d --driver nuopc --execution mpi4 \
  --build-dir=/path/to/scratch/build/nuopc \
  --run-root=/path/to/scratch/results
```

`quick` exercises each standalone case plus threaded and distributed terrain
cases. `pr` adds the one-rank and one-thread configurations and checks all
cases across layouts. Hybrid configurations reserve ranks times threads.
The full experiments use 320 x 320 fire grids, 3600 s integrations, and method
pairs `(9,4)` and `(2,4)`. Register them explicitly with
`cmake -S . -B BUILD_DIR -DCFBM_FULL_TESTS=ON`, then select `--suite full` in
an appropriately sized HPC allocation. They are absent from ordinary CTest
registration so a plain `ctest` cannot accidentally start the large campaign.


`--test` selects one exact CTest name. `--suite unit` runs focused tests. No
selection accepts regex syntax. Internally, Python reads CTest's JSON listing
and supplies a numeric list of selected tests to CTest.

`--suite` accepts **five values**. `--test` accepts **any exact CTest name registered in the selected build**, so its available names depend on the build configuration.

The suite options are defined in [regression.py]:

| Option | Tests selected |
|---|---|
| `quick` | Default. Focused tests plus the four standard cases in serial, and both terrain cases with `omp4`, `mpi4`, or `hybrid4`, where supported. |
| `pr` | Focused tests plus all four standard cases across `serial`, `omp1`, `omp4`, `mpi1`, `mpi4`, and `hybrid4`, where supported. |
| `full` | Focused tests plus the larger experiments: 320 × 320 grids, 3600 s integrations, both method pairs `(9,4)` and `(2,4)`, and the additional `mpi8` execution. Requires `CFBM_FULL_TESTS=ON`. |
| `unit` | Focused Fortran tests and the Python harness tests. No generated fire-case integrations. |
| `legacy` | Original tests registered by the legacy build route. |

These selections follow [cases.yaml]. Each build contributes only the executions it supports (e.g. Serial or MPI). **`full` selects the larger experiments; it does not also select the standard experiments.**

For `--test`, list every available name in a particular build without running tests:

```bash
ctest --test-dir /path/to/build/tests -N
```

To list only its focused tests:

```bash
ctest --test-dir /path/to/build/tests -N -L '^unit$'
```

The valid names for `--test` fall into three groups:

** 1. Generated model tests** follow this naming convention:

```text
<driver>_<case>_<scale>_<method>_<execution>
```

| Component | Values |
|---|---|
| Driver | `standalone`, `nuopc`, `esmx` |
| Case | `circle_nowind`, `fuel_strip_wind`, `terrain_10m`, `terrain_3d` |
| Scale and method | `standard_ref94`, `full_ref94`, `full_ref24` |
| Execution | `serial`, `omp1`, `omp4`, `mpi1`, `mpi4`, `mpi8`, `hybrid4` |

NUOPC and ESMX register only the two terrain cases. `mpi8` is available only for full experiments. Execution choices must match the build: serial, OpenMP, MPI, or hybrid. These rules are implemented in [config.py].

Examples:

```text
standalone_circle_nowind_standard_ref94_serial
standalone_terrain_3d_standard_ref94_omp4
nuopc_terrain_10m_standard_ref94_mpi4
esmx_terrain_3d_full_ref24_mpi8
```

** 2. Focused tests** are unit tests for an existing Fortran program or Python script. They have exact names, as registered in [unit/CMakeLists.txt] and [regression/CMakeLists.txt]:

```text
regression_python
*_unit
namelist_broadcast_mpi

```
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

The implementation is in [python_harness_test.py] (18 tests) and [generator_comparator_test.py] (16 tests).

These tests use generated inputs and small synthetic output files. Passing them supports confidence in the testing infrastructure; validating model integration and scientific output requires the generated model tests.


`namelist_broadcast_mpi` requires an MPI build. `regression_python` is registered by the generated test system and runs both Python test files.

** 3. Legacy tests** are `test7`, `test8`, `test7esmf`, `test8esmf`, `test7esmx`, `test8esmx`, and `testx`, subject to the enabled coupling capabilities. They require the legacy test system. See [tests/CMakeLists.txt].

Finally, **`--test` overrides the suite-label selection**. For example, `--test regression_python` selects that test even if `--suite` remains at its default, `quick`. Any supplied `--case`, `--driver`, or `--execution` filters still apply. See [run_suite.py].

## Scientific cases and coupled checks

| Case | Purpose |
| --- | --- |
| `circle_nowind` | Idealized point ignition and uniform Anderson fuel |
| `fuel_strip_wind` | Line ignition, twelve fuel categories, nearest interpolation |
| `terrain_10m` | Terrain, delayed observed perimeter, varying moisture, terrain-dependent 10 m winds |
| `terrain_3d` | Same terrain experiment with a terrain-dependent vertical wind profile |

The shared configuration uses a 72 x 72 fire grid, 100 m spacing, 4 s timesteps,
and 60 s integrations. NUOPC and ESMX run both terrain cases with generated
WRF-data forcing. All real-case forcing includes staggered U/V and PH/PHB,
because the NUOPC WRF-data component reads both wind representations.

The atmospheric grid extends beyond the fire grid. This makes every fire cell
an interpolation target inside the forcing domain, avoiding unmapped coupled
boundary cells. 

Both terrain cases multiply prescribed winds by
`1 + wind_terrain_gradient_per_m * (height - terrain.base_elevation_m)`.
The gradient is 0.001 per meter, giving approximately 15% weaker winds in
valleys and 15% stronger winds on hills around the 1600 m reference elevation.
This is deterministic synthetic forcing, not a terrain-flow parameterization.
Terrain is averaged onto mass centres for U10/V10 and onto the respective
U/V faces for 3D winds. The same factor multiplies all vertical levels.

The 3D case samples the shear profile at 20 m, between mass levels at 10 and
40 m. Its unscaled analytical wind is `(12, 8)` m/s. Spatial interpolation
and destaggering affect the terrain-dependent result, so the variable-wind
case checks its prescribed component bounds and nonzero spatial variation.
Cross-execution and cross-driver comparisons retain their numerical tolerance.
Setting the gradient to zero restores the exact uniform-profile check at
rtol=1e-4, atol=0; Python tests retain that control. Neither experiment equates
the 3D wind with the fuel-adjusted 10 m wind or requires their fire outputs
to match. Existing uniform-wind reference candidates do not certify these cases.

Staggering is a property of the atmospheric host: this WRF-data fixture uses
native WRF staggering, while UFS can supply mass-centred winds. Both current
WRF-data readers destagger before subsequent processing. These tests do not
validate UFS imports or establish one staggering convention as universally
preferable.

Each case requires the expected output timestamps, field schema and metadata,
finite values, evolving level set, fuel consumption, positive fire fluxes,
and the configured moisture/perimeter behavior, and nonzero surface winds with
the prescribed direction on every fueled cell. Coupled cases also require
the driver's completion marker. A zero process exit alone is insufficient.

Standalone and the coupled drivers save atmospheric fields used during the
completed fire interval. With 4 s fire and atmospheric intervals, output at
60 s contains the 56 s forcing record. The forcing is refreshed for the next
advance after output; the atmospheric checks use this shared convention.

Generated atmospheric `XLAT` and `XLONG` use float64 and the model reader
preserves them into the ESMF cell-centre grid. Forcing fields, saved model fields,
and fire-grid coordinates retain their existing storage precision. The model's
remaining-fuel and timestep-consumption arithmetic uses float64 internally.
Legacy float32 coordinates are
still accepted, but conversion to float64 cannot recover lost precision.

### Deferred

The humidity interpretation remains the separate scientific review tracked
by PR #46. Restart, PR #39, and method `(4,5)` remain deferred. `testx` remains
in the legacy route; generated tests exercise the WRF-data coupling, not the
ESMX_Data feedback fixture.


## Results and references

Every execution gets a new directory with the resolved namelist, generated
inputs, model log, outputs, and `result.json`. The resolved scientific settings,
source revision, and local edits are recorded in that result. A suite writes
one `summary.json` with case locations and cross-execution comparisons. CTest
provides JUnit output in CI. Failed runs retain all files.

Normal tests allow local edits. Reference creation requires a passing suite
from the current clean source revision:

```bash
python -B tests/regression/regression.py reference-create \
  --suite-root=/path/to/scratch/results/pr-UNIQUE \
  --destination=/path/to/new/reference-candidate
```

This creates an **unapproved** reference and never activates it. Supply an
explicit directory through `-DCFBM_REFERENCE=/path/to/reference` when configuring
a build. Normal comparisons reject unapproved references; explicit directory
selection does not replace recorded team approval. Without that option, tests check physical evolution and agreement
among executions; they do not claim agreement with an approved historical
baseline. The report records the supplied reference's approval status.

SHA256 checksums are retained only for reference integrity. A checksum is a
file fingerprint that detects changes to stored reference data; it does not
establish scientific correctness. There are no automatic fallback directories
or repeated source-tree inventories. Numerical comparisons retain the rule
`abs(test - reference) <= 1e-4 * abs(reference)`, with zero absolute tolerance;
static fields require identical stored bits within each driver.

Suites also compare each NUOPC/ESMX run with the matching standalone run,
preferentially serial when available. Only the spatially interpolated `fz0`
field uses the same numerical tolerance across these drivers. Its value is
constant in time, but the standalone Lambert and ESMF mapping weights differ.
Fire coordinates, terrain, fuel categories, and same-driver static fields
retain exact checks. Every comparison reports both drivers, the applied rule,
and bitwise differences even when numerical tolerance governs acceptance.
Reference comparisons retain their existing same-driver policy.

Passing the 60 s cases does not establish agreement over longer integrations.
A separate 600 s terrain experiment, with output every 60 s, first exceeded the
same cross-driver tolerance at 180 s. All integrations completed, and NUOPC and
ESMX agreed with each other. Their differences from standalone included local
timestep fuel consumption and heat-flux discrepancies despite small integrated
burned-area and fuel differences. These longer-run discrepancies remain unresolved;
the ordinary CI duration and numerical tolerance have not been adjusted to
accept them.

Dimensions, metadata, masks,
NaNs, and output inventories are checked separately.

## Legacy compatibility

`compile_legacy.sh` retains the old build/test interface and chooses the legacy
CTest registrations. Give it separate build and installation directories:

```bash
./compile_legacy.sh --nuopc --test \
  --build-dir=/path/to/scratch/build/legacy \
  --prefix=/path/to/scratch/install/legacy
```

The seven original tests remain available according to build capabilities:
`test7`, `test8`, their NUOPC/ESMX variants, and `testx`. Original fixture files,
scripts, and reference text are unchanged. A small Bash wrapper runs a staged
script in a private directory, replacing its cleanup commands with no-ops so
diagnostics survive. Original comparison criteria remain intact.

Existing legacy numerical failures caused by the prerequisite timing change
are not repaired by this harness. The ESMX build now lists the model archive
explicitly and uses the Python interpreter selected during CMake configuration.
With matching parallel NetCDF/HDF5 modules, the normal Derecho build and
generated coupled cases pass without an external linking workaround.
No model physics or legacy reference is changed by these harness/build fixes.

## Source organization and style

| File | Responsibility |
| --- | --- |
| `regression.py` | Parse arguments and dispatch |
| `run_case.py` | Stage inputs and execute one existing binary |
| `run_suite.py` | Select CTests and compare equivalent executions |
| `config.py`, `cases.yaml` | Defaults, named scientific cases, rank/thread policy |
| `generate_inputs.py` | Write deterministic NetCDF inputs |
| `render_namelist.py` | Fill one template with standard Python formatting |
| `check_outputs.py` | Check completion and scientific behavior |
| `compare_outputs.py` | Compare NetCDF fields |
| `reports.py` | Save results and print their locations |
| `reference.py` | Create and verify explicit reference directories |

The CFBM development team maintains these files. Reused code retains its
original creation date and coding-assistance attribution; institutional
copyright remains at repository level. This convention replaces personal
headers in personal script-style guidance. Preserve creation dates and use this
header for the reused September scripts:

```python
# Created on 2026-09-12 by the CFBM development team assisted by GPT-6-Astra.
```

**Required Python style:** use Google-based YAPF formatting, with four-space
indentation and an 80-column target, as configured in `.style.yapf`. Use
snake_case names, type hints on public functions, and pathlib for paths.
Comments must explain scientific or workflow decisions; blank lines must
separate functions and logical operations. Review readability as well as the
formatter result. YAPF does not enforce the entire Google Python style guide.
Agents editing this subsystem must also read `AGENTS.md` in this directory.

YAPF is a Python package. Only developers formatting the code and the separate
CI formatting job need it; model builds and regression execution do not. Install
the pinned version into a writable development environment, not a shared NCAR
environment:

```bash
python -m pip install -r tests/regression/requirements-style.txt
python -m yapf --diff --recursive --style tests/regression/.style.yapf tests/regression
```

The check prints differences and returns failure if formatting changes are
needed. To apply those changes locally, replace `--diff` with `--in-place`.
CI checks formatting without modifying files.

`tests/python_harness_test.py` checks configuration, generated wind fields,
exact test selection, reference integrity, and incomplete model execution.
`tests/generator_comparator_test.py` checks input reproducibility and numerical
comparison edge cases. Neither file launches the Fortran model. Test filenames
end in `_test.py`, so discovery must include the pattern below.

For a direct invocation, select an artifact directory outside the source tree:

```bash
export CFBM_TEST_TMP=/path/to/scratch/python-test-artifacts
python -B -m unittest discover -s tests/regression/tests -p '*_test.py' -v
```

Every test retains its files in a unique directory. No personal path or system
`/tmp` default is embedded in these scripts. CTest sets `CFBM_TEST_TMP` to
`BUILD_DIR/python-test-artifacts` automatically.

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

cmake -S . -B /path/to/scratch/build/mpi -DPython3_EXECUTABLE="$CFBM_PYTHON"
./compile.sh --nuopc --build-dir=/path/to/scratch/build/mpi \
  --prefix=/path/to/scratch/install/mpi
"$CFBM_PYTHON" -B tests/regression/regression.py suite --suite quick \
  --build-dir=/path/to/scratch/build/mpi --run-root=/path/to/scratch/results
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

The shared environment was checked on Casper and Derecho on 2026-10-02.
Derecho PBS `7702254.desched1` subsequently validated commit `1426bed` on
2026-10-03: all four builds and focused CTests passed, including all 34 Python
checks. The generated quick suite passed 14 runs and 10 comparisons; the PR
suite passed 32 runs and 32 comparisons. Same-driver layouts were bitwise
identical. NUOPC and ESMX used the normal build route. All seven retained legacy
tests still failed their original comparisons. This validates the 60 s generated
cases, not the separate 600 s experiment described above.
