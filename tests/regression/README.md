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

The build/launch separation is the same as in the legacy suite. `compile.sh`
chooses whether MPI and OpenMP are compiled in. The case runner launches the
resulting executable with the rank count and `OMP_NUM_THREADS` from the
`executions` section of `cases.yaml`. The model/coupler derives the MPI domain
layout from its communicator; `num_tiles` and `tile_strategy` in the rendered
namelist control the within-rank tiles. Compilation does not select rank counts
or domain decomposition. CI now builds four configurations explicitly so each
parallel mode is exercised.

Build other configurations in separate directories:

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

Select a smaller set with plain names:

```bash
python -B tests/regression/regression.py suite --suite pr \
  --case terrain_3d --driver nuopc --execution mpi4 \
  --build-dir=/path/to/scratch/build/nuopc \
  --run-root=/path/to/scratch/results
```

`--test` selects one exact CTest name. `--suite unit` runs focused tests. No
selection accepts regex syntax. Internally, Python reads CTest's JSON listing
and supplies a numeric list of selected tests to CTest.

`quick` exercises each standalone case plus threaded and distributed terrain
cases. `pr` adds the one-rank and one-thread configurations and checks all
cases across layouts. Hybrid configurations reserve ranks times threads.
The full experiments use 320 x 320 fire grids, 3600 s integrations, and method
pairs `(9,4)` and `(2,4)`. Register them explicitly with
`cmake -S . -B BUILD_DIR -DCFBM_FULL_TESTS=ON`, then select `--suite full` in
an appropriately sized HPC allocation. They are absent from ordinary CTest
registration so a plain `ctest` cannot accidentally start the large campaign.

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
boundary cells. This extension changes the old harness's real-case forcing
files, so earlier reference candidates do not certify this implementation.

Both terrain cases multiply prescribed winds by
`1 + wind_terrain_gradient_per_m * (height - terrain.base_elevation_m)`.
The gradient is 0.001 per metre, giving approximately 15% weaker winds in
valleys and 15% stronger winds on hills around the 1600 m reference elevation.
This is deterministic synthetic forcing, not a terrain-flow parameterization.
Terrain is averaged onto mass centres for U10/V10 and onto the respective
U/V faces for 3D winds. The same factor multiplies all vertical levels.

The 3D case samples the shear profile at 20 m, between mass levels at 10 and
40 m. Its unscaled analytical wind is `(12, 8)` m/s. Spatial interpolation
and destaggering affect the terrain-dependent result, so the variable-wind
case checks its prescribed component bounds and nonzero spatial variation.
Those checks do not establish the accuracy of the horizontal interpolation.
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
The humidity interpretation remains the separate scientific review tracked
by PR #46. Restart, PR #39, and method `(4,5)` remain deferred. `testx` remains
in the legacy route; generated tests exercise the WRF-data coupling, not the
ESMX_Data feedback fixture.

Generated atmospheric `XLAT` and `XLONG` use float64 and the model reader
preserves them into the ESMF cell-centre grid. Physical fields and fire-grid
coordinates retain their existing precision. Legacy float32 coordinates are
still accepted, but conversion to float64 cannot recover lost precision.

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

The generated 10 m wind tests cover the repaired local-to-global index mapping
in the NUOPC cap, shared by ESMX. Both 10 m and 3D wind cases are compared
across the registered layouts and against matching standalone cases.

Existing legacy numerical failures caused by the prerequisite timing change
are not repaired by this harness. Casper's existing production ESMX link issue
is also separate; validation with an externally linked diagnostic executable
must be identified as such. No model physics or legacy reference is changed.

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

All 27 Python tests passed through CTest on 2026-10-02 in this shared environment:
Casper PBS `6122111.casper-pbs` and Derecho PBS `7690077.desched1`, both exit 0.
These checks verified Python imports, CMake/CTest registration, discovery of both
test files, and build-tree artifact paths. They did not run the Fortran model.
The earlier coupled model runs used a different Python interpreter; these Python
checks do not certify full model execution with the shared environment.
