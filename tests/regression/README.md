# CFBM standalone regression harness

This directory contains the configuration, deterministic inputs, runner,
NetCDF comparator, reports, CTest interface, and immutable baseline controls
for the pre-PR39 standalone CFBM tests. `regression.py` is the only public
command-line interface. Normal regression commands never create or approve a
reference.

Use the pinned Derecho/Casper Python environment:

```text
/glade/work/frediani/casper/anaconda3/envs/py314/bin/python \
  tests/regression/regression.py all \
  --suite quick --variants serial,omp,mpi \
  --platform tests/regression/platforms/derecho.yaml \
  --work-root /glade/derecho/scratch/frediani/cfbm-regression/quick-001
```

Load the current NCAR compiler, NetCDF, and MPI modules before invoking the
outer command. The retained `env/derecho/gnu-12.2.0` file references an old
module stack and is deliberately not used by the platform profiles.

The work root must not exist. The outer command creates independent build,
install, run, and report trees. Model integrations, including serial tests,
must run within a PBS compute job on Casper or Derecho. Synthetic Python unit
tests may run on a login node:

```text
PYTHONDONTWRITEBYTECODE=1 \
  /glade/work/frediani/casper/anaconda3/envs/py314/bin/python \
  -m unittest discover -s tests/regression/tests -v
```

The outer summary records each compile argument vector, the required CMake
cache switches, installed executable hashes, exact model and harness commits,
and tracked, untracked, and ignored inventories before and after execution.
Candidate publication also records the hashes of the configuration, template,
generator, resolved specification, rendered namelist, generated inputs,
executable, run manifest, and every output. If an approved baseline is
selected, the harness records and compares every baseline file hash as well.
Any change to these source or baseline trees fails the invocation.

Direct CMake builds register the quick cases against
`$<TARGET_FILE:fire_behavior.exe>`. Set the documented
`CFBM_REGRESSION_EXECUTABLE` CMake cache variable when CTest must exercise a
specific installed executable. `CFBM_REGRESSION_BASELINE` selects an approved
immutable reference, and `CFBM_REGRESSION_ATTEMPT_ROOT` places isolated rerun
artifacts outside the build tree when required.

## Scientific case policy

All numerical values are defined in `cases.yaml`. Values resolve in this
order: defaults, case, suite, named numerical method, named feature, and
execution. Execution definitions contain only rank, thread, affinity,
timeout, and environment policy. Lists replace earlier lists; mappings merge
recursively.

| Case | Grid in quick/pr | Fuel and forcing | Ignition and interpolation |
| --- | --- | --- | --- |
| `circle_nowind` | 72 × 72 at 100 m | Anderson category 3; U=V=0 | 500 m point/circle; bilinear horizontal; 10 m winds |
| `fuel_strip_wind` | 72 × 72 at 100 m | Anderson categories 7, 3, 6, 5, 1, 13, 2, 12, 10, 11, 9, 8; U=10 m s⁻¹, V=0 | vertical ignition line at 35% of domain width; nearest-neighbor horizontal; 10 m winds |
| `terrain_fuel_fmc_wind` | 72 × 72 at 100 m | sinusoidal 150 m terrain; same 12-category strips as `fuel_strip_wind`; U=V=10 m s⁻¹; changing T2 and water-vapor mixing ratio | 500 m observed perimeter; bilinear horizontal; 10 m winds; moisture updated every timestep |

“Stacked vertically” is represented by horizontal strips whose category
changes with the NetCDF `south_north` index. The 12-category order is the
head-rate-of-spread ranking used by the reference strip generator at
U10=10 m s⁻¹, with Anderson chaparral category 4 excluded by the documented
2026-08-30 scientific decision. This is why the categories are not 1 through
12. The source script SHA-256 values are recorded in `cases.yaml`.

The point ignition follows the reference circle radius of 500 m. The complex
case uses the identical `NFUEL_CAT` strip field on a smooth analytic terrain,
so `ZSF`, `DZDXF`, and `DZDYF` are mutually consistent while fuel coverage is
shared between the two cases. Its generated relative-humidity forcing changes
during the run.
`fmoist_freq=1` follows `physics/fmc_wrffire_mod.F90`: moisture advances when
`mod(itimestep,fmoist_freq)==0`. Successful validation requires `fmc_g` to
change between the initialization and final output.

The line ignition remains in the western half of the domain at 35% of its
width and extends from 25% to 75% of its height. These bounds provide enough
clearance for the required 60 s integration; the earlier line from 8% to 92%
of domain height reached a boundary before the final output.

Every case requests four internal tiles and `tile_strategy=3`. This gives the
OpenMP-4 configuration one nonempty tile per thread and keeps serial, OpenMP,
and MPI tile policy explicit in the resolved namelist.

Observed-perimeter runs set `fire_num_ignitions=0`. The supplied `lfn_init`
is installed as the initial condition at simulation-relative time 0 before
the first output and is not reapplied during propagation.

Quick and PR use dt=4 s for 60 s and require initialization plus the 60 s
output. Full uses dt=2 s for 3600 s on a 320 × 320 grid at 25 m and requires
initialization plus 900, 1800, 2700, and 3600 s. The two allowed numerical
methods are `(9,4)` and `(2,4)`. Restart settings and `(4,5)` are rejected.

## Generated NetCDF schema

NetCDF dimensions are written in Python/C order below. The netCDF Fortran API
reverses those dimensions when reading into arrays, yielding the model's
`(west_east,south_north)` layout.

| File | Variables | NetCDF dimensions | Type and units |
| --- | --- | --- | --- |
| `geo_em.d01.nc` | `XLAT_M`, `XLONG_M` | `(Time,south_north,west_east)` | float32; degrees |
| `geo_em.d01.nc` | `XLAT_C`, `XLONG_C` | `(Time,south_north_stag,west_east_stag)` | float32; degrees |
| `geo_em.d01.nc` | `ZSF`, `DZDXF`, `DZDYF`, `NFUEL_CAT` | `(Time,south_north_subgrid,west_east_subgrid)` | float32; m, 1, 1, category |
| `geo_em.d01.nc` | `lfn_init` for the perimeter case | `(south_north_subgrid,west_east_subgrid)` | float32; m |
| `wrf.nc` | `Times` | `(Time,DateStrLen=19)` | char; `YYYY-MM-DD_HH:MM:SS` |
| `wrf.nc` | `XLAT`, `XLONG`, `T2`, `Q2`, `ZNT`, `PSFC`, `RAINC`, `RAINNC`, `U10`, `V10` | `(Time,south_north,west_east)` | float32; standard WRF surface units; `Q2` is water-vapor mixing ratio and `ZNT` is m |

The real-case mass grid is one cell smaller than the staggered/fire grid in
each horizontal direction, following the current readers and reference
generator. WRF forcing includes the initial record and every configured
atmospheric update through the final time. Identical resolved configurations
produce identical field values and, in the pinned environment, identical file
hashes.

## Stages and evidence

`prepare` writes resolved YAML, the namelist, generated inputs, file hashes,
and `run_manifest.json` in a new directory. `run` verifies those identities,
refuses stale output, records the exact argument vector and executable hash,
captures both process streams, detects source-derived fatal prefixes, and
validates the independently expected output inventory. `compare` accepts an
approved immutable baseline or an explicit diagnostic result and creates a
new report directory. `case` composes these stages. Repeated CTest runs use a
new `attempt-NNNNNN` directory.

Dynamic finite floating-point values pass only when
`abs(test-reference) <= 1.0e-4*abs(reference)`, using the reference in the
denominator and no absolute tolerance. Static float fields `lats`, `lons`,
`zsf`, `nfuel_cat`, and `fz0` require equal element storage bits. Integers,
strings, and categories are exact. Masks, NaN locations, infinity signs,
dimensions, dtypes, variables, and non-volatile metadata are checked
separately. Reports distinguish decoded numerical agreement from storage-bit
agreement and use JSON `null` for undefined or infinite metrics.

## Baselines

Candidate creation consumes a complete, passing matrix summary and requires
clean committed model and harness repositories. The immutable identifier must
contain the model commit abbreviation:

```text
python tests/regression/regression.py baseline-create \
  --candidate-root /glade/derecho/scratch/frediani/cfbm-candidates \
  --identifier pre-pr39-6ae8078-candidate001 \
  --work-root /glade/derecho/scratch/frediani/cfbm-regression/quick-001 \
  --model-repository /path/to/clean/model-checkout
```

`baseline-accept` requires an approver and decision string. It records the
decision and selects the exact identifier without changing reference NetCDF
payloads. Acceptance is an explicit team action and is never performed by
candidate generation.

## Baseline gate

Baseline production requires the reviewed pre-baseline repair stack. The
timestep correction remains gated on confirmation that the first physics call
represents `[0,dt]` while physics receives `itimestep=1`. Candidate validation
must remain blocked until that decision is implemented and the complete PBS
quick and PR matrices pass.
