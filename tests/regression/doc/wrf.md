# WRF-host fractional-step tests

[Documentation index](../README.md)

The optional `wrf` suite runs the actual WRF atmosphere with CFBM (`ifire=1`).
It uses WRF's `EM_FIRE` `ideal.exe` to produce the hydrostatic initial state.
It does not require WPS, meteorological analysis files, or `wrfbdy_d01`: all
four lateral boundaries are open. These tests are separate from the prescribed
WRF-data component used by NUOPC and ESMX.

## Initial state

The `terrain/wrf_03` and `terrain/wrf_rounded` configurations share the
sinusoidal terrain, Anderson fuel strips, Lambert projection, and lower-level
3D wind profile of `terrain/u3d`. The evolving WRF atmosphere requires these
explicit substitutions, recorded in `cases.yaml`:

| Setting | WRF case |
| --- | --- |
| Horizontal grid | 71 × 71 atmospheric mass cells; 72 × 72 fire grid, 100 m spacing, refinement 1 |
| Vertical grid | 41 interfaces, 8000 m MSL top, hyperbolic eta stretching with `z_grd_scale=2` |
| Sounding | Reference potential temperature 300 K at 0 m MSL, gradient 0.003 K m−1, reference pressure 900 hPa, mixing ratio 8 g kg−1 |
| Wind | Shared profile interpolated to actual AGL levels; constant above the uppermost supplied profile value; shared terrain modulation |
| Ignition | Central point, 100 m radius, 5 m s−1 imposed ignition spread over 20 s |
| Surface | Revised MM5 surface layer, Noah land surface, grassland/loam initialization; roughness diagnosed by WRF |
| Other physics | No microphysics, radiation, cumulus, or PBL parameterization; 3D turbulent diffusion |
| Moisture | Evolving CFBM moisture, updated each fire step |
| Duration/output | 72 s, fire and host output every 12 s |
| Timesteps | 0.3 s and rounded input 0.222222 s, resolved to 2/9 s for the host clock |
| Execution | MPI-only WRF, one rank or four ranks in a 2 × 2 decomposition |

The sounding temperature is potential temperature, whereas the offline forcing
specifies actual 2 m temperature. Consequently, WRF fields are not expected to
match the offline numerical solution. The WRF interface currently rejects
observed-perimeter initialization; point ignition exercises its supported path.

The initializer reads the shared terrain, slope, and fuel arrays as ASCII.
The harness retains its unmodified `wrfinput_d01` under `initialization/` and
copies it for the host run. That copy receives Lambert coordinates, map factors,
rotation, Coriolis parameter, and the AGL wind profile. This is necessary because
`EM_FIRE` writes Cartesian geometry but CFBM's host initialization requires
Lambert conformal. Projection attributes retain WRF's NC_INT/NC_FLOAT types,
and updates explicitly address time record zero. A unit test detects accidental
growth of the unlimited time dimension and checks the staggered wind profile.

## Build and run

Use a matching WRF source, `ideal.exe`, and `wrf.exe`, built together against the
CFBM revision being tested. The host used to develop this test was WRF master
`06d4240ae989cc3e50af412bb472df3d9048783c`. Configure its CMake build with
`WRF_CORE=ARW`, `WRF_CASE=EM_FIRE`, `WRF_NESTING=BASIC`, `ENABLE_CFBM=ON`,
`USE_MPI=ON`, and `USE_OPENMP=OFF`. Pin WRF's managed CFBM external to the
intended feature revision before building; a checkout of WRF alone does not
select the matching CFBM library.

Register the external executables in an MPI CFBM build, using paths appropriate
to the local build. For a CMake WRF build, the executable names are `wrf` and
`ideal` under `main/`:

```bash
cmake -S . -B "$CFBM_BUILD" \
  -DDM_PARALLEL=ON -DOPENMP=OFF \
  -DCFBM_WRF_EXECUTABLE="$WRF_BUILD/main/wrf" \
  -DCFBM_WRF_IDEAL_EXECUTABLE="$WRF_BUILD/main/ideal" \
  -DCFBM_WRF_SOURCE="$WRF_SOURCE"

python -B tests/regression/regression.py suite --suite wrf \
  --build-dir "$CFBM_BUILD" --run-root "$RUN_ROOT"
```

Use the existing compiler/MPI/Python environment and launcher settings described
in [running tests](running.md). On NCAR systems, run inside PBS with at least four
MPI ranks available; CTest runs cases sequentially. Initialization always uses
one rank because `EM_FIRE`'s ASCII terrain reader is serial. Runtime uses the
case's rank count. Neither compilation nor a PBS exit status establishes a pass.

For an individual case, use the existing case interface with `--driver wrf`,
`--configuration wrf_03` or `wrf_rounded`, `--executable`, `--wrf-ideal`, and
`--wrf-source`. Every invocation retains a fresh run directory. To reuse an
accepted input, preserve `wrfinput_d01`, `namelist.input`, `namelist.cfbm`, and
the linked WRF runtime tables; preserve `initialization/` for regeneration.
Resolve or copy runtime-table links when archiving outside the source tree.
The result records executable hashes, input hash, WRF source identity, its CFBM
submodule identity, and harness identity.

## Acceptance and limits

Require all six fire outputs at 12–72 s, the corresponding WRF timestamps, exactly
240 or 324 host steps, finite fields, nonnegative water vapor, and positive fire
heat release and fuel consumption. At every saved time, WRF's `GRNHFX` and
`GRNQFX` must equal CFBM's current fluxes exactly on the coupled interior cells.
At refinement one, `Provide_atm_feedback` copies these values without spatial
averaging. This checks per-step feedback at times both on and between the nominal
18 s `interval_atm` boundaries. WRF calls the adapter each solver step;
`interval_atm` does not change its solver timestep or impose a NUOPC exchange
window on this host.

The suite compares MPI layouts with the existing field policies and does not
use or replace the approved offline reference. No WRF restart acceptance is
claimed: the specified host revision does not coordinate native WRF restart
with the CFBM checkpoint interface. Standalone restart continuity and NUOPC
exchange-window accumulation/reset are covered by the [scheduling suite](scheduling.md).
