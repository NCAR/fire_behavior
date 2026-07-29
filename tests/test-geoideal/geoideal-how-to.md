# Test-Geoideal Student Guide

This guide describes how to configure, generate, and run the `test-geoideal`
standalone CFBM case. The case uses normal NetCDF inputs with `ideal_opt = 0`,
but the input fields are generated as homogeneous or idealized arrays so the
test can be changed without editing model source code.

## 1. Work From The Test Directory

Start from the fire-behavior repository root and move into the case directory:

```bash
cd tests/test-geoideal
```

Check that the standalone executable exists:

```bash
ls ../../install/bin/fire_behavior.exe
```

If it is missing, build from the fire-behavior root:

```bash
cd ../..
./compile.sh --mpi-off
cd tests/test-geoideal
```

## 2. Load The Needed Environment

Use a Python environment that has `netCDF4` and `numpy`:

```bash
python -m py_compile make_geoideal_inputs_from_dict.py
```

If you need NetCDF command-line inspection tools, load NCO or NetCDF utilities
from modules as appropriate on Casper or Derecho. For example:

```bash
module load nco
```

## 3. Edit The Case Configuration

Edit the `CONFIG` dictionary in `make_geoideal_inputs_from_dict.py`.

The main sections are:

- `time`: start time, duration, model time step, forcing interval, and output interval.
- `grid`: atmospheric grid dimensions, atmospheric spacing, and fire subgrid ratios.
- `projection`: Lambert metadata used by CFBM to regenerate fire-grid latitudes and longitudes.
- `ignition`: local-meter ignition coordinates measured from the lower-left fire-array origin.
- `fuel`: background fuel and the no-fuel patch placed downwind from ignition.
- `terrain`: homogeneous topography and slopes.
- `atmosphere`: homogeneous wind, near-surface thermodynamic fields, pressure, roughness, and rainfall.

The default grid is:

- Atmospheric global dimensions: `20 x 20`
- Atmospheric mass dimensions: `19 x 19`
- Atmospheric spacing: `100 m`
- Fire subgrid ratio: `4 x 4`
- Fire grid: `80 x 80`
- Fire spacing: `25 m`

The default projection uses `CEN_LAT = TRUELAT1 = TRUELAT2 = 35 deg`.
This is an artificial Lambert case, but it is better conditioned than a
near-equatorial `1 deg` setting because the current Fortran projection uses
single-precision `real` values.

## 4. Regenerate The Inputs

Run the dictionary generator:

```bash
python make_geoideal_inputs_from_dict.py
```

This rewrites:

- `geo_em.d01.nc`
- `wrf.nc`
- `namelist.fire`

The generator derives staggered dimensions, fire-grid dimensions, patch bounds,
projection attributes repeated between files, and namelist timing values from
the editable dictionaries. Do not edit those derived values manually in the
NetCDF files.

## 5. Inspect The Generated Files

Check the NetCDF headers:

```bash
ncdump -h geo_em.d01.nc
ncdump -h wrf.nc
```

Useful checks:

- `geo_em.d01.nc` has `NFUEL_CAT`, `ZSF`, `DZDXF`, and `DZDYF` on the fire subgrid.
- `geo_em.d01.nc` has `sr_x = 4` and `sr_y = 4`.
- `wrf.nc` has homogeneous atmospheric forcing fields such as `U`, `V`, `T2`, `Q2`, `PSFC`, and `ZNT`.
- Both files have consistent `DX`, `DY`, `CEN_LAT`, `CEN_LON`, `TRUELAT1`, `TRUELAT2`, and `STAND_LON`.

## 6. Run The Case

Run the standalone executable from the case directory:

```bash
../../install/bin/fire_behavior.exe > geoideal_run.log
```

Check the end of the log:

```bash
tail -n 40 geoideal_run.log
```

For the default `30 s` case, the run should reach:

```text
2012-06-25_18:00:30
```

## 7. Inspect The Fire Output

List the output files:

```bash
ls fire_output_*.nc
```

Inspect the final file:

```bash
ncdump -h fire_output_2012-06-25_18:00:30.nc
```

For diagnostics, focus on:

- `nfuel_cat`: verifies the background fuel and the no-fuel patch.
- `lfn`: verifies ignition and front location.
- `uf`, `vf`: fire-grid wind components after interpolation.
- `ros`, `ros_front`: spread-rate diagnostics, available when `devel_opt = 1` and `output_level = 1`.

Remember that NetCDF tools and Python normally present fire fields as
`(south_north, west_east)`, while the Fortran state arrays are indexed as
`(west_east, south_north)`.

## 8. Common Edits

To move ignition, edit local-meter coordinates in `CONFIG["ignition"]`.
For example, a vertical ignition line centered halfway up the fire grid has
nearly constant `x` and `y` values around `1000 m`.

To move the no-fuel patch farther downwind, edit:

```python
"no_fuel_patch_offset_m": 250.0
```

This offset is measured from the ignition-line midpoint in the direction of the
homogeneous wind vector. With `u_m_s = 6.0` and `v_m_s = 0.0`, the patch is
placed eastward, in the positive `x` direction.

To change fire-grid spacing, edit atmospheric `dx_m`, `dy_m`, and/or `sr_x`,
`sr_y`. For example, `dx_m = 100 m` and `sr_x = 10` gives `10 m` fire spacing.

## 9. Interpretation Notes

This case is designed for controlled numerical behavior, not geographic realism.
The local-meter coordinates are the user-facing control system. Lambert
latitude and longitude are intermediate coordinates required by the current
standalone source path.

With the recommended mid-latitude Lambert settings, the generator maps
local-meter coordinates directly to atmospheric projection indices before
writing namelist ignition latitude and longitude. A true Cartesian projection
option in the source code would remove most of this lat/lon indirection for
idealized tests.
