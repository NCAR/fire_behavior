# YAML configurations

[Documentation index](../README.md)

Use one [cases.yaml](../cases.yaml) with exact Fortran block and option names
under `namelist`. For example, `namelist.fire.wind_vinterp_opt: 0` writes
`wind_vinterp_opt=0` in `&fire`. Input-generator settings live under `inputs`;
they describe synthetic fields and are not additional model namelist options.

## Names and roles

| Selection | Meaning | Examples |
| --- | --- | --- |
| Case | Shared physical setup and its purpose | `circle`, `fuels`, `terrain` |
| Configuration | Explicit set of scientific option/input overrides within a case | `base`, `u10m`, `u3d`, `godunov3d` |
| Scale | Grid dimensions, spacing, integration duration, and intervals | `small`, `large` |
| Driver | Executable entry point | `standalone`, `nuopc`, `esmx` |
| Execution | Compiled build, MPI ranks, OpenMP threads, timeout | `serial`, `omp4`, `mpi4`, `hybrid4` |
| Suite | Explicit selections of cases, configurations, scale, and executions | `quick`, `pr`, `full` |

`unit` and `legacy` are additional CTest selections, not generated experiments.
The suite `full` uses scale `large`. Scale is more than domain size: it also
changes the integration schedule. The name `small` replaces `standard`, and
`large` replaces the old scale name `full`.

Resolution is **defaults → case → configuration → scale**. Nested mappings
merge; scalar values and lists replace earlier values. Execution settings are
kept separately and cannot override science. If `case --configuration` is
omitted, the first listed configuration is selected: `base` for circle/fuels,
`u10m` for terrain. CMake always passes the explicit configuration name.

## Example spanning the supported option groups

This is an excerpt illustrating the organization, not a replacement for the
complete file. Omitted input parameters and required namelist defaults remain
in the production YAML.

```yaml
defaults:
  start: 2020-01-01_00:00:00
  duration_seconds: 60
  namelist:
    time:
      dt: 4
      interval_output: 60
      num_tiles: 4
      tile_strategy: 3
    atm:
      interval_atm: 4
    fire:
      fuel_opt: 1              # Anderson fuels supported by the model.
      fire_upwinding: 9        # WENO propagation.
      fire_upwinding_reinit: 4
      wind_vinterp_opt: 1      # Fuel-adjusted 10 m winds.
      hinterp_opt: 2           # Bilinear horizontal interpolation.
      fire_wind_height: 6.096  # Metres; also used for the 3D profile.
      fire_is_real_perim: false
      fmoist_run: false
    devel:
      output_level: 0
  inputs:
    grid: {nx: 72, ny: 72, dx_m: 100.0, dy_m: 100.0}
    projection: {map_proj: 1, cen_lat: 40.0, cen_lon: -105.0}
    terrain: {kind: flat, base_elevation_m: 1600.0, amplitude_m: 0.0}
    fuel: {family: Anderson13, categories: [3]}
    ignition: {kind: point, center_x_fraction: 0.5, center_y_fraction: 0.5}
    atmosphere:
      u10_m_s: 10.0
      v10_m_s: 10.0
      height_interfaces_m: [0.0, 20.0, 60.0, 120.0, 200.0]
      u_profile_m_s: [10.0, 14.0, 18.0, 22.0]
      v_profile_m_s: [10.0, 6.0, 2.0, -2.0]

cases:
  terrain:
    description: Delayed perimeter over sinusoidal terrain and fuel strips.
    drivers: [standalone, nuopc, esmx]
    namelist:
      fire:
        ideal_opt: 0
        fire_is_real_perim: true
        fire_ignition_start_time1: 8.0
        fmoist_run: true
        fmoist_freq: 1
        fmoist_dt: 4.0
    inputs:
      terrain: {kind: sinusoidal, amplitude_m: 150.0}
      ignition: {kind: perimeter}
      atmosphere: {wind_terrain_gradient_per_m: 0.001}
    configurations:
      u10m:
        namelist:
          fire: {wind_vinterp_opt: 1}
      u3d:
        namelist:
          fire: {wind_vinterp_opt: 0}
      godunov10m:
        namelist:
          fire: {wind_vinterp_opt: 1, fire_upwinding: 2}
      godunov3d:
        namelist:
          fire: {wind_vinterp_opt: 0, fire_upwinding: 2}

scales:
  small: {}
  large:
    duration_seconds: 3600
    inputs:
      grid: {nx: 320, ny: 320, dx_m: 25.0, dy_m: 25.0}
    namelist:
      time: {dt: 2, interval_output: 900}
      atm: {interval_atm: 60}

executions:
  serial: {build: serial, ranks: 1, threads: 1, timeout_seconds: 900}
  mpi4: {build: mpi, ranks: 4, threads: 1, timeout_seconds: 900}

suites:
  pr:
    scale: small
    cases:
      terrain:
        configurations: [u10m, u3d]
        executions: [serial, mpi4]
```

## Add options without adding a category system

A configuration can set any supported option under its real namelist block.
There is no separate methods file, wind_modes file, category registry, or
automatic Cartesian product. A short name identifies a scientifically useful
combination. Add it to a suite only when ongoing coverage is needed.

For example, an eventual SB40 configuration could override the documented fuel
option and generated categories together. First implement that model support
and validate its meaning; the existing crosswalk does not establish a native
SB40 option. Likewise, a future WRF staggering option should use its actual
namelist name once implemented. UFS mass-centred imports need host-specific
coverage, not a label that assumes all atmospheric winds are staggered.

At present `u10m` and `u3d` change only `wind_vinterp_opt`. Their generated
files contain both representations. The 3D target is **6.096 m**, below the
lowest 10 m mass level, and uses the surface logarithmic profile described in
[cases.md](cases.md).

## Generated values and comparisons

The renderer derives start/end date fields, `atm.kde`, ignition coordinates,
and the `ideal` grid block from the resolved clock and geometry. These are not
independent override locations. All other supported namelist entries are
written directly from `namelist`, with validation before any model launch.
The old template is retained under `templates/namelist.fire.pre-configurations.in`
for provenance and is not read by the harness.

Full test names are `<driver>_<case>_<configuration>_<scale>_<execution>`, for
example `nuopc_terrain_u3d_small_mpi4`. Only runs sharing the same case,
configuration, and scale are compared. Wind choices and numerical schemes are
separate experiments, not numerical references for one another. Resolved
settings are saved in each `result.json`.
