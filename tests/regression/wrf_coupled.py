#!/usr/bin/env python3
# Copyright 2026 Research Applications Laboratory (RAL), NCAR, UCAR.
# Created on 2026-10-20 by the CFBM development team.
# run python -B tests/regression/regression.py case --driver wrf --help
"""Generate and run a WRF-host terrain case using WRF's EM_FIRE initializer."""
from __future__ import annotations

import datetime as dt
from fractions import Fraction
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import time
from typing import Any

import netCDF4
import numpy as np

from config import load_yaml, resolve_spec, resolved_clock
from generate_inputs import (DATE_FORMAT, LambertProjection, _fields,
                             generate_inputs, sha256_file)
from render_namelist import render_namelist, fortran_value
from reports import print_result, write_result

#--------------------------------------------------------------------------------
# Host initialization and boundary settings
#--------------------------------------------------------------------------------


def validate_wrf_mpi_layout(spec: dict[str, Any]) -> tuple[int, int]:
    """Select a rectangular decomposition with at least ten mass cells per tile."""
    ranks = spec['execution']['ranks']
    if spec['execution']['build'] != 'mpi' or spec['execution']['threads'] != 1:
        raise ValueError('The WRF suite currently requires an MPI-only host')
    ny = int(np.sqrt(ranks))
    while ranks % ny:
        ny -= 1
    nx = ranks // ny
    grid = spec['inputs']['grid']
    if min((grid['nx'] - 1) // nx, (grid['ny'] - 1) // ny) < 10:
        raise ValueError(
            'WRF decomposition requires at least ten cells per tile')
    return nx, ny


def host_namelist(spec: dict[str, Any], initialize: bool) -> str:
    """Describe one WRF domain and separate initialization from CFBM execution."""
    grid = spec['inputs']['grid']
    if grid['sr_x'] != 1 or grid['sr_y'] != 1:
        raise ValueError(
            'The generated WRF case currently requires sr_x=sr_y=1')
    seconds, steps = resolved_clock(spec)
    timestep = Fraction(seconds, steps)
    px, py = (1, 1) if initialize else validate_wrf_mpi_layout(spec)
    blocks = {
        'time_control': {
            'run_seconds': spec['duration_seconds'],
            'history_interval_s': spec['namelist']['time']['interval_output'],
            'frames_per_outfile': 1,
            'restart': False,
            'io_form_history': 2,
            'io_form_input': 2,
            'io_form_restart': 2,
            'io_form_boundary': 2,
        },
        'domains': {
            'time_step': 0,
            'time_step_fract_num': timestep.numerator,
            'time_step_fract_den': timestep.denominator,
            'max_dom': 1,
            'e_we': grid['nx'],
            'e_sn': grid['ny'],
            'e_vert': spec['inputs']['wrf']['e_vert'],
            'dx': grid['dx_m'],
            'dy': grid['dy_m'],
            'ztop': spec['inputs']['wrf']['ztop'],
            'grid_id': 1,
            'parent_id': 0,
            'i_parent_start': 1,
            'j_parent_start': 1,
            'parent_grid_ratio': 1,
            'parent_time_step_ratio': 1,
            'sr_x': 1,
            'sr_y': 1,
            'numtiles': spec['namelist']['time']['num_tiles'],
            'tile_strategy': spec['namelist']['time']['tile_strategy'],
            'nproc_x': px,
            'nproc_y': py,
        },
        'physics': {
            'mp_physics': 0,
            'ra_lw_physics': 0,
            'ra_sw_physics': 0,
            'sf_sfclay_physics': 1,
            'sf_surface_physics': 2,
            'bl_pbl_physics': 0,
            'cu_physics': 0,
            'num_soil_layers': 4,
            'num_land_cat': 24,
            'isfflx': 1,
        },
        'dynamics': {
            'hybrid_opt': 0,
            'rk_ord': 3,
            'diff_opt': 2,
            'km_opt': 2,
            'damp_opt': 0,
            'time_step_sound': 4,
            'moist_adv_opt': 1,
            'tracer_opt': 0,
        },
        'bdy_control': {
            'open_xs': True,
            'open_xe': True,
            'open_ys': True,
            'open_ye': True,
        },
        'namelist_quilt': {
            'nio_tasks_per_group': 0,
            'nio_groups': 1,
        },
        'fire': {
            'ifire': 2 if initialize else 1,
            'fire_fuel_read': 2,
            'fire_fmc_read': 0,
            'fire_num_ignitions': 0,
            'fire_read_atm_ht': True,
            'fire_read_fire_ht': True,
            'fire_read_fire_grad': True,
            'fire_mountain_type': 0,
            'fire_topo_from_atm': 0,
            'delt_perturbation': 0.0,
            'sfc_full_init': True,
            'sfc_lu_index': 7,
            'sfc_ivgtyp': 7,
            'sfc_isltyp': 4,
            'sfc_tsk': spec['inputs']['atmosphere']['temperature_start_k'],
            'sfc_tmn': spec['inputs']['atmosphere']['temperature_start_k'],
            'sfc_vegfra': 50.0,
            'sfc_canwat': 0.0,
            # Resolve the terrain/u3d wind profile near the surface. The
            # initializer's default exponential eta grid puts k=1 near 160 m.
            'stretch_grd': True,
            'stretch_hyp': True,
            'z_grd_scale': spec['inputs']['wrf']['z_grd_scale'],
        },
    }
    for endpoint in ('start', 'end'):
        date = dt.datetime.strptime(spec[endpoint], DATE_FORMAT)
        for part in ('year', 'month', 'day', 'hour', 'minute', 'second'):
            blocks['time_control'][f'{endpoint}_{part}'] = getattr(date, part)
    lines = []
    for block, values in blocks.items():
        lines.append(f'&{block}')
        lines.extend(f' {name} = {fortran_value(value)},'
                     for name, value in values.items())
        lines.append('/\n')
    return '\n'.join(lines)


def stage_initializer(directory: Path, spec: dict[str, Any],
                      source: Path) -> None:
    """Write deterministic terrain and sounding inputs without external datasets."""
    fields = _fields(spec)
    for name, values in {
            'input_ht': fields['ZSF'],
            'input_zsf': fields['ZSF'],
            'input_dzdxf': fields['DZDXF'],
            'input_dzdyf': fields['DZDYF'],
            'input_fc': fields['NFUEL_CAT'],
    }.items():
        # WRF's reader iterates i outside j despite describing rows in its header.
        np.savetxt(directory / name,
                   values.T,
                   fmt='%.9g',
                   comments='',
                   header=f'{values.shape[1]} {values.shape[0]}')
    forcing = spec['inputs']['atmosphere']
    qv = forcing['mixing_ratio_start_kg_kg'] * 1000.0
    host = spec['inputs']['wrf']
    theta = host['theta_surface_k']
    sounding = [f"{forcing['surface_pressure_pa'] / 100.0} {theta} {qv}"]
    # Extend the sounding above the model top for hydrostatic interpolation.
    for height in np.linspace(0.0, host['ztop'] + 4000.0, 121):
        sounding.append(
            f"{height} {theta + host['theta_gradient_k_m'] * height} "
            f'{qv} 10.0 10.0')
    (directory / 'input_sounding').write_text('\n'.join(sounding) + '\n')
    (directory / 'namelist.input').write_text(host_namelist(spec, True))
    shutil.copy2(source / 'test/em_fire/namelist.fire',
                 directory / 'namelist.fire')
    for table in (source / 'run').iterdir():
        if table.is_file() and not (directory / table.name).exists():
            (directory / table.name).symlink_to(table.resolve())


def complete_host_input(path: Path, spec: dict[str, Any]) -> None:
    """Apply Lambert geometry and the shared AGL wind profile to WRF's columns."""
    grid, projection = spec['inputs']['grid'], spec['inputs']['projection']
    forcing = spec['inputs']['atmosphere']
    proj = LambertProjection(grid['nx'] - 1, grid['ny'] - 1, grid['dx_m'],
                             grid['dy_m'], projection)
    with netCDF4.Dataset(path, 'r+') as dataset:
        for key, value in {
                'MAP_PROJ': 1,
                'CEN_LAT': projection['cen_lat'],
                'CEN_LON': projection['cen_lon'],
                'MOAD_CEN_LAT': projection['cen_lat'],
                'STAND_LON': projection['stand_lon'],
                'TRUELAT1': projection['true_lat_1'],
                'TRUELAT2': projection['true_lat_2'],
        }.items():
            # WRF's metadata readers require NC_INT/NC_FLOAT, not Python's
            # default int64/float64 attribute types in a NetCDF4 file.
            dataset.setncattr(
                key,
                np.int32(value) if key == 'MAP_PROJ' else np.float32(value))
        dataset.setncattr('MAP_PROJ_CHAR', 'Lambert Conformal')
        dataset.setncattr('DT', np.float32(spec['namelist']['time']['dt']))
        for suffix, offset_i, offset_j in (('', 1.0, 1.0), ('_U', 0.5, 1.0),
                                           ('_V', 1.0, 0.5)):
            lat_var = dataset[f'XLAT{suffix}']
            lat = np.empty(lat_var.shape[-2:])
            lon = np.empty_like(lat)
            for j, i in np.ndindex(lat.shape):
                lat[j, i], lon[j, i] = proj.latlon(i + offset_i, j + offset_j)
            lat_var[0] = lat
            dataset[f'XLONG{suffix}'][0] = lon
            phi = np.radians(proj.hemi * lat)
            phi1 = np.radians(abs(projection['true_lat_1']))
            scale = np.cos(phi1) / np.cos(phi) * (
                np.tan(np.pi / 4 - phi / 2) /
                np.tan(np.pi / 4 - phi1 / 2))**proj.cone
            stagger = suffix.removeprefix('_') or 'M'
            for name in (f'MAPFAC_{stagger}', f'MAPFAC_{stagger}X',
                         f'MAPFAC_{stagger}Y'):
                if name in dataset.variables:
                    dataset[name][0] = scale
            if not suffix:
                angle = proj.cone * np.radians(lon - projection['stand_lon'])
                dataset['SINALPHA'][0] = np.sin(angle)
                dataset['COSALPHA'][0] = np.cos(angle)
                dataset['F'][0] = 2 * 7.292e-5 * np.sin(np.radians(lat))
        height = (dataset['PH'][0] + dataset['PHB'][0]) / 9.81
        height = 0.5 * (height[:-1] + height[1:]) - dataset['HGT'][0]
        profile_height = np.asarray(forcing['height_interfaces_m'])
        profile_height = 0.5 * (profile_height[:-1] + profile_height[1:])
        for name, profile, axis in (('U', 'u_profile_m_s', 2),
                                    ('V', 'v_profile_m_s', 1)):
            pads = [(0, 0)] * 3
            pads[axis] = (1, 1)
            extended = np.pad(height, pads, mode='edge')
            left, right = [slice(None)] * 3, [slice(None)] * 3
            left[axis], right[axis] = slice(None, -1), slice(1, None)
            face_height = 0.5 * (extended[tuple(left)] + extended[tuple(right)])
            terrain = dataset['HGT'][0]
            terrain_pads = [(0, 0), (0, 0)]
            terrain_pads[axis - 1] = (1, 1)
            extended_terrain = np.pad(terrain, terrain_pads, mode='edge')
            face_terrain = 0.5 * (extended_terrain[tuple(left[1:])] +
                                  extended_terrain[tuple(right[1:])])
            factor = 1 + forcing['wind_terrain_gradient_per_m'] * (
                face_terrain - spec['inputs']['terrain']['base_elevation_m'])
            dataset[name][0] = np.interp(face_height, profile_height,
                                         forcing[profile]) * factor
        # ZNT is diagnosed by WRF surface physics and is not an input field.


def run_wrf_case(args: Any) -> dict[str, Any]:
    """Keep host initialization, execution, and checks in the existing case harness."""
    from run_case import run_model, source_identity, SOURCE_ROOT
    if Path('/glade').exists() and not os.environ.get('PBS_JOBID'):
        raise RuntimeError('Run WRF integrations inside a PBS allocation')
    spec = resolve_spec(load_yaml(args.config), args.case, args.scale,
                        args.configuration, args.execution)
    if not spec['identity']['configuration'].startswith('wrf_'):
        raise ValueError('WRF requires an explicit wrf_ configuration')
    if spec['namelist']['fire']['fire_is_real_perim']:
        raise ValueError(
            'The WRF interface does not support observed perimeters')
    root = Path(os.environ.get('CFBM_RUN_ROOT', str(args.run_root))).resolve()
    if root.is_relative_to(SOURCE_ROOT) and not root.is_relative_to(
            SOURCE_ROOT / 'build'):
        raise ValueError(
            'Keep generated runs in build/ or outside the checkout')
    root.mkdir(parents=True, exist_ok=True)
    name = f"wrf_{args.case}_{args.configuration}_{args.scale}_{args.execution}"
    directory = Path(tempfile.mkdtemp(prefix=name + '-', dir=root))
    result = dict(name=name,
                  directory=str(directory),
                  driver='wrf',
                  source=source_identity(),
                  spec=spec,
                  reasons=[],
                  executable=str(args.executable.resolve()),
                  executable_sha256=sha256_file(args.executable),
                  initializer_sha256=sha256_file(args.wrf_ideal),
                  host_source=source_identity(args.wrf_source),
                  host_cfbm_source=source_identity(args.wrf_source /
                                                   'phys/fire_behavior'),
                  environment=dict(
                      PBS_JOBID=os.environ.get('PBS_JOBID'),
                      LOADEDMODULES=os.environ.get('LOADEDMODULES')))
    started = time.monotonic()
    try:
        spec, _ = generate_inputs(spec, directory)
        result['spec'] = spec
        initial = directory / 'initialization'
        initial.mkdir()
        stage_initializer(initial, spec, args.wrf_source.resolve(strict=True))
        environment = dict(os.environ, OMP_NUM_THREADS='1')
        launcher = [args.launcher, *args.launcher_arg, args.process_flag]
        init_command = [
            *launcher, '1',
            str(args.wrf_ideal.resolve(strict=True)), *args.launcher_post_arg
        ]
        result['initialization'] = run_model(init_command, initial, environment,
                                             600, 'wrf_ideal')
        if result['initialization']['reasons']:
            raise RuntimeError(str(result['initialization']['reasons']))
        shutil.copy2(initial / 'wrfinput_d01', directory / 'wrfinput_d01')
        complete_host_input(directory / 'wrfinput_d01', spec)
        (directory / 'namelist.input').write_text(host_namelist(spec, False))
        (directory / 'namelist.cfbm').write_text(render_namelist(spec))
        for table in initial.iterdir():
            if table.is_symlink() and not (directory / table.name).exists():
                (directory / table.name).symlink_to(table.resolve())
        command = [
            *launcher,
            str(spec['execution']['ranks']),
            str(args.executable.resolve()), *args.launcher_post_arg
        ]
        result.update(
            run_model(command, directory, environment,
                      spec['execution']['timeout_seconds'], 'wrf'))
        result['expected_fields'] = load_yaml(
            args.config)['expected_output_fields']
        result['validation'] = check_wrf_outputs(directory, spec,
                                                 result['expected_fields'])
        result['expected_outputs'] = result['validation']['expected_outputs']
        result['reasons'].extend(result['validation']['reasons'])
        result['host_input_sha256'] = sha256_file(directory / 'wrfinput_d01')
    except (OSError, ValueError, KeyError, IndexError, RuntimeError,
            subprocess.SubprocessError) as error:
        result['reasons'].append(f'{type(error).__name__}: {error}')
    result['elapsed_seconds'] = time.monotonic() - started
    result['pass'] = not result['reasons']
    write_result(directory / 'result.json', result)
    print_result(result, directory / 'result.json')
    return result


def check_wrf_outputs(
        directory: Path,
        spec: dict[str, Any],
        expected_fields: list[str] | None = None) -> dict[str, Any]:
    """Require every scheduled fire output, finite evolving fields, and host feedback."""
    start = dt.datetime.strptime(spec['start'], DATE_FORMAT)
    interval = int(spec['namelist']['time']['interval_output'])
    times = [
        start + dt.timedelta(seconds=s)
        for s in range(interval,
                       int(spec['duration_seconds']) + 1, interval)
    ]
    expected = [
        f"fire_output_{value.strftime(DATE_FORMAT)}.nc" for value in times
    ]
    actual = sorted(p.name for p in directory.glob('fire_output_*.nc'))
    reasons = []
    if actual != expected:
        reasons.append(f'Fire output inventory differs: {actual} != {expected}')
    fluxes, fuel, feedback_errors = [], [], []
    for filename in actual:
        with netCDF4.Dataset(directory / filename) as data:
            if expected_fields is not None and set(
                    data.variables) != set(expected_fields):
                reasons.append(f'Fire field inventory differs in {filename}')
            for variable in data.variables.values():
                if np.issubdtype(variable.dtype, np.number):
                    if not np.isfinite(variable[:].compressed()).all():
                        reasons.append(
                            f'Nonfinite {variable.name} in {filename}')
            fluxes.append(float(data['fgrnhfx'][:].max()))
            fuel.append(float(data['fuel_frac'][:].min()))
            timestamp = filename.removeprefix('fire_output_').removesuffix(
                '.nc')
            host = directory / f'wrfout_d01_{timestamp}'
            if not host.is_file():
                reasons.append(f'Missing scheduled WRF output: {host.name}')
                continue
            with netCDF4.Dataset(host) as atmosphere:
                recorded = atmosphere['Times'][0].tobytes().decode('ascii')
                if recorded != timestamp:
                    reasons.append(f'Incorrect WRF timestamp in {host.name}')
                for name in ('U', 'V', 'T', 'QVAPOR', 'T2', 'Q2', 'PSFC',
                             'GRNHFX', 'GRNQFX'):
                    values = atmosphere[name][:]
                    if np.ma.count_masked(
                            values) or not np.isfinite(values).all():
                        reasons.append(
                            f'Invalid host field {name} in {host.name}')
                for name in ('Q2', 'QVAPOR'):
                    if atmosphere[name][:].min() < 0:
                        reasons.append(f'Negative host {name} in {host.name}')
                for name in ('T2', 'PSFC'):
                    if atmosphere[name][:].min() <= 0:
                        reasons.append(
                            f'Nonpositive host {name} in {host.name}')
                # With sr_x=sr_y=1, Provide_atm_feedback copies each step's
                # flux exactly on ids+1:ide-2, jds+1:jde-2. Testing every
                # output detects stale or accumulated exchange-window flux.
                for fire_name, host_name in (('fgrnhfx', 'GRNHFX'), ('fgrnqfx',
                                                                     'GRNQFX')):
                    host_flux = atmosphere[host_name][0]
                    ny, nx = host_flux.shape
                    fire_flux = data[fire_name][1:ny - 1, 1:nx - 1]
                    error = float(
                        np.max(np.abs(fire_flux - host_flux[1:-1, 1:-1])))
                    feedback_errors.append(error)
                    if error != 0:
                        reasons.append(
                            f'{host_name} differs from current fire flux in {host.name}'
                        )
    if not fluxes or max(fluxes) <= 0 or min(fuel) >= 1:
        reasons.append(
            'Fire consumption and positive heat flux were not observed')
    if not (directory / f"wrfout_d01_{spec['end']}").is_file():
        reasons.append('WRF did not write the exact final time')
    seconds, steps = resolved_clock(spec)
    expected_steps = int(spec['duration_seconds']) * steps // seconds
    log = directory / 'rsl.error.0000'
    actual_steps = log.read_text().count(
        'Timing for main:') if log.exists() else 0
    if actual_steps != expected_steps:
        reasons.append(
            f'WRF step count differs: {actual_steps} != {expected_steps}')
    return {
        'pass': not reasons,
        'reasons': reasons,
        'expected_outputs': expected,
        'fire_flux_maxima': fluxes,
        'minimum_fuel_fractions': fuel,
        'feedback_max_absolute_error': max(feedback_errors, default=None),
        'host_steps': actual_steps,
    }
