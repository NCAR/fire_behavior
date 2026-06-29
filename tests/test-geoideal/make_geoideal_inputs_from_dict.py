#!/usr/bin/env python3
"""Assemble idealized NetCDF inputs for fire_behavior.exe from dictionaries."""

from __future__ import annotations

import math
import os
import re
from datetime import datetime, timedelta
from pathlib import Path
from typing import Any

import netCDF4
import numpy as np

# =============================================================================
# User-editable case configuration
# =============================================================================

# Edit this dictionary to change the idealized case. The script does not parse
# command-line arguments and does not read reference NetCDF files.
CONFIG: dict[str, Any] = {
    "paths": {
        "geo": Path("geo_em.d01.nc"),
        "wrf": Path("wrf.nc"),
        "namelist": Path("namelist.fire"),
        "update_namelist": True,},
    "time": {
        "start": "2012-06-25_18:00:00",
        "duration_s": 30.0,
        "dt_s": 0.5,
        "atm_interval_s": 1.0,
        "output_interval_s": 10.0,},
    "grid": {
        # WRF global grid dimensions include the staggered end point. The mass
        # dimensions are therefore one less than these values.
        "west_east_grid_dimension": 20,
        "south_north_grid_dimension": 20,
        "bottom_top_grid_dimension": 45,
        "dx_m": 100.0,
        "dy_m": 100.0,
        "sr_x": 4,
        "sr_y": 4,},
    "projection": {
        # Keep a low positive Lambert latitude for near-origin ideal cases.
        # Exactly zero latitude makes the current Lambert cone factor singular.
        "cen_lat": 1.0,
        "cen_lon": 0.0,
        "truelat1": 1.0,
        "truelat2": 1.0,
        "stand_lon": 0.0,
        "map_proj": 1,
        "map_proj_char": "Lambert Conformal",},
    "ignition": {
        # Coordinates are local meters from the lower-left fire-array origin.
        # With the default 80 x 80 fire grid and 25 m fire spacing, the visual
        # midpoint of the fire array is at x = 1000 m, y = 1000 m.
        # Use None to place a point ignition at the fire-array midpoint.
        "start_x_m": 300,
        "start_y_m": 990,
        "end_x_m": 300,
        "end_y_m": 1010,
        "radius_m": 50.0,
        "ros_m_s": 12.55,
        "start_time_s": 0.0,
        "end_time_s": 0.0,},
    "fuel": {
        "background": 101.0,
        "no_fuel": 91.0,
        "no_fuel_patch_radius_cells": 4.0,
        "no_fuel_patch_offset_m": 250.0,},
    "terrain": {
        "height_m": 0.0,
        "dzdx": 0.0,
        "dzdy": 0.0,},
    "atmosphere": {
        "u_m_s": 6.0,
        "v_m_s": 0.0,
        "t2_k": 300.0,
        "q2_kg_kg": 0.006,
        "psfc_pa": 90000.0,
        "znt_m": 0.1,
        "rainc_mm": 0.0,
        "rainnc_mm": 0.0,
        "model_top_m": 6000.0,},}

# =============================================================================
# Editable global attributes
# =============================================================================

# These are user-facing attributes that are not simple dimension derivatives.
# Dimensions, patch ends, subgrid sizes, and repeated WRF projection attributes
# are derived below from CONFIG to avoid inconsistent files.
EDITABLE_GEO_GLOBAL_ATTRS: dict[str, Any] = {
    "TITLE": "IDEALIZED GEO_EM INPUT FOR CFBM TEST-GEOIDEAL",
    "SIMULATION_START_DATE": "0000-00-00_00:00:00",
    "GRIDTYPE": "C",
    "POLE_LAT": np.float32(90.0),
    "POLE_LON": np.float32(0.0),
    "grid_id": np.int32(1),
    "parent_id": np.int32(1),
    "i_parent_start": np.int32(1),
    "j_parent_start": np.int32(1),
    "parent_grid_ratio": np.int32(1),}

EDITABLE_WRF_GLOBAL_ATTRS: dict[str, Any] = {
    "TITLE": "IDEALIZED WRF FORCING FOR CFBM TEST-GEOIDEAL",
    "SIMULATION_INITIALIZATION_TYPE": "IDEALIZED CASE",
    "GRID_ID": np.int32(1),
    "PARENT_ID": np.int32(1),
    "I_PARENT_START": np.int32(1),
    "J_PARENT_START": np.int32(1),
    "PARENT_GRID_RATIO": np.int32(1),}

# =============================================================================
# Constants and small utilities
# =============================================================================

G = 9.81
EARTH_RADIUS_M = 6_370_000.0
DEG_TO_RAD = math.pi / 180.0
RAD_TO_DEG = 180.0 / math.pi
DATE_FORMAT = "%Y-%m-%d_%H:%M:%S"
DATE_STR_LEN = 19


def as_f32(value: float) -> np.float32:
    return np.float32(value)


def as_i32(value: int) -> np.int32:
    return np.int32(value)


def start_datetime() -> datetime:
    return datetime.strptime(CONFIG["time"]["start"], DATE_FORMAT)


def wrf_times() -> list[datetime]:
    start = start_datetime()
    interval = float(CONFIG["time"]["atm_interval_s"])
    duration = float(CONFIG["time"]["duration_s"])
    n_times = int(round(duration / interval)) + 1
    return [start + timedelta(seconds=i * interval) for i in range(n_times)]


def end_datetime() -> datetime:
    return start_datetime() + timedelta(seconds=float(CONFIG["time"]["duration_s"]))


def write_times(var: netCDF4.Variable, times: list[datetime]) -> None:
    data = np.full((len(times), DATE_STR_LEN), b" ", dtype="S1")
    for itime, value in enumerate(times):
        text = value.strftime(DATE_FORMAT).encode("ascii")
        data[itime, :len(text)] = np.frombuffer(text, dtype="S1")
    var[:] = data


# =============================================================================
# Derived grid, projection, and global attributes
# =============================================================================


def derived_grid() -> dict[str, int | float]:
    grid = CONFIG["grid"]
    we_grid = int(grid["west_east_grid_dimension"])
    sn_grid = int(grid["south_north_grid_dimension"])
    bt_grid = int(grid["bottom_top_grid_dimension"])
    sr_x = int(grid["sr_x"])
    sr_y = int(grid["sr_y"])

    return {
        "we_grid": we_grid,
        "sn_grid": sn_grid,
        "bt_grid": bt_grid,
        "we": we_grid - 1,
        "sn": sn_grid - 1,
        "bt": bt_grid - 1,
        "we_stag": we_grid,
        "sn_stag": sn_grid,
        "bt_stag": bt_grid,
        "we_fire": we_grid * sr_x,
        "sn_fire": sn_grid * sr_y,
        "dx": float(grid["dx_m"]),
        "dy": float(grid["dy_m"]),
        "sr_x": sr_x,
        "sr_y": sr_y,}


def derived_projection_attrs() -> dict[str, Any]:
    proj = CONFIG["projection"]
    return {
        "CEN_LAT": as_f32(proj["cen_lat"]),
        "CEN_LON": as_f32(proj["cen_lon"]),
        "TRUELAT1": as_f32(proj["truelat1"]),
        "TRUELAT2": as_f32(proj["truelat2"]),
        "MOAD_CEN_LAT": as_f32(proj["cen_lat"]),
        "STAND_LON": as_f32(proj["stand_lon"]),
        "MAP_PROJ": as_i32(proj["map_proj"]),}


def derived_geo_global_attrs(corner_lats: np.ndarray,
                             corner_lons: np.ndarray) -> dict[str, Any]:
    grid = derived_grid()
    attrs = dict(EDITABLE_GEO_GLOBAL_ATTRS)
    attrs.update({
        "WEST-EAST_GRID_DIMENSION": as_i32(grid["we_grid"]),
        "SOUTH-NORTH_GRID_DIMENSION": as_i32(grid["sn_grid"]),
        "BOTTOM-TOP_GRID_DIMENSION": as_i32(0),
        "WEST-EAST_PATCH_START_UNSTAG": as_i32(1),
        "WEST-EAST_PATCH_END_UNSTAG": as_i32(grid["we"]),
        "WEST-EAST_PATCH_START_STAG": as_i32(1),
        "WEST-EAST_PATCH_END_STAG": as_i32(grid["we_stag"]),
        "SOUTH-NORTH_PATCH_START_UNSTAG": as_i32(1),
        "SOUTH-NORTH_PATCH_END_UNSTAG": as_i32(grid["sn"]),
        "SOUTH-NORTH_PATCH_START_STAG": as_i32(1),
        "SOUTH-NORTH_PATCH_END_STAG": as_i32(grid["sn_stag"]),
        "DX": as_f32(grid["dx"]),
        "DY": as_f32(grid["dy"]),
        "i_parent_end": as_i32(grid["we_grid"]),
        "j_parent_end": as_i32(grid["sn_grid"]),
        "sr_x": as_i32(grid["sr_x"]),
        "sr_y": as_i32(grid["sr_y"]),
        "corner_lats": corner_lats.astype(np.float32),
        "corner_lons": corner_lons.astype(np.float32),})
    attrs.update(derived_projection_attrs())
    return attrs


def derived_wrf_global_attrs() -> dict[str, Any]:
    grid = derived_grid()
    proj_attrs = derived_projection_attrs()
    start_dt = start_datetime()
    start = start_dt.strftime(DATE_FORMAT)

    attrs = dict(EDITABLE_WRF_GLOBAL_ATTRS)
    attrs.update({
        "START_DATE": start,
        "SIMULATION_START_DATE": start,
        "WEST-EAST_GRID_DIMENSION": as_i32(grid["we_grid"]),
        "SOUTH-NORTH_GRID_DIMENSION": as_i32(grid["sn_grid"]),
        "BOTTOM-TOP_GRID_DIMENSION": as_i32(grid["bt_grid"]),
        "DX": as_f32(grid["dx"]),
        "DY": as_f32(grid["dy"]),
        "GRIDTYPE": EDITABLE_GEO_GLOBAL_ATTRS["GRIDTYPE"],
        "DT": as_f32(CONFIG["time"]["dt_s"]),
        "WEST-EAST_PATCH_START_UNSTAG": as_i32(1),
        "WEST-EAST_PATCH_END_UNSTAG": as_i32(grid["we"]),
        "WEST-EAST_PATCH_START_STAG": as_i32(1),
        "WEST-EAST_PATCH_END_STAG": as_i32(grid["we_stag"]),
        "SOUTH-NORTH_PATCH_START_UNSTAG": as_i32(1),
        "SOUTH-NORTH_PATCH_END_UNSTAG": as_i32(grid["sn"]),
        "SOUTH-NORTH_PATCH_START_STAG": as_i32(1),
        "SOUTH-NORTH_PATCH_END_STAG": as_i32(grid["sn_stag"]),
        "BOTTOM-TOP_PATCH_START_UNSTAG": as_i32(1),
        "BOTTOM-TOP_PATCH_END_UNSTAG": as_i32(grid["bt"]),
        "BOTTOM-TOP_PATCH_START_STAG": as_i32(1),
        "BOTTOM-TOP_PATCH_END_STAG": as_i32(grid["bt_stag"]),
        "MAP_PROJ_CHAR": CONFIG["projection"]["map_proj_char"],
        "POLE_LAT": EDITABLE_GEO_GLOBAL_ATTRS["POLE_LAT"],
        "POLE_LON": EDITABLE_GEO_GLOBAL_ATTRS["POLE_LON"],
        "GMT": as_f32(start_dt.hour + start_dt.minute / 60.0 + start_dt.second / 3600.0),
        "JULYR": as_i32(start_dt.year),
        "JULDAY": as_i32(int(start_dt.strftime("%j"))),})
    attrs.update(proj_attrs)
    return attrs


# =============================================================================
# Lambert projection used by the current standalone CFBM code path
# =============================================================================


class LambertProjection:
    """Python equivalent of share/proj_lc_mod.F90 for this test generator."""

    def __init__(self) -> None:
        grid = derived_grid()
        proj = CONFIG["projection"]
        self.cen_lat = float(proj["cen_lat"])
        self.cen_lon = float(proj["cen_lon"])
        self.dx = float(grid["dx"])
        self.dy = float(grid["dy"])
        self.standard_lon = float(proj["stand_lon"])
        self.true_lat_1 = float(proj["truelat1"])
        self.true_lat_2 = float(proj["truelat2"])
        self.nx = int(grid["we"])
        self.ny = int(grid["sn"])
        self.known_i = (self.nx + 1) / 2.0
        self.known_j = (self.ny + 1) / 2.0
        self.hemi = -1.0 if self.true_lat_1 < 0.0 else 1.0
        self.cone_factor = self._calc_cone()
        if abs(self.cone_factor) < 1.0e-7:
            raise ValueError(
                "Lambert cone factor is too close to zero; use a nonzero true latitude.")
        self.pole_i, self.pole_j = self._calc_pole()

    def _calc_cone(self) -> float:
        if abs(self.true_lat_1 - self.true_lat_2) > 0.1:
            cone = math.log10(math.cos(self.true_lat_1 * DEG_TO_RAD)) - math.log10(
                math.cos(self.true_lat_2 * DEG_TO_RAD))
            cone /= math.log10(math.tan(
                (45.0 - abs(self.true_lat_1) / 2.0) * DEG_TO_RAD)) - math.log10(
                    math.tan((45.0 - abs(self.true_lat_2) / 2.0) * DEG_TO_RAD))
            return cone
        return math.sin(abs(self.true_lat_1) * DEG_TO_RAD)

    def _calc_pole(self) -> tuple[float, float]:
        deltalon = self.cen_lon - self.standard_lon
        if deltalon > 180.0:
            deltalon -= 360.0
        if deltalon < -180.0:
            deltalon += 360.0

        rebydx = EARTH_RADIUS_M / self.dx
        rsw = (rebydx * math.cos(self.true_lat_1 * DEG_TO_RAD) / self.cone_factor *
               (math.tan((90.0 * self.hemi - self.cen_lat) * DEG_TO_RAD / 2.0) / math.tan(
                   (90.0 * self.hemi - self.true_lat_1) * DEG_TO_RAD / 2.0))**
               self.cone_factor)
        arg = self.cone_factor * (deltalon * DEG_TO_RAD)
        pole_i = self.hemi * self.known_i - self.hemi * rsw * math.sin(arg)
        pole_j = self.hemi * self.known_j + rsw * math.cos(arg)
        return pole_i, pole_j

    def calc_latlon(self, i: float, j: float) -> tuple[float, float]:
        chi1 = (90.0 - self.hemi * self.true_lat_1) * DEG_TO_RAD
        chi2 = (90.0 - self.hemi * self.true_lat_2) * DEG_TO_RAD

        xx = self.hemi * i - self.pole_i
        yy = self.pole_j - self.hemi * j
        r2 = xx * xx + yy * yy
        r = math.sqrt(r2) / (EARTH_RADIUS_M / self.dx)

        if r2 == 0.0:
            lat = self.hemi * 90.0
            lon = self.standard_lon
        else:
            lon = self.standard_lon + RAD_TO_DEG * math.atan2(self.hemi * xx,
                                                              yy) / self.cone_factor
            lon = math.fmod(lon + 360.0, 360.0)
            if chi1 == chi2:
                chi = 2.0 * math.atan(
                    (r / math.tan(chi1))**(1.0 / self.cone_factor) * math.tan(chi1 * 0.5))
            else:
                chi = 2.0 * math.atan(
                    (r * self.cone_factor / math.sin(chi1))**(1.0 / self.cone_factor) *
                    math.tan(chi1 * 0.5))
            lat = (90.0 - chi * RAD_TO_DEG) * self.hemi

        if lon > 180.0:
            lon -= 360.0
        if lon < -180.0:
            lon += 360.0
        return lat, lon


# =============================================================================
# Coordinate and field construction
# =============================================================================


def mass_latlon() -> tuple[np.ndarray, np.ndarray]:
    grid = derived_grid()
    proj = LambertProjection()
    lat = np.empty((grid["sn"], grid["we"]), dtype=np.float32)
    lon = np.empty_like(lat)
    for j in range(grid["sn"]):
        for i in range(grid["we"]):
            lat[j, i], lon[j, i] = proj.calc_latlon(i + 1.0, j + 1.0)
    return lat, lon


def corner_latlon() -> tuple[np.ndarray, np.ndarray]:
    grid = derived_grid()
    proj = LambertProjection()
    lat = np.empty((grid["sn_stag"], grid["we_stag"]), dtype=np.float32)
    lon = np.empty_like(lat)
    for j in range(grid["sn_stag"]):
        for i in range(grid["we_stag"]):
            lat[j, i], lon[j, i] = proj.calc_latlon(i + 0.5, j + 0.5)
    return lat, lon


def ignition_xy() -> dict[str, float]:
    grid = derived_grid()
    ign = CONFIG["ignition"]
    dx_fire = grid["dx"] / grid["sr_x"]
    dy_fire = grid["dy"] / grid["sr_y"]
    center_x = 0.5 * grid["we_fire"] * dx_fire
    center_y = 0.5 * grid["sn_fire"] * dy_fire

    start_x = center_x if ign["start_x_m"] is None else float(ign["start_x_m"])
    start_y = center_y if ign["start_y_m"] is None else float(ign["start_y_m"])
    end_x = start_x if ign["end_x_m"] is None else float(ign["end_x_m"])
    end_y = start_y if ign["end_y_m"] is None else float(ign["end_y_m"])
    return {
        "start_x": start_x,
        "start_y": start_y,
        "end_x": end_x,
        "end_y": end_y}


def xy_to_latlon(x_m: float, y_m: float) -> tuple[float, float]:
    grid = derived_grid()
    proj = LambertProjection()
    i_atm = x_m / grid["dx"] + 0.5
    # The standalone output grid shows fire-array rows shifted northward from
    # the direct lower-left-to-atmospheric-index conversion. Apply the same
    # offset here so namelist ignition coordinates and geogrid fuel placement
    # refer to the same visual fire-grid row in fire_output_*.nc.
    j_atm = y_m / grid["dy"] + 0.5 + (grid["sn_grid"] - 1) / grid["sr_y"]
    return proj.calc_latlon(i_atm, j_atm)


def ignition_latlon() -> dict[str, float]:
    xy = ignition_xy()
    start_lat, start_lon = xy_to_latlon(xy["start_x"], xy["start_y"])
    end_lat, end_lon = xy_to_latlon(xy["end_x"], xy["end_y"])
    return {
        **xy,
        "start_lat": start_lat,
        "start_lon": start_lon,
        "end_lat": end_lat,
        "end_lon": end_lon,}


def nfuel_cat() -> np.ndarray:
    grid = derived_grid()
    fuel = CONFIG["fuel"]
    atm = CONFIG["atmosphere"]
    ign = ignition_xy()

    out = np.full((grid["sn_fire"], grid["we_fire"]),
                  fuel["background"],
                  dtype=np.float32)

    dx_fire = grid["dx"] / grid["sr_x"]
    dy_fire = grid["dy"] / grid["sr_y"]
    x = (np.arange(grid["we_fire"], dtype=np.float64) + 0.5) * dx_fire
    y = (np.arange(grid["sn_fire"], dtype=np.float64) + 0.5) * dy_fire
    xx, yy = np.meshgrid(x, y)

    wind_norm = math.hypot(float(atm["u_m_s"]), float(atm["v_m_s"]))
    if wind_norm <= 0.0:
        raise ValueError("The downwind no-fuel patch requires a nonzero wind vector.")

    ign_x = 0.5 * (ign["start_x"] + ign["end_x"])
    ign_y = 0.5 * (ign["start_y"] + ign["end_y"])
    target_x = ign_x + fuel["no_fuel_patch_offset_m"] * atm["u_m_s"] / wind_norm
    target_y = ign_y + fuel["no_fuel_patch_offset_m"] * atm["v_m_s"] / wind_norm

    dist2 = (xx - target_x)**2 + (yy - target_y)**2
    center_j, center_i = np.unravel_index(np.argmin(dist2), dist2.shape)
    jj, ii = np.indices(out.shape)
    patch = (ii - center_i)**2 + (jj -
                                  center_j)**2 <= fuel["no_fuel_patch_radius_cells"]**2
    out[patch] = fuel["no_fuel"]
    return out


# =============================================================================
# NetCDF metadata helpers
# =============================================================================


def set_attrs(obj: netCDF4.Dataset | netCDF4.Variable, attrs: dict[str, Any]) -> None:
    for key, value in attrs.items():
        obj.setncattr(key, value)


def make_var(ds: netCDF4.Dataset, name: str, dtype: str, dims: tuple[str, ...],
             attrs: dict[str, Any]) -> netCDF4.Variable:
    var = ds.createVariable(name, dtype, dims)
    set_attrs(var, attrs)
    return var


def geo_var_attrs(description: str, units: str, stagger: str, sr_x: int,
                  sr_y: int) -> dict[str, Any]:
    return {
        "FieldType": as_i32(104),
        "MemoryOrder": "XY ",
        "units": units,
        "description": description,
        "stagger": stagger,
        "sr_x": as_i32(sr_x),
        "sr_y": as_i32(sr_y),}


def wrf_var_attrs(description: str, units: str, stagger: str,
                  coordinates: str) -> dict[str, Any]:
    return {
        "FieldType": as_i32(104),
        "MemoryOrder": "XYZ" if stagger in {"X", "Y", "Z"} else "XY ",
        "description": description,
        "units": units,
        "stagger": stagger,
        "coordinates": coordinates,}


def replace_file(path: str | Path, build_func) -> None:
    path = Path(path)
    tmp = path.with_suffix(path.suffix + ".tmp")
    build_func(tmp)
    os.replace(tmp, path)


# =============================================================================
# NetCDF file assembly
# =============================================================================


def write_geo(path: Path) -> None:
    grid = derived_grid()
    lat_m, lon_m = mass_latlon()
    lat_c, lon_c = corner_latlon()
    corner_lats = np.array([lat_c[0, 0], lat_c[-1, 0], lat_c[-1, -1], lat_c[0, -1]],
                           dtype=np.float32)
    corner_lons = np.array([lon_c[0, 0], lon_c[-1, 0], lon_c[-1, -1], lon_c[0, -1]],
                           dtype=np.float32)

    def build(tmp: Path) -> None:
        with netCDF4.Dataset(tmp, "w", format="NETCDF4_CLASSIC") as ds:
            ds.createDimension("Time", None)
            ds.createDimension("DateStrLen", DATE_STR_LEN)
            ds.createDimension("south_north", grid["sn"])
            ds.createDimension("west_east", grid["we"])
            ds.createDimension("south_north_stag", grid["sn_stag"])
            ds.createDimension("west_east_stag", grid["we_stag"])
            ds.createDimension("south_north_subgrid", grid["sn_fire"])
            ds.createDimension("west_east_subgrid", grid["we_fire"])
            set_attrs(ds, derived_geo_global_attrs(corner_lats, corner_lons))

            times = ds.createVariable("Times", "S1", ("Time", "DateStrLen"))
            write_times(times, [start_datetime()])

            make_var(
                ds, "XLAT_M", "f4", ("Time", "south_north", "west_east"),
                geo_var_attrs("Latitude on mass grid", "degrees latitude", "M", 1,
                              1))[:] = lat_m[np.newaxis, :, :]
            make_var(
                ds, "XLONG_M", "f4", ("Time", "south_north", "west_east"),
                geo_var_attrs("Longitude on mass grid", "degrees longitude", "M", 1,
                              1))[:] = lon_m[np.newaxis, :, :]
            make_var(
                ds, "XLAT_C", "f4", ("Time", "south_north_stag", "west_east_stag"),
                geo_var_attrs("Latitude at grid cell corners", "degrees latitude",
                              "CORNER", 1, 1))[:] = lat_c[np.newaxis, :, :]
            make_var(
                ds, "XLONG_C", "f4", ("Time", "south_north_stag", "west_east_stag"),
                geo_var_attrs("Longitude at grid cell corners", "degrees longitude",
                              "CORNER", 1, 1))[:] = lon_c[np.newaxis, :, :]
            make_var(
                ds, "ZSF", "f4", ("Time", "south_north_subgrid", "west_east_subgrid"),
                geo_var_attrs("Topography height", "meters MSL", "M", grid["sr_x"],
                              grid["sr_y"]))[:] = CONFIG["terrain"]["height_m"]
            make_var(ds, "DZDXF", "f4",
                     ("Time", "south_north_subgrid", "west_east_subgrid"),
                     geo_var_attrs("df/dx", "-", "M", grid["sr_x"],
                                   grid["sr_y"]))[:] = CONFIG["terrain"]["dzdx"]
            make_var(ds, "DZDYF", "f4",
                     ("Time", "south_north_subgrid", "west_east_subgrid"),
                     geo_var_attrs("df/dy", "-", "M", grid["sr_x"],
                                   grid["sr_y"]))[:] = CONFIG["terrain"]["dzdy"]
            make_var(
                ds, "NFUEL_CAT", "f4",
                ("Time", "south_north_subgrid", "west_east_subgrid"),
                geo_var_attrs("Dominant category", "category", "M", grid["sr_x"],
                              grid["sr_y"]))[:] = nfuel_cat()[np.newaxis, :, :]

    replace_file(path, build)


def write_wrf(path: Path) -> None:
    grid = derived_grid()
    atm = CONFIG["atmosphere"]
    times = wrf_times()
    lat, lon = mass_latlon()
    z_w = np.linspace(0.0, float(atm["model_top_m"]), grid["bt_stag"], dtype=np.float32)
    phb_profile = (G * z_w).reshape((1, grid["bt_stag"], 1, 1))

    def build(tmp: Path) -> None:
        with netCDF4.Dataset(tmp, "w", format="NETCDF4_CLASSIC") as ds:
            ds.createDimension("Time", None)
            ds.createDimension("DateStrLen", DATE_STR_LEN)
            ds.createDimension("south_north", grid["sn"])
            ds.createDimension("west_east", grid["we"])
            ds.createDimension("bottom_top", grid["bt"])
            ds.createDimension("west_east_stag", grid["we_stag"])
            ds.createDimension("south_north_stag", grid["sn_stag"])
            ds.createDimension("bottom_top_stag", grid["bt_stag"])
            set_attrs(ds, derived_wrf_global_attrs())

            time_var = ds.createVariable("Times", "S1", ("Time", "DateStrLen"))
            write_times(time_var, times)
            nt = len(times)

            make_var(
                ds, "XLAT", "f4", ("Time", "south_north", "west_east"),
                wrf_var_attrs("LATITUDE, SOUTH IS NEGATIVE", "degree_north", "",
                              "XLONG XLAT"))[:] = np.broadcast_to(
                                  lat, (nt, grid["sn"], grid["we"]))
            make_var(
                ds, "XLONG", "f4", ("Time", "south_north", "west_east"),
                wrf_var_attrs("LONGITUDE, WEST IS NEGATIVE", "degree_east", "",
                              "XLONG XLAT"))[:] = np.broadcast_to(
                                  lon, (nt, grid["sn"], grid["we"]))
            make_var(ds, "T2", "f4", ("Time", "south_north", "west_east"),
                     wrf_var_attrs("TEMP at 2 M", "K", "",
                                   "XLONG XLAT XTIME"))[:] = atm["t2_k"]
            make_var(ds, "Q2", "f4", ("Time", "south_north", "west_east"),
                     wrf_var_attrs("QV at 2 M", "kg kg-1", "",
                                   "XLONG XLAT XTIME"))[:] = atm["q2_kg_kg"]
            make_var(ds, "PSFC", "f4", ("Time", "south_north", "west_east"),
                     wrf_var_attrs("SFC PRESSURE", "Pa", "",
                                   "XLONG XLAT XTIME"))[:] = atm["psfc_pa"]
            make_var(
                ds, "RAINC", "f4", ("Time", "south_north", "west_east"),
                wrf_var_attrs("ACCUMULATED TOTAL CUMULUS PRECIPITATION", "mm", "",
                              "XLONG XLAT XTIME"))[:] = atm["rainc_mm"]
            make_var(
                ds, "RAINNC", "f4", ("Time", "south_north", "west_east"),
                wrf_var_attrs("ACCUMULATED TOTAL GRID SCALE PRECIPITATION", "mm", "",
                              "XLONG XLAT XTIME"))[:] = atm["rainnc_mm"]
            make_var(
                ds, "U", "f4", ("Time", "bottom_top", "south_north", "west_east_stag"),
                wrf_var_attrs("x-wind component", "m s-1", "X",
                              "XLONG_U XLAT_U XTIME"))[:] = atm["u_m_s"]
            make_var(
                ds, "V", "f4", ("Time", "bottom_top", "south_north_stag", "west_east"),
                wrf_var_attrs("y-wind component", "m s-1", "Y",
                              "XLONG_V XLAT_V XTIME"))[:] = atm["v_m_s"]
            make_var(
                ds, "PH", "f4", ("Time", "bottom_top_stag", "south_north", "west_east"),
                wrf_var_attrs("perturbation geopotential", "m2 s-2", "Z",
                              "XLONG XLAT XTIME"))[:] = 0.0
            make_var(
                ds, "PHB", "f4", ("Time", "bottom_top_stag", "south_north", "west_east"),
                wrf_var_attrs("base-state geopotential", "m2 s-2", "Z",
                              "XLONG XLAT XTIME"))[:] = np.broadcast_to(
                                  phb_profile,
                                  (nt, grid["bt_stag"], grid["sn"], grid["we"]))
            make_var(
                ds, "ZNT", "f4", ("Time", "south_north", "west_east"),
                wrf_var_attrs("TIME-VARYING ROUGHNESS LENGTH", "m", "",
                              "XLONG XLAT XTIME"))[:] = atm["znt_m"]

    replace_file(path, build)


# =============================================================================
# Namelist synchronization and execution entry point
# =============================================================================


def update_namelist(path: str | Path) -> None:
    path = Path(path)
    ign = ignition_latlon()
    start = start_datetime()
    end = end_datetime()
    replacements = {
        "start_year": start.year,
        "start_month": start.month,
        "start_day": start.day,
        "start_hour": start.hour,
        "start_minute": start.minute,
        "start_second": start.second,
        "end_year": end.year,
        "end_month": end.month,
        "end_day": end.day,
        "end_hour": end.hour,
        "end_minute": end.minute,
        "end_second": end.second,
        "dt": CONFIG["time"]["dt_s"],
        "interval_output": CONFIG["time"]["output_interval_s"],
        "interval_atm": CONFIG["time"]["atm_interval_s"],
        "kde": derived_grid()["bt_grid"],
        "fire_ignition_start_lat1": ign["start_lat"],
        "fire_ignition_start_lon1": ign["start_lon"],
        "fire_ignition_end_lat1": ign["end_lat"],
        "fire_ignition_end_lon1": ign["end_lon"],
        "fire_ignition_radius1": CONFIG["ignition"]["radius_m"],
        "fire_ignition_ros1": CONFIG["ignition"]["ros_m_s"],
        "fire_ignition_start_time1": CONFIG["ignition"]["start_time_s"],
        "fire_ignition_end_time1": CONFIG["ignition"]["end_time_s"],}

    text = path.read_text()
    for key, value in replacements.items():
        pattern = re.compile(rf"^(\s*{re.escape(key)}\s*=\s*)[-+0-9.Ee]+(\s*,?.*)$",
                             re.MULTILINE)
        text, n_match = pattern.subn(rf"\g<1>{float(value):.8g}\2", text, count=1)
        if n_match != 1:
            raise ValueError(f"Could not find exactly one {key} entry in {path}")
    path.write_text(text)


def main() -> None:
    paths = CONFIG["paths"]
    write_geo(paths["geo"])
    write_wrf(paths["wrf"])
    if paths["update_namelist"]:
        update_namelist(paths["namelist"])

    grid = derived_grid()
    ign = ignition_latlon()
    print("Generated dictionary-configured geoideal inputs")
    print(f"  geo: {paths['geo']}")
    print(f"  wrf: {paths['wrf']}")
    print(
        f"  mass grid: {grid['we']} x {grid['sn']}, fire grid: {grid['we_fire']} x {grid['sn_fire']}"
    )
    print(
        f"  dx, dy: {grid['dx']:g}, {grid['dy']:g} m; sr_x, sr_y: {grid['sr_x']}, {grid['sr_y']}"
    )
    print(f"  ignition xy: ({ign['start_x']:.3f}, {ign['start_y']:.3f}) m")
    print(f"  ignition lat/lon: ({ign['start_lat']:.8f}, {ign['start_lon']:.8f})")


if __name__ == "__main__":
    main()
