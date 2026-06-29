#!/usr/bin/env python3
"""Generate reduced idealized NetCDF inputs for fire_behavior.exe."""

from __future__ import annotations

import argparse
import math
import os
import re
from pathlib import Path
from typing import Iterable

import netCDF4
import numpy as np


G = 9.81
EARTH_RADIUS_M = 6_370_000.0
DEG_TO_RAD = math.pi / 180.0
RAD_TO_DEG = 180.0 / math.pi

GEO_VARS = (
    "Times",
    "XLAT_M",
    "XLONG_M",
    "XLAT_C",
    "XLONG_C",
    "ZSF",
    "DZDXF",
    "DZDYF",
    "NFUEL_CAT",
)

WRF_VARS = (
    "Times",
    "XLAT",
    "XLONG",
    "T2",
    "Q2",
    "PSFC",
    "RAINC",
    "RAINNC",
    "U",
    "V",
    "PH",
    "PHB",
    "ZNT",
)

GEO_GLOBAL_ATTRS = (
    "TITLE",
    "SIMULATION_START_DATE",
    "WEST-EAST_GRID_DIMENSION",
    "SOUTH-NORTH_GRID_DIMENSION",
    "BOTTOM-TOP_GRID_DIMENSION",
    "WEST-EAST_PATCH_START_UNSTAG",
    "WEST-EAST_PATCH_END_UNSTAG",
    "WEST-EAST_PATCH_START_STAG",
    "WEST-EAST_PATCH_END_STAG",
    "SOUTH-NORTH_PATCH_START_UNSTAG",
    "SOUTH-NORTH_PATCH_END_UNSTAG",
    "SOUTH-NORTH_PATCH_START_STAG",
    "SOUTH-NORTH_PATCH_END_STAG",
    "GRIDTYPE",
    "DX",
    "DY",
    "CEN_LAT",
    "CEN_LON",
    "MAP_PROJ",
    "TRUELAT1",
    "TRUELAT2",
    "MOAD_CEN_LAT",
    "STAND_LON",
    "POLE_LAT",
    "POLE_LON",
    "corner_lats",
    "corner_lons",
    "grid_id",
    "parent_id",
    "i_parent_start",
    "j_parent_start",
    "i_parent_end",
    "j_parent_end",
    "parent_grid_ratio",
    "sr_x",
    "sr_y",
)

WRF_GLOBAL_ATTRS = (
    "TITLE",
    "START_DATE",
    "SIMULATION_START_DATE",
    "WEST-EAST_GRID_DIMENSION",
    "SOUTH-NORTH_GRID_DIMENSION",
    "BOTTOM-TOP_GRID_DIMENSION",
    "DX",
    "DY",
    "GRIDTYPE",
    "DT",
    "SIMULATION_INITIALIZATION_TYPE",
    "WEST-EAST_PATCH_START_UNSTAG",
    "WEST-EAST_PATCH_END_UNSTAG",
    "WEST-EAST_PATCH_START_STAG",
    "WEST-EAST_PATCH_END_STAG",
    "SOUTH-NORTH_PATCH_START_UNSTAG",
    "SOUTH-NORTH_PATCH_END_UNSTAG",
    "SOUTH-NORTH_PATCH_START_STAG",
    "SOUTH-NORTH_PATCH_END_STAG",
    "BOTTOM-TOP_PATCH_START_UNSTAG",
    "BOTTOM-TOP_PATCH_END_UNSTAG",
    "BOTTOM-TOP_PATCH_START_STAG",
    "BOTTOM-TOP_PATCH_END_STAG",
    "GRID_ID",
    "PARENT_ID",
    "I_PARENT_START",
    "J_PARENT_START",
    "PARENT_GRID_RATIO",
    "CEN_LAT",
    "CEN_LON",
    "TRUELAT1",
    "TRUELAT2",
    "MOAD_CEN_LAT",
    "STAND_LON",
    "POLE_LAT",
    "POLE_LON",
    "GMT",
    "JULYR",
    "JULDAY",
    "MAP_PROJ",
    "MAP_PROJ_CHAR",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Reduce and idealize geo_em.d01.nc and wrf.nc for the "
            "test-geoideal standalone fire_behavior.exe case."
        )
    )
    parser.add_argument("--source-geo", type=Path, default=Path("geo_em.d01.nc"))
    parser.add_argument("--source-wrf", type=Path, default=Path("wrf.nc"))
    parser.add_argument("--output-geo", type=Path, default=Path("geo_em.d01.nc"))
    parser.add_argument("--output-wrf", type=Path, default=Path("wrf.nc"))
    parser.add_argument("--namelist", type=Path, default=Path("namelist.fire"))
    parser.add_argument("--no-update-namelist", action="store_true")
    parser.add_argument(
        "--projection-mode",
        choices=("source", "near-origin"),
        default="source",
        help=(
            "Use source projection metadata, or rewrite the case as a compact "
            "Lambert domain near lon=0 with low positive latitude."
        ),
    )
    parser.add_argument("--near-origin-cen-lat", type=float, default=1.0)
    parser.add_argument("--near-origin-cen-lon", type=float, default=0.0)
    parser.add_argument("--near-origin-true-lat", type=float, default=1.0)
    parser.add_argument("--background-fuel", type=float, default=101.0)
    parser.add_argument("--no-fuel", type=float, default=91.0)
    parser.add_argument("--patch-radius-cells", type=float, default=4.0)
    parser.add_argument("--patch-offset-m", type=float, default=350.0)
    parser.add_argument("--terrain-height-m", type=float, default=0.0)
    parser.add_argument("--u-wind", type=float, default=6.0)
    parser.add_argument("--v-wind", type=float, default=0.0)
    parser.add_argument("--t2", type=float, default=300.0)
    parser.add_argument("--q2", type=float, default=0.006)
    parser.add_argument("--psfc", type=float, default=90000.0)
    parser.add_argument("--znt", type=float, default=0.1)
    parser.add_argument(
        "--coordinate-mode",
        choices=("xy", "index", "latlon"),
        default="xy",
        help="How to interpret ignition inputs.",
    )
    parser.add_argument("--ignition-x-m", type=float)
    parser.add_argument("--ignition-y-m", type=float)
    parser.add_argument("--ignition-end-x-m", type=float)
    parser.add_argument("--ignition-end-y-m", type=float)
    parser.add_argument("--ignition-i", type=float)
    parser.add_argument("--ignition-j", type=float)
    parser.add_argument("--ignition-end-i", type=float)
    parser.add_argument("--ignition-end-j", type=float)
    parser.add_argument("--ignition-start-lat", type=float)
    parser.add_argument("--ignition-start-lon", type=float)
    parser.add_argument("--ignition-end-lat", type=float)
    parser.add_argument("--ignition-end-lon", type=float)
    return parser.parse_args()


class LambertProjection:
    """Minimal Python copy of share/proj_lc_mod.F90 for test input generation."""

    def __init__(
        self,
        *,
        cen_lat: float,
        cen_lon: float,
        dx: float,
        dy: float,
        standard_lon: float,
        true_lat_1: float,
        true_lat_2: float,
        nx: int,
        ny: int,
    ) -> None:
        self.cen_lat = cen_lat
        self.cen_lon = cen_lon
        self.dx = dx
        self.dy = dy
        self.standard_lon = standard_lon
        self.true_lat_1 = true_lat_1
        self.true_lat_2 = true_lat_2
        self.nx = nx
        self.ny = ny
        self.known_i = (nx + 1) / 2.0
        self.known_j = (ny + 1) / 2.0
        self.hemi = -1.0 if true_lat_1 < 0.0 else 1.0
        self.cone_factor = self._calc_cone(true_lat_1, true_lat_2)
        if abs(self.cone_factor) < 1.0e-7:
            raise ValueError(
                "Lambert cone factor is too close to zero. "
                "Use a nonzero near-origin latitude, for example 1 degree."
            )
        self.pole_i, self.pole_j = self._calc_pole()

    @staticmethod
    def _calc_cone(truelat1: float, truelat2: float) -> float:
        if abs(truelat1 - truelat2) > 0.1:
            cone = math.log10(math.cos(truelat1 * DEG_TO_RAD)) - math.log10(math.cos(truelat2 * DEG_TO_RAD))
            cone /= math.log10(math.tan((45.0 - abs(truelat1) / 2.0) * DEG_TO_RAD)) - math.log10(
                math.tan((45.0 - abs(truelat2) / 2.0) * DEG_TO_RAD)
            )
            return cone
        return math.sin(abs(truelat1) * DEG_TO_RAD)

    def _calc_pole(self) -> tuple[float, float]:
        deltalon = self.cen_lon - self.standard_lon
        if deltalon > 180.0:
            deltalon -= 360.0
        if deltalon < -180.0:
            deltalon += 360.0

        tl1r = self.true_lat_1 * DEG_TO_RAD
        rebydx = EARTH_RADIUS_M / self.dx
        rsw = (
            rebydx
            * math.cos(tl1r)
            / self.cone_factor
            * (
                math.tan((90.0 * self.hemi - self.cen_lat) * DEG_TO_RAD / 2.0)
                / math.tan((90.0 * self.hemi - self.true_lat_1) * DEG_TO_RAD / 2.0)
            )
            ** self.cone_factor
        )

        arg = self.cone_factor * (deltalon * DEG_TO_RAD)
        pole_i = self.hemi * self.known_i - self.hemi * rsw * math.sin(arg)
        pole_j = self.hemi * self.known_j + rsw * math.cos(arg)
        return pole_i, pole_j

    def calc_latlon(self, i: float, j: float) -> tuple[float, float]:
        chi1 = (90.0 - self.hemi * self.true_lat_1) * DEG_TO_RAD
        chi2 = (90.0 - self.hemi * self.true_lat_2) * DEG_TO_RAD

        inew = self.hemi * i
        jnew = self.hemi * j
        rebydx = EARTH_RADIUS_M / self.dx
        xx = inew - self.pole_i
        yy = self.pole_j - jnew
        r2 = xx * xx + yy * yy
        r = math.sqrt(r2) / rebydx

        if r2 == 0.0:
            lat = self.hemi * 90.0
            lon = self.standard_lon
        else:
            lon = self.standard_lon + RAD_TO_DEG * math.atan2(self.hemi * xx, yy) / self.cone_factor
            lon = math.fmod(lon + 360.0, 360.0)
            if chi1 == chi2:
                chi = 2.0 * math.atan((r / math.tan(chi1)) ** (1.0 / self.cone_factor) * math.tan(chi1 * 0.5))
            else:
                chi = 2.0 * math.atan(
                    (r * self.cone_factor / math.sin(chi1)) ** (1.0 / self.cone_factor)
                    * math.tan(chi1 * 0.5)
                )
            lat = (90.0 - chi * RAD_TO_DEG) * self.hemi

        if lon > 180.0:
            lon -= 360.0
        if lon < -180.0:
            lon += 360.0
        return lat, lon


def require_vars(dataset: netCDF4.Dataset, names: Iterable[str], path: Path) -> None:
    missing = [name for name in names if name not in dataset.variables]
    if missing:
        raise ValueError(f"{path} is missing required variables: {', '.join(missing)}")


def copy_attrs(src, dst, names: Iterable[str] | None = None) -> None:
    attr_names = src.ncattrs() if names is None else names
    for name in attr_names:
        if name in src.ncattrs():
            dst.setncattr(name, src.getncattr(name))


def needed_dimensions(src: netCDF4.Dataset, var_names: Iterable[str]) -> list[str]:
    dims: list[str] = []
    for var_name in var_names:
        for dim_name in src.variables[var_name].dimensions:
            if dim_name not in dims:
                dims.append(dim_name)
    return dims


def copy_structure(src: netCDF4.Dataset, dst: netCDF4.Dataset, var_names: Iterable[str], attr_names: Iterable[str]) -> None:
    for dim_name in needed_dimensions(src, var_names):
        dim = src.dimensions[dim_name]
        dst.createDimension(dim_name, None if dim.isunlimited() else len(dim))

    copy_attrs(src, dst, attr_names)

    for var_name in var_names:
        src_var = src.variables[var_name]
        fill_value = getattr(src_var, "_FillValue", None)
        kwargs = {}
        if fill_value is not None:
            kwargs["fill_value"] = fill_value
        dst_var = dst.createVariable(var_name, src_var.datatype, src_var.dimensions, **kwargs)
        copy_attrs(src_var, dst_var, (name for name in src_var.ncattrs() if name != "_FillValue"))
        dst_var[:] = src_var[:]


def write_reduced_file(source: Path, output: Path, var_names: Iterable[str], attr_names: Iterable[str]) -> None:
    tmp = output.with_suffix(output.suffix + ".tmp")
    with netCDF4.Dataset(source) as src:
        require_vars(src, var_names, source)
        with netCDF4.Dataset(tmp, "w", format="NETCDF4_CLASSIC") as dst:
            copy_structure(src, dst, var_names, attr_names)
    os.replace(tmp, output)


def get_attr(ds: netCDF4.Dataset, name: str) -> float:
    return float(ds.getncattr(name))


def get_int_attr(ds: netCDF4.Dataset, name: str) -> int:
    return int(ds.getncattr(name))


def projection_from_dataset(ds: netCDF4.Dataset) -> LambertProjection:
    return LambertProjection(
        cen_lat=get_attr(ds, "CEN_LAT"),
        cen_lon=get_attr(ds, "CEN_LON"),
        dx=get_attr(ds, "DX"),
        dy=get_attr(ds, "DY"),
        standard_lon=get_attr(ds, "STAND_LON"),
        true_lat_1=get_attr(ds, "TRUELAT1"),
        true_lat_2=get_attr(ds, "TRUELAT2"),
        nx=get_int_attr(ds, "WEST-EAST_GRID_DIMENSION") - 1,
        ny=get_int_attr(ds, "SOUTH-NORTH_GRID_DIMENSION") - 1,
    )


def apply_near_origin_projection(path: Path, args: argparse.Namespace) -> None:
    with netCDF4.Dataset(path, "r+") as ds:
        for name, value in (
            ("CEN_LAT", args.near_origin_cen_lat),
            ("CEN_LON", args.near_origin_cen_lon),
            ("TRUELAT1", args.near_origin_true_lat),
            ("TRUELAT2", args.near_origin_true_lat),
            ("MOAD_CEN_LAT", args.near_origin_cen_lat),
            ("STAND_LON", args.near_origin_cen_lon),
        ):
            if name in ds.ncattrs():
                ds.setncattr(name, np.float32(value))

        if "MAP_PROJ" in ds.ncattrs():
            ds.setncattr("MAP_PROJ", np.int32(1))
        if "MAP_PROJ_CHAR" in ds.ncattrs():
            ds.setncattr("MAP_PROJ_CHAR", "Lambert Conformal")


def fill_latlon_var(var: netCDF4.Variable, proj: LambertProjection, *, corner: bool = False) -> None:
    data = np.empty(var.shape, dtype=np.float32)
    has_time = len(var.shape) == 3
    ny = var.shape[-2]
    nx = var.shape[-1]

    for j in range(ny):
        for i in range(nx):
            i_atm = i + 0.5 if corner else i + 1.0
            j_atm = j + 0.5 if corner else j + 1.0
            lat, lon = proj.calc_latlon(i_atm, j_atm)
            if "LAT" in var.name:
                value = lat
            else:
                value = lon
            if has_time:
                data[:, j, i] = value
            else:
                data[j, i] = value
    var[:] = data


def refresh_coordinate_arrays(geo_path: Path, wrf_path: Path) -> None:
    with netCDF4.Dataset(geo_path, "r+") as geo:
        proj = projection_from_dataset(geo)
        fill_latlon_var(geo.variables["XLAT_M"], proj)
        fill_latlon_var(geo.variables["XLONG_M"], proj)
        fill_latlon_var(geo.variables["XLAT_C"], proj, corner=True)
        fill_latlon_var(geo.variables["XLONG_C"], proj, corner=True)

        if "corner_lats" in geo.ncattrs():
            lat_c = np.asarray(geo.variables["XLAT_C"][0, :, :])
            geo.setncattr(
                "corner_lats",
                np.asarray(
                    [
                        lat_c[0, 0],
                        lat_c[-1, 0],
                        lat_c[-1, -1],
                        lat_c[0, -1],
                    ],
                    dtype=np.float32,
                ),
            )
        if "corner_lons" in geo.ncattrs():
            lon_c = np.asarray(geo.variables["XLONG_C"][0, :, :])
            geo.setncattr(
                "corner_lons",
                np.asarray(
                    [
                        lon_c[0, 0],
                        lon_c[-1, 0],
                        lon_c[-1, -1],
                        lon_c[0, -1],
                    ],
                    dtype=np.float32,
                ),
            )

    with netCDF4.Dataset(wrf_path, "r+") as wrf:
        proj = projection_from_dataset(wrf)
        fill_latlon_var(wrf.variables["XLAT"], proj)
        fill_latlon_var(wrf.variables["XLONG"], proj)


def fire_grid_spacing(ds: netCDF4.Dataset) -> tuple[float, float]:
    return get_attr(ds, "DX") / get_int_attr(ds, "sr_x"), get_attr(ds, "DY") / get_int_attr(ds, "sr_y")


def fire_domain_size(ds: netCDF4.Dataset) -> tuple[int, int]:
    return len(ds.dimensions["west_east_subgrid"]), len(ds.dimensions["south_north_subgrid"])


def xy_to_latlon(ds: netCDF4.Dataset, x_m: float, y_m: float) -> tuple[float, float]:
    proj = projection_from_dataset(ds)
    i_atm = x_m / get_attr(ds, "DX") + 0.5
    j_atm = y_m / get_attr(ds, "DY") + 0.5
    return proj.calc_latlon(i_atm, j_atm)


def resolve_ignition_xy(geo: netCDF4.Dataset, args: argparse.Namespace) -> tuple[float, float, float, float]:
    dx_fire, dy_fire = fire_grid_spacing(geo)
    center_x = 0.5 * (get_int_attr(geo, "WEST-EAST_GRID_DIMENSION") - 1) * get_attr(geo, "DX")
    center_y = 0.5 * (get_int_attr(geo, "SOUTH-NORTH_GRID_DIMENSION") - 1) * get_attr(geo, "DY")

    if args.coordinate_mode == "xy":
        start_x = center_x if args.ignition_x_m is None else args.ignition_x_m
        start_y = center_y if args.ignition_y_m is None else args.ignition_y_m
        end_x = start_x if args.ignition_end_x_m is None else args.ignition_end_x_m
        end_y = start_y if args.ignition_end_y_m is None else args.ignition_end_y_m
    elif args.coordinate_mode == "index":
        start_i = center_x / dx_fire + 0.5 if args.ignition_i is None else args.ignition_i
        start_j = center_y / dy_fire + 0.5 if args.ignition_j is None else args.ignition_j
        end_i = start_i if args.ignition_end_i is None else args.ignition_end_i
        end_j = start_j if args.ignition_end_j is None else args.ignition_end_j
        start_x = (start_i - 0.5) * dx_fire
        start_y = (start_j - 0.5) * dy_fire
        end_x = (end_i - 0.5) * dx_fire
        end_y = (end_j - 0.5) * dy_fire
    else:
        required = (
            args.ignition_start_lat,
            args.ignition_start_lon,
            args.ignition_end_lat,
            args.ignition_end_lon,
        )
        if any(value is None for value in required):
            raise ValueError("--coordinate-mode latlon requires all ignition lat/lon arguments")
        start_x, start_y = latlon_to_local_xy(geo, args.ignition_start_lat, args.ignition_start_lon)
        end_x, end_y = latlon_to_local_xy(geo, args.ignition_end_lat, args.ignition_end_lon)

    return start_x, start_y, end_x, end_y


def latlon_to_local_xy(ds: netCDF4.Dataset, lat: float, lon: float) -> tuple[float, float]:
    proj = projection_from_dataset(ds)
    i, j = lc_ij_from_latlon(proj, lat, lon)
    return (i - 0.5) * get_attr(ds, "DX"), (j - 0.5) * get_attr(ds, "DY")


def lc_ij_from_latlon(proj: LambertProjection, lat: float, lon: float) -> tuple[float, float]:
    deltalon = lon - proj.standard_lon
    if deltalon > 180.0:
        deltalon -= 360.0
    if deltalon < -180.0:
        deltalon += 360.0

    tl1r = proj.true_lat_1 * DEG_TO_RAD
    rebydx = EARTH_RADIUS_M / proj.dx
    rm = (
        rebydx
        * math.cos(tl1r)
        / proj.cone_factor
        * (
            math.tan((90.0 * proj.hemi - lat) * DEG_TO_RAD / 2.0)
            / math.tan((90.0 * proj.hemi - proj.true_lat_1) * DEG_TO_RAD / 2.0)
        )
        ** proj.cone_factor
    )
    arg = proj.cone_factor * (deltalon * DEG_TO_RAD)
    i = proj.pole_i + proj.hemi * rm * math.sin(arg)
    i = proj.hemi * i
    j = proj.pole_j - rm * math.cos(arg)
    j = proj.hemi * j
    return i, j


def resolve_ignition_latlon(geo_path: Path, args: argparse.Namespace) -> dict[str, float]:
    with netCDF4.Dataset(geo_path) as geo:
        start_x, start_y, end_x, end_y = resolve_ignition_xy(geo, args)
        start_lat, start_lon = xy_to_latlon(geo, start_x, start_y)
        end_lat, end_lon = xy_to_latlon(geo, end_x, end_y)

    return {
        "start_x_m": start_x,
        "start_y_m": start_y,
        "end_x_m": end_x,
        "end_y_m": end_y,
        "start_lat": start_lat,
        "start_lon": start_lon,
        "end_lat": end_lat,
        "end_lon": end_lon,
    }


def fire_cell_latlon(geo: netCDF4.Dataset) -> tuple[np.ndarray, np.ndarray]:
    lat_c = np.asarray(geo.variables["XLAT_C"][0, :, :], dtype=np.float64)
    lon_c = np.asarray(geo.variables["XLONG_C"][0, :, :], dtype=np.float64)
    ny_fire = len(geo.dimensions["south_north_subgrid"])
    nx_fire = len(geo.dimensions["west_east_subgrid"])

    xi = (np.arange(nx_fire, dtype=np.float64) + 0.5) / nx_fire
    eta = (np.arange(ny_fire, dtype=np.float64) + 0.5) / ny_fire
    xx, yy = np.meshgrid(xi, eta)

    sw_lat = lat_c[0, 0]
    se_lat = lat_c[0, -1]
    nw_lat = lat_c[-1, 0]
    ne_lat = lat_c[-1, -1]
    sw_lon = lon_c[0, 0]
    se_lon = lon_c[0, -1]
    nw_lon = lon_c[-1, 0]
    ne_lon = lon_c[-1, -1]

    lat = (
        (1.0 - xx) * (1.0 - yy) * sw_lat
        + xx * (1.0 - yy) * se_lat
        + (1.0 - xx) * yy * nw_lat
        + xx * yy * ne_lat
    )
    lon = (
        (1.0 - xx) * (1.0 - yy) * sw_lon
        + xx * (1.0 - yy) * se_lon
        + (1.0 - xx) * yy * nw_lon
        + xx * yy * ne_lon
    )
    return lat, lon


def local_xy(lat: np.ndarray, lon: np.ndarray, cen_lat: float, cen_lon: float) -> tuple[np.ndarray, np.ndarray]:
    x = (lon - cen_lon) * 111_320.0 * math.cos(math.radians(cen_lat))
    y = (lat - cen_lat) * 110_540.0
    return x, y


def apply_geo_idealization(geo_path: Path, args: argparse.Namespace, ignition: dict[str, float]) -> tuple[int, int, int]:
    with netCDF4.Dataset(geo_path, "r+") as geo:
        geo.variables["ZSF"][:] = args.terrain_height_m
        geo.variables["DZDXF"][:] = 0.0
        geo.variables["DZDYF"][:] = 0.0
        geo.variables["NFUEL_CAT"][:] = args.background_fuel

        dx_fire, dy_fire = fire_grid_spacing(geo)
        nx_fire, ny_fire = fire_domain_size(geo)
        x = (np.arange(nx_fire, dtype=np.float64) + 0.5) * dx_fire
        y = (np.arange(ny_fire, dtype=np.float64) + 0.5) * dy_fire
        xx, yy = np.meshgrid(x, y)

        ign_x = 0.5 * (ignition["start_x_m"] + ignition["end_x_m"])
        ign_y = 0.5 * (ignition["start_y_m"] + ignition["end_y_m"])

        wind_norm = math.hypot(args.u_wind, args.v_wind)
        if wind_norm <= 0.0:
            raise ValueError("The downwind no-fuel patch requires a nonzero horizontal wind vector")

        target_x = ign_x + args.patch_offset_m * args.u_wind / wind_norm
        target_y = ign_y + args.patch_offset_m * args.v_wind / wind_norm
        dist2 = (xx - target_x) ** 2 + (yy - target_y) ** 2
        center_j, center_i = np.unravel_index(np.argmin(dist2), dist2.shape)

        yy, xx = np.indices(dist2.shape)
        patch = (xx - center_i) ** 2 + (yy - center_j) ** 2 <= args.patch_radius_cells ** 2
        nfuel = geo.variables["NFUEL_CAT"][:]
        nfuel[:, patch] = args.no_fuel
        geo.variables["NFUEL_CAT"][:] = nfuel

        return int(center_i), int(center_j), int(np.count_nonzero(patch))


def update_namelist_ignition(namelist_path: Path, ignition: dict[str, float]) -> None:
    replacements = {
        "fire_ignition_start_lat1": ignition["start_lat"],
        "fire_ignition_start_lon1": ignition["start_lon"],
        "fire_ignition_end_lat1": ignition["end_lat"],
        "fire_ignition_end_lon1": ignition["end_lon"],
    }
    text = namelist_path.read_text()
    for key, value in replacements.items():
        pattern = re.compile(rf"^(\s*{re.escape(key)}\s*=\s*)[-+0-9.Ee]+(\s*,?.*)$", re.MULTILINE)
        text, n_match = pattern.subn(rf"\g<1>{value:.8f}\2", text, count=1)
        if n_match != 1:
            raise ValueError(f"Could not find exactly one {key} entry in {namelist_path}")
    namelist_path.write_text(text)


def apply_wrf_idealization(wrf_path: Path, args: argparse.Namespace) -> None:
    with netCDF4.Dataset(wrf_path, "r+") as wrf:
        wrf.variables["T2"][:] = args.t2
        wrf.variables["Q2"][:] = args.q2
        wrf.variables["PSFC"][:] = args.psfc
        wrf.variables["RAINC"][:] = 0.0
        wrf.variables["RAINNC"][:] = 0.0
        wrf.variables["U"][:] = args.u_wind
        wrf.variables["V"][:] = args.v_wind
        wrf.variables["ZNT"][:] = args.znt

        ph = wrf.variables["PH"]
        phb = wrf.variables["PHB"]
        nlev = ph.shape[1]
        z_w = np.linspace(0.0, 6000.0, nlev, dtype=np.float32)
        profile = (G * z_w).reshape((1, nlev, 1, 1))
        ph[:] = 0.0
        phb[:] = np.broadcast_to(profile, phb.shape)


def main() -> None:
    args = parse_args()
    if args.output_geo.resolve() == args.source_geo.resolve():
        geo_source = args.source_geo.with_suffix(args.source_geo.suffix + ".source")
        if not geo_source.exists():
            raise ValueError(
                f"Refusing to overwrite {args.source_geo} in place without {geo_source}. "
                "Pass --source-geo pointing to a preserved source file."
            )
        args.source_geo = geo_source
    if args.output_wrf.resolve() == args.source_wrf.resolve():
        wrf_source = args.source_wrf.with_suffix(args.source_wrf.suffix + ".source")
        if not wrf_source.exists():
            raise ValueError(
                f"Refusing to overwrite {args.source_wrf} in place without {wrf_source}. "
                "Pass --source-wrf pointing to a preserved source file."
            )
        args.source_wrf = wrf_source

    write_reduced_file(args.source_geo, args.output_geo, GEO_VARS, GEO_GLOBAL_ATTRS)
    write_reduced_file(args.source_wrf, args.output_wrf, WRF_VARS, WRF_GLOBAL_ATTRS)

    if args.projection_mode == "near-origin":
        apply_near_origin_projection(args.output_geo, args)
        apply_near_origin_projection(args.output_wrf, args)
    refresh_coordinate_arrays(args.output_geo, args.output_wrf)

    ignition = resolve_ignition_latlon(args.output_geo, args)
    center_i, center_j, n_patch = apply_geo_idealization(args.output_geo, args, ignition)
    apply_wrf_idealization(args.output_wrf, args)
    if not args.no_update_namelist:
        update_namelist_ignition(args.namelist, ignition)

    print("Generated geoideal inputs")
    print(f"  geo: {args.output_geo}")
    print(f"  wrf: {args.output_wrf}")
    print(f"  projection mode: {args.projection_mode}")
    print(
        "  ignition local xy: "
        f"start=({ignition['start_x_m']:.3f}, {ignition['start_y_m']:.3f}) m, "
        f"end=({ignition['end_x_m']:.3f}, {ignition['end_y_m']:.3f}) m"
    )
    print(
        "  ignition namelist lat/lon: "
        f"start=({ignition['start_lat']:.8f}, {ignition['start_lon']:.8f}), "
        f"end=({ignition['end_lat']:.8f}, {ignition['end_lon']:.8f})"
    )
    print(f"  background fuel: {args.background_fuel:g}")
    print(f"  no-fuel patch: fuel={args.no_fuel:g}, center_i={center_i + 1}, center_j={center_j + 1}, cells={n_patch}")
    print(f"  terrain: z={args.terrain_height_m:g} m, dzdx=0, dzdy=0")
    print(f"  wind: U={args.u_wind:g} m/s, V={args.v_wind:g} m/s")


if __name__ == "__main__":
    main()
