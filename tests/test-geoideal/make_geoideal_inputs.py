#!/usr/bin/env python3
"""Generate reduced idealized NetCDF inputs for fire_behavior.exe."""

from __future__ import annotations

import argparse
import math
import os
from pathlib import Path
from typing import Iterable

import netCDF4
import numpy as np


G = 9.81

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
    parser.add_argument("--ignition-start-lat", type=float, default=39.67999)
    parser.add_argument("--ignition-start-lon", type=float, default=-103.58)
    parser.add_argument("--ignition-end-lat", type=float, default=39.67999)
    parser.add_argument("--ignition-end-lon", type=float, default=-103.58)
    return parser.parse_args()


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


def apply_geo_idealization(geo_path: Path, args: argparse.Namespace) -> tuple[int, int, int]:
    with netCDF4.Dataset(geo_path, "r+") as geo:
        geo.variables["ZSF"][:] = args.terrain_height_m
        geo.variables["DZDXF"][:] = 0.0
        geo.variables["DZDYF"][:] = 0.0
        geo.variables["NFUEL_CAT"][:] = args.background_fuel

        lat, lon = fire_cell_latlon(geo)
        cen_lat = get_attr(geo, "CEN_LAT")
        cen_lon = get_attr(geo, "CEN_LON")
        x, y = local_xy(lat, lon, cen_lat, cen_lon)

        ign_lat = 0.5 * (args.ignition_start_lat + args.ignition_end_lat)
        ign_lon = 0.5 * (args.ignition_start_lon + args.ignition_end_lon)
        ign_x, ign_y = local_xy(np.array(ign_lat), np.array(ign_lon), cen_lat, cen_lon)

        wind_norm = math.hypot(args.u_wind, args.v_wind)
        if wind_norm <= 0.0:
            raise ValueError("The downwind no-fuel patch requires a nonzero horizontal wind vector")

        target_x = float(ign_x) + args.patch_offset_m * args.u_wind / wind_norm
        target_y = float(ign_y) + args.patch_offset_m * args.v_wind / wind_norm
        dist2 = (x - target_x) ** 2 + (y - target_y) ** 2
        center_j, center_i = np.unravel_index(np.argmin(dist2), dist2.shape)

        yy, xx = np.indices(dist2.shape)
        patch = (xx - center_i) ** 2 + (yy - center_j) ** 2 <= args.patch_radius_cells ** 2
        nfuel = geo.variables["NFUEL_CAT"][:]
        nfuel[:, patch] = args.no_fuel
        geo.variables["NFUEL_CAT"][:] = nfuel

        return int(center_i), int(center_j), int(np.count_nonzero(patch))


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
    center_i, center_j, n_patch = apply_geo_idealization(args.output_geo, args)
    apply_wrf_idealization(args.output_wrf, args)

    print("Generated geoideal inputs")
    print(f"  geo: {args.output_geo}")
    print(f"  wrf: {args.output_wrf}")
    print(f"  background fuel: {args.background_fuel:g}")
    print(f"  no-fuel patch: fuel={args.no_fuel:g}, center_i={center_i + 1}, center_j={center_j + 1}, cells={n_patch}")
    print(f"  terrain: z={args.terrain_height_m:g} m, dzdx=0, dzdy=0")
    print(f"  wind: U={args.u_wind:g} m/s, V={args.v_wind:g} m/s")


if __name__ == "__main__":
    main()
