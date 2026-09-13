#!/usr/bin/env python3
# Copyright 2026      Research Applications Laboratory (RAL),
#                     National Center for Atmospheric Research (NCAR),
#                     University Corporation for Atmospheric Research (UCAR)
#
#--------------------------------------------------------------------------------
# Created by Maria Frediani (frediani@ucar.edu) on 2026-09-12
#--------------------------------------------------------------------------------
# run python -B tests/regression/regression.py prepare --help
#
"""Generate deterministic geogrid and WRF inputs for CFBM regression cases."""

from __future__ import annotations

#--------------------------------------------------------------------------------
# Python modules
#--------------------------------------------------------------------------------

import copy
import datetime as dt
import hashlib
import math
from pathlib import Path
from typing import Any

import netCDF4
import numpy as np


#--------------------------------------------------------------------------------
# Input schema and projection constants
#--------------------------------------------------------------------------------

EARTH_RADIUS_M = 6_370_000.0
DATE_FORMAT = "%Y-%m-%d_%H:%M:%S"
DATE_STR_LEN = 19
GEO_VARIABLES = {
    "Times": ("|S1", ("Time", "DateStrLen")),
    "XLAT_M": ("float32", ("Time", "south_north", "west_east")),
    "XLONG_M": ("float32", ("Time", "south_north", "west_east")),
    "XLAT_C": ("float32", ("Time", "south_north_stag", "west_east_stag")),
    "XLONG_C": ("float32", ("Time", "south_north_stag", "west_east_stag")),
    "ZSF": ("float32", ("Time", "south_north_subgrid", "west_east_subgrid")),
    "DZDXF": ("float32", ("Time", "south_north_subgrid", "west_east_subgrid")),
    "DZDYF": ("float32", ("Time", "south_north_subgrid", "west_east_subgrid")),
    "NFUEL_CAT": ("float32", ("Time", "south_north_subgrid", "west_east_subgrid")),
}
WRF_VARIABLES = {
    "Times": ("|S1", ("Time", "DateStrLen")),
    "XLAT": ("float32", ("Time", "south_north", "west_east")),
    "XLONG": ("float32", ("Time", "south_north", "west_east")),
    "T2": ("float32", ("Time", "south_north", "west_east")),
    "Q2": ("float32", ("Time", "south_north", "west_east")),
    "ZNT": ("float32", ("Time", "south_north", "west_east")),
    "PSFC": ("float32", ("Time", "south_north", "west_east")),
    "RAINC": ("float32", ("Time", "south_north", "west_east")),
    "RAINNC": ("float32", ("Time", "south_north", "west_east")),
    "U10": ("float32", ("Time", "south_north", "west_east")),
    "V10": ("float32", ("Time", "south_north", "west_east")),
}


class LambertProjection:
    """Reproduce the model Lambert grid-to-geographic coordinate transform."""

    def __init__(self, nx: int, ny: int, dx_m: float, dy_m: float, projection: dict[str, float]) -> None:
        self.nx = nx
        self.ny = ny
        self.dx_m = dx_m
        self.dy_m = dy_m
        self.cen_lat = projection["cen_lat"]
        self.cen_lon = projection["cen_lon"]
        self.stand_lon = projection["stand_lon"]
        self.true_lat_1 = projection["true_lat_1"]
        self.true_lat_2 = projection["true_lat_2"]
        self.hemi = 1.0 if self.true_lat_1 >= 0.0 else -1.0
        phi1 = math.radians(abs(self.true_lat_1))
        phi2 = math.radians(abs(self.true_lat_2))
        if abs(self.true_lat_1 - self.true_lat_2) > 0.1:
            self.cone = math.log(math.cos(phi1) / math.cos(phi2)) / math.log(
                math.tan(math.pi / 4.0 + phi2 / 2.0) / math.tan(math.pi / 4.0 + phi1 / 2.0)
            )
        else:
            self.cone = math.sin(phi1)
        self.known_i = (nx + 1.0) / 2.0
        self.known_j = (ny + 1.0) / 2.0
        self.pole_i, self.pole_j = self._pole()

    def _pole(self) -> tuple[float, float]:
        """Calculate Lambert pole coordinates in grid units."""
        rebydx = EARTH_RADIUS_M / self.dx_m
        chi1 = math.radians(90.0 - self.hemi * self.true_lat_1)
        chic = math.radians(90.0 - self.hemi * self.cen_lat)
        rsw = rebydx * math.sin(chi1) / self.cone * (math.tan(chic / 2.0) / math.tan(chi1 / 2.0)) ** self.cone
        arg = self.cone * math.radians(self.cen_lon - self.stand_lon)
        return (
            self.hemi * self.known_i - self.hemi * rsw * math.sin(arg),
            self.hemi * self.known_j + rsw * math.cos(arg),
        )

    def latlon(self, i: float, j: float) -> tuple[float, float]:
        """Convert one-based fractional grid coordinates to latitude and longitude."""
        xx = self.hemi * i - self.pole_i
        yy = self.pole_j - self.hemi * j
        radius_grid = math.hypot(xx, yy)
        if radius_grid == 0.0:
            return self.hemi * 90.0, self.stand_lon
        chi1 = math.radians(90.0 - self.hemi * self.true_lat_1)
        radius = radius_grid / (EARTH_RADIUS_M / self.dx_m)
        chi = 2.0 * math.atan((radius * self.cone / math.sin(chi1)) ** (1.0 / self.cone) * math.tan(chi1 / 2.0))
        lat = self.hemi * (90.0 - math.degrees(chi))
        lon = self.stand_lon + math.degrees(math.atan2(self.hemi * xx, yy) / self.cone)
        return lat, (lon + 180.0) % 360.0 - 180.0


def sha256_file(path: Path) -> str:
    """Return the SHA-256 digest of one generated or model file."""
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def _grid_latlon(spec: dict[str, Any], corners: bool = False) -> tuple[np.ndarray, np.ndarray]:
    """Generate mass-point or corner latitude and longitude arrays."""
    grid = spec["grid"]
    nx = grid["nx"] if corners else grid["nx"] - 1
    ny = grid["ny"] if corners else grid["ny"] - 1
    proj = LambertProjection(grid["nx"] - 1, grid["ny"] - 1, grid["dx_m"], grid["dy_m"], spec["projection"])
    lat = np.empty((ny, nx), dtype=np.float32)
    lon = np.empty_like(lat)
    offset = 0.5 if corners else 1.0
    for j in range(ny):
        for i in range(nx):
            lat[j, i], lon[j, i] = proj.latlon(i + offset, j + offset)
    return lat, lon


def _fire_latlon(spec: dict[str, Any], x_fraction: float, y_fraction: float) -> tuple[float, float]:
    """Convert a fractional fire-domain point to geographic coordinates."""
    grid = spec["grid"]
    proj = LambertProjection(grid["nx"] - 1, grid["ny"] - 1, grid["dx_m"], grid["dy_m"], spec["projection"])
    return proj.latlon(0.5 + x_fraction * grid["nx"], 0.5 + y_fraction * grid["ny"])


def _fields(spec: dict[str, Any]) -> dict[str, np.ndarray]:
    """Construct terrain, slopes, fuel categories, and optional perimeter level set."""
    grid = spec["grid"]
    ny, nx = grid["ny"], grid["nx"]
    y, x = np.indices((ny, nx), dtype=np.float64)
    xm = (x + 0.5) * grid["dx_m"]
    ym = (y + 0.5) * grid["dy_m"]
    terrain = spec["terrain"]
    if terrain["kind"] == "sinusoidal":
        phase_x = 2.0 * math.pi * xm / terrain["wavelength_x_m"]
        phase_y = 2.0 * math.pi * ym / terrain["wavelength_y_m"]
        zsf = terrain["base_elevation_m"] + terrain["amplitude_m"] * np.sin(phase_x) * np.sin(phase_y)
        dzdx = terrain["amplitude_m"] * (2.0 * math.pi / terrain["wavelength_x_m"]) * np.cos(phase_x) * np.sin(phase_y)
        dzdy = terrain["amplitude_m"] * (2.0 * math.pi / terrain["wavelength_y_m"]) * np.sin(phase_x) * np.cos(phase_y)
    else:
        zsf = np.full((ny, nx), terrain["base_elevation_m"])
        dzdx = np.zeros((ny, nx))
        dzdy = np.zeros((ny, nx))

    fuel = spec["fuel"]
    case = spec["identity"]["case"]
    if case in {"fuel_strip_wind", "terrain_fuel_fmc_wind"}:
        categories = np.asarray(fuel["categories"], dtype=np.float32)
        strip_index = np.minimum((np.arange(ny) * len(categories)) // ny, len(categories) - 1)
        nfuel = np.broadcast_to(categories[strip_index, None], (ny, nx)).copy()
    else:
        nfuel = np.full((ny, nx), fuel["uniform_category"], dtype=np.float32)

    fields = {"ZSF": zsf.astype("f4"), "DZDXF": dzdx.astype("f4"), "DZDYF": dzdy.astype("f4"), "NFUEL_CAT": nfuel}
    if spec["feature"]["real_perimeter"]:
        cx = 0.5 * nx * grid["dx_m"]
        cy = 0.5 * ny * grid["dy_m"]
        fields["lfn_init"] = (np.hypot(xm - cx, ym - cy) - spec["ignition"]["radius_m"]).astype("f4")
    return fields


def _set_attrs(target: Any, attributes: dict[str, Any]) -> None:
    """Assign NetCDF attributes in a stable insertion order."""
    for name in sorted(attributes):
        target.setncattr(name, attributes[name])


def _write_times(variable: netCDF4.Variable, times: list[dt.datetime]) -> None:
    """Write WRF Times records as fixed-width ASCII character arrays."""
    values = np.full((len(times), DATE_STR_LEN), b" ", dtype="S1")
    for index, value in enumerate(times):
        encoded = value.strftime(DATE_FORMAT).encode("ascii")
        values[index] = np.frombuffer(encoded, dtype="S1")
    variable[:] = values


def _common_attrs(spec: dict[str, Any], title: str) -> dict[str, Any]:
    """Return projection and grid attributes used by both generated files."""
    grid = spec["grid"]
    projection = spec["projection"]
    return {
        "TITLE": title, "SIMULATION_START_DATE": spec["time"]["start"],
        "DX": np.float32(grid["dx_m"]), "DY": np.float32(grid["dy_m"]),
        "CEN_LAT": np.float32(projection["cen_lat"]), "CEN_LON": np.float32(projection["cen_lon"]),
        "TRUELAT1": np.float32(projection["true_lat_1"]), "TRUELAT2": np.float32(projection["true_lat_2"]),
        "STAND_LON": np.float32(projection["stand_lon"]), "MAP_PROJ": np.int32(projection["map_proj"]),
        "sr_x": np.int32(grid["sr_x"]), "sr_y": np.int32(grid["sr_y"]),
    }


def _write_geo(path: Path, spec: dict[str, Any], fields: dict[str, np.ndarray]) -> None:
    """Write the exact geogrid fields read by the standalone initialization path."""
    grid = spec["grid"]
    mass_lat, mass_lon = _grid_latlon(spec)
    corner_lat, corner_lon = _grid_latlon(spec, corners=True)
    with netCDF4.Dataset(path, "w", format="NETCDF4_CLASSIC") as dataset:
        for name, length in (
            ("Time", None), ("DateStrLen", DATE_STR_LEN), ("south_north", grid["ny"] - 1),
            ("west_east", grid["nx"] - 1), ("south_north_stag", grid["ny"]),
            ("west_east_stag", grid["nx"]), ("south_north_subgrid", grid["ny"]),
            ("west_east_subgrid", grid["nx"]),
        ):
            dataset.createDimension(name, length)
        _set_attrs(dataset, _common_attrs(spec, "DETERMINISTIC CFBM REGRESSION GEOGRID INPUT"))
        _write_times(dataset.createVariable("Times", "S1", ("Time", "DateStrLen")), [dt.datetime.strptime(spec["time"]["start"], DATE_FORMAT)])
        for name, values, dimensions, units in (
            ("XLAT_M", mass_lat, ("Time", "south_north", "west_east"), "degrees_north"),
            ("XLONG_M", mass_lon, ("Time", "south_north", "west_east"), "degrees_east"),
            ("XLAT_C", corner_lat, ("Time", "south_north_stag", "west_east_stag"), "degrees_north"),
            ("XLONG_C", corner_lon, ("Time", "south_north_stag", "west_east_stag"), "degrees_east"),
        ):
            variable = dataset.createVariable(name, "f4", dimensions)
            variable.units = units
            variable[:] = values[np.newaxis]
        for name in ("ZSF", "DZDXF", "DZDYF", "NFUEL_CAT"):
            variable = dataset.createVariable(name, "f4", ("Time", "south_north_subgrid", "west_east_subgrid"))
            variable.units = {"ZSF": "m", "DZDXF": "1", "DZDYF": "1", "NFUEL_CAT": "category"}[name]
            variable[:] = fields[name][np.newaxis]
        if "lfn_init" in fields:
            variable = dataset.createVariable("lfn_init", "f4", ("south_north_subgrid", "west_east_subgrid"))
            variable.units = "m"
            variable[:] = fields["lfn_init"]


def _write_wrf(path: Path, spec: dict[str, Any]) -> None:
    """Write time-varying surface forcing used by wind and moisture readers."""
    grid = spec["grid"]
    time = spec["time"]
    forcing = spec["forcing"]
    start = dt.datetime.strptime(time["start"], DATE_FORMAT)
    count = int(round(time["duration_seconds"] / time["atmosphere_interval_seconds"])) + 1
    times = [start + dt.timedelta(seconds=index * time["atmosphere_interval_seconds"]) for index in range(count)]
    lat, lon = _grid_latlon(spec)
    fraction = np.linspace(0.0, 1.0, count, dtype=np.float32)[:, None, None]
    shape = (count, grid["ny"] - 1, grid["nx"] - 1)
    with netCDF4.Dataset(path, "w", format="NETCDF4_CLASSIC") as dataset:
        for name, length in (
            ("Time", None), ("DateStrLen", DATE_STR_LEN), ("south_north", grid["ny"] - 1),
            ("west_east", grid["nx"] - 1), ("south_north_stag", grid["ny"]),
            ("west_east_stag", grid["nx"]), ("bottom_top_stag", forcing["vertical_levels_stag"]),
        ):
            dataset.createDimension(name, length)
        attrs = _common_attrs(spec, "DETERMINISTIC CFBM REGRESSION WRF FORCING")
        attrs["START_DATE"] = time["start"]
        _set_attrs(dataset, attrs)
        _write_times(dataset.createVariable("Times", "S1", ("Time", "DateStrLen")), times)
        values = {
            "XLAT": np.broadcast_to(lat, shape), "XLONG": np.broadcast_to(lon, shape),
            "T2": np.broadcast_to(forcing["temperature_start_k"] + fraction * (forcing["temperature_end_k"] - forcing["temperature_start_k"]), shape),
            "Q2": np.broadcast_to(forcing["mixing_ratio_start_kg_kg"] + fraction * (forcing["mixing_ratio_end_kg_kg"] - forcing["mixing_ratio_start_kg_kg"]), shape),
            "ZNT": np.broadcast_to(
                np.linspace(forcing["roughness_length_min_m"], forcing["roughness_length_max_m"], grid["nx"] - 1, dtype=np.float32)[None, None, :],
                shape,
            ),
            "PSFC": np.full(shape, forcing["surface_pressure_pa"]),
            "RAINC": np.broadcast_to(forcing["accumulated_rain_start_mm"] + fraction * (forcing["accumulated_rain_end_mm"] - forcing["accumulated_rain_start_mm"]), shape),
            "RAINNC": np.zeros(shape), "U10": np.full(shape, forcing["u10_m_s"]), "V10": np.full(shape, forcing["v10_m_s"]),
        }
        units = {"XLAT": "degree_north", "XLONG": "degree_east", "T2": "K", "Q2": "kg kg-1", "ZNT": "m", "PSFC": "Pa", "RAINC": "mm", "RAINNC": "mm", "U10": "m s-1", "V10": "m s-1"}
        for name in WRF_VARIABLES:
            if name == "Times":
                continue
            variable = dataset.createVariable(name, "f4", ("Time", "south_north", "west_east"))
            variable.units = units[name]
            variable[:] = np.asarray(values[name], dtype=np.float32)


def validate_input_file(path: Path, expected: dict[str, tuple[str, tuple[str, ...]]]) -> dict[str, Any]:
    """Validate generated dimensions, variables, dtypes, finiteness, and attributes."""
    checks: dict[str, Any] = {"path": str(path), "sha256": sha256_file(path), "fields": {}}
    with netCDF4.Dataset(path) as dataset:
        missing = set(expected) - set(dataset.variables)
        extra = set(dataset.variables) - set(expected) - {"lfn_init"}
        if missing or extra:
            raise ValueError(f"Generated schema mismatch for {path}: missing={sorted(missing)}, extra={sorted(extra)}")
        for name, (dtype, dimensions) in expected.items():
            variable = dataset.variables[name]
            if variable.dimensions != dimensions or str(variable.dtype) != dtype:
                raise ValueError(f"Generated variable {name} has dtype/dimensions {variable.dtype}/{variable.dimensions}, expected {dtype}/{dimensions}")
            if variable.dtype.kind in "f" and not np.isfinite(variable[:]).all():
                raise ValueError(f"Generated variable {name} contains nonfinite values")
            checks["fields"][name] = {"shape": list(variable.shape), "dtype": str(variable.dtype), "dimensions": list(variable.dimensions)}
    return checks


def generate_inputs(spec: dict[str, Any], run_dir: Path) -> tuple[dict[str, Any], list[dict[str, Any]]]:
    """Generate required inputs, derive ignition coordinates, and return provenance."""
    resolved = copy.deepcopy(spec)
    if resolved["model"]["ideal_opt"] == 1:
        lat, lon = _fire_latlon(resolved, resolved["ignition"]["center_x_fraction"], resolved["ignition"]["center_y_fraction"])
        resolved["ignition"].update({"start_lat": lat, "start_lon": lon, "end_lat": lat, "end_lon": lon})
        return resolved, []
    fields = _fields(resolved)
    geo_path = run_dir / "geo_em.d01.nc"
    wrf_path = run_dir / "wrf.nc"
    _write_geo(geo_path, resolved, fields)
    _write_wrf(wrf_path, resolved)
    if resolved["ignition"]["kind"] == "line":
        start_lat, start_lon = _fire_latlon(
            resolved, resolved["ignition"]["line_x_fraction"], resolved["ignition"]["line_y_start_fraction"]
        )
        end_lat, end_lon = _fire_latlon(
            resolved, resolved["ignition"]["line_x_fraction"], resolved["ignition"]["line_y_end_fraction"]
        )
        resolved["ignition"].update({"start_lat": start_lat, "start_lon": start_lon, "end_lat": end_lat, "end_lon": end_lon})
    geo_schema = dict(GEO_VARIABLES)
    if "lfn_init" in fields:
        geo_schema["lfn_init"] = ("float32", ("south_north_subgrid", "west_east_subgrid"))
    return resolved, [validate_input_file(geo_path, geo_schema), validate_input_file(wrf_path, WRF_VARIABLES)]
