#!/usr/bin/env python3
# Created on 2026-10-02.
# Developed by the CFBM development team.
# run python -B tests/regression/regression.py --help
"""Check output times, field metadata, fire evolution, and forcing."""
from __future__ import annotations
import datetime as dt
from pathlib import Path
from typing import Any
import netCDF4
import numpy as np
from generate_inputs import DATE_FORMAT

OUTPUT_METADATA = {
    "lats": ("degrees_north", "fire-grid cell-center latitude"),
    "lons": ("degrees_east", "fire-grid cell-center longitude"),
    "fgrnhfx": ("W m-2", "ground fire sensible heat flux"),
    "fgrnqfx": ("W m-2", "ground fire latent heat flux"),
    "fire_area": ("1", "fire-area fraction within cell"),
    "fuel_frac_burnt_dt":
        ("1", "fuel fraction burned during current fire timestep"),
    "fuel_frac": ("1", "remaining fuel fraction"),
    "emis_smoke":
        ("kg m-2",
         "fire particulate emissions per cell area during current timestep"),
    "fire_t2": ("K", "air temperature at 2 m"),
    "fire_q2":
        ("kg kg-1", "water-vapor mixing ratio at 2 m (legacy variable name)"),
    "fire_psfc": ("Pa", "surface air pressure"),
    "fire_rain":
        (None,
         "standalone accumulated precipitation; coupled-driver units unresolved"
        ),
    "fz0": ("m", "surface roughness length"),
    "fmc_g": ("kg kg-1", "ground fuel moisture content"),
    "uf": ("m s-1", "eastward wind used by fire spread"),
    "vf": ("m s-1", "northward wind used by fire spread"),
    "zsf": ("m", "fire-grid terrain height"),
    "lfn": ("m", "signed level-set distance to fire perimeter"),
    "nfuel_cat": ("1", "fuel category identifier"),
    "grad_norm_ls": ("1", "level-set gradient norm used during propagation"),
    "grad_norm_reinit":
        ("1", "level-set gradient norm used during reinitialization"),
}
ATMOSPHERIC_FIELDS = {"fire_t2", "fire_q2", "fire_psfc", "fire_rain", "fz0"}
FORCING_CHECK_RTOL = 8.0 * np.finfo(np.float32).eps

#--------------------------------------------------------------------------------
# Output validation
#--------------------------------------------------------------------------------


def expected_output_names(spec: dict[str, Any]) -> list[str]:
    """Enumerate initialization and scheduled output names from configured time semantics."""
    time_cfg = spec
    start = dt.datetime.strptime(time_cfg["start"], "%Y-%m-%d_%H:%M:%S")
    seconds = [0] + list(
        range(int(spec["namelist"]["time"]["interval_output"]),
              int(time_cfg["duration_seconds"]) + 1,
              int(spec["namelist"]["time"]["interval_output"])))
    return [
        f"fire_output_{(start + dt.timedelta(seconds=value)).strftime('%Y-%m-%d_%H:%M:%S')}.nc"
        for value in seconds
    ]


def validate_outputs(run_dir: Path, manifest: dict[str, Any]) -> dict[str, Any]:
    """Require the output schema and evidence that each configured scientific path ran."""
    expected = set(manifest["expected_outputs"])
    actual = {path.name for path in run_dir.glob("fire_output_*.nc")}
    validation: dict[str, Any] = {"pass": True, "reasons": [], "files": []}
    if actual != expected:
        validation["reasons"].append(
            f"output inventory differs: expected={sorted(expected)}, actual={sorted(actual)}"
        )
    expected_fields = set(manifest["expected_fields"])
    for name in sorted(expected & actual):
        path = run_dir / name
        with netCDF4.Dataset(path) as dataset:
            fields = set(dataset.variables)
            if fields != expected_fields:
                validation["reasons"].append(
                    f"{name} fields differ: expected={sorted(expected_fields)}, actual={sorted(fields)}"
                )
            nonfinite = []
            for variable_name, variable in dataset.variables.items():
                if variable.dtype.kind != "f":
                    continue
                values = np.ma.asarray(variable[:])
                valid = values.compressed()
                if valid.size and not np.isfinite(valid).all():
                    nonfinite.append(variable_name)
                expected_units, expected_long_name = OUTPUT_METADATA.get(
                    variable_name, (None, None))
                attributes = set(variable.ncattrs())
                required_attributes = {"_FillValue", "long_name"}
                if expected_units is not None:
                    required_attributes.add("units")
                if not required_attributes <= attributes:
                    validation["reasons"].append(
                        f"{name} {variable_name} lacks metadata {sorted(required_attributes - attributes)}"
                    )
                if expected_units is None and "units" in attributes:
                    validation["reasons"].append(
                        f"{name} {variable_name} has unresolved units but declares {variable.units!r}"
                    )
                if expected_units is not None and getattr(
                        variable, "units", None) != expected_units:
                    validation["reasons"].append(
                        f"{name} {variable_name} units differ from the output contract"
                    )
                if getattr(variable, "long_name", None) != expected_long_name:
                    validation["reasons"].append(
                        f"{name} {variable_name} long_name differs from the output contract"
                    )
            if nonfinite:
                validation["reasons"].append(
                    f"{name} contains nonfinite fields: {sorted(nonfinite)}")
            validation["files"].append({
                "name": name,
                "size": path.stat().st_size,
                "fields": sorted(fields)
            })
    spec = manifest["spec"]
    if expected <= actual and len(expected) >= 2:
        first = run_dir / sorted(expected)[0]
        last = run_dir / sorted(expected)[-1]
        with netCDF4.Dataset(first) as first_ds, netCDF4.Dataset(
                last) as last_ds:
            for name, variable in last_ds.variables.items():
                if spec["namelist"]["fire"][
                        "ideal_opt"] == 1 and name in ATMOSPHERIC_FIELDS:
                    continue
                if np.ma.count_masked(variable[:]):
                    validation["reasons"].append(
                        f"final {name} contains missing cells")
            lfn_changed = not np.array_equal(first_ds.variables["lfn"][:],
                                             last_ds.variables["lfn"][:])
            fuel_consumed = bool(
                np.any(last_ds.variables["fuel_frac"][:] <
                       first_ds.variables["fuel_frac"][:]))
            active_flux = bool(
                np.any(last_ds.variables["fgrnhfx"][:] > 0.0) or
                np.any(last_ds.variables["fgrnqfx"][:] > 0.0))
            validation.update({
                "level_set_changed": lfn_changed,
                "fuel_consumption_observed": fuel_consumed,
                "positive_fire_flux_observed": active_flux,
            })
            if not lfn_changed:
                validation["reasons"].append(
                    "lfn did not change between initialization and final output"
                )
            if not fuel_consumed:
                validation["reasons"].append(
                    "fuel_frac did not decrease between initialization and final output"
                )
            if not active_flux:
                validation["reasons"].append(
                    "no positive sensible or latent fire flux was produced")
            if spec["namelist"]["fire"]["fmoist_run"]:
                moisture_changed = bool(
                    np.any(first_ds.variables["fmc_g"][:] !=
                           last_ds.variables["fmc_g"][:]))
                validation["moisture_update_observed"] = moisture_changed
                if not moisture_changed:
                    validation["reasons"].append(
                        "fmc_g did not change between initialization and final output"
                    )

            if spec["namelist"]["fire"]["ideal_opt"] == 1:
                unmasked = [
                    name for name in ATMOSPHERIC_FIELDS
                    if np.ma.count(last_ds.variables[name][:])
                ]
                if unmasked:
                    validation["reasons"].append(
                        f"ideal output contains applicable values in {sorted(unmasked)}"
                    )
            else:
                forcing = spec["inputs"]["atmosphere"]
                # Both drivers save the forcing used during the completed
                # interval, before refreshing it for the next fire advance.
                time = spec
                last_step_start = time["duration_seconds"] - spec["namelist"][
                    "time"]["dt"]
                interval = spec["namelist"]["atm"]["interval_atm"]
                forcing_time = (last_step_start // interval) * interval
                fraction = forcing_time / time["duration_seconds"]
                temperature = forcing["temperature_start_k"] + fraction * (
                    forcing["temperature_end_k"] -
                    forcing["temperature_start_k"])
                humidity = forcing["mixing_ratio_start_kg_kg"] + fraction * (
                    forcing["mixing_ratio_end_kg_kg"] -
                    forcing["mixing_ratio_start_kg_kg"])
                validation["forcing_fraction_at_final_output"] = fraction
                expected_forcing = {
                    "fire_t2": temperature,
                    "fire_q2": humidity,
                    "fire_psfc": forcing["surface_pressure_pa"],
                    "fire_rain": forcing["accumulated_rain_end_mm"],
                }
                for field, target in expected_forcing.items():
                    values = np.ma.asarray(
                        last_ds.variables[field][:]).compressed()
                    if not values.size or not np.allclose(
                            values, target, rtol=FORCING_CHECK_RTOL, atol=0.0):
                        validation["reasons"].append(
                            f"final {field} does not contain the forcing used during the completed fire interval"
                        )
                z0 = np.ma.asarray(last_ds.variables["fz0"][:]).compressed()
                if not z0.size or float(np.ptp(z0)) <= 0.0:
                    validation["reasons"].append(
                        "fz0 does not retain the spatially varying WRF ZNT field"
                    )

            if (spec["inputs"]["ignition"]["kind"] == "perimeter"):
                geo_path = run_dir / "geo_em.d01.nc"
                with netCDF4.Dataset(geo_path) as geo:
                    supplied = np.asarray(geo.variables["lfn_init"][:])
                initial = np.asarray(first_ds.variables["lfn"][:])
                matches = initial.shape == supplied.shape and np.array_equal(
                    initial, supplied)
                if not matches and initial.shape == supplied.T.shape:
                    matches = np.array_equal(initial, supplied.T)
                activation_time = float(
                    spec["namelist"]["fire"]["fire_ignition_start_time1"])
                validation[
                    "observed_perimeter_activation_time_s"] = activation_time
                if activation_time <= 0.0:
                    validation[
                        "observed_perimeter_installed_at_initial_time"] = matches
                    if not matches:
                        validation["reasons"].append(
                            "zero-time perimeter is absent from the initial lfn"
                        )
                else:
                    inactive = not matches and bool(np.all(initial > 0.0))
                    validation[
                        "observed_perimeter_inactive_at_initial_time"] = inactive
                    if not inactive:
                        validation["reasons"].append(
                            "delayed perimeter is active before its scheduled time"
                        )
    if spec["namelist"]["fire"]["wind_vinterp_opt"] == 0 and expected <= actual:
        validation["wind_profile"] = check_wind_profile(
            run_dir / sorted(expected)[-1], spec)
        if not validation["wind_profile"]["pass"]:
            validation["reasons"].append(
                "Fire winds differ from the prescribed 3D profile")
    elif spec["namelist"]["fire"]["ideal_opt"] == 0 and expected <= actual:
        validation["surface_wind"] = check_surface_wind(
            run_dir / sorted(expected)[-1], spec)
        if not validation["surface_wind"]["pass"]:
            validation["reasons"].append(
                "Fuel-adjusted 10 m winds lost the prescribed direction or magnitude bounds"
            )
    validation["pass"] = not validation["reasons"]
    return validation


def check_wind_profile(path: Path, spec: dict[str, Any]) -> dict[str, Any]:
    """Check the prescribed profile, including the surface logarithmic layer.

    Below the first mass level, wind is zero at z0 and increases logarithmically
    to the first supplied wind. At 6.096 m the expected wind therefore depends
    on each cell's saved roughness. Between mass levels use log-height weights.
    The uppermost mass level is excluded by Interp_profile's kfde-2 bound.
    """
    forcing = spec["inputs"]["atmosphere"]
    interfaces = np.asarray(forcing["height_interfaces_m"])
    heights = 0.5 * (interfaces[:-2] + interfaces[1:-1])
    target = spec["namelist"]["fire"]["fire_wind_height"]
    variation = (forcing["wind_terrain_gradient_per_m"] *
                 spec["inputs"]["terrain"]["amplitude_m"])
    evidence = {"pass": True, "target_height_m": target, "fields": {}}
    with netCDF4.Dataset(path) as dataset:
        z0 = np.ma.asarray(dataset["fz0"][:], dtype=float)
        if (np.ma.count_masked(z0) or not z0.size or
                not np.isfinite(z0).all() or np.any(z0 <= 0) or
                np.any(z0 >= heights[0])):
            return {
                **evidence, "pass": False,
                "reason": "Invalid surface roughness"
            }
        z0 = np.asarray(z0)
        for name, profile in (("uf", "u_profile_m_s"), ("vf", "v_profile_m_s")):
            wind = forcing[profile][:len(heights)]
            if target <= heights[0]:
                expected = wind[0] * np.maximum(0, np.log(
                    target / z0)) / np.log(heights[0] / z0)
            else:
                expected = np.full(
                    z0.shape, np.interp(np.log(target), np.log(heights), wind))
                expected = np.where(target <= z0, 0, expected)
            values = np.ma.asarray(dataset[name][:])
            valid = bool(values.shape == expected.shape and values.size and
                         not np.ma.count_masked(values) and
                         np.isfinite(values).all())
            passed = valid and bool(
                np.allclose(values, expected, rtol=1.0e-4, atol=0))
            if variation and valid:
                # Horizontal mapping may sample different terrain elevations.
                # Bound it with the prescribed amplitude, retaining the local
                # z0 dependence. Cross-driver field tolerances are unchanged.
                lower = np.minimum(expected * (1 - variation),
                                   expected * (1 + variation))
                upper = np.maximum(expected * (1 - variation),
                                   expected * (1 + variation))
                margin = 1.0e-4 * np.abs(expected)
                passed = bool(
                    np.all(values >= lower - margin) and
                    np.all(values <= upper + margin) and
                    (np.all(expected == 0) or np.ptp(values) > 0))
            evidence["fields"][name] = {
                "check":
                    "terrain_scaled_bounds"
                    if variation else "analytic_profile",
                "expected_unscaled_range_m_s": [
                    float(expected.min()),
                    float(expected.max())
                ],
                "maximum_error_from_unscaled_m_s":
                    float(np.max(abs(values - expected))) if valid else None,
                "observed_range_m_s":
                    [float(values.min()),
                     float(values.max())] if valid else None,
                "pass":
                    passed,
            }
            evidence["pass"] = bool(evidence["pass"] and passed)
    return evidence


def check_surface_wind(path: Path, spec: dict[str, Any]) -> dict[str, Any]:
    """Require surface wind direction and attenuation on every fueled cell.

    All selected Anderson fuels have a positive adjustment factor below one.
    Thus nonzero forcing must remain nonzero with the same direction;
    an initially zero component must remain zero. Cross-execution comparisons
    separately check the exact numerical values.
    """
    evidence = {"pass": True, "invalid_cells": {}}
    maximum_factor = (
        1.0 + spec["inputs"]["atmosphere"]["wind_terrain_gradient_per_m"] *
        spec["inputs"]["terrain"]["amplitude_m"])
    with netCDF4.Dataset(path) as dataset:
        for name, forcing_name in (("uf", "u10_m_s"), ("vf", "v10_m_s")):
            values = np.ma.asarray(dataset[name][:]).compressed()
            prescribed = spec["inputs"]["atmosphere"][forcing_name]
            if prescribed == 0:
                invalid = values != 0
            else:
                ratio = values / prescribed
                invalid = (ratio <= 0) | (ratio > maximum_factor)
            count = int(np.count_nonzero(invalid))
            evidence["invalid_cells"][name] = count
            evidence["pass"] = bool(evidence["pass"] and values.size and
                                    count == 0)
    return evidence
