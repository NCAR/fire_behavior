# Compared variables and acceptance rules

[Documentation index](../README.md)

The standard inventory contains 19 variables. Each expected output file must
contain exactly the fields listed in `expected_output_fields` in
[cases.yaml](../cases.yaml). The comparator checks all variables, not a selected
subset. Adding an output therefore requires an explicit inventory and metadata
update, even if the same new field appears in both runs.

## Rules

**Bits** means identical stored bits per element after normalizing byte order.
This is independent of NetCDF file compression and whole-file checksums.
**Tolerance** means, for finite unmasked values:

```text
abs(test - reference) <= 1e-4 * abs(reference)
```

This is 0.01% relative tolerance with zero absolute allowance. A zero reference
requires a zero test value. For execution comparisons, serial is preferred as
the reference. Within a coupled driver, the smallest available MPI layout is
preferred. Cross-driver comparisons use the matching standalone case. Approved
historical references use the same-driver policy.

## Inventory

| NetCDF variable | Meaning | Units | Same driver / historical reference | Standalone versus NUOPC or ESMX |
| --- | --- | --- | --- | --- |
| `lats` | Fire-cell latitude | degrees_north | Bits | Bits |
| `lons` | Fire-cell longitude | degrees_east | Bits | Bits |
| `zsf` | Terrain height | m | Bits | Bits |
| `nfuel_cat` | Fuel category identifier | 1 | Bits | Bits |
| `fz0` | Mapped roughness length | m | Bits | Tolerance |
| `uf` | Eastward wind used by fire spread | m s-1 | Tolerance | Tolerance |
| `vf` | Northward wind used by fire spread | m s-1 | Tolerance | Tolerance |
| `lfn` | Signed distance to fire perimeter | m | Tolerance | Tolerance |
| `fire_area` | Burned-area fraction within cell | 1 | Tolerance | Tolerance |
| `fuel_frac` | Remaining fuel fraction | 1 | Tolerance | Tolerance |
| `fuel_frac_burnt_dt` | Fuel fraction burned during current timestep | 1 | Tolerance | Tolerance |
| `fgrnhfx` | Ground sensible heat flux | W m-2 | Tolerance | Tolerance |
| `fgrnqfx` | Ground latent heat flux | W m-2 | Tolerance | Tolerance |
| `emis_smoke` | Particulate emissions per cell area during current timestep | kg m-2 | Tolerance | Tolerance |
| `fmc_g` | Ground fuel moisture content | kg kg-1 | Tolerance | Tolerance |
| `fire_t2` | Air temperature at 2 m | K | Tolerance | Tolerance |
| `fire_q2` | WRF water-vapor mixing ratio at 2 m | kg kg-1 | Tolerance | Tolerance |
| `fire_psfc` | Surface air pressure | Pa | Tolerance | Tolerance |
| `fire_rain` | Precipitation diagnostic | Unresolved across drivers | Tolerance | Tolerance |

`nfuel_cat` is stored as floating point by the current writer but represents
categories, so it remains exact. Terrain and coordinates also remain exact. Only
`fz0` receives a cross-driver static-field exception because it is spatially
interpolated using different standalone and ESMF mapping weights.

The observed same-driver results were bitwise identical for all fields. That is
stronger than the acceptance requirement for the dynamic fields in this table.
Do not describe dynamic-field bitwise equality as an enforced rule. Likewise,
double-precision internal fuel arithmetic does not change the fraction-based
comparison rule or add an absolute mass tolerance.

`fire_q2` keeps its legacy name but represents WRF mixing ratio. Humidity
interpretation remains under separate review. `fire_rain` intentionally has no
units attribute until the coupled convention is resolved. Current generated
cases use zero rain and cannot validate nonzero precipitation conversion.

## Output-level and applicability limits

`grad_norm_ls` and `grad_norm_reinit` have metadata definitions in
[check_outputs.py](../check_outputs.py), with units `1`, but are **not part of
the standard 19-field inventory**. A case enabling the writer's diagnostic
output must also request these fields. They would use numerical tolerance unless
an explicitly justified policy changed their classification.

The current YAML inventory is shared by all cases. If a development introduces
case-dependent output sets, extend inventory selection in the runner and test
that selection explicitly. Adding a diagnostic field globally while only one
case writes it will correctly make the other cases fail.

The idealized case represents `fire_t2`, `fire_q2`, `fire_psfc`, `fire_rain`,
and `fz0` as masked, inapplicable data. These variables remain in the inventory
and their masks are checked. The final real-case fields must have no masked
cells. Initialization can contain missing values before a field becomes
applicable, which is distinct from a valid zero.

## Checks beyond numerical values

File names/times, variable sets, dimension lengths/order, shapes, storage types,
and variable metadata must match. Global metadata is exact in the current suite.
The low-level comparator accepts an explicit exclusion list, but suite and
reference calls do not supply one. Missing-data masks and NaN locations must
agree. Infinities fail, and per-run validation rejects unmasked nonfinite
values. Matching NaNs in a direct comparison do not establish valid model
output. Reports include bitwise differences even when tolerance governs
acceptance. Fully masked fields have no numerical error statistic.

Per-run checks also require evolving level set, fuel consumption, positive fire
flux, configured moisture evolution, delayed-perimeter initialization, and valid
forcing/winds. The atmospheric forcing check has a separate bound of eight
float32 machine epsilons. The uniform 3D wind control uses `rtol=1e-4` and
`atol=0`. Variable 3D winds use prescribed bounds and nonzero spatial variation,
followed by the field comparisons above.

## When the model gains an output

1. Confirm the writer's name, dimensions, precision, units, fill value, and
   physical meaning, including which drivers and output levels provide it.
2. Add the variable to `expected_output_fields` and its metadata to
   `OUTPUT_METADATA`. Add an applicability check if some cases legitimately
   leave it masked.
3. Decide whether exact comparison or tolerance is scientifically appropriate.
   Add static fields to `static_fields`. Do not infer that a categorical field
   permits tolerance just because it is stored as a real number.
4. Add a focused test that changes the field and demonstrates the intended
   failure. Exercise the feature in at least one generated model case.
5. Update this table and model documentation. Validate before creating a new
   reference candidate. Never silently replace an approved reference.

Implementation sources: [compare_outputs.py](../compare_outputs.py),
[run_suite.py](../run_suite.py), [check_outputs.py](../check_outputs.py), and
[cases.yaml](../cases.yaml).
