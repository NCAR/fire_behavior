# Restart continuity

[Documentation index](../README.md)

Run `regression.py suite --suite restart --build-dir=BUILD --run-root=RESULTS`
inside a PBS allocation on NCAR systems. Repeat `--build-dir` for serial,
OpenMP, MPI, and hybrid builds. `compile.sh` still owns compilation and CTest
owns invocation. No production physics is changed by these tests.

The suite uses the existing small circle and terrain geometries, fuel maps,
forcing endpoints, and propagation methods. Their existing configuration
figures remain applicable. This table specifies the different event schedules;
all times are seconds from the original 2020-01-01 00:00:00 origin.

| Configuration | Moisture | Atmospheric records | Restart splits | Perimeter |
| --- | --- | --- | --- | --- |
| circle/restart_prescribed | Prescribed 0.08 kg kg-1 | Idealized, no input file | 28 | Line/point ignition |
| terrain/restart_time | `fmoist_freq=0`, `fmoist_dt=16` | Every 12 s | 4, 12, 28 | Observed perimeter at 8 s |
| terrain/restart_step | `fmoist_freq=3`, `fmoist_dt=12` | Every 12 s | 4, 12, 28 | Observed perimeter at 8 s |

All integrations use `dt=4`, run through 60 s, write regular output every 12 s,
and write checkpoints every 4 s. Time-based moisture updates occur at 4, 20,
36, and 52 s on the reviewed base. The split at 4 s precedes perimeter
activation; 12 and 28 s follow it. The 28 s split is between moisture updates
and atmospheric records. Step-based updates first occur at 12 s, exposing the
known initialization defect assigned to P02. A failure remains a failing CTest;
the suite has no expected-failure conversion.

Each case runs one uninterrupted integration and a separate cold first segment,
then resumes only from the preceding segment's checkpoint. Generated input files
are copied unchanged to every segment. In particular, shortening a segment does
not regenerate the forcing or change its temperature/humidity trajectory. Input
and incoming-checkpoint hashes, namelists, original/resolved settings, command,
process status, and model logs are retained. Schedule inventories derive from
the original origin, including output after a split between output events.

| Compared content | Acceptance |
| --- | --- |
| Regular output at every scheduled time | Existing static-field bitwise policy and dynamic `rtol=1e-4`, `atol=0`; unchanged schema/metadata checks |
| Physical checkpoint fields in `restart.STATE_FIELDS`, current forcing, previous forcing and five moisture classes where applicable | Bitwise equality for the same executable and execution layout |
| Checkpoint dimensions, full variable inventory, and global metadata | Exact equality, including clock origin, step count, perimeter activation, and moisture times |
| Fuel fraction and fuel fraction burned in the current timestep | REAL64 storage and a non-REAL32 signal at the final checkpoint; catches quantization even when stored as float64 |
| Independent checkpoint validity | Required fields, finite/unmasked owned cells, expected clock and activation state, five moisture-class dimensions, valid moisture times and moisture in (0,1) for these dry-forcing cases |

The PR #39 writer also saves method work arrays `lfn_0/1/2`, `lfn_s0/1/2/3`
and unused crown fluxes `fcanhfx/fcanqfx`. Their values are not all initialized
for these methods. They remain in the schema comparison but are explicitly
listed as uncompared fields. Physical fields are not excluded to obtain a pass.
`lfn_hist` is checked only for observed perimeters, whose initialization sets
it; `fz0` and atmospheric forcing are checked only for real cases. Idealized
runs do not use those atmospheric fields.
Regular-output tolerances do not weaken the exact checkpoint-state comparison.

The complete selection contains 15 cases across `serial`, `omp4`, `mpi1`,
`mpi4`, and `hybrid4`: 65 model invocations, including the 15 continuous runs,
and 315 file comparisons (90 output and 225 checkpoint comparisons). Counts
of attempted/completed segments and failed comparisons are recorded separately
in `summary.json`; complete field metrics and checkpoint checks are in each
case's `restart.json`. Failed cases are excluded from cross-layout comparisons
and remain failures in the summary. Cross-layout comparisons use continuous
outputs and the existing comparison policy.

The new configurations use same-revision comparisons and do not select the
historical `github-pr` reference, which does not contain their keys. The
existing `pr` suite remains unchanged and checks its approved reference.
No reference candidate is created or approved by this suite.

Restart experiments currently require the standalone driver. NUOPC/ESMX and
WRF-host restart continuity are not established here. Existing coupled PR
tests exercise ordinary integrations only. Cross-layout checkpoint transfers,
legacy-file compatibility across revisions, fractional timesteps, and coupled
flux accumulation/reset are outside P00 and require their assigned sessions.

## Extend a restart experiment

Add `restart_splits` (an increasing list of whole seconds) and the exact
`namelist.time.restart_interval` to a named configuration in `cases.yaml`.
Splits and the final time must align with checkpoint writes; checkpoint writes
must align with the fire timestep. The segment runner derives `time.restart`
and the start/end dates. Add the configuration and executions explicitly to
the `restart` suite. Preserve original forcing and compare every expected
checkpoint and regular output, including the first resumed event.
