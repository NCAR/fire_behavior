# Fractional event schedules

Fractional scheduling coverage runs in the existing `pr` and `restart` suites,
including GitHub CI. Use `regression.py suite --suite pr` for continuous runs
and `--suite restart` for checkpoint chains and rejected schedules. Supply
`--build-dir=BUILD` for each serial, OpenMP, MPI, and hybrid build and
`--run-root=RESULTS`; run inside PBS on NCAR systems.

The cases use the existing terrain geometry, fuel map, forcing endpoints,
propagation method, and restart runner. The geometry figures remain applicable;
the table below records the different timing. The observed perimeter is active
at time zero, and moisture updates every fire step.

| Configuration suffix | Input dt (s) | Clock dt (s) | End (s) | Output (s) | Atmosphere (s) | Restart (s) | Chained restart splits (s) |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 03 | 0.3 | 3/10 | 72 | 12 | 18 | 9 | 9, 27 |
| rounded | 0.222222 | 2/9 | 72 | 12 | 18 | 8 | 8, 24 |
| nested | 0.277777777777778 | 5/18 | 60 | 15 | 20 | 10 | 10, 30 |

The `pr` suite runs each `schedule_*` configuration continuously through
standalone, NUOPC, and ESMX where supported, using its normal `serial`, `omp1`,
`omp4`, `mpi1`, `mpi4`, and `hybrid4` layouts. This adds 36 model runs to the
36 existing reference cases. The `restart` suite runs the three matching
`schedule_*_restart` configurations on `serial`, `omp4`, `mpi1`, `mpi4`, and
`hybrid4`, adding 15 case-level tests. A coupled continuous run does not
establish coupled restart continuity.

The existing `circle/restart_prescribed` configuration also checks legacy
idealized checkpoints without `clock_seconds`, `clock_steps`, and `interval_atm`.
Only checkpoint copies lose these attributes; original files remain unchanged.
This tests whether the original clock can be reconstructed from the remaining
metadata, not equivalence across a scientific correction.

Each `schedule_03_restart` case additionally runs seven expected startup
rejections and one valid run ending between events. Across five layouts this
gives 35 rejection probes and five endpoint checks. Rejections must produce
their specific diagnostic before writing history: unaligned output/end times,
forcing-record gaps, incompatible or incomplete clock/cadence metadata, and
legacy forcing state whose cadence cannot be established. The valid 69 s
endpoint is between output, atmospheric, and checkpoint events and must be
reached exactly. `scheduling.py` supplies these checks to the existing case
runner; there is no separate scheduling suite or launcher.

Acceptance requires the exact history-file inventory, forcing temperature and
humidity from the correct record at every output, existing physical checks,
layout/driver comparisons, and bitwise checkpoint comparisons. Restart splits
include times between output and atmospheric events. Fractional configurations
have no approved historical baseline; their exact timing/forcing assertions
and same-revision comparisons provide acceptance. The existing 36 reference
cases retain their baseline selection and tolerances.

`fire_clock_unit` checks seven-day event counts, exact event timestamps, leap-day
rollover, subsecond resolution at long elapsed times, independent output/restart
periods, restored event phase, and the exchange reset predicate used by NUOPC.
Its weighted flux accumulator checks complete windows and rejects duplicate or
mid-window resets. Negative tests reject unaligned intervals/final time and
missed events. Python checks reject partial steps and stale intermediate forcing
records even when the final forcing record is correct.

The new clock treats input rounding as specified in `doc/Configuration.rst`.
The physics timestep remains the configured default-real value. Moisture elapsed
time still follows the component's configured timestep. Connecting that lifecycle
to the REAL64 scheduling clock, including long-duration moisture-time precision,
requires separate validation; these short cases do not establish that behavior.
