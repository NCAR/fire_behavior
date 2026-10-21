# Fractional event schedules

Run `regression.py suite --suite scheduling --build-dir=BUILD --run-root=RESULTS`
inside PBS. Repeat `--build-dir` for serial, OpenMP, MPI, and hybrid builds.
This suite uses P00's restart runner and the existing terrain geometry, fuel
map, forcing endpoints, and propagation method. The geometry figures remain
applicable; the table below records the different timing. The observed
perimeter is active at time zero, and moisture updates every fire step.

| Configuration suffix | Input dt (s) | Clock dt (s) | End (s) | Output (s) | Atmosphere (s) | Restart (s) | Chained restart splits (s) |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 03 | 0.3 | 3/10 | 72 | 12 | 18 | 9 | 9, 27 |
| rounded | 0.222222 | 2/9 | 72 | 12 | 18 | 8 | 8, 24 |
| nested | 0.277777777777778 | 5/18 | 60 | 15 | 20 | 10 | 10, 30 |

Each `schedule_*` configuration runs continuously through standalone, NUOPC,
and ESMX where the build supports them. The matching `schedule_*_restart`
configuration adds standalone chained restarts. These remain separate test
names: a coupled continuous run does not establish coupled restart continuity.
The suite selects serial, omp4, mpi4, and hybrid4. It also includes P00's
`circle/restart_prescribed` to check the legacy idealized clock schema. Four
copies of that checkpoint lose only the P04 clock attributes before restart;
the input checkpoints remain unchanged. This is a schema compatibility fixture,
not a claim of equivalence across a scientific correction.

The 40 continuous cases, 44 restart-segment integrations, 28 expected startup
rejections, four runs ending between events, and four legacy-schema restarts
give 120 model invocations. Each rejected configuration must produce its specific
diagnostic before writing history. Rejections cover unaligned output/end times,
forcing-record gaps, incompatible or incomplete clock/cadence metadata, and legacy forcing
state whose cadence cannot be established. The valid 69 s endpoint is between
output, atmospheric, and checkpoint events and must be reached exactly.

Acceptance requires the exact history-file inventory, forcing temperature and
humidity from the correct record at every output, existing physical checks,
layout/driver comparisons, and P00's bitwise checkpoint comparisons. Restart
splits include times between output and atmospheric events. P04 does not change
P00's moisture initialization acceptance or convert its known P02 failures into
passes. New configurations have no approved historical baseline; existing `pr`
checks retain their baseline selection and tolerances.

`fire_clock_unit` checks seven-day event counts, exact event timestamps, leap-day
rollover, subsecond resolution at long elapsed times, independent output/restart
periods, restored event phase, and the exchange reset predicate used by NUOPC.
Its weighted flux accumulator checks complete windows and rejects duplicate or
mid-window resets. Negative tests reject unaligned intervals/final time and
missed events. Python checks reject partial steps and stale intermediate forcing
records even when the final forcing record is correct.

The new clock treats input rounding as specified in `doc/Configuration.rst`.
The physics timestep remains the configured default-real value. P02 must use the
shared REAL64 scheduling-time interface explicitly when integrating its moisture
lifecycle; this suite does not implement that separate correction.
