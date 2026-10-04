# Validation history and limits

[Documentation index](../README.md)

These records describe specific revisions. They do not automatically validate
later source, documentation examples, renamed registrations, or new cases.

## Standard generated cases

Derecho PBS `7702254.desched1` validated model/harness commit `1426bed` on
2026-10-03 using the normal build route, including ESMX without an external
linking workaround. All four builds and focused CTests passed, including
34 Python tests. Quick passed 14 runs and 10 comparisons. PR passed 32 runs
and 32 comparisons. Maximum cross-driver relative error was approximately
0.001572%, below 0.01%.

Within each standalone case, serial, OpenMP with one/four threads, MPI with
one/four ranks, and four-rank × four-thread hybrid fields were bitwise equal.
Within NUOPC and within ESMX, one/four-rank terrain outputs were bitwise equal.
Coupled OpenMP/hybrid configurations were not tested. These records use the
former names `terrain_10m` and `terrain_3d`.

All seven original legacy tests completed but failed all 35 original text
comparisons. The job's overall exit was zero because its earlier collection
script recorded suite statuses separately. That exit was not an all-tests pass.
The supplied PBS template now propagates any suite failure to the job exit.

## Longer integration

A separate 600 s experiment in PBS `7702238.desched1` completed six runs:
standalone serial, four-rank NUOPC, and four-rank ESMX for each terrain wind
case. It used model `29bc1b8` and the earlier external ESMX link workaround.
All four standalone/coupled comparisons failed the unchanged tolerance, first
at 180 s. NUOPC and ESMX saved fields agreed with each other. Longer-duration
same-driver MPI/OpenMP/hybrid comparisons were not performed.

Final integrated differences were small, but local 3D timestep fuel-consumption
and heat-flux differences were substantial. The largest final local timestep
fuel-consumption relative difference was approximately 52.2%. These failures
cannot be dismissed as an aggregate rounding artifact. The 60 s CI duration
and tolerances were not changed to accept them. Temperature/humidity endpoints
were stretched over 600 s, so the forcing history during the first 60 s also
differs from the standard experiment.

## Historical text diagnosis

The [legacy comparison](advantages.md#evidence-that-printed-agreement-is-insufficient)
describes the 2026-10-04 re-evaluation. A controlled earlier test changed only
the fire integration interval: corrected timing failed the old comparisons,
while original timing passed. Shifting output rows by 0.5 s reproduces part of
the agreement but does not align every evolving state variable. Fuel moisture
and forcing timing remain possible contributors to test8 residuals, without
an isolated causal experiment.

## Evidence locations

These are historical artifact locations, not required contributor paths:

- Standard build/results: `/glade/derecho/scratch/frediani/tmp/cfbm-pr49-final-20261003/`.
- Longer experiment: `/glade/derecho/scratch/frediani/tmp/cfbm-pr49-long-20261003/`.
- Controlled timing: `/glade/derecho/scratch/frediani/tmp/cfbm-pr47-original-timing-20261002/`.
- Aligned test8 audit: `/glade/derecho/scratch/frediani/tmp/cfbm-test8-aligned-audit-20261004/`.

No new approved baseline or full 3600 s campaign is implied by these records.
Restart, PR #39, method `(4,5)`, and humidity PR #46 remain deferred.
