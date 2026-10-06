# Validation history and limits

[Documentation index](../README.md)

These records describe specific revisions. They do not automatically validate
later source, documentation examples, renamed registrations, or new cases.

## Explicit wind modes and atmospheric-array cleanup

The follow-up to Pedro's #57 review replaces the numeric wind-option branch
with named constants and an explicit invalid-option diagnostic. It also removes
the atmospheric `ua`/`va` duplicates: the 10 m path maps `u10`/`v10` directly,
then applies WAF. Both wind paths retain their final values in `uf`/`vf`.

Derecho PBS `7723710.desched1` tests combined revision `f3e78f8`, containing
#57 code `028fc8b`, through the normal submission template. All four builds,
including NUOPC/ESMX, pass. All 150 unit CTest invocations pass (37 serial,
37 OpenMP, 38 MPI, 38 hybrid), including 40 Python checks per build and the new
10 m state-mapping and invalid-option tests. All 32 generated runs and all
32 comparisons pass. The 24 within-driver comparisons remain bitwise equal;
maximum cross-driver relative error remains 0.0079873469%.

A separate before-and-after audit against `4e71058` compares all 19 variables
in all 64 generated output files. Every stored field is bitwise identical,
including masks; schema and metadata match, and generated inputs and namelists
are byte-identical. This establishes unchanged output for the tested cases.
No interpolation formula, scientific setting, or tolerance changed.

All seven legacy runs complete and retain their 35 original comparison failures.
Their 77 NetCDF outputs are bitwise identical to the preceding validation, with
matching schema and metadata. All 100 checked diagnostic values/timestamps per
case also match. `before-after-legacy.json` records this separate audit. The
job correctly exits 1 after 8 min 24 s, with generated PR status 0 and legacy
status 8. Passing the generated suite does not imply a legacy reference pass.

Evidence is under `cfbm_f3e78f8_20261005/`: `results/pr-pm3srp1g/summary.json`,
`before-after-generated.json`, `validation-audit.json`, `suite-pr.log`, and
`pr57-source-equivalence.json`. The last file confirms that the modified model
files and new unit test match the prerequisite PR source exactly. The compiler,
MPI, library, and Python environment is the same as the run below. Subsequent
commits add documentation only. No new intermediate-time or longer integration
is implied; the previously recorded limitations remain.

## Wind interpolation order

PR #57 (`1bb49f9`) changes the standalone 3D pathway to remap wind and
geopotential levels horizontally before vertical sampling, using mapped
roughness, consistently with NUOPC. The sampling height remains 6.096 m and
the comparison tolerance remains 0.01%. Inputs and the vertical formula are
unchanged. This resolves the four configured 60 s comparison failures recorded
below, without establishing agreement at every intermediate timestep.

The combined harness revision is `4e71058`, which merges #57 `1bb49f9` into
the explicit-configuration harness. Derecho PBS `7715106.desched1` uses the
normal [submission template](../submit_derecho.pbs), account NERP0002, one
node, and a one-hour request in `develop` (routed to `cpudev`). Submit from
the clean repository root with the allocation set in `PBS_ACCOUNT`:

```bash
qsub -A "$PBS_ACCOUNT" tests/regression/submit_derecho.pbs
```

The template records the exact source, modules, commands, and isolated outputs
under `cfbm_4e71058_20261004/`. The compiler/library stack is Intel 2025.2.1,
Cray MPICH 8.1.32, parallel NetCDF 4.9.3, ESMF 8.9.1, CMake 3.31.8, and the
module-provided `npl-2026a` Python environment. Model launches use the module
MPI after Conda is deactivated.

| Combined validation | Result |
| --- | --- |
| Serial, OpenMP, MPI/NUOPC/ESMX, hybrid builds | 4/4 pass |
| Unit CTest invocations across the four builds | 142/142 pass, including 40 Python tests per build |
| Generated integrations and physical/output checks | 32/32 pass |
| Same-driver comparisons | 24/24 pass; all stored fields bitwise identical |
| Standalone/coupled comparisons | 8/8 pass, including both wind configurations and one/four ranks |
| All generated comparisons | 32/32 pass; PR suite status 0 |
| Maximum cross-driver relative error | 0.0079873469%, below 0.01% |

This combined run rebuilds ESMX through the normal build path and tests the
corrected analytical wind-order fixture in all four builds. The source snapshot
is clean at `4e71058b8b9cb9171b96542ae5ea4ca9d3972f0d`; subsequent documentation
edits do not change the tested code or configuration. Unit CTest
counts are 35 serial, 35 OpenMP, 36 MPI, and 36 hybrid. The 40 Python checks
are collected by one of those CTests in each build.

Artifacts include `results/pr-e8bx92_b/summary.json`, `suite-pr.log`,
`validation-audit.json`, per-build configuration/build logs, and the individual
run directories listed in the summary. Local YAPF, shell-syntax, and relative
documentation-link checks pass. No GitHub CI run or baseline approval is
claimed for these unpublished commits.

All seven legacy integrations completed, with eleven NetCDF outputs per case.
All 35 original text comparisons still fail. An audit against `32428ed` found
the same 100 checked diagnostic values and timestamps in every case, with zero
changed rows. This audit does not assert equality of the untested legacy NetCDF
fields. `legacy-preservation-audit.json` records the comparison and output paths.
The final statuses are `pr 0` and `legacy 8`; PBS correctly exited **1** after
8 min 21 s. The full job is therefore not an all-tests pass.

### Intermediate-time diagnostic and remaining limits

The separate every-4-second diagnostic retained in the #57 investigation
fails at 20 s in one `lfn` cell, zero-based `(y, x) = (40, 33)`:

| Quantity | Value |
| --- | --- |
| Standalone level set | 0.3912063241004944 m |
| Four-rank NUOPC level set | 0.3912506699562073 m |
| Absolute difference | 4.4345855712890625e-5 m |
| Relative difference | 0.0113356694% |
| Relative tolerance | 0.01% |

All other saved times through 60 s pass in that diagnostic. Maximum wind
discrepancy increases after order alignment, from `3.33786e-5` to
`7.82013e-5 m/s`. Profile recording and wind-replay controls support a remaining
contribution from absolute-geopotential remapping and conversion to height above
ground. Order alignment therefore does not eliminate numerical differences.

The default `fire_upwinding=9` is hybrid WENO5/ENO1, with fifth-order
reconstruction near the front. Its sensitivity is a reason for a controlled
method comparison, not an established explanation of the failing cell. No
method change or tolerance relaxation is included here. The earlier 600 s
failures and the untested 3600 s suite remain separate limitations.

Diagnostic evidence is in `cfbm-u3d-order-20261004/diagnostic/`, including
`summary.json`, per-run outputs, and recorded profiles. The exact prerequisite
was validated separately in PBS `7712874.desched1`: four builds, 138 model unit
CTest invocations, 16 terrain integrations, and 16 comparisons passed. That
prerequisite-only job did not rebuild ESMX with the complete harness.

## Explicit configurations and 6.096 m winds

Derecho PBS `7710945.desched1` tested code revision `32428ed` on 2026-10-04
through the normal [PBS template](../submit_derecho.pbs). All four builds and
unit CTests passed, including 40 Python tests per build. The revised cases are
`circle`, `fuels`, and `terrain`, with explicit named configurations and
`small`/`large` scales. An earlier run of `c709b00` in job `7710923.desched1`
produced the same numerical results; the follow-up restored fractional-timestep
support without changing any selected experiment.

| Check at 60 s | Result |
| --- | --- |
| Individual generated integrations and physical checks | 32/32 pass |
| Same-driver execution-layout comparisons | 24/24 pass; all stored fields bitwise identical |
| Standalone/coupled `terrain / u10m` comparisons | 4/4 pass |
| Standalone/coupled `terrain / u3d` comparisons | 0/4 pass |
| All cross-execution comparisons | 28/32 pass; PR suite fails |

The four failures compare standalone serial against NUOPC/ESMX with one/four
ranks. At the requested **6.096 m** sampling height, each comparison exceeds
the unchanged **0.01%** threshold in six fields at 60 s. Each field has one
violating cell. The maximum relative error is **0.01434025%**.

Indices below are zero-based `(y, x)` in the saved `(ny, nx)` arrays. The field
values are representative of all four failed comparisons.

| Field | Cell | Standalone | Coupled | Relative difference |
| --- | --- | --- | --- | --- |
| `fuel_frac_burnt_dt` | `(37, 43)` | 0.00091175473 | 0.00091188541 | 0.0143324% |
| `fire_area` | `(37, 43)` | 0.0046392241 | 0.0046398705 | 0.0139320% |
| `fgrnhfx` (W m-2) | `(37, 43)` | 3208.1943 | 3208.6543 | 0.0143371% |
| `fgrnqfx` (W m-2) | `(37, 43)` | 308.14972 | 308.19391 | 0.0143403% |
| `emis_smoke` (kg m-2) | `(37, 43)` | 0.000016338645 | 0.000016340988 | 0.0143394% |
| `lfn` (m) | `(33, 40)` | 0.49328893 | 0.49322414 | 0.0131343% |

The first cell has Anderson fuel category 2; the level-set cell has category
13. Coordinates, terrain, and categories remain bitwise identical. Wind and
roughness comparisons pass: maximum wind-component error is
`3.33786e-5 m/s` (0.00035565%), and maximum roughness error is `4.47035e-8 m`.
Small remapping differences are present, but these output comparisons alone do
not isolate which difference causes the fire-field threshold crossings.

All seven legacy integrations completed with eleven outputs each and retained
all 100 checked diagnostic rows per case exactly relative to the previous
validation. Their 35 original comparison failures remain unchanged. The final
job exited **1** after 8 min 28 s, with `pr 1` and `legacy 8` in
`suite-status.txt`. This is a failed regression suite, despite all individual
generated integrations and unit tests completing successfully.

### Controlled attribution

A configuration audit against `c070f82` found byte-identical generated NetCDF
inputs and identical namelist values except the requested 3D sampling-height
change from 20 to 6.096 m. Derecho PBS `7710983.desched1` then reused the
`32428ed` executables and changed only that height back to 20 m in an external
configuration file. The control exited 0 after 32 s. All three integrations and both standalone/coupled
comparisons passed, with maximum relative error 0.00157165%. All saved fields
for each driver were bitwise identical to its earlier 20 m outputs from
`42d7700`. This attributes the newly failing acceptance to the revised physical
configuration, not the YAML/renderer refactor. It does not establish a complete
physical cause or justify changing the requested height back.

The production configuration retains 6.096 m. No tolerance, model physics,
legacy reference, or approved baseline was changed. This revision retained the short 3D cross-driver
failure; the subsequent [wind-order correction](#wind-interpolation-order)
addresses the configured 60 s comparison.

Artifacts, beneath the validation submitter's scratch `tmp` directory:

- `cfbm_32428ed_20261004/results/pr-nv0td689/summary.json`: all 32 runs and comparisons.
- The case directories listed there contain the full NetCDF outputs and logs.
- `cfbm_32428ed_20261004/u3d-failure-audit.json`: exact failing cells and nearby diagnostics.
- `cfbm_32428ed_20261004/control20m/`: configuration, outputs, comparison report, prior-output audit, and PBS status.
- `pr49-config-audit/report.json`: input-byte and namelist audit against the preceding configuration.

## Folder and case-name changes

Derecho PBS `7709881.desched1` tested commit `42d7700` on 2026-10-04 using
[submit_derecho.pbs](../submit_derecho.pbs), one node in `develop` (routed to
`cpudev`), and a one-hour request. All four builds passed. The PR suite passed
32 generated runs and 32 comparisons, including NUOPC and ESMX. Unit CTests
passed in every build, with 34 Python checks in each generated test system.
Same-driver fields remained bitwise identical. Maximum cross-driver relative
error was 0.00157165%, below the unchanged 0.01% threshold.

At that revision the case names were `terrain_u10m` and `terrain_u3d`;
the rename preserved their settings, including the former 20 m 3D target. All 30 relocated legacy files were checked against
their previous contents. Only the isolation wrapper's usage comment changed.
The normal coupled runners also successfully staged the two shared
configuration files from their new location.

The seven legacy cases completed and reproduced all 100 checked diagnostic
rows per case exactly, including their timestamps, compared with the saved
pre-move runs. Each produced eleven NetCDF files. All 35 original legacy
comparisons still failed. `suite-status.txt` records PR status 0 and legacy
status 8. The PBS job correctly exited 1 after 8 min 31 s, demonstrating that
collection of failures no longer reports overall success.

Artifacts are in the validation submitter's scratch directory
`cfbm_42d7700_20261004/`, including
`results/pr-r83a5dl9/summary.json`, per-build logs, and `suite-status.txt`.
The environment resolved `ncarcompilers/1.2.0` during this run. The template
subsequently names that loaded version explicitly. Later documentation
clarifications do not change the validated model or comparison settings.

## Standard generated cases

Derecho PBS `7702254.desched1` validated model/harness commit `1426bed` on
2026-10-03 using the normal build route, including ESMX without an external
linking workaround. All four builds and unit CTests passed, including 34
Python tests. Quick passed 14 runs and 10 comparisons. PR passed 32 runs and 32
comparisons. Maximum cross-driver relative error was approximately 0.001572%,
below 0.01%.

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
case. It used model `29bc1b8` and the earlier external ESMX link workaround. All
four standalone/coupled comparisons failed the unchanged tolerance, first at 180
s. NUOPC and ESMX saved fields agreed with each other. Longer-duration
same-driver MPI/OpenMP/hybrid comparisons were not performed.

Final integrated differences were small, but local 3D timestep fuel-consumption
and heat-flux differences were substantial. The largest final local timestep
fuel-consumption relative difference was approximately 52.2%. These failures
cannot be dismissed as an aggregate rounding artifact. The 60 s CI duration and
tolerances were not changed to accept them. Temperature/humidity endpoints were
stretched over 600 s, so the forcing history during the first 60 s also differs
from the standard experiment.

## Historical text diagnosis

The [legacy
comparison](advantages.md#evidence-that-printed-agreement-is-insufficient)
describes the 2026-10-04 re-evaluation. A controlled earlier test changed only
the fire integration interval: corrected timing failed the old comparisons,
while original timing passed. Shifting output rows by 0.5 s reproduces part of
the agreement but does not align every evolving state variable. Fuel moisture
and forcing timing remain possible contributors to test8 residuals, without an
isolated causal experiment.

## Evidence locations

These are directory names in the original validation submitter's scratch
archive. They are provenance, not required contributor paths:

- Standard build/results: `cfbm-pr49-final-20261003/`.
- Longer experiment: `cfbm-pr49-long-20261003/`.
- Controlled timing: `cfbm-pr47-original-timing-20261002/`.
- Aligned test8 audit: `cfbm-test8-aligned-audit-20261004/`.

No new approved baseline or full 3600 s campaign is implied by these records.
See [deferred work](deferred.md) for the current follow-up list.
