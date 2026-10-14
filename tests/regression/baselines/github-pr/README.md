# Approved GitHub PR-suite baseline

Maria Frediani requested this strict-flags refresh on 2026-10-14:

> make the changes and push it then update the baseline with the strict flags

The numerical payload retains its prior approval: all 64 NetCDF outputs and
32 scientific-settings files are byte-identical to the reference approved from
[run 38612379390](https://github.com/NCAR/fire_behavior/actions/runs/38612379390).
All 96 checksums were independently verified against both generations. This
refresh records the strict-flags source and environment without changing the
accepted numerical results or comparison tolerances.

- Source: `6cefd1545c3ddc993e26df4caa1d3ff9295037ba`, clean worktree.
- Generation: [GitHub run 38644049156](https://github.com/NCAR/fire_behavior/actions/runs/38644049156).
- Coverage: the complete `pr` suite, 32 executions across serial, OpenMP,
  MPI, hybrid, NUOPC, and ESMX, with outputs at 0 and 60 s.
- Environment: Ubuntu 22.04, GNU Fortran 11.4.0, OpenMPI 4.0.3, and ESMF 8.5.0.
  See [environment.txt](environment.txt) for the full record.
- GNU floating-point flags, including the external ESMX driver:
  `-fno-fast-math -ffp-contract=off -fprotect-parens -frounding-math`.
  The `Strict` build type uses `-O2`; existing build types retain their prior flags.

The generating run passed formatting, four builds, 154 unit CTests, 32 model
runs, and 32 cross-execution comparisons. All 24 within-driver comparisons
were bitwise identical; maximum cross-driver relative error was 0.000636870574%,
below the 0.01% limit. Normal PR
[run 38644053759](https://github.com/NCAR/fire_behavior/actions/runs/38644053759)
also passed all 32 comparisons against the previous approved reference, with
every compared field bitwise identical.

The manifest retains the generating source revision, rather than the later
commit that records this refresh. The original approval and provenance remain
preserved in commit `7461e3c`. The downloaded strict-flags candidate was generated
in a separate directory and remained unapproved until this verified refresh.

Normal GitHub checks compare each matching execution against these files.
The tested native Derecho GNU 12.2 environment also matches these fields with
the strict flags; see [validation history](../../doc/validation.md). This does
not approve the separate Intel reference or establish longer-duration agreement.
Future numerical changes require a newly generated and reviewed reference.
