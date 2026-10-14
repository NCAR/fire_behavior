# Approved GitHub PR-suite baseline

Maria Frediani approved this candidate on 2026-10-14:

> Approve the GitHub baseline from run 38612379390 and commit it to PR49.

- Source: `cac21adf13a3ce426b6b89b93f7df436f0f7f103`, clean worktree.
- Generation: [GitHub run 38612379390](https://github.com/NCAR/fire_behavior/actions/runs/38612379390).
- Coverage: the complete `pr` suite, 32 executions across serial, OpenMP,
  MPI, hybrid, NUOPC, and ESMX, with outputs at 0 and 60 s.
- Environment: Ubuntu 22.04, GNU Fortran 11.4.0, OpenMPI 4.0.3, and ESMF 8.5.0.
  See [environment.txt](environment.txt) for the full record.

The generating run passed formatting, four builds, 154 unit CTests, 32 model
runs, and 32 cross-execution comparisons. All 24 within-driver comparisons
were bitwise identical; maximum cross-driver relative error was 0.000636870574%,
below the 0.01% limit.

All 96 recorded checksums were verified after download and again when copying
the baseline here. The 64 NetCDF outputs and 32 scientific-settings files are
unchanged from the candidate. Only the manifest approval flag was changed;
this README records the authorization. The manifest retains the generating
source revision, rather than the later commit that stores the baseline.

Normal GitHub checks compare each matching execution against these files.
This reference is specific to the recorded GitHub environment; it does not
approve an HPC baseline or establish longer-duration agreement. Future changes
require a newly generated and reviewed reference in a separate directory.
