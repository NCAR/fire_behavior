# GitHub PR-suite baseline with coupled hybrid coverage

Maria Frediani requested this extension on 2026-10-14:

> can you update the baseline to include hybrid esmx and nuopc?

This reference adds four cases to the previous approved selection: NUOPC and
ESMX terrain runs with `u10m` and `u3d`, using four MPI ranks and four OpenMP
threads per rank. The previous 32-case reference remains in
[github-pr/](../github-pr/README.md).

- Source: `4b6535d14db185c20d40f471873580311b1feb00`, clean worktree.
- Generation: [GitHub run 38648160726](https://github.com/NCAR/fire_behavior/actions/runs/38648160726).
- Coverage: 36 runs, with 72 NetCDF outputs at 0 and 60 s and 36 settings files.
- Environment: Ubuntu 22.04, GNU Fortran 11.4.0, OpenMPI 4.0.3, ESMF 8.5.0.
  Full provenance is in [environment.txt](environment.txt).
- Build type: GNU `Strict`, including the external ESMX driver.

The generating run passes formatting, four builds, 154 unit CTests, 36 model
runs, and 40 cross-execution comparisons. All 28 within-driver comparisons
are bitwise equal. Maximum cross-driver relative error remains
0.000636870574%, below the unchanged 0.01% limit.

All 108 payload checksums were independently verified. The existing 64 NetCDF
outputs and 32 settings files are byte-identical to the previous approved
reference. Each added hybrid case also has NetCDF outputs and scientific
settings byte-identical to its corresponding four-rank MPI case. This adds
execution coverage without changing accepted numerical results, scientific
settings, comparison rules, or model source.

The manifest records the generating source revision, not the later commit that
stores this reference. Approval records the requested extension after these
checks; candidate generation alone does not approve a reference. The raw
downloaded candidate is retained separately with `approval: unapproved`.

Derecho GNU validation (PBS `7823739.desched1`) passes the same 36 runs and
40 internal comparisons; all 1,368 saved fields match this GitHub reference
bitwise. The updated Intel PBS template (job `7823740.desched1`) also passes
36 runs and 40 internal comparisons, without a historical GNU-reference check.

Use `--reference=tests/regression/baselines/github-pr-hybrid` for historical
comparisons, including coupled hybrid runs. This reference does not establish
agreement for arbitrary compilers or longer integrations, and does not approve
the separate Intel candidate. See [validation history](../../doc/validation.md).
