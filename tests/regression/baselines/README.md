# Approved regression baselines

The GitHub workflow expects its reviewed PR-suite reference in `github-pr/`.
That directory is intentionally absent until the team approves a candidate.
Normal PR checks fail when the reference is missing; they never generate it.

Use the manual workflow's `create_reference` input to build all four Release
configurations and generate an unapproved candidate after the PR suite passes.
Download the candidate artifact and review its NetCDF fields, scientific settings,
coverage, source revision, environment, and cross-execution comparison reports.

Each reference contains `reference.json`, one `settings.json` per named execution,
and the expected NetCDF output files. Keep the environment record and a README
with the approving reviewer, date, source revision, and workflow run URL.
Only after explicit approval may `approval` be changed to `approved` and the
reference committed. The final PR workflow must then pass against those files.

Do not refresh references automatically or edit data to make a comparison pass.
For a future baseline change, use a new directory and update the workflow's
explicit reference path after review. The older baseline remains preserved.
