# Approved regression baselines

The GitHub workflow uses the approved PR-suite reference in [github-pr/](github-pr/README.md).
Maria Frediani approved the original candidate from GitHub run `38612379390`
on 2026-10-14 and requested regeneration with strict GNU flags. Run
`38644049156` regenerated the identical numerical payload; the source and
environment records now describe that run. Normal PR checks compare against
this committed reference; they never generate or refresh it.

Use the manual workflow's `create_reference` input to build all four GNU Strict
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
