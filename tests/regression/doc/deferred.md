# Deferred work and coverage limits

[Documentation index](../README.md)

These items are outside the current generated matrix. PR acceptance uses the
60 s endpoint; intermediate-time and longer-run agreement are not additional
gates. Keep the relevant legacy tests and diagnostic artifacts as evidence of
coverage limits.

| Item | What remains to be established |
| --- | --- |
| Intermediate-time 3D standalone/coupled agreement | Earlier diagnostics exceeded tolerance between saved PR endpoints. The bilinear-precision experiment passed an every-4-second GitHub comparison through 60 s. These are diagnostic results, not an extension of the PR acceptance schedule. See [validation history](validation.md). |
| Longer standalone/coupled agreement | The 600 s terrain diagnostics still exceed the short-run comparison tolerance, including at 6.096 m with double-precision bilinear arithmetic. A longer-duration acceptance policy would need separate scientific review; it is not required for this PR. See [validation history](validation.md). |
| Humidity interpretation | Scientific review associated with PR #46 is independent of reproducibility; current moisture remains enabled. |
| Restart | Standalone same-revision chains are covered by the [restart suite](restart.md). Coupled/WRF-host continuity and cross-revision compatibility remain untested. Known production failures remain failures. |
| PR #39 changes and method `(4,5)` | Separate scientific review and suitable comparison cases before matrix inclusion. |
| ESMX_Data feedback | The generated coupled cases use WRF-data forcing. Legacy `testx` still exercises a distinct feedback fixture. |
| Coupled OpenMP-only | MPI and hybrid NUOPC/ESMX are included in the PR matrix. Coupled runs using an OpenMP-only build remain outside this selection. |
| Atmospheric hosts | WRF/UFS host integrations, including UFS mass-centred winds and any future staggered interpolation option. |
| SB40 fuels | Native model support, documented option value, input generation, and physical output checks. A crosswalk table alone is insufficient. |
| Diagnostic output levels | The default inventory has 19 fields; higher-level diagnostics need explicit inventory and behavior checks. |
| Historical acceptance | No reference is approved or activated by a generated cross-execution pass. Reference approval remains a separate recorded decision. |

The original legacy comparisons remain unchanged. Their current failures and
timing-related diagnosis are recorded in [validation.md](validation.md); this
refactor does not update references or relax acceptance rules to make them pass.
