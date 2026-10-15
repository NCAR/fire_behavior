# Deferred work and coverage limits

[Documentation index](../README.md)

These items are outside the current generated matrix. PR acceptance uses the
60 s endpoint; intermediate-time and longer-run agreement are not additional
gates. The retired legacy suite remains in Git history. Preserve diagnostic
artifacts as evidence of coverage limits.

| Item | What remains to be established |
| --- | --- |
| Intermediate-time 3D standalone/coupled agreement | Earlier diagnostics exceeded tolerance between saved PR endpoints. The bilinear-precision experiment passed an every-4-second GitHub comparison through 60 s. These are diagnostic results, not an extension of the PR acceptance schedule. See [validation history](validation.md). |
| Longer standalone/coupled agreement | The 600 s terrain diagnostics still exceed the short-run comparison tolerance, including at 6.096 m with double-precision bilinear arithmetic. A longer-duration acceptance policy would need separate scientific review; it is not required for this PR. See [validation history](validation.md). |
| Humidity interpretation | Scientific review associated with PR #46 is independent of reproducibility; current moisture remains enabled. |
| Restart | Restart continuity, restart inputs, and segmented-run comparisons. |
| PR #39 changes and method `(4,5)` | Separate scientific review and suitable comparison cases before matrix inclusion. |
| ESMX_Data feedback | The generated coupled cases use WRF-data forcing. The retired `testx` exercised a distinct feedback fixture that still needs a generated replacement. |
| Coupled OpenMP-only | MPI and hybrid NUOPC/ESMX are included in the PR matrix. Coupled runs using an OpenMP-only build remain outside this selection. |
| Atmospheric hosts | WRF/UFS host integrations, including UFS mass-centred winds and any future staggered interpolation option. |
| SB40 fuels | Native model support, documented option value, input generation, and physical output checks. A crosswalk table alone is insufficient. |
| Diagnostic output levels | The default inventory has 19 fields; higher-level diagnostics need explicit inventory and behavior checks. |
| Historical acceptance | No reference is approved or activated by a generated cross-execution pass. Reference approval remains a separate recorded decision. |

Historical legacy comparison failures and their timing-related diagnosis are
recorded in [validation.md](validation.md). Retiring that suite does not resolve
those failures or establish coverage of its ESMX_Data feedback configuration.
