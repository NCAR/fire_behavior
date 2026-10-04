# Deferred work and coverage limits

[Documentation index](../README.md)

These items are outside the current generated matrix. A passing 60 s suite does
not establish their behavior. Keep the relevant legacy tests and failure
artifacts until replacement coverage is validated.

| Item | What remains to be established |
| --- | --- |
| Short-run 3D standalone/coupled agreement | At 6.096 m, four comparisons fail at 60 s in six fields across two cells; maximum relative error 0.01434025% exceeds 0.01%. The 20 m control passes. Isolate the physical/numerical source without relaxing acceptance. See [validation history](validation.md). |
| Longer standalone/coupled agreement | A previous 600 s terrain experiment exceeded the unchanged tolerance from 180 s. Its 3D target was 20 m. Reassess longer runs at 6.096 m after the short matrix; see [validation history](validation.md). |
| Humidity interpretation | Scientific review associated with PR #46 is independent of reproducibility; current moisture remains enabled. |
| Restart | Restart continuity, restart inputs, and segmented-run comparisons. |
| PR #39 changes and method `(4,5)` | Separate scientific review and suitable comparison cases before matrix inclusion. |
| ESMX_Data feedback | The generated coupled cases use WRF-data forcing. Legacy `testx` still exercises a distinct feedback fixture. |
| Coupled OpenMP/hybrid | The four-build validation covers standalone OpenMP/hybrid and MPI NUOPC/ESMX. It does not establish coupled OpenMP/hybrid behavior. |
| Atmospheric hosts | WRF/UFS host integrations, including UFS mass-centred winds and any future staggered interpolation option. |
| SB40 fuels | Native model support, documented option value, input generation, and physical output checks. A crosswalk table alone is insufficient. |
| Diagnostic output levels | The default inventory has 19 fields; higher-level diagnostics need explicit inventory and behavior checks. |
| Historical acceptance | No reference is approved or activated by a generated cross-execution pass. Reference approval remains a separate recorded decision. |

The original legacy comparisons remain unchanged. Their current failures and
timing-related diagnosis are recorded in [validation.md](validation.md); this
refactor does not update references or relax acceptance rules to make them pass.
