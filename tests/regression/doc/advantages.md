# What the new regression tests add

[Documentation index](../README.md)

The generated tests add spatial comparisons, explicit parallel coverage, and
reproducible forcing while preserving the original cases under
[tests/legacy](../../legacy/README.md). They serve different purposes, so the
original references remain available for historical diagnosis.

## Comparison of capabilities

| Area | Original legacy tests | Generated regression tests |
| --- | --- | --- |
| Quantities compared | Five printed diagnostics and their timestamps: fire area, heat output, latent heat output, maximum heat flux, maximum latent heat flux | Every field in the expected NetCDF inventory, at every scheduled output time |
| Spatial information | Domain summaries, including maxima, can hide cell-by-cell differences | Per-field cell counts, first differing index and values, maximum absolute/percentage errors, RMS error, and bitwise differences |
| Numerical acceptance | `grep`/`awk` extraction followed by a required count of `diff` lines, sometimes exactly nonzero | Explicit bitwise or relative-tolerance rules documented for each field |
| Standard duration | 10 s | 60 s, six times longer, with 4 s timesteps |
| Temporal sampling | Text every 0.5 s and saved NetCDF every 1 s, but NetCDF fields were not compared | Standard NetCDF checks at 0 and 60 s. Longer duration does not mean more frequent sampling |
| Wind checks | Wind diagnostics were printed but were not among the five comparisons | Both 10 m and 3D terrain cases, component bounds/direction, spatial variation, and field comparisons |
| Forcing | Archived WRF/geogrid fixtures | Deterministic generated terrain, fuels, roughness, time-varying temperature/humidity, and terrain-dependent winds |
| Domain coverage | Saved legacy coupled fields include small-wind boundary regions | Atmospheric domain extends beyond the fire grid, and final real-case fields must contain no missing cells |
| Parallel coverage | CI compiled MPI on and OpenMP off. Legacy model executables ran directly as one process. A separate MPI unit test used multiple ranks | Four explicit builds, with one/four-thread and one/four-rank checks and a four-rank × four-thread standalone hybrid check |
| Coupling coverage | NUOPC/ESMX fixtures and ESMX_Data `testx` | Both terrain wind representations through NUOPC and ESMX, with one/four MPI ranks and comparison to standalone |
| Completion | Text comparisons | Exit status, fatal diagnostics, required coupled completion marker, exact file/field inventory, and physical evolution checks |
| Isolation | Historical scripts used shared working paths and cleanup commands | New directories for each attempt, preserved inputs/outputs/logs, safe reruns |
| Selection | Individual shell scripts | Plain case, driver, execution, and exact CTest names, plus `quick`, `pr`, `full`, `unit`, and `legacy` suites |
| Reports | Console text | `result.json`, suite `summary.json`, and CI CTest JUnit reports with artifact locations |
| Historical references | Shared text solutions | Explicit immutable reference directories, recorded approval, and file-integrity checks |
| Maintainability | Repeated case scripts and implicit settings | Separate Python responsibilities, one scientific YAML file, one namelist template, and focused Python tests |

The legacy duration and text rules are visible in
[test7.s](../../legacy/test7.s), [test8.s](../../legacy/test8.s), and their
[namelists](../../legacy/test8/namelist.fire). The new settings and field
inventory are in [cases.yaml](../cases.yaml). The [comparison
table](comparison.md) documents the implemented acceptance rules.

## Evidence that printed agreement is insufficient

The 2026-10-04 audit found identical printed fire diagnostics among current
standalone, NUOPC, and ESMX runs within each test7/test8 family. Their spatial
fields were not identical. At 10 s, test7 mean wind speeds were approximately
3.46 m/s in standalone and 2.79 m/s in coupled output. Of 6400 coupled cells,
1216 had speeds near 0.001414 m/s. Excluding six cells on each boundary reduced
the mean discrepancy substantially, while small interior differences remained.
The wind field was not part of the original acceptance checks.

The same audit found standalone/coupled maximum sensible-heat-flux differences
across saved times of 8.09375 W/m² in test7 and 67.09375 W/m² in test8 despite
identical printed fire diagnostics. Rounded aggregate values therefore did not
establish field equality.

Aligning current diagnostics by 0.5 s with the old references gave 26/35 passing
original comparisons over 19 overlapping records. Test8 still passed only two of
its five criteria. This reconstruction omitted unmatched endpoints and is not a
passing result for the unchanged full legacy tests. The integration interval
changed, so shifting timestamps alone does not align all model state.

Evidence: the audit recorded in
`cfbm-pr49-final-20261003/compacted-context-202610041111.md` in the validation
submitter's scratch archive, using outputs from Derecho job `7702254.desched1`.
The archive name records provenance. Contributors use their own scratch root and
do not need this archive to run the tests.

## Coverage that is still needed

The generated cases do not replace `testx` feedback, restart tests, WRF/UFS host
integration, or scientific evaluation of the humidity formulation. Coupled
OpenMP/hybrid coverage has not been established by the standard four-build
validation. The 60 s tests do not establish long-duration agreement: a separate
600 s experiment exceeded the unchanged standalone/coupled tolerance from 180 s
onward. See [validation history](validation.md).

Without an explicitly approved reference, these tests evaluate physical behavior
and agreement among selected executions. Two drivers can agree while sharing a
defect. Historical reference comparisons and independent physical checks remain
complementary requirements.
