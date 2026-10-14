# Results and historical references

[Documentation index](../README.md)

## Locate the results

Every execution gets a new directory with the resolved namelist, generated
inputs, model log, outputs, and `result.json`. The resolved scientific settings,
source revision, and local edits are recorded in that result. A suite writes one
`summary.json` with case locations and cross-execution comparisons. CTest
provides JUnit output in CI. Failed runs retain all files.

From the repository root, define `TEST="/glade/derecho/scratch/$USER/tmp"`.

Normal tests allow local edits. Reference creation requires a passing suite from
the current clean source revision:

```bash
python -B tests/regression/regression.py reference-create \
    --suite-root="$TEST/results/pr-UNIQUE" \
    --destination="$TEST/references/candidate-NEW"
```

This creates an **unapproved** reference and never activates it. Supply an
explicit directory through `./compile.sh --reference="$TEST/references/APPROVED_ID"`
(or `-DCFBM_REFERENCE` with direct CMake). Normal comparisons reject unapproved references;
explicit directory selection does not replace recorded team approval. Without
that option, tests check physical evolution and agreement among executions; they
do not claim agreement with an approved historical baseline. The report records
the supplied reference's approval status.

SHA256 checksums are file fingerprints used to detect changed reference
payloads. They establish data integrity, not scientific correctness. Numerical
acceptance is described in the [comparison table](comparison.md). There are no
automatic fallback reference directories. A case-name or scientific-setting
change requires a new reviewed reference mapping, not edits to an existing
immutable reference.

For the limits of current 60 s and 600 s evidence, see [validation
history](validation.md).

## GitHub review and activation

CI uses the fixed `pr` suite with all four Release builds: serial, OpenMP,
MPI including NUOPC/ESMX, and hybrid. Its 32 generated runs produce 32
cross-execution comparisons. Unit checks run in each build. PRs targeting
prerequisite branches are included, so a merge into `develop` is not needed.

Normal checks require the explicitly selected committed reference. Until the
first candidate is approved, the missing reference deliberately prevents a green
normal PR check. To generate the candidate, select **Run workflow** for
**Fire Behavior CI Test**, choose the PR branch, and enable `create_reference`.
That run checks the same complete PR selection without a reference, then
uploads an unapproved baseline only if all checks and comparisons pass.

Download and extract both `.tar.gz` artifacts (candidate and test evidence).
Archiving preserves the colons in NetCDF timestamp filenames, which GitHub's
artifact uploader rejects as individual files. Review the source and
environment, all case mappings, NetCDF fields, scientific behavior, and comparison
reports. Check the corresponding HPC results before deciding whether a separate
HPC reference is necessary. A GitHub reference is not automatically portable
across compilers and libraries.

After explicit team approval, commit the candidate under the workflow's selected
baseline directory, record the approval, and rerun the normal workflow on the
final PR revision. The candidate-generation run alone is not reference acceptance.
See [baseline storage](../baselines/README.md). The larger `full` suite remains available through the local CLI and PBS template, with references covering
those selections when historical comparisons are required.
