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

This creates an **unapproved** reference and never activates it. Build with
`--no-reference` when generating a new candidate so the tests do not depend on
an older baseline. The workflow's candidate-generation mode does this explicitly.

`Strict` selects the bundled `github-pr` reference by default for all
parallel configurations. Override it with
`./compile.sh --reference="$TEST/references/APPROVED_ID"` or disable it with
`--no-reference`. Direct CMake accepts `-DCFBM_REFERENCE=AUTO`, a directory,
or an empty string, respectively. The selected reference path is printed;
when none is selected, configuration warns that historical baseline comparisons
are disabled. Such runs check physical evolution and agreement among executions,
without claiming historical regression acceptance. Normal comparisons reject
unapproved references, and reports record the supplied reference's approval status.

SHA256 checksums are file fingerprints used to detect changed reference
payloads. They establish data integrity, not scientific correctness. Numerical
acceptance is described in the [comparison table](comparison.md). There are no
automatic fallback reference directories. A case-name or scientific-setting
change requires a newly generated and reviewed candidate. Replace the current
`github-pr` only after approval; Git history preserves earlier versions.

For the limits of current 60 s and 600 s evidence, see [validation
history](validation.md).

## GitHub review and activation

CI runs `pr` and `restart` using all four GNU Strict builds: serial, OpenMP,
MPI and hybrid, with NUOPC/ESMX in the latter two. The `pr` suite's 36 generated
runs produce 40 cross-execution comparisons. The `restart` suite compares
continuous and segmented standalone integrations from the same revision and
does not use a stored baseline. Both suites must pass. Unit checks run in each
build. PRs targeting prerequisite branches are included, so a merge into
`develop` is not needed.

Normal Strict checks default to the [approved GitHub reference](../baselines/github-pr/README.md)
committed for the complete PR suite. A missing or unapproved reference fails
the check. To generate a future candidate, select **Run workflow** for
**Fire Behavior CI Test**, choose the PR branch, and enable `create_reference`.
That run checks both suites without a stored reference, then creates a candidate
from only the `pr` results. It uploads the unapproved baseline only if all
checks and comparisons pass. Adding restart coverage does not change the
approved baseline or include restart checkpoints in a reference candidate.

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
