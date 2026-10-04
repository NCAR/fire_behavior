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
explicit directory through `-DCFBM_REFERENCE="$TEST/references/APPROVED_ID"`
when configuring a build. Normal comparisons reject unapproved references;
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
