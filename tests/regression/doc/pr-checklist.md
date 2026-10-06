# Checklist for a model or regression PR

[Documentation index](../README.md)

Use this as a review checklist. Mark an item not applicable with a short reason
when the change does not affect it. This file is guidance, not an automatic PR
template or a claim that any existing PR has met every item.

## Scientific behavior and documentation

- [ ] State the problem, intended behavior, and affected standalone/coupled paths.
- [ ] Document new or changed model options, units, defaults, supported values,
      and limitations in the model documentation.
- [ ] Update regression case descriptions, figures, run instructions, or structure
      diagrams wherever the implementation changes them.
- [ ] Explain numerical changes, known limitations, and separately deferred work.

## Regression coverage

- [ ] Represent new namelist options in YAML, validation, rendering, and at least
      one case that exercises their effect.
- [ ] Add new outputs to the field inventory, metadata checks, comparison table,
      and applicable scientific behavior checks.
- [ ] State each field's bitwise/tolerance policy with physical rationale.
- [ ] Add unit tests that would fail for the reported defect or missing behavior.
- [ ] Exercise affected serial, MPI, OpenMP, hybrid, NUOPC, and ESMX configurations,
      or explain which are inapplicable or still untested.
- [ ] Preserve legacy tests and references, including coverage not yet reproduced
      by generated cases.

## Validation evidence

- [ ] Run Python tests, formatting, and affected build/CTest checks.
- [ ] Record exact source revision, local edits if present, environment, commands,
      PBS job IDs, and artifact locations.
- [ ] Report run counts and comparison counts separately, with maximum numerical
      errors, failures, and missing coverage.
- [ ] Distinguish completed integrations from passing comparisons and passing PBS
      status from individual suite status.
- [ ] Preserve failed artifacts and explain existing failures without changing
      tolerances or references to obtain a pass.
- [ ] If references change, create a new immutable candidate and record separate
      team approval before activation. Do not overwrite historical references.

## Review readiness

- [ ] Check the PR base and dependencies so the diff contains the intended work.
- [ ] Summarize the final implementation and validation for someone unfamiliar
      with the development conversation.
- [ ] Include unresolved scientific questions and the tests still required.
