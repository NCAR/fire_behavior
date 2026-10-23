# Instructions for test documentation

Write user-facing test documentation so readers can understand it without
knowing the development history. Describe the scientific behavior, configuration,
or compatibility condition being tested. Do not use internal planning labels
such as P00/P02 or PR numbers as substitutes for those descriptions.

For example, use "restart-loading probes for moisture initialization" instead
of "probes for P02", and "legacy restart metadata" instead of "PR #39 metadata".
Explain which metadata is absent and why it matters when discussing legacy
compatibility; "restart metadata" alone does not identify that distinction.

Keep PR references, planning labels, revisions, and historical validation results
in validation records such as regression/doc/validation.md or session notes.
Preserve that provenance when revising user-facing guides.
