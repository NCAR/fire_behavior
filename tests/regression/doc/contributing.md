# Extending the regression tests

[Documentation index](../README.md)

Start with a scientific question: what behavior would be wrong if the change
failed? Choose a case that makes that error observable, then add a check that
would fail for it. Compiling a new option or writing a new variable does not by
itself test the behavior.

## Add a new case

1. Read [cases.yaml](../cases.yaml) and [case configurations](cases.md). Add a
   descriptive key under `cases`, overriding only settings that differ from
   `defaults`. Resolution order is defaults, case, scale, then named method.
   Execution settings contain rank/thread/timeout choices only.
2. Use supported generator settings where possible. If a new terrain, forcing,
   fuel, or ignition pattern is required, implement it in
   [generate_inputs.py](../generate_inputs.py), update input schema checks, and
   add a small deterministic Python test. State units and staggering explicitly.
3. Add the case to the intended `suites` maps with explicit execution names.
   Include serial and the relevant parallel layouts. Add it to `coupled_cases`
   only when the WRF-data forcing and NUOPC/ESMX paths support it. Coupled entry
   alone does not create a new atmospheric host implementation.
4. Add checks in [check_outputs.py](../check_outputs.py) that establish the new
   physical behavior. Confirm that the existing fire-evolution checks are
   appropriate. For example, a deliberately non-burning case needs explicitly
   designed checks rather than disabling failures after the fact.
5. Add Python tests for new configuration and generation behavior, and focused
   Fortran tests if model logic changed. Document the configuration and add or
   regenerate its figures.
6. Reconfigure each affected CMake build to register the new name. Inspect the
   listing with `ctest --test-dir "$TEST/build/mpi/tests" -N`. Run the smallest
   relevant case first, then the affected `quick` and `pr` selections through PBS.
   Save the source commit, commands, logs, and numerical errors.

Example selection after adding `new_case` and configuring the serial build:

```bash
TEST="/glade/derecho/scratch/$USER/tmp"
python -B tests/regression/regression.py suite --suite pr \
    --case new_case \
    --build-dir="$TEST/build/serial" \
    --run-root="$TEST/results"
```

This command is illustrative until the case is added. No regular expression is
needed. Use the [run guide](running.md) for exact test-name selection.

## Extend an existing case

Keep the case name when its scientific purpose is unchanged. Add a separate case
when both old and new behaviors need ongoing coverage, such as 10 m versus 3D
wind imports. Record which inputs changed and why. A duration change can also
change forcing history: temperature and humidity endpoints are spread across the
configured duration by the generator.

Update checks and figures with the settings. Re-run all affected execution
layouts and driver comparisons. Old outputs do not validate a revised case.
Changing only a case identifier also changes exact CTest names and reference
keys. The rename from `terrain_10m`/`terrain_3d` to `terrain_u10m`/`terrain_u3d`
does not alter physical settings, but old names are no longer accepted. Preserve
historical artifacts with their original names. Do not rename data within an
immutable approved reference to make it match.

## Add a namelist option

Document the option in the model's
[Configuration.rst](../../../doc/Configuration.rst), including meaning, units,
valid values, and driver applicability. Implement and test the model behavior
first. Then:

1. Add a scientific default under the appropriate section in `cases.yaml`.
2. Add type/range/combination checks in `config.py`. The default keys participate
   in typo detection, while some sections also have explicit allowed-key lists.
3. Map the option in `namelist_values()` in `render_namelist.py` and add its
   substitution to `templates/namelist.fire.in`.
4. Set it in a case that exercises a meaningful value. Check the rendered
   namelist and a consequence in the outputs. A setting that never affects a
   tested run is not covered.
5. For MPI, update/test the model's namelist broadcast if the option requires it.
   For coupled runs, verify the component that reads the relevant namelist block.

## Add a validated output

Follow the steps under [the comparison
inventory](comparison.md#when-the-model-gains-an-output). Update the writer's
metadata, YAML inventory, Python metadata and applicability checks, a targeted
failing comparison, and the documentation together. Diagnostic output levels may
add fields, so ensure the inventory matches the selected output level. Preserve
exact category and coordinate checks. A new scientific acceptance threshold
requires rationale and review.

## Add an execution layout or driver

For an existing driver, add an `executions` entry with `variant`, `ranks`,
`threads`, and `timeout_seconds`, then select it in the suite maps. Ensure the
PBS allocation covers ranks × threads and the requested layout is supported by
the model. Do not change domain size or physics to make a parallel comparison
pass. CMake supplies MPI launcher arguments as a list.

A genuinely new driver also needs CLI/CMake executable selection, staging,
completion checks, and driver-appropriate forcing. Update `run_case.py`,
`run_suite.py`, and the documentation as needed. The present coupled cases use
WRF-data forcing. UFS mass-centred winds require a separate host-specific test.

## Style and Python checks

Use Google-based YAPF, four spaces, an 80-column target, descriptive snake_case,
`pathlib.Path`, and public-function type hints. Separate logical operations with
blank lines. Explain physical assumptions and synthetic substitutions in
comments. YAPF controls formatting, not scientific clarity or the full Google
style guide. Preserve existing creation dates and the repository's CFBM header:

```python
# Created on 2026-09-12 by the CFBM development team assisted by GPT-6-Astra.
```

New scripts use their actual creation date. Institutional copyright remains at
repository level. This overrides personal headers in local script-style advice.

Install YAPF only in a writable development environment. CI uses the pinned
version in `requirements-style.txt`, independently of model execution:

```bash
python -m pip install -r tests/regression/requirements-style.txt
python -m yapf --diff --recursive \
    --style tests/regression/.style.yapf tests/regression

TEST="/glade/derecho/scratch/$USER/tmp"
export CFBM_TEST_TMP="$TEST/python-test-artifacts"
python -B -m unittest discover \
    -s tests/regression/tests -p '*_test.py' -v
```

Replace `--diff` with `--in-place` to apply formatting. The two Python test
files exercise the infrastructure using small synthetic data, without running
the model. Both are collected by the `_test.py` pattern. CTest supplies
`CFBM_TEST_TMP` automatically. Use PBS for the shared NPL environment because
its netCDF4 library can initialize MPI. A serial-NetCDF development environment
can run these small Python tests on a login node.

## Regenerate case figures

[plot_cases.py](plot_cases.py) reads the same YAML and generator functions as
the tests. It creates configuration figures, not simulated fire-spread results.
Matplotlib is needed only to regenerate the committed figures, not to run tests.
Use a development environment with Matplotlib and the runtime packages, or NPL
inside PBS. From the repository root:

```bash
TEST="/glade/derecho/scratch/$USER/tmp"
export MPLCONFIGDIR="$TEST/matplotlib"
python -B tests/regression/doc/plot_cases.py \
    --output-dir=tests/regression/doc/figures
```

Review each PNG for labels, units, and consistency with `cases.yaml`. Update
captions and commit the script and figures together. The structure diagrams are
Mermaid blocks in [structure.md](structure.md), rendered by GitHub.

## Prepare review evidence

Use the [PR checklist](pr-checklist.md). Report incomplete or failing selections
and distinguish same-driver layout comparisons, cross-driver comparisons, and
historical reference comparisons. Preserve failed runs. A new candidate may be
created only from a passing suite at a clean current revision, with a new
identifier. Team approval and activation remain separate steps.
