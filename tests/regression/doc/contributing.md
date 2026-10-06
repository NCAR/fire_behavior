# Extending the regression tests

[Documentation index](../README.md)

Start with a scientific question: what behavior would be wrong if the change
failed? Choose a case that makes that error observable, then add a check that
would fail for it. Compiling a new option or writing a new variable does not by
itself test the behavior.

## Add a new case

Choose a short case name that describes a different physical setup. Define its
`description`, supported `drivers`, shared `inputs` and `namelist` overrides,
and at least one named `configuration` in `cases.yaml`. Use `base: {}` when no
configuration-specific overrides are needed. See the complete
[configuration example](configuration.md).

1. Generate physically meaningful terrain, fuel, forcing, and ignition inputs.
   Extend `generate_inputs.py` only if the new geometry or forcing needs it.
2. Validate the new input settings and supported combinations in `config.py`.
3. Add checks in `check_outputs.py` showing that the intended model path ran.
4. Select configurations and executions explicitly in `quick`, `pr`, or `full`.
   Listing a configuration alone does not add it to a suite.
5. Add an independent Python unit test, document the purpose and acceptance,
   regenerate relevant figures, and run the affected model layouts/drivers.

## Extend an existing case

Use another configuration when geometry and purpose remain shared but a
namelist choice needs separate coverage. For example, terrain's `u3d` sets
`namelist.fire.wind_vinterp_opt: 0`; `godunov3d` additionally sets
`fire_upwinding: 2`. A configuration can also override inputs if necessary.
There are no automatically multiplied option categories or generated names.

Give each configuration a meaningful name and select it explicitly in the
suite tables. New fuel families or host-specific wind representations belong
here once the model and generator support them. Do not infer an SB40 `fuel_opt`
value from the existing crosswalk table. Current native coverage is Anderson.

Extend a shared case setting only when it should change every configuration
that inherits it. Changing duration also changes the generated temperature and
humidity evolution because their endpoints span that duration. Revalidate all
affected runs and update the figures. Existing references remain immutable;
renaming cases, configurations, or scales changes their keys and does not
validate or migrate an old reference automatically.

## Add a namelist option

Document meaning, units, valid values, and driver applicability in the model's
[Configuration.rst](../../../doc/Configuration.rst). Implement and unit-test
the behavior first, then:

1. Add the exact block/option name under `defaults.namelist` in `cases.yaml`.
2. Add its type to `NAMELIST_TYPES` and any scientific range or combination
   checks in `config.py`.
3. Set a meaningful alternative in a named configuration. The renderer writes
   this exact name directly; no alias or template substitution is needed.
4. Check the rendered namelist and an observable consequence in model output.
5. Verify MPI broadcast and which coupled component reads the relevant block.

Dates, `atm.kde`, ignition coordinates, and the `ideal` grid block are derived
from the clock and generated geometry. Extend these derivations only when the
input format requires it, avoiding two independently configurable sources for
the same dimension or coordinate.

## Add a validated output

Follow the steps under [the comparison
inventory](comparison.md#when-the-model-gains-an-output). Update the writer's
metadata, YAML inventory, Python metadata and applicability checks, a targeted
failing comparison, and the documentation together. Diagnostic output levels may
add fields, so ensure the inventory matches the selected output level. Preserve
exact category and coordinate checks. A new scientific acceptance threshold
requires rationale and review.

## Add an execution layout or driver

For an existing driver, add an `executions` entry with `build`, `ranks`,
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
style guide. There is no required creation or assistance header.

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
