# Manual active-front numerical regression

This test verifies that `use_active_front` changes diagnostic support only. It
is intentionally not registered with CTest. Model outputs are staged under
`/glade/derecho/scratch/frediani` and are not written into the tracked source
tree.

## Reference definitions

The paired matrix uses one final executable and inputs that differ only in
`use_active_front`:

| Case | Propagation | Reinitialization | Additional controls |
|---|---:|---:|---|
| `p2_r4` | 2 | 4 | defaults |
| `p4_r4` | 4 | 4 | defaults |
| `p4_r5_rs_off` | 4 | 5 | Russo-Smereka off |
| `p4_r5_rs_on` | 4 | 5 | Russo-Smereka and sign-aware Godunov branch on |
| `p8_r4` | 8 | 4 | defaults |
| `p9_r4` | 9 | 4 | defaults |
| `p10_r4` | 10 | 4 | defaults |

The pre-refactor reference is commit `ca9c810`, the implementation immediately
before the active-front diagnostic reconstruction. Its option-8, option-9, and
option-10 false-mode outputs must be bitwise identical to the corresponding
final false-mode numerical fields.

The Godunov-Russo-Smereka reference is exact commit `27cd977`, configured with
the dropped adaptive and ROS-smoothing features disabled. Reference directories
must use the same six-second input and the layout
`CASE_ID/baseline_false/fire_output_*.nc`. The comparator checks the two
reinitialization-option-5 cases against those externally generated outputs.

## Bitwise criteria

After the first physical advance, the false/true pairs require bitwise identity
for every defined NetCDF variable except:

- `active_front_mask`
- `barrier_contact_front_mask`
- `band_mask`
- `ros_lfn_error_front`

The standalone test7 driver reads atmospheric roughness for the fire-wind
calculation but never assigns it to `state_fire_t%fz0`, which is nevertheless
written. The coupled NUOPC driver assigns `fz0` from its imported roughness
field. This manual regression therefore excludes `fz0` at every record rather
than comparing undefined storage.

The required numerical fields are `lfn`, `ros`, `fuel_frac`, `fire_area`, and
`fuel_frac_burnt_dt`. This driver does not write `tign_g`; `fgrnhfx` and
`fgrnqfx` are compared as available downstream ignition-time-sensitive fields.
`fire_area_change_rate` is also required to be bitwise identical between modes.

At the initial pre-advance output, the base driver does not define `fgrnhfx`,
`fgrnqfx`, `fuel_frac_burnt_dt`, `ros`, `ros_front`, `grad_norm_ls`, or
`grad_norm_reinit`. The comparator excludes only those fields at that initial
record, in addition to the test-wide `fz0` exclusion. It still checks all
initialized state, including `lfn`, `fuel_frac`,
`fire_area`, `fire_area_change_rate`, and the new masks, and verifies every
field after the first physical timestep.

On failure, `compare_matrix.py` reports the first output file, field, array
index, values, and stored bytes that differ.

The comparator also verifies:

- cellwise `fire_area_change_rate = (fire_area_new - fire_area_previous) / dt`;
- zero initial rate and masks;
- strict nonzero false-mode seeds and exact width-1 and width-4 bounded
  four-neighbor expansion;
- zero exact masks in false mode;
- true-mode exterior-connected masks, barrier classification, two-sided band
  geometry, and `ros_lfn_error_front` formula and support;
- exact connectivity-call paths 0, 1, and 2;
- omission of the complete specialized-output set for independent
  `devel_opt=0` and `output_level=0` cases;
- serial/MPI agreement for every defined output field except `fz0` when MPI
  cases are supplied.

Every model case must produce the exact seven timestamps through 18:00:06,
contain six call-count reports where applicable, and have no `STOP:` message.

## Serial matrix

Build the serial executable and choose a new timestamped scratch root. Model
execution must occur inside PBS, including serial reference cases:

```bash
./compile.sh --mpi-off \
  --build-dir=/glade/derecho/scratch/frediani/active-front-regression/build-serial \
  --prefix=/glade/derecho/scratch/frediani/active-front-regression/install-serial

RUN_ROOT=/glade/derecho/scratch/frediani/active-front-regression/run-YYYYMMDDHHMMSS
qcmd -A NSAP0003 --nchunks 1 --ntasks 1 --mem 20GB -l walltime=00:20:00 -- \
  /glade/work/frediani/casper/anaconda3/envs/py314/bin/python \
  tests/active_front_diagnostic/run_matrix.py \
  --mode serial \
  --executable /glade/derecho/scratch/frediani/active-front-regression/install-serial/bin/fire_behavior.exe \
  --run-root "$RUN_ROOT"
```

Each case directory is immutable. The runner stops rather than overwrite an
existing case, so use a new timestamped root for a repeated experiment.

## MPI validation through PBS

Build the MPI executable, then run selected cases inside a Casper PBS
allocation. The runner rejects every model mode outside PBS and rejects run
roots outside `/glade/derecho/scratch/frediani`.

```bash
./compile.sh \
  --build-dir=/glade/derecho/scratch/frediani/active-front-regression/build-mpi \
  --prefix=/glade/derecho/scratch/frediani/active-front-regression/install-mpi

RUN_ROOT=/glade/derecho/scratch/frediani/active-front-regression/run-YYYYMMDDHHMMSS
qcmd -A NSAP0003 --nchunks 1 --ntasks 4 --mem 20GB -l walltime=00:20:00 -- \
  /glade/work/frediani/casper/anaconda3/envs/py314/bin/python \
  tests/active_front_diagnostic/run_matrix.py \
  --mode mpi \
  --mpi-ranks 4 \
  --case p4_r5_rs_on \
  --executable /glade/derecho/scratch/frediani/active-front-regression/install-mpi/bin/fire_behavior.exe \
  --run-root "$RUN_ROOT"
```

## Pre-refactor and external references

Run the retained `ca9c810` executable for its three supported cases:

```bash
RUN_ROOT=/glade/derecho/scratch/frediani/active-front-regression/run-YYYYMMDDHHMMSS
qcmd -A NSAP0003 --nchunks 1 --ntasks 1 --mem 20GB -l walltime=00:20:00 -- \
  /glade/work/frediani/casper/anaconda3/envs/py314/bin/python \
  tests/active_front_diagnostic/run_matrix.py \
  --mode pre-refactor \
  --executable /path/to/fire_behavior-ca9c810.exe \
  --run-root "$RUN_ROOT"
```

Preserve that executable as
`$RUN_ROOT/pre_refactor/reference_executable/fire_behavior.exe` and add
`$RUN_ROOT/pre_refactor/pre_refactor_receipt.txt` with concrete values:

```text
source_revision=ca9c8109b2f4e80e40d96fc8492c1b0a2475d50a
pbs_job_ids=JOBID
executable_sha256=SHA256
```

The comparator recomputes the preserved executable hash, matches it to every
pre-refactor case, checks the PBS job receipts recorded by the runner, and
rejects an executable identical to any corresponding maintained final
executable.

Generate the `27cd977` outputs in a separate checkout using the reference
definition above and a PBS allocation. Verify the checkout with
`git rev-parse HEAD`, build its executable in scratch, and use its historical
input surface to disable every dropped feature. Historical option names are
deliberately not copied into this maintained branch. Keep the generated
outputs outside this repository in the documented
`CASE_ID/baseline_false/` layout.

The external reference root must contain `reference_manifest.txt`:

```text
source_revision=27cd977998ee23f14aa0b56c1cdca401ddca63ee
dropped_features_disabled=true
pbs_job_ids=JOBID_OFF,JOBID_ON
executable_sha256=SHA256
geo_em.d01.nc_sha256=SHA256
wrf.nc_sha256=SHA256
namelist_sha256_p4_r5_rs_off=SHA256
namelist_sha256_p4_r5_rs_on=SHA256
```

Preserve the exact historical namelists, executable, and PBS logs beside those
external outputs. Add `pbs_job_id.txt` containing the corresponding job ID to
each `baseline_false` directory. The comparator recomputes every artifact hash,
cross-checks both inputs against the maintained runs, rejects `STOP:` in the
historical logs, and rejects a missing or mismatched manifest. The maintained
runner also rejects a historical executable identical to any corresponding
maintained final executable, requires a clean worktree,
and records its revision, historical source revision, executable, inputs,
namelist, PBS job ID, execution mode, and MPI rank count for every case.

## Comparison

```bash
/glade/work/frediani/casper/anaconda3/envs/py314/bin/python \
  tests/active_front_diagnostic/compare_matrix.py \
  --run-root "$RUN_ROOT" \
  --check-mpi \
  --pre-refactor-root "$RUN_ROOT/pre_refactor" \
  --godunov-reference-root /path/to/27cd977-reference \
  --require-complete
```

For partial development checks, omit `--require-complete` and optional
reference arguments, or use repeated `--case CASE_ID` arguments. Every omitted
reference or special check is reported as `SKIP`; `--check-mpi` always fails if
it finds no complete false/true MPI pair.
