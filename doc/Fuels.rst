Fuel models and category mapping
===============================

``fuel_opt = 1`` selects Anderson 13 compatibility behavior. ``fuel_opt = 2``
selects native Scott and Burgan 40 (SB40), including dead/live Rothermel
calculations and dynamic herbaceous curing. Both choices use the same external
category convention for ideal input, geogrid input, WRF coupling, and restart.

Runtime parameter tables
------------------------

The distributed ``tables/ANDERSON13.TBL`` and ``tables/SCOTT_BURGAN40.TBL``
contain fuel properties and moisture-class parameters. These are ordinary
text tables with section markers and column headers, not Fortran namelists.
``fuel_opt`` selects the family and therefore the filename. The supplied data
preserve the preceding compiled parameters, including Anderson's independently
rounded ``fgi`` and depth values. Recomputing that inventory from its component
loads would change the legacy calculation. Anderson component loads follow
Albini (1976), as reprinted in Anderson (1982), Table 1; other Anderson values
retain CFBM/WRF-Fire conventions. SB40 component properties follow Scott and
Burgan (2005), Table 7, while consumption weights retain the existing model
convention.

To use an experimental table, copy the selected file into the run directory
and edit its numeric values, or set ``CFBM_FUEL_TABLE_DIR`` to a directory
containing the selected filename. No rebuild is needed. File lookup is:

1. The directory specified by ``CFBM_FUEL_TABLE_DIR``, if set and nonempty.
2. The run directory.
3. The table directory in the configured build tree.
4. ``share/fire_behavior`` beneath the configured installation prefix.

An explicitly selected directory or an existing file with invalid contents
causes a fatal diagnostic; it does not fall back to another parameter set.
For relocated installations, set ``CFBM_FUEL_TABLE_DIR`` or stage the files
with the run inputs. CMake copies both files into the build and installs them
with the executable. In MPI runs, rank zero of the fire communicator reads
the file and broadcasts its contents before physics initialization. The
timestepping routines access arrays in memory and perform no table I/O.

The version-1 format contains these sections, in order:

* ``CFBM_FUEL_TABLE 1`` and ``FAMILY`` identify the schema and fuel family.
* ``FUEL_MODELS`` declares 14 Anderson or 41 SB40 property records. Each starts
  with an external category, contains the numeric columns named in its header,
  and ends with a double-quoted descriptive name. Records may be reordered;
  the reader assigns each category to its canonical internal row.
* ``NONBURNABLE_ALIASES`` lists the six accepted external no-fuel codes and
  their names. It documents and checks the supported classification. It does
  not add physics rows or permit arbitrary new aliases.
* ``MOISTURE_CLASSES`` contains five Anderson or six SB40 rows: class number,
  drying lag [h], wetting lag [h], saturation moisture [kg kg-1], saturation
  rain intensity [mm h-1], rain threshold [mm h-1], drying-model selector,
  wetting-model selector, initialization policy, and a quoted class name.
* ``MOISTURE_ADJUSTMENT_DECAY_H`` specifies the decay time for assimilated
  equilibrium-moisture adjustments. SB40 additionally specifies
  ``CURING_FULL_MOISTURE`` and ``CURING_NONE_MOISTURE`` in kg kg-1.
* ``END`` terminates the file; additional data records are rejected.

Blank lines and full-line comments beginning with ``#`` or ``!`` are ignored.
Numeric fields use whitespace separators. Missing values, nonfinite numbers,
duplicate or unknown codes, incompatible headers or units, invalid selectors,
and nonpositive lags are rejected during initialization. Column order and
header text are fixed by the schema. Changing units requires converting the
data to the stated units, not relabeling a column. Names are limited to 80
characters; records to 4096 characters.

Fuel columns identify their units explicitly. Component loads use US short
tons per acre, particle density uses lb ft-3, and SAV uses ft-1. Anderson
inventory and canopy dry load use kg m-2 and depth uses m. SB40 depth uses ft
and heat content uses BTU lb-1; the reader retains the existing conversion
order. Mineral contents and moisture are mass fractions. ``weight`` is the
existing empirical consumption-time parameter; canopy burnout time is in s.
Anderson ``ichap`` selects its chaparral treatment and ``windrf`` retains its
legacy wind-reduction parameter. SB40's 1000-hour component remains structurally
absent, and its live extinction is calculated from current fuelbed state.

Only drying/wetting selector 1 is currently implemented. Initialization policy
1 uses ``fuelmc_g``; 2 uses atmospheric equilibrium; 3 uses the corresponding
class seed from ``fuelmc_g``, ``fuelmc_g_live``, or ``fuelmc_g_woody``. Policy 0
is rejected in these tables because a cold start has no restored class state.
Restart loading restores prognostic moisture without applying these cold-start
policies. Run-specific initial/fixed moisture values remain model configuration;
the tables contain the fuel and moisture-response parameters.

The supplied moisture rows reproduce the existing WRF-Fire parameters: dead
drying lags 1, 10, 100, and 1000 h; wetting lags 1.4 times those values; live
lags 1e9 h; saturation moisture 2.5; saturation rain 8 mm h-1; and rain threshold
0.05 mm h-1. These are inherited model parameters, not additional measurements
from the Scott and Burgan fuel catalog. The equations and universal unit
conversions remain in source code.

Restart files record ``fuel_table_schema`` and ``fuel_table_crc64``. The table
path and CRC64 are also written to the initialization log. History files retain
their established metadata format. The CRC64-ECMA identifier is calculated over
non-comment records with surrounding spaces removed and LF separators. A restart
requires a matching table identity.
Changes to record order, labels, internal whitespace, or numeric spelling also
change this conservative identifier; blank lines and full-line comments do not.
Older restart files without the identifier are accepted only with the unchanged
distributed table. The original table identities are retained in the reader
for this compatibility check. A customized table therefore requires either a
matching checkpoint or a new integration.

External categories and internal indices
---------------------------------------

``nfuel_cat`` retains the external map category. It is written unchanged to
history and restart files. ``fuel_index`` is an integer array of validated rows
in the selected fuel table; physics routines use this array for table access.
This separation prevents an external value such as 204 from becoming an array
subscript in a 41-row table, while preserving interpretable classifications.

Native SB40 resolves categories as follows:

.. list-table:: External codes and internal rows
   :header-rows: 1

   * - External category
     - Fuel group
     - Internal row
   * - 101 through 109
     - GR1 through GR9
     - 1 through 9
   * - 121 through 124
     - GS1 through GS4
     - 10 through 13
   * - 141 through 149
     - SH1 through SH9
     - 14 through 22
   * - 161 through 165
     - TU1 through TU5
     - 23 through 27
   * - 181 through 189
     - TL1 through TL9
     - 28 through 36
   * - 201 through 204
     - SB1 through SB4
     - 37 through 40
   * - 14, 91, 92, 93, 98, 99
     - Explicit nonburnable categories
     - 41

``N_FUEL_CAT_SCOTT_BURGAN`` counts the 40 burnable fuel models. Arrays allocate
``N_FUEL_CAT_SCOTT_BURGAN + 1`` rows, including the shared nonburnable row. This
row count is independent of the number of fuel components or moisture classes.
Anderson similarly uses 13 burnable rows and no-fuel row 14. It retains the
existing SB-to-Anderson crosswalk as degraded compatibility behavior; this
crosswalk does not provide native SB40 physics.

Resolution runs over all owned tiles before physics initialization and after
restart category reads. OpenMP distributes the tiles; the initial thread then
reduces invalid-category counts across MPI tasks and reports sample locations.
Unknown, missing, nonfinite, and fractional categories stop the simulation.
Unavailable halo indices remain unresolved; fuel-table consumers use owned
tiles. A new consumer requiring halo indices must establish their categories
and resolve them before table access.

Nonburnable classes
-------------------

The named categories are NB1 (91, urban/developed), NB2 (92, snow/ice), NB3
(93, nonburnable agriculture), NB8 (98, open water), and NB9 (99, barren).
Code 14 is retained as the existing CFBM/WRF no-fuel alias. Their external
identities remain distinct even though the surface-fire solver maps all of
them to one no-fuel row. For example, water is saved as ``nfuel_cat = 98``
while its internal SB40 index is 41.

Native SB40 no-fuel cells have zero consumable load, reaction intensity, and
spread. Their WAF table entry is allocated and initialized so either wind
coupling path can safely access it. The shared row does not change land-surface
properties in the atmospheric model. NB1 does not model structural combustion;
NB3 applies to agricultural land maintained in a nonburnable condition.

Codes 94 through 97 are not implicitly accepted. Neither missing-data values
such as -9999 nor arbitrary custom categories are converted silently to no
fuel. Legacy WRF-Fire internal SB rows 15 through 54 are also rejected in
native mode. Additional classifications require an explicit documented mapping.

Component fuel and moisture treatment
-------------------------------------

The abstract ``fuel_t`` interface requires each fuel family to implement
``Resolve_fuel_index`` and ``Calc_effective_dead_load``. Anderson returns its
existing ``fgi`` for the latter. SB40 returns dead size-class load plus the
currently cured herbaceous load. The shared ``Resolve_fuel_indices`` method
handles the grid traversal and validation; the driver supplies tile bounds
after initializing the selected fuel tables. This keeps grid-state ownership
out of the fuel classes while making the scalar mapping family-specific.

SB40 herbaceous/woody loads and SAV arrays reside in ``fuel_scott_burgan_t``.
They are populated and used by the component calculation. Anderson keeps its
existing ``fgi_live`` representation. A zero-filled live-extinction table is
unnecessary because SB40 live extinction is calculated from the fuelbed state.

SB40 tables store oven-dry dead 1-hour, 10-hour, and 100-hour loads, live
herbaceous load, and live woody load in US short tons per acre. The conversion
to kg m-2 uses ``2000 * 0.45359237 / (43560 * 0.3048**2)``. Herbaceous and woody
SAV values remain distinct from dead 1-hour SAV. Characteristic fuelbed SAV and
dead/live moisture weighting are computed from component surface areas.

With the distributed table, at herbaceous moisture of 0.30 or less, all herbaceous load transfers to dead;
at 1.20 or greater, none transfers. Between those limits the transfer is linear.
The cured fraction keeps herbaceous SAV and uses dead 1-hour moisture. Remaining
herbaceous and woody loads enter the live reaction calculation. Live moisture
of extinction is derived from current dead/live fine-fuel loads and dead
moisture; it is not a fixed table of zero values. The calculation includes
Albini's SAV-band weighting of net reaction load and separate dead/live damping.

``fuelmc_g_live`` retains the public Anderson name and default of 0.30. It
initializes herbaceous moisture in SB40. ``fuelmc_g_woody`` initializes SB40
woody moisture and defaults to 0.90.

The defaults are defined in ``namelist_t``. After the namelist is read and
broadcast, the fire driver passes all three configured surface-moisture values
to ``Init_ros_wrffire``. That routine assigns them to the ROS object before any
spread parameters are calculated. All three arguments are required; the ROS
class has no separate defaults. These scalar copies supply prescribed SB40 ROS;
Anderson ROS uses cell-level ``fmc_g``. The namelist live-moisture control still
initializes Anderson's live FMC class when the moisture model is active.
The component unit test uses distinct nondefault
values to verify prescribed-moisture propagation and subsequent replacement by
evolving class state. Specify these moistures deliberately for the case:

.. code-block:: fortran

   &fire
     devel_opt = 1,
     fuel_opt = 2,
     fuelmc_g = 0.08,
     fuelmc_g_live = 0.75,
     fuelmc_g_woody = 0.90,
   /

With ``fmoist_run = .false.``, prescribed dead and live moistures supply the
component ROS calculation. With evolving moisture enabled, SB40 uses six FMC
classes: 1-hour, 10-hour, 100-hour, 1000-hour, herbaceous, and woody. The 1000-hour
slot retains its meaning but has zero load in the standard SB40 tables.
Anderson retains five classes. Both live classes retain the existing very long
relaxation time of 1e9 hours; separate state variables do not constitute a
physiological vegetation-moisture model. Curing uses current herbaceous state
whenever ROS parameters are updated.

For SB40, ``fmc_g`` is a dry-load-weighted mean used by consumption diagnostics
and coupling. ROS uses the individual classes, including their separate live
values, before this aggregation. It never substitutes that mean for component
moisture.

Consumption and coupled heat
----------------------------

``fuel_load_g`` holds total consumable dry mass, including live components.
Curing changes the partition but not this inventory. The existing common
exponential consumption fraction is retained for all components; this is not
a separate live-fuel burnup model. SB40 heat release uses each model's heat
content (including GR6), dry mass consumed, and moisture expressed per unit dry
mass. Anderson retains its existing arithmetic, including its legacy
wet/dry-load factors. Correcting those factors for Anderson is outside this
change.

Consequently, ``Init_fuel_vars`` uses total ``fgi`` for the consumable inventory.
Using ``Calc_effective_dead_load`` there would discard the live inventory and
make available mass depend on curing. That function describes a dead
partition; it does not define the total inventory for the component formulation.

Restarts retain external categories and prognostic moisture and consumption
state. Internal indices are reconstructed, then component ROS parameters are
recomputed from restored moisture. Curing must not reinitialize the stored
burn fraction or consumable inventory.

SB40 restart files carry ``sb40_formulation = 1`` to identify total dry
inventory and component moisture treatment. Earlier experimental SB40 files
without this attribute are rejected. For prescribed moisture, the three
namelist moisture values must also match the restart metadata. Evolving
moisture is restored from its six-class state. Anderson restart compatibility
with the distributed parameters is retained, subject to the table-identity
check described above.

Unit validation
---------------

``fuel_table_*`` checks both distributed identities and Anderson's legacy
rounding. An experimental SB40 table doubles GR1's 1-hour load, changes its
drying lag, and shifts the curing interval; assertions require changes in
inventory, reciprocal lag, and curing fraction. Reordered fuel records must
retain identical model properties and names. Rejection cases cover malformed
headers, units, codes, numeric fields, nonburnable loads, moisture policies,
missing files, and incomplete or extra records. Text metadata must survive a
NetCDF roundtrip. MPI cases reverse the communicator and give non-root ranks
an invalid filename, requiring that only the communicator root open the table
and that all ranks receive identical parameters or reject the same defect.

``sb40_components_unit`` checks every native category, all accepted no-fuel
aliases, unchanged external metadata, four-tile coverage, the no-fuel WAF row,
component table values, curing thresholds, conserved dry load, separate live
reaction contributions, extinction, six-class moisture weighting, ROS use
of component state, and dry-mass sensible/latent heat including GR6 and NB.
``sb40_unknown_unit``, ``sb40_reserved_nb_unit``,
``sb40_fractional_unit``, ``sb40_missing_unit``, and ``sb40_nonfinite_unit``
require the corresponding invalid map to produce a category diagnostic before
physics access. Existing Anderson moisture and extinction unit tests retain
their scientific expectations while supplying resolved integer rows.
MPI builds also register ``sb40_components_mpi`` and ``sb40_fractional_mpi``;
the latter places an invalid value only on rank zero and checks collective
rejection. The namelist broadcast test uses distinct herbaceous and woody
values with a communicator whose root differs from the world communicator.

The unit-test source describes each setup, its physical assumptions, and the
failure it is intended to detect. Regression configurations and reference
updates are maintained separately from these unit checks.

Independent numerical reference
-------------------------------

The component unit test includes zero-wind, zero-slope numerical values
generated by compiling and running the USFS ``firelab/behave`` library at
commit ``f3b2943e69abca942e795c5b1b45276f6338abb5`` in a separate reference
experiment. The generator is archived outside this repository. These are
model-to-model comparisons, not observational validation. The unit suite
compares CFBM results with fixed reference values; it neither builds nor runs
Behave and requires no C++ reference executable or installed Behave library.

Columns are external fuel code, herbaceous moisture, spread [ft min-1], dead
reaction intensity, live reaction intensity [BTU ft-2 min-1], and live moisture
of extinction [kg kg-1]. Dead class moistures are 0.06, 0.07, and 0.08; woody
moisture is 0.90. The generator evaluates twelve models spanning all six SB40
groups. The unit test fixes eight cases covering full curing, uncured fuel,
woody reaction, coarse dead fuel, and GR6 heat content, with a relative
tolerance of 2e-6 for the default-real CFBM interface.
Live extinction has no physical role in a bed without live fuel; the fixed
numerical assertions compare spread and the two reaction intensities, rather
than requiring the same diagnostic convention for that undefined quantity.

Behave's dynamic-load transfer uses ``1.333 - 1.11 * moisture`` between 0.30
and 1.20. This rounded expression leaves a cured fraction of 0.001 at 1.20.
CFBM uses ``(1.20 - moisture) / 0.90``, clipped to [0, 1], and therefore reaches
exactly zero at 1.20. The fixed numerical references use 0.30 and 1.21 to avoid
conflating that intentional difference with reaction-calculation errors.
Separate unit assertions verify CFBM's 0.30, 0.75, and 1.20 curing fractions.
The reference generator reconstructs base spread from Behave's reaction
intensity, propagating-flux ratio, and heat sink; it does not validate the
complete wind/slope coupling or spatial fire solver.

Sources
-------

* Scott and Burgan (2005), `Standard fire behavior fuel models, RMRS-GTR-153
  <https://www.fs.usda.gov/rm/pubs_series/rmrs/gtr/rmrs_gtr153.pdf>`_, Table 7
  and the dynamic-fuel and nonburnable-model descriptions.
* Andrews (2018), `The Rothermel surface fire spread model and associated
  developments, RMRS-GTR-371 <https://research.fs.usda.gov/treesearch/55928>`_.
* `USFS Behave source at the reference revision
  <https://github.com/firelab/behave/tree/f3b2943e69abca942e795c5b1b45276f6338abb5>`_.
* `LANDFIRE FBFM40 attribute dictionary
  <https://www.landfire.gov/sites/default/files/DataDictionary/LF2025/LF2025_FBFM40ADD.pdf>`_.
