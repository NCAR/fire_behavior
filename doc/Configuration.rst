.. _Configuration:

==============================================
Configuring the Community Fire Behavior Module
==============================================


.. _domain_config:

Configuring a domain with the WRF Pre-processing System (WPS)
=============================================================

Because the CFBM was originally developed as part of the WRF model, creating a domain must be done using the WRF Pre-processing System (WPS). These instructions can be found in `the WRF Users Guide <https://www2.mmm.ucar.edu/wrf/users/wrf_users_guide/build/html/fire.html#running-wrf-fire-on-real-data>`_. To run the CFBM with the UFS or with WRF data, users need to provide a geo_em.d01.nc file containting the interpolated static data fields.

Future releases will include a method for creating domains without needing to compile WPS.

.. _namelist:

Namelist Configuration
======================

The options specific to the CFBM are controlled by a :term:`namelist` file
``namelist.fire`` for standalone and NUOPC runs. When coupled directly to WRF
v4.8.0, these options are read from ``namelist.cfbm``; WRF's ``namelist.input``
enables the coupling (see :ref:`WRF`). This namelist file consists of three
required sections (``&time``, ``&atm``, and ``&fire``) and an optional ``&ideal``
section. The ``ideal_opt`` option and ``&ideal`` section are available only for
standalone runs, not for WRF-CFBM or NUOPC-coupled runs. The available options
in each section are described below.

Example namelists can be found in the various test subdirectories under the ``tests/legacy/`` directory.


&time
---------------------------------

``start_year``: *integer* (**Required**)
   Start year of the simulation.

``start_month``: *integer* (**Required**)
   Start month of the simulation.

``start_day``: *integer* (**Required**)
   Start day of the simulation.

``start_hour``: *integer* (**Required**)
   Start hour of the simulation.

``start_minute``: *integer* (**Required**)
   Start minute of the simulation.

``start_second``: *integer* (**Required**)
   Start second of the simulation.

``end_year``: *integer* (**Required**)
   End year of the simulation.

``end_month``: *integer* (**Required**)
   End month of the simulation.

``end_day``: *integer* (**Required**)
   End day of the simulation.

``end_hour``: *integer* (**Required**)
   End hour of the simulation.

``end_minute``: *integer* (**Required**)
   End minute of the simulation.

``end_second``: *integer* (**Required**)
   End second of the simulation.

``dt``: *real* (Default: ``2.0``)
   Model (fire) integration time step in seconds. This controls how often the fire model advances.

``interval_output``: *integer* (**Required**)
   [Units: s]
   Specifies the time interval (in seconds) for writing to the history output files.
   The timestamp describes the completed fire state. In standalone and NUOPC
   runs, the saved atmospheric fields are the forcing used during the completed
   fire interval, before the next atmospheric refresh. For example, with 4 s
   fire and atmospheric intervals, output at 60 s contains the 56 s forcing.
   This output ordering does not change the forcing used for fire integration.

``num_tiles``: *integer* (Default: ``1``)
   Number of OpenMP tiles per MPI process. The fire computations loop over ``num_tiles`` tiles under ``!$OMP PARALLEL DO``, so this sets the shared-memory (OpenMP) threading granularity. The example namelists in ``tests/legacy/`` use ``num_tiles = 16``.


&atm
----
``kde``: *integer* (Default: ``1``)
   Number of vertical levels for the atmospheric simulation

``interval_atm``: *integer* (Default: ``-1``)
   [Units: s]
   Time interval for incoming atmospheric data. In offline runs, set a positive
   value matching the interval between atmospheric records in ``wrf.nc``. The
   default ``-1`` is an unset value; it does not detect the interval automatically.
   Standalone idealized runs do not read atmospheric data and ignore this option,
   so it may be left at its default. NUOPC runs require a positive atmospheric
   coupling interval. Direct WRF-CFBM coupling uses WRF's time stepping instead
   and does not use this option (see :ref:`WRF`).


Numerical precision
-------------------

Atmospheric coordinates
~~~~~~~~~~~~~~~~~~~~~~~

The WRF-data reader accepts single- or double-precision ``XLAT`` and ``XLONG``
and retains cell-centre coordinates in double precision when constructing the
NUOPC atmospheric grid. Existing single-precision WRF files remain supported;
promoting their stored values cannot recover coordinate precision already lost.

The standalone inverse Lambert transform evaluates its geometry in double
precision before returning grid indices at the existing precision. This avoids
loss of interpolation detail when subtracting large pole and radius terms.
The forward transform and stored fire-grid coordinates retain their existing
precision, so this change does not relocate the fire grid or change its spacing.

Fuel accounting
~~~~~~~~~~~~~~~

Remaining fuel and the fraction consumed during each fire timestep are calculated
and stored internally in double precision. Subcell interpolation, burning-curve
evaluation, and accumulation retain that precision before successive remaining
fractions are subtracted. This preserves small consumption increments when most
of the fuel is still present. The burning law and fractional accounting are
unchanged. Heat and emission calculations use the more precise increments;
output fields retain their existing single-precision NetCDF representation.


&fire
-----

``fire_print_msg``: *integer* (Default: ``0``)
   Debug print level for the fire module.
     Levels greater than 0 will print extra messages at run time.

     0: no extra prints

     1: Extra prints

     2: More extra prints

     3: Even more extra prints

``fire_atm_feedback``: *real* (Default: ``1.0``)
   Multiplier for heat fluxes from the fire to the atmosphere.
     0.0: one-way (atmosphere --> fire) coupling.

     1.0: normal two-way coupling.

     Intermediate values will vary the amount of forcing provided from the fire to the :term:`dynamical core`.

``fire_upwinding``: *integer* (Default: ``9``)
   This option controls the type of upwinding scheme used for calculating the normal spread of the fire front. The choice of upwinding scheme significantly impacts the accuracy of fire spread simulations. Higher-order schemes, like WENO3 and WENO5, generally offer better accuracy but can be more computationally expensive.
     0: Central Difference: Uses central differences for calculating gradients, combining left- and right-sided differences for both x- and y-directions to compute a central gradient approximation.

     1: Standard: Employs an upwind scheme, selecting between left- and right-sided differences based on flow direction.

     2: Godunov: The Godunov scheme is a first-order upwind scheme based on Osher & Fedkiw

     3: ENO1: The First-Order Essentially Non-Oscillatory (ENO1) scheme uses the smoothest stencil to avoid sharp gradients, which can lead to underestimations of fire area and errors in the rate of spread.

     4: Sethian scheme :cite:`SethianMethod`

     5: 2nd-order: Calculates gradients using a second-order central difference.

     6: WENO3: Third-Order Weighted Essentially Non-Oscillatory (WENO3) scheme.

     7: WENO5: Fifth-Order Weighted Essentially Non-Oscillatory (WENO5) scheme.

     8: Hybrid WENO3/ENO1: A hybrid scheme that combines WENO3 in a band surrounding the fire front interface with ENO1 for regions further away. This approach reduces computational cost while maintaining accuracy near the front.

     9: Hybrid WENO5/ENO1 (default): Similar to option 8, but uses WENO5 instead of WENO3 in the band surrounding the fire front. This approach reduces computational cost while maintaining accuracy near the front.

``fire_viscosity``: *real* (Default: ``0.4``)
   Artificial viscocity in :term:`level-set method` away from the near-front region.

``fire_lsm_reinit``: *logical* (Default: ``.true.``)
   Flag to activate reinitialization of the :term:`level-set method`

``fire_lsm_reinit_iter``: *integer* (Default: ``1``)
   Number of iterations for reinitialization :term:`PDE`

``fire_upwinding_reinit``: *integer* (Default: ``4``)
   Numerical scheme (space) for reinitialization :term:`PDE`.
     1: WENO3

     2: WENO5

     3: hybrid WENO3-ENO1

     4: hybrid WENO5-ENO1

``fire_lsm_band_ngp``: *integer* (Default: ``4``)
   When using ``fire_upwinding_reinit=3,4`` and ``fire_upwinding=8/9``, the number of grid points around lfn=0 that WENO5/3 is used

``fire_lsm_zcoupling``: *logical* (Default: ``.false.``)
   When true, uses ``fire_lsm_zcoupling_ref`` instead of ``fire_wind_height`` as a reference height to calculate the logarithmic surface layer wind profile

``fire_lsm_zcoupling_ref``: *real* (Default: ``50.0``)
   [Units: m]
   Reference height from which the velocity at ``fire_wind_height`` is calculated using a logarithmic profile

``fire_viscosity_bg``: *real* (Default: ``0.4``)
   Artificial viscosity in the near-front region

``fire_viscosity_band``: *real* (Default: ``0.5``)
   Number of times the hybrid advection band to transition from ``fire_viscosity_bg`` to ``fire_viscosity``

``fire_viscosity_ngp``: *integer* (Default: ``2``)
   Number of grid points around lfn=0 where ``fire_viscosity_bg`` is used

``fmoist_run``: *logical* (Default: ``.false.``)
   Enables evolving class moisture on the fire grid. The first moisture call
   initializes the four dead classes from equilibrium and the live class from
   ``fuelmc_g_live``, using the first supplied atmospheric forcing. Initialization
   occurs independently of the update cadence and copies that forcing into the
   previous-forcing state. Missing, nonfinite, or invalid forcing stops the run.
   Effective moisture ``fmc_g`` is used by the spread model; class state
   ``fmc_gc`` is stored in checkpoints, not ordinary output.

   The effective initialization time is the first call's ``itimestep*dt``
   timestamp, normally ``dt`` seconds from the original run origin. Initialization
   applies zero elapsed moisture time. Later updates use the interval since
   initialization or the last update. This retains the existing component's
   timestamp convention without introducing an artificial initial advance.

``fmoist_freq``: *integer* (Default: ``0``)
   Frequency of moisture updates after initialization.
     0: use ``fmoist_dt``

     k>0: every "k" timesteps

``fmoist_dt``: *real* (Default: ``600.0``)
   [Units: s]
   Numerical update interval of the moisture model (only used if ``fmoist_freq=0``),
   measured from its effective initialization time. It is distinct from a fuel
   class's physical response time. Step-based updates retain their phase relative
   to the original run origin. For example, with ``dt=4`` and ``fmoist_freq=3``,
   initialization occurs at 4 s, the first update at 12 s uses 8 s elapsed time,
   and subsequent updates use 12 s intervals.

   Checkpoints save initialization status/time, class moisture, previous forcing,
   last/next update times, and the moisture activation and cadence settings.
   A resumed run restores this state without reinitialization and requires the
   same activation and cadence. New checkpoints cannot be written before
   moisture initialization. PR #39 time-based checkpoints are accepted when their
   positive next-update time establishes initialization and their saved interval
   matches ``fmoist_dt``. Legacy step-based files are rejected because their
   metadata cannot distinguish initialized state from the old delayed-initialization
   defect. Prescribed-moisture legacy files remain readable. This compatibility
   rule does not resolve the separate humidity-convention audit.

``fire_wind_height``: *real* (Default: ``6.096``)
   [Units: m]
   Height of uah,vah wind in fire spread formula

``fire_is_real_perim``: *logical* (Default: ``.false.``)
   Determines if the supplied perimeter represents an observed fire boundary.
   When this option is true, set ``fire_num_ignitions=1`` and use
   ``fire_ignition_start_time1`` for the perimeter activation time.  The line
   coordinates, radius, rate of spread, and end time in ignition record 1 are
   ignored.

     .true. = observed perimeter

     .false. = point/line ignition

``frac_fburnt_to_smoke``: *real* (Default: ``0.02``)
   [Units: kg/kg]
   Fraction of burned fuel mass released as smoke when ``emis_opt=0``. The
   default corresponds to 20 g of smoke per kilogram of burned fuel.

``fuelmc_g``: *real* (Default: ``0.08``)
   Fuel moisture content ground (Dead :term:`FMC`)

``fuelmc_g_live``: *real* (Default: ``0.30``)
   Fuel moisture content ground (Live :term:`FMC`). 30% Completely cured, treat as dead fuel

``fuelmc_c``: *real* (Default: ``1.00``)
   Fuel moisture content of the canopy

``fuel_opt``: *integer* (Default: ``1``)
   Fuel type model.
     1:  Anderson fuel model (only option currently implemented)

``ros_opt``: *integer* (Default: ``0``)
   Rate of Spread (ROS) parameterization option.
     0: Rothermel model (only option currently implemented)

``fmc_opt``: *integer* (Default: ``-1``)
   :term:`FMC` model
     -1 = Constant fuel moisture (only option currently implemented)

``ideal_opt``: *integer* (Default: ``0``)
   Available only for standalone runs. Selects a real-world or an idealized
   standalone simulation; it does not configure idealized WRF-CFBM or
   NUOPC-coupled simulations.
     0: Real-world run. The domain (grid, map projection, fuel, and topography) is read from ``geo_em.d01.nc`` and the fire is driven by external atmospheric data.

     1: Idealized run. The domain and a constant wind forcing are constructed from the ``&ideal`` section below instead of being read from input files. Idealized runs do not support the fuel moisture model (``fmoist_run`` must be ``.false.``).

``fire_num_ignitions``: *integer* (Default: ``0``)
   Number of ignitions for fire initiation. Maximum of 5.

.. note::
  For each additional fire ignition, you must specify an additional set of ignition parameters below, with increasing numerical suffixes ( *e.g.* ``fire_ignition_start_lon2``, ``fire_ignition_start_lon3``, etc. )

``fire_ignition_start_lon1``: *real* (Default: ``0.0``)
   Longitude of first ignition start point.

``fire_ignition_start_lat1``: *real* (Default: ``0.0``)
   Latitude of first ignition start point.

``fire_ignition_end_lon1``: *real* (Default: ``0.0``)
   Longitude of first ignition end point.

``fire_ignition_end_lat1``: *real* (Default: ``0.0``)
   Latitude of first ignition end point.

``fire_ignition_ros1``: *real* (Default: ``0.01``)
   [Units: m/s]
   Rate of spread for first ignition (Rothermel parameterization).

``fire_ignition_start_time1``: *real* (Default: ``0.0``)
   [Units: s]
   Start time of first ignition in seconds (counting from the beginning of the simulation)

``fire_ignition_end_time1``: *real* (Default: ``0.0``)
   [Units: s]
   End time of first ignition in seconds (counting from the beginning of the simulation)

``fire_ignition_radius1``: *real* (Default: ``0.0``)
   [Units: m]
   Radius of the ignition area for first ignition.


&ideal
------

This section is available only for standalone runs and is read when
``ideal_opt = 1``. It is not available for WRF-CFBM or NUOPC-coupled runs.
It defines an idealized domain (uniform fuel, a simple slope, and a constant
wind) so the standalone model can run without ``geo_em.d01.nc`` or external
atmospheric data.

``nx``: *integer* (Default: ``100``)
   Number of fire-grid points in the x (west-east) direction.

``ny``: *integer* (Default: ``100``)
   Number of fire-grid points in the y (south-north) direction.

``dx``: *real* (Default: ``100.0``)
   [Units: m]
   Grid spacing in the x direction.

``dy``: *real* (Default: ``100.0``)
   [Units: m]
   Grid spacing in the y direction.

``zonal_wind``: *real* (Default: ``5.0``)
   [Units: m/s]
   Constant zonal (west-east) wind component used to force the fire.

``meridional_wind``: *real* (Default: ``0.0``)
   [Units: m/s]
   Constant meridional (south-north) wind component used to force the fire.

``fuel_cat``: *integer* (Default: ``1``)
   Uniform fuel category assigned to the whole domain (Anderson fuel model; see ``fuel_opt``).

``dz_dx``: *real* (Default: ``0.0``)
   Terrain slope in the x direction (rise over run).

``dz_dy``: *real* (Default: ``0.0``)
   Terrain slope in the y direction (rise over run).

``elevation``: *real* (Default: ``0.0``)
   [Units: m]
   Uniform terrain elevation of the domain.

``cen_lat``: *real* (Default: ``40.3636``)
   [Units: degrees]
   Center latitude of the idealized domain.

``cen_lon``: *real* (Default: ``-4.4035``)
   [Units: degrees]
   Center longitude of the idealized domain.

``stand_lon``: *real* (Default: ``-4.4035``)
   [Units: degrees]
   Standard (reference) longitude of the map projection.

``true_lat_1``: *real* (Default: ``40.363``)
   [Units: degrees]
   First true latitude of the map projection.

``true_lat_2``: *real* (Default: ``40.363``)
   [Units: degrees]
   Second true latitude of the map projection.

Timing and perimeter validation
===============================

Output, forcing/exchange, restart writes, and the final time must align with
fire steps. Positive ``interval_output``, ``interval_atm``, and
``restart_interval`` values are whole seconds; ``-1`` disables an optional
schedule. At least one positive interval is needed to establish the fire clock.
Output and restart periods need not divide one another. For example,
``dt=0.3``, output every 12 s, forcing every 18 s, and restart writes every 9 s
are supported. An output interval of 13 s with that timestep is rejected.

The common interval is the greatest common divisor of enabled periods. Its
ratio to ``dt`` must differ from the nearest integer by at most
``min(0.01, 5e-6 * ratio)`` fire steps. The 5 ppm allowance covers timestep
inputs rounded to six significant digits, including ``0.222222`` for 2/9 s;
the 0.01-step bound prevents large intervals from admitting ambiguous alignment.
The resulting reduced rational seconds/steps defines the scheduling clock.
All other event and final times must be exactly reachable on this clock.
No tolerance grows with run duration, and no event fires after a missed step.

For example, consider these two accepted schedules:

.. list-table:: Converting event periods to fire-step counts
   :header-rows: 1
   :widths: 45 25 30

   * - Quantity
     - ``dt=0.3`` s
     - ``dt=0.222222`` s
   * - Output / atmosphere / restart periods
     - 12 / 18 / 9 s
     - 12 / 18 / 8 s
   * - Common interval (greatest common divisor)
     - 3 s
     - 2 s
   * - Common interval divided by input ``dt``
     - Approximately 10 steps
     - Approximately 9.000009 steps
   * - Accepted integer step count
     - 10
     - 9
   * - Scheduling timestep (common interval / step count)
     - 3/10 s
     - 2/9 s
   * - Output / atmosphere / restart step intervals
     - 40 / 60 / 30 steps
     - 54 / 81 / 36 steps

In the ``dt=0.3`` example, 3 s is the largest whole-second interval that divides
12, 18, and 9 s. It is used to construct the clock; it does not request an
event every 3 s. In the rounded-timestep column, the discrepancy of about
0.000009 step is smaller than the allowed ``5e-6 * 9 = 0.000045`` step.
The scheduler therefore represents each step as exactly 2/9 s, while physics
continues to use the configured ``dt=0.222222`` s.

For a rejected example, change the ``dt=0.3`` output period to 13 s while
keeping the 18 s atmospheric and 9 s restart periods. Their common interval
becomes 1 s, and ``1 / 0.3`` is about 3.333333 steps. Its distance from the
nearest integer is about 0.333333 step, far above the allowed 0.0000167 step.
The model rejects this schedule because 13 s falls between fire steps.

Event detection uses integer step counts from the original origin. Calendar
timestamps use integer whole seconds and a subsecond remainder, so elapsed
single-precision multiplication cannot stall output or drift across calendar
boundaries. Physics retains the configured ``dt``; normalization corrects the
schedule within the documented input-rounding allowance. The shared
``grid%clock%Elapsed(itimestep)`` interface returns REAL64 scheduling seconds
from the original origin. Moisture initialization and its elapsed-time state
remain the separate P02 change; the existing component still receives the
configured physics timestep.

With the 0.3 s clock above, output every 12 s means output at steps 40, 80,
120, and so on. The event check compares the current step with those integers;
it does not need a floating-point elapsed time to equal 12 s exactly. At step
41, the timestamp is constructed from ``41 * 3 / 10``: 12 whole seconds plus
a 3/10 s remainder after the original start time. With the rounded 2/9 s
clock, the first 12 s output occurs at step 54 because ``54 * 2 / 9 = 12``.
If the caller reaches step 55 without processing that event, the scheduler
reports a missed event instead of writing late or skipping a forcing record.

``interval_atm`` is the exchange cadence, independently of the atmospheric
solver timestep. File-driven and NUOPC entry points require it. Direct WRF
stepping and unforced ideal runs may leave it unset. Supplied positive intervals
are validated. NUOPC uses the exact rational fire clock for ESMF subcycling,
accumulates ``1/atm_step_interval`` of each instantaneous flux, and resets before
the first advance of the next exchange window. The file-driven component
requires consecutive records spaced by ``interval_atm`` and an exact record at
every requested update; missing records and implicit skipping are rejected.

Standalone stops at the validated final step without overshooting or taking a
partial step. Its endpoint need not be an output, forcing, or restart event.
NUOPC/ESMX run endpoints must additionally be reachable by the host exchange
clock. Direct WRF retains responsibility for its host integration endpoint.

New checkpoints store the reduced clock ratio and atmospheric interval.
Restart restores the original origin and absolute timestep, then reconstructs
the next events strictly after that step. A checkpoint between events therefore
preserves their original phase. Output/restart intervals may change if they
remain compatible with the saved clock; atmospheric cadence must match in
file-driven runs. Restart writes require whole-second boundaries, consistent
with the existing filenames and namelist date interface. The REAL64 fuel fields
and saved perimeter-activation state are unchanged.

For example, start a run at 00:00:00 with ``dt=0.3``, output every 12 s,
and checkpoints every 9 s. A checkpoint at 00:00:09 stores the original
00:00:00 origin and absolute step 30. To resume, the new namelist's ``start_*``
values select 00:00:09, but reading the checkpoint restores that original
origin and step count. The next advance is step 31. The next output is step
40 at 00:00:12, followed by step 80 at 00:00:24. Restarting does not shift
those outputs to 00:00:21 and 00:00:33. Checkpoints continue on their own
9 s schedule, next at step 60 (00:00:18). Both schedules remain measured
from 00:00:00, although their events do not always coincide.

Legacy PR #39 idealized checkpoints are accepted when their timestamp agrees
with the reconstructed original clock and their other configuration metadata
match. Legacy file-driven checkpoints lack atmospheric-cadence metadata and are
rejected with a specific compatibility diagnostic. This does not infer unknown
forcing or humidity semantics. NUOPC/WRF-host checkpoint support is not added by
this change.

For a supplied perimeter, ``fire_ignition_start_time1`` must be nonnegative
and align with a fire timestep. Roundoff-sized offsets are normalized to that
boundary; the requested and effective times are printed when they differ.
Validation uses four default-real roundoff units in the dimensionless step
count, capped at 0.0001 timestep. Clearly interior times are rejected, not
rounded. Prescribed line/circle times do not have this restriction: their
elapsed-time expansion is distinct from level-set propagation.

A perimeter at zero is installed before initial output. A delayed perimeter
is installed at the inclusive end of its scheduled interval and propagates
from the next interval. The former mid-step path could account for elapsed
burning time without propagating for that partial interval; supporting that
case consistently requires a separately reviewed split advance.
