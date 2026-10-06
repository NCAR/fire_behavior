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
   Runs moisture model on the atmospheric grid, outputting the result as a variable named ``fmc_gc``

``fmoist_freq``: *integer* (Default: ``0``)
   Frequency to run moisture model.
     0: use ``fmoist_dt``

     k>0: every "k" timesteps

``fmoist_dt``: *real* (Default: ``600.0``)
   [Units: s]
   Time step of moisture model (only used if ``fmoist_freq=0``)

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

``interval_atm`` must be a positive integer multiple of ``dt`` when using the
standalone forcing reader or NUOPC exchange schedule. For example, a 4 s fire
step and 60 s atmospheric interval are aligned. An 8 s fire step with a 4 s
atmospheric interval is not: the equality-based update trigger would miss the
first update and retain its expired schedule. The interval is an exchange
cadence, not necessarily the atmospheric solver timestep. Direct WRF stepping
and unforced ideal runs may leave ``interval_atm`` unset. Supplied positive
intervals are checked; file-driven and NUOPC entry points also require that the
interval is supplied. Programmatically constructed configurations are checked
before state allocation.

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
