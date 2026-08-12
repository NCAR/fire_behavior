.. _Idealized:

========================================
CFBM: Idealized cases
========================================

An idealized case runs the fire model on a synthetic domain with prescribed,
uniform conditions instead of real geographic and atmospheric data. Idealized
runs are the simplest way to exercise the model. They
are useful for testing a build, learning the workflow, and studying the
sensitivity of fire spread to wind, fuel, and slope in a controlled setting.

The domain (grid and map projection), fuels, terrain, and a constant wind are
all defined in the ``&ideal`` section of the namelist.

Enabling idealized mode
========================================

Set ``ideal_opt = 1`` in the ``&fire`` section. This tells the model to build
the domain from the ``&ideal`` section rather than reading ``geo_em.d01.nc`` and
atmospheric input. In this mode:

* The wind is constant and uniform, set by ``zonal_wind`` and
  ``meridional_wind``; it does not evolve in time.
* Fuel is a single uniform category given by ``fuel_cat`` (Anderson fuel model).
* Terrain is a uniform slope (``dz_dx``, ``dz_dy``) at a constant ``elevation``.
* The fuel moisture model is not available; ``fmoist_run`` must be ``.false.``
  (its default). Fuel moisture instead comes from the constant ``fuelmc_*``
  values in ``&fire``.

Ignitions and timing are configured exactly as in a real-world run, through the
``&fire`` ignition parameters and the ``&time`` section. The full list of
options for every section is documented in :ref:`namelist`.

Example configuration
========================================

The ``namelist.fire`` below sets up a 10 km x 10 km flat domain (100 x 100 cells
at 100 m spacing) with a steady 10 m/s westerly wind over uniform grass fuel,
ignited along a north-south line near the western edge of the domain. Driven by
the wind, the fire spreads eastward across the domain. The run covers twenty
minutes and writes output every minute.

.. code-block:: fortran

   &time
    start_year        = 2020
    start_month       = 6
    start_day         = 1
    start_hour        = 12
    start_minute      = 0
    start_second      = 0
    end_year          = 2020
    end_month         = 6
    end_day           = 1
    end_hour          = 12
    end_minute        = 20
    end_second        = 0
    dt                = 0.5
    interval_output   = 60
   /

   &atm
    kde               = 1     ! single level; no atmospheric data is read in idealized mode
    interval_atm      = 1     ! unused in idealized mode
   /

   &fire
    ideal_opt          = 1    ! build the domain from &ideal
    fire_num_ignitions = 1
    fire_ignition_start_lon1  = -105.04
    fire_ignition_start_lat1  = 40.03
    fire_ignition_end_lon1    = -105.04
    fire_ignition_end_lat1    = 39.97
    fire_ignition_radius1     = 100.0
    fire_ignition_start_time1 = 0.0
    fire_ignition_end_time1   = 2.0
    fire_ignition_ros1        = 1.0
   /

   &ideal
    nx              = 100
    ny              = 100
    dx              = 100.0
    dy              = 100.0
    zonal_wind      = 10.0
    meridional_wind = 0.0
    fuel_cat        = 3
    dz_dx           = 0.0
    dz_dy           = 0.0
    elevation       = 0.0
    cen_lat         = 40.0
    cen_lon         = -105.0
    stand_lon       = -105.0
    true_lat_1      = 40.0
    true_lat_2      = 40.0
   /

The ignition coordinates are given in longitude/latitude and must fall within
the domain. To change the case, adjust the wind in ``&ideal``, the fuel category, 
the slope, or the ignition location and timing in ``&fire``.

Running an idealized case
========================================

Build the standalone model as described in :ref:`building`, then run it from a
directory containing only the ``namelist.fire`` above:

.. code-block:: console

   cd <run-directory>
   /path/to/build/fire_behavior.exe

No other input files are required. Model output is written to ``fire_output_*``
files and run-time diagnostics to a ``log`` file.
