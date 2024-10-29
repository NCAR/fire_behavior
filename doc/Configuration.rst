.. _Configuration:

==============================================
Configuring the Community Fire Behavior Module
==============================================


.. _domain_config:

Configuring a domain with the WRF Pre-processing System (WPS)
=============================================================

Because the CFBM was originally developed as part of the WRF model, creating a domain must be done using the WRF Pre-processing System (WPS). These instructions can be found in `the WRF Users Guide <https://www2.mmm.ucar.edu/wrf/users/wrf_users_guide/build/html/fire.html#running-wrf-fire-on-real-data>`_.

Future releases will include a method for creating domains without needing to compile WPS.

.. _namelist:

Namelist Configuration
======================

The options specific to the CFBM are controlled by a :term:`namelist` file ``namelist.fire``. This namelist file consists of three sections: ``&time``, ``&atm``, and ``&fire``. The available options in each section are described below.

Example namelists can be found in the various test subdirectories under the ``tests/`` directory.


&time
---------------------------------

``start_year``: *integer* (Required)  
   Start year of the simulation.

``start_month``: *integer* (Required)  
   Start month of the simulation.

``start_day``: *integer* (Required)  
   Start day of the simulation.

``start_hour``: *integer* (Required)  
   Start hour of the simulation.

``start_minute``: *integer* (Required)  
   Start minute of the simulation.

``start_second``: *integer* (Required)  
   Start second of the simulation.

``end_year``: *integer* (Required)  
   End year of the simulation.

``end_month``: *integer* (Required)  
   End month of the simulation.

``end_day``: *integer* (Required)  
   End day of the simulation.

``end_hour``: *integer* (Required)  
   End hour of the simulation.

``end_minute``: *integer* (Required)  
   End minute of the simulation.

``end_second``: *integer* (Required)
   End second of the simulation.

``dt``: *real* (Default: ``2.0``)
   Atmospheric time step in seconds.

``interval_output``: *integer* (Required)  
   Description needed

``num_tiles``: *integer* (Default: ``1``)
   Description needed


&atm
----
``kde``: *integer* (Default: ``2``)
   Number of vertical levels for the atmospheric simulation

``interval_atm``: *integer* (Default: ``0``)
   Description needed


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
     Use 0.0 for one-way (atmosphere --> fire) coupling. Use 1.0 for normal two-way coupling. 
     Intermediate values will vary the amount of forcing provided from the fire to the dynamical core.

``fire_upwinding``: *integer* (Default: ``9``)
   Method for calculating fire spread upwind.
     0: None

     1: Standard

     2: GODUNOV

     3: ENO

     4: SETHIAN

     5: 2nd-order

     6: WENO3

     7: WENO5

     8: hybrid WENO3/ENO1

     9: hybrid WENO5/ENO1

   More description needed for these options

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

``fire_lsm_zcoupling``: *logical* (Default: ``1``)
   When true, uses ``fire_lsm_zcoupling_ref`` instead of ``fire_wind_height`` as a reference height to calculate the logarithmic surface layer wind profile

``fire_lsm_zcoupling_ref``: *real* (Default: ``50.0``)
   Units: m
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

``fmoist_dt``: *integer* (Default: ``600``)
   Units: s
     Time step of moisture model (only used if ``fmoist_freq=0``)

``fire_wind_height``: *integer* (Default: ``6.096``)
   Units: m
     Height of uah,vah wind in fire spread formula

``fire_is_real_perim``: *logical* (Default: ``.false.``)
   Determines if perimeter represents a real fire boundary.
     .true. = observed perimeter

     .false. = point/line ignition

``frac_fburnt_to_smoke``: *real* (Default: ``0.02``)
   Units: g/kg
     Parts per unit of burned fuel converted to smoke, represented as grams of smoke per kilogram of air.

``fuelmc_g``: *real* (Default: ``0.08``)
   Fuel moisture content ground (Dead :term:`FMC`)

``fuelmc_g_live``: *real* (Default: ``0.30``)
   Fuel moisture content ground (Live :term:`FMC`). 30% Completely cured, treat as dead fuel

``fuelmc_c``: *real* (Default: ``1.00``)
   Fuel moisture content of the canopy

``fuel_opt``: *integer* (Default: ``1``)  
   Do not change.
     Fuel model

``ros_opt``: *integer* (Default: ``0``)
   Do not change.
     Rate of spread option parameterization.

``fmc_opt``: *integer* (Default: ``1``)  
   Do not change.
     :term:`FMC` model

``fire_num_ignitions``: *integer* (Default: ``1``)  
   Number of ignitions for fire initiation. Maximum of 5.

.. note::
  For each additional fire ignition, you must specify an additional set of ignition parameters below, with increasing numerical suffixes ( *i.e.* ``fire_ignition_start_lon2``, ``fire_ignition_start_lon3``, etc. )

``fire_ignition_start_lon1``: *real* (Default: ``0.0``)
   Longitude of first ignition start point.

``fire_ignition_start_lat1``: *real* (Default: ``0.0``)  
   Latitude of first ignition start point.

``fire_ignition_end_lon1``: *real* (Default: ``0.0``)
   Longitude of first ignition end point.

``fire_ignition_end_lat1``: *real* (Default: ``0.0``)
   Latitude of first ignition end point.

``fire_ignition_ros1``: *real* (Default: ``0.01``)  
   Rate of spread for first ignition (Rothermel parameterization).

``fire_ignition_start_time1``: *integer* (Default: ``0.0``)
   Start time of first ignition in seconds (counting from the beginning of the simulation)

``fire_ignition_end_time1``: *integer* (Default: ``1``)  
   End time of first ignition in seconds (counting from the beginning of the simulation)

``fire_ignition_radius1``: *real* (Default: ``1``)
   Units: m
     Radius of the ignition area for first ignition.


