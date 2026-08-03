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

The options specific to the CFBM are controlled by a :term:`namelist` file ``namelist.fire``. This namelist file consists of three sections: ``&time``, ``&atm``, and ``&fire``. The available options in each section are described below.

Example namelists can be found in the various test subdirectories under the ``tests/`` directory.


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
   Atmospheric time step in seconds.

``interval_output``: *integer* (**Required**)
   [Units: s]
   Specifies the time interval (in seconds) for writing to the history output files

``num_tiles``: *integer* (Default: ``1``)
   Number of tiles for MPI domain decomposition. Not yet implemented.


&atm
----
``kde``: *integer* (Default: ``2``)
   Number of vertical levels for the atmospheric simulation

``interval_atm``: *integer* (Default: ``0``)
   [Units: s]
   Time interval (in seconds) for incoming atmospheric data. When running a coupled model, this value represents the atmospheric timestep. In offline mode, it determines the frequency of reading atmospheric data from the input file.


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

     Intermediate values will vary the amount of forcing provided from the fire to the dynamical core.

``fire_upwinding``: *integer* (Default: ``9``)
   This option controls the type of upwinding scheme used for calculating the normal spread of the fire front. The choice of upwinding scheme significantly impacts the accuracy of fire spread simulations. Higher-order schemes, like WENO3 and WENO5, generally offer better accuracy but can be more computationally expensive.
     0: Central Difference: Uses central differences for calculating gradients, combining left- and right-sided differences for both x- and y-directions to compute a central gradient approximation.

     1: Standard: Employs an upwind scheme, selecting between left- and right-sided differences based on flow direction.

     2: Godunov: The Godunov scheme is a first-order upwind scheme based on Osher & Fedkiw

     3: ENO1: The First-Order Essentially Non-Oscillatory (ENO1) scheme uses the smoothest stencil to avoid sharp gradients, which can lead to underestimations of fire area and errors in the rate of spread.

     4: Sethian scheme :cite:`SethianMethod`. The propagation Hamiltonian uses
     the Godunov magnitude while the wind- and slope-relative ROS calculation
     uses signed Godunov normal derivatives.

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

     5: Godunov gradient with optional sign-aware branching and
     Russo-Smereka interface pinning

``reinit_godunov_sign_branch``: *logical* (Default: ``.false.``)
   For ``fire_upwinding_reinit=5``, select the Godunov one-sided derivative
   branch from the frozen pre-reinitialization sign. When disabled, option 5
   uses the corrected non-branch Godunov norm based on the maximum one-sided
   contribution in each coordinate direction.

``reinit_use_russo_smereka``: *logical* (Default: ``.false.``)
   Apply Russo-Smereka interface pinning during reinitialization option 5.
   The entering zero contour, sign, and subcell distance are frozen before the
   pseudo-time iterations. On the detected interface ring, the distance
   estimate is capped at 1.5 times the larger horizontal grid spacing.
   Disabled mode leaves the PDE tendency unpinned.

``reinit_rs_buffer_ngp``: *integer* (Default: ``0``)
   Nonnegative number of grid-cell layers added around the detected
   Russo-Smereka interface ring. A value of 0 pins only the ring.

``allow_RS_any_reinit``: *logical* (Default: ``.false.``)
   Developer override that permits Russo-Smereka diagnostics to be
   constructed with another reinitialization option. Pinning remains limited
   to option 5.

``reinit_conditional_no_retreat``: *logical* (Default: ``.false.``)
   Control where the post-reinitialization no-retreat clamp is applied. The
   default applies the historical global clamp, ``lfn_out=min(lfn_reinit,
   lfn_in)``. When enabled, the clamp is applied only where the entering
   level-set field is negative, allowing reinitialization to increase ``lfn``
   on previously unburned cells.

``reinit_pseudot_rate``: *real* (Default: ``-1.0``)
   Reinitialization pseudo-time rate [m s-1]. The sentinel value ``-1.0``
   preserves legacy coefficient mode, in which each PDE iteration uses
   the pseudo-time increment ``reinit_pseudot_coef * dx`` [m]. A nonnegative
   value applies the total pseudo-time increment ``reinit_pseudot_rate * dt``
   [m] per model step, divided equally among ``fire_lsm_reinit_iter``
   iterations.

``reinit_pseudot_cfl``: *real* (Default: ``0.5``)
   Positive upper bound on ``dt_pseudo/min(dx,dy)`` in rate mode. The model
   stops when this bound is exceeded and reports stable alternatives for the
   iteration count, pseudo-time rate, or physical timestep. This check does not
   alter legacy coefficient mode.

``fire_lsm_band_ngp``: *integer* (Default: ``4``)
   When using ``fire_upwinding_reinit=3,4`` and ``fire_upwinding=8/9``, the number of grid points around lfn=0 that WENO5/3 is used

``fast_dist_reinit_at_startup``: *logical* (Default: ``.false.``)
   For real-perimeter initialization, run one fast-sweeping distance
   reinitialization pass before the first level-set propagation. The startup
   pass always uses method 1 and is independent of ``fast_dist_reinit_opt``.

``fast_dist_reinit_opt``: *integer* (Default: ``0``)
   Periodic fast-distance reinitialization method used during time stepping.
   A value of 0 disables periodic fast-distance reinitialization and a value
   of 1 selects the fast-sweeping method. This option does not disable an
   explicitly enabled startup pass.

``fast_dist_reinit_freq``: *integer* (Default: ``600``)
   Number of model timesteps between periodic fast-distance reinitialization
   operations when ``fast_dist_reinit_opt=1``.

``fire_lsm_zcoupling``: *logical* (Default: ``1``)
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

``fire_wind_height``: *integer* (Default: ``6.096``)
   [Units: m]
   Height of uah,vah wind in fire spread formula

``fire_is_real_perim``: *logical* (Default: ``.false.``)
   Determines if perimeter represents a real fire boundary.
     .true. = observed perimeter

     .false. = point/line ignition

``frac_fburnt_to_smoke``: *real* (Default: ``0.02``)
   [Units: g/kg]
   Parts per unit of burned fuel converted to smoke, represented as grams of smoke per kilogram of air.

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

``use_ros_cap``: *logical* (Default: ``.true.``)
   Apply an upper bound to the rate of spread calculated by the Rothermel
   parameterization. The default retains the prior fixed upper bound. When
   ``.false.``, the calculated rate of spread is not capped.

``ros_cap_value``: *real* (Default: ``6.0``)
   [Units: m s-1]
   Upper bound applied to the combined no-wind, wind, and slope rate of spread
   when ``use_ros_cap=.true.``. The value must be positive when the cap is
   enabled.

``fmc_opt``: *integer* (Default: ``1``)
   :term:`FMC` model
     -1 = Constant fuel moisture (only option currently implemented)

``fire_num_ignitions``: *integer* (Default: ``1``)
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

``fire_ignition_end_time1``: *real* (Default: ``1``)
   [Units: s]
   End time of first ignition in seconds (counting from the beginning of the simulation)

``fire_ignition_radius1``: *real* (Default: ``0.0``)
   [Units: m]
   Radius of the ignition area for first ignition.

``output_level``: *integer* (Default: ``0``)
   Developer-facing output selector in the ``devel`` namelist block. Values
   greater than 0 add specialized level-set fields to NetCDF output:

   * ``grad_norm_ls`` is the propagation-stage level-set gradient norm.
   * ``grad_norm_reinit`` is the reinitialization-stage gradient norm.
   * ``lfn_tend_dbg`` is the total final-stage level-set tendency [s-1].
   * ``lfn_adv_dbg`` is the final-stage physical spread contribution [s-1].
   * ``lfn_visc_dbg`` is the final-stage artificial-viscosity contribution [s-1].
   * ``lfn_pre_reinit_dbg`` and ``lfn_post_reinit_dbg`` are the level-set
     fields immediately before and after reinitialization, respectively.
   * ``lfn_reinit_delta_dbg`` is the post-minus-pre reinitialization increment.
   * ``lfn_retreat_delta_dbg`` is the part of the raw PDE-reinitialization
     increment [m] rejected by the no-retreat clamp. It is zero where the clamp
     is inactive or does not alter the result.
   * ``lfn_fastdist_delta_dbg`` is the post-minus-pre fast-distance
     reinitialization increment [m], and is zero on timesteps without a
     periodic fast-distance operation.
   * ``lfn_laplacian_dbg`` is the centered discrete Laplacian of the final
     level-set field [m-2].
   * ``rs_interface_mask`` is a dimensionless 0/1 field identifying the frozen
     Russo-Smereka interface ring and optional buffer cells.
   * ``rs_distance_dbg`` is the frozen Russo-Smereka distance [m]. The detected
     ring uses the capped subcell estimate; optional buffer cells use the
     entering ``abs(lfn)``. The field is zero outside ``rs_interface_mask`` and
     when Russo-Smereka construction is disabled.

Each PDE reinitialization also logs the number of strict nonzero sign reversals
between the entering and final level-set fields. If the Russo-Smereka distance
gradient falls below its defensive denominator threshold, each MPI rank also
reports its local fallback count; those cells use the entering ``abs(lfn)``.

``check_isolated_neg_lfn``: *integer* (Default: ``0``)
   Developer diagnostic evaluated after level-set propagation and
   reinitialization, before prescribed ignition is applied. A qualifying
   component contains 1 through 6 cells with ``lfn < 0``, uses eight-neighbor
   connectivity, and does not touch the local MPI patch boundary.

     0: disable the connected-component search

     1: save the current state and stop when a component is detected

     2: report the component size and location and continue the simulation

   Enabled modes allocate search work arrays, scan the local physical domain,
   and perform MPI reductions every model timestep. Report-only mode therefore
   retains the diagnostic cost even when no component is found.
