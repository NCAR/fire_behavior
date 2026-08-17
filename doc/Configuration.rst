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
   This option controls the type of upwinding scheme used for calculating the normal spread of the fire front. The choice of upwinding scheme significantly impacts the accuracy of fire spread simulations. Higher-order schemes, like WENO3 and WENO5, generally offer better accuracy but can be more computationally expensive and unstable in heterogeneous fuels and terrain, producing numerical instability.
     0: Central Difference: Uses central differences for calculating gradients, combining left- and right-sided differences for both x- and y-directions to compute a central gradient approximation.

     1: Standard: Employs an upwind scheme, selecting between left- and right-sided differences based on flow direction.

     2: Legacy Godunov: A first-order, component-based (x,y) upwind scheme based on Osher & Fedkiw. 

     3: ENO1: The First-Order Essentially Non-Oscillatory (ENO1) scheme uses the smoothest stencil to avoid sharp gradients, which can lead to underestimations of fire area and errors in the rate of spread.

     4: Classical Godunov: The propagation Hamiltonian uses
     the Godunov magnitude while the wind- and slope-relative ROS calculation
     uses signed Godunov normal derivatives. This propagation option requires
     ``fire_upwinding_reinit=5``.

     5: 2nd-order: Calculates gradients using a second-order central difference.

     6: WENO3: Third-Order Weighted Essentially Non-Oscillatory (WENO3) scheme.

     7: WENO5: Fifth-Order Weighted Essentially Non-Oscillatory (WENO5) scheme.

     8: Hybrid WENO3/ENO1: A hybrid scheme that combines WENO3 in a band surrounding the fire front interface with ENO1 for regions further away. This approach reduces computational cost while maintaining accuracy near the front.

     9: Hybrid WENO5/ENO1 (default): Similar to option 8, but uses WENO5 instead of WENO3 in the band surrounding the fire front. This approach reduces computational cost while maintaining accuracy near the front.

     10: Hybrid WENO5/ENO1 with a cosine transition from WENO5 to ENO1
     across the outer half of the LFN-distance band.

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

     5: Sign-aware Godunov gradient with intrinsic Russo-Smereka interface
     pinning. The one-sided derivative branch is selected from the frozen
     pre-reinitialization sign. The entering zero contour, sign, and subcell
     distance are frozen before the pseudo-time iterations. On the detected
     interface ring, the distance estimate is capped at 1.5 times the larger
     horizontal grid spacing. The post-reinitialization no-retreat clamp is
     applied only where the entering level-set field is negative, allowing
     reinitialization to increase the level-set value on previously unburned
     cells.

``reinit_rs_buffer_ngp``: *integer* (Default: ``0``)
   Nonnegative number of grid-cell layers added around the detected
   Russo-Smereka interface ring for ``fire_upwinding_reinit=5``. A value of
   0 pins only the ring.

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
   Width in grid points of the numerical LFN-distance band. Propagation
   options 8 and 9 use WENO3 or WENO5, respectively, where
   ``abs(lfn) < fire_lsm_band_ngp * dx`` and ENO1 outside. Option 10 uses full
   WENO5 within that threshold, a cosine WENO5-to-ENO1 blend out to twice the
   threshold, and ENO1 beyond it. The condition is recalculated from the
   current LFN at every Runge-Kutta stage. Reinitialization options 3 and 4
   use the same parameter for their established LFN-distance stencil
   selection. This numerical control is independent of ``use_active_front``.

``fast_dist_reinit_at_startup``: *logical* (Default: ``.false.``)
   For real-perimeter initialization only, run one fast-sweeping distance
   reinitialization pass before the first level-set propagation. This option
   has no effect when ``fire_is_real_perim = .false.``. The startup pass
   always uses method 1 and is independent of ``fast_dist_reinit_opt``.

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

``ros_cap_value``: *real* (Default: ``6.0``)
   Developer control in the ``devel`` namelist block, read when
   ``devel_opt > 0``. [Units: m s-1]

   A positive value sets the upper bound applied to the combined no-wind,
   wind, and slope rate of spread calculated by the Rothermel
   parameterization. A value less than or equal to 0 disables the cap. The
   default retains the historical 6 m s-1 upper bound.

``rothermel_wind_speed_cap``: *real* (Default: ``30.0``)
   Developer control in the ``devel`` namelist block, read when
   ``devel_opt > 0``. [Units: m s-1]

   A positive value limits the wind speed used to calculate the Rothermel
   wind enhancement factor. A value less than or equal to 0 disables this
   wind-input cap. The option does not modify the atmospheric wind fields and
   does not apply to the chaparral rate-of-spread formulation. The final
   combined rate of spread remains independently controlled by
   ``ros_cap_value``.

``output_level``: *integer* (Default: ``0``)
   Developer-facing output selector in the ``devel`` namelist block. Values
   greater than 0 add these fields to NetCDF output:

   * ``ros`` is the modeled rate of spread [m s-1] evaluated from the local
     wind, terrain, fuel, and final propagation-stage level-set normal.
   * ``fire_area_change_rate`` is the signed model-timestep change in the
     dimensionless burned-area fraction [s-1]. The initial output is zero
     because no model advance has occurred. Area introduced during the first
     advance by prescribed ignition or real-perimeter initialization is
     included in the rate.

``lfn_diag``: *integer* (Default: ``0``)
   Developer-facing level-set diagnostic selector in the ``devel`` namelist
   block. A value of 0 disables the diagnostic calculations and storage. A
   value of 1 allocates the diagnostic fields and adds them to NetCDF output,
   independently of ``output_level``:

   * ``active_front_mask`` identifies burned, burnable interface cells that
     touch exterior-connected nonnegative-LFN burnable space when
     ``use_active_front=.true.``. Exterior connectivity is four-neighbor and
     interface contact is eight-neighbor. The field is zero when the option is
     false.
   * ``barrier_contact_front_mask`` identifies burned, burnable interface
     cells that touch only trapped nonnegative-LFN burnable pockets when
     ``use_active_front=.true.``. It is zero when that option is false.
   * ``band_mask`` is the diagnostic support band selected by
     ``use_active_front`` and expanded according to
     ``active_front_band_ngp``. In exact mode, the width-0/1 seed contains the
     burned-side active-front cells and their eight-neighbor exterior
     nonnegative-LFN burnable cells. Larger widths add four-neighbor layers on
     both sides, excluding barrier-contact cells from burned-side expansion.
   * ``ros_lfn_error_front`` is the kinematic rate-of-spread residual [m s-1]
     calculated from the final-stage level-set tendency, propagation-stage
     gradient norm, and modeled rate of spread where the gradient norm is
     numerically resolvable. Its support is the exact pre-reinitialization
     active-front mask when reinitialization occurs in exact-mask mode, the
     exact final active-front mask when it does not, or the area-change band
     in local diagnostic mode.
   * ``grad_norm_ls`` is the propagation-stage level-set gradient norm.
   * ``grad_norm_reinit`` is the reinitialization-stage gradient norm.
   * ``lfn_pre_reinit_dbg`` and ``lfn_post_reinit_dbg`` are the level-set
     fields immediately before and after reinitialization, respectively.
   * ``rs_interface_mask`` is a dimensionless 0/1 field identifying the frozen
     Russo-Smereka interface ring and optional buffer cells.
   * ``rs_distance_dbg`` is the frozen Russo-Smereka distance [m]. The detected
     ring uses the capped subcell estimate; optional buffer cells use the
     entering ``abs(lfn)``. The field is zero outside ``rs_interface_mask`` and
     when Russo-Smereka construction is disabled.

``use_active_front``: *logical* (Default: ``.false.``)
   Diagnostic-only selector in the ``devel`` namelist block. It is read when
   ``devel_opt > 0``, requires ``lfn_diag=1``, and never selects a propagation
   or reinitialization stencil.

   When false, ``band_mask`` is seeded exactly where
   ``abs(fire_area_change_rate) > 0`` and expanded with
   ``max(0, active_front_band_ngp - 1)`` four-neighbor passes. Every seed is
   retained, including seeds in fuel category 14, while added cells must have
   a fuel category other than 14. This path performs no exterior-connectivity
   calculation or global convergence loop.

   When true, the model diagnoses the exterior-connected active front and
   barrier-contact interface. A timestep with fast-distance or PDE
   reinitialization computes the exact masks before and after reinitialization;
   a timestep without reinitialization computes them once on the final LFN.
   The saved masks follow the final LFN after prescribed ignition. With
   ``fire_print_msg > 1``, the model reports the exact connectivity-call count
   for each physical timestep. The initial output contains zero masks and zero
   ``ros_lfn_error_front`` because no physical timestep has occurred.

``active_front_band_ngp``: *integer* (Default: ``4``)
   Nonnegative diagnostic-band width in grid points in the ``devel`` namelist
   block. Values 0 and 1 retain the selected seed; each larger value adds one
   bounded four-neighbor layer per additional grid point. This option controls
   only ``band_mask`` when ``lfn_diag=1`` and does not affect propagation,
   artificial viscosity, or reinitialization.

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
