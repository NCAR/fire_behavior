Output schema
=============

The standalone writer creates one two-dimensional, single-precision variable
for each row below.  All variables use the NetCDF default ``float`` fill value
(``9.96921e36``).  The regression comparator requires the listed dimensions,
units, long name, fill value, and applicability.  Static fields are compared
bit for bit.  Dynamic fields use the configured numerical tolerance after
masks are confirmed equal.

``fire_t2``, ``fire_q2``, ``fire_psfc``, ``fire_rain``, and ``fz0`` are not
applicable to standalone ideal simulations and contain only fill values in
that mode.  WRF input metadata are not required because the standalone reader
identifies its inputs by the established WRF variable names and dimensions.

.. list-table:: CFBM output variables
   :header-rows: 1
   :widths: 18 11 11 37 13

   * - Variable
     - Units
     - Applicability
     - Long name
     - Comparator
   * - ``lats``
     - degrees_north
     - all
     - fire-grid cell-center latitude
     - static
   * - ``lons``
     - degrees_east
     - all
     - fire-grid cell-center longitude
     - static
   * - ``fgrnhfx``
     - W m-2
     - all
     - ground fire sensible heat flux
     - dynamic
   * - ``fgrnqfx``
     - W m-2
     - all
     - ground fire latent heat flux
     - dynamic
   * - ``fire_area``
     - 1
     - all
     - fire-area fraction within cell
     - dynamic
   * - ``fuel_frac_burnt_dt``
     - 1
     - all
     - fuel fraction burned during current fire timestep
     - dynamic
   * - ``fuel_frac``
     - 1
     - all
     - remaining fuel fraction
     - dynamic
   * - ``emis_smoke``
     - kg m-2
     - all
     - fire particulate emissions per cell area during current timestep
     - dynamic
   * - ``fire_t2``
     - K
     - atmospheric
     - air temperature at 2 m
     - dynamic
   * - ``fire_q2``
     - kg kg-1
     - atmospheric
     - water-vapor mixing ratio at 2 m (legacy variable name)
     - dynamic
   * - ``fire_psfc``
     - Pa
     - atmospheric
     - surface air pressure
     - dynamic
   * - ``fire_rain``
     - mm [1]_
     - atmospheric
     - standalone accumulated precipitation; coupled-driver units unresolved
     - dynamic
   * - ``fz0``
     - m
     - atmospheric
     - surface roughness length
     - static [2]_
   * - ``fmc_g``
     - kg kg-1
     - all
     - ground fuel moisture content
     - dynamic
   * - ``uf``
     - m s-1
     - all
     - eastward wind used by fire spread
     - dynamic
   * - ``vf``
     - m s-1
     - all
     - northward wind used by fire spread
     - dynamic
   * - ``zsf``
     - m
     - all
     - fire-grid terrain height
     - static
   * - ``lfn``
     - m
     - all
     - signed level-set distance to fire perimeter
     - dynamic
   * - ``nfuel_cat``
     - 1
     - all
     - fuel category identifier
     - static
   * - ``grad_norm_ls``
     - 1
     - ``output_level > 0``
     - level-set gradient norm used during propagation
     - dynamic
   * - ``grad_norm_reinit``
     - 1
     - ``output_level > 0``
     - level-set gradient norm used during reinitialization
     - dynamic

.. [1] The standalone WRF reader sums ``RAINC`` and ``RAINNC``, which WRF
   stores as accumulated millimetres.  NUOPC rainfall units and accumulation
   semantics are not yet unified, so this attribute describes standalone
   output only and is not a shared-driver contract.
.. [2] ``fz0`` is static in the current regression cases.  The standalone
   reader nevertheless reloads and interpolates ``ZNT`` at every atmospheric
   update and does not assume temporal constancy.

This limited metadata contract does not declare CF compliance and does not
assign a CF ``standard_name`` to ``fire_q2``.
