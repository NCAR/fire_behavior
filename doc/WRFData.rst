.. _WRF_data:

=========================================
Running Offline Simulations with WRF Data
=========================================

The CFBM can run in offline mode using WRF atmospheric fields as input. To run CFBM in the offline mode, users need to provide WRF atmospheric data, including all available timestamps, in a file named ``wrf.nc``, along with static inputs in ``geo_em.d01.nc``. The atmospheric data must include wind components (U, V), geopotential heights (PH, PHB), surface variables used with the fuel moisture model (RAINC, RAINNC, T2, Q2, PSFC), and roughness length (ZNT).

The simulation configuration is set up in the ``namelist.fire`` file, as described in :ref:`namelist`. Users must specify the number of vertical levels (``kde``) and the time interval for incoming atmospheric data (``interval_atm``) to match the data provided in the ``wrf.nc`` file.

Wind processing depends on ``wind_vinterp_opt``:

* ``0`` (``VINTERP_WINDS_FROM_3D_WINDS``) reads and destaggers the 3D
  ``U``/``V`` profiles, maps wind and geopotential levels horizontally to the
  fire grid, then samples vertically using the mapped roughness and configured
  fire-wind height. The temporary profiles are released after interpolation.
* ``1`` (``VINTERP_WINDS_FROM_10M_WINDS``) requires ``U10`` and ``V10``. These
  fields are mapped directly to the fire grid, then multiplied by the
  fuel-dependent wind adjustment factor. The input arrays are released afterward.

Both methods retain the resulting fire winds in ``state_fire_t%uf`` and
``state_fire_t%vf``, which are used for spread calculations and NetCDF output.
The atmospheric reader does not retain duplicate ``ua``/``va`` arrays.
Unsupported wind options stop with an explicit diagnostic.

Users can obtain the model from the GitHub repository:

.. code-block:: console

   git clone https://github.com/NCAR/fire_behavior.git

To compile the code on Derecho, run:

.. code-block:: console

   ./compile.sh --env-auto

See :ref:`building` for the full set of build options. If the compilation is successful, the model can be run using ``fire_behavior.exe`` located in the ``build`` directory.

An example is provided in the ``tests/test7/`` directory with the ``namelist.fire``.

If the simulation is successful, the model outputs are written to files named ``fire_output_*``, and diagnostic messages are printed to standard output (redirect them to keep a copy, for example ``fire_behavior.exe > log 2>&1``).
