.. _WRF:

==================================================
Coupling to WRF: Building and Running WRF-CFBM
==================================================

The CFBM can be built directly into the Weather Research and Forecasting (WRF)
model as an in-line physics component of the Advanced Research WRF (ARW) core.
The result is a single coupled ``WRF-CFBM`` executable in which WRF supplies the
near-surface atmospheric state to the fire model and the fire model returns heat,
moisture, and smoke fluxes to WRF at every time step.

This differs from the two other ways of running the model documented in this
guide:

* :ref:`WRF_data` describes running the CFBM **offline**, driven by precomputed
  WRF output rather than coupled to a live WRF integration.
* :ref:`SRW` describes coupling the CFBM to the **UFS** atmospheric component.

Prerequisites
=============

* A C, C++, and Fortran compiler toolchain (GNU is used in the examples below).
* An MPI library, plus NetCDF and HDF5.
* CMake (3.x).
* Git, with access to the WRF and CFBM repositories.

The module commands below use the environment available on NSF NCAR's Derecho
system. The specific module names and versions are site- and time-dependent;
adapt them to your platform.

Step 0: Clone WRF
=================

Clone WRF and check out the release used for CFBM coupling:

.. code-block:: console

   git clone git@github.com:wrf-model/WRF.git
   cd WRF
   git checkout release-v4.8.0

Step 1: Initialize the Git submodules
========================================

After checking out the WRF release, initialize its Git submodules. This retrieves
the CFBM source under ``phys/fire_behavior`` along with WRF's other submodules:

.. code-block:: console

   git submodule update --init --recursive

You can confirm the versions that were checked out with:

.. code-block:: console

   # WRF commit
   git rev-parse --short HEAD

   # CFBM (fire_behavior) commit
   git -C phys/fire_behavior rev-parse --short HEAD

.. note::

   The WRF release records the ``fire_behavior`` submodule commit;
   ``git submodule update`` checks out that recorded version. The version
   commands above report the exact WRF and CFBM commits in your checkout.

Step 2: Configure
=================

Load the build environment (Derecho / GNU example):

.. code-block:: console

   module --force purge
   module load ncarenv/24.12
   module load gcc/12.4.0
   module load craype/2.7.31
   module load ncarcompilers/1.0.0
   module load cray-mpich/8.1.29
   module load hdf5/1.12.3
   module load netcdf/4.9.2
   module load cmake/3.26.6

Configure the WRF build with the CFBM enabled:

.. code-block:: console

   ./configure_new -p GNU -x -- \
       -DENABLE_CFBM=ON \
       -DWRF_CORE=ARW \
       -DWRF_NESTING=BASIC \
       -DWRF_CASE=EM_REAL \
       -DUSE_MPI=ON \
       -DUSE_OPENMP=OFF

The key option is ``-DENABLE_CFBM=ON``, which builds and links the CFBM into
WRF. The remaining options select the ARW core, basic nesting, and the real-data
(``EM_REAL``) case, and enable MPI.

Step 3: Compile
===============

Build the coupled executable using Bash:

.. code-block:: console

   # Optional: report build failures even when tee succeeds.
   # set -o pipefail
   ./compile_new -j 8 2>&1 | tee compile.log

On success the coupled ``real.exe`` and ``wrf.exe`` executables are linked in
the ``install/run`` directory, and ``compile.log`` holds the full build output.

Running WRF-CFBM
================

A coupled WRF-CFBM run follows the standard WRF real-data workflow (WPS,
``real.exe``, ``wrf.exe``). For the WRF v4.8.0 release used above, configure two
namelist files in the run directory:

* In WRF's ``namelist.input``, set ``ifire = 1`` in the ``&fire`` section for
  the domain where CFBM is active. WRF's domain, physics, and simulation timing
  settings also belong in this file.
* Put the CFBM options described in :ref:`namelist` in a separate
  ``namelist.cfbm``, with ``&time``, ``&atm``, and ``&fire`` sections. This is
  the filename WRF reads when CFBM is enabled; the standalone filename
  ``namelist.fire`` is not used for these settings in WRF-CFBM runs.

Set ``dt`` in the ``&time`` section of ``namelist.cfbm`` to match WRF's time
step for the fire domain. A mismatch stops initialization; CFBM does not use
an independent time step within WRF. WRF supplies the start and end dates to
CFBM and controls atmospheric updates, so ``interval_atm`` is unused in this
mode and the ``&atm`` section may be empty. WRF v4.8.0 supports CFBM in one
domain per simulation and requires a Lambert conformal projection.

.. _wrf_configuration_ownership:

Timing and configuration ownership
----------------------------------

In WRF-CFBM runs, the timing and tiling controls below have different
owners. WRF overrides apply to the in-memory CFBM configuration; they do not
rewrite ``namelist.cfbm``.

.. list-table:: Timing, restart, and tiling settings in WRF-CFBM runs
   :header-rows: 1
   :widths: 23 12 35 30

   * - CFBM setting
     - Type
     - Coupled behavior
     - What to put in ``namelist.cfbm``
   * - ``start_year``, ``start_month``, ``start_day``, ``start_hour``,
       ``start_minute``, ``start_second``
     - Integer
     - Replaced by WRF's start-date settings before fire initialization.
     - Leave the default ``-1`` values or omit these fields from ``&time``.
   * - ``end_year``, ``end_month``, ``end_day``, ``end_hour``,
       ``end_minute``, ``end_second``
     - Integer
     - Replaced by WRF's end-date settings before fire initialization.
     - Leave the default ``-1`` values or omit these fields from ``&time``.
   * - ``restart``
     - Logical
     - Replaced by WRF's restart flag.
     - Set ``restart`` in WRF's ``namelist.input``.
   * - ``restart_interval``
     - Integer
     - WRF's restart alarm controls paired atmosphere/fire checkpoint writes.
     - Leave the default ``-1``; other values are rejected in WRF-CFBM runs.
   * - ``dt``
     - Real, seconds
     - Must equal WRF's time step for the fire domain. A mismatch stops
       initialization; CFBM does not use an independent time step.
     - Use a positive value matching that domain's WRF time step.
   * - ``interval_atm``
     - Integer, seconds
     - Unused for scheduling: WRF supplies fields at each coupled step.
       Explicit positive values still undergo CFBM's interval validation.
     - Omit it from ``&atm``; the section may be empty.
   * - ``interval_output``
     - Integer, seconds
     - Remains under CFBM control and sets fire-output frequency.
     - Set a positive interval that is an integer multiple of ``dt``.
   * - ``num_tiles`` and ``tile_strategy``
     - Integer
     - Supplied by the CFBM input and required to match WRF's tiling.
     - Match WRF's ``numtiles`` and ``tile_strategy`` settings.

Restart selection and checkpoint timing
---------------------------------------

WRF controls restart selection through ``restart`` in ``namelist.input``.
Before initializing the fire state, WRF replaces the CFBM ``restart`` flag
with that value. When resuming a coupled run, the CFBM flag may remain at its
default ``.false.``; it does not need to be set separately. A CFBM request for
``restart = .true.`` with WRF ``restart = .false.`` is rejected because a
coupled restart requires both atmosphere and fire checkpoint files.

In the ``&time`` section of ``namelist.cfbm``, leave ``restart_interval = -1``
(the default). Other values are rejected in WRF-CFBM runs. Here ``-1``
disables only the independent CFBM checkpoint schedule: fire checkpoints are
still written by CFBM when WRF's restart alarm fires, alongside WRF's
atmospheric checkpoint. Set the checkpoint frequency in WRF's
``namelist.input`` instead. WRF's legacy ``restart_interval`` is in
minutes. A nonzero ``restart_interval_m`` takes precedence over that legacy
minute value. WRF combines its day, hour, selected minute, and second components
to construct the alarm interval. These values are not copied into CFBM's
seconds-based interval.

A startup message on the monitor rank reports the effective restart flag and
WRF's ownership of checkpoint timing. To resume, provide matching
``wrfrst_d01_<time>`` and ``fire_restart_<time>.nc`` files and set WRF's start
date to that checkpoint time.

.. _wrf_wind_interpolation:

Wind forcing and interpolation
------------------------------

Set ``wind_vinterp_opt`` and ``hinterp_opt`` in the ``&fire`` section of
``namelist.cfbm``. The default ``wind_vinterp_opt = 0`` uses 3D wind profiles.
Setting ``wind_vinterp_opt = 1`` uses WRF's ``U10`` and ``V10`` winds,
maps them horizontally to the fire grid, and applies fuel-dependent
wind adjustment factors (WAFs). WAFs are applied only with the 10 m
wind option. The currently supported fuel model is Anderson 13;
Scott--Burgan input categories are converted to Anderson categories rather
than used with native Scott--Burgan WAFs. Other ``fuel_opt`` values are
rejected during initialization. ``hinterp_opt = 1`` selects
nearest-neighbor mapping; ``hinterp_opt = 2`` selects bilinear mapping and is
the default.

For ``wind_vinterp_opt = 0``, WRF supplies the destaggered mass-grid wind
profiles ``u_phy`` and ``v_phy`` and the interface heights ``z_at_w``. CFBM
first maps these profiles horizontally to the fire grid at each model level,
using ``hinterp_opt`` for both wind components and the height geometry. It
then samples the mapped profiles vertically using the fire-grid roughness
``fz0``. In the current coupling, ``fz0`` is obtained by interpolating WRF's
``ZNT`` with the same horizontal interpolation option.

The physical height of each mapped wind level is calculated from its bounding
interface heights relative to the mapped atmospheric surface, represented by
the lowest ``z_at_w`` interface. Vertical interpolation uses a logarithmic
profile to sample at ``fire_wind_height``. If ``fire_lsm_zcoupling`` is enabled,
profiles are sampled at ``fire_lsm_zcoupling_ref`` first, followed by a
logarithmic adjustment to the fire wind height.

For example, use 3D profiles with bilinear horizontal mapping by setting:

.. code-block:: fortran

   &fire
     wind_vinterp_opt = 0,
     hinterp_opt = 2,
   /
