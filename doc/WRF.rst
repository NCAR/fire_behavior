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
  ``namelist.fire`` is not used for these settings in direct WRF coupling.

Set ``dt`` in the ``&time`` section of ``namelist.cfbm`` to match WRF's time
step for the fire domain. WRF supplies the start and end dates to CFBM and
controls atmospheric updates, so ``interval_atm`` is unused in this mode and
the ``&atm`` section may be empty. WRF v4.8.0 supports CFBM in one domain per
simulation and requires a Lambert conformal projection.
