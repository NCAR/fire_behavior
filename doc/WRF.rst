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

Step 1: Pull the fire_behavior external
=======================================

The CFBM source is brought into the WRF tree under ``phys/fire_behavior`` using
WRF's external-management tooling, followed by a submodule update:

.. code-block:: console

   # Clear the placeholder directory before checking out the external
   rm -rf phys/fire_behavior

   ./tools/manage_externals/checkout_externals -e arch/OptionalExternals_cfbm.cfg

   # Initialize submodules
   git submodule update --init --recursive

You can confirm the versions that were checked out with:

.. code-block:: console

   # WRF commit
   git rev-parse --short HEAD

   # CFBM (fire_behavior) commit
   git -C phys/fire_behavior rev-parse --short HEAD

.. note::

   The ``fire_behavior`` version is pinned by
   ``arch/OptionalExternals_cfbm.cfg``. Use the commands above to see the exact
   WRF and CFBM commits your checkout contains.

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

Build the coupled executable:

.. code-block:: console

   set -o pipefail
   ./compile_new -j 8 2>&1 | tee compile.log

On success the coupled ``real.exe`` and ``wrf.exe`` executables are produced in
the build directory, and ``compile.log`` holds the full build output.

Running WRF-CFBM
================

A coupled WRF-CFBM run follows the standard WRF real-data workflow (WPS,
``real.exe``, ``wrf.exe``), with the fire model enabled through the fire options
in ``namelist.input``. The fire configuration mirrors the options documented in
:ref:`namelist`.
