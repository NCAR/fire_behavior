.. _building:

========================================
Building and Installing the CFBM
========================================

This page describes how to build the **standalone** Community Fire Behavior
Module: the ``fire_behavior.exe`` executable that runs the fire model on its own,
either in idealized mode or driven offline by atmospheric data. The standalone
build uses CMake and is driven by the ``compile.sh`` wrapper script at the root
of the repository.

.. note::

   To build the CFBM coupled *inside* an atmospheric model instead, see
   :ref:`WRF` (WRF) or :ref:`SRW` (UFS). Those use their host model's build
   system rather than ``compile.sh``.

Getting the code
========================================

Clone the repository from GitHub:

.. code-block:: console

   git clone https://github.com/NCAR/fire_behavior.git
   cd fire_behavior

Prerequisites
========================================

Building the standalone CFBM requires:

* C, C++, and Fortran compilers (GNU is used in the provided environments).
* CMake 3.x.
* NetCDF with the Fortran interface.
* An MPI library, unless building with ``--mpi-off``.

ESMF is **not** required for the standalone model; it is needed only for the
NUOPC and ESMX builds.

On NSF NCAR's Derecho these dependencies are provided by preconfigured
environment files under ``env/`` and loaded automatically with ``--env-auto``.
Derecho (``env/derecho/gnu-12.2.0``) is the tested platform; the
``env/cheyenne/`` files are retained for reference only, as Cheyenne has been
decommissioned. To build elsewhere, copy an existing ``env/`` file, adjust its
modules and compiler settings, and pass it with ``--env-file``:

.. code-block:: console

   ./compile.sh --env-file=env/mysystem/mycompiler

Quick start
========================================

On Derecho, a default build is a single command:

.. code-block:: console

   ./compile.sh --env-auto

This loads the Derecho GNU environment, configures the project with CMake, and
builds and installs the model. On success the executable is produced at
``build/fire_behavior.exe`` and installed to ``install/bin/fire_behavior.exe``.

To rebuild from scratch, add ``--clean``, which removes the build directory
first.

``compile.sh`` options
========================================

``--env-auto``
   Load a preconfigured environment from ``env/<system>/`` based on the detected
   hostname.

``--env-file=ENV_FILE``
   Load the build environment from a specific file instead of auto-detection.

``--env-dir=ENV_DIR``
   Directory searched for environment files (default: ``env/``).

``--system=SYSTEM``
   Override the auto-detected system name used by ``--env-auto``.

``--build-dir=BUILD_DIR``
   Build directory (default: ``build/``).

``--build-type=BUILD_TYPE``
   CMake build type: ``debug``, ``release`` (default), or ``relWithDebInfo``.

``--build-jobs=BUILD_JOBS``
   Number of parallel build jobs.

``--prefix=INSTALL_PREFIX``
   Installation prefix (default: ``install/``).

``--mpi-off``
   Build without MPI. MPI is **on** by default.

``--openmp-on``
   Enable OpenMP threading. OpenMP is **off** by default.

``--nuopc``, ``-n``
   Build the NUOPC cap and library in addition to the standalone model.

``--esmx``, ``-x``
   Build the ESMX application (implies ``--nuopc``).

``--test[=TEST_NAME]``, ``-t[=TEST_NAME]``
   Run the test suite with ``ctest`` after building. An optional name restricts
   the run to matching tests.

``--verbose``, ``-v``
   Verbose build output.

``--clean``
   Delete the build directory before building.

``--help``, ``-h``
   Print usage and exit.

Parallelization: MPI and OpenMP
========================================

The CFBM supports distributed-memory (MPI) and shared-memory (OpenMP)
parallelism, selected at build time:

* **MPI** is enabled by default. Build without it using ``--mpi-off`` (useful for
  small idealized runs or debugging).
* **OpenMP** is disabled by default. Enable it with ``--openmp-on``.

At run time, OpenMP threading is applied over the tiles that each MPI patch is
divided into. The number of tiles is set by ``num_tiles`` in the namelist; see
:ref:`namelist`.

Coupled and ESMX builds
========================================

The same script builds the coupling infrastructure:

* ``--nuopc`` builds the NUOPC cap and library used to run the CFBM as a NUOPC
  component.
* ``--esmx`` additionally builds the ESMX application, which drives the CFBM as a
  standalone ESMF/NUOPC component.

For coupled forecasts with the UFS, see :ref:`SRW`.

Running the model
========================================

After building, run the standalone executable from a directory that contains a
``namelist.fire`` and the required input files:

.. code-block:: console

   cd <run-directory>
   /path/to/build/fire_behavior.exe

The run directory must contain a ``namelist.fire`` (see :ref:`namelist`). For
real-world runs it must also contain the static ``geo_em.d01.nc`` and the
atmospheric input; see :ref:`WRF_data` for offline runs driven by WRF data.
Idealized runs need no external atmospheric input; see :ref:`Idealized`. Model
output is written to ``fire_output_*`` files. Run-time diagnostics are printed to standard output; redirect them to keep a copy (for example ``fire_behavior.exe > log 2>&1``).
