require("posix")

-- This module loads modules for fire_module on cheyenne

-- set family
family("fire_module")

-- The message printed by the module whatis command
whatis("Fire Module Intel compiler on Cheyenne")

-- The message printed by the module help command
help([[
This module loads the environment for cheyenne.
]])

-- load modules
unload("netcdf")
load("ncarenv/1.3")
load("intel/19.1.1")
load("ncarcompilers/0.5.0")
load("mpt/2.25")
prepend_path("MODULEPATH","/glade/p/cesmdata/cseg/PROGS/modulefiles/esmfpkgs/intel/19.1.1/")
load("esmf-8.4.0b14-ncdfio-mpt-g")
load("netcdf-mpi/4.8.1")
load("pnetcdf/1.12.2")
load("pio/2.5.8")
load("cmake/3.18.2")
--load("python/3.7.9")
