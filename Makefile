
FC=gfortran
#FCFLAGS=-ffree-form -std=f2008
FCFLAGS=-std=f2008 -ffree-form -fbacktrace -fcheck=all -finit-real=nan -fall-intrinsics -Wall -g -Og
CPP=cpp
CPPFLAGS=-nostdinc -C -P -w

#FC=ifort
#FCFLAGS_UNFORM= -O2 -stand f08
#FCFLAGS=$(FCFLAGS_UNFORM) -FR
#CC=icc

DIR_FIRE_BEHAVIOR = $(PWD)
ESMFMKFILE = $(DIR_FIRE_BEHAVIOR)/vendors/esmf/DEFAULTINSTALLDIR/lib/libO/Linux.gfortran.64.mpiuni.default/esmf.mk

NETCDF_DIR := $(DIR_FIRE_BEHAVIOR)/vendors/netcdf-gnu
#NETCDF_DIR := $(DIR_FIRE_BEHAVIOR)/vendors/netcdf-intel
NETCDF_LIBS=-L$(NETCDF_DIR)/lib -lnetcdff -lnetcdf -lhdf5_hl -lhdf5 -lm -lz -ldl -lbz2 -lxml2 -lcurl
NETCDF_INCLUDE="-I$(NETCDF_DIR)/include"

export FC FCFLAGS CPP CPPFLAGS ESMFMKFILE NETCDF_LIBS NETCDF_INCLUDE

SUBDIRS = physics state io driver nuopc
     
.PHONY: clean subdirs $(SUBDIRS)
     
subdirs: $(SUBDIRS)
     
$(SUBDIRS):
	$(MAKE) -C $@

clean:
	@ for dir in $(SUBDIRS); do \
	  $(MAKE) -C $$dir $@; \
	done

  # Dependencies
state: io

physics: io state

driver: io state physics

nuopc: io state driver
