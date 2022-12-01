
FC=gfortran
#FCFLAGS=-ffree-form -std=f2008
FCFLAGS=-std=f2008 -ffree-form -fbacktrace -fcheck=all -finit-real=nan -fall-intrinsics -Wall -g -Og
CPP=cpp
CPPFLAGS=-nostdinc -C -P -w

DIR_FIRE_BEHAVIOR = $(PWD)
ESMFMKFILE = $(DIR_FIRE_BEHAVIOR)/vendors/esmf/DEFAULTINSTALLDIR/lib/libO/Linux.gfortran.64.mpiuni.default/esmf.mk

export FC FCFLAGS CPP CPPFLAGS ESMFMKFILE

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
