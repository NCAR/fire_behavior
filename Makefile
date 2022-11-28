
FC=gfortran
#FCFLAGS=-ffree-form -std=f2008
FCFLAGS=-std=f2008 -ffree-form -fbacktrace -fcheck=all -finit-real=nan -fall-intrinsics -Wall -g -Og
CPP=cpp
CPPFLAGS=-nostdinc -C -P -w

DIR_FIRE_BEHAVIOR = $(PWD)
ESMF_INCLUDE = -I$(DIR_FIRE_BEHAVIOR)/vendors/esmf/DEFAULTINSTALLDIR/mod/modO/Linux.gfortran.64.mpiuni.default

export FC FCFLAGS CPP CPPFLAGS ESMF_INCLUDE

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
