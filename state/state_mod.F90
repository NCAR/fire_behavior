  module state_mod

    use wrf_atmosphere_mod, only : domain
    use namelist_mod, only : namelist_t

    implicit none

    private

    public :: state_t

    type :: state_t
      real, dimension (:, :), allocatable :: lats, lons, elevations, dz_dxs, dz_dys, fuel_cats
      real :: dx, dy, cen_lat, cen_lon
      integer :: ids, ide, jds, jde
      type (domain) :: wrf_domain
    contains
      private
      procedure, public :: Initialize_state => Initialize_state
      procedure, public :: Advance_state => Advance_state
    end type state_t

  contains

    subroutine Advance_state (this)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (state_t), intent (inout) :: this

      logical, parameter :: DEBUG_LOCAL = .true.


      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Entering subroutinte Advance_state'

      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Leaving subroutinte Advance_state'

    end subroutine Advance_state

    subroutine Initialize_state (this)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (state_t), intent (inout) :: this

      type (namelist_t) :: nl
      logical, parameter :: DEBUG_LOCAL = .true.


      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Entering subroutinte Initialize_state'

        call nl%Read_namelist ()

      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Leaving subroutinte Initialize_state'

    end subroutine Initialize_state

  end module state_mod
