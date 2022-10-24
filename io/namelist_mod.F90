  module namelist_mod

    implicit none

    private

    public :: namelist_t

    type :: namelist_t
    contains
      private
      procedure, public :: Read_namelist => Read_namelist
    end type namelist_t

  contains

    subroutine Read_namelist (this)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (namelist_t), intent (in out) :: this

      logical, parameter :: DEBUG_LOCAL = .true.


      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Entering subroutine Read_namelist'

      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Leaving subroutine Read_namelist'

    end subroutine Read_namelist

  end module namelist_mod
