  module datetime_mod

    implicit none

    private

    public :: datetime_t

    integer, parameter :: DATETIME_LEN = 19

    type :: datetime_t 
      character (len = DATETIME_LEN) :: datetime
    end type datetime_t

    interface datetime_t
      module procedure Datetime_t_const
    end interface datetime_t

  contains

    pure function Datetime_t_const (yyyy, mm, dd, hh, minutes, ss) result (return_value)

      implicit none

      integer, intent (in) :: yyyy, mm, dd, hh, minutes, ss
      type (datetime_t) :: return_value


      write (return_value%datetime, "(i4, 5(a,i2.2))") yyyy, '-', mm, '-', dd, '_', hh, ':', minutes, ':', ss

    end function Datetime_t_const

  end module datetime_mod
