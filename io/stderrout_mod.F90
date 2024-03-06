  module stderrout_mod

    use, intrinsic :: iso_fortran_env, only : ERROR_UNIT, OUTPUT_UNIT

    implicit none

    private

    public :: Print_message, Stop_simulation

  contains

    subroutine Print_message (msg)

      implicit none

      character (len = *), intent (in) :: msg


      write (OUTPUT_UNIT, *) trim (msg)

    end subroutine Print_message

    subroutine Stop_simulation (msg)

      implicit none

      character (len = *), intent (in) :: msg


      write (ERROR_UNIT, *) 'STOP:' // trim (msg)
      stop

    end subroutine Stop_simulation

  end module stderrout_mod
