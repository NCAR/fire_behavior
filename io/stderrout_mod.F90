  module stderrout_mod

    use, intrinsic :: iso_fortran_env, only : ERROR_UNIT, OUTPUT_UNIT

    implicit none

    private

    public :: Message, Crash, Stop_simulation

  contains

    subroutine Crash (msg)

      implicit none

      character (len = *), intent(in) :: msg


      write (ERROR_UNIT, *) trim ('Crash: ' // msg)
      stop

    end subroutine Crash

    subroutine Message (msg, fire_print_msg, level)

      implicit none

      character (len = *), intent(in) :: msg
      integer, intent(in) :: fire_print_msg
      integer, intent(in), optional :: level

      integer :: mlevel


      if (present (level)) then
        mlevel = level
      else
        mlevel = 2
      endif

      if (fire_print_msg >= mlevel) write (OUTPUT_UNIT, *) 'FIRE:' // trim (msg)

    end subroutine Message

    subroutine Stop_simulation (msg)

      implicit none

      character (len = *), intent (in) :: msg


      write (ERROR_UNIT, *) 'ERROR:' // trim (msg)
      stop

    end subroutine Stop_simulation

  end module stderrout_mod
