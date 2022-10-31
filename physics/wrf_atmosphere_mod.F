  module wrf_atmosphere_mod

    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

    use state_mod, only : domain

    implicit none

    private

    public :: CP, XLV, RERADIUS, G, PI2, Wrf_error_fatal, Wrf_message, Get_ijk_from_subgrid, Wrf_debug

    real, parameter :: R_D = 287.0                ! gas constant of dry air (J deg^-1 kg^-1)
    real, parameter :: CP = 7.0 * R_D / 2.0
    real, parameter :: RERADIUS = 1.0 / 6370.0e03 ! reciprocal of earth radius (m^-1)
    real, parameter :: XLV = 2.5E6                ! latent heat of vaporization of water at 0^oC (J kg^-1)
    real, parameter :: G = 9.81                   ! acceleration due to gravity (m {s}^-2)
    real, parameter :: PI2 = 2.0 * 3.1415926

  contains

    subroutine Get_ijk_from_subgrid (  grid ,                &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0    )

    ! return the values for subgrid whose refinement is in grid%sr
    ! note when using this routine, it does not affect K. For K 
    ! (vertical), it just returns what get_ijk_from_grid does

      type (domain), intent (in) :: grid
      integer, intent(out) ::                                 &
                       ids0, ide0, jds0, jde0, kds0, kde0,    &
                       ims0, ime0, jms0, jme0, kms0, kme0,    &
                       ips0, ipe0, jps0, jpe0, kps0, kpe0
        ! Local
      integer ::                              &
                ids, ide, jds, jde, kds, kde, &
                ims, ime, jms, jme, kms, kme, &
                ips, ipe, jps, jpe, kps, kpe


      ids0 = grid%ids
      ide0 = grid%ide * grid%sr_x
      ims0 = (grid%ims - 1) * grid%sr_x + 1
      ime0 = grid%ime * grid%sr_x
      ips0 = (grid%ips - 1) * grid%sr_x + 1
      ipe0 = grid%ipe * grid%sr_x

      jds0 = grid%jds
      jde0 = grid%jde * grid%sr_y
      jms0 = (grid%jms - 1) * grid%sr_y + 1
      jme0 = grid%jme * grid%sr_y
      jps0 = (grid%jps - 1) * grid%sr_y + 1
      jpe0 = grid%jpe * grid%sr_y

      kds0 = grid%kds
      kde0 = grid%kde
      kms0 = grid%kms
      kme0 = grid%kme
      kps0 = grid%kps
      kpe0 = grid%kpe

      return

    end subroutine Get_ijk_from_subgrid

    subroutine Wrf_debug (level , str)

      implicit none

      character (len = *) :: str
      integer, intent (in) :: level


      call wrf_message (str)

      return

    end subroutine wrf_debug

    subroutine wrf_error_fatal( str )

      implicit none

      character (len = *) :: str


      call Wrf_error_fatal3 (' ', 0, str)

    end subroutine wrf_error_fatal

    subroutine Wrf_error_fatal3 (file_str, line, str)

      implicit none

      character (len = *) :: file_str
      integer, intent (in) :: line  ! only print file and line if line > 0
      character (len = *) :: str

      character (len = 256) :: line_str


      write (line_str,'(i6)') line

        ! Fatal errors are printed to stdout and stderr regardless of
        ! any &logging namelist settings.
      call Wrf_message ('-------------- FATAL CALLED ---------------')
        ! only print file and line if line is positive
      if ( line > 0 ) then
        call wrf_message ('FATAL CALLED FROM FILE:  ' // file_str // '  LINE:  ' // trim (line_str))
      end if
      call Wrf_message (str)
      call Wrf_message ('-------------------------------------------')

      stop

    end subroutine wrf_error_fatal3

    subroutine Wrf_message (str)

      implicit none

      character (len = *) :: str

 300  format (A)
      print 300, trim(str)

    end subroutine wrf_message

  end module wrf_atmosphere_mod
