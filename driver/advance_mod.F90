  module advance_mod

    use state_mod, only : domain
    use namelist_mod, only : namelist_t
    use module_fr_fire_driver_wrf, only : fire_driver_em_step

    implicit none

    private

    public :: Advance_state

  contains

    subroutine Advance_state (grid, config_flags, read_wrf_input, check_tends)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      type (domain), intent (in out) :: grid
      type (namelist_t), intent (in out) :: config_flags
      logical, intent (in) :: read_wrf_input, check_tends

      logical, parameter :: DEBUG_LOCAL = .true., WRITE_OUTPUT = .false.
      real :: check_val
      integer :: io_stat, j, n, wrf_input_unit


      if (DEBUG_LOCAL) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) '  Entering Advance_state... '
      end if

      if (read_wrf_input) then
        open (newunit = wrf_input_unit, file = 'wrf_input.dat', iostat = io_stat)
        if (io_stat /= 0) then
          write (OUTPUT_UNIT, *) 'Problems opening the wrf_input.dat file'
          stop
        end if
      end if

      grid%itimestep = grid%itimestep + 1
          ! Run one step
      check_val = 0
      if (read_wrf_input) then
        read (wrf_input_unit, *, iostat = io_stat) grid%u_2
        if (io_stat /= 0) then
          write (OUTPUT_UNIT, *) 'Problems reading wrf_input.dat'
          stop
        end if

        read (wrf_input_unit, *) grid%v_2
        read (wrf_input_unit, *) grid%ph_2
        read (wrf_input_unit, *) grid%phb
        read (wrf_input_unit, *) grid%rho
        read (wrf_input_unit, *) grid%z_at_w
        read (wrf_input_unit, *) grid%dz8w
        read (wrf_input_unit, *) grid%z0
        read (wrf_input_unit, *) grid%mut
        read (wrf_input_unit, *) check_val
      end if
      print *, check_val
      call fire_driver_em_step (grid , config_flags & 
            ,grid%ids,grid%ide, grid%kds, grid%kde, grid%jds, grid%jde  &
            ,grid%ims, grid%ime, grid%kms, grid%kme, grid%jms, grid%jme &
            ,grid%ips, grid%ipe, grid%kps, grid%kpe, grid%jps, grid%jpe &
            ,grid%rho, grid%z_at_w, grid%dz8w)
      if (check_tends) write (34, *) grid%rthfrten(13, 2, 20), grid%rqvfrten(13, 2, 20)

        ! write output if necessary
      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'Completed time step ', n
      if (WRITE_OUTPUT) then
        do j = grid%jfde - 150, grid%jfds + 150, -1
          write (OUTPUT_UNIT, '(110(f3.1,1x))') grid%fire_area(150:200, j)
        end do
      end if

      if (read_wrf_input) close (wrf_input_unit)

      if (DEBUG_LOCAL) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) '  Leaving Advance_state... '
      end if

    end subroutine Advance_state

  end module advance_mod
