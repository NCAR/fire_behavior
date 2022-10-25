  program fire_behavior

    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, INPUT_UNIT

    use wrf_atmosphere_mod, only : domain
    use namelist_mod, only : namelist_t
    use wrf_fire_test1_mod, only : Set_wrf_fire_test1, n_steps_test1, read_wrf_input_test1, check_tends_test1
    use wrf_fire_test2_mod, only : Set_wrf_fire_test2, n_steps_test2, read_wrf_input_test2, check_tends_test2
    use wrf_fire_test3_mod, only : Set_wrf_fire_test3, n_steps_test3, read_wrf_input_test3, check_tends_test3
    use wrf_fire_test4_mod, only : Set_wrf_fire_test4, n_steps_test4, read_wrf_input_test4, check_tends_test4
    use module_fr_fire_driver_wrf, only : fire_driver_em_init, fire_driver_em_step

    implicit none

    integer, parameter :: CASE_WRF_FIRE_TEST1 = 1, CASE_WRF_FIRE_TEST2 = 2, CASE_WRF_FIRE_TEST3 = 3, CASE_WRF_FIRE_TEST4 = 4
    type (domain) :: grid
    type (namelist_t) :: config_flags
    integer :: case_to_run, n, j, n_steps, wrf_input_unit
    logical, parameter :: DEBUG = .true., WRITE_OUTPUT = .false.
    real :: check_val
    logical :: read_wrf_input, check_tends


    write (OUTPUT_UNIT, *) 'Please, enter case to run:'
    write (OUTPUT_UNIT, *) '1. WRF-Fire test1 (laminar offline)'
    write (OUTPUT_UNIT, *) '2. WRF-Fire test2 (laminar with WRF input)'
    write (OUTPUT_UNIT, *) '3. WRF-Fire test3 (coupled atm-fire with WRF input)'
    write (OUTPUT_UNIT, *) '4. WRF-Fire test4 (Real case with WRF input)'
    read (INPUT_UNIT, *) case_to_run

    select_case_to_run: select case (case_to_run)
      case (CASE_WRF_FIRE_TEST1)
        if (DEBUG) then
          write (OUTPUT_UNIT, *) ''
          write (OUTPUT_UNIT, *) 'Loading grid/config for WRF-Fire test1'
        end if
        call Set_wrf_fire_test1 (grid, config_flags)
        n_steps = n_steps_test1
        read_wrf_input = read_wrf_input_test1
        check_tends = check_tends_test1

      case (CASE_WRF_FIRE_TEST2)
        if (DEBUG) then
          write (OUTPUT_UNIT, *) ''
          write (OUTPUT_UNIT, *) 'Loading grid/config for WRF-Fire test2'
        end if
        call Set_wrf_fire_test2 (grid, config_flags)
        n_steps = n_steps_test2
        read_wrf_input = read_wrf_input_test2
        check_tends = check_tends_test2

      case (CASE_WRF_FIRE_TEST3)
        if (DEBUG) then
          write (OUTPUT_UNIT, *) ''
          write (OUTPUT_UNIT, *) 'Loading grid/config for WRF-Fire test3'
        end if
        call Set_wrf_fire_test3 (grid, config_flags)
        n_steps = n_steps_test3
        read_wrf_input = read_wrf_input_test3
        check_tends = check_tends_test3

      case (CASE_WRF_FIRE_TEST4)
        if (DEBUG) then
          write (OUTPUT_UNIT, *) ''
          write (OUTPUT_UNIT, *) 'Loading grid/config for WRF-Fire test4'
        end if
        call Set_wrf_fire_test4 (grid, config_flags)
        n_steps = n_steps_test4
        read_wrf_input = read_wrf_input_test4
        check_tends = check_tends_test4

      case default
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) 'Error: invalid option for case to run entered'
        stop
    end select select_case_to_run

    call Init_fire_behavior_model ()

    call Run_fire_behavior_model ()

  contains

    subroutine Init_fire_behavior_model ()

      implicit none


      if (DEBUG) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) 'Initialization of the fire behavior model...'
      end if

      call fire_driver_em_init (grid , config_flags                          & 
                ,grid%ids, grid%ide, grid%kds, grid%kde, grid%jds, grid%jde  &
                ,grid%ims, grid%ime, grid%kms, grid%kme, grid%jms, grid%jme  &
                ,grid%ips, grid%ipe, grid%kps, grid%kpe, grid%jps, grid%jpe)

    end subroutine Init_fire_behavior_model

    subroutine Run_fire_behavior_model ()

      implicit none

      integer :: io_stat

      if (DEBUG) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) 'Run the fire behavior model... '
      end if

      if (read_wrf_input) then
        open (newunit = wrf_input_unit, file = 'wrf_input.dat', iostat = io_stat)
        if (io_stat /= 0) then
          write (OUTPUT_UNIT, *) 'Problems opening the wrf_input.dat file'
          stop
        end if
      end if
      do n = 1, n_steps
        grid%itimestep = n
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
        if (DEBUG) write (OUTPUT_UNIT, *) 'Completed time step ', n
        if (WRITE_OUTPUT) then
          do j = grid%jfde - 150, grid%jfds + 150, -1
            write (OUTPUT_UNIT, '(110(f3.1,1x))') grid%fire_area(150:200, j)
          end do
        end if
      end do
      if (read_wrf_input) close (wrf_input_unit)

    end subroutine Run_fire_behavior_model

  end program fire_behavior
