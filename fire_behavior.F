  program fire_behavior

    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, INPUT_UNIT

    use wrf_atmosphere_mod, only : domain, grid_config_rec_type
    use wrf_fire_test1_mod, only : Set_wrf_fire_test1, n_steps
    use module_fr_fire_driver_wrf, only : fire_driver_em_init, fire_driver_em_step

    implicit none

    integer, parameter :: CASE_WRF_FIRE_TEST1 = 1
    type (domain) :: grid
    type (grid_config_rec_type) :: config_flags
    integer :: case_to_run, n, j
    logical, parameter :: DEBUG = .true., WRITE_OUTPUT = .false.


    write (OUTPUT_UNIT, *) 'Please, enter case to run:'
    write (OUTPUT_UNIT, *) '1. WRF-Fire test1'
    read (INPUT_UNIT, *) case_to_run

    select_case_to_run: select case (case_to_run)
      case (CASE_WRF_FIRE_TEST1)
          ! Loading test1
        if (DEBUG) then
          write (OUTPUT_UNIT, *) ''
          write (OUTPUT_UNIT, *) 'Loading grid/config for WRF-Fire test1'
        end if
        call Set_wrf_fire_test1 (grid, config_flags)

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


      if (DEBUG) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) 'Run the fire behavior model... '
      end if

      do n = 1, n_steps
        grid%itimestep = n
          ! Run one step
        call fire_driver_em_step (grid , config_flags & 
              ,grid%ids,grid%ide, grid%kds, grid%kde, grid%jds, grid%jde  &
              ,grid%ims, grid%ime, grid%kms, grid%kme, grid%jms, grid%jme &
              ,grid%ips, grid%ipe, grid%kps, grid%kpe, grid%jps, grid%jpe &
              ,grid%rho, grid%z_at_w, grid%dz8w)

          ! write output if necessary
        if (DEBUG) write (OUTPUT_UNIT, *) 'Completed time step ', n
        if (WRITE_OUTPUT) then
          do j = grid%jfde - 150, grid%jfds + 150, -1
            write (OUTPUT_UNIT, '(110(f3.1,1x))') grid%fire_area(150:200, j)
          end do
        end if
      end do

    end subroutine Run_fire_behavior_model

  end program fire_behavior
