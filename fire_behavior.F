  program fire_behavior

    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, INPUT_UNIT

    use wrf_atmosphere_mod, only : domain, grid_config_rec_type
    use wrf_fire_test1_mod, only : Set_wrf_fire_test1
    use module_fr_fire_driver_wrf, only : fire_driver_em_init, fire_driver_em_step

    implicit none

    integer, parameter :: CASE_WRF_FIRE_TEST1 = 1
    type (domain) :: grid
    type (grid_config_rec_type) :: config_flags
    integer :: case_to_run
    logical, parameter :: DEBUG = .true.


    write (OUTPUT_UNIT, *) 'Please, enter case to run:'
    write (OUTPUT_UNIT, *) '1. WRF-Fire test1'
    read (INPUT_UNIT, *) case_to_run

      ! Loading default settings
    call config_flags%Set_default ()
    if (DEBUG) then
      write (OUTPUT_UNIT, *) 'Contents of config_flags:'
      call config_flags%Print ()
    end if

    call grid%Set_default ()
    if (DEBUG) then
      write (OUTPUT_UNIT, *) ''
      write (OUTPUT_UNIT, *) 'Contents of grid:'
      call grid%Print ()
    end if

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

      ! initialize fire behavior
!    call fire_driver_em_init (grid , config_flags               & 
!                ,ids,ide, kds,kde, jds,jde                              &
!                ,ims,ime, kms,kme, jms,jme                              &
!                ,ips,ipe, kps,kpe, jps,jpe)


        ! Run fire behavior
!     call fire_driver_em_step (grid , config_flags               & 
!            ,ids,ide, kds,kde, jds,jde                              &
!            ,ims,ime, kms,kme, jms,jme                              &
!            ,ips,ipe, kps,kpe, jps,jpe                              &
!            ,rho,z_at_w,dz8w )

        ! write output or check something

  end program fire_behavior
