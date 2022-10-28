  program fire_behavior

    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, INPUT_UNIT

    use state_mod, only : domain
    use namelist_mod, only : namelist_t
    use wrf_fire_test1_mod, only : Set_wrf_fire_test1, n_steps_test1, read_wrf_input_test1, check_tends_test1
    use wrf_fire_test2_mod, only : Set_wrf_fire_test2, n_steps_test2, read_wrf_input_test2, check_tends_test2
    use wrf_fire_test3_mod, only : Set_wrf_fire_test3, n_steps_test3, read_wrf_input_test3, check_tends_test3
    use wrf_fire_test4_mod, only : Set_wrf_fire_test4, n_steps_test4, read_wrf_input_test4, check_tends_test4
    use initialize_mod, only : Init_state
    use advance_mod, only : Advance_state

    implicit none

    integer, parameter :: CASE_WRF_FIRE_TEST1 = 1, CASE_WRF_FIRE_TEST2 = 2, CASE_WRF_FIRE_TEST3 = 3, CASE_WRF_FIRE_TEST4 = 4
    type (domain) :: grid
    type (namelist_t) :: config_flags
    integer :: case_to_run, n, n_steps
    logical, parameter :: DEBUG = .true., WRITE_OUTPUT = .false.
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

    call Init_state (grid, config_flags)

    do n = 1, n_steps
      call Advance_state (grid, config_flags, read_wrf_input, check_tends)
    end do

  end program fire_behavior
