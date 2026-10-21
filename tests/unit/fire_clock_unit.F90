program fire_clock_unit

  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fire_clock_mod, only : fire_clock_t, Event_due, Next_event_step
  use datetime_mod, only : datetime_t
#ifdef DM_PARALLEL
  use mpi
#endif
  implicit none
  type(fire_clock_t) :: clock
  type(datetime_t) :: origin, now, expected
  integer(int64) :: next_output, next_exchange
  integer :: scenario_index, step, output_interval, exchange_interval, stop_step, outputs, resets, ierr
  real :: dt, flux, sum_flux
  real, parameter :: timesteps(*) = [0.3, 2.0/9.0, 5.0/18.0, 4.0/15.0, 10.0/27.0, 0.222222, 0.111111]
  character(len=40) :: scenario

#ifdef DM_PARALLEL
  call MPI_Init (ierr)
#endif
  call get_command_argument (1, scenario)
  select case (trim (scenario))
    case ('unaligned')
      call clock%Init (0.3, [4, 6])
      error stop 'unaligned clock accepted'
    case ('final')
      call clock%Init (0.3, [60, 30])
      step = clock%Step_at (604801_int64, 'Final time')
      error stop 'unaligned final time accepted'
    case ('missed')
      next_output = 10
      if (Event_due (11, next_output, 10)) error stop 'missed event accepted'
    case ('valid')
      origin = datetime_t (2020, 2, 28, 23, 59, 0)
      do scenario_index = 1, size (timesteps)
        dt = timesteps(scenario_index)
        call clock%Init (dt, [60, 30, 90])
        output_interval = clock%Step_at (60_int64, 'output')
        exchange_interval = clock%Step_at (30_int64, 'exchange')
        stop_step = clock%Step_at (604800_int64, 'Final time')
        next_output = output_interval
        next_exchange = 0
        outputs = 0
        resets = 0
        flux = 0.0
        sum_flux = 0.0
        do step = 0, stop_step
          ! NUOPC resets before an advance, after exporting the previous mean.
          if (Event_due (step, next_exchange, exchange_interval)) then
            if (step > 0 .and. abs (flux - 7.0) > 3.0e-5) error stop 'incorrect exchange mean'
            resets = resets + 1
            flux = 0.0
          end if
          if (Event_due (step, next_output, output_interval)) then
            outputs = outputs + 1
            now = clock%Datetime_at (origin, int (step, int64))
            expected = origin
            call expected%Add_seconds (int (outputs, int64) * 60)
            if (now /= expected) error stop 'event timestamp drifted'
          end if
          ! Restore schedule phase between output and exchange events.
          if (step == exchange_interval + 1) then
            next_output = Next_event_step (step, output_interval)
            next_exchange = Next_event_step (step, exchange_interval)
          end if
          flux = flux + 7.0 / exchange_interval
        end do
        if (outputs /= 10080 .or. resets /= 20161) error stop 'wrong event count'
        now = clock%Datetime_at (origin, int (stop_step, int64))
        if (now%datetime /= '2020-03-06_23:59:00') error stop 'calendar rollover failed'
        if (clock%Elapsed (stop_step) /= 604800.0_real64) error stop 'long elapsed time drifted'
        if (abs (clock%Elapsed (stop_step) - clock%Elapsed (stop_step-1) - &
            real (clock%seconds, real64) / clock%steps) > 1.0e-9_real64) error stop 'clock lost subsecond resolution'
      end do
      ! The 300 s cadence reported for dt=0.7 is not aligned; 7 s is aligned.
      call clock%Init (0.7, [7, 14, 21])
      if (clock%Step_at (21_int64, 'output') /= 30) error stop '0.7 s clock failed'
      print *, 'fire clock checks passed'
    case default
      error stop 'unknown fire clock scenario'
  end select
#ifdef DM_PARALLEL
  call MPI_Finalize (ierr)
#endif
end program fire_clock_unit
