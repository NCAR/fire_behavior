module fire_clock_mod

  use, intrinsic :: iso_fortran_env, only : int64, real64
  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
  use datetime_mod, only : datetime_t
  use stderrout_mod, only : Stop_simulation

  implicit none
  private
  public :: fire_clock_t, Event_due, Next_event_step

  ! Six-significant-digit timestep inputs may differ by up to 5 ppm. Limit
  ! normalization to 0.01 step of the common interval to reject ambiguous clocks.
  real(real64), parameter :: DT_REL_TOL = 5.0e-6_real64, STEP_TOL_MAX = 0.01_real64

  type :: fire_clock_t
    ! Reduced rational seconds per fire step. Physics retains the input dt;
    ! these integers remove accumulated representation error from event times.
    integer :: seconds = 0, steps = 0
  contains
    procedure :: Init => Init_clock
    procedure :: Step_at
    procedure :: Datetime_at
    procedure :: Elapsed
  end type fire_clock_t

contains

  pure integer function Gcd (a, b) result (value)
    integer, intent(in) :: a, b
    integer :: other, remainder

    value = a
    other = b
    do while (other /= 0)
      remainder = mod (value, other)
      value = other
      other = remainder
    end do
  end function Gcd

  subroutine Init_clock (this, dt, intervals)
    class(fire_clock_t), intent(out) :: this
    real, intent(in) :: dt
    integer, intent(in) :: intervals(:)
    integer :: interval, common, divisor
    real(real64) :: ratio, tolerance

    if (.not. ieee_is_finite (dt)) call Stop_simulation ('dt must be finite and positive')
    if (dt <= 0.0) call Stop_simulation ('dt must be finite and positive')
    common = 0
    do interval = 1, size (intervals)
      if (intervals(interval) > 0) common = Gcd (common, intervals(interval))
    end do
    ! Partial configurations can validate dt without enabling any event.
    if (common == 0) return
    ratio = real (common, real64) / real (dt, real64)
    if (ratio > real (huge (this%steps), real64)) call Stop_simulation ('Fire schedule exceeds timestep counter range')
    tolerance = min (STEP_TOL_MAX, DT_REL_TOL * ratio)
    if (anint (ratio) < 1.0_real64 .or. abs (ratio - anint (ratio)) > tolerance) &
        call Stop_simulation ('Time intervals must be integer multiples of dt within the alignment tolerance')
    this%steps = nint (ratio)
    divisor = Gcd (common, this%steps)
    this%seconds = common / divisor
    this%steps = this%steps / divisor
  end subroutine Init_clock

  function Step_at (this, seconds, name) result (step)
    class(fire_clock_t), intent(in) :: this
    integer(int64), intent(in) :: seconds
    character(len=*), intent(in) :: name
    integer :: step
    integer(int64) :: count

    if (this%seconds <= 0 .or. this%steps <= 0) call Stop_simulation ('Fire clock requires a positive event interval')
    if (seconds < 0_int64) call Stop_simulation (trim (name)//' precedes the simulation origin')
    if (seconds > huge (count) / this%steps) call Stop_simulation (trim (name)//' exceeds clock range')
    count = seconds * this%steps
    if (mod (count, int (this%seconds, int64)) /= 0) &
        call Stop_simulation (trim (name)//' must align with a fire timestep')
    count = count / this%seconds
    if (count > huge (step)) call Stop_simulation (trim (name)//' exceeds timestep counter range')
    step = int (count)
  end function Step_at

  pure real(real64) function Elapsed (this, step) result (seconds)
    class(fire_clock_t), intent(in) :: this
    integer, intent(in) :: step

    seconds = real (int (step, int64) * this%seconds, real64) / this%steps
  end function Elapsed

  function Datetime_at (this, origin, step) result (datetime)
    class(fire_clock_t), intent(in) :: this
    type(datetime_t), intent(in) :: origin
    integer(int64), intent(in) :: step
    type(datetime_t) :: datetime
    integer(int64) :: numerator

    if (step < 0 .or. this%steps <= 0 .or. this%seconds <= 0) call Stop_simulation ('Invalid fire clock step')
    if (step > huge (numerator) / this%seconds) call Stop_simulation ('Fire clock exceeds integer range')
    numerator = step * this%seconds
    datetime = origin
    call datetime%Add_seconds (numerator / this%steps)
    ! Only the subsecond remainder is converted to real, independently of run length.
    datetime%fractional_seconds = real (mod (numerator, int (this%steps, int64)), real64) / this%steps
  end function Datetime_at

  pure integer(int64) function Next_event_step (step, interval) result (next)
    integer, intent(in) :: step, interval

    next = huge (next)
    if (interval > 0) next = (int (step, int64) / interval + 1_int64) * interval
  end function Next_event_step

  logical function Event_due (step, next, interval) result (due)
    integer, intent(in) :: step, interval
    integer(int64), intent(inout) :: next

    due = .false.
    if (interval <= 0) return
    ! A missed call is an error, not permission to skip atmospheric records.
    if (int (step, int64) > next) call Stop_simulation ('Missed scheduled fire event')
    due = int (step, int64) == next
    if (due) next = next + interval
  end function Event_due

end module fire_clock_mod
