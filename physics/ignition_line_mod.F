  module ignition_line_mod

    use namelist_mod, only: namelist_t

    implicit none

    private

    public :: ignition_line_type, Initialize_ignitions, ignition_line, fire_num_ignitions

    integer :: fire_num_ignitions
    integer, parameter :: FIRE_MAX_IGNITIONS = 5

    type ignition_line_type
      real  ros, &        ! subscale rate of spread during the ignition process
          stop_time, &    ! when the ignition process stops from ignition start (s)
          wind_red,  &    ! wind reduction factor at the ignition line
          wrdist,   &     ! distance from the ignition line when the wind reduction stops
          wrupwind, &     ! use distance interpolated between 0. = nearest 1. = upwind
          start_x, &      ! x coordinate of the ignition line start point (m, or long/lat)
          start_y, &      ! y coordinate of the ignition line start point
          end_x, &        ! x coordinate of the ignition line end point
          end_y, &        ! y coordinate of the ignition line end point
          start_time, &   ! ignition time for the start point from simulation start (s)
          end_time, &     ! ignition time for the end poin from simulation start (s)
          radius          ! all within this radius ignites immediately
    end type ignition_line_type

    type (ignition_line_type), dimension (FIRE_MAX_IGNITIONS) :: ignition_line

  contains

    subroutine Initialize_ignitions (config_flags)

      implicit none

      type (namelist_t), intent (in) :: config_flags

      integer, parameter :: FIRE_MAX_IGNITIONS_IN_NAMELIST = 5
      integer :: i


      if (FIRE_MAX_IGNITIONS < FIRE_MAX_IGNITIONS_IN_NAMELIST) call crash ('FIRE_MAX_IGNITIONS too small')

      ignition_line(1)%start_x = config_flags%fire_ignition_start_lon1
      ignition_line(1)%start_y = config_flags%fire_ignition_start_lat1
      ignition_line(1)%end_x = config_flags%fire_ignition_end_lon1
      ignition_line(1)%end_y = config_flags%fire_ignition_end_lat1
      ignition_line(1)%ros = config_flags%fire_ignition_ros1
      ignition_line(1)%radius = config_flags%fire_ignition_radius1
      ignition_line(1)%start_time = config_flags%fire_ignition_start_time1
      ignition_line(1)%end_time = config_flags%fire_ignition_end_time1

      ignition_line(2)%start_x = config_flags%fire_ignition_start_lon2
      ignition_line(2)%start_y = config_flags%fire_ignition_start_lat2
      ignition_line(2)%end_x = config_flags%fire_ignition_end_lon2
      ignition_line(2)%end_y = config_flags%fire_ignition_end_lat2
      ignition_line(2)%ros = config_flags%fire_ignition_ros2
      ignition_line(2)%radius = config_flags%fire_ignition_radius2
      ignition_line(2)%start_time = config_flags%fire_ignition_start_time2
      ignition_line(2)%end_time = config_flags%fire_ignition_end_time2

      ignition_line(3)%start_x = config_flags%fire_ignition_start_lon3
      ignition_line(3)%start_y = config_flags%fire_ignition_start_lat3
      ignition_line(3)%end_x = config_flags%fire_ignition_end_lon3
      ignition_line(3)%end_y = config_flags%fire_ignition_end_lat3
      ignition_line(3)%ros = config_flags%fire_ignition_ros3
      ignition_line(3)%radius = config_flags%fire_ignition_radius3
      ignition_line(3)%start_time = config_flags%fire_ignition_start_time3
      ignition_line(3)%end_time = config_flags%fire_ignition_end_time3

      ignition_line(4)%start_x = config_flags%fire_ignition_start_lon4
      ignition_line(4)%start_y = config_flags%fire_ignition_start_lat4
      ignition_line(4)%end_x = config_flags%fire_ignition_end_lon4
      ignition_line(4)%end_y = config_flags%fire_ignition_end_lat4
      ignition_line(4)%ros = config_flags%fire_ignition_ros4
      ignition_line(4)%radius = config_flags%fire_ignition_radius4
      ignition_line(4)%start_time = config_flags%fire_ignition_start_time4
      ignition_line(4)%end_time = config_flags%fire_ignition_end_time4

      ignition_line(5)%start_x = config_flags%fire_ignition_start_lon5
      ignition_line(5)%start_y = config_flags%fire_ignition_start_lat5
      ignition_line(5)%end_x = config_flags%fire_ignition_end_lon5
      ignition_line(5)%end_y = config_flags%fire_ignition_end_lat5
      ignition_line(5)%ros = config_flags%fire_ignition_ros5
      ignition_line(5)%radius = config_flags%fire_ignition_radius5
      ignition_line(5)%start_time = config_flags%fire_ignition_start_time5
      ignition_line(5)%end_time = config_flags%fire_ignition_end_time5

      fire_num_ignitions = 0
      do i = 1, min (FIRE_MAX_IGNITIONS_IN_NAMELIST, config_flags%fire_num_ignitions)
          ! Count the ignitions
        if (ignition_line(i)%radius > 0.0) fire_num_ignitions = i
          ! Expand ignition data given as zero
        if (ignition_line(i)%end_x == 0.0) ignition_line(i)%end_x = ignition_line(i)%start_x
        if (ignition_line(i)%end_y == 0.0) ignition_line(i)%end_y = ignition_line(i)%start_y
        if (ignition_line(i)%end_time == 0.0) ignition_line(i)%end_time = ignition_line(i)%start_time
      end do

    end subroutine Initialize_ignitions

  end module ignition_line_mod

