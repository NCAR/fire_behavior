  module ignition_line_mod

    use namelist_mod, only: namelist_t, FIRE_MAX_IGNITIONS_IN_NAMELIST
    use stderrout_mod, only: Crash, Message

    implicit none

    private

    public :: ignition_line_t, Initialize_ignitions, Ignite_fire

    integer :: fire_num_ignitions
    integer, parameter :: FIRE_MAX_IGNITIONS = 5

    type :: ignition_line_t
      real  ros, &        ! subscale rate of spread during the ignition process
          start_x, &      ! x coordinate of the ignition line start point (m, or long/lat)
          start_y, &      ! y coordinate of the ignition line start point
          end_x, &        ! x coordinate of the ignition line end point
          end_y, &        ! y coordinate of the ignition line end point
          start_time, &   ! ignition time for the start point from simulation start (s)
          end_time, &     ! ignition time for the end poin from simulation start (s)
          radius          ! all within this radius ignites immediately
    end type ignition_line_t

  contains

    subroutine Initialize_ignitions (config_flags, ignition_lines)

      implicit none

      type (namelist_t), intent (in) :: config_flags
      type (ignition_line_t), dimension (:), allocatable, intent (out) :: ignition_lines

      integer, parameter :: N_IGNS_INIT = 5
      type (ignition_line_t), dimension (N_IGNS_INIT) :: ignition_line
      integer :: i


      if (config_flags%fire_num_ignitions > FIRE_MAX_IGNITIONS_IN_NAMELIST) call Crash ('FIRE_MAX_IGNITIONS_IN_NAMELIST too small')

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

      do i = 1, config_flags%fire_num_ignitions
          ! Count the ignitions
        if (ignition_line(i)%radius <= 0.0) call Crash ('Radius ignition line must be > 0')
          ! Expand ignition data given as zero
        if (ignition_line(i)%end_x == 0.0) ignition_line(i)%end_x = ignition_line(i)%start_x
        if (ignition_line(i)%end_y == 0.0) ignition_line(i)%end_y = ignition_line(i)%start_y
        if (ignition_line(i)%end_time == 0.0) ignition_line(i)%end_time = ignition_line(i)%start_time
      end do

      ignition_lines = ignition_line(1:config_flags%fire_num_ignitions)

    end subroutine Initialize_ignitions

    subroutine Ignite_fire (ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, &
        ignition_line, start_ts, end_ts, coord_xf, coord_yf, &
        unit_xf, unit_yf, lfn, tign, ignited)

    ! purpose: ignite a circular/line fire
    !          ignite fire in the region within radius r from the line (sx, sy) to (ex,e y).

      implicit none

      integer, intent (in) :: ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme
      type (ignition_line_t), intent (in) :: ignition_line
      real, intent (in) :: unit_xf, unit_yf     !  coordinate units in m
      real, intent (in) :: start_ts, end_ts     ! the time step start and end
      real, dimension (ifms:ifme, jfms:jfme), intent (in) :: coord_xf, coord_yf !  node coordinates
      real, dimension (ifms:ifme, jfms:jfme), intent (in out) :: lfn, tign
      integer, intent(out) :: ignited ! number of nodes newly ignited

      integer :: i, j
      real :: lfn_new, time_ign, ax, ay, rele, d, sx, sy, ex, ey, st, et, cx2, cy2, dmax, dmin, &
           end_x, end_y, radius, start_time, end_time, ros, tos


      sx = ignition_line%start_x  ! x coordinate of the ignition line start point
      sy = ignition_line%start_y  ! y coordinate of the ignition line start point
      end_x = ignition_line%end_x ! x coordinate of the ignition line end point
      end_y = ignition_line%end_y ! y coordinate of the ignition line end point
      start_time = ignition_line%start_time ! ignition time for the start point from simulation start (s)
      end_time = ignition_line%end_time     ! ignition time for the end poin from simulation start (s)
      radius = ignition_line%radius         ! all within this radius ignites immediately
      ros = ignition_line%ros     ! rate of spread
      tos = radius / ros          ! time of spread to the given radius
      st = start_time             ! the start time of ignition considered in this time step
      et = min (end_ts, end_time) ! the end time of the ignition segment in this time step

        ! (start_ts, end_ts) must be subset (start_time, end_time + tos)
      if (start_ts > et + tos .or. end_ts < st) return

      if (start_time < end_time) then  ! we really want to test start_time .ne. end_time, but avoiding test of floats on equality
        rele =  (et - start_time) / (end_time - start_time)    ! relative position of et in the segment (start,end)
        ex = sx + rele * (end_x - sx)
        ey = sy + rele * (end_y - sy)
      else
        ex = end_x
        ey = end_y
      end if

      cx2 = unit_xf * unit_xf
      cy2 = unit_yf * unit_yf

      ignited = 0
      dmax = 0.0
      dmin = huge (dmax)
      do j = jfts, jfte
        do i = ifts, ifte
          ax = coord_xf(i, j)
          ay = coord_yf(i, j)
            ! get d= distance from the nearest point on the ignition segment
            ! and time_ign = the ignition time there
          call Nearest (d, time_ign, ax, ay, sx, sy, st, ex, ey, et, cx2, cy2)
          dmax = max (d, dmax)
          dmin = min (d, dmin)
            ! lft at end_ts
          lfn_new = d - min (radius, ros * (end_ts - time_ign))

          if (.not. lfn_new > 0.0) ignited = ignited + 1

          if (lfn(i, j) > 0.0 .and. .not. lfn_new > 0.0) then
              ! newly ignited now
            tign(i, j) = time_ign + d / ros
            tign(i, j) = min (max (tign(i, j), start_ts), end_ts)
          end if
          lfn(i, j) = min (lfn(i, j), lfn_new)
        end do
      end do

      return

    contains

      subroutine Nearest (d, t, ax, ay, sx, sy, st, ex, ey, et, cx2, cy2)

      ! input:
      ! ax, ay       coordinates of point a
      ! sx,sy,ex,ey  coordinates of endpoints of segment [x,y]
      ! st,et        start time ignition and end time ignition in time step
      ! cx2,cy2      x and y unit squared for computing distance

      ! output
      ! d            the distance of a and the nearest point z on the segment [x,y]
      ! t            linear interpolation from the values st,et to the point z
      !
      ! method: compute d as the distance (ax,ay) from the midpoint (mx,my)
      ! minus a correction (because of rounding errors): |a-c|^2 = |a-m|^2 - |m-c|^2
      ! when |m-c| >= |s-e|/2 the nearest point is one of the endpoints
      ! the computation work also for the case when s=e exactly or approximately
      !
      !
      !           a
      !          /| \
      !     s---m-c--e
      !
      ! |m-c| = |a-m| cos (a-m,e-s)
      !       = |a-m| (a-m).(e-s))/(|a-m|*|e-s|)

        implicit none

        real, intent (in) :: ax, ay, sx, sy, st, ex, ey, et, cx2, cy2
        real, intent (out) :: d, t

        real :: mx, my, dam2, dames, am_es, cos2, dmc2, mcrel, mid_t, dif_t, des2, cx, cy


          ! midpoint m = (mx,my)
        mx = (sx + ex) * 0.5
        my = (sy + ey) * 0.5
          ! |a-m|^2
        dam2 = (ax - mx) * (ax - mx) * cx2 + (ay - my) * (ay - my) * cy2
          ! des2 = |e-s|^2
        des2 = (ex - sx) * (ex - sx) * cx2 + (ey - sy) * (ey - sy) * cy2
        dames = dam2 * des2
          ! am_es = (a-m).(e-s)
        am_es = (ax - mx) * (ex - sx) * cx2 + (ay - my) * (ey - sy) * cy2
        if (dames > 0.0) then
            ! cos2 = cos^2 (a-m,e-s)
          cos2 = (am_es * am_es) / dames
        else ! point a already is the midpoint
          cos2 = 0.0
        end if
          ! dmc2 = |m-c|^2
        dmc2 = dam2 * cos2
        if (4.0 * dmc2 < des2) then
            ! if |m-c|<=|e-s|/2
            ! d = sqrt(max(dam2 - dmc2,0.))     ! d=|a-m|^2 - |m-c|^2, guard rounding
            ! relative distance of c from m
          mcrel = sign (sqrt (4.0 * dmc2 / des2), am_es)
        else if (am_es > 0) then
            ! if cos > 0, closest is e
          mcrel = 1.0
        else
           ! closest is s
          mcrel = -1.0
        end if
         ! interpolate to c by going from m
        cx = (ex + sx) * 0.5 + mcrel * (ex - sx) * 0.5
          ! interpolate to c by going from m
        cy = (ey + sy) * 0.5 + mcrel * (ey - sy) * 0.5

          ! output
          ! 1) |a-c|^2
        d = sqrt ((ax - cx) * (ax - cx) * cx2 + (ay - cy) * (ay - cy) * cy2)
          ! 2) interpolate to c by going from m
        t = (et + st) * 0.5 + mcrel * (et - st) * 0.5

      end subroutine Nearest

    end subroutine Ignite_fire

  end module ignition_line_mod

