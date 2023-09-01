  module ignition_line_mod

    use namelist_mod, only: namelist_t, FIRE_MAX_IGNITIONS_IN_NAMELIST
    use module_fr_fire_util, only: Crash, Message

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

subroutine ignite_fire( ifds,ifde,jfds,jfde,                    & ! fire domain dims - the whole domain
                        ifms,ifme,jfms,jfme,                      &
                        ifts,ifte,jfts,jfte,                      &
                        fire_print_msg,                           &
                        ignition_line,                            &
                        start_ts,end_ts,                    &
                        coord_xf,coord_yf,                &
                        unit_xf,unit_yf,                  &
                        lfn,tign,ignited)
implicit none

!*** purpose: ignite a circular/line/prescribed fire

!*** description
! ignite fire in the region within radius r from the line (sx,sy) to (ex,ey).
! the coordinates of nodes are given as the arrays coord_xf and coord_yf
! r is given in m
! one unit of coord_xf is unit_xf m
! one unit of coord_yf is unit_yf m
! so a node (i,j) will be ignited iff for some (x,y) on the line
! || ( (coord_xf(i,j) - x)*unit_xf , (coord_yf(i,j) - y)*unit_yf ) || <= r


!*** arguments
integer, intent(in):: ifds,ifde,jfds,jfde   ! fire domain bounds
integer, intent(in):: ifts,ifte,jfts,jfte   ! fire tile bounds
integer, intent(in):: ifms,ifme,jfms,jfme   ! array bounds
integer, intent(in):: fire_print_msg
type(ignition_line_t), intent(in):: ignition_line    ! description of the ignition line
real, intent(in):: start_ts,end_ts          ! the time step start and end
real, dimension(ifms:ifme, jfms:jfme), intent(in):: &
    coord_xf,coord_yf                       !  node coordinates
real, intent(in):: unit_xf,unit_yf          !  coordinate units in m
real, intent(inout), dimension (ifms:ifme,jfms:jfme) :: &
                   lfn, tign                ! level function, ignition time (state)
integer, intent(out):: ignited              ! number of nodes newly ignited

!*** local
integer:: i,j
real::lfn_new,time_ign,ax,ay,rels,rele,d
real:: sx,sy                    ! start of ignition line, from lower left corner
real:: ex,ey                    ! end of ignition line, or zero
real:: st,et                    ! start and end of time of the ignition line
character(len=128):: msg
real::cx2,cy2,dmax,axmin,axmax,aymin,aymax,dmin
real:: start_x,start_y          ! start of ignition line, from lower left corner
real:: end_x,end_y              ! end of ignition line, or zero
real:: radius                   ! all within the radius of the line will ignite
real:: start_time,end_time      ! the ignition time for the start and the end of the line
real:: ros,tos                  ! ignition rate and time of spread


! copy ignition line fields to local variables
start_x    = ignition_line%start_x ! x coordinate of the ignition line start point (m, or long/lat)
start_y    = ignition_line%start_y ! y coordinate of the ignition line start point
end_x      = ignition_line%end_x   ! x coordinate of the ignition line end point
end_y      = ignition_line%end_y   ! y coordinate of the ignition line end point
start_time = ignition_line%start_time ! ignition time for the start point from simulation start (s)
end_time   = ignition_line%end_time! ignition time for the end poin from simulation start (s)
radius     = ignition_line%radius  ! all within this radius ignites immediately
ros        = ignition_line%ros     ! rate of spread
tos        = radius/ros            ! time of spread to the given radius
st         = start_time            ! the start time of ignition considered in this time step
et         = min(end_ts,end_time)  ! the end time of the ignition segment in this time step

! this should be called whenever (start_ts, end_ts) \subset (start_time, end_time + tos)
if(start_ts>et+tos .or. end_ts<st)return   ! too late or too early, nothing to do
!print *, 'IGNITING'
if(start_time < end_time)then  ! we really want to test start_time .ne. end_time, but avoiding test of floats on equality
        ! segment of nonzero length
        !rels =  (st - start_time) / (end_time - start_time)  ! relative position of st in the segment (start,end)
        !sx = start_x + rels * (end_x - start_x)
        !sy = start_y + rels * (end_y - start_y)
!paj        rels = 0.
        sx = start_x
        sy = start_y
        rele =  (et - start_time) / (end_time - start_time)    ! relative position of et in the segment (start,end)
        ex = start_x + rele * (end_x - start_x)
        ey = start_y + rele * (end_y - start_y)
else
        ! just a point
!paj        rels = 0.
!paj        rele = 1.
        sx = start_x
        sy = start_y
        ex = end_x
        ey = end_y
endif


cx2=unit_xf*unit_xf
cy2=unit_yf*unit_yf

axmin=coord_xf(ifts,jfts)
aymin=coord_yf(ifts,jfts)
axmax=coord_xf(ifte,jfte)
aymax=coord_yf(ifte,jfte)
!$OMP CRITICAL(FIRE_CORE_CRIT)
write(msg,'(a,2g13.6,a,2g13.6)')'IGN from ',sx,sy,' to ',ex,ey
call message(msg,fire_print_msg)
write(msg,'(a,2f10.2,a,2f10.2,a)')'IGN timestep [',start_ts,end_ts,'] in [',start_time,end_time,']'
call message(msg,fire_print_msg)
write(msg,'(a,2g13.6,a,2g13.6)')'IGN tile coord from  ',axmin,aymin,' to ',axmax,aymax
call message(msg,fire_print_msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
ignited=0
dmax=0
dmin=huge(dmax)
11      format('IGN ',6(a,g17.7,1x))
12      format('IGN ',4(a,2g13.7,1x))
do j=jfts,jfte
    do i=ifts,ifte
        ax=coord_xf(i,j)
        ay=coord_yf(i,j)

        ! get d= distance from the nearest point on the ignition segment
        ! and time_ign = the ignition time there
        call nearest(d,time_ign,ax,ay,sx,sy,st,ex,ey,et,cx2,cy2,fire_print_msg)
        dmax=max(d,dmax)
        dmin=min(d,dmin)

        lfn_new=d - min( radius, ros*(end_ts - time_ign) )  ! lft at end_ts
        if(fire_print_msg.ge.3)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
            write(msg,*)'IGN1 i,j=',i,j,' lfn(i,j)=',lfn(i,j),' tign(i,j)=',tign(i,j)
            call message(msg,fire_print_msg)
            write(msg,*)'IGN2 i,j=',i,j,' lfn_new= ',lfn_new, ' time_ign= ',time_ign,' d=',d
            call message(msg,fire_print_msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
        endif
        if(.not.lfn_new>0.) then
            ignited=ignited+1   ! count
        endif
        if(lfn(i,j)>0. .and. .not. lfn_new > 0.) then
            tign(i,j)=time_ign + d/ros  ! newly ignited now
            if(fire_print_msg.ge.3)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
                write(msg,'(a,2i6,a,2g13.6,a,f10.2,a,2f10.2,a)')'IGN ignited cell ',i,j,' at',ax,ay, &
                    ' time',tign(i,j),' in [',start_ts,end_ts,']'
                call message(msg,fire_print_msg)
                write(msg,'(a,g10.3,a,f10.2,a,2f10.2,a)')'IGN distance',d,' from ignition line at',time_ign,' in [',st,et,']'
                call message(msg,fire_print_msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
            endif
            if(tign(i,j) < start_ts .or. tign(i,j) > end_ts )then
!$OMP CRITICAL(FIRE_CORE_CRIT)
                write(msg,'(a,2i6,a,f11.6,a,2f11.6,a)')'WARNING ',i,j, &
                ' fixing ignition time ',tign(i,j),' outside of the time step [',start_ts,end_ts,']'
                call message (msg,fire_print_msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
                tign(i,j) = min(max(tign(i,j),start_ts),end_ts)
            endif
        endif
        lfn(i,j)=min(lfn(i,j),lfn_new)  ! update the level set function
        if(fire_print_msg.ge.3)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
            write(msg,*)'IGN3 i,j=',i,j,' lfn(i,j)=',lfn(i,j),' tign(i,j)=',tign(i,j)
            call message(msg,fire_print_msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
        endif
    enddo
enddo
!$OMP CRITICAL(FIRE_CORE_CRIT)
write(msg,'(a,2g13.2,a,g10.2,a,g10.2)')'IGN units ',unit_xf,unit_yf,' m max dist ',dmax,' min',dmin
call message(msg,fire_print_msg)
write(msg,'(a,f6.1,a,f8.1,a,i10)')'IGN radius ',radius,' time of spread',tos,' ignited nodes',ignited
call message(msg,fire_print_msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)

return
99 continue

end subroutine ignite_fire

! called from the inside of a loop, inline if possible
!DEC$ ATTRIBUTES FORCEINLINE
SUBROUTINE nearest(d,t,ax,ay,sx,sy,st,ex,ey,et,cx2,cy2,fire_print_msg)
        implicit none
!***    arguments
        integer, intent(in):: fire_print_msg
        real, intent(out):: d,t
        real, intent(in):: ax,ay,sx,sy,st,ex,ey,et,cx2,cy2
        ! input:
        ! ax, ay       coordinates of point a
        ! sx,sy,ex,ey  coordinates of endpoints of segment [x,y]
        ! st,et        values at the endpoints x,y
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

        real:: mx,my,dam2,dames,am_es,cos2,dmc2,mcrel,mid_t,dif_t,des2,cx,cy
        character(len=128):: msg


11      format('IGN ',6(a,g17.7,1x))
12      format('IGN ',4(a,2g13.7,1x))

        ! midpoint m = (mx,my)
        mx = (sx + ex)*0.5
        my = (sy + ey)*0.5
        dam2=(ax-mx)*(ax-mx)*cx2+(ay-my)*(ay-my)*cy2      ! |a-m|^2
        des2 = (ex-sx)*(ex-sx)*cx2+(ey-sy)*(ey-sy)*cy2          ! des2 = |e-s|^2
        dames = dam2*des2
        am_es=(ax-mx)*(ex-sx)*cx2+(ay-my)*(ey-sy)*cy2       ! am_es = (a-m).(e-s)
        if(dames>0)then
            cos2 = (am_es*am_es)/dames                  ! cos2 = cos^2 (a-m,e-s)
        else ! point a already is the midpoint
            cos2 = 0.
        endif
        dmc2 = dam2*cos2                                ! dmc2 = |m-c|^2
        if(4.*dmc2 < des2)then                          ! if |m-c|<=|e-s|/2
            ! d = sqrt(max(dam2 - dmc2,0.))               ! d=|a-m|^2 - |m-c|^2, guard rounding
            mcrel = sign(sqrt(4.*dmc2/des2),am_es)      ! relative distance of c from m
        elseif(am_es>0)then                             ! if cos > 0, closest is e
            mcrel = 1.0
        else                                            ! closest is s
            mcrel = -1.0
        endif
        cx = (ex + sx)*0.5 + mcrel*(ex - sx)*0.5     ! interpolate to c by going from m
        cy = (ey + sy)*0.5 + mcrel*(ey - sy)*0.5     ! interpolate to c by going from m
        d=sqrt((ax-cx)*(ax-cx)*cx2+(ay-cy)*(ay-cy)*cy2) ! |a-c|^2
        t = (et + st)*0.5 + mcrel*(et - st)*0.5     ! interpolate to c by going from m
        if(fire_print_msg.ge.3)then
!$OMP CRITICAL(FIRE_CORE_CRIT)
            write(msg,12)'find nearest to [',ax,ay,'] from [',sx,sy,'] [',ex,ey,']' ! DEB
            call message(msg,fire_print_msg)
            write(msg,12)'end times',st,et,' scale squared',cx2,cy2 ! DEB
            call message(msg,fire_print_msg)
            write(msg,11)'nearest at mcrel=',mcrel,'from the midpoint, t=',t ! DEB
            call message(msg,fire_print_msg)
            write(msg,12)'nearest is [',cx,cy,'] d=',d ! DEB
            call message(msg,fire_print_msg)
            write(msg,11)'dam2=',dam2,'des2=',des2,'dames=',dames
            call message(msg,fire_print_msg)
            write(msg,11)'am_es=',am_es,'cos2=',cos2,'dmc2=',dmc2 ! DEB
            call message(msg,fire_print_msg)
!$OMP END CRITICAL(FIRE_CORE_CRIT)
        endif
END SUBROUTINE nearest

  end module ignition_line_mod

