
  module module_fr_fire_core

    use ros_wrffire_mod, only: ros_wrffire_t
    use stderrout_mod, only: Crash, Message
    use state_mod, only: state_fire_t
    use ignition_line_mod, only : ignition_line_t
    use ros_wrffire_mod, only : ros_wrffire_t

    implicit none

    private

    public :: Fuel_left, Tign_update, Reinit_ls_rk3, Prop_ls_rk3, Continue_at_boundary

    logical, parameter :: FIRE_GROWS_ONLY = .true.

  contains

    subroutine Fuel_left (ims, ime, jms, jme, its, ite, jts, jte, ifs, ife, jfs, jfe, &
        lfn, tign, fuel_time, time_now, fuel_frac, fire_area, fire_print_msg )

      implicit none

      !*** purpose: determine fraction of fuel remaining
      !*** NOTE: because variables are cell centered, need halo/sync width 1 before

      integer, intent (in) :: its, ite, jts, jte, ims, ime, jms, jme, ifs, ife, jfs, jfe, &
          fire_print_msg
      real, intent (in), dimension (ims:ime, jms:jme) :: lfn,tign, fuel_time
      real, intent (in) :: time_now
      real, intent (out), dimension (ifs:ife, jfs:jfe) :: fuel_frac
      real, intent (out), dimension (ims:ime, jms:jme) :: fire_area

      integer :: i, j, ir, jr, icl, jcl, isubcl, jsubcl, i2, j2, ii, jj
      real :: fmax, frat, helpsum1, helpsum2, fuel_left_ff, fire_area_ff, rx, ry, tignf(2,2)
         ! help variables instead of arrays fuel_left_f and fire_area_f 
      real :: lffij, lffi1j, lffij1, lffi1j1, tifij, tifi1j, tifij1, tifi1j1, tx, ty, txx, tyy
         ! variables for calculation instead of lff(i,j) and tif(i,j)is lffij,tifij etc..
      character (len = 128) :: msg
      integer :: m, omp_get_thread_num
      integer, parameter :: fuel_left_irl = 2       ! "submesh to compute fuel lwft, even, at least 2" ""
      integer, parameter :: fuel_left_jrl = 2       ! "submesh to compute fuel lwft, even, at least 2" ""

        ! refinement
      ir = fuel_left_irl
      jr = fuel_left_jrl

      if ((ir /= 2) .or. (jr /= 2)) then 
         call Crash ('fuel_left: ir.ne.2 or jr.ne.2 ')
      end if

      rx = 1.0 / ir 
      ry = 1.0 / jr

        ! example for ir=2:
        !
        !                     |      coarse cell      |
        !      its-1                     its                                   ite             ite+1
        ! -------X------------|-----.-----X-----.-----|--........----|----------X----------|---------X
        !           fine node 1           2           3                   2*(ite-its+1) 
        !                fine cell  1           2                             cell 2*(ite-its+1)

        !  Loop over cells in Tile 
        !  Changes made by Volodymyr Kondratenko 09/24/2009
      do icl = its, ite
        do jcl = jts, jte
          helpsum1 = 0
          helpsum2 = 0
            ! Loop over subcells in cell #(icl,jcl)
          do isubcl = 1, ir
            do jsubcl = 1, jr 
              i = (icl - its) * ir + isubcl
              j = (jcl - jts) * jr + jsubcl
                ! Direct calculation tif and lff, avoiding arrays, just for case ir=jr=2
              if ((isubcl == 1) .and. (jsubcl == 1)) then
                 i2 = icl - 1
                 j2 = jcl - 1
                 ty = 0.5
                 tx = 0.5
                 tyy = 1.0
                 txx = 1.0
              else if ((isubcl == 2).and. (jsubcl == 1)) then
                i2 = icl
                j2 = jcl - 1
                ty = 0.5
                tx = 0
                tyy = 1.0
                txx = 0.5
              else if ((isubcl == 1) .and. (jsubcl == 2)) then
                i2 = icl - 1
                j2 = jcl
                tx = 0.5
                ty = 0
                txx = 1.0
                tyy = 0.5
              else if ((isubcl == 2) .and. (jsubcl == 2)) then
                i2 = icl
                j2 = jcl
                tx = 0
                ty = 0
                txx = 0.5
                tyy = 0.5
              else
                call Crash ('fuel_left: isubcl,jsubcl should be only 1 or 2')
              endif 

              ! calculation lff and tif in 4 endpoints of subcell
              lffij=                                &    
                  (1 - tx) * (1 - ty) * lfn(i2, j2) &
                  + (1 - tx) * ty * lfn(i2, j2 + 1) &
                  + tx * (1 - ty) * lfn(i2 + 1, j2) &
                  + tx * ty * lfn(i2 + 1, j2 + 1)
              lffi1j=                                &
                  (1 - txx) * (1 - ty) * lfn(i2, j2) &
                  + (1 - txx) * ty * lfn(i2 ,j2 + 1) &
                  + (txx) * (1 - ty) * lfn(i2 + 1, j2) &
                  + (txx) * ty * lfn(i2 + 1, j2 + 1)
              lffij1=                            &
                  (1 - tx) * (1 - tyy) * lfn(i2, j2) &
                  + (1 - tx) * tyy * lfn(i2, j2 + 1) &
                  + tx * (1 - tyy) * lfn(i2 + 1, j2) &
                  + tx * tyy * lfn(i2 + 1, j2 + 1)
              lffi1j1 =                               &
                  (1 - txx) * (1 - tyy) * lfn(i2, j2) &
                  + (1 - txx) * tyy * lfn(i2, j2 + 1) &        
                  + txx * (1 - tyy) * lfn(i2 + 1, j2) &
                  + txx * tyy * lfn(i2 + 1, j2 + 1)
 
               ! get ready to fix up tign values to be interpolated
              do ii = 1, 2
                do jj = 1, 2
                  tignf(ii, jj) = tign(i2 + ii - 1, j2 + jj - 1)
                end do
              end do
              tifij =                                 &
                  (1 - tx) * (1 - ty) * tignf(1, 1) &
                  + (1 - tx) * ty * tignf(1, 1 + 1) &
                  + tx * (1 - ty) * tignf(1 + 1, 1) &
                  + tx * ty * tignf(1 + 1, 1 + 1)
              tifi1j =                               &
                  (1 - txx) * (1 - ty) * tignf(1, 1) &
                  + (1 - txx) * ty * tignf(1, 1 + 1) &
                  + txx * (1 - ty) * tignf(1 + 1, 1) &
                  + txx * ty * tignf(1 + 1, 1 + 1)
              tifij1 =                               &
                  (1 - tx) * (1 - tyy) * tignf(1, 1) &
                  + (1 - tx) * tyy * tignf(1, 1 + 1) &
                  + tx * (1 - tyy) * tignf(1+1, 1) &
                  + tx * tyy * tignf(1 + 1, 1 + 1)
             tifi1j1 =                               &
                  (1 - txx) * (1 - tyy) * tignf(1, 1) &
                  + (1 - txx) * tyy * tignf(1, 1 + 1) &
                  + txx * (1 - tyy) * tignf(1 + 1, 1) &
                  + txx * tyy * tignf(1 + 1, 1 + 1)
 
             call fuel_left_cell_1 (fuel_left_ff, fire_area_ff, &
                 lffij, lffij1, lffi1j, lffi1j1, &
                 tifij, tifij1, tifi1j, tifi1j1, &
                 time_now, fuel_time(icl,jcl), fire_print_msg)

                ! consistency check
              if (fire_area_ff < -1e-6 .or.  &
                  (fire_area_ff == 0.0 .and. fuel_left_ff < 1.0 - 1e-6)) then
                !$OMP CRITICAL(FIRE_CORE_CRIT)
                write (msg, '(a, 2i6, 2(a, f11.8))') 'fuel_left: at node', i, j, &
                    ' of refined mesh fuel burnt', 1 - fuel_left_ff, ' fire area', fire_area_ff
                !$OMP END CRITICAL(FIRE_CORE_CRIT)
                call crash(msg)
              endif

              helpsum1 = helpsum1 + fuel_left_ff
              helpsum2 = helpsum2 + fire_area_ff
            end do
          end do
          fuel_frac(icl, jcl) = helpsum1 
          fire_area(icl, jcl) = helpsum2
        end do 
      end do

        ! finish the averaging
      do j = jts, jte
        do i = its, ite        
          fuel_frac(i, j) = fuel_frac(i,j) / (ir * jr) ! multiply by weight for averaging over coarse cell
          fire_area(i, j) = fire_area(i,j) / (ir * jr) ! 
        end do
      end do

        ! consistency check after sum
      fmax = 0
      do j = jts, jte
          do i = its, ite        
             if (fire_area(i, j) == 0.0) then
               if (fuel_frac(i, j) < 1.-1e-6) then
                 !$OMP CRITICAL(FIRE_CORE_CRIT)
                 write (msg, '(a, 2i6, 2(a, f11.8))') 'fuel_left: at node', i, j, &
                     ' fuel burnt', 1 - fuel_frac(i, j), ' but fire area', fire_area(i, j)
                 !$OMP END CRITICAL(FIRE_CORE_CRIT)
                     call crash (msg)
               end if
             else
               frat = (1 - fuel_frac(i, j)) / fire_area(i, j)
               fmax = max (fmax, frat)
             end if
          end do
      end do
      !$OMP CRITICAL(FIRE_CORE_CRIT)
      write (msg,'(a, 4i6, a, f10.7)') 'fuel_left: tile', its, ite, jts, jte, ' max fuel burnt/area', fmax 
      !$OMP END CRITICAL(FIRE_CORE_CRIT)
      call message (msg, fire_print_msg)

      return
    end subroutine fuel_left

    subroutine fuel_left_cell_1( fuel_frac_left, fire_frac_area, &
        lfn00,lfn01,lfn10,lfn11, &
        tign00,tign01,tign10,tign11,&
        time_now, fuel_time_cell, fire_print_msg)

      implicit none

      integer, intent(in)::fire_print_msg
      real, intent(out):: fuel_frac_left, fire_frac_area ! 
      real, intent(in)::lfn00,lfn01,lfn10,lfn11    ! level set function at 4 corners of the cell
      real, intent(in)::tign00,tign01,tign10,tign11! ignition time at the  4 corners of the cell
      real, intent(in)::time_now                   ! the time now
      real, intent(in)::fuel_time_cell            ! time to burns off to 1/e

      !*** Description
      ! The area burning is given by the condition L <= 0, where the function P is
      ! interpolated from lfn(i,j)
      !
      ! The time since ignition is the function T, interpolated in from tign(i,j)-time_now.
      ! The values of tign(i,j) where lfn(i,j)>=0 are ignored, tign(i,j)=0 is taken 
      ! when lfn(i,j)=0.
      !
      ! The function computes an approxmation  of the integral
      !
      !
      !                                  /\
      !                                  |              
      ! fuel_frac_left  =      1   -     | 1 -  exp(-T(x,y)/fuel_time_cell)) dxdy
      !                                  |            
      !                                 \/
      !                                0<x<1
      !                                0<y<1
      !                             L(x,y)<=0
      !
      ! When the cell is not burning at all (all lfn>=0), then fuel_frac(i,j)=1.
      ! Because of symmetries, the result should not depend on the mesh spacing dx dy
      ! so dx=1 and dy=1 assumed.
      !
      ! Example:
      !
      !        lfn<0         lfn>0
      !      (0,1) -----O--(1,1)            O = points on the fireline, T=tnow
      !            |      \ |               A = the burning area for computing
      !            |       \|                        fuel_frac(i,j)
      !            |   A    O 
      !            |        |
      !            |        |
      !       (0,0)---------(1,0)
      !       lfn<0          lfn<0
      !
      ! Approximations allowed: 
      ! The fireline can be approximated by straight line(s).
      ! When all cell is burning, approximation by 1 point Gaussian quadrature is OK.
      ! 
      ! Requirements:
      ! 1. The output should be a continuous function of the arrays lfn and
      !  tign whenever lfn(i,j)=0 implies tign(i,j)=tnow.  
      ! 2. The output should be invariant to the symmetries of the input in each cell.
      ! 3. Arbitrary combinations of the signs of lfn(i,j) should work.
      ! 4. The result should be at least 1st order accurate in the sense that it is
      !    exact if the time from ignition is a linear function.
      !
      ! If time from ignition is approximated by polynomial in the burnt
      ! region of the cell, this is integral of polynomial times exponential
      ! over a polygon, which can be computed exactly.
      !
      ! Requirement 4 is particularly important when there is a significant decrease
      ! of the fuel fraction behind the fireline on the mesh scale, because the
      ! rate of fuel decrease right behind the fireline is much larger 
      ! (exponential...). This will happen when
      !
      ! change of time from ignition within one mesh cell / fuel_time_cell is not << 1
      !
      ! This is the same as
      !
      !               mesh cell size
      !  X =    -------------------------      is not << 1
      !       fireline speed * fuel_time_cell
      !         
      !
      ! When X is large then the fuel burnt in one timestep in the cell is
      ! approximately proportional to length of  fireline in that cell.
      !
      ! When X is small then the fuel burnt in one timestep in the cell is
      ! approximately proportional to the area of the burning region.
      !

      !*** calls
      intrinsic tiny

      real::ps,aps,area,ta,out
      real::t00,t01,t10,t11
      real,parameter::safe=tiny(aps)
      character(len=128)::msg

        ! the following algorithm is a very crude approximation

        ! minus time since ignition, 0 if no ignition yet
        ! it is possible to have 0 in fire region when ignitin time falls in 
        ! inside the time step because lfn is updated at the beginning of the time step
      t00=tign00-time_now
      if(lfn00>0. .or. t00>0.)t00=0.
      t01=tign01-time_now
      if(lfn01>0. .or. t01>0.)t01=0.
      t10=tign10-time_now
      if(lfn10>0. .or. t10>0.)t10=0.
      t11=tign11-time_now
      if(lfn11>0. .or. t11>0.)t11=0.

        ! approximate burning area, between 0 and 1   
      ps = lfn00+lfn01+lfn10+lfn11   
      aps = abs(lfn00)+abs(lfn01)+abs(lfn10)+abs(lfn11)
      aps=max(aps,safe)
      area =(-ps/aps+1.)/2.
      area = max(area,0.) ! make sure area is between 0 and 1
      area = min(area,1.)
    
        ! average negative time since ignition
      ta=0.25*(t00+t01+t10+t11)

        ! exp decay in the burning area
      out=1.
      !if(area>0.)out=1. - area*(1. - exp(ta/fuel_time_cell))
      if(area>0)out=area*exp(ta/fuel_time_cell) + (1. - area)

      if(out>1.)then
        !$OMP CRITICAL(FIRE_CORE_CRIT)
        write(msg,*)'out=',out,'>1 area=',area,' ta=',ta
        call message(msg,fire_print_msg)
        write(msg,*)'tign=',tign00,tign01,tign10,tign11,' time_now=',time_now
        !$OMP END CRITICAL(FIRE_CORE_CRIT)
        call message(msg,fire_print_msg)
        !call message('WARNING: fuel_left_cell_1: fuel fraction > 1')
        call crash('fuel_left_cell_1: fuel fraction > 1')
      endif

      !out = max(out,0.) ! make sure out is between 0 and 1
      !out = min(out,1.)

      fuel_frac_left = out
      fire_frac_area = area

    end subroutine fuel_left_cell_1

    subroutine prop_ls_rk3(               &             
                ifds,ifde,jfds,jfde,     &                 
                ifms,ifme,jfms,jfme,     &                    
                ifts,ifte,jfts,jfte,     &                    
                ts,dt,dx,dy,             &
                fire_upwinding, &
                fire_viscosity, fire_viscosity_bg, &
                fire_viscosity_band, fire_viscosity_ngp, &
                fire_advection, fire_slope_factor,       &
                fire_lsm_band_ngp, fire_print_msg,       &
                tbound,                  &                  
                lfn_in,                  &
                lfn_0,lfn_1,lfn_2,       & 
                lfn_out,tign,ros,        &           
                grid,                    &
                ros_model &
                   )


      implicit none

      !***************************************************************************************!
      !*** Level-set method recoded for true mpi communications and added                  ***!
      !*** 3rd-order Runge-Kutta time integration and 3rd/5th-order WENO advection schemes ***!
      !*** Implemented by: Domingo Munoz-Esparza (NCAR/RAL, April 2016)                    ***!
      !*** Reference: D. Munoz-Esparza, B. Kosovic, P. Jimenez, J. Coen: "An accurate      ***!
      !*** fire-spread algorithm in the Weather Research and Forecasting model using the   ***!
      !*** level-set method", Journal of Advances in Modeling Earth Systems, 2018          ***!
      !*** https://doi.org/10.1002/2017MS001108                                            ***!
      !***************************************************************************************!

      ! Propagation of closed curve by a level function method. The level function
      ! lfn is defined by its values at the nodes of a rectangular grid. 
      ! The area where lfn < 0 is inside the curve. The curve is 
      ! described implicitly by lfn=0. Points where the curve intersects gridlines
      ! can be found by linear interpolation from nodes.
      !
      ! The level function is advanced from time ts to time ts + dt. 
      !
      ! The level function should be initialized to (an approximation of) the signed
      ! distance from the curve. If the initial curve is a circle, the initial level
      ! function is simply the distance from the center minus the radius.
      ! 
      ! The curve moves outside with speed given by function speed_func.
      !   
      ! Method: Godunov/ENO method for the normal motion. The timestep is checked for
      ! CFL condition. For a straight segment in a constant field and locally linear
      ! level function, the method reduces to the exact normal motion. The advantage of 
      ! the level set method is that it treats automatically special cases such as
      ! the curve approaching itself and merging components of the area inside the curve.
      !
      ! Based on S. Osher and R. Fedkiw, Level set methods and dynamic implicit surfaces,
      ! Springer, 2003, Sec. 6.4, as implemented in toolboxLS for Matlab by 
      ! I. Mitchell, A toolbox of Level Set Methods (Version 1.1), TR-2007-11,
      ! Dept. Computer Science, University of British Columbia, 2007
      ! http://www.cs.ubc.ca/\~mitchell/Toolbo\LS
      ! 
  
      integer,intent(in)::ifms,ifme,jfms,jfme,ifds,ifde,jfds,jfde,ifts,ifte,jfts,jfte
      integer,intent(in):: fire_upwinding,fire_viscosity_ngp, &
                    fire_advection,fire_lsm_band_ngp,fire_print_msg
      real,intent(in)::fire_viscosity,fire_viscosity_bg,fire_viscosity_band,fire_slope_factor
      real,dimension(ifms:ifme,jfms:jfme),intent(inout)::lfn_in,tign
      real,dimension(ifms:ifme,jfms:jfme),intent(inout)::lfn_1,lfn_2,lfn_0 
      real,dimension(ifms:ifme,jfms:jfme),intent(out)::lfn_out,ros
      real,intent(in)::dx,dy,ts,dt
      real,intent(out)::tbound
      type (state_fire_t), target :: grid
      type (ros_wrffire_t), intent (in) :: ros_model

      real,dimension(ifms:ifme,jfms:jfme):: tend
      ! scalars
      real::grad2,rr,tbound2,tbound3

      real::gradx,grady,aspeed,err,aerr,time_now
      integer::i,j,its1,ite1,jts1,jte1,k,kk
      character(len=128)::msg
      integer::nfirenodes,nfireline
      real::sum_err,min_err,max_err,sum_aerr,min_aerr,max_aerr   

      ! constants
      integer,parameter :: mstep=1000, printl=1
      real, parameter:: zero=0.,one=1.,eps=epsilon(zero),tol=100*eps, &
          safe=2.,rmin=safe*tiny(zero),rmax=huge(zero)/safe

        ! f90 intrinsic function
      intrinsic max,min,sqrt,nint,epsilon,tiny,huge


      !$OMP CRITICAL(FIRE_CORE_CRIT)
      write(msg,'(a8,i5,a6,i5,3(a1,i5))')'prop_ls: tile ',ifts,':',ifte,',',jfts,':',jfte
      !$OMP END CRITICAL(FIRE_CORE_CRIT)
      call message(msg,fire_print_msg)

      !!!!!!!!!!!!!!!!!!!!!!
      ! Runge-Kutta step 1 !
      !!!!!!!!!!!!!!!!!!!!!!

     do j=jfts,jfte
        do i=ifts,ifte
            lfn_0(i,j) = lfn_in(i,j)
        enddo
     enddo

     call tend_ls(    &
         ifds,ifde,jfds,jfde, &                       ! domain dims 
        ifts,ifte,jfts,jfte, &                       ! tile dims
        ifms,ifme,jfms,jfme, &                       ! memory dims
        ts,dt,dx,dy,         &                       ! scalars in
        fire_upwinding, &
        fire_viscosity, fire_viscosity_bg, &
        fire_viscosity_band, fire_viscosity_ngp, &
        fire_advection, fire_slope_factor,       &
        fire_lsm_band_ngp, fire_print_msg,   &
        lfn_0,               &                       ! arrays in
        tbound,              &                       ! scalars out 
        tend, ros,           &                       ! arrays out        
        grid,                &                       ! params
        ros_model &
        )

      do j=jfts,jfte 
        do i=ifts,ifte 
            lfn_1(i,j) = lfn_0(i,j) + (dt/3.0)*tend(i,j) 
        enddo
      enddo

      !!!!!!!!!!!!!!!!!!!!!!
      ! Runge-Kutta step 2 !
      !!!!!!!!!!!!!!!!!!!!!!

     call tend_ls(    &
         ifds,ifde,jfds,jfde, &                     
         ifts,ifte,jfts,jfte, &                    
         ifms,ifme,jfms,jfme, &               
         ts+dt,dt,dx,dy,      &
         fire_upwinding, &
         fire_viscosity, fire_viscosity_bg, &
         fire_viscosity_band, fire_viscosity_ngp, &
         fire_advection, fire_slope_factor,       &
         fire_lsm_band_ngp, fire_print_msg,   &
         lfn_1,               &                             
         tbound2,             &                              
         tend,ros,            &                           
         grid,                &
         ros_model &
         )

      do j=jfts,jfte
        do i=ifts,ifte
            lfn_2(i,j) = lfn_0(i,j) + (dt/2.0)*tend(i,j)
        enddo
      enddo     

      !!!!!!!!!!!!!!!!!!!!!!
      ! Runge-Kutta step 3 !
      !!!!!!!!!!!!!!!!!!!!!!

     call tend_ls(    &
         ifds,ifde,jfds,jfde, &                      
         ifts,ifte,jfts,jfte, &                    
         ifms,ifme,jfms,jfme, &                 
         ts+dt,dt,dx,dy,      &
         fire_upwinding, &
         fire_viscosity, fire_viscosity_bg, &
         fire_viscosity_band, fire_viscosity_ngp, &
         fire_advection, fire_slope_factor,       &
         fire_lsm_band_ngp, fire_print_msg,   &
         lfn_2,               &                            
         tbound3,             &                            
         tend,ros,            &                           
         grid,                &
         ros_model &
         )

      do j=jfts,jfte
        do i=ifts,ifte
            lfn_out(i,j) = lfn_0(i,j) + dt*tend(i,j)
            lfn_2(i,j) = lfn_out(i,j) ! lfn_2=lfn_out (needed for reinitialization purposes)
        enddo
      enddo     

        ! CFL check
      tbound=min(tbound,tbound2,tbound3)

      !$OMP CRITICAL(FIRE_CORE_CRIT)
      write(msg,'(a,f10.2,4(a,f7.2))')'prop_ls: time',ts,' dt=',dt,' bound',min(tbound,999.99), &
          ' dx=',dx,' dy=',dy
      !$OMP END CRITICAL(FIRE_CORE_CRIT)

      call message(msg,fire_print_msg)
      if(dt>tbound)then
        !$OMP CRITICAL(FIRE_CORE_CRIT)
        write(msg,'(2(a,f10.2))')'prop_ls: WARNING: time step ',dt, &
        ' > bound =',tbound
        !$OMP END CRITICAL(FIRE_CORE_CRIT)
        call message(msg,fire_print_msg)
      endif
    
    end subroutine prop_ls_rk3

    subroutine reinit_ls_rk3(                       &
                ifts,ifte,jfts,jfte,               &                     
                ifms,ifme,jfms,jfme,               &                     
                ifds,ifde,jfds,jfde,               &                     
                ts,dt,dx,dy,                       &
                fire_upwinding_reinit, fire_lsm_reinit_iter, &
                fire_lsm_band_ngp,                 &
                lfn_in,                            & 
                lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3, & 
                lfn_out,tign,                      &
                fire_print_msg) 


      implicit none

      !*************************************************************************************!
      !*** Level-set function reinitialization following:                                ***!
      !*** Sussman, Smereka, Osher. Journal of Computational Physics 114, 146-159 (1994) ***!
      !*** implemented by Domingo Munoz-Esparza (NCAR/RAL, April 2016)                   ***!
      !*** Reference: D. Munoz-Esparza, B. Kosovic, P. Jimenez, J. Coen: "An accurate    ***!
      !*** fire-spread algorithm in the Weather Research and Forecasting model using the ***!
      !*** level-set method", Journal of Advances in Modeling Earth Systems, 2018        ***!
      !*** https://doi.org/10.1002/2017MS001108                                          ***!
      !*************************************************************************************!

      integer,intent(in)::ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme
      integer,intent(in)::ifds,ifde,jfds,jfde
      integer,intent(in)::fire_upwinding_reinit,fire_lsm_reinit_iter,fire_lsm_band_ngp
      real,dimension(ifms:ifme,jfms:jfme),intent(inout)::lfn_in,tign
      real,dimension(ifms:ifme,jfms:jfme),intent(inout)::lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3
      real,dimension(ifms:ifme,jfms:jfme),intent(inout)::lfn_out
      real,intent(in)::dx,dy,ts,dt
      integer,intent(in)::fire_print_msg

      real::dt_s
      real,dimension(ifts:ifte,jfts:jfte):: tend_1,tend_2,tend_3
      real::diffLx,diffLy,diffRx,diffRy,diff2x,diff2y,grad,time_now
      integer::nts,i,j,k,kk
      intrinsic epsilon
      character(len=128)::msg
      integer::itso,iteo,jtso,jteo
      real::threshold_HLl,threshold_HLu
      integer,parameter::bdy_eno1=10


      threshold_HLl=-fire_lsm_band_ngp*dx 
      threshold_HLu=fire_lsm_band_ngp*dx

        ! Define S0 based on current lfn values
      do j=jfts,jfte 
        do i=ifts,ifte 
            lfn_s0(i,j) = lfn_2(i,j)/sqrt(lfn_2(i,j)**2.0+dx**2.0) 
            lfn_s3(i,j) = lfn_2(i,j)
        enddo
      enddo

      call Continue_at_boundary(1,1, &   !extend by extrapolation but never down
          ifms,ifme,jfms,jfme, &                             ! memory dims
          ifds,ifde,jfds,jfde, &                             ! domain
          ifts,ifte,jfts,jfte, &                             ! tile
          itso,iteo,jtso,jteo, &                             ! where set now
          lfn_s3,fire_print_msg)

      dt_s=0.0001*dx
      do nts=1,fire_lsm_reinit_iter ! iterate to solve to steady state (1 iter each time step is enoguh)
          ! Runge-Kutta step 1 !
    
        call advance_ls_reinit( &
            ifms,ifme,jfms,jfme,    &           
            ifds,ifde,jfds,jfde,    &           
            ifts,ifte,jfts,jfte,    &               
            dx,dy,dt_s,bdy_eno1,threshold_HLu, &               
            lfn_s0,lfn_s3,lfn_s3,lfn_s1,1.0/3.0, &   ! sign funcition, initial ls, current stage ls, next stage advanced ls, RK coefficient
            fire_upwinding_reinit)

        call Continue_at_boundary(1,1, &
            ifms,ifme,jfms,jfme, &
        ifds,ifde,jfds,jfde, &
        ifts,ifte,jfts,jfte, &
        itso,iteo,jtso,jteo, &
        lfn_s1,fire_print_msg)

          ! Runge-Kutta step 2 !

        call advance_ls_reinit( &
            ifms,ifme,jfms,jfme,    &
            ifds,ifde,jfds,jfde,    &
            ifts,ifte,jfts,jfte,    &
            dx,dy,dt_s,bdy_eno1,threshold_HLu, &
            lfn_s0,lfn_s3,lfn_s1,lfn_s2,1.0/2.0, &
            fire_upwinding_reinit)

        call Continue_at_boundary(1,1, &
            ifms,ifme,jfms,jfme, &
            ifds,ifde,jfds,jfde, &
            ifts,ifte,jfts,jfte, &
            itso,iteo,jtso,jteo, &
            lfn_s2,fire_print_msg)

          ! Runge-Kutta step 3 !

        call advance_ls_reinit( &
            ifms,ifme,jfms,jfme,    &
            ifds,ifde,jfds,jfde,    &
            ifts,ifte,jfts,jfte,    &
            dx,dy,dt_s,bdy_eno1,threshold_HLu, &
            lfn_s0,lfn_s3,lfn_s2,lfn_s3,1.0, &
            fire_upwinding_reinit) 

        call Continue_at_boundary(1,1, &
            ifms,ifme,jfms,jfme, &
            ifds,ifde,jfds,jfde, &
            ifts,ifte,jfts,jfte, &
            itso,iteo,jtso,jteo, &
            lfn_s3,fire_print_msg)

      end do ! end iterations for steady state solution of reinitialization PDE

      do j=jfts,jfte 
        do i=ifts,ifte 
          lfn_out(i,j)=lfn_s3(i,j) ! assing to lfn_out the reinitialized level-set function
          lfn_out(i,j)=min(lfn_out(i,j),lfn_in(i,j)) ! fire area can only increase
        enddo
      enddo

    end subroutine reinit_ls_rk3

    subroutine advance_ls_reinit(ifms,ifme,jfms,jfme, &
                          ifds,ifde,jfds,jfde,    &
                          ifts,ifte,jfts,jfte,    &
                          dx,dy,dt_s,bdy_eno1,threshold_HLu, &
                          lfn_s0,lfn_ini,lfn_curr,lfn_fin,rk_coeff, &
                          fire_upwinding_reinit)

      ! Calculates right-hand-side forcing and advances a RK-stage the level-set reinitialization PDE
      ! Domingo Munoz-Esparza, NCAR/RAL
      ! Reference: D. Munoz-Esparza, B. Kosovic, P. Jimenez, J. Coen: "An accurate   
      ! fire-spread algorithm in the Weather Research and Forecasting model using the 
      ! level-set method", Journal of Advances in Modeling Earth Systems, 2018      
      ! https://doi.org/10.1002/2017MS001108

      implicit none

      integer,intent(in)::ifms,ifme,jfms,jfme,ifts,ifte,jfts,jfte,ifds,ifde,jfds,jfde,bdy_eno1
      integer,intent(in)::fire_upwinding_reinit
      real,dimension(ifms:ifme,jfms:jfme),intent(in)::lfn_s0,lfn_ini,lfn_curr
      real,dimension(ifms:ifme,jfms:jfme),intent(inout)::lfn_fin
      real,intent(in)::dx,dy,dt_s,threshold_HLu,rk_coeff

      integer::i,j
      real::diffLx,diffLy,diffRx,diffRy,diff2x,diff2y,grad,tend_r


      do j=jfts,jfte 
        do i=ifts,ifte 
          if (i.lt.ifds+bdy_eno1 .OR. i.gt.ifde-bdy_eno1 .OR. j.lt.jfds+bdy_eno1 .OR. j.gt.jfde-bdy_eno1) then ! 
            diffLx=(lfn_curr(i,j)-lfn_curr(i-1,j))/dx
            diffLy=(lfn_curr(i,j)-lfn_curr(i,j-1))/dy
            diffRx=(lfn_curr(i+1,j)-lfn_curr(i,j))/dx
            diffRy=(lfn_curr(i,j+1)-lfn_curr(i,j))/dy
            diff2x=select_eno(diffLx,diffRx)
            diff2y=select_eno(diffLy,diffRy)
          else
            select case(fire_upwinding_reinit)
            case(1)
             diff2x=select_4th(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i+1,j),lfn_curr(i+2,j))
             diff2y=select_4th(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j+1),lfn_curr(i,j+2))
             diff2x=select_weno3(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i+1,j),lfn_curr(i+2,j), &
                 lfn_s0(i,j)*diff2x)
             diff2y=select_weno3(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j+1),lfn_curr(i,j+2), &
               lfn_s0(i,j)*diff2y)
            case(2)
             diff2x=select_4th(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i+1,j),lfn_curr(i+2,j))
             diff2y=select_4th(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j+1),lfn_curr(i,j+2))
             diff2x=select_weno5(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i-3,j),lfn_curr(i+1,j), &
                 lfn_curr(i+2,j),lfn_curr(i+3,j),lfn_s0(i,j)*diff2x)
             diff2y=select_weno5(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j-3),lfn_curr(i,j+1), &
                 lfn_curr(i,j+2),lfn_curr(i,j+3),lfn_s0(i,j)*diff2y)
            case(3)
             if (lfn_curr(i,j).lt.threshold_HLu) then
               diff2x=select_4th(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i+1,j),lfn_curr(i+2,j))
               diff2y=select_4th(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j+1),lfn_curr(i,j+2))
               diff2x=select_weno3(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i+1,j),lfn_curr(i+2,j), &
                   lfn_s0(i,j)*diff2x)
               diff2y=select_weno3(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j+1),lfn_curr(i,j+2), &
                   lfn_s0(i,j)*diff2y)
             else
               diffLx=(lfn_curr(i,j)-lfn_curr(i-1,j))/dx
               diffLy=(lfn_curr(i,j)-lfn_curr(i,j-1))/dy
               diffRx=(lfn_curr(i+1,j)-lfn_curr(i,j))/dx
               diffRy=(lfn_curr(i,j+1)-lfn_curr(i,j))/dy
               diff2x=select_eno(diffLx,diffRx)
               diff2y=select_eno(diffLy,diffRy)
             endif
            case(4)
             if (lfn_curr(i,j).lt.threshold_HLu) then
               diff2x=select_4th(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i+1,j),lfn_curr(i+2,j))
               diff2y=select_4th(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j+1),lfn_curr(i,j+2))
               diff2x=select_weno5(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i-3,j),lfn_curr(i+1,j), &
                   lfn_curr(i+2,j),lfn_curr(i+3,j),lfn_s0(i,j)*diff2x)
               diff2y=select_weno5(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j-3),lfn_curr(i,j+1), &
                   lfn_curr(i,j+2),lfn_curr(i,j+3),lfn_s0(i,j)*diff2y)
             else
               diffLx=(lfn_curr(i,j)-lfn_curr(i-1,j))/dx
               diffLy=(lfn_curr(i,j)-lfn_curr(i,j-1))/dy
               diffRx=(lfn_curr(i+1,j)-lfn_curr(i,j))/dx
               diffRy=(lfn_curr(i,j+1)-lfn_curr(i,j))/dy
               diff2x=select_eno(diffLx,diffRx)
               diff2y=select_eno(diffLy,diffRy)
             endif
            case default
             if (lfn_curr(i,j).lt.threshold_HLu) then
               diff2x=select_4th(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i+1,j),lfn_curr(i+2,j))
               diff2y=select_4th(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j+1),lfn_curr(i,j+2))
               diff2x=select_weno5(dx,lfn_curr(i,j),lfn_curr(i-1,j),lfn_curr(i-2,j),lfn_curr(i-3,j),lfn_curr(i+1,j), &
                   lfn_curr(i+2,j),lfn_curr(i+3,j),lfn_s0(i,j)*diff2x)
               diff2y=select_weno5(dy,lfn_curr(i,j),lfn_curr(i,j-1),lfn_curr(i,j-2),lfn_curr(i,j-3),lfn_curr(i,j+1), &
                   lfn_curr(i,j+2),lfn_curr(i,j+3),lfn_s0(i,j)*diff2y)
             else
               diffLx=(lfn_curr(i,j)-lfn_curr(i-1,j))/dx
               diffLy=(lfn_curr(i,j)-lfn_curr(i,j-1))/dy
               diffRx=(lfn_curr(i+1,j)-lfn_curr(i,j))/dx
               diffRy=(lfn_curr(i,j+1)-lfn_curr(i,j))/dy
               diff2x=select_eno(diffLx,diffRx)
               diff2y=select_eno(diffLy,diffRy)
             endif
            end select
          endif
            grad=sqrt(diff2x*diff2x+diff2y*diff2y)
            tend_r=lfn_s0(i,j)*(1.0-grad)
            lfn_fin(i,j)=lfn_ini(i,j)+(dt_s*rk_coeff)*tend_r
        enddo
      enddo

    end subroutine advance_ls_reinit

    subroutine tign_update(ifts,ifte,jfts,jfte, &              ! tile dims
                       ifms,ifme,jfms,jfme, &              ! memory dims
                       ifds,jfds,ifde,jfde, &              ! domain dims         
                       ts,dt,boundary_guard,&              ! scalars in
                       fire_print_msg,      &
                       lfn_in,lfn_out,tign  &              ! arrays inout          
                      )

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      integer,intent(in)::ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,ifds,jfds,ifde,jfde
      integer,intent(in)::boundary_guard,fire_print_msg
      real,dimension(ifms:ifme,jfms:jfme),intent(inout)::tign
      real,dimension(ifms:ifme,jfms:jfme),intent(in)::lfn_in,lfn_out
      real,intent(in)::ts,dt
      real::time_now
      integer::i,j,k,kk
      intrinsic epsilon
      character(len=128)::msg


      ! compute ignition time by interpolation
      ! the node was not burning at start but it is burning at end
      ! interpolate from the level functions at start and at end
      ! lfn_in==lfn   is the level set function value at time ts
      ! lfn_out  is the level set function value at time ts+dt (after reinitialization)
      ! 0        is the level set function value at time tign(i,j)
      ! thus assuming the level function is approximately linear =>
      ! tign(i,j)= ts + ((ts + td) - ts) * lfn_in / (lfn_in - lfn_out)
      !        = ts + dt * lfn_in / (lfn_in - lfn_out)
    
      time_now=ts+dt
      time_now = time_now + abs(time_now)*epsilon(time_now)*2.
      do j=jfts,jfte
          do i=ifts,ifte
              ! interpolate the cross-over time
              if (.not. lfn_out(i,j)>0 .and. lfn_in(i,j)>0)then
                  tign(i,j) = ts + dt * lfn_in(i,j) / (lfn_in(i,j) - lfn_out(i,j))
          endif
              ! set the ignition time outside of burning region
              if(lfn_out(i,j)>0.)tign(i,j)=time_now
          enddo
      enddo

        ! stop simulation if fire is within boundary_guard grid points from the domain boundaries 
      do j=jfts,jfte
        if (j.le.boundary_guard .or. j.gt.(jfde-boundary_guard)) then
          do i=ifts,ifte
            if (lfn_out(i,j).lt.0.) then 
              write (OUTPUT_UNIT, *) 'j-boundary reached'
              write (OUTPUT_UNIT, *) 'i, j, lfn_out = ', i, j, lfn_out(i, j)
              call crash ('wrf: SUCCESS COMPLETE WRF. Fire has reached domain boundary.')
            endif
          enddo
        endif
      enddo
      do i=ifts,ifte
        if (i.le.boundary_guard .or. i.gt.(ifde-boundary_guard)) then
          do j=jfts,jfte
            if (lfn_out(i,j).lt.0.) then 
              write (OUTPUT_UNIT, *) 'j-boundary reached'
              write (OUTPUT_UNIT, *) 'i, j, lfn_out = ', i, j, lfn_out(i, j)
              call crash ('wrf: SUCCESS COMPLETE WRF. Fire has reached domain boundary.')
            endif
          enddo
        endif
      enddo
    
    end subroutine tign_update

    subroutine tend_ls( &
        ids,ide,jds,jde,    &              ! domain - nodes where lfn defined
        its,ite,jts,jte,    &              ! region - nodes where tend computed
        ims,ime,jms,jme,    &              ! memory dims for ros 
        t,dt,dx,dy,         &              ! scalars in
        fire_upwinding, &
        fire_viscosity, fire_viscosity_bg, &
        fire_viscosity_band, fire_viscosity_ngp, &
        fire_advection, fire_slope_factor,       &
        fire_lsm_band_ngp, fire_print_msg,  &
        lfn,                &              ! arrays in
        tbound,             &              ! scalars out 
        tend, ros,          &              ! arrays out
        grid,               &
        ros_model &
        )

      implicit none

      ! purpose
      ! compute the right hand side of the level set equation

      integer,intent(in)::ims,ime,jms,jme,its,ite,jts,jte
      integer, intent(in)::ids,ide,jds,jde
      integer, intent(in):: fire_upwinding,fire_viscosity_ngp, &
                     fire_advection,fire_lsm_band_ngp,fire_print_msg
      real,intent(in)::fire_viscosity,fire_viscosity_bg,fire_viscosity_band,fire_slope_factor
      real,intent(in)::t                                 ! time
      real,intent(in)::dt,dx,dy                          ! mesh step
      real,dimension(ims:ime,jms:jme),intent(inout)::lfn ! level set function 
      real,dimension(ims:ime,jms:jme),intent(out)::tend  ! tendency (rhs of the level set pde)
      real,dimension(ims:ime,jms:jme),intent(out)::ros   ! rate of spread 
      real,intent(out)::tbound                           ! max allowed time step
      type (state_fire_t), target :: grid
      type (ros_wrffire_t), intent (in) :: ros_model

      real:: te,diffLx,diffLy,diffRx,diffRy, & 
         diffCx,diffCy,diff2x,diff2y,grad,rr, &
         ros_base,ros_wind,ros_slope,ros_back,advx,advy,scale,nvx,nvy, &
         speed,tanphi
      integer::i,j,itso,iteo,jtso,jteo
      character(len=128)msg

        ! constants
      real, parameter:: eps=epsilon(0.0)
      !intrinsic epsilon
      real, parameter:: zero=0.,one=1.,tol=100*eps, &
          safe=2.,rmin=safe*tiny(zero),rmax=huge(zero)/safe
      real :: diff2xn, diff2yn
      real :: a_valor, signo_x, signo_y
      real :: threshold_HLl,threshold_HLu
      real :: threshold_AV,fire_viscosity_var
      integer,parameter :: bdy_eno1=10

        ! f90 intrinsic function
      intrinsic max,min,sqrt,nint,tiny,huge


      threshold_HLl=-fire_lsm_band_ngp*dx 
      threshold_HLu=fire_lsm_band_ngp*dx
      threshold_AV=fire_viscosity_ngp*dx

      call Continue_at_boundary(1,1, &   !extend by extrapolation but never down
          ims,ime,jms,jme, &                                 ! memory dims
          ids,ide,jds,jde, &                                 ! domain
          its,ite,jts,jte, &                                 ! tile
          itso,iteo,jtso,jteo, &                             ! where set now
          lfn,fire_print_msg)                                               ! field 

      tbound=0    
      do j=jts,jte
        do i=its,ite
            ! one sided differences
            diffRx = (lfn(i+1,j)-lfn(i,j))/dx
            diffLx = (lfn(i,j)-lfn(i-1,j))/dx
            diffRy = (lfn(i,j+1)-lfn(i,j))/dy
            diffLy = (lfn(i,j)-lfn(i,j-1))/dy
            diffCx = diffLx+diffRx   ! 	TWICE CENTRAL DIFFERENCE
            diffCy = diffLy+diffRy

            if (i.lt.ids+bdy_eno1 .OR. i.gt.ide-bdy_eno1 .OR. j.lt.jds+bdy_eno1 .OR. j.gt.jde-bdy_eno1) then ! DME: use eno1 near domain boundaries
              diff2x=select_eno(diffLx,diffRx)
              diff2y=select_eno(diffLy,diffRy)
              grad=sqrt(diff2x*diff2x + diff2y*diff2y)
            else
              select case(fire_upwinding)
              case(0)  ! none
                  grad=sqrt(diffCx**2 + diffCy**2)
              case(1) ! standard
                  diff2x=select_upwind(diffLx,diffRx)
                  diff2y=select_upwind(diffLy,diffRy)
                  grad=sqrt(diff2x*diff2x + diff2y*diff2y)
              case(2) ! godunov per osher/fedkiw
                  diff2x=select_godunov(diffLx,diffRx)
                  diff2y=select_godunov(diffLy,diffRy)
                  grad=sqrt(diff2x*diff2x + diff2y*diff2y)
              case(3) ! eno
                  diff2x=select_eno(diffLx,diffRx)
                  diff2y=select_eno(diffLy,diffRy)
                  grad=sqrt(diff2x*diff2x + diff2y*diff2y)
              case(4) ! Sethian - twice stronger pushdown of bumps
                  grad=sqrt(max(diffLx,0.)**2+min(diffRx,0.)**2   &
                          + max(diffLy,0.)**2+min(diffRy,0.)**2)
              case(5) ! 2nd order (DME)
                  diff2x=select_2nd(rr,dx,lfn(i,j),lfn(i-1,j),lfn(i+1,j))
                  diff2y=select_2nd(rr,dy,lfn(i,j),lfn(i,j-1),lfn(i,j+1))
                  grad=sqrt(diff2x*diff2x + diff2y*diff2y)
              case(6) ! 3rd-order WENO (DME)
                  a_valor=select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))*grid%uf(i,j)+ &
                          select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))*grid%vf(i,j)
                  signo_x=a_valor*select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))
                  signo_y=a_valor*select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))
                  diff2x=select_weno3(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j),signo_x)
                  diff2y=select_weno3(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2),signo_y)
                  grad=sqrt(diff2x*diff2x + diff2y*diff2y)
              case(7) ! 5th-order WENO (DME)
                  a_valor=select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))*grid%uf(i,j)+ &
                          select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))*grid%vf(i,j)
                  signo_x=a_valor*select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))
                  signo_y=a_valor*select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))
                  diff2x=select_weno5(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i-3,j),lfn(i+1,j),lfn(i+2,j),lfn(i+3,j),signo_x)
                  diff2y=select_weno5(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j-3),lfn(i,j+1),lfn(i,j+2),lfn(i,j+3),signo_y)
                  grad=sqrt(diff2x*diff2x + diff2y*diff2y)
              case(8) ! high-order local band method WENO3/ENO1 (DME)
                  if (abs(lfn(i,j)).lt.threshold_HLu) then
                    a_valor=select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))*grid%uf(i,j)+ &
                            select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))*grid%vf(i,j)
                    signo_x=a_valor*select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))
                    signo_y=a_valor*select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))
                    diff2x=select_weno3(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j),signo_x)
                    diff2y=select_weno3(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2),signo_y)
                    grad=sqrt(diff2x*diff2x + diff2y*diff2y)
                  else
                    diff2x=select_eno(diffLx,diffRx)
                    diff2y=select_eno(diffLy,diffRy)
                    grad=sqrt(diff2x*diff2x + diff2y*diff2y)
                  endif
              case(9) ! high-order local band method WENO5/ENO1 (DME)
                  if (abs(lfn(i,j)).lt.threshold_HLu) then
                    a_valor=select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))*grid%uf(i,j)+ &
                            select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))*grid%vf(i,j)
                    signo_x=a_valor*select_4th(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i+1,j),lfn(i+2,j))
                    signo_y=a_valor*select_4th(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))
                    diff2x=select_weno5(dx,lfn(i,j),lfn(i-1,j),lfn(i-2,j),lfn(i-3,j),lfn(i+1,j),lfn(i+2,j),lfn(i+3,j),signo_x)
                    diff2y=select_weno5(dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j-3),lfn(i,j+1),lfn(i,j+2),lfn(i,j+3),signo_y)
                    grad=sqrt(diff2x*diff2x + diff2y*diff2y)
                  else
                    diff2x=select_eno(diffLx,diffRx)
                    diff2y=select_eno(diffLy,diffRy)
                    grad=sqrt(diff2x*diff2x + diff2y*diff2y)
                  endif
              case default
                   grad=0.
              end select
            endif
              ! DME use the same discretization scheme from fire_upwinding option to calculate the normals
            scale=sqrt(grad**2.0+eps) 
            nvx=diff2x/scale 
            nvy=diff2y/scale 

            ! wind speed in direction of spread
            speed =  grid%uf(i,j)*nvx + grid%vf(i,j)*nvy
            ! get rate of spread from wind speed and slope
            call ros_model%Calc_ros (ros_base, ros_wind, ros_slope, &
            nvx, nvy, i, j, grid, fire_advection)
            rr=ros_base + ros_wind + fire_slope_factor*ros_slope

            if (FIRE_GROWS_ONLY) rr = max (rr, 0.0)
            ros(i, j) = rr

            te = -rr*grad   ! normal term 

            ! cfl condition
            if (grad > 0.) then
                 tbound = max(tbound,rr*(abs(diff2x)/dx+abs(diff2y)/dy)/grad)
            endif

            ! DME: adds spatially variable artificial viscosity
            if (abs(lfn(i,j)).lt.threshold_AV .AND. (i.gt.ids+bdy_eno1 .and. i.lt.ide-bdy_eno1) .AND. &
                (j.gt.jds+bdy_eno1 .and. j.lt.jde-bdy_eno1)) then 
              fire_viscosity_var=fire_viscosity_bg
            elseif (abs(lfn(i,j)).ge.threshold_AV .AND. abs(lfn(i,j)).lt.threshold_AV*(1.0+fire_viscosity_band) .AND. &
                (i.gt.ids+10 .and. i.lt.ide-10) .AND. (j.gt.jds+10 .and. j.lt.jde-10)) then
              fire_viscosity_var=min(fire_viscosity_bg+(fire_viscosity-fire_viscosity_bg)* &
                  (abs(lfn(i,j))-threshold_AV)/(fire_viscosity_band*threshold_AV),fire_viscosity)
            else
              fire_viscosity_var=fire_viscosity
            endif
            te=te + fire_viscosity_var*abs(rr)*((diffRx-diffLx)+(diffRy-diffLy))
            tend(i,j)=te
        enddo
      enddo        

        ! the final CFL bound
      tbound = 1/(tbound+tol)

    end subroutine tend_ls

    pure function Select_upwind (diff_lx, diff_rx) result (return_value)

      ! upwind differences, L or R if both same sign, otherwise zero

      implicit none

      real, intent (in) :: diff_lx, diff_rx
      real :: return_value

      real :: diff2x


      diff2x = 0.0
      if (diff_lx > 0.0 .and. diff_rx > 0.0) diff2x = diff_lx
      if (diff_lx < 0.0 .and. diff_rx < 0.0) diff2x = diff_rx

      return_value = diff2x

    end function Select_upwind

    pure function Select_godunov (diff_lx, diff_rx) result (return_value)

      ! Godunov scheme: upwind differences, L or R or none    
      ! always test on > or < never = , much faster because of IEEE
      ! central diff >= 0 => take left diff if >0, ortherwise 0
      ! central diff <= 0 => take right diff if <0, ortherwise 0

      implicit none

      real, intent (in) :: diff_lx, diff_rx
      real :: return_value

      real :: diff2x, diff_cx


      diff2x = 0.0
      diff_cx = diff_rx + diff_lx
      if (diff_lx > 0.0 .and. .not. diff_cx < 0.0) diff2x = diff_lx
      if (diff_rx < 0.0 .and.       diff_cx < 0.0) diff2x = diff_rx

      return_value = diff2x

    end function select_godunov

    pure function Select_eno (diff_lx, diff_rx) result (return_value)

      ! 1st order ENO scheme

      implicit none

      real, intent (in):: diff_lx, diff_rx
      real :: return_value

      real :: diff2x


      if (.not. diff_lx > 0.0 .and. .not. diff_rx > 0.0) then
        diff2x = diff_rx
      else if (.not. diff_lx < 0.0 .and. .not. diff_rx < 0.0) then
        diff2x = diff_lx
      else if (.not. diff_lx < 0.0 .and. .not. diff_rx > 0.0) then
        if (.not. abs (diff_rx) < abs(diff_lx)) then
          diff2x = diff_rx
        else
          diff2x = diff_lx
        end if
      else
        diff2x = 0.0
      end if

      return_value = diff2x

    end function Select_eno
      
    pure function Select_2nd (ros, dx, lfn_i, lfn_im1, lfn_ip1) result (return_value)

      ! 2nd-order advection scheme in the x,y-direction (DME)

      implicit none

      real, intent(in):: lfn_i, lfn_im1, lfn_ip1
      real, intent(in):: ros, dx
      real :: return_value

      real  :: diff2x_p, diff2x_m


      diff2x_p = 0.0
      diff2x_m = 0.0
      diff2x_p = (lfn_ip1 + lfn_i) / (2.0 * dx)
      diff2x_m = (lfn_i + lfn_im1) / (2.0 * dx)
      return_value = diff2x_p - diff2x_m

    end function Select_2nd

    pure function Select_4th (dx, lfn_i, lfn_im1, lfn_im2, lfn_ip1, lfn_ip2) &
        result (return_value)

      ! 4th-order advection scheme in the x,y-direction (DME)

      implicit none

      real, intent(in) :: dx, lfn_i, lfn_im1, lfn_im2, lfn_ip1, lfn_ip2
      real :: return_value

      real :: diff2x_p, diff2x_m


      diff2x_p = 0.0
      diff2x_m = 0.0
      diff2x_p = (7.0 * lfn_ip1 + 7.0 * lfn_i - lfn_ip2 - lfn_im1) / (12.0 * dx)
      diff2x_m = (7.0 * lfn_i + 7.0 * lfn_im1 - lfn_ip1 - lfn_im2) / (12.0 * dx)
      return_value = diff2x_p - diff2x_m

    end function Select_4th

    pure function Select_weno3 (dx, lfn_it, lfn_im1t, lfn_im2t, lfn_ip1t, &
        lfn_ip2t, uf) result (return_value)

      ! 3rd-order advection WENO scheme in the x,y-direction (DME)

      implicit none

      real, intent(in) :: dx, lfn_it, lfn_im1t, lfn_im2t, lfn_ip1t, lfn_ip2t, uf
      real :: return_value

      real, parameter :: GAMMA1 = 1.0 / 3.0, GAMMA2 = 2.0 / 3.0, TOL = 1e-6
      real :: lfn_i, lfn_im1, lfn_im2, lfn_ip1, lfn_ip2, flux_p,flux_m, &
          w1, w2, w1t, w2t, beta1, beta2, fh_1, fh_2


      if (uf >= 0.0) then
        lfn_i = lfn_it
        lfn_im1 = lfn_im1t
        lfn_im2 = lfn_im2t
        lfn_ip1 = lfn_ip1t
      else
        lfn_i = lfn_it
        lfn_im1 = lfn_ip1t
        lfn_im2 = lfn_ip2t
        lfn_ip1 = lfn_im1t
      end if

        ! numerical flux at i,j+1/2 face
      fh_1 = -0.5 * lfn_im1 + 1.5 * lfn_i
      fh_2 = 0.5 * lfn_i + 0.5 * lfn_ip1
      beta1 = (lfn_i - lfn_im1) ** 2
      beta2 = (lfn_ip1 - lfn_i) ** 2
      w1t = GAMMA1 / (beta1 + TOL) ** 2
      w2t = GAMMA2 / (beta2 + TOL) ** 2
      w1 = w1t / (w1t + w2t)
      w2 = w2t / (w1t + w2t)
      flux_p = w1 * fh_1 + w2 * fh_2
        ! numerical flux at i,j-1/2 face
      fh_1 = -0.5 * lfn_im2 + 1.5 * lfn_im1
      fh_2 = 0.5 * lfn_im1 + 0.5 * lfn_i
      beta1 = (lfn_im1 - lfn_im2) ** 2
      beta2 = (lfn_i - lfn_im1) ** 2
      w1t = GAMMA1 / (beta1 + TOL) ** 2
      w2t = GAMMA2 / (beta2 + TOL) ** 2
      w1 = w1t / (w1t + w2t)
      w2 = w2t / (w1t + w2t)
      flux_m = w1 * fh_1 + w2 * fh_2

      if (uf >= 0.0 ) then
        return_value = (flux_p - flux_m) / dx
      else
        return_value = (flux_m - flux_p) / dx
      end if

    end function Select_weno3

    pure function Select_weno5 (dx, lfn_it, lfn_im1t, lfn_im2t, lfn_im3t, lfn_ip1t, &
        lfn_ip2t, lfn_ip3t, uf) result (return_value)

      ! 5th-order advection WENO scheme in the x,y-direction (DME)

      implicit none

      real, intent(in):: dx, lfn_it, lfn_im1t, lfn_im2t, lfn_im3t, lfn_ip1t, lfn_ip2t, lfn_ip3t, uf
      real :: return_value

      real, parameter:: GAMMA1 = 1.0 / 10.0, GAMMA2 = 3.0 / 5.0, GAMMA3 = 3.0 / 10.0, TOL = 1e-6
      real :: lfn_i, lfn_im1, lfn_im2, lfn_im3, lfn_ip1, lfn_ip2, lfn_ip3,  flux_p, flux_m , &
          w1, w2, w3, w1t, w2t, w3t, beta1, beta2, beta3, fh_1, fh_2, fh_3


      if (uf >= 0.0) then
        lfn_i = lfn_it
        lfn_im1 = lfn_im1t
        lfn_im2 = lfn_im2t
        lfn_im3 = lfn_im3t
        lfn_ip1 = lfn_ip1t
        lfn_ip2 = lfn_ip2t
      else
        lfn_i = lfn_it
        lfn_im1 = lfn_ip1t
        lfn_im2 = lfn_ip2t
        lfn_im3 = lfn_ip3t
        lfn_ip1 = lfn_im1t
        lfn_ip2 = lfn_im2t
      endif

        ! numerical flux at i,j+1/2 face
      fh_1 = (2.0 * lfn_im2 - 7.0 * lfn_im1 + 11.0 * lfn_i) / 6.0
      fh_2 = (-1.0 * lfn_im1 + 5.0 * lfn_i + 2.0 * lfn_ip1) / 6.0
      fh_3 = (2.0 * lfn_i + 5.0 * lfn_ip1 - 1.0 * lfn_ip2) / 6.0
      beta1 = (13.0 / 12.0) * (lfn_im2 - 2.0 * lfn_im1 + lfn_i) ** 2 + &
          0.25 * (lfn_im2 - 4.0 * lfn_im1 + 3.0 * lfn_i) ** 2
      beta2 = (13.0 / 12.0) * (lfn_im1 - 2.0 * lfn_i + lfn_ip1) ** 2 + &
          0.25 * (lfn_im1 - lfn_ip1) ** 2
      beta3 = (13.0 / 12.0) * (lfn_i - 2.0 * lfn_ip1 + lfn_ip2) ** 2 + &
          0.25 * (3.0 * lfn_i - 4.0 * lfn_ip1 + lfn_ip2) ** 2
      w1t = GAMMA1 / (beta1 + TOL) ** 2
      w2t = GAMMA2 / (beta2 + TOL) ** 2
      w3t = GAMMA3 / (beta3 + TOL) ** 2
      w1 = w1t / (w1t + w2t + w3t)
      w2 = w2t / (w1t + w2t + w3t)
      w3 = w3t / (w1t + w2t + w3t)
      flux_p = w1 * fh_1 + w2 * fh_2 + w3 * fh_3

        ! numerical flux at i,j-1/2 face
      fh_1 = (2.0 * lfn_im3 - 7.0 * lfn_im2 + 11.0 * lfn_im1) / 6.0
      fh_2 = (-1.0 * lfn_im2 + 5.0 * lfn_im1 + 2.0 * lfn_i) / 6.0
      fh_3 = (2.0 * lfn_im1 + 5.0 * lfn_i - 1.0 * lfn_ip1) / 6.0
      beta1 = (13.0 / 12.0) * (lfn_im3 - 2.0 * lfn_im2 + lfn_im1) ** 2 + &
          0.25 * (lfn_im3 - 4.0 * lfn_im2 + 3.0 * lfn_im1) ** 2
      beta2 = (13.0 / 12.0) * (lfn_im2 - 2.0 * lfn_im1 + lfn_i) ** 2 + &
          0.25 * (lfn_im2 - lfn_i) ** 2
      beta3 = (13.0 / 12.0) * (lfn_im1 - 2.0 * lfn_i + lfn_ip1) ** 2 + &
          0.25 * (3.0 * lfn_im1 - 4.0 * lfn_i + lfn_ip1) ** 2
      w1t = GAMMA1 / (beta1 + TOL) ** 2
      w2t = GAMMA2 / (beta2 + TOL) ** 2
      w3t = GAMMA3 / (beta3 + TOL) ** 2
      w1 = w1t / (w1t + w2t + w3t)
      w2 = w2t / (w1t + w2t + w3t)
      w3 = w3t / (w1t + w2t + w3t)
      flux_m = w1 * fh_1 + w2 * fh_2 + w3 * fh_3

      if (uf >= 0.0) then
        return_value = (flux_p - flux_m) / dx
      else
        return_value = (flux_m - flux_p) / dx
      endif

    end function Select_weno5

    subroutine Continue_at_boundary(ix,iy, & ! do x direction or y direction
          ims,ime,jms,jme, &                ! memory dims
          ids,ide,jds,jde, &                ! domain dims
          its,ite,jts,jte, &                ! tile dims
          itso,iteo,jtso,jteo, &            ! tile dims where set
          lfn,fire_print_msg)               ! array

      implicit none

      !*** description
      ! extend array by one beyond the domain by linear continuation
      !*** arguments
      integer, intent(in)::ix,iy              ! not 0 = do x or y (1 or 2) direction
      integer, intent(in)::ims,ime,jms,jme, & ! memory dims
          ids,ide,jds,jde,                  & ! domain dims
          its,ite,jts,jte                     ! tile dims
      integer, intent(out)::itso,jtso,iteo,jteo ! where set
      real,intent(inout),dimension(ims:ime,jms:jme)::lfn
      integer, intent(in)::fire_print_msg

      integer i,j
      character(len=128)::msg
      integer::its1,ite1,jts1,jte1
      integer,parameter::halo=1           ! only 1 domain halo is needed since ENO1 is used near domain boundaries

      ! for dislay only
      itso=its
      jtso=jts
      iteo=ite
      jteo=jte
      ! go halo width beyond if at patch boundary but not at domain boudnary
      ! assume we have halo need to compute the value we do not have
      ! the next thread that would conveniently computer the extended values at patch corners
      ! besides halo may not transfer values outside of the domain
      !
      its1=its
      jts1=jts
      ite1=ite
      jte1=jte
      if(.not.its.eq.ids)its1=its-halo
      if(.not.jts.eq.jds)jts1=jts-halo
      if(.not.ite.eq.ide)ite1=ite+halo
      if(.not.jte.eq.jde)jte1=jte+halo
      if(ix.ne.0)then
        if(its.eq.ids)then
            do j=jts1,jte1
                lfn(ids-1,j)=EX(lfn(ids,j),lfn(ids+1,j))
            enddo
            itso=ids-1
        endif
        if(ite.eq.ide)then
            do j=jts1,jte1
                lfn(ide+1,j)=EX(lfn(ide,j),lfn(ide-1,j))
            enddo
            iteo=ide+1
        endif
      !$OMP CRITICAL(FIRE_UTIL_CRIT)
        write(msg,'(8(a,i5))')'Continue_at_boundary: x:',its,':',ite,',',jts,':',jte,' ->',itso,':',iteo,',',jts1,':',jte1
        call message(msg, fire_print_msg)
      !$OMP END CRITICAL(FIRE_UTIL_CRIT)
      endif
      if(iy.ne.0)then
        if(jts.eq.jds)then
            do i=its1,ite1
                lfn(i,jds-1)=EX(lfn(i,jds),lfn(i,jds+1))
            enddo
            jtso=jds-1
        endif
        if(jte.eq.jde)then
            do i=its1,ite1
                lfn(i,jde+1)=EX(lfn(i,jde),lfn(i,jde-1))
            enddo
            jteo=jde+1
        endif
      !$OMP CRITICAL(FIRE_UTIL_CRIT)
        write(msg,'(8(a,i5))')'Continue_at_boundary: y:',its,':',ite,',',jts,':',jte,' ->',its1,':',ite1,',',jtso,':',jteo
      !$OMP END CRITICAL(FIRE_UTIL_CRIT)
        call message(msg, fire_print_msg)
      endif
      ! corners of the domain
      if(ix.ne.0.and.iy.ne.0)then
        if(its.eq.ids.and.jts.eq.jds)lfn(ids-1,jds-1)=EX(lfn(ids,jds),lfn(ids+1,jds+1))
        if(its.eq.ids.and.jte.eq.jde)lfn(ids-1,jde+1)=EX(lfn(ids,jde),lfn(ids+1,jde-1))
        if(ite.eq.ide.and.jts.eq.jds)lfn(ide+1,jds-1)=EX(lfn(ide,jds),lfn(ide-1,jds+1))
        if(ite.eq.ide.and.jte.eq.jde)lfn(ide+1,jde+1)=EX(lfn(ide,jde),lfn(ide-1,jde-1))
      endif

      return

    contains

      pure function EX (a,b) result (return_value)

        implicit none

        real, intent (in) :: a, b

        real :: return_value
        real, parameter :: BIAS = 1.0

          ! extrapolation, max quarded
        return_value = (1.0 - BIAS) * (2.0 * a - b) + BIAS * max (2.0 * a - b, a, b)

      end function EX

    end subroutine Continue_at_boundary

  end module module_fr_fire_core

