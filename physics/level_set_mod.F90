
  module level_set_mod

  ! References:
  !
  ! Based on S. Osher and R. Fedkiw, Level set methods and dynamic implicit surfaces,
  ! Springer, 2003, Sec. 6.4, as implemented in toolboxLS for Matlab by
  ! I. Mitchell, A toolbox of Level Set Methods (Version 1.1), TR-2007-11,
  ! Dept. Computer Science, University of British Columbia, 2007
  ! http://www.cs.ubc.ca/\~mitchell/Toolbo\LS
  !
  ! D. Munoz-Esparza, B. Kosovic, P. Jimenez, J. Coen: "An accurate
  ! fire-spread algorithm in the Weather Research and Forecasting model using the
  ! level-set method", Journal of Advances in Modeling Earth Systems, 2018
  ! https://doi.org/10.1002/2017MS001108

    use ros_wrffire_mod, only: ros_wrffire_t
    use stderrout_mod, only: Crash, Message, Stop_simulation
    use state_mod, only: state_fire_t
    use ignition_line_mod, only : ignition_line_t
    use ros_wrffire_mod, only : ros_wrffire_t
    use constants_mod, only : MSG_LEN

    implicit none

    private

    public :: Fuel_left, Update_ignition_times, Reinit_level_set, Prop_level_set, Extrapol_var_at_bdys, Stop_if_close_to_bdy

    logical, parameter :: FIRE_GROWS_ONLY = .true.
    integer, parameter :: BDY_ENO1 = 10, SLOPE_FACTOR = 1.0

  contains

    subroutine Fuel_left (ims, ime, jms, jme, its, ite, jts, jte, ifs, ife, jfs, jfe, &
        lfn, tign, fuel_time, time_now, fuel_frac, fire_area, fire_print_msg)

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
 
             call Fuel_left_cell_1 (fuel_left_ff, fire_area_ff, &
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
                call Crash(msg)
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
                     call Crash (msg)
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
      call Message (msg, fire_print_msg)

      return
    end subroutine Fuel_left

    subroutine Fuel_left_cell_1( fuel_frac_left, fire_frac_area, &
        lfn00, lfn01, lfn10, lfn11, tign00, tign01, tign10, tign11, &
        time_now, fuel_time_cell, fire_print_msg)

      implicit none

      integer, intent(in) :: fire_print_msg
      real, intent(out) :: fuel_frac_left, fire_frac_area
      real, intent(in) :: lfn00, lfn01, lfn10, lfn11     ! level set function at 4 corners of the cell
      real, intent(in) :: tign00, tign01, tign10, tign11 ! ignition time at the  4 corners of the cell
      real, intent(in) :: time_now                       ! the time now
      real, intent(in) :: fuel_time_cell                 ! time to burns off to 1/e

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

      real :: ps, aps, area, ta, out
      real :: t00, t01, t10, t11
      real, parameter :: SAFE = tiny (aps)
      character (len = 128) :: msg

        ! the following algorithm is a very crude approximation

        ! minus time since ignition, 0 if no ignition yet
        ! it is possible to have 0 in fire region when ignitin time falls in 
        ! inside the time step because lfn is updated at the beginning of the time step
      t00 = tign00 - time_now
      if (lfn00 > 0.0 .or. t00 > 0.0) t00 = 0.0
      t01 = tign01 - time_now
      if (lfn01 > 0.0 .or. t01 > 0.0) t01 = 0.0
      t10 = tign10 - time_now
      if (lfn10 > 0.0 .or. t10 > 0.0) t10 = 0.0
      t11 = tign11 - time_now
      if (lfn11 > 0.0 .or. t11 > 0.0) t11 = 0.0

        ! approximate burning area, between 0 and 1   
      ps = lfn00 + lfn01 + lfn10 + lfn11
      aps = abs (lfn00) + abs (lfn01) + abs (lfn10) + abs (lfn11)
      aps = max (aps, SAFE)
      area = (-ps / aps + 1.0) / 2.0
      area = max (area, 0.0) ! make sure area is between 0 and 1
      area = min (area, 1.0)
    
        ! average negative time since ignition
      ta = 0.25 * (t00 + t01 + t10 + t11)

        ! exp decay in the burning area
      out = 1.0
      !if(area>0.)out=1. - area*(1. - exp(ta/fuel_time_cell))
      if (area > 0) out = area * exp (ta / fuel_time_cell) + (1.0 - area)

      if(out > 1.0)then
        !$OMP CRITICAL(FIRE_CORE_CRIT)
        write (msg, *) 'out=', out, '>1 area=', area,' ta=', ta
        call Message (msg, fire_print_msg)
        write(msg,*)'tign=', tign00, tign01, tign10, tign11,' time_now=', time_now
        !$OMP END CRITICAL(FIRE_CORE_CRIT)
        call Message(msg, fire_print_msg)
        !call Message('WARNING: fuel_left_cell_1: fuel fraction > 1')
        call Crash ('fuel_left_cell_1: fuel fraction > 1')
      end if

      !out = max(out,0.) ! make sure out is between 0 and 1
      !out = min(out,1.)

      fuel_frac_left = out
      fire_frac_area = area

    end subroutine Fuel_left_cell_1

    subroutine Prop_level_set (ifds, ifde, jfds, jfde, ifms, ifme, jfms, jfme, &
        ifts, ifte, jfts, jfte, ts, dt, dx, dy, fire_upwinding, fire_viscosity, &
        fire_viscosity_bg, fire_viscosity_band, fire_viscosity_ngp, fire_lsm_band_ngp, &
        tbound, lfn_in, lfn_0, lfn_1, lfn_2, lfn_out, tign, ros, grid, ros_model)

      ! Purpose: Advance the level set function from time ts to time ts + dt

      implicit none
      
      integer, intent(in) :: ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, ifts, ifte, jfts, jfte, &
          fire_upwinding, fire_viscosity_ngp, fire_lsm_band_ngp
      real, intent(in) :: fire_viscosity, fire_viscosity_bg, fire_viscosity_band
      real, dimension(ifms:ifme, jfms:jfme), intent (in out) :: lfn_in, tign, lfn_1, lfn_2, lfn_0
      real, dimension(ifms:ifme, jfms:jfme), intent (out) :: lfn_out, ros
      real, intent (in) :: dx, dy, ts, dt
      real, intent (out) :: tbound
      type (state_fire_t) :: grid
      type (ros_wrffire_t), intent (in) :: ros_model

        ! to store tendency (rhs of the level set pde)
      real, dimension(ifms:ifme, jfms:jfme) :: tend
      real :: tbound2, tbound3
      integer :: i, j
      character (len = MSG_LEN) :: msg


        ! Runge-Kutta step 1
      do j = jfts, jfte
        do i = ifts, ifte
          lfn_0(i, j) = lfn_in(i, j)
        end do
      end do

      call Calc_tend_ls (ifds, ifde, jfds, jfde, ifts, ifte, jfts, jfte, &
          ifms, ifme, jfms, jfme, ts, dt, dx, dy, fire_upwinding, &
          fire_viscosity, fire_viscosity_bg, fire_viscosity_band, &
          fire_viscosity_ngp, fire_lsm_band_ngp, lfn_0, tbound, tend, ros, grid, ros_model)

      do j = jfts, jfte 
        do i = ifts, ifte 
          lfn_1(i, j) = lfn_0(i, j) + (dt / 3.0) * tend(i, j)
        end do
      end do

        ! Runge-Kutta step 2
     call Calc_tend_ls (ifds, ifde, jfds, jfde, ifts, ifte, jfts, jfte, &
         ifms,ifme,jfms,jfme, ts + dt, dt, dx, dy, fire_upwinding, &
         fire_viscosity, fire_viscosity_bg, fire_viscosity_band, &
         fire_viscosity_ngp, fire_lsm_band_ngp, lfn_1, tbound2, tend, ros, grid, ros_model)

      do j = jfts, jfte
        do i = ifts, ifte
          lfn_2(i, j) = lfn_0(i, j) + (dt / 2.0) * tend(i, j)
        end do
      end do

        ! Runge-Kutta step 3
     call Calc_tend_ls (ifds,ifde,jfds,jfde, ifts, ifte, jfts, jfte, &
         ifms, ifme, jfms, jfme, ts + dt, dt, dx, dy, fire_upwinding, &
         fire_viscosity, fire_viscosity_bg, fire_viscosity_band, &
         fire_viscosity_ngp, fire_lsm_band_ngp, lfn_2, tbound3, tend, ros, grid, ros_model)

      do j = jfts, jfte
        do i = ifts, ifte
          lfn_out(i, j) = lfn_0(i, j) + dt * tend(i, j)
        end do
      end do     

        ! CFL check, tbound is the max allowed time step
      tbound = min (tbound, tbound2, tbound3)

      if (dt > tbound) then
        !$omp critical
        write (msg, '(2(a, f10.2))') 'CFL violation: time step ', dt, ' > bound =', tbound
        call Stop_simulation (msg)
        !$omp end critical
      end if
    
    end subroutine Prop_level_set

    subroutine Reinit_level_set (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, &
        ifds, ifde, jfds, jfde, ts, dt, dx, dy, fire_upwinding_reinit, &
        fire_lsm_reinit_iter, fire_lsm_band_ngp, lfn_in, lfn_2, lfn_s0, &
        lfn_s1, lfn_s2, lfn_s3, lfn_out, tign, fire_print_msg) 

    ! Purpose: Level-set function reinitialization
    !
    ! Referencess:
    ! Sussman, Smereka, Osher. Journal of Computational Physics 114, 146-159 (1994)
    !
    ! D. Munoz-Esparza, B. Kosovic, P. Jimenez, J. Coen: "An accurate
    ! fire-spread algorithm in the Weather Research and Forecasting model using the
    ! level-set method", Journal of Advances in Modeling Earth Systems, 2018
      

      implicit none

      integer, intent (in) :: ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme
      integer, intent (in) :: ifds, ifde, jfds, jfde
      integer, intent (in) :: fire_upwinding_reinit, fire_lsm_reinit_iter, fire_lsm_band_ngp
      real, dimension (ifms:ifme, jfms:jfme), intent (in out) :: lfn_in, tign
      real, dimension (ifms:ifme, jfms:jfme), intent (in out) :: lfn_2, lfn_s0, lfn_s1, lfn_s2, lfn_s3
      real, dimension (ifms:ifme, jfms:jfme), intent (in out) :: lfn_out
      real, intent (in) :: dx, dy, ts, dt
      integer, intent (in) :: fire_print_msg

      real :: dt_s, threshold_hlu
      integer :: nts, i, j


      threshold_hlu = fire_lsm_band_ngp * dx

        ! Define S0 based on current lfn values
      do j = jfts, jfte 
        do i = ifts, ifte 
          lfn_s0(i, j) = lfn_out(i,j) / sqrt (lfn_out(i, j) ** 2.0 + dx ** 2.0)
          lfn_s3(i, j) = lfn_out(i,j)
        end do
      end do

      call Extrapol_var_at_bdys (ifms, ifme, jfms, jfme, ifds, ifde, &
          jfds, jfde, ifts, ifte, jfts, jfte, lfn_s3)

      dt_s = 0.0001 * dx
        ! iterate to solve to steady state reinit PDE
        ! 1 iter each time step is enoguh
      do nts = 1, fire_lsm_reinit_iter
          ! Runge-Kutta step 1
        call Advance_ls_reinit (ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, &
            ifts, ifte, jfts, jfte, dx, dy, dt_s, threshold_hlu, &
            lfn_s0, lfn_s3, lfn_s3, lfn_s1, 1.0 / 3.0, & ! sign funcition, initial ls, current stage ls, next stage advanced ls, RK coefficient
            fire_upwinding_reinit)

        call Extrapol_var_at_bdys (ifms, ifme, jfms, jfme, ifds, ifde, &
            jfds, jfde, ifts, ifte, jfts, jfte, lfn_s1)

          ! Runge-Kutta step 2
        call Advance_ls_reinit (ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, &
            ifts, ifte, jfts, jfte, dx, dy, dt_s, threshold_hlu, &
            lfn_s0, lfn_s3, lfn_s1, lfn_s2, 1.0 / 2.0, &
            fire_upwinding_reinit)

        call Extrapol_var_at_bdys (ifms, ifme, jfms, jfme, ifds, ifde, &
            jfds, jfde, ifts, ifte, jfts, jfte, lfn_s2)

          ! Runge-Kutta step 3
        call Advance_ls_reinit (ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, &
            ifts, ifte, jfts, jfte, dx, dy, dt_s, threshold_hlu, &
            lfn_s0, lfn_s3, lfn_s2, lfn_s3, 1.0, &
            fire_upwinding_reinit) 

        call Extrapol_var_at_bdys (ifms, ifme, jfms, jfme, ifds, ifde, &
            jfds,jfde,  ifts, ifte, jfts, jfte, lfn_s3)
      end do

      do j = jfts, jfte 
        do i = ifts, ifte 
            ! assing to lfn_out the reinitialized level-set function
          lfn_out(i, j) = lfn_s3(i, j)
            ! fire area can only increase
          lfn_out(i, j) = min (lfn_out(i, j), lfn_in(i, j))
        end do
      end do

    end subroutine Reinit_level_set

    subroutine Advance_ls_reinit (ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, &
        ifts, ifte, jfts, jfte, dx, dy, dt_s, threshold_hlu, lfn_s0, &
        lfn_ini, lfn_curr, lfn_fin, rk_coeff, fire_upwinding_reinit)

      ! Calculates right-hand-side forcing and advances a RK-stage the level-set reinitialization PDE

      implicit none

      integer, intent (in) :: ifms, ifme, jfms, jfme, ifts, ifte, jfts, &
          jfte, ifds, ifde, jfds, jfde
      integer, intent (in) :: fire_upwinding_reinit
      real, dimension (ifms:ifme, jfms:jfme), intent (in) :: lfn_s0, lfn_ini, lfn_curr
      real, dimension (ifms:ifme, jfms:jfme), intent (in out) :: lfn_fin
      real, intent (in) :: dx, dy, dt_s, threshold_hlu, rk_coeff

      integer :: i, j
      real :: diffLx, diffLy, diffRx, diffRy, diff2x, diff2y, grad, tend_r


      do j = jfts, jfte 
        do i = ifts, ifte 
          if (i < ifds + BDY_ENO1 .or. i > ifde - BDY_ENO1 .or. &
              j < jfds + BDY_ENO1 .or. j > jfde - BDY_ENO1) then
            diffLx = (lfn_curr(i, j) - lfn_curr(i - 1, j)) / dx
            diffLy = (lfn_curr(i, j) - lfn_curr(i, j - 1)) / dy
            diffRx = (lfn_curr(i + 1, j) - lfn_curr(i, j)) / dx
            diffRy = (lfn_curr(i, j + 1) - lfn_curr(i, j)) / dy
            diff2x = Select_eno (diffLx, diffRx)
            diff2y = Select_eno (diffLy, diffRy)
          else
            select case (fire_upwinding_reinit)
              case (1)
                diff2x = Select_4th (dx, lfn_curr(i, j), lfn_curr(i - 1, j), &
                    lfn_curr(i - 2, j), lfn_curr(i + 1, j), lfn_curr(i + 2, j))
                diff2y = Select_4th (dy, lfn_curr(i, j), lfn_curr(i, j - 1), &
                    lfn_curr(i, j - 2), lfn_curr(i, j + 1), lfn_curr(i, j + 2))
                diff2x = Select_weno3 (dx, lfn_curr(i, j), lfn_curr(i - 1, j), &
                    lfn_curr(i - 2, j), lfn_curr(i + 1, j), lfn_curr(i + 2, j), &
                    lfn_s0(i, j) * diff2x)
                diff2y = Select_weno3 (dy, lfn_curr(i, j), lfn_curr(i, j - 1), &
                    lfn_curr(i, j - 2), lfn_curr(i, j + 1), lfn_curr(i, j + 2), &
                    lfn_s0(i, j) * diff2y)

              case (2)
                diff2x = Select_4th (dx, lfn_curr(i, j), lfn_curr(i - 1, j), & 
                    lfn_curr(i - 2, j), lfn_curr(i + 1, j), lfn_curr(i + 2, j))
                diff2y = Select_4th (dy, lfn_curr(i, j), lfn_curr(i, j - 1), &
                    lfn_curr(i, j - 2), lfn_curr(i, j + 1), lfn_curr(i, j + 2))
                diff2x = Select_weno5 (dx, lfn_curr(i, j), lfn_curr(i - 1, j), &
                    lfn_curr(i - 2, j), lfn_curr(i - 3, j), lfn_curr(i + 1, j), &
                    lfn_curr(i + 2, j), lfn_curr(i + 3, j), lfn_s0(i, j) * diff2x)
                diff2y = Select_weno5 (dy, lfn_curr(i, j), lfn_curr(i, j - 1), &
                    lfn_curr(i, j - 2), lfn_curr(i, j - 3), lfn_curr(i, j + 1), &
                    lfn_curr(i, j + 2), lfn_curr(i, j + 3), lfn_s0(i, j) * diff2y)

              case (3)
                if (lfn_curr(i, j) < threshold_hlu) then
                  diff2x = Select_4th (dx, lfn_curr(i, j), lfn_curr(i - 1, j), &
                      lfn_curr(i - 2, j), lfn_curr(i + 1, j), lfn_curr(i + 2, j))
                  diff2y = Select_4th (dy, lfn_curr(i, j), lfn_curr(i, j - 1), & 
                      lfn_curr(i, j - 2), lfn_curr(i, j + 1), lfn_curr(i, j + 2))
                  diff2x = Select_weno3 (dx, lfn_curr(i, j), lfn_curr(i - 1, j), &
                      lfn_curr(i - 2, j), lfn_curr(i + 1, j), lfn_curr(i + 2, j), &
                      lfn_s0(i, j) * diff2x)
                  diff2y = Select_weno3 (dy, lfn_curr(i, j), lfn_curr(i, j - 1), &
                      lfn_curr(i, j - 2), lfn_curr(i, j + 1), lfn_curr(i, j + 2), &
                      lfn_s0(i, j) * diff2y)
                else
                  diffLx = (lfn_curr(i, j) - lfn_curr(i - 1, j)) / dx
                  diffLy = (lfn_curr(i, j) - lfn_curr(i, j - 1)) / dy
                  diffRx = (lfn_curr(i + 1, j) - lfn_curr(i, j)) / dx
                  diffRy = (lfn_curr(i, j + 1) - lfn_curr(i, j)) / dy
                  diff2x = Select_eno (diffLx, diffRx)
                  diff2y = Select_eno (diffLy, diffRy)
                endif

              case(4)
                if (lfn_curr(i, j) < threshold_hlu) then
                  diff2x = Select_4th (dx, lfn_curr(i, j), lfn_curr(i - 1, j), &
                      lfn_curr(i - 2, j), lfn_curr(i + 1, j), lfn_curr(i + 2, j))
                  diff2y = Select_4th (dy,lfn_curr(i, j), lfn_curr(i, j - 1), &
                      lfn_curr(i, j - 2), lfn_curr(i, j + 1), lfn_curr(i, j + 2))
                  diff2x = Select_weno5 (dx, lfn_curr(i, j), lfn_curr(i - 1, j), &
                      lfn_curr(i - 2, j), lfn_curr(i - 3, j), lfn_curr(i + 1, j), &
                      lfn_curr(i + 2, j), lfn_curr(i + 3, j), lfn_s0(i, j) * diff2x)
                  diff2y = Select_weno5 (dy, lfn_curr(i,j), lfn_curr(i, j - 1), &
                      lfn_curr(i, j - 2), lfn_curr(i, j - 3), lfn_curr(i, j + 1), &
                      lfn_curr(i, j + 2), lfn_curr(i, j + 3), lfn_s0(i, j) * diff2y)
                else
                  diffLx = (lfn_curr(i, j) - lfn_curr(i - 1, j)) / dx
                  diffLy = (lfn_curr(i, j) - lfn_curr(i, j - 1)) / dy
                  diffRx = (lfn_curr(i + 1, j) - lfn_curr(i, j)) / dx
                  diffRy = (lfn_curr(i, j + 1) - lfn_curr(i, j)) / dy
                  diff2x = Select_eno (diffLx, diffRx)
                  diff2y = Select_eno (diffLy, diffRy)
                endif

              case default
                if (lfn_curr(i,j) < threshold_hlu) then
                  diff2x = Select_4th (dx, lfn_curr(i, j), lfn_curr(i - 1, j), &
                      lfn_curr(i - 2, j), lfn_curr(i + 1, j), lfn_curr(i + 2, j))
                  diff2y = Select_4th (dy, lfn_curr(i, j), lfn_curr(i, j - 1), &
                      lfn_curr(i, j - 2), lfn_curr(i, j + 1), lfn_curr(i, j + 2))
                  diff2x = Select_weno5 (dx, lfn_curr(i, j), lfn_curr(i - 1, j), &
                      lfn_curr(i - 2, j), lfn_curr(i - 3, j), lfn_curr(i + 1, j), &
                      lfn_curr(i + 2, j), lfn_curr(i + 3, j), lfn_s0(i, j) * diff2x)
                  diff2y = Select_weno5 (dy, lfn_curr(i, j), lfn_curr(i, j - 1), &
                      lfn_curr(i, j - 2), lfn_curr(i, j - 3), lfn_curr(i, j + 1), &
                      lfn_curr(i, j + 2), lfn_curr(i, j + 3), lfn_s0(i, j) * diff2y)
                else
                  diffLx = (lfn_curr(i, j) - lfn_curr(i - 1, j)) / dx
                  diffLy = (lfn_curr(i, j) - lfn_curr(i, j - 1)) / dy
                  diffRx = (lfn_curr(i + 1, j) - lfn_curr(i, j)) / dx
                  diffRy = (lfn_curr(i, j + 1) - lfn_curr(i, j)) / dy
                  diff2x = Select_eno (diffLx, diffRx)
                  diff2y = Select_eno (diffLy, diffRy)
                end if

            end select
          end if
            grad = sqrt (diff2x * diff2x + diff2y * diff2y)
            tend_r = lfn_s0(i, j) * (1.0 - grad)
            lfn_fin(i, j) = lfn_ini(i, j) + (dt_s * rk_coeff) * tend_r
        end do
      end do

    end subroutine Advance_ls_reinit

    subroutine Update_ignition_times (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, &
        ifds, jfds, ifde, jfde, ts, dt, lfn_in, lfn_out, tign)

      ! Purpose: compute ignition time by interpolation.
      !          the node was not burning at start but it is burning at end

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      integer, intent (in) :: ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, &
          ifds, jfds, ifde, jfde
      real, intent(in) :: ts, dt
      real, dimension (ifms:ifme, jfms:jfme), intent (in) :: lfn_in, lfn_out
      real, dimension (ifms:ifme, jfms:jfme), intent (in out) :: tign
      real :: time_now
      integer :: i, j


      time_now = ts + dt
      time_now = time_now + abs (time_now) * epsilon (time_now) * 2.0
      do j = jfts, jfte
        do i = ifts, ifte
            ! interpolate the cross-over time
          if (lfn_out(i, j) <= 0.0 .and. lfn_in(i, j) > 0.0) then
            tign(i, j) = ts + dt * lfn_in(i, j) / (lfn_in(i, j) - lfn_out(i, j))
          end if
          if (lfn_out(i,j) > 0.0) tign(i, j) = time_now
        end do
      end do

    end subroutine Update_ignition_times

    subroutine Calc_tend_ls (ids, ide, jds, jde, its, ite, jts, jte, ims, ime, jms, jme, &
        t, dt, dx, dy, fire_upwinding, fire_viscosity, fire_viscosity_bg, &
        fire_viscosity_band, fire_viscosity_ngp, fire_lsm_band_ngp, lfn, tbound, tend, ros, grid, ros_model)

      ! compute the right hand side of the level set equation

      implicit none

      integer, intent (in) :: ims, ime, jms, jme, its, ite, jts, jte, ids, ide, jds, jde, &
          fire_upwinding,fire_viscosity_ngp, fire_lsm_band_ngp
      real, intent (in) :: fire_viscosity, fire_viscosity_bg, fire_viscosity_band, t, dt, dx, dy
      real, dimension(ims:ime, jms:jme), intent (in out) :: lfn
      real, dimension(ims:ime, jms:jme), intent (out) :: tend, ros
      real, intent (out) :: tbound
      type (state_fire_t) :: grid
      type (ros_wrffire_t), intent (in) :: ros_model

      real, parameter :: EPS = epsilon (0.0), TOL = 100.0 * EPS
      real :: difflx, diffly, diffrx, diffry, diffcx, diffcy, &
         diff2x, diff2y, grad, rr, ros_base, ros_wind, ros_slope, &
         scale, nvx, nvy, a_valor, signo_x, signo_y, threshold_hll, &
         threshold_hlu, threshold_av, fire_viscosity_var
      integer :: i, j
      character (len = MSG_LEN) :: msg


      threshold_hll = -fire_lsm_band_ngp * dx
      threshold_hlu = fire_lsm_band_ngp * dx
      threshold_av = fire_viscosity_ngp * dx

      call Extrapol_var_at_bdys (ims, ime, jms, jme, ids, ide, jds, jde, &
          its, ite, jts, jte, lfn)

      tbound = 0.0
      do j = jts, jte
        do i = its, ite
            ! one sided differences
          diffrx = (lfn(i + 1, j) - lfn(i, j)) / dx
          difflx = (lfn(i, j) - lfn(i - 1, j)) / dx
          diffry = (lfn(i, j + 1) - lfn(i, j)) / dy
          diffly = (lfn(i, j) - lfn(i, j - 1)) / dy
            ! twice central difference
          diffcx = difflx + diffrx
          diffcy = diffly + diffry
            ! use eno1 near domain boundaries
          if (i < ids + BDY_ENO1 .or. i > ide - BDY_ENO1 .or. &
              j < jds + BDY_ENO1 .or. j > jde - BDY_ENO1) then 
            diff2x = Select_eno (difflx, diffrx)
            diff2y = Select_eno (diffly, diffry)
            grad = sqrt (diff2x * diff2x + diff2y * diff2y)
          else
            select case (fire_upwinding)
                ! none
              case (0)
                grad = sqrt (diffcx ** 2 + diffcy ** 2)

                ! standard
              case (1)
                diff2x = Select_upwind (difflx, diffrx)
                diff2y = Select_upwind (diffly, diffry)
                grad = sqrt (diff2x * diff2x + diff2y * diff2y)

                ! godunov per osher/fedkiw
              case (2)
                diff2x = Select_godunov (difflx, diffrx)
                diff2y = Select_godunov (diffly, diffry)
                grad = sqrt (diff2x * diff2x + diff2y * diff2y)

                ! ENO1
              case (3)
                diff2x = Select_eno (difflx, diffrx)
                diff2y = Select_eno (diffly, diffry)
                grad = sqrt (diff2x * diff2x + diff2y * diff2y)

                ! Sethian - twice stronger pushdown of bumps
              case(4)
                grad = sqrt (max (difflx, 0.0) ** 2 + min (diffrx, 0.0) ** 2 &
                    + max (diffly, 0.0) ** 2 + min(diffry, 0.0) ** 2)
                ! 2nd order
              case(5)
                diff2x = Select_2nd (rr, dx, lfn(i, j), lfn(i - 1, j), lfn(i + 1, j))
                diff2y = Select_2nd (rr, dy, lfn(i, j), lfn(i, j - 1), lfn(i, j + 1))
                grad = sqrt (diff2x * diff2x + diff2y * diff2y)

                ! WENO3
              case(6)
                a_valor = Select_4th (dx, lfn(i, j), lfn(i - 1, j), lfn(i - 2, j), lfn(i + 1, j), lfn(i + 2, j)) * grid%uf(i, j) + &
                    Select_4th (dy, lfn(i, j), lfn(i, j - 1), lfn(i, j - 2), lfn(i, j + 1), lfn(i, j + 2)) * grid%vf(i, j)
                signo_x = a_valor * Select_4th (dx, lfn(i, j), lfn(i - 1, j), &
                    lfn(i - 2, j), lfn(i + 1, j), lfn(i + 2, j))
                signo_y = a_valor * Select_4th (dy, lfn(i, j), lfn(i, j - 1), &
                    lfn(i, j - 2), lfn(i, j + 1), lfn(i, j + 2))
                diff2x = Select_weno3 (dx, lfn(i, j), lfn(i - 1, j), lfn(i - 2, j), &
                    lfn(i + 1, j), lfn(i + 2, j), signo_x)
                diff2y = Select_weno3 (dy, lfn(i, j), lfn(i, j - 1), lfn(i, j - 2), & 
                    lfn(i, j + 1), lfn(i, j + 2), signo_y)
                grad = sqrt (diff2x * diff2x + diff2y * diff2y)

                ! WENO5
              case(7)
                a_valor = Select_4th (dx, lfn(i, j), lfn(i - 1, j), lfn(i - 2, j), lfn(i + 1, j), lfn(i + 2, j)) * grid%uf(i, j)+ &
                    Select_4th (dy, lfn(i, j), lfn(i, j - 1), lfn(i, j - 2), lfn(i, j + 1), lfn(i, j + 2)) * grid%vf(i, j)
                signo_x = a_valor * Select_4th (dx, lfn(i, j), lfn(i - 1, j), lfn(i - 2, j), lfn(i + 1, j), lfn(i + 2, j))
                signo_y = a_valor * Select_4th (dy,lfn(i,j),lfn(i,j-1),lfn(i,j-2),lfn(i,j+1),lfn(i,j+2))
                diff2x = Select_weno5 (dx, lfn(i, j), lfn(i - 1, j), lfn(i - 2, j), &
                    lfn(i - 3, j), lfn(i + 1, j), lfn(i + 2, j), lfn(i + 3, j), signo_x)
                diff2y = Select_weno5 (dy, lfn(i, j), lfn(i, j - 1), lfn(i, j - 2), &
                    lfn(i ,j - 3), lfn(i, j + 1), lfn(i, j + 2), lfn(i, j + 3), signo_y)
                grad = sqrt (diff2x * diff2x + diff2y * diff2y)

                ! WENO3/ENO1
              case(8)
                if (abs (lfn(i, j)) < threshold_hlu) then
                  a_valor = Select_4th (dx, lfn(i, j), lfn(i - 1, j), lfn(i - 2, j), lfn(i + 1, j), lfn(i + 2, j)) * grid%uf(i, j) + &
                      Select_4th (dy, lfn(i, j), lfn(i, j - 1), lfn(i, j - 2), lfn(i, j + 1), lfn(i, j + 2)) * grid%vf(i, j)
                  signo_x = a_valor * Select_4th (dx, lfn(i, j), lfn(i - 1, j), & 
                      lfn(i - 2, j), lfn(i + 1, j), lfn(i + 2, j))
                  signo_y = a_valor * Select_4th (dy, lfn(i, j), lfn(i, j - 1), &
                      lfn(i, j - 2), lfn(i, j + 1), lfn(i, j + 2))
                  diff2x = Select_weno3 (dx, lfn(i, j), lfn(i - 1, j), lfn(i - 2, j), &
                      lfn(i + 1, j), lfn(i + 2, j), signo_x)
                  diff2y = Select_weno3 (dy, lfn(i, j), lfn(i, j - 1), lfn(i, j - 2), &
                      lfn(i, j + 1), lfn(i, j + 2), signo_y)
                  grad = sqrt (diff2x * diff2x + diff2y * diff2y)
                else
                  diff2x = Select_eno (difflx, diffrx)
                  diff2y = Select_eno (diffly, diffry)
                  grad = sqrt (diff2x * diff2x + diff2y * diff2y)
                end if

                ! WENO5/ENO1
              case(9)
                if (abs (lfn(i, j)) < threshold_hlu) then
                  a_valor = Select_4th (dx, lfn(i, j), lfn(i - 1, j), lfn(i - 2, j), lfn(i + 1, j), lfn(i + 2, j)) * grid%uf(i, j) + &
                      Select_4th (dy,lfn(i, j), lfn(i, j - 1), lfn(i, j - 2), lfn(i, j + 1), lfn(i, j + 2)) * grid%vf(i, j)
                  signo_x = a_valor * Select_4th (dx, lfn(i, j), lfn(i - 1, j), &
                      lfn(i - 2, j), lfn(i + 1, j), lfn(i + 2, j))
                  signo_y = a_valor * Select_4th (dy, lfn(i, j), lfn(i, j - 1), &
                      lfn(i, j - 2), lfn(i, j + 1), lfn(i, j + 2))
                  diff2x = Select_weno5 (dx, lfn(i, j), lfn(i - 1, j), lfn(i - 2, j), &
                      lfn(i - 3, j), lfn(i + 1, j), lfn(i + 2, j), lfn(i + 3, j), signo_x)
                  diff2y = Select_weno5 (dy, lfn(i, j), lfn(i, j - 1), lfn(i, j - 2), &
                      lfn(i, j - 3), lfn(i, j + 1), lfn(i, j + 2), lfn(i, j + 3), signo_y)
                  grad = sqrt (diff2x * diff2x + diff2y * diff2y)
                else
                  diff2x = Select_eno (difflx, diffrx)
                  diff2y = Select_eno (diffly, diffry)
                  grad = sqrt (diff2x * diff2x + diff2y * diff2y)
                end if

              case default
                !$omp critical
                write (msg, '(a, i2)') 'Unknown upwinding option in level set : ', fire_upwinding
                call Stop_simulation (msg)
                !$omp end critical

            end select
          end if

            ! Calc normal
          scale = sqrt (grad ** 2.0 + EPS)
          nvx = diff2x / scale
          nvy = diff2y / scale

            ! Get rate of spread from wind speed and slope
          call ros_model%Calc_ros (ros_base, ros_wind, ros_slope, &
              nvx, nvy, i, j, grid)
          rr = ros_base + ros_wind + SLOPE_FACTOR * ros_slope
          if (FIRE_GROWS_ONLY) rr = max (rr, 0.0)
          ros(i, j) = rr

            ! CFL condition
          if (grad > 0.0) tbound = max (tbound, rr * (abs (diff2x) / dx + &
              abs (diff2y) / dy) / grad)

            ! Tendency level set function
          tend(i, j) = -rr * grad

            ! Add to tend effect Artificial viscosity
          if (abs (lfn(i,j)) < threshold_av .and. (i > ids + BDY_ENO1 .and. i < ide - BDY_ENO1) .and. &
              (j > jds + BDY_ENO1 .and. j < jde - BDY_ENO1)) then 
            fire_viscosity_var = fire_viscosity_bg
          else if (abs (lfn(i,j)) >= threshold_av .and. abs (lfn(i,j)) < threshold_av * (1.0 + fIre_viscosity_band) .and. &
              (i > ids + BDY_ENO1 .and. i < ide - BDY_ENO1) .and. (j > jds + BDY_ENO1 .and. j < jde - BDY_ENO1)) then
            fire_viscosity_var = min (fire_viscosity_bg + (fire_viscosity - fire_viscosity_bg) * &
                (abs (lfn(i, j)) - threshold_av) / (fire_viscosity_band * threshold_av), fire_viscosity)
          else
            fire_viscosity_var = fire_viscosity
          end if

          tend(i, j) = tend(i, j) + fire_viscosity_var * abs (rr) * ((diffrx - difflx) + (diffry - diffly))
        end do
      end do

        ! final CFL bound
      tbound = 1.0 / (tbound + TOL)

    end subroutine Calc_tend_ls

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

    end function Select_godunov

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
      end if

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
      end if

    end function Select_weno5

    subroutine Stop_if_close_to_bdy (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, &
        ifds, jfds, ifde, jfde, lfn_out)

      implicit none

      integer, intent (in) :: ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, &
          ifds, jfds, ifde, jfde
      real, dimension (ifms:ifme, jfms:jfme), intent (in) :: lfn_out

      integer, parameter :: BOUNDARY_GUARD = 8
      integer :: i, j


      do j = jfts, jfte
        if (j <= BOUNDARY_GUARD .or. j > (jfde - BOUNDARY_GUARD)) then
          do i = ifts, ifte
            if (lfn_out(i, j) < 0.0) call Stop_simulation ('Fire too close to domain boundary.')
          end do
        end if
      end do

      do i = ifts, ifte
        if (i <= BOUNDARY_GUARD .or. i > (ifde - BOUNDARY_GUARD)) then
          do j = jfts, jfte
            if (lfn_out(i, j) < 0.0) call Stop_simulation ('Fire too close to domain boundary.')
          end do
        end if
      end do

    end subroutine Stop_if_close_to_bdy

    subroutine Extrapol_var_at_bdys (ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, &
        ifts, ifte, jfts, jfte, var)

      ! extend 2D array beyond domain boundaries

      implicit none

      integer, intent (in) :: ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, ifts, ifte, jfts, jfte
      real, dimension (ifms:ifme, jfms:jfme), intent (in out) :: var

      integer :: i, j
      integer :: ifts1, ifte1, jfts1, jfte1

        ! only 1 is needed since ENO1 is 
        ! used near domain boundaries
      integer, parameter :: HALO = 1


        ! Go HALO width beyond if not a tile in a domain corner
        ! assume we have halo need to compute the value we do not have
      ifts1 = ifts
      jfts1 = jfts
      ifte1 = ifte
      jfte1 = jfte
      if (.not. ifts == ifds) ifts1 = ifts - HALO
      if (.not. jfts == jfds) jfts1 = jfts - HALO
      if (.not. ifte == ifde) ifte1 = ifte + HALO
      if (.not. jfte == jfde) jfte1 = jfte + HALO

      if (ifts == ifds)then
        do j = jfts1, jfte1
          var(ifds - 1, j) = Extrapol (var(ifds, j), var(ifds + 1, j))
        end do
      end if

      if (ifte == ifde) then
        do j = jfts1, jfte1
          var(ifde + 1, j) = Extrapol (var(ifde, j), var(ifde - 1, j))
        end do
      end if

      if (jfts == jfds) then
        do i = ifts1, ifte1
          var(i, jfds - 1) = Extrapol (var(i, jfds), var(i, jfds + 1))
        end do
      end if

      if (jfte == jfde) then
        do i = ifts1, ifte1
          var(i, jfde + 1) = Extrapol (var(i, jfde), var(i, jfde - 1))
        end do
      end if

        ! corners
      if (ifts == ifds .and. jfts == jfds) var(ifds - 1, jfds - 1) = Extrapol (var(ifds, jfds), var(ifds + 1, jfds + 1))
      if (ifts == ifds .and. jfte == jfde) var(ifds - 1, jfde + 1) = Extrapol (var(ifds, jfde), var(ifds + 1, jfde - 1))
      if (ifte == ifde .and. jfts == jfds) var(ifde + 1, jfds - 1) = Extrapol (var(ifde, jfds), var(ifde - 1, jfds + 1))
      if (ifte == ifde .and. jfte == jfde) var(ifde + 1, jfde + 1) = Extrapol (var(ifde, jfde), var(ifde - 1, jfde - 1))

    contains

      pure function Extrapol (a, b) result (return_value)

        implicit none

        real, intent (in) :: a, b

        real :: return_value
        real, parameter :: BIAS = 1.0

          ! extrapolation, max quarded
        return_value = (1.0 - BIAS) * (2.0 * a - b) + BIAS * max (2.0 * a - b, a, b)

      end function Extrapol

    end subroutine Extrapol_var_at_bdys

  end module level_set_mod

