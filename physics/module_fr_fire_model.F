
module module_fr_fire_model

  use module_fr_fire_core, only: ignition_line_type, ignition_line, &
                               Fuel_left, Tign_update, Calc_flame_length, Reinit_ls_rk3, &
                               Ignite_fire, Prop_ls_rk3
  use module_fr_fire_util, only: real_max, real_sum, rnrm_max, rnrm_sum, fun_real, &
                               Message, Print_2d_stats, Crash, Check_mesh_2dim
  use module_fr_fire_phys

  private

  public :: Fire_model

  contains

    subroutine fire_model (                    &
        num_ignitions,                          & ! number of ignitions before advancing
        ifds,ifde,jfds,jfde,                    & ! fire domain dims - the whole domain
        ifms,ifme,jfms,jfme,                    & ! fire memory dims - how declared
        ifts,ifte,jfts,jfte,                    & ! fire tile dims  - this thread
        time_start,dt,                          & ! time and increment
        fdx,fdy,                                & ! fire mesh spacing,
        ignition_line,                          & ! small array of ignition line descriptions
        ignitions_done,ignited_tile,            &
        coord_xf,coord_yf,unit_xf,unit_yf,      & ! fire mesh coordinates
        lfn,                                & ! state: level function
        lfn_hist,                           & ! PAJ: to init obs fire perimeter.
        fire_is_real_perim,                 & ! PAJ: to init obs fire perimeter.
        lfn_0,lfn_1,lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,flame_length,ros_front, & ! state
        lfn_out,tign,fuel_frac,fire_area,   & ! state: level function, ign time, fuel left, area burning
        burnt_area_dt,                          & 
        grnhfx,grnqfx,                          & ! output: heat fluxes
        ros,                                    & ! output: rate of spread
        fuel_time,                              & ! save derived internal data
        fp,                                     &
        fuel_left_method, fuel_left_irl, fuel_left_jrl, &
        boundary_guard, fire_grows_only, fire_upwinding, &
        fire_upwind_split, fire_upwinding_reinit, &
        fire_lsm_reinit, fire_lsm_reinit_iter, &
        fire_viscosity, fire_viscosity_bg, &
        fire_viscosity_band, fire_viscosity_ngp, &
        fire_advection, fire_slope_factor, &
        fire_lsm_band_ngp, fire_print_msg )

! This subroutine implements the fire spread model.
! All quantities are on the fire grid. It inputs
! winds given on the nodes of the fire grid
! and outputs the heat fluxes on the cells of the fire grid.
! This subroutine has no knowledge of any atmospheric model.
! This code was written to conform with the WRF parallelism model, however it
! does not depend on it. It can be called with domain equal to tile.
! Wind and height must be given on 1 more node beyond the domain bounds. 
! The subroutine changes only array entries of the arguments in the tile.
! Upon exit with ifun=2 (time step), lfn_out is to be copied into lfn by the caller.
! When this subroutine is used on separate tiles that make a domain the value, the
! it uses lfn on a strip of width 2 from neighboring tiles.
!
! All computation is done on one tile. 
!

      implicit none

integer, intent(in) :: num_ignitions        ! number of ignition lines
integer, intent(in) :: ifds,ifde,jfds,jfde  ! fire domain bounds
integer, intent(in) :: ifts,ifte,jfts,jfte  ! fire tile bounds         
integer, intent(in) :: ifms,ifme,jfms,jfme  ! fire memory array bounds
integer, intent(in) :: fuel_left_method,fuel_left_irl,fuel_left_jrl, &
                       boundary_guard,fire_grows_only,fire_upwinding, &
                       fire_upwind_split,fire_upwinding_reinit,fire_lsm_reinit_iter, &
                       fire_viscosity_ngp,fire_advection,fire_lsm_band_ngp, &
                       fire_print_msg
logical, intent(in) :: fire_lsm_reinit
real,intent(in) :: time_start,dt            ! starting time, time step
real,intent(in) :: fdx,fdy                  ! spacing of the fire mesh
real,intent(in)::fire_viscosity,fire_viscosity_bg,fire_viscosity_band,fire_slope_factor
type(ignition_line_type), dimension (num_ignitions), intent(in):: ignition_line ! descriptions of ignition lines
integer, intent(out):: ignited_tile(num_ignitions) ! number of gridpts ignited in a given ignition
integer, intent(out):: ignitions_done              ! number of isolated ignitions
real, dimension(ifms:ifme, jfms:jfme), intent(in):: & 
    coord_xf,coord_yf                       !  node coordinates  
real, intent(in):: unit_xf,unit_yf          !  coordinate units in m
  real, intent(in), dimension(ifms:ifme,jfms:jfme):: lfn_hist   
  logical, intent(in) :: fire_is_real_perim
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: &
    lfn   , &                               ! level function: fire is where lfn<0 (node)
    tign  , &                               ! absolute time of ignition (node)
    fuel_frac                               ! fuel fraction (node), currently redundant
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: &
    lfn_0,lfn_1,lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,flame_length,ros_front           ! level function stages
real, intent(out), dimension(ifms:ifme,jfms:jfme):: &
    fire_area                               ! fraction of each cell burning
real, intent(out), dimension(ifms:ifme,jfms:jfme):: &
    burnt_area_dt 
real, intent(out), dimension(ifms:ifme,jfms:jfme):: &
    lfn_out, &                              !                              
    grnhfx,grnqfx, &                        ! heat fluxes J/m^2/s  (cell)             
    ros                                     ! output: rate of spread
real, intent(inout),dimension(ifms:ifme,jfms:jfme):: fuel_time
type(fire_params),intent(inout)::fp

real, dimension(ifts:ifte,jfts:jfte)::fuel_frac_burnt,fuel_frac_end
integer::ignited,ig,i,j
real::tbound,tfa,thf,mhf,tqf,mqf,aw,mw
character(len=128)::msg
integer:: stat_lev=1
  real :: start_time_ig, end_time_ig
  real, parameter :: EPSILON = 0.00001


call check_mesh_2dim(ifts-1,ifte+1,jfts-1,jfte+1,ifms,ifme,jfms,jfme,fire_print_msg)

! init flags
ignitions_done=0

! ifun4: do the timestep
    if(fire_print_msg.ge.stat_lev)then
      aw=fun_real(RNRM_SUM,  &
        ifms,ifme,1,1,jfms,jfme, &                ! memory dims
        ifds,ifde,1,1,jfds,jfde, &                ! domain dims
        ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
        0,0,0,       &                            ! staggering
        fp%vx,fp%vy)/((ifde-ifds+1)*(jfde-jfds+1))
      mw=fun_real(RNRM_MAX,  &
        ifms,ifme,1,1,jfms,jfme, &                ! memory dims
        ifds,ifde,1,1,jfds,jfde, &                ! domain dims
        ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
        0,0,0,       &                            ! staggering
        fp%vx,fp%vy)
!$OMP MASTER 
      write(msg,91)time_start,'Average wind        ',aw,'m/s'
      call message(msg,fire_print_msg,stat_lev)
      write(msg,91)time_start,'Maximum wind        ',mw,'m/s'
      call message(msg,fire_print_msg,stat_lev)
!$OMP END MASTER 
    endif

    call print_2d_stats(ifts,ifte,jfts,jfte, &
                   ifms,ifme,jfms,jfme, &
                   fuel_frac,'model: fuel_frac start',fire_print_msg)

    ! advance the model from time_start to time_start+dt
    ! return the fuel fraction burnt this call in each fire cell
    ! will call module_fr_fire_speed::normal_spread for propagation speed
    ! We cannot simply compute the spread rate here because that will change with the
    ! angle of the wind and the direction of propagation, thus it is done in subroutine
    ! normal_spread at each fire time step. Instead, we pass arguments that 
    ! the speed function may use as fp. 

!   propagate level set function in time
!   set lfn_out tign
!   lfn does not change, tign has no halos

         call prop_ls_rk3(                              &
             ifds,ifde,jfds,jfde,                       & 
             ifms,ifme,jfms,jfme,                       &
             ifts,ifte,jfts,jfte,                       &
             time_start,dt,fdx,fdy,                     &
             fire_grows_only,                           &
             fire_upwinding,fire_upwind_split,          &
             fire_viscosity, fire_viscosity_bg,         &
             fire_viscosity_band, fire_viscosity_ngp,   &
             fire_advection, fire_slope_factor,         &
             fire_lsm_band_ngp, fire_print_msg,         &
             tbound,                                    &
             lfn,                                       &
             lfn_0,lfn_1,lfn_2,                         & 
             lfn_out,tign,ros, fp                       &
         ) 

       call tign_update(ifts,ifte,jfts,jfte, & 
                        ifms,ifme,jfms,jfme, &                    
                        ifds,jfds,ifde,jfde, &
                        time_start,dt,boundary_guard, &
                        fire_print_msg, &
                        lfn,lfn_out,tign     &             
                       )

       call calc_flame_length(ifts,ifte,jfts,jfte, & 
                              ifms,ifme,jfms,jfme, &
                              ros,fp%iboros,flame_length,ros_front,fire_area)

       if (fire_lsm_reinit) then ! DME added call to reinitialize level-set function
          
           call reinit_ls_rk3(                                       &
                          ifts,ifte,jfts,jfte,                       &                  
                          ifms,ifme,jfms,jfme,                       &                     
                          ifds,ifde,jfds,jfde,                       &                     
                          time_start,dt,fdx,fdy,                     &
                          fire_upwinding_reinit,fire_lsm_reinit_iter,&
                          fire_lsm_band_ngp,                         &
                          lfn,                                       &
                          lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,         &
                          lfn_out,tign                               &
                         )
       endif ! fire_lsm_reinit

! ifun5: copy the result of timestep back to input
    ! this cannot be done in the time step itself because of race condition
    ! some thread may still be using lfn as input in their tile halo

       do j=jfts,jfte
           do i=ifts,ifte
               lfn(i,j)=lfn_out(i,j)
               ! if want to try timestep again treat tign the same way here
               ! even if tign does not need a halo
           enddo
       enddo

    ! check for ignitions
  ig = 1
  start_time_ig = ignition_line(ig)%start_time 
  end_time_ig  = ignition_line(ig)%end_time

  if ( fire_is_real_perim .and. time_start >= start_time_ig .and. time_start < start_time_ig + dt) then
    ignited = 0
    do j = jfts, jfte
      do i = ifts, ifte
        lfn(i, j) = lfn_hist(i, j)
        if (abs(lfn(i, j)) < EPSILON) then
          tign(i, j)= time_start
          ignited = ignited + 1
        end if
      enddo
    enddo
  elseif (.not. fire_is_real_perim) then
    do ig = 1,num_ignitions
    
!  for now, check for ignition every time step...
!        if(ignition_line(ig)%end_time>=time_start.and.ignition_line(ig)%start_time<time_start+dt)then 
            call ignite_fire(                             &
                ifds,ifde,jfds,jfde,                      & ! fire domain dims - the whole domain
                ifms,ifme,jfms,jfme,                      &
                ifts,ifte,jfts,jfte,                      &
                fire_print_msg,                           &
                ignition_line(ig),                        &
                time_start,time_start+dt,                 &
                coord_xf,coord_yf,unit_xf,unit_yf,        & 
                lfn,tign,ignited)

            ignitions_done=ignitions_done+1
            ignited_tile(ignitions_done)=ignited
                
!        endif
        
    enddo
 end if
            
    call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme, &
                   lfn,'fire_model: lfn out',fire_print_msg)
    
!ifun6 ! timestep postprocessing
    ! compute the heat fluxes from the fuel burned
    ! needs lfn and tign from neighbors so halo must be updated before
    call fuel_left(&
        ifms,ifme,jfms,jfme, &
        ifts,ifte,jfts,jfte, &
        ifts,ifte,jfts,jfte, &
        lfn,tign,fuel_time,time_start+dt,fuel_frac_end,fire_area,fuel_left_method, &
        fuel_left_irl, fuel_left_jrl, fire_print_msg) !fuel_frac_end is private and tile based

    call print_2d_stats(ifts,ifte,jfts,jfte, &
                   ifts,ifte,jfts,jfte, &
                   fuel_frac_end,'model: fuel_frac end',fire_print_msg)
    
    do j=jfts,jfte
        do i=ifts,ifte
            fuel_frac_burnt(i,j)=fuel_frac(i,j)-fuel_frac_end(i,j) ! fuel lost this timestep
            burnt_area_dt(i,j)=fuel_frac_burnt(i,j)
            fuel_frac(i,j)=fuel_frac_end(i,j) ! copy new value to state array
        enddo
    enddo

    call print_2d_stats(ifts,ifte,jfts,jfte, &
                   ifts,ifte,jfts,jfte, &
                   fuel_frac_burnt,'model: fuel_frac burned',fire_print_msg)
        
    call heat_fluxes(dt,fp,                       &
        ifms,ifme,jfms,jfme,                      &
        ifts,ifte,jfts,jfte,                      &
        ifts,ifte,jfts,jfte,                      &  ! fuel_frac_burned is tile dimensioned
        fp%fgip,                                     &
        fuel_frac_burnt,                          & !
        grnhfx,grnqfx)                              !out

    if(fire_print_msg.ge.stat_lev)then
      tfa=fun_real(REAL_SUM,  &
        ifms,ifme,1,1,jfms,jfme, &                ! memory dims
        ifds,ifde,1,1,jfds,jfde, &                ! domain dims
        ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
        0,0,0,       &                            ! staggering
        fire_area,fire_area) * fdx * fdy
      thf=fun_real(REAL_SUM,  &
        ifms,ifme,1,1,jfms,jfme, &                ! memory dims
        ifds,ifde,1,1,jfds,jfde, &                ! domain dims
        ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
        0,0,0,       &                            ! staggering
        grnhfx,grnhfx) * fdx * fdy
      mhf=fun_real(REAL_MAX,  &
        ifms,ifme,1,1,jfms,jfme, &                ! memory dims
        ifds,ifde,1,1,jfds,jfde, &                ! domain dims
        ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
        0,0,0,       &                            ! staggering
        grnhfx,grnhfx) 
      tqf=fun_real(REAL_SUM,  &
        ifms,ifme,1,1,jfms,jfme, &                ! memory dims
        ifds,ifde,1,1,jfds,jfde, &                ! domain dims
        ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
        0,0,0,       &                            ! staggering
        grnqfx,grnqfx) * fdx * fdy
      mqf=fun_real(REAL_MAX,  &
        ifms,ifme,1,1,jfms,jfme, &                ! memory dims
        ifds,ifde,1,1,jfds,jfde, &                ! domain dims
        ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
        0,0,0,       &                            ! staggering
        grnqfx,grnqfx) 
!$OMP MASTER 
      write(msg,91)time_start,'Fire area           ',tfa,'m^2'
      call message(msg,fire_print_msg,stat_lev)
      write(msg,91)time_start,'Heat output         ',thf,'W'
      call message(msg,fire_print_msg,stat_lev)
      write(msg,91)time_start,'Max heat flux       ',mhf,'W/m^2'
      call message(msg,fire_print_msg,stat_lev)
      write(msg,91)time_start,'Latent heat output  ',tqf,'W'
      call message(msg,fire_print_msg,stat_lev)
      write(msg,91)time_start,'Max latent heat flux',mqf,'W/m^2'
      call message(msg,fire_print_msg,stat_lev)
!$OMP END MASTER
91  format('Time ',f11.3,' s ',a,e12.3,1x,a)
    endif

    call print_2d_stats(ifts,ifte,jfts,jfte, &
                   ifms,ifme,jfms,jfme, &
                   grnhfx,'model: heat flux(J/m^2/s)',fire_print_msg)

  end subroutine fire_model

end module module_fr_fire_model
