  module module_fr_fire_model

    use module_fr_fire_core, only: Fuel_left, Tign_update, Reinit_ls_rk3,  Ignite_fire, Prop_ls_rk3
    use module_fr_fire_util, only: REAL_MAX, REAL_SUM, RNRM_MAX, RNRM_SUM, Fun_real, Message, &
        Print_tile_stats, Crash, Check_mesh_2dim
    use module_fr_fire_phys, only: Calc_flame_length, Calc_fire_fluxes
    use ignition_line_mod, only: ignition_line_t
    use ros_wrffire_mod, only : ros_wrffire_t
    use state_mod, only: state_fire_t
    use namelist_mod, only : namelist_t

    private

    public :: Fire_model

  contains

    subroutine fire_model (config_flags, ros_model, ignition_line, grid)

    ! This subroutine implements the fire spread model.
    ! Wind and height must be given on 1 more node beyond the domain bounds. 
    ! it uses lfn on a strip of width 2 from neighboring tiles.

      implicit none

      type (namelist_t), intent (in) :: config_flags
      type (ros_wrffire_t), intent (in) :: ros_model
      type(ignition_line_t), dimension (:), intent(in):: ignition_line
      type (state_fire_t), intent (in out) :: grid

      real, dimension(grid%ifts:grid%ifte, grid%jfts:grid%jfte) :: fuel_frac_burnt, fuel_frac_end

      integer :: ifds,ifde,jfds,jfde, ifts,ifte,jfts,jfte, ifms,ifme,jfms,jfme
      integer :: ignited,ig,i,j
      real :: tbound,tfa,thf,mhf,tqf,mqf,aw,mw
      character (len = 128) :: msg
      integer :: stat_lev = 1
      real :: start_time_ig, end_time_ig, time_start
      real, parameter :: EPSILON = 0.00001
        ! number of isolated ignitions
      integer :: ignitions_done
        ! number of gridpts ignited in a given ignition
      integer :: ignited_tile(config_flags%fire_num_ignitions)

      
      ifds = grid%ifds
      ifde = grid%ifde
      jfds = grid%jfds
      jfde = grid%jfde

      ifts = grid%ifts
      ifte = grid%ifte
      jfts = grid%jfts
      jfte = grid%jfte

      ifms = grid%ifms
      ifme = grid%ifme
      jfms = grid%jfms
      jfme = grid%jfme

      call Check_mesh_2dim (ifts - 1, ifte + 1, jfts - 1, jfte + 1, ifms, ifme, jfms, jfme, config_flags%fire_print_msg)

        ! init flags
      ignitions_done = 0
      time_start = grid%itimestep * grid%dt

      if (config_flags%fire_print_msg.ge.stat_lev)then
        aw=Fun_real(RNRM_SUM,  &
            ifms,ifme,1,1,jfms,jfme, &              ! memory dims
          ifds,ifde,1,1,jfds,jfde, &                ! domain dims
          ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
          grid%uf,grid%vf)/((ifde-ifds+1)*(jfde-jfds+1))
        mw=Fun_real(RNRM_MAX,  &
            ifms,ifme,1,1,jfms,jfme, &                ! memory dims
            ifds,ifde,1,1,jfds,jfde, &                ! domain dims
            ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
            grid%uf,grid%vf)
        !$OMP MASTER 
        write (msg, 91) time_start,'Average wind        ',aw,'m/s'
        call Message (msg, config_flags%fire_print_msg,stat_lev)

        write (msg, 91) time_start,'Maximum wind        ',mw,'m/s'
        call Message (msg, config_flags%fire_print_msg,stat_lev)
        !$OMP END MASTER 
      end if

      call Print_tile_stats (ifts,ifte,jfts,jfte, ifms,ifme,jfms,jfme, &
          grid%fuel_frac,'model: fuel_frac start',config_flags%fire_print_msg)

        ! advance the model from time_start to time_start+dt
        ! return the fuel fraction burnt this call in each fire cell
        ! will call module_fr_fire_speed::normal_spread for propagation speed
        ! We cannot simply compute the spread rate here because that will change with the
        ! angle of the wind and the direction of propagation, thus it is done in subroutine
        ! normal_spread at each fire time step.

        !   propagate level set function in time
        !   set lfn_out tign
        !   lfn does not change, tign has no halos
      call prop_ls_rk3(                              &
             ifds,ifde,jfds,jfde,                       & 
             ifms,ifme,jfms,jfme,                       &
             ifts,ifte,jfts,jfte,                       &
             time_start,grid%dt,grid%dx,grid%dy,                     &
             config_flags%fire_grows_only,              &
             config_flags%fire_upwinding, config_flags%fire_upwind_split,          &
             config_flags%fire_viscosity, config_flags%fire_viscosity_bg,         &
             config_flags%fire_viscosity_band, config_flags%fire_viscosity_ngp,   &
             config_flags%fire_advection, config_flags%fire_slope_factor,         &
             config_flags%fire_lsm_band_ngp, config_flags%fire_print_msg,         &
             tbound,                                    &
             grid%lfn,                                       &
             grid%lfn_0,grid%lfn_1,grid%lfn_2,                         & 
             grid%lfn_out,grid%tign_g,grid%ros, grid,                    &
             ros_model) 

      call tign_update(ifts,ifte,jfts,jfte, & 
                        ifms,ifme,jfms,jfme, &                    
                        ifds,jfds,ifde,jfde, &
                        time_start,grid%dt, config_flags%fire_boundary_guard, &
                        config_flags%fire_print_msg, &
                        grid%lfn,grid%lfn_out, grid%tign_g     &             
                       )

      call Calc_flame_length (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, &
           grid%ros, grid%iboros, grid%flame_length, grid%ros_front, grid%fire_area)

      if (config_flags%fire_lsm_reinit) then ! DME added call to reinitialize level-set function
        call reinit_ls_rk3(                                       &
                          ifts,ifte,jfts,jfte,                       &                  
                          ifms,ifme,jfms,jfme,                       &                     
                          ifds,ifde,jfds,jfde,                       &                     
                          time_start,grid%dt,grid%dx,grid%dy,                     &
                          config_flags%fire_upwinding_reinit, config_flags%fire_lsm_reinit_iter,&
                          config_flags%fire_lsm_band_ngp,                         &
                          grid%lfn,                                       &
                          grid%lfn_2,grid%lfn_s0, grid%lfn_s1, grid%lfn_s2, grid%lfn_s3,         &
                          grid%lfn_out, grid%tign_g                               &
                         )
      end if

        ! ifun5: copy the result of timestep back to input
        ! this cannot be done in the time step itself because of race condition
        ! some thread may still be using lfn as input in their tile halo
      do j = jfts, jfte
        do i=ifts, ifte
          grid%lfn(i, j) = grid%lfn_out(i, j)
            ! if want to try timestep again treat tign the same way here
            ! even if tign does not need a halo
        end do
      end do

       ! check for ignitions
      ig = 1
      start_time_ig = ignition_line(ig)%start_time 
      end_time_ig  = ignition_line(ig)%end_time

      if (config_flags%fire_is_real_perim .and. time_start >= start_time_ig .and. time_start < start_time_ig + grid%dt) then
        ignited = 0
        do j = jfts, jfte
          do i = ifts, ifte
            grid%lfn(i, j) = grid%lfn_hist(i, j)
            if (abs(grid%lfn(i, j)) < EPSILON) then
              grid%tign_g(i, j) = time_start
              ignited = ignited + 1
            end if
          end do
        end do
      elseif (.not. config_flags%fire_is_real_perim) then
        do ig = 1,config_flags%fire_num_ignitions
            !  for now, check for ignition every time step...
            !        if(ignition_line(ig)%end_time>=time_start.and.ignition_line(ig)%start_time<time_start+dt)then 
          call ignite_fire(                             &
               ifds,ifde,jfds,jfde,                      & ! fire domain dims - the whole domain
               ifms,ifme,jfms,jfme,                      &
               ifts,ifte,jfts,jfte,                      &
               config_flags%fire_print_msg,                           &
               ignition_line(ig),                        &
               time_start,time_start+grid%dt,                 &
               grid%lons,grid%lats,grid%unit_fxlong,grid%unit_fxlat,        & 
               grid%lfn, grid%tign_g,ignited)

          ignitions_done=ignitions_done+1
          ignited_tile(ignitions_done)=ignited
            !        endif
        end do
      end if
           
      call Print_tile_stats (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, &
          grid%lfn, 'fire_model: lfn out', config_flags%fire_print_msg)
    
        !ifun6 ! timestep postprocessing
        ! compute the heat fluxes from the fuel burned
        ! needs lfn and tign from neighbors so halo must be updated before
      call fuel_left (&
          ifms, ifme, jfms, jfme, &
          ifts, ifte, jfts, jfte, &
          ifts, ifte, jfts, jfte, &
          grid%lfn,grid%tign_g,grid%fuel_time, time_start + grid%dt, fuel_frac_end, grid%fire_area, &
          config_flags%fire_fuel_left_method, config_flags%fire_fuel_left_irl, &
          config_flags%fire_fuel_left_jrl, config_flags%fire_print_msg) !fuel_frac_end is private and tile based

      call Print_tile_stats (ifts, ifte, jfts, jfte, ifts, ifte, jfts, jfte, &
          fuel_frac_end, 'model: fuel_frac end', config_flags%fire_print_msg)
    
      do j = jfts, jfte
        do i = ifts, ifte
          fuel_frac_burnt(i, j) = grid%fuel_frac(i, j) - fuel_frac_end(i, j) ! fuel lost this timestep
          grid%burnt_area_dt(i, j) = fuel_frac_burnt(i, j)
          grid%fuel_frac(i, j) = fuel_frac_end(i, j) ! copy new value to state array
        end do
      end do

      call Print_tile_stats (ifts, ifte, jfts, jfte, ifts, ifte, jfts, jfte, &
          fuel_frac_burnt,'model: fuel_frac burned',config_flags%fire_print_msg)
        
      call Calc_fire_fluxes (grid%dt, grid, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, &
          ifts, ifte, jfts, jfte, grid%fgip, fuel_frac_burnt, grid%fgrnhfx, grid%fgrnqfx)

      If_pring_summary: if (config_flags%fire_print_msg .ge. stat_lev) then
        tfa = Fun_real (REAL_SUM,  &
            ifms,ifme,1,1,jfms,jfme, &                ! memory dims
            ifds,ifde,1,1,jfds,jfde, &                ! domain dims
            ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
            grid%fire_area,grid%fire_area) * grid%dx * grid%dy

        thf = Fun_real (REAL_SUM,  &
            ifms,ifme,1,1,jfms,jfme, &                ! memory dims
            ifds,ifde,1,1,jfds,jfde, &                ! domain dims
            ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
            grid%fgrnhfx,grid%fgrnhfx) * grid%dx * grid%dy

        mhf = Fun_real (REAL_MAX,  &
            ifms,ifme,1,1,jfms,jfme, &                ! memory dims
            ifds,ifde,1,1,jfds,jfde, &                ! domain dims
            ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
            grid%fgrnhfx,grid%fgrnhfx) 

        tqf = Fun_real (REAL_SUM,  &
            ifms,ifme,1,1,jfms,jfme, &                ! memory dims
            ifds,ifde,1,1,jfds,jfde, &                ! domain dims
            ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
            grid%fgrnqfx,grid%fgrnqfx) * grid%dx * grid%dy

        mqf = Fun_real(REAL_MAX,  &
            ifms,ifme,1,1,jfms,jfme, &                ! memory dims
            ifds,ifde,1,1,jfds,jfde, &                ! domain dims
            ifts,ifte,1,1,jfts,jfte, &                ! patch or tile dims
            grid%fgrnqfx,grid%fgrnqfx) 

        !$OMP MASTER 
        write(msg, 91) time_start,'Fire area           ',tfa,'m^2'
        call Message (msg,config_flags%fire_print_msg,stat_lev)

        write(msg, 91) time_start,'Heat output         ',thf,'W'
        call Message (msg,config_flags%fire_print_msg,stat_lev)

        write(msg, 91) time_start,'Max heat flux       ',mhf,'W/m^2'
        call Message (msg,config_flags%fire_print_msg,stat_lev)

        write(msg, 91) time_start,'Latent heat output  ',tqf,'W'
        call Message(msg,config_flags%fire_print_msg,stat_lev)

        write(msg, 91) time_start,'Max latent heat flux',mqf,'W/m^2'
        call Message (msg,config_flags%fire_print_msg,stat_lev)
        !$OMP END MASTER

 91     format('Time ',f11.3,' s ',a,e12.3,1x,a)
      end if If_pring_summary

      call Print_tile_stats (ifts,ifte,jfts,jfte, ifms,ifme,jfms,jfme, &
          grid%fgrnhfx,'model: heat flux(J/m^2/s)',config_flags%fire_print_msg)

    end subroutine fire_model

  end module module_fr_fire_model
