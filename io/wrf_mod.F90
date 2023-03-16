  module wrf_mod

    use netcdf_mod, only : Get_netcdf_var, Get_netcdf_att
    use datetime_mod, only : datetime_t
    use proj_lc_mod, only : proj_lc_t
    use namelist_mod, only : namelist_t
    use geogrid_mod, only : geogrid_t
    use constants_mod, only : CP, XLV

    implicit none

    private

    public :: wrf_t, G, RERADIUS

    integer, parameter :: NUM_TRACER = 1
    real, parameter :: G = 9.81                   ! acceleration due to gravity [m s-2]
    real, parameter :: RERADIUS = 1.0 / 6370.0e03 ! reciprocal of earth radius (m^-1)

    type :: wrf_t
      character (len = 300) :: file_name
        ! Atmosphere
        ! 4D
      real, dimension(:, :, :, :), allocatable :: tracer
        ! 3D
      real, dimension(:, :, :), allocatable :: u3d, v3d, phb, ph, phl, pres, dz8w, z_at_w, rho
      real, dimension(:, :, :), allocatable :: u3d_stag, v3d_stag, phb_stag, ph_stag, dz8w_stag, z_at_w_stag, rho_stag
        ! 2D
      real, dimension(:, :), allocatable :: lats, lons, lats_c, lons_c, t2, q2, z0, mut, psfc, rain, rainc, rainnc
      real, dimension(:, :), allocatable :: xlat, xlong
      real, dimension(:, :), allocatable :: t2_stag, q2_stag, z0_stag, mut_stag, psfc_stag, rainc_stag, rainnc_stag
        ! feedback to atm
      real, dimension(:), allocatable :: c1h ! "half levels, c1h = d bf / d eta, using znw"        "Dimensionless"
      real, dimension(:), allocatable :: c2h ! "half levels, c2h = (1-c1h)*(p0-pt)"                "Pa"
      real, dimension(:, :), allocatable :: avg_fuel_frac ! "fuel remaining averaged to atmospheric grid" "1"
      real, dimension(:, :), allocatable :: grnhfx ! "heat flux from ground fire" "W/m^2"
      real, dimension(:, :), allocatable :: grnqfx ! "moisture flux from ground fire" "W/m^2"
      real, dimension(:, :), allocatable :: canhfx ! "heat flux from crown fire" "W/m^2"
      real, dimension(:, :), allocatable :: canqfx ! "moisture flux from crown fire" "W/m^2"
      real, dimension(:, :), allocatable :: grnhfx_fu ! "heat flux from ground fire (feedback unsensitive)" "W/m^2"
      real, dimension(:, :), allocatable :: grnqfx_fu ! "moisture flux from ground fire (feedback unsensitive)" "W/m^2"
      real, dimension(:, :, :), allocatable :: rthfrten ! "temperature tendency" "K/s"
      real, dimension(:, :, :), allocatable :: rqvfrten ! "RQVFRTEN" "humidity tendency" Stagger in z

      integer :: ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe, &
                 its, ite, jts, jte, kts, kte
      integer :: num_tiles
      integer, dimension (:), allocatable :: i_start, i_end, j_start, j_end
        ! projection
      real :: cen_lat, cen_lon, dx, dy, truelat1, truelat2, stand_lon
      integer :: sr_x = 0, sr_y = 0
    contains
      procedure, public :: Add_fire_tracer_emissions => Add_fire_tracer_emissions
      procedure, public :: Destroy_dz8w => Destroy_distance_between_vertical_layers
      procedure, public :: Destroy_mut => Destroy_mut
      procedure, public :: Destroy_phl => Destroy_geopotential_levels
      procedure, public :: Destroy_pres => Destroy_pressure_levels
      procedure, public :: Destroy_ph => Destroy_geopotential
      procedure, public :: Destroy_phb => Destroy_geopotential_base
      procedure, public :: Destroy_psfc => Destroy_surface_pressure
      procedure, public :: Destroy_rho => Destroy_rho
      procedure, public :: Destroy_rainc => Destroy_rain_convective
      procedure, public :: Destroy_rainnc => Destroy_rain_non_convective
      procedure, public :: Destroy_rain => Destroy_rain
      procedure, public :: Destroy_q2 => Destroy_specific_humidity_2m
      procedure, public :: Destroy_t2 => Destroy_temperature_2m
      procedure, public :: Destroy_u3d => Destroy_zonal_wind
      procedure, public :: Destroy_v3d => Destroy_meridional_wind
      procedure, public :: Destroy_z0 => Destroy_z0
      procedure, public :: Destroy_z_at_w => Destroy_height_agl_at_walls
      procedure, public :: Fire_tendency => Fire_tendency
      procedure, public :: Get_datetime_index => Get_datetime_index
      procedure, public :: Get_dz8w => Get_distance_between_vertical_layers
      procedure, public :: Get_latlons => Get_latlons
      procedure, public :: Get_latcloncs => Get_latcloncs
      procedure, public :: Get_mut => Get_mut
      procedure, public :: Get_phl => Get_geopotential_levels
      procedure, public :: Get_pres => Get_pressure_levels
      procedure, public :: Get_ph_stag => Get_geopotential_stag_3d
      procedure, public :: Get_phb_stag => Get_geopotential_base_stag_3d
      procedure, public :: Get_projection => Get_projection
      procedure, public :: Get_rainc => Get_rain_convective
      procedure, public :: Get_rainnc => Get_rain_non_convective
      procedure, public :: Get_rain => Get_rain
      procedure, public :: Get_rho => Get_rho
      procedure, public :: Get_psfc => Get_surface_pressure
      procedure, public :: Get_q2 => Get_specific_humidity_2m
      procedure, public :: Get_t2 => Get_temperature_2m
      procedure, public :: Get_u3d => Get_zonal_wind_3d
      procedure, public :: Get_u3d_stag => Get_zonal_wind_stag_3d
      procedure, public :: Get_v3d => Get_meridional_wind_3d
      procedure, public :: Get_v3d_stag => Get_meridional_wind_stag_3d
      procedure, public :: Get_z0 => Get_z0
      procedure, public :: Get_z_at_w => Get_height_agl_at_walls
      procedure, public :: Interp_var2grid_nearest => Interp_var2grid_nearest
      procedure, public :: Interpolate_z2fire => Interpolate_z2fire
      procedure, public :: Interpolate_wind2fire => Interpolate_wind2fire
      procedure, public :: Load_atmosphere_test1 => Load_atmosphere_test1
      procedure, public :: Print_domain => Print_domain
      procedure, public :: Read_wrf_input => Read_wrf_input
      procedure, public :: Update_atm_state => Update_atm_state
    end type wrf_t

    interface wrf_t
      module procedure Wrf_t_const
    end interface wrf_t

  contains

    subroutine Add_fire_tracer_emissions(    &
           this,                             &
           ifms,ifme,jfms,jfme,              &
           ifts,ifte,jtfs,jfte,              &
           ids,ide,kds,kde,jds,jde,          &
           ims,ime,kms,kme,jms,jme,          &
           its,ite,kts,kte,jts,jte,          &
           rho,dz8w,                         &
           burnt_area_dt,fgip,               &
           tracer,emis)

      implicit none

      class (wrf_t), intent(in out) :: this
      integer, intent(in) :: ifms,ifme,jfms,jfme,  &
                          ifts,ifte,jtfs,jfte,     &
                          ids,ide,kds,kde,jds,jde, &
                          ims,ime,kms,kme,jms,jme, &
                          its,ite,kts,kte,jts,jte
      real, intent(in) :: rho(ims:ime,kms:kme,jms:jme),dz8w(ims:ime,kms:kme,jms:jme)
      real, intent(in), dimension(ifms:ifme,jfms:jfme) :: burnt_area_dt,fgip,emis
      real, intent(inout) :: tracer(ims:ime,kms:kme,jms:jme)

      integer :: isz1,jsz1,isz2,jsz2,ir,jr
      integer :: i,j,ibase,jbase,i_f,ioff,j_f,joff
      real :: avgw


      isz1 = ite-its+1
      jsz1 = jte-jts+1
      isz2 = ifte-ifts+1
      jsz2 = jfte-jtfs+1
      ir=isz2/isz1
      jr=jsz2/jsz1
      avgw = 1.0/(ir*jr)

      do j=max(jds+1,jts),min(jte,jde-2)
        jbase=jtfs+jr*(j-jts)
        do i=max(ids+1,its),min(ite,ide-2)
          ibase=ifts+ir*(i-its)
          do joff=0,jr-1
            j_f=joff+jbase
            do ioff=0,ir-1
              i_f=ioff+ibase
              if (num_tracer >0) then
                tracer(i,kts,j)=tracer(i,kts,j) &
                  + (avgw*emis(i_f,j_f)*1000/(rho(i,kts,j)*dz8w(i,kts,j))) ! g_smoke/kg_air
              endif
            enddo
          enddo
        enddo
      enddo

    end subroutine Add_fire_tracer_emissions

    subroutine Continue_at_boundary(ix,iy,bias, & ! do x direction or y direction
          ims,ime,jms,jme, &                ! memory dims
          ids,ide,jds,jde, &                ! domain dims
          its,ite,jts,jte, &                ! tile dims
          itso,iteo,jtso,jteo, &            ! tile dims where set
          lfn)
                              ! array
      implicit none
      !*** description
      ! extend array by one beyond the domain by linear continuation
      !*** arguments
      integer, intent(in) :: ix,iy               ! not 0 = do x or y (1 or 2) direction
      real, intent(in) :: bias                   ! 0=none, 1.=max
      integer, intent(in) :: ims,ime,jms,jme, &  ! memory dims
                             ids,ide,jds,jde, &  ! domain dims
                             its,ite,jts,jte     ! tile dims
      integer, intent(out) :: itso,jtso,iteo,jteo  ! where set
      real, intent(inout), dimension(ims:ime,jms:jme) :: lfn
      !*** local
      integer i,j
      character(len=128)::msg
      integer::its1,ite1,jts1,jte1
      integer,parameter::halo=1     ! only 1 domain halo is needed since ENO1 is used near domain boundaries
      !*** executable

      ! for dislay only
      itso = its
      jtso = jts
      iteo = ite
      jteo = jte
      ! go halo width beyond if at patch boundary but not at domain boudnary
      ! assume we have halo need to compute the value we do not have
      ! the next thread that would conveniently computer the extended values at patch corners
      ! besides halo may not transfer values outside of the domain

      its1 = its
      jts1 = jts
      ite1 = ite
      jte1 = jte
      if(.not.its.eq.ids)its1=its-halo
      if(.not.jts.eq.jds)jts1=jts-halo
      if(.not.ite.eq.ide)ite1=ite+halo
      if(.not.jte.eq.jde)jte1=jte+halo
      !$OMP CRITICAL(FIRE_UTIL_CRIT)
      write(msg,'(a,2i5,a,f5.2)')'continue_at_boundary: directions',ix,iy,' bias ',bias
!      call message(msg)
      !$OMP END CRITICAL(FIRE_UTIL_CRIT)
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
          write(msg,'(8(a,i5))')'continue_at_boundary: x:',its,':',ite,',',jts,':',jte,' ->',itso,':',iteo,',',jts1,':',jte1
!          call message(msg)
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
          write(msg,'(8(a,i5))')'continue_at_boundary: y:',its,':',ite,',',jts,':',jte,' ->',its1,':',ite1,',',jtso,':',jteo
      !$OMP END CRITICAL(FIRE_UTIL_CRIT)
!          call message(msg)
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
        real function EX(a,b)
        !*** statement function
        real a,b
        EX=(1.-bias)*(2.*a-b)+bias*max(2.*a-b,a,b)   ! extrapolation, max quarded
        end function EX
    end subroutine Continue_at_boundary

    subroutine Destroy_distance_between_vertical_layers (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%dz8w)) deallocate (this%dz8w)

    end subroutine Destroy_distance_between_vertical_layers

    subroutine Destroy_geopotential_levels (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%phl)) deallocate (this%phl)

    end subroutine Destroy_geopotential_levels

    subroutine Destroy_pressure_levels (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%pres)) deallocate (this%pres)

    end subroutine Destroy_pressure_levels

    subroutine Destroy_geopotential (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%ph)) deallocate (this%ph)

    end subroutine Destroy_geopotential

    subroutine Destroy_geopotential_base (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%phb)) deallocate (this%phb)

    end subroutine Destroy_geopotential_base

    subroutine Destroy_meridional_wind (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%v3d)) deallocate (this%v3d)

    end subroutine Destroy_meridional_wind

    subroutine Destroy_mut (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%mut)) deallocate (this%mut)

    end subroutine Destroy_mut

    subroutine Destroy_rain_convective (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%rainc)) deallocate (this%rainc)

    end subroutine Destroy_rain_convective

    subroutine Destroy_rain_non_convective (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%rainnc)) deallocate (this%rainnc)

    end subroutine Destroy_rain_non_convective

    subroutine Destroy_rain (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%rain)) deallocate (this%rain)

    end subroutine Destroy_rain

    subroutine Destroy_rho (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%rho)) deallocate (this%rho)

    end subroutine Destroy_rho

    subroutine Destroy_surface_pressure (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%psfc)) deallocate (this%psfc)

    end subroutine Destroy_surface_pressure

    subroutine Destroy_specific_humidity_2m (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%q2)) deallocate (this%q2)

    end subroutine Destroy_specific_humidity_2m

    subroutine Destroy_temperature_2m (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%t2)) deallocate (this%t2)

    end subroutine Destroy_temperature_2m

    subroutine Destroy_z0 (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%z0)) deallocate (this%z0)

    end subroutine Destroy_z0

    subroutine Destroy_zonal_wind (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%u3d)) deallocate (this%u3d)

    end subroutine Destroy_zonal_wind

    subroutine Destroy_height_agl_at_walls (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%z_at_w)) deallocate (this%z_at_w)

    end subroutine Destroy_height_agl_at_walls

    subroutine Fire_tendency(this,   &
        ids,ide, kds,kde, jds,jde,   & ! dimensions
        ims,ime, kms,kme, jms,jme,   &
        its,ite, kts,kte, jts,jte,   &
        grnhfx,grnqfx,canhfx,canqfx, & ! heat fluxes summed up to  atm grid
        alfg,alfc,z1can,             & ! coeffients, properties, geometry
        z_at_w,dz8w,mu,c1h,c2h,rho,  &
        rthfrten,rqvfrten)             ! theta and Qv tendencies

    ! This routine is atmospheric physics
    ! it does NOT go into module_fr_fire_phys because it is not related to fire physical processes

    ! --- this routine takes fire generated heat and moisture fluxes and
    !     calculates their influence on the theta and water vapor
    ! --- note that these tendencies are valid at the Arakawa-A location

      implicit none

    ! --- incoming variables

      class (wrf_t), intent(in out) :: this
      integer, intent(in) :: ids,ide, kds,kde, jds,jde, &
                               ims,ime, kms,kme, jms,jme, &
                               its,ite, kts,kte, jts,jte

      real, intent(in), dimension( ims:ime,jms:jme ) :: grnhfx,grnqfx  ! w/m^2
      real, intent(in), dimension( ims:ime,jms:jme ) :: canhfx,canqfx  ! w/m^2
      real, intent(in), dimension( ims:ime,jms:jme ) :: mu  ! dry air mass (pa)
      real, intent(in), dimension( kms:kme         ) :: c1h, c2h ! hybrid coordinate weights

      real, intent(in), dimension( ims:ime,kms:kme,jms:jme ) :: z_at_w ! m abv sealvl
      real, intent(in), dimension( ims:ime,kms:kme,jms:jme ) :: dz8w   ! dz across w-lvl
      real, intent(in), dimension( ims:ime,kms:kme,jms:jme ) :: rho    ! density

      real, intent(in) :: alfg ! extinction depth surface fire heat (m)
      real, intent(in) :: alfc ! extinction depth crown  fire heat (m)
      real, intent(in) :: z1can    ! height of crown fire heat release (m)

    ! --- outgoing variables

      real, intent(out), dimension( ims:ime,kms:kme,jms:jme ) ::   &
           rthfrten, & ! theta tendency from fire (in mass units)
           rqvfrten    ! Qv tendency from fire (in mass units)
    ! --- local variables

      integer :: i,j,k
      integer :: i_st,i_en, j_st,j_en, k_st,k_en

      real :: cp_i
      real :: rho_i
      real :: xlv_i
      real :: z_w
      real :: fact_g, fact_c
      real :: alfg_i, alfc_i

      real, dimension( its:ite,kts:kte,jts:jte ) :: hfx,qfx


      do j=jts,jte
        do k=kts,min(kte+1,kde)
          do i=its,ite
            rthfrten(i,k,j)=0.
            rqvfrten(i,k,j)=0.
          enddo
        enddo
      enddo

    ! --- set some local constants

      cp_i = 1./cp     ! inverse of specific heat
      xlv_i = 1./xlv   ! inverse of latent heat
      alfg_i = 1./alfg
      alfc_i = 1./alfc

    ! --- set loop indicies : note that

      i_st = MAX(its,ids+1)
      i_en = MIN(ite,ide-1)
      k_st = kts
      k_en = MIN(kte,kde-1)
      j_st = MAX(jts,jds+1)
      j_en = MIN(jte,jde-1)

    ! --- distribute fluxes

      do j = j_st,j_en
        do k = k_st,k_en
          do i = i_st,i_en

            ! --- set z (in meters above ground)

            z_w = z_at_w(i,k,j) - z_at_w(i, 1, j)

            ! --- heat flux

            fact_g = cp_i * EXP( - alfg_i * z_w )
            if ( z_w < z1can ) then
                   fact_c = cp_i
            else
                   fact_c = cp_i * EXP( - alfc_i * (z_w - z1can) )
            end if
            hfx(i,k,j) = fact_g * grnhfx(i,j) + fact_c * canhfx(i,j)

    !!            write(msg,2)i,k,j,z_w,grnhfx(i,j),hfx(i,k,j)
    !!2           format('hfx:',3i4,6e11.3)
    !!            call message(msg)

            ! --- vapor flux

            fact_g = xlv_i * EXP( - alfg_i * z_w )
            if (z_w < z1can) then
                   fact_c = xlv_i
            else
                   fact_c = xlv_i * EXP( - alfc_i * (z_w - z1can) )
            end if
            qfx(i,k,j) = fact_g * grnqfx(i,j) + fact_c * canqfx(i,j)


          end do
        end do
      end do

    ! --- add flux divergence to tendencies
    !
    !   multiply by dry air mass (mu) to eliminate the need to
    !   call sr. calculate_phy_tend (in dyn_em/module_em.F)

      do j = j_st,j_en
        do k = k_st,k_en-1
          do i = i_st,i_en

            rho_i = 1./rho(i,k,j)

            rthfrten(i,k,j) = - (c1h(k)*mu(i,j)+c2h(k)) * rho_i * (hfx(i,k+1,j)-hfx(i,k,j)) / dz8w(i,k,j)
            rqvfrten(i,k,j) = - (c1h(k)*mu(i,j)+c2h(k)) * rho_i * (qfx(i,k+1,j)-qfx(i,k,j)) / dz8w(i,k,j)

          end do
        end do
      end do

      return

    end subroutine Fire_tendency

    function Get_datetime_index (this, datetime) result (return_value)

      use, intrinsic :: iso_fortran_env, only : ERROR_UNIT

      implicit none

      class (wrf_t), intent (in) :: this
      type (datetime_t), intent (in) :: datetime
      integer :: return_value

      character (len = :), dimension (:), allocatable :: times
      integer :: n


      call Get_netcdf_var (trim (this%file_name), 'Times', times)

      do n = 1, size (times)
        if (times(n) == datetime%datetime) then
          return_value = n
          return
        end if
      end do

      write (ERROR_UNIT, *) 'Datetime not found in WRF file:'
      call datetime%Print_datetime ()
      stop

    end function Get_datetime_index

    subroutine Get_latlons (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      real, dimension(:, :, :), allocatable :: var3d


      call Get_netcdf_var (trim (this%file_name), 'XLAT', var3d)
      this%lats = var3d(:, :, 1)
      deallocate (var3d)

      call Get_netcdf_var (trim (this%file_name), 'XLONG', var3d)
      this%lons = var3d(:, :, 1)
      deallocate (var3d)

    end subroutine Get_latlons

    subroutine Get_distance_between_vertical_layers (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :, :), allocatable :: var4d, var4d2
      real, dimension(:, :, :), allocatable :: z
      integer :: nt, nlevels


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'PH', var4d)
      call Get_netcdf_var (trim (this%file_name), 'PHB', var4d2)
      z = (var4d(:, :, :, nt) + var4d2(:, :, :, nt)) / G
      deallocate (var4d, var4d2)

      nlevels = size (z, dim = 3)
      this%dz8w = z(:, :, 2:nlevels) - z(:, :, 1:nlevels - 1)
      deallocate (z)

    end subroutine Get_distance_between_vertical_layers

    subroutine Get_geopotential_levels (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :, :), allocatable :: var4d, var4d2
      integer :: nt, nlevels


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'PH', var4d)
      call Get_netcdf_var (trim (this%file_name), 'PHB', var4d2)
      this%phl = var4d(:, :, :, nt) + var4d2(:, :, :, nt)
      deallocate (var4d, var4d2)

    end subroutine Get_geopotential_levels

    subroutine Get_pressure_levels (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :, :), allocatable :: var4d, var4d2
      integer :: nt


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'P', var4d)
      call Get_netcdf_var (trim (this%file_name), 'PB', var4d2)

      this%pres = var4d(:, :, :, nt) + var4d2(:, :, :, nt)
      deallocate (var4d, var4d2)

    end subroutine Get_pressure_levels

    subroutine Get_geopotential_stag_3d (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :, :), allocatable :: var4d
      integer :: nt


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'PH', var4d)
      this%ph = var4d(:, :, :, nt)
      deallocate (var4d)

    end subroutine Get_geopotential_stag_3d

    subroutine Get_geopotential_base_stag_3d (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :, :), allocatable :: var4d
      integer :: nt


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'PHB', var4d)
      this%phb = var4d(:, :, :, nt)
      deallocate (var4d)

    end subroutine Get_geopotential_base_stag_3d

    subroutine Get_height_agl_at_walls (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :, :), allocatable :: var4d, var4d2
      real, dimension(:, :, :), allocatable :: z
      integer :: nt, nx, ny, nz, i, j


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'PH', var4d)
      call Get_netcdf_var (trim (this%file_name), 'PHB', var4d2)
      z = (var4d(:, :, :, nt) + var4d2(:, :, :, nt)) / G
      deallocate (var4d, var4d2)

      nx = size (z, dim = 1)
      ny = size (z, dim = 2)
      nz = size (z, dim = 3)
      allocate (this%z_at_w(nx, ny, nz))
      do j = 1, ny
        do i = 1, nx
          this%z_at_w(i, j, :) = z(i, j, :) - z(i, j, 1)
        end do
      end do

    end subroutine Get_height_agl_at_walls

    subroutine Get_meridional_wind_3d (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :, :), allocatable :: var4d
      integer :: nt, nmass


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'V', var4d)
      nmass = size (var4d, dim = 1)
      this%v3d = 0.5 * (var4d(:, 1:nmass, :, nt) + var4d(:, 2:nmass + 1, :, nt))
      deallocate (var4d)

    end subroutine Get_meridional_wind_3d

    subroutine Get_meridional_wind_stag_3d (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :, :), allocatable :: var4d
      integer :: nt


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'V', var4d)
      this%v3d = var4d(:, :, :, nt)
      deallocate (var4d)

    end subroutine Get_meridional_wind_stag_3d

    subroutine Get_mut (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :), allocatable :: mu, mub
      integer :: nt


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'MU', mu)
      call Get_netcdf_var (trim (this%file_name), 'MUB', mub)
      this%mut = mub(:, :, nt) + mu(:, :, nt)
      deallocate (mu, mub)

    end subroutine Get_mut

    function Get_projection (this, stagger) result (return_value)

      use, intrinsic :: iso_fortran_env, only : ERROR_UNIT

      implicit none

      class (wrf_t), intent (in) :: this
      logical, intent (in), optional :: stagger
      type (proj_lc_t) :: return_value

      integer :: nx, ny, offset


      offset = 0
      if (present (stagger)) then
        offset = 1
      end if

      if (allocated (this%lats)) then
        nx = size (this%lats, dim = 1) + offset
        ny = size (this%lats, dim = 2) + offset
      else
        write (ERROR_UNIT, *) 'lats array needs to be initialized to get the WRF projection'
      end if

      return_value = proj_lc_t (cen_lat = this%cen_lat , cen_lon = this%cen_lon, &
          dx = this%dx, dy = this%dy, standard_lon = this%stand_lon , true_lat_1 = this%truelat1 , true_lat_2 = this%truelat2, &
          nx = nx, ny = ny)

    end function Get_projection

    subroutine Get_specific_humidity_2m (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :), allocatable :: var3d
      integer :: nt


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'Q2', var3d)
      this%q2 = var3d(:, :, nt)

    end subroutine Get_specific_humidity_2m

    subroutine Get_rain_convective (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :), allocatable :: var3d
      integer :: nt


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'RAINC', var3d)
      this%rainc = var3d(:, :, nt)

    end subroutine Get_rain_convective

    subroutine Get_rain_non_convective (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :), allocatable :: var3d
      integer :: nt


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'RAINNC', var3d)
      this%rainnc = var3d(:, :, nt)

    end subroutine Get_rain_non_convective

    subroutine Get_rain (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :), allocatable :: var3d, var3d2
      integer :: nt


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'RAINC', var3d)
      call Get_netcdf_var (trim (this%file_name), 'RAINNC', var3d2)
      this%rain = var3d(:, :, nt) + var3d2(:, :, nt)

    end subroutine Get_rain

    subroutine Get_rho (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :, :), allocatable :: alt, qvapor
      integer :: nt


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'ALT', alt)
      call Get_netcdf_var (trim (this%file_name), 'QVAPOR', qvapor)

      this%rho = 1.0 / alt(:, :, :, nt) *  (1.0 + qvapor(:, :, :, nt))
      deallocate (alt, qvapor)

    end subroutine Get_rho

    subroutine Get_surface_pressure (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :), allocatable :: var3d
      integer :: nt


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'PSFC', var3d)
      this%psfc = var3d(:, :, nt)

    end subroutine Get_surface_pressure

    subroutine Get_temperature_2m (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :), allocatable :: var3d
      integer :: nt


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'T2', var3d)
      this%t2 = var3d(:, :, nt)

    end subroutine Get_temperature_2m

    subroutine Get_z0 (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :), allocatable :: var3d
      integer :: nt


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'ZNT', var3d)
      this%z0 = var3d(:, :, nt)

    end subroutine Get_z0

    subroutine Get_zonal_wind_3d (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :, :), allocatable :: var4d
      integer :: nt, nmass


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'U', var4d)
      nmass = size (var4d, dim = 2)
      this%u3d = 0.5 * (var4d(1:nmass, :, :, nt) + var4d(2:nmass + 1, :, :, nt))
      deallocate (var4d)

    end subroutine Get_zonal_wind_3d

    subroutine Get_zonal_wind_stag_3d (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :, :), allocatable :: var4d
      integer :: nt


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'U', var4d)
      this%u3d = var4d(:, :, :, nt)
      deallocate (var4d)

    end subroutine Get_zonal_wind_stag_3d

    function Wrf_t_const (file_name, config_flags, geogrid) result (return_value)

      use, intrinsic :: iso_fortran_env, only : ERROR_UNIT, REAL32

      implicit none

      character (len = *), intent (in), optional :: file_name
      type (namelist_t), intent (in), optional :: config_flags
      type (geogrid_t), intent (in), optional :: geogrid
      type (wrf_t) :: return_value

      real, parameter :: DEFAULT_Z0 = 0.1, DEFAULT_MUT = 0.0, DEFAULT_ZSF = 0.0, DEFAULT_DZDXF = 0.0, &
          DEFAULT_DZDYF = 0.0, DEFAULT_C1H = 1.0, DEFAULT_C2H = 0.0
        ! Atm vars needed by the fuel moisture model
      real, parameter :: DEFAULT_T2 = 0.0, DEFAULT_Q2 = 0.0, DEFAULT_PSFC = 0.0, DEFAULT_RAINC = 0.0, &
          DEFAULT_RAINNC = 0.0

      logical :: use_geogrid, use_config_flags, use_file
      integer, parameter :: N_POINTS_IN_HALO = 5
      real (kind = REAL32) :: att_real32


      use_file = .false.
      use_geogrid = .false.
      use_config_flags = .false.

      if (present (file_name)) then
        use_file = .true.
      end if

      if (present (geogrid)) then
        use_geogrid = .true.
      end if

      if (present (config_flags)) then
        use_config_flags = .true.
      end if

      if (use_file) then
        return_value%file_name = trim (file_name)

          ! Init projection
        call Get_netcdf_att (trim (return_value%file_name), 'global', 'CEN_LAT', att_real32)
        return_value%cen_lat = att_real32

        call Get_netcdf_att (trim (return_value%file_name), 'global', 'CEN_LON', att_real32)
        return_value%cen_lon = att_real32

        call Get_netcdf_att (trim (return_value%file_name), 'global', 'TRUELAT1', att_real32)
        return_value%truelat1 = att_real32

        call Get_netcdf_att (trim (return_value%file_name), 'global', 'TRUELAT2', att_real32)
        return_value%truelat2 = att_real32

        call Get_netcdf_att (trim (return_value%file_name), 'global', 'STAND_LON', att_real32)
        return_value%stand_lon = att_real32

        call Get_netcdf_att (trim (return_value%file_name), 'global', 'DX', att_real32)
        return_value%dx = att_real32

        call Get_netcdf_att (trim (return_value%file_name), 'global', 'DY', att_real32)
        return_value%dy = att_real32

          ! latlon at mass points
        call return_value%Get_latlons ()

          ! latlon at corners
        call return_value%Get_latcloncs ()

      end if

      if_use_config_flags: if (use_config_flags) then
        ! Domain dimensions
      if (use_geogrid) then

        if (.not. allocated (return_value%lons) .and. .not. allocated (return_value%lats)) then
          return_value%lats = geogrid%xlat
          return_value%lons = geogrid%xlong
          return_value%lats_c = geogrid%xlat_c
          return_value%lons_c = geogrid%xlong_c
        end if

        if (geogrid%ids == config_flags%ids) then
          return_value%ids = config_flags%ids
        else
          write (ERROR_UNIT, *) 'ids in namelist and geogrid differ'
          stop
        end if
        if (geogrid%ide == config_flags%ide) then
          return_value%ide = config_flags%ide
        else
          write (ERROR_UNIT, *) 'ide in namelist and geogrid differ'
          stop
        end if
        if (geogrid%jds == config_flags%jds) then
          return_value%jds = config_flags%jds
        else
          write (ERROR_UNIT, *) 'jds in namelist and geogrid differ'
          stop
        end if
        if (geogrid%jde == config_flags%jde) then
          return_value%jde = config_flags%jde
        else
          write (ERROR_UNIT, *) 'jde in namelist and geogrid differ'
          stop
        end if
        if (geogrid%sr_x == config_flags%sr_x) then
          return_value%sr_x = geogrid%sr_x
        else
          write (ERROR_UNIT, *) 'sr_x in namelist and in geogrid differ'
          stop
        end if
        if (geogrid%sr_y == config_flags%sr_y) then
          return_value%sr_y = geogrid%sr_y
        else
          write (ERROR_UNIT, *) 'sr_y in namelist and in geogrid differ'
          stop
        end if
      else
        return_value%ids = config_flags%ids
        return_value%ide = config_flags%ide
        return_value%jds = config_flags%jds
        return_value%jde = config_flags%jde
        return_value%sr_x = config_flags%sr_x
        return_value%sr_y = config_flags%sr_y
      end if

      return_value%kds = config_flags%kds
      return_value%kde = config_flags%kde

      return_value%ims = config_flags%ids - N_POINTS_IN_HALO
      return_value%ime = config_flags%ide + N_POINTS_IN_HALO
      return_value%kms = config_flags%kds
      return_value%kme = config_flags%kde
      return_value%jms = config_flags%jds - N_POINTS_IN_HALO
      return_value%jme = config_flags%jde + N_POINTS_IN_HALO

      return_value%ips = config_flags%ids
      return_value%ipe = config_flags%ide
      return_value%kps = config_flags%kds
      return_value%kpe = config_flags%kde
      return_value%jps = config_flags%jds
      return_value%jpe = config_flags%jde

      return_value%its = config_flags%ids
      return_value%ite = config_flags%ide
      return_value%kts = config_flags%kds
      return_value%kte = config_flags%kde
      return_value%jts = config_flags%jds
      return_value%jte = config_flags%jde

      call return_value%Print_domain()

      return_value%num_tiles = 1
      allocate (return_value%i_start(return_value%num_tiles))
      return_value%i_start = return_value%ids
      allocate (return_value%i_end(return_value%num_tiles))
      return_value%i_end = return_value%ide
      allocate (return_value%j_start(return_value%num_tiles))
      return_value%j_start = return_value%jds
      allocate (return_value%j_end(return_value%num_tiles))
      return_value%j_end = return_value%jde

       ! Atmosphere vars
      allocate (return_value%tracer(return_value%ims:return_value%ime, &
                return_value%kms:return_value%kme, return_value%jms:return_value%jme, &
                NUM_TRACER))
      return_value%tracer = 0.0
      allocate (return_value%ph_stag(return_value%ims:return_value%ime, &
                return_value%kms:return_value%kme, return_value%jms:return_value%jme))
      return_value%ph_stag = 0.0
      allocate (return_value%phb_stag(return_value%ims:return_value%ime, &
                return_value%kms:return_value%kme, return_value%jms:return_value%jme))
      return_value%phb_stag = 0.0
      allocate (return_value%u3d_stag(return_value%ims:return_value%ime, &
                return_value%kms:return_value%kme, return_value%jms:return_value%jme))
      return_value%u3d_stag = 0.0
      allocate (return_value%v3d_stag(return_value%ims:return_value%ime, &
                return_value%kms:return_value%kme, return_value%jms:return_value%jme))
      return_value%v3d_stag = 0.0
      allocate (return_value%rho_stag(return_value%ims:return_value%ime, &
                return_value%kms:return_value%kme, return_value%jms:return_value%jme))
      return_value%rho_stag = 0.0
      allocate (return_value%z_at_w_stag(return_value%ims:return_value%ime, &
                return_value%kms:return_value%kme, return_value%jms:return_value%jme))
      return_value%z_at_w_stag = 0.0
      allocate (return_value%dz8w_stag(return_value%ims:return_value%ime, &
                return_value%kms:return_value%kme, return_value%jms:return_value%jme))
      return_value%dz8w_stag = 0.0

      allocate (return_value%z0_stag(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      return_value%z0_stag = DEFAULT_Z0
      allocate (return_value%mut_stag(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      return_value%mut_stag = DEFAULT_MUT
      allocate (return_value%rainc_stag(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      return_value%rainc_stag = DEFAULT_RAINC
      allocate (return_value%rainnc_stag(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      return_value%rainnc_stag = DEFAULT_RAINNC
      allocate (return_value%t2_stag(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      return_value%t2_stag = DEFAULT_T2
      allocate (return_value%q2_stag(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      return_value%q2_stag = DEFAULT_Q2
      allocate (return_value%psfc_stag(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      return_value%psfc_stag = DEFAULT_PSFC

      allocate (return_value%c1h(return_value%kms:return_value%kme))
      return_value%c1h = DEFAULT_C1H
      allocate (return_value%c2h(return_value%kms:return_value%kme))
      return_value%c2h = DEFAULT_C2H

      allocate (return_value%rthfrten(return_value%ims:return_value%ime, &
                return_value%kms:return_value%kme, return_value%jms:return_value%jme))
      allocate (return_value%rqvfrten(return_value%ims:return_value%ime, &
                return_value%kms:return_value%kme, return_value%jms:return_value%jme))

      allocate (return_value%xlat(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      allocate (return_value%xlong(return_value%ims:return_value%ime, return_value%jms:return_value%jme))

      allocate (return_value%avg_fuel_frac(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      allocate (return_value%grnhfx(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      allocate (return_value%grnqfx(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      allocate (return_value%canhfx(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      allocate (return_value%canqfx(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      allocate (return_value%grnhfx_fu(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      allocate (return_value%grnqfx_fu(return_value%ims:return_value%ime, return_value%jms:return_value%jme))


        ! Grid dimensions
      if_geogrid: if (use_geogrid) then
        return_value%xlat = 0.0
        return_value%xlat(return_value%ids:return_value%ide - 1, return_value%jds:return_value%jde - 1) = geogrid%xlat
        return_value%xlong = 0.0
        return_value%xlong(return_value%ids:return_value%ide - 1, return_value%jds:return_value%jde - 1) = geogrid%xlong
      end if if_geogrid

    end if if_use_config_flags

    end function Wrf_t_const

    subroutine Get_latcloncs (this)

      use, intrinsic :: iso_fortran_env, only : REAL32
      implicit none

      class (wrf_t), intent (in out) :: this
      type (proj_lc_t) :: proj

      logical, parameter :: OUTPUT_LATLON_CHECK = .false.
      real (kind = REAL32) :: att_real32
      real :: cen_lat, cen_lon, truelat1, truelat2, stand_lon, dx, dy
      integer :: nx, ny, i, j


      nx = size (this%lats, dim = 1) + 1
      ny = size (this%lats, dim = 2) + 1

      allocate (this%lats_c(nx, ny))
      allocate (this%lons_c(nx, ny))

      proj = this%Get_projection (stagger = .true.)

      do j = 1, ny
        do i = 1, nx
          call proj%Calc_latlon (i = real (i), j = real (j), lat = this%lats_c(i, j), lon = this%lons_c(i, j))
        end do
      end do

      if (OUTPUT_LATLON_CHECK) call Write_latlon_check ()

    contains

      subroutine Write_latlon_check ()

        implicit none

        integer :: i, j, unit1, unit2


        open (newunit = unit1, file = "latlons_wrf_mass.dat")
        do j = 1,  size (this%lats, dim = 2)
          do i = 1,  size (this%lats, dim = 1)
            write (unit1, *) i, j, this%lats(i, j), this%lons(i, j)
          end do
        end do
        close (unit1)

        open (newunit = unit2, file = "latlons_wrf_corners_estimated.dat")
        do j = 1,  size (this%lats, dim = 2) + 1
          do i = 1,  size (this%lats, dim = 1) + 1
            write (unit2, *) i, j, this%lats_c(i, j), this%lons_c(i, j)
          end do
        end do
        close (unit2)

      end subroutine Write_latlon_check

    end subroutine Get_latcloncs

    subroutine interpolate_2d(  &
          ims2,ime2,jms2,jme2, & ! array coarse grid
          its2,ite2,jts2,jte2, & ! dimensions coarse grid
          ims1,ime1,jms1,jme1, & ! array coarse grid
          its1,ite1,jts1,jte1, & ! dimensions fine grid
          ir,jr,               & ! refinement ratio
          rip2,rjp2,rip1,rjp1, & ! (rip2,rjp2) on grid 2 lines up with (rip1,rjp1) on grid 1
          v2, &                  ! in coarse grid
          v1  )                  ! out fine grid
      implicit none

      !*** purpose
      ! interpolate nodal values in mesh2 to nodal values in mesh1
      ! interpolation runs over the mesh2 region its2:ite2,jts2:jte2
      ! only the part of mesh 1 in the region its1:ite1,jts1:jte1 is modified

      !*** arguments

      integer, intent(in)::its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1
      integer, intent(in)::its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2
      integer, intent(in)::ir,jr
      real,intent(in):: rjp1,rip1,rjp2,rip2
      real, intent(out)::v1(ims1:ime1,jms1:jme1)
      real, intent(in)::v2(ims2:ime2,jms2:jme2)

      !*** local
      integer:: i1,i2,j1,j2,is,ie,js,je
      real:: tx,ty,rx,ry
      real:: rio,rjo
      intrinsic::ceiling,floor


      !*** executable

      ! compute mesh ratios
      rx=1./ir
      ry=1./jr

      do j2=jts2,jte2-1             ! loop over mesh 2 cells
        rjo=rjp1+jr*(j2-rjp2)       ! mesh 1 coordinate of the mesh 2 patch start
        js=max(jts1,ceiling(rjo))   ! lower bound of mesh 1 patch for this mesh 2 cell
        je=min(jte1,floor(rjo)+jr)  ! upper bound of mesh 1 patch for this mesh 2 cell
        do i2=its2,ite2-1
          rio=rip1+ir*(i2-rip2)
          is=max(its1,ceiling(rio))
          ie=min(ite1,floor(rio)+ir)
          do j1=js,je
            ty = (j1-rjo)*ry
            do i1=is,ie
              ! in case mesh 1 node lies on the boundary of several mesh 2 cells
              ! the result will be written multiple times with the same value
              ! up to a rounding error
              tx = (i1-rio)*rx
              !print *,'coarse ',i2,j2,'to',i2+1,j2+1,' fine ',is,js,' to ',ie,je
              v1(i1,j1)=                     &
                    (1-tx)*(1-ty)*v2(i2,j2)  &
               +    (1-tx)*ty  *v2(i2,j2+1)  &
               +      tx*(1-ty)*v2(i2+1,j2)  &
               +        tx*ty  *v2(i2+1,j2+1)
            enddo
          enddo
        enddo
      enddo

    end subroutine interpolate_2d

    ! general conditional expression
    pure integer function ifval(l,i,j)

      implicit none

      logical, intent(in)::l
      integer, intent(in)::i,j


      if(l)then
        ifval=i
      else
        ifval=j
      endif

    end function ifval

    subroutine Interp_var2grid_nearest (this, lats_in, lons_in, var_name, vals_out)

      use, intrinsic :: iso_fortran_env, only : ERROR_UNIT

      implicit none

      class (wrf_t), intent(in) :: this
      real, dimension(:, :), intent(in) :: lats_in, lons_in
      character (len = *), intent (in) :: var_name
      real, dimension(:, :), allocatable, intent(out) :: vals_out

      logical, parameter :: OUTPUT_LATLON_CHECK = .true.
      real, parameter :: DEFAULT_INIT = 0.0
      real, dimension(:, :), allocatable :: ds, var_wrf
      real :: d, i_real, j_real
      type (proj_lc_t) :: proj
      integer :: nx, ny, nx_wrf, ny_wrf, i, j, i_wrf, j_wrf


        ! Init
      nx_wrf = this%ide - 1
      ny_wrf = this%jde - 1
      nx = size (lats_in, dim = 1)
      ny = size (lats_in, dim = 2)

      if (allocated (vals_out)) deallocate (vals_out)
      allocate (vals_out(nx, ny))
      vals_out = DEFAULT_INIT

      allocate (ds(nx, ny))
      ds = huge (d)

      proj = this%Get_projection ()

      select case (var_name)
        case ('xlat')
          var_wrf = this%xlat

        case ('xlong')
          var_wrf = this%xlong

        case ('t2')
          var_wrf = this%t2_stag(this%ids:this%ide - 1, this%jds:this%jde - 1)

        case default
          write (ERROR_UNIT, *) 'Unknown variable name to interpolate'
          stop
      end select

      if (OUTPUT_LATLON_CHECK) call Write_latlon_check ()

        ! Algorithm
      do j = 1, ny
        do i = 1, nx
          call proj%Calc_ij (lats_in(i, j), lons_in(i, j), i_real, j_real)
          i_wrf = min (max (1, nint (i_real)), nx_wrf)
          j_wrf = min (max (1, nint (j_real)), ny_wrf)
          d = (this%xlat(i_wrf, j_wrf) - lats_in(i, j)) ** 2 + (this%xlong(i_wrf, j_wrf) - lons_in(i, j)) ** 2
          if (d < ds(i, j)) then
            vals_out(i, j) = var_wrf(i_wrf, j_wrf)
            ds(i, j) = d
          end if
        end do
      end do

    contains

      subroutine Write_latlon_check ()

        implicit none

        real :: lat_test, lon_test
        integer :: i, j, unit1, unit2


        open (newunit = unit1, file = 'latlons_wrf_and_wrfbis.dat')
        do j = this%jds, this%jde - 1
          do i = this%ids, this%ide - 1
            call proj%Calc_latlon (real(i), real(j), lat_test, lon_test)
            write (unit1, *) i, j, this%xlat(i, j), this%xlong(i, j), lat_test, lon_test
          end do
        end do
        close (unit1)

        open (newunit = unit2, file = 'latlons_fire.dat')
        do j = 1, ny
          do i = 1, nx
            write (unit2, *) i, j, lats_in(i, j), lons_in(i, j)
          end do
        end do
        close (unit2)

      end subroutine Write_latlon_check

    end subroutine Interp_var2grid_nearest

    subroutine Interpolate_wind2fire(this, config_flags,  & ! for debug output, <= 0 no output
          fire_wind_height,                               & ! interpolation height
          ids,ide, kds,kde, jds,jde,                      & ! atm grid dimensions
          ims,ime, kms,kme, jms,jme,                      &
          ips,ipe,jps,jpe,                                &
          its,ite,jts,jte,                                &
          ifds, ifde, jfds, jfde,                         & ! fire grid dimensions
          ifms, ifme, jfms, jfme,                         &
          ifts,ifte,jfts,jfte,                            &
          ir,jr,                                          & ! atm/fire grid ratio
          u,v,                                            & ! atm grid arrays in
          ph,phb,                                         &
          z0,                                             &
          uf,vf,z0f)                                        ! fire grid arrays out

      implicit none

      class (wrf_t), intent(in) :: this
      type (namelist_t), intent(in)          :: config_flags
      real, intent(in):: fire_wind_height                 ! height above the terrain for vertical interpolation
      integer, intent(in)::                             &
          ids,ide, kds,kde, jds,jde,                    & ! atm domain bounds
          ims,ime, kms,kme, jms,jme,                    & ! atm memory bounds
          ips,ipe,jps,jpe,                              &
          its,ite,jts,jte,                              & ! atm tile bounds
          ifds, ifde, jfds, jfde,                       & ! fire domain bounds
          ifms, ifme, jfms, jfme,                       & ! fire memory bounds
          ifts,ifte,jfts,jfte,                          & ! fire tile bounds
          ir,jr                                         ! atm/fire grid refinement ratio
      real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::&
          u,v,                                          & ! atm wind velocity, staggered
          ph,phb                                          ! geopotential
      real,intent(in),dimension(ims:ime,jms:jme) :: z0    ! roughness height
      real,intent(out), dimension(ifms:ifme,jfms:jfme)::&
          uf,vf                                           ! wind velocity fire grid nodes
      real,intent(in),dimension(ifms:ifme,jfms:jfme)::z0f ! roughness length in fire grid


      !*** local
      character(len=256)::msg
      real, dimension(its-2:ite+2,jts-2:jte+2):: ua,va   ! atm winds, interpolated over height, still staggered grid
      real, dimension(its-2:ite+2,kds:kde,jts-2:jte+2):: altw,altub,altvb,hgtu,hgtv ! altitudes
      integer:: i,j,k,ifts1,ifte1,jfts1,jfte1,ite1,jte1
      integer::itst,itet,jtst,jtet,itsu,iteu,jtsu,jteu,itsv,itev,jtsv,jtev
      integer::kdmax,its1,jts1,ips1,jps1
      integer::itsou,iteou,jtsou,jteou,itsov,iteov,jtsov,jteov
      real:: ground,loght,loglast,logz0,logfwh,ht,zr
      real::r_nan
      integer::i_nan
      equivalence (i_nan,r_nan)
      real::fire_wind_height_local,z0fc
      real::ust_d,wsf,wsf1,uf_temp,vf_temp
      real,parameter::vk_kappa=0.4

      !*** executable
      ! debug init local arrays
      i_nan=2147483647
      ua=r_nan
      va=r_nan
      altw=r_nan
      altub=r_nan
      hgtu=r_nan
      hgtv=r_nan

      !                            ^ j
      !        ------------        |
      !        |          |         ----> i
      !        u    p     |
      !        |          |    nodes in cell (i,j)
      !        ------v-----    view from top
      !
      !             v(ide,jde+1)
      !            -------x------
      !            |            |
      !            |            |
      ! u(ide,jde) x      x     x u(ide+1,jde)
      !            | p(ide,hde) |
      !            |            |   p=ph,phb,z0,...
      !            -------x------
      !              v(ide,jde)
      !
      ! staggered values set u(ids:ide+1,jds:jde) v(ids:ide,jds:jde+1)
      ! p=ph+phb set at (ids:ide,jds:jde)
      ! location of u(i,j) needs p(i-1,j) and p(i,j)
      ! location of v(i,j) needs p(i,j-1) and p(i,j)
      ! *** NOTE need HALO in ph, phb
      ! so we can compute only u(ids+1:ide,jds:jde) v(ids:ide,jds+1,jde)
      ! unless we extend p at the boundary
      ! but because we care about the fire way in the inside it does not matter
      ! if the fire gets close to domain boundary the simulation is over anyway

      ite1=snode(ite,ide,1)
      jte1=snode(jte,jde,1)

      ! indexing

      ! file for u
      itst=ifval(ids.eq.its,its,its-1)
      itet=ifval(ide.eq.ite,ite,ite+1)
      jtst=ifval(jds.ge.jts,jts,jts-1)
      jtet=ifval(jde.eq.jte,jte,jte+1)

      kdmax=kde-1   ! max layer to interpolate from, can be less

      do j = jtst,jtet
        do k=kds,kdmax+1
          do i = itst,itet
            altw(i,k,j) = (ph(i,k,j)+phb(i,k,j)) / G             ! altitude of the bottom w-point
          enddo
        enddo
      enddo

      ! values at u points
      itsu=ifval(ids.eq.its,its+1,its)  ! staggered direction
      iteu=ifval(ide.eq.ite,ite,ite+1)  ! staggered direction
      jtsu=ifval(jds.ge.jts,jts,jts-1)
      jteu=ifval(jde.eq.jte,jte,jte+1)

      do j = jtsu,jteu
        do k=kds,kdmax+1
          do i = itsu,iteu
            altub(i,k,j)= 0.5*(altw(i-1,k,j)+altw(i,k,j))      ! altitude of the bottom point under u-point
          enddo
        enddo
        do k=kds,kdmax
          do i = itsu,iteu
            hgtu(i,k,j) =  0.5*(altub(i,k,j)+altub(i,k+1,j)) - altub(i,kds,j)  ! height of the u-point above the ground
          enddo
        enddo
      enddo

      ! values at v points
      jtsv=ifval(jds.eq.jts,jts+1,jts)  ! staggered direction
      jtev=ifval(jde.eq.jte,jte,jte+1)  ! staggered direction
      itsv=ifval(ids.ge.its,its,its-1)
      itev=ifval(ide.eq.ite,ite,ite+1)

      do j = jtsv,jtev
        do k=kds,kdmax+1
          do i = itsv,itev
            altvb(i,k,j)= 0.5*(altw(i,k,j-1)+altw(i,k,j))      ! altitude of the bottom point under v-point
          enddo
        enddo
        do k=kds,kdmax
          do i = itsv,itev
            hgtv(i,k,j) =  0.5*(altvb(i,k,j)+altvb(i,k+1,j)) - altvb(i,kds,j)  ! height of the v-point above the ground
          enddo
        enddo
      enddo

      ! DME
      if (config_flags%fire_lsm_zcoupling) then
        logfwh = log(config_flags%fire_lsm_zcoupling_ref)
        fire_wind_height_local = config_flags%fire_lsm_zcoupling_ref
      else
        logfwh = log(fire_wind_height)
        fire_wind_height_local = fire_wind_height
      endif

          ! interpolate u, staggered in X

          do j = jtsu,jteu            ! compute on domain by 1 smaller, otherwise z0 and ph not available
      !    do i = itsu,iteu        ! compute with halo 2
            do i = itsu,iteu
              zr = 0.5*(z0(i,j)+z0(i-1,j)) ! interpolated roughness length under this u point
              if(fire_wind_height_local > zr)then       !
                do k=kds,kdmax
                  ht = hgtu(i,k,j)      ! height of this u point above the ground
                  if( .not. ht < fire_wind_height_local) then ! found layer k this point is in
                    loght = log(ht)
                    if(k.eq.kds)then               ! first layer, log linear interpolation from 0 at zr
                      logz0 = log(zr)
                      ua(i,j)= u(i,k,j)*(logfwh-logz0)/(loght-logz0)
                    else                           ! log linear interpolation
                      loglast=log(hgtu(i,k-1,j))
                      ua(i,j)= u(i,k-1,j) + (u(i,k,j) - u(i,k-1,j)) * ( logfwh - loglast) / (loght - loglast)
                    endif
                    goto 10
                  endif
                  if(k.eq.kdmax)then                 ! last layer, still not high enough
                    ua(i,j)=u(i,k,j)
                  endif
                enddo
      10        continue
              else  ! roughness higher than the fire wind height
                ua(i,j)=0.
              endif
            enddo
          enddo

          do j = jtsu,jteu
            ua(itsu-1,j)=ua(itsu,j)
          enddo

          ! interpolate v, staggered in Y

          do j = jtsv,jtev
            do i = itsv,itev
              zr = 0.5*(z0(i,j-1)+z0(i,j)) ! roughness length under this v point
              if(fire_wind_height_local > zr)then       !
                do k=kds,kdmax
                  ht = hgtv(i,k,j)      ! height of this u point above the ground
                  if( .not. ht < fire_wind_height_local) then ! found layer k this point is in
                    loght = log(ht)
                    if(k.eq.kds)then               ! first layer, log linear interpolation from 0 at zr
                      logz0 = log(zr)
                      va(i,j)= v(i,k,j)*(logfwh-logz0)/(loght-logz0)
                    else                           ! log linear interpolation
                      loglast=log(hgtv(i,k-1,j))
                      va(i,j)= v(i,k-1,j) + (v(i,k,j) - v(i,k-1,j)) * ( logfwh - loglast) / (loght - loglast)
                    endif
                    goto 11
                  endif
                  if(k.eq.kdmax)then                 ! last layer, still not high enough
                    va(i,j)=v(i,k,j)
                  endif
                enddo
      11        continue
              else  ! roughness higher than the fire wind height
                va(i,j)=0.
              endif
            enddo
          enddo

          do i = itsv,itev
            va(i,jtsv-1)=va(i,jtsv)
          enddo
          ips1 = ifval(ips.eq.ids,ips+1,ips)

          call continue_at_boundary(1,1,0., & ! x direction
             its-2,ite+2,jts-2,jte+2,       & ! memory dims atm grid tile
             ids+1,ide,jds,jde, &     ! domain dims - where u defined
             itsu,iteu,jtsu,jteu, & ! tile dims - in nonextended direction one beyond if at patch boundary but not domain
             itsou,iteou,jtsou,jteou, & ! out, where set
             ua)                           ! array

          jps1 = ifval(jps.eq.jds,jps+1,jps)

          call continue_at_boundary(1,1,0., & ! y direction
             its-2,ite+2,jts-2,jte+2,       & ! memory dims atm grid tile
             ids,ide,jds+1,jde, &      ! domain dims - where v wind defined
             itsv,itev,jtsv,jtev, & ! tile dims
             itsov,iteov,jtsov,jteov, & ! where set
             va)                           ! array

      !      ---------------
      !     | F | F | F | F |   Example of atmospheric and fire grid with
      !     |-------|-------|   ir=jr=4.
      !     | F | F | F | F |   Winds are given at the midpoints of the sides of the atmosphere grid,
      !     ua------z-------|   interpolated to midpoints of the cells of the fine fire grid F.
      !     | F | F | F | F |   This is (1,1) cell of atmosphere grid, and [*] is the (1,1) cell of the fire grid.
      !     |---------------|   ua(1,1) <--> uf(0.5,2.5)
      !     | * | F | F | F |   va(1,1) <--> vf(2.5,0.5)
      !      -------va------    za(1,1) <--> zf(2.5,2.5)
      !
      !   ^ x2
      !   |  --------va(1,2)---------
      !   | |            |           |   Example of atmospheric and fire grid with
      !   | |            |           |   ir=jr=1.
      !   | |          za,zf         |   Winds are given at the midpoints of the sides of the atmosphere grid,
      !   | ua(1,1)----uf,vf-----ua(2,1) interpolated to midpoints of the cells of the (the same) fire grid
      !   | |           (1,1)        |   ua(1,1) <--> uf(0.5,1)
      !   | |            |           |   va(1,1) <--> vf(1,0.5)
      !   | |            |           |   za(1,1) <--> zf(1,1)
      !   |  --------va(1,1)---------
      !   |--------------------> x1
      !
      ! Meshes are aligned by the lower left cell of the domain. Then in the above figure
      ! u = node with the ua component of the wind at (ids,jds), midpoint of side
      ! v = node with the va component of the wind at (ids,jds), midpoint of side
      ! * = fire grid node at (ifds,jfds)
      ! z = node with height, midpoint of cell
      !
      ! ua(ids,jds)=uf(ifds-0.5,jfds+jr*0.5-0.5)         = uf(ifds-0.5,jfds+(jr-1)*0.5)
      ! va(ids,jds)=vf(ifds+ir*0.5-0.5,jfds-0.5)         = vf(ifds+(ir-1)*0.5,jfds-0.5)
      ! za(ids,jds)=zf(ifds+ir*0.5-0.5,jfds+jr*0.5-0.5)  = zf(ifds+(ir-1)*0.5,jfds+(jr-1)*0.5)

          ! ifts1=max(snode(ifts,ifps,-1),ifds) ! go 1 beyond patch boundary but not at domain boundary
          ! ifte1=min(snode(ifte,ifpe,+1),ifde)
          ! jfts1=max(snode(jfts,jfps,-1),jfds)
          ! jfte1=min(snode(jfte,jfpe,+1),jfde)

          call interpolate_2d(  &
              its-2,ite+2,jts-2,jte+2,& ! memory dims atm grid tile
              itsou,iteou,jtsou,jteou,& ! where set
              ifms,ifme,jfms,jfme,    & ! array dims fire grid
              ifts,ifte,jfts,jfte,& ! dimensions on the fire grid to interpolate to
              ir,jr,                  & ! refinement ratio
              real(ids),real(jds),ifds-0.5,jfds+(jr-1)*0.5, & ! line up by lower left corner of domain
              ua,                     & ! in atm grid
              uf)                      ! out fire grid

          call interpolate_2d(  &
              its-2,ite+2,jts-2,jte+2,& ! memory dims atm grid tile
              itsov,iteov,jtsov,jteov,& ! where set
              ifms,ifme,jfms,jfme,    & ! array dims fire grid
              ifts,ifte,jfts,jfte,& ! dimensions on the fire grid to interpolate to
              ir,jr,                  & ! refinement ratio
              real(ids),real(jds),ifds+(ir-1)*0.5,jfds-0.5, & ! line up by lower left corner of domain
              va,                     & ! in atm grid
              vf)                      ! out fire grid

      ! DME here code to extrapolate mid-flame height velocity -> fire_lsm_zcoupling = .true.
      if (config_flags%fire_lsm_zcoupling) then
        do j = jfts,jfte
          do i = ifts,ifte
            uf_temp=uf(i,j)
            vf_temp=vf(i,j)
            wsf=max(sqrt(uf_temp**2.+vf_temp**2.),0.1)
            z0fc=z0f(i,j)
            ust_d=wsf*vk_kappa/log(config_flags%fire_lsm_zcoupling_ref/z0fc)
            wsf1=(ust_d/vk_kappa)*log((fire_wind_height+z0fc)/z0fc)
            uf(i,j)=wsf1*uf_temp/wsf
            vf(i,j)=wsf1*vf_temp/wsf
          enddo
        enddo
      endif

      return

      end subroutine Interpolate_wind2fire

      subroutine Interpolate_z2fire(this,      & ! for debug output, <= 0 no output
            ifds,ifde, jfds,jfde,              & ! fire grid dimensions
            ifms,ifme, jfms,jfme,              &
            ifts,ifte, jfts,jfte,              &
            ir, jr,                            & ! atm/fire grid ratio
            zs,                                & ! atm grid arrays in
            zsf, flag_z0)                          ! fire grid arrays out

        implicit none
        !*** purpose: interpolate height or any other 2d variable defined at mesh cell centers

        !*** arguments
        class (wrf_t), intent (in) :: this
        integer, intent(in) :: ifds,ifde, jfds,jfde,     & ! fire domain bounds
                               ifms,ifme, jfms,jfme,     & ! fire memory bounds
                               ifts,ifte, jfts,jfte,     & ! fire tile bounds
                               ir, jr                        ! atm/fire grid refinement ratio
        real, intent(in), dimension(this%ims:this%ime, this%jms:this%jme) :: zs  ! terrain height at atm cell centers & ! terrain height
        real,intent(out), dimension(ifms:ifme, jfms:jfme) :: &
            zsf                                              ! terrain height fire grid nodes
        integer,intent(in) :: flag_z0


        !*** local
        real, dimension(this%its-2:this%ite+2,this%jts-2:this%jte+2) :: za      ! terrain height
        integer :: i,j,jts1,jte1,its1,ite1,jfts1,jfte1,ifts1,ifte1,itso,jtso,iteo,jteo

        ! terrain height

        jts1 = max(this%jts-1, this%jds) ! lower loop limit by one less when at end of domain
        its1 = max(this%its-1, this%ids) ! ASSUMES THE HALO IS THERE if patch != domain
        jte1 = min(this%jte+1, this%jde)
        ite1 = min(this%ite+1, this%ide)

        do j = jts1,jte1
            do i = its1,ite1
                ! copy to local array
                za(i, j) = zs(i, j)
            enddo
        enddo

        call continue_at_boundary(1,1,0., & ! do x direction or y direction
        this%its-2,this%ite+2,this%jts-2,this%jte+2,           &                ! memory dims
        this%ids,this%ide,this%jds,this%jde, &            ! domain dims - winds defined up to +1
        its1,ite1,jts1,jte1, &                ! tile dims
        itso,jtso,iteo,jteo, &
        za)                               ! array

        ! interpolate to tile plus strip along domain boundary if at boundary
        jfts1 = snode(jfts, jfds, -1) ! lower loop limit by one less when at end of domain
        ifts1 = snode(ifts, ifds, -1)
        jfte1 = snode(jfte, jfde, +1)
        ifte1 = snode(ifte, ifde, +1)

        call interpolate_2d(  &
            this%its-2,this%ite+2,this%jts-2,this%jte+2,     & ! memory dims atm grid tile
            its1-1,ite1+1,jts1-1,jte1+1, & ! where atm grid values set
            ifms,ifme,jfms,jfme,         & ! array dims fire grid
            ifts1,ifte1,jfts1,jfte1,     & ! dimensions fire grid tile
            ir,jr,                       & ! refinement ratio
            real(this%ids),real(this%jds),ifds+(ir-1)*0.5,jfds+(jr-1)*0.5, & ! line up by lower left corner of domain
            za,                     & ! in atm grid
            zsf)                      ! out fire grid

        if (flag_z0 .eq. 1 ) then
          do j=jfts1,jfte1
            do i=ifts1,ifte1
              zsf(i, j) = max(zsf(i, j), 0.001)
            enddo
          enddo
        endif

    end subroutine Interpolate_z2fire

    subroutine Load_atmosphere_test1 (this, config_flags)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (wrf_t), intent (in out) :: this
      type (namelist_t), intent (in) :: config_flags

      integer, parameter :: KDE_TEST4 = 51
      logical, parameter  :: DEBUG = .true.
      real, parameter, dimension(KDE_TEST4) :: PH_2_PROFILE = [ 0.00000000, 13.8204203, 26.5920029, 38.3797607, 49.2364426, &
          59.2161636, &
          68.3687134, 76.7347412, 84.2828140, 91.0704575, 97.1466751, 102.561920, 107.365410, 111.603745, 115.318642, 118.537750, &
          121.365837, 123.837875, 125.986755, 127.842049, 129.431870, 130.781860, 131.915421, 132.854492, 133.619125, 134.227219, &
          134.695877, 134.984268, 135.150009, 135.207733, 135.170517, 135.050400, 134.858658, 134.604935, 134.298111, 133.934662, &
          133.517365, 133.069000, 132.595490, 132.102081, 131.593399, 131.073563, 130.546265, 130.014786, 129.481873, 128.950027, &
          128.421371, 127.897804, 127.380852, 126.871918, 126.372375 ]
      real, dimension(KDE_TEST4) :: PHB_PROFILE = [ 0.00000000, 1707.28320, 3361.90430, 4964.50537, 6515.84424, &
          8016.72852, &
          9467.99121, 10870.6191, 12225.7480, 13534.3027, 14797.2246, 16015.4951, 17190.0977, 18322.0605, 19412.4102, &
          20462.0918, 21471.9375, 22443.0254, 23376.4375, 24273.2539, 25134.5645, 25961.4453, 26754.9785, 27516.2227, &
          28246.2402, 28946.0723, 29616.7441, 30259.1211, 30874.1660, 31462.8828, 32026.2500, 32565.2148, 33080.7070, &
          33573.6367, 34044.8750, 34495.2852, 34925.6172, 35336.6758, 35729.2500, 36104.1055, 36461.9805, 36803.5859, &
          37129.6133, 37440.7266, 37737.5625, 38020.7383, 38290.8477, 38548.4609, 38794.1250, 39028.3711, 39251.6992 ]
      real, dimension(KDE_TEST4) :: RHO_PROFILE = [ 1.20651519, 1.18431544, 1.16323423, 1.14317775, 1.12409139, &
          1.10593629, &
          1.08858371, 1.07191896, 1.05606472, 1.04098368, 1.02663994, 1.01299584, 1.00001323, 0.987663746, 0.976037860, &
          0.965157628, 0.954824507, 0.945008934, 0.935684621, 0.926825523, 0.918407857, 0.910409093, 0.902807295, &
          0.895582259, 0.888715386, 0.882187963, 0.876253724, 0.870619595, 0.865259767, 0.860160649, 0.855310082, &
          0.850694954, 0.846304417, 0.842127442, 0.838175058, 0.834568560, 0.831135690, 0.827868402, 0.824758351, &
          0.821798563, 0.818981647, 0.816300511, 0.813748956, 0.811320662, 0.809009790, 0.806810796, 0.804717839, &
          0.802726388, 0.800831199, 0.799025893, 0.00000000 ]
      real, dimension(KDE_TEST4) :: ZATW_PROFILE = [ 0.00000000, 175.443787, 345.412445, 509.978088, 669.223267, 823.235901, &
          972.106018, 1115.93811, 1254.84509, 1388.92688, 1518.28442, 1643.02307, 1763.24792, 1879.06860, 1990.59399, 2097.92334, &
          2201.15210, 2300.39380, 2395.76172, 2487.36938, 2575.33081, 2659.75806, 2740.76392, 2818.45850, 2892.95190, 2964.35254, &
          3032.76636, 3098.27759, 3160.99023, 3221.00806, 3278.43213, 3333.36035, 3385.88843, 3436.11011, 3484.11523, 3529.99146, &
          3573.81567, 3615.67212, 3655.64136, 3693.80273, 3730.23169, 3765.00098, 3798.18140, 3829.84106, 3860.04492, 3888.85693, &
          3916.33716, 3942.54419, 3967.53345, 3991.35986, 4014.07422 ]
      real, parameter, dimension(KDE_TEST4) :: DZ8W_PROFILE = [ 175.443787, 169.968658, 164.565643, 159.245178, 154.012634, &
          148.870117, &
          143.832092, 138.906982, 134.081787, 129.357544, 124.738647, 120.224854, 115.820679, 111.525391, 107.329346, 103.228760, &
          99.2416992, 95.3679199, 91.6076660, 87.9614258, 84.4272461, 81.0058594, 77.6945801, 74.4934082, 71.4006348, 68.4138184, &
          65.5112305, 62.7126465, 60.0178223, 57.4240723, 54.9282227, 52.5280762, 50.2216797, 48.0051270, 45.8762207, 43.8242188, &
          41.8564453, 39.9692383, 38.1613770, 36.4289551, 34.7692871, 33.1804199, 31.6596680, 30.2038574, 28.8120117, 27.4802246, &
          26.2070312, 24.9892578, 23.8264160, 22.7143555, 0.00000000 ]
      real, parameter :: Z0 = 0.1, MUT = 38739.3828

      integer :: k


      if (DEBUG) write (OUTPUT_UNIT, *) '  Entering subroutine Load_atmosphere_test1'

        ! 3D arrays
      do k = 1, config_flags%kde
        this%ph_stag(:, k, :) = PH_2_PROFILE(k)
        this%phb_stag(:, k, :) = PHB_PROFILE(k)
        this%rho_stag(:, k, :) = RHO_PROFILE(k)
        this%z_at_w_stag(:, k, :) = ZATW_PROFILE(k)
        this%dz8w_stag(:, k, :) = DZ8W_PROFILE(k)
      end do
      this%u3d_stag = 3.0
      this%v3d_stag = 0.0

        ! 2D arrays
      this%z0_stag = Z0
      this%mut_stag = MUT

      if (DEBUG) write (OUTPUT_UNIT, *) '  Leaving subroutine Load_atmosphere_test1'

    end subroutine Load_atmosphere_test1

    subroutine Print_domain (this)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (wrf_t), intent(in out) :: this


      write (OUTPUT_UNIT, *) ''
      write (OUTPUT_UNIT, *) 'ids = ', this%ids, 'ide = ', this%ide
      write (OUTPUT_UNIT, *) 'jds = ', this%jds, 'jde = ', this%jde
      write (OUTPUT_UNIT, *) 'kds = ', this%kds, 'kde = ', this%kde

      write (OUTPUT_UNIT, *) 'ims = ', this%ims, 'ime = ', this%ime
      write (OUTPUT_UNIT, *) 'jms = ', this%jms, 'jme = ', this%jme
      write (OUTPUT_UNIT, *) 'kms = ', this%kms, 'kme = ', this%kme

      write (OUTPUT_UNIT, *) 'ips = ', this%ips, 'ipe = ', this%ipe
      write (OUTPUT_UNIT, *) 'jps = ', this%jps, 'jpe = ', this%jpe
      write (OUTPUT_UNIT, *) 'kps = ', this%kps, 'kpe = ', this%kpe

      write (OUTPUT_UNIT, *) 'its = ', this%its, 'ite = ', this%ite
      write (OUTPUT_UNIT, *) 'jts = ', this%jts, 'jte = ', this%jte
      write (OUTPUT_UNIT, *) 'kts = ', this%kts, 'kte = ', this%kte

    end subroutine Print_domain

    subroutine Read_wrf_input (this)

      use, intrinsic :: iso_fortran_env, only : ERROR_UNIT, OUTPUT_UNIT

      implicit none

      class (wrf_t), intent (in out) :: this

      real :: check_val
      integer :: io_stat, wrf_input_unit


      open (newunit = wrf_input_unit, file = 'wrf_input.dat', iostat = io_stat)
      if (io_stat /= 0) then
        write (ERROR_UNIT, *) 'Problems opening the wrf_input.dat file'
        stop
      end if

      check_val = 0
      read (wrf_input_unit, *, iostat = io_stat) this%u3d_stag
      if (io_stat /= 0) then
        write (ERROR_UNIT, *) 'Problems reading wrf_input.dat'
        stop
      end if

      read (wrf_input_unit, *) this%v3d_stag
      read (wrf_input_unit, *) this%ph_stag
      read (wrf_input_unit, *) this%phb_stag
      read (wrf_input_unit, *) this%rho_stag
      read (wrf_input_unit, *) this%z_at_w_stag
      read (wrf_input_unit, *) this%dz8w_stag
      read (wrf_input_unit, *) this%z0_stag
      read (wrf_input_unit, *) this%mut_stag
      read (wrf_input_unit, *) check_val

      write (OUTPUT_UNIT, *) check_val

      close (wrf_input_unit)

    end subroutine Read_wrf_input

    ! function to go beyond domain boundary if tile is next to it
    pure integer function snode(t,d,i)
      implicit none
      integer, intent(in)::t,d,i
      if(t.ne.d)then
          snode=t
      else
          snode=t+i
      endif
    end function snode

    subroutine Update_atm_state (this, datetime_now)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (wrf_t), intent(in out) :: this
!      type (namelist_t), intent (in) :: config_flags
      type (datetime_t), intent (in) :: datetime_now

      logical, parameter :: DEBUG_LOCAL = .true.
      integer :: i, j, k


!      must go outside
!      If_update_atm: if (this%datetime_now == this%datetime_next_atm_update) then
!        if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'Updating wrfdata...'
!        if (DEBUG_LOCAL) call this%datetime_now%Print_datetime ()


          ! Update t2_stag
        call this%Get_t2 (datetime_now)
        this%t2_stag(this%ids:this%ide - 1, this%jds:this%jde - 1) = this%t2(:, :)
        call this%Destroy_t2 ()

          ! Update q2
        call this%Get_q2 (datetime_now)
        this%q2_stag(this%ids:this%ide - 1, this%jds:this%jde - 1) = this%q2(:, :)
        call this%Destroy_q2 ()

          ! Update psfc
        call this%Get_psfc (datetime_now)
        this%psfc_stag(this%ids:this%ide - 1, this%jds:this%jde - 1) = this%psfc(:, :)
        call this%Destroy_psfc ()

          ! Update rainc
        call this%Get_rainc (datetime_now)
        this%rainc_stag(this%ids:this%ide - 1, this%jds:this%jde - 1) = this%rainc(:, :)
        call this%Destroy_rainc ()

          ! Update rainnc
        call this%Get_rainnc (datetime_now)
        this%rainnc_stag(this%ids:this%ide - 1, this%jds:this%jde - 1) = this%rainnc(:, :)
        call this%Destroy_rainnc ()

          ! Update z0
        call this%Get_z0 (datetime_now)
        this%z0_stag(this%ids:this%ide - 1, this%jds:this%jde - 1) = this%z0(:, :)
        call this%Destroy_z0 ()

          ! Update MUT
        call this%Get_mut (datetime_now)
        this%mut_stag(this%ids:this%ide - 1, this%jds:this%jde - 1) = this%mut(:, :)
        call this%Destroy_mut ()

          ! Update U3D
        call this%Get_u3d_stag (datetime_now)
        do k = this%kds, this%kde - 1
          do j = this%jds, this%jde - 1
            do i = this%ids, this%ide
              this%u3d_stag(i, k, j) = this%u3d(i, j, k)
            end do
          end do
        end do
        call this%Destroy_u3d ()

          ! Update V3D
        call this%Get_v3d_stag (datetime_now)
        do k = this%kds, this%kde - 1
          do j = this%jds, this%jde
            do i = this%ids, this%ide - 1
              this%v3d_stag(i, k, j) = this%v3d(i, j, k)
            end do
          end do
        end do
        call this%Destroy_v3d ()

          ! PHB
        call this%Get_phb_stag (datetime_now)
        do k = this%kds, this%kde
          do j = this%jds, this%jde - 1
            do i = this%ids, this%ide - 1
              this%phb_stag(i, k, j) = this%phb(i, j, k)
            end do
          end do
        end do
        call this%Destroy_phb ()

          ! PH
        call this%Get_ph_stag (datetime_now)
        do k = this%kds, this%kde
          do j = this%jds, this%jde - 1
            do i = this%ids, this%ide - 1
              this%ph_stag(i, k, j) = this%ph(i, j, k)
            end do
          end do
        end do

          ! DZ8W
        call this%Get_dz8w (datetime_now)
        do k = this%kds, this%kde - 1
          do j = this%jds, this%jde - 1
            do i = this%ids, this%ide - 1
              this%dz8w_stag(i, k, j) = this%dz8w(i, j, k)
            end do
          end do
        end do
        call this%Destroy_dz8w ()

          ! Z_AT_W
        call this%Get_z_at_w (datetime_now)
        do k = this%kds, this%kde
          do j = this%jds, this%jde - 1
            do i = this%ids, this%ide - 1
              this%z_at_w_stag(i, k, j) = this%z_at_w(i, j, k)
            end do
          end do
        end do
        call this%Destroy_z_at_w ()

          ! RHO
        call this%Get_rho (datetime_now)
        do k = this%kds, this%kde - 1
          do j = this%jds, this%jde - 1
            do i = this%ids, this%ide - 1
              this%rho_stag(i, k, j) = this%rho(i, j, k)
            end do
          end do
        end do
        call this%Destroy_rho ()

!        call this%datetime_next_atm_update%Add_seconds (config_flags%interval_atm)
!      end if If_update_atm

    end subroutine Update_atm_state

  end module wrf_mod

