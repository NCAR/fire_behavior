  module wrf_mod

    use netcdf_mod, only : Get_netcdf_var, Get_netcdf_att, Get_netcdf_dim
    use datetime_mod, only : datetime_t
    use proj_lc_mod, only : proj_lc_t
    use namelist_mod, only : namelist_t
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
      real, dimension(:, :, :), allocatable :: u3d, v3d, phb, ph, phl, pres
      real, dimension(:, :, :), allocatable :: u3d_stag, v3d_stag, phb_stag, ph_stag
        ! 2D
      real, dimension(:, :), allocatable :: lats, lons, lats_c, lons_c, t2, q2, z0, mut, psfc, rain, rainc, rainnc, ua, va
      real, dimension(:, :), allocatable :: t2_stag, q2_stag, z0_stag, mut_stag, psfc_stag, rainc_stag, rainnc_stag

      integer :: ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe, &
                 its, ite, jts, jte, kts, kte
        ! projection
      real :: cen_lat, cen_lon, dx, dy, truelat1, truelat2, stand_lon
    contains
!      procedure, public :: Add_fire_tracer_emissions => Add_fire_tracer_emissions
      procedure, public :: Destroy_mut => Destroy_mut
      procedure, public :: Destroy_phl => Destroy_geopotential_levels
      procedure, public :: Destroy_pres => Destroy_pressure_levels
      procedure, public :: Destroy_ph => Destroy_geopotential
      procedure, public :: Destroy_phb => Destroy_geopotential_base
      procedure, public :: Destroy_psfc => Destroy_surface_pressure
      procedure, public :: Destroy_rainc => Destroy_rain_convective
      procedure, public :: Destroy_rainnc => Destroy_rain_non_convective
      procedure, public :: Destroy_rain => Destroy_rain
      procedure, public :: Destroy_q2 => Destroy_specific_humidity_2m
      procedure, public :: Destroy_t2 => Destroy_temperature_2m
      procedure, public :: Destroy_u3d => Destroy_zonal_wind
      procedure, public :: Destroy_v3d => Destroy_meridional_wind
      procedure, public :: Destroy_z0 => Destroy_z0
!      procedure, public :: Fire_tendency => Fire_tendency
      procedure, public :: Get_datetime_index => Get_datetime_index
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
      procedure, public :: Get_psfc => Get_surface_pressure
      procedure, public :: Get_q2 => Get_specific_humidity_2m
      procedure, public :: Get_t2 => Get_temperature_2m
      procedure, public :: Get_u3d => Get_zonal_wind_3d
      procedure, public :: Get_u3d_stag => Get_zonal_wind_stag_3d
      procedure, public :: Get_v3d => Get_meridional_wind_3d
      procedure, public :: Get_v3d_stag => Get_meridional_wind_stag_3d
      procedure, public :: Get_z0 => Get_z0
      procedure, public :: Interp_var2grid_nearest => Interp_var2grid_nearest
      procedure, public :: Print_domain => Print_domain
      procedure, public :: Update_atm_state => Update_atm_state
    end type wrf_t

    interface wrf_t
      module procedure Wrf_t_const
    end interface wrf_t

  contains

!    subroutine Add_fire_tracer_emissions(    &
!           this,                             &
!           ifms,ifme,jfms,jfme,              &
!           ifts,ifte,jtfs,jfte,              &
!           ids,ide,kds,kde,jds,jde,          &
!           ims,ime,kms,kme,jms,jme,          &
!           its,ite,kts,kte,jts,jte,          &
!           rho,dz8w,                         &
!           tracer,emis)
!
!      implicit none
!
!      class (wrf_t), intent(in out) :: this
!      integer, intent(in) :: ifms,ifme,jfms,jfme,  &
!                          ifts,ifte,jtfs,jfte,     &
!                          ids,ide,kds,kde,jds,jde, &
!                          ims,ime,kms,kme,jms,jme, &
!                          its,ite,kts,kte,jts,jte
!      real, intent(in) :: rho(ims:ime,kms:kme,jms:jme),dz8w(ims:ime,kms:kme,jms:jme)
!      real, intent(in), dimension(ifms:ifme,jfms:jfme) :: emis
!      real, intent(inout) :: tracer(ims:ime,kms:kme,jms:jme)
!
!      integer :: isz1,jsz1,isz2,jsz2,ir,jr
!      integer :: i,j,ibase,jbase,i_f,ioff,j_f,joff
!      real :: avgw
!
!
!      isz1 = ite-its+1
!      jsz1 = jte-jts+1
!      isz2 = ifte-ifts+1
!      jsz2 = jfte-jtfs+1
!      ir=isz2/isz1
!      jr=jsz2/jsz1
!      avgw = 1.0/(ir*jr)
!
!      do j=max(jds+1,jts),min(jte,jde-2)
!        jbase=jtfs+jr*(j-jts)
!        do i=max(ids+1,its),min(ite,ide-2)
!          ibase=ifts+ir*(i-its)
!          do joff=0,jr-1
!            j_f=joff+jbase
!            do ioff=0,ir-1
!              i_f=ioff+ibase
!              if (num_tracer >0) then
!                tracer(i,kts,j)=tracer(i,kts,j) &
!                  + (avgw*emis(i_f,j_f)*1000/(rho(i,kts,j)*dz8w(i,kts,j))) ! g_smoke/kg_air
!              endif
!            enddo
!          enddo
!        enddo
!      enddo
!
!    end subroutine Add_fire_tracer_emissions

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

!    subroutine Fire_tendency(this,   &
!        ids,ide, kds,kde, jds,jde,   & ! dimensions
!        ims,ime, kms,kme, jms,jme,   &
!        its,ite, kts,kte, jts,jte,   &
!        grnhfx,grnqfx,canhfx,canqfx, & ! heat fluxes summed up to  atm grid
!        alfg,alfc,z1can,             & ! coeffients, properties, geometry
!        z_at_w,dz8w,mu,c1h,c2h,rho,  &
!        rthfrten,rqvfrten)             ! theta and Qv tendencies
!
!    ! This routine is atmospheric physics
!    ! it does NOT go into module_fr_fire_phys because it is not related to fire physical processes
!
!    ! --- this routine takes fire generated heat and moisture fluxes and
!    !     calculates their influence on the theta and water vapor
!    ! --- note that these tendencies are valid at the Arakawa-A location
!
!      implicit none
!
!    ! --- incoming variables
!
!      class (wrf_t), intent(in out) :: this
!      integer, intent(in) :: ids,ide, kds,kde, jds,jde, &
!                               ims,ime, kms,kme, jms,jme, &
!                               its,ite, kts,kte, jts,jte
!
!      real, intent(in), dimension( ims:ime,jms:jme ) :: grnhfx,grnqfx  ! w/m^2
!      real, intent(in), dimension( ims:ime,jms:jme ) :: canhfx,canqfx  ! w/m^2
!      real, intent(in), dimension( ims:ime,jms:jme ) :: mu  ! dry air mass (pa)
!      real, intent(in), dimension( kms:kme         ) :: c1h, c2h ! hybrid coordinate weights
!
!      real, intent(in), dimension( ims:ime,kms:kme,jms:jme ) :: z_at_w ! m abv sealvl
!      real, intent(in), dimension( ims:ime,kms:kme,jms:jme ) :: dz8w   ! dz across w-lvl
!      real, intent(in), dimension( ims:ime,kms:kme,jms:jme ) :: rho    ! density
!
!      real, intent(in) :: alfg ! extinction depth surface fire heat (m)
!      real, intent(in) :: alfc ! extinction depth crown  fire heat (m)
!      real, intent(in) :: z1can    ! height of crown fire heat release (m)
!
!    ! --- outgoing variables
!
!      real, intent(out), dimension( ims:ime,kms:kme,jms:jme ) ::   &
!           rthfrten, & ! theta tendency from fire (in mass units)
!           rqvfrten    ! Qv tendency from fire (in mass units)
!    ! --- local variables
!
!      integer :: i,j,k
!      integer :: i_st,i_en, j_st,j_en, k_st,k_en
!
!      real :: cp_i
!      real :: rho_i
!      real :: xlv_i
!      real :: z_w
!      real :: fact_g, fact_c
!      real :: alfg_i, alfc_i
!
!      real, dimension( its:ite,kts:kte,jts:jte ) :: hfx,qfx
!
!
!      do j=jts,jte
!        do k=kts,min(kte+1,kde)
!          do i=its,ite
!            rthfrten(i,k,j)=0.
!            rqvfrten(i,k,j)=0.
!          enddo
!        enddo
!      enddo
!
!    ! --- set some local constants
!
!      cp_i = 1./cp     ! inverse of specific heat
!      xlv_i = 1./xlv   ! inverse of latent heat
!      alfg_i = 1./alfg
!      alfc_i = 1./alfc
!
!    ! --- set loop indicies : note that
!
!      i_st = MAX(its,ids+1)
!      i_en = MIN(ite,ide-1)
!      k_st = kts
!      k_en = MIN(kte,kde-1)
!      j_st = MAX(jts,jds+1)
!      j_en = MIN(jte,jde-1)
!
!    ! --- distribute fluxes
!
!      do j = j_st,j_en
!        do k = k_st,k_en
!          do i = i_st,i_en
!
!            ! --- set z (in meters above ground)
!            z_w = z_at_w(i,k,j) - z_at_w(i, 1, j)
!
!            ! --- the fire tendencies are too small in uppder levels
!            if (z_w > 2500.) then
!              cycle
!            end if
!
!            ! --- heat flux
!            fact_g = cp_i * EXP( - alfg_i * z_w )
!            if ( z_w < z1can ) then
!                   fact_c = cp_i
!            else
!                   fact_c = cp_i * EXP( - alfc_i * (z_w - z1can) )
!            end if
!            hfx(i,k,j) = fact_g * grnhfx(i,j) + fact_c * canhfx(i,j)
!
!            ! --- vapor flux
!            fact_g = xlv_i * EXP( - alfg_i * z_w )
!
!            if (z_w < z1can) then
!                   fact_c = xlv_i
!            else
!                   fact_c = xlv_i * EXP( - alfc_i * (z_w - z1can) )
!            end if
!
!            qfx(i,k,j) = fact_g * grnqfx(i,j) + fact_c * canqfx(i,j)
!
!
!          end do
!        end do
!      end do
!
!    ! --- add flux divergence to tendencies
!    !
!    !   multiply by dry air mass (mu) to eliminate the need to
!    !   call sr. calculate_phy_tend (in dyn_em/module_em.F)
!
!      do j = j_st,j_en
!        do k = k_st,k_en-1
!          do i = i_st,i_en
!
!            rho_i = 1./rho(i,k,j)
!
!            rthfrten(i,k,j) = - (c1h(k)*mu(i,j)+c2h(k)) * rho_i * (hfx(i,k+1,j)-hfx(i,k,j)) / dz8w(i,k,j)
!            rqvfrten(i,k,j) = - (c1h(k)*mu(i,j)+c2h(k)) * rho_i * (qfx(i,k+1,j)-qfx(i,k,j)) / dz8w(i,k,j)
!
!          end do
!        end do
!      end do
!
!      return
!
!    end subroutine Fire_tendency

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

    subroutine Get_geopotential_levels (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :, :), allocatable :: var4d, var4d2
      integer :: nt


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

    subroutine Get_meridional_wind_3d (this, datetime)

      implicit none

      class (wrf_t), intent (in out) :: this
      type (datetime_t), intent (in) :: datetime

      real, dimension(:, :, :, :), allocatable :: var4d
      integer :: nt, nmass


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'V', var4d)
      nmass = size (var4d, dim = 2)
      this%v3d = 0.5 * (var4d(:, 1:nmass - 1, :, nt) + var4d(:, 2:nmass, :, nt))
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
      nmass = size (var4d, dim = 1)
      this%u3d = 0.5 * (var4d(1:nmass - 1, :, :, nt) + var4d(2:nmass, :, :, nt))
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

    function Wrf_t_const (file_name, config_flags) result (return_value)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, ERROR_UNIT, REAL32, INT32

      implicit none

      character (len = *), intent (in) :: file_name
      type (namelist_t), intent (in) :: config_flags
      type (wrf_t) :: return_value

      logical, parameter :: DEBUG_LOCAL = .true.
      real, parameter :: DEFAULT_Z0 = 0.1, DEFAULT_MUT = 0.0, DEFAULT_ZSF = 0.0, DEFAULT_DZDXF = 0.0, &
          DEFAULT_DZDYF = 0.0
        ! Atm vars needed by the fuel moisture model
      real, parameter :: DEFAULT_T2 = 123.4, DEFAULT_Q2 = 0.0, DEFAULT_PSFC = 0.0, DEFAULT_RAINC = 0.0, &
          DEFAULT_RAINNC = 0.0

      integer, parameter :: N_POINTS_IN_HALO = 0
      real (kind = REAL32) :: att_real32
      integer (kind = INT32) :: att_int32


      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'Entering wrf_t constructor'
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

      return_value%ids = 1
      return_value%jds = 1
      call Get_netcdf_dim (trim (file_name), 'west_east_stag', att_int32)
      return_value%ide = att_int32
      call Get_netcdf_dim (trim (file_name), 'south_north_stag', att_int32)
      return_value%jde = att_int32

      return_value%kds = config_flags%kds
      return_value%kde = config_flags%kde

      return_value%ims = return_value%ids - N_POINTS_IN_HALO
      return_value%ime = return_value%ide + N_POINTS_IN_HALO
      return_value%kms = return_value%kds
      return_value%kme = return_value%kde
      return_value%jms = return_value%jds - N_POINTS_IN_HALO
      return_value%jme = return_value%jde + N_POINTS_IN_HALO

      return_value%ips = return_value%ids
      return_value%ipe = return_value%ide
      return_value%kps = return_value%kds
      return_value%kpe = return_value%kde
      return_value%jps = return_value%jds
      return_value%jpe = return_value%jde

      return_value%its = return_value%ids
      return_value%ite = return_value%ide
      return_value%kts = return_value%kds
      return_value%kte = return_value%kde
      return_value%jts = return_value%jds
      return_value%jte = return_value%jde

      call return_value%Print_domain()

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
      allocate (return_value%ua(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      return_value%ua = 0.0
      allocate (return_value%va(return_value%ims:return_value%ime, return_value%jms:return_value%jme))
      return_value%va = 0.0

      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'Leaving wrf_t constructor'

    end function Wrf_t_const

    subroutine Get_latcloncs (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      type (proj_lc_t) :: proj
      logical, parameter :: OUTPUT_LATLON_CHECK = .false.
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

    subroutine Interp_var2grid_nearest (this, lats_in, lons_in, var_name, vals_out)

      use, intrinsic :: iso_fortran_env, only : ERROR_UNIT

      implicit none

      class (wrf_t), intent(in) :: this
      real, dimension(:, :), intent(in) :: lats_in, lons_in
      character (len = *), intent (in) :: var_name
      real, dimension(:, :), allocatable, intent(out) :: vals_out

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
        case ('t2')
          var_wrf = this%t2_stag(this%ids:this%ide - 1, this%jds:this%jde - 1)

        case ('q2')
          var_wrf = this%q2_stag(this%ids:this%ide - 1, this%jds:this%jde - 1)

        case ('psfc')
          var_wrf = this%psfc_stag(this%ids:this%ide - 1, this%jds:this%jde - 1)

        case ('rain')
          var_wrf = this%rainnc_stag(this%ids:this%ide - 1, this%jds:this%jde - 1) &
                   + this%rainc_stag(this%ids:this%ide - 1, this%jds:this%jde - 1)
        case ('fz0')
          var_wrf = this%z0_stag(this%ids:this%ide - 1, this%jds:this%jde - 1)

        case ('uf')
          var_wrf = this%ua(this%ids:this%ide - 1, this%jds:this%jde - 1)

        case ('vf')
          var_wrf = this%va(this%ids:this%ide - 1, this%jds:this%jde - 1)

        case default
          write (ERROR_UNIT, *) 'Unknown variable name to interpolate'
          stop
      end select

        ! Algorithm
      do j = 1, ny
        do i = 1, nx
          call proj%Calc_ij (lats_in(i, j), lons_in(i, j), i_real, j_real)
          i_wrf = min (max (1, nint (i_real)), nx_wrf)
          j_wrf = min (max (1, nint (j_real)), ny_wrf)
          d = (this%lats(i_wrf, j_wrf) - lats_in(i, j)) ** 2 + (this%lons(i_wrf, j_wrf) - lons_in(i, j)) ** 2
          if (d < ds(i, j)) then
            vals_out(i, j) = var_wrf(i_wrf, j_wrf)
            ds(i, j) = d
          end if
        end do
      end do

    end subroutine Interp_var2grid_nearest

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

    subroutine Update_atm_state (this, datetime_now)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (wrf_t), intent(in out) :: this
      type (datetime_t), intent (in) :: datetime_now

      logical, parameter :: DEBUG_LOCAL = .true.
      integer :: i, j, k


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
        call this%Destroy_ph ()

    end subroutine Update_atm_state

  end module wrf_mod

