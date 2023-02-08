  module wrf_mod

    use netcdf_mod, only : Get_netcdf_var, Get_netcdf_att
    use datetime_mod, only : datetime_t
    use proj_lc_mod, only : proj_lc_t
    use namelist_mod, only : namelist_t
    use geogrid_mod, only : geogrid_t

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
      real, dimension(:, :), allocatable :: lats, lons, lats_c, lons_c, t2, q2, z0, mut, psfc, rain, rainc, rainnc
      real, dimension(:, :), allocatable :: t2_stag, q2_stag, z0_stag, mut_stag, psfc_stag, rainc_stag, rainnc_stag
      real, dimension(:, :, :), allocatable :: u3d, v3d, phb, ph, phl, pres, dz8w, z_at_w, rho
      real, dimension(:, :, :), allocatable :: u3d_stag, v3d_stag, phb_stag, ph_stag, dz8w_stag, z_at_w_stag, rho_stag
      integer :: bottom_top, bottom_top_stag
      integer :: ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe, &
                 its, ite, jts, jte, kts, kte
      integer :: num_tiles
      integer, dimension (:), allocatable :: i_start, i_end, j_start, j_end
    contains
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
      procedure, public :: Get_datetime_index => Get_datetime_index
      procedure, public :: Get_dz8w => Get_distance_between_vertical_layers
      procedure, public :: Get_latlons => Get_latlons
      procedure, public :: Get_latcloncs => Get_latcloncs
      procedure, public :: Get_mut => Get_mut
      procedure, public :: Get_phl => Get_geopotential_levels
      procedure, public :: Get_pres => Get_pressure_levels
      procedure, public :: Get_ph_stag => Get_geopotential_stag_3d
      procedure, public :: Get_phb_stag => Get_geopotential_base_stag_3d
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
      procedure, public :: Interpolate_z2fire => Interpolate_z2fire
      procedure, public :: Load_atmosphere_test1 => Load_atmosphere_test1
      procedure, public :: Read_wrf_input => Read_wrf_input
      procedure, public :: Update_atm_state => Update_atm_state
    end type wrf_t

    interface wrf_t
      module procedure Wrf_t_const
    end interface wrf_t

  contains

    subroutine Continue_at_boundary(ix,iy,bias, & ! do x direction or y direction
          ims,ime,jms,jme, &                ! memory dims
          ids,ide,jds,jde, &                ! domain dims
          ips,ipe,jps,jpe, &                ! patch dims
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
                             ips,ipe,jps,jpe, &  ! patch dims
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
      if(its.eq.ips.and..not.its.eq.ids)its1=its-halo
      if(jts.eq.jps.and..not.jts.eq.jds)jts1=jts-halo
      if(ite.eq.ipe.and..not.ite.eq.ide)ite1=ite+halo
      if(jte.eq.jpe.and..not.jte.eq.jde)jte1=jte+halo
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
      real, dimension(:, :, :), allocatable :: temp
      integer :: nt, nlevels


      nt = this%Get_datetime_index (datetime)
      call Get_netcdf_var (trim (this%file_name), 'PH', var4d)
      call Get_netcdf_var (trim (this%file_name), 'PHB', var4d2)
      temp = var4d(:, :, :, nt) + var4d(:, :, :, nt)
      deallocate (var4d, var4d2)

      nlevels = size (temp, dim = 3)
      this%phl = (temp(:, :, 2:nlevels) + temp(:, :, 1:nlevels - 1)) / 2.0
      deallocate (temp)

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

      use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
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
          ! Centers
        call return_value%Get_latlons ()
          ! Corners
        call return_value%Get_latcloncs ()

        call Get_netcdf_att (trim (return_value%file_name), 'global', 'BOTTOM-TOP_PATCH_END_UNSTAG', return_value%bottom_top)
        call Get_netcdf_att (trim (return_value%file_name), 'global', 'BOTTOM-TOP_PATCH_END_STAG', return_value%bottom_top_stag)
      end if


      if_use_config_flags: if (use_config_flags) then
        ! Domain dimensions
      if (use_geogrid) then
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
      else
        return_value%ids = config_flags%ids
        return_value%ide = config_flags%ide
        return_value%jds = config_flags%jds
        return_value%jde = config_flags%jde
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

!      allocate (return_value%c1h(return_value%kms:return_value%kme))
!      return_value%c1h = DEFAULT_C1H
!      allocate (return_value%c2h(return_value%kms:return_value%kme))
!      return_value%c2h = DEFAULT_C2H
!
!      allocate (return_value%rthfrten(return_value%ims:return_value%ime, return_value%kms:return_value%kme, return_value%jms:return_value%jme))
!      allocate (return_value%rqvfrten(return_value%ims:return_value%ime, return_value%kms:return_value%kme, return_value%jms:return_value%jme))

        ! Grid dimensions
!      if_geogrid: if (use_geogrid) then
!        if (geogrid%dx == config_flags%dx) then
!          return_value%dx = geogrid%dx
!        else
!          write (ERROR_UNIT, *) 'dx in namelist and in geogrid differ'
!          stop
!        end if
!        if (geogrid%dy == config_flags%dy) then
!          return_value%dy = geogrid%dy
!        else
!          write (ERROR_UNIT, *) 'dy in namelist and in geogrid differ'
!          stop
!        end if
!      else
        ! we need to initialize nx, ny here
!        return_value%dx = config_flags%dx
!        return_value%dy = config_flags%dy

!        return_value%xlat = 0.0
!        return_value%xlat(return_value%ids:return_value%ide - 1, return_value%jds:return_value%jde - 1) = geogrid%xlat
!        return_value%xlong = 0.0
!        return_value%xlong(return_value%ids:return_value%ide - 1, return_value%jds:return_value%jde - 1) = geogrid%xlong
!      end if if_geogrid
!      return_value%dt = config_flags%dt
    end if if_use_config_flags

    end function Wrf_t_const

    subroutine Get_latcloncs (this)

      use, intrinsic :: iso_fortran_env, only : REAL32
      implicit none

      class (wrf_t), intent (in out) :: this
      type (proj_lc_t) :: proj

      real (kind = REAL32) :: att_real32
      real :: cen_lat, cen_lon, truelat1, truelat2, stand_lon, dx, dy
      integer :: nx, ny, i, j

      call Get_netcdf_att (trim (this%file_name), 'global', 'CEN_LAT', att_real32)
      cen_lat = att_real32

      call Get_netcdf_att (trim (this%file_name), 'global', 'CEN_LON', att_real32)
      cen_lon = att_real32

      call Get_netcdf_att (trim (this%file_name), 'global', 'TRUELAT1', att_real32)
      truelat1 = att_real32

      call Get_netcdf_att (trim (this%file_name), 'global', 'TRUELAT2', att_real32)
      truelat2 = att_real32

      call Get_netcdf_att (trim (this%file_name), 'global', 'STAND_LON', att_real32)
      stand_lon = att_real32

      call Get_netcdf_att (trim (this%file_name), 'global', 'DX', att_real32)
      dx = att_real32

      call Get_netcdf_att (trim (this%file_name), 'global', 'DY', att_real32)
      dy = att_real32

      nx = size (this%lats, dim = 1) + 1
      ny = size (this%lats, dim = 2) + 1

      allocate (this%lats_c(nx, ny))
      allocate (this%lons_c(nx, ny))

      proj = proj_lc_t (cen_lat = cen_lat , cen_lon = cen_lon, dx = dx, dy = dy, &
          standard_lon = stand_lon , true_lat_1 = truelat1 , true_lat_2 = truelat2 , &
          nx = nx, ny = ny)

      do j = 1, ny
        do i = 1, nx
          call proj%Calc_latlon (i = real (i), j = real (j), lat = this%lats_c(i, j), lon = this%lons_c(i, j))
        end do
      end do

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
      this%ips,this%ipe,this%jps,this%jpe, &            ! patch dims - winds defined up to +1
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

