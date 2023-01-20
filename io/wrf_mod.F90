  module wrf_mod

    use netcdf_mod, only : Get_netcdf_var, Get_netcdf_att
    use datetime_mod, only : datetime_t
    use proj_lc_mod, only : proj_lc_t

    implicit none

    private

    public :: wrf_t

    real, parameter :: G = 9.81  ! acceleration due to gravity [m s-2]

    type :: wrf_t
      character (len = 300) :: file_name
      real, dimension(:, :), allocatable :: lats, lons, lats_c, lons_c, t2, q2, z0, mut, psfc, rainc, rainnc
      real, dimension(:, :, :), allocatable :: u3d, v3d, phb, ph, dz8w, z_at_w, rho
      integer :: bottom_top, bottom_top_stag
    contains
      procedure, public :: Destroy_dz8w => Destroy_distance_between_vertical_layers
      procedure, public :: Destroy_mut => Destroy_mut
      procedure, public :: Destroy_ph => Destroy_geopotential
      procedure, public :: Destroy_phb => Destroy_geopotential_base
      procedure, public :: Destroy_psfc => Destroy_surface_pressure
      procedure, public :: Destroy_q2 => Destroy_specific_humidity_2m
      procedure, public :: Destroy_rho => Destroy_rho
      procedure, public :: Destroy_rainc => Destroy_rain_convective
      procedure, public :: Destroy_rainnc => Destroy_rain_non_convective
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
      procedure, public :: Get_ph_stag => Get_geopotential_stag_3d
      procedure, public :: Get_phb_stag => Get_geopotential_base_stag_3d
      procedure, public :: Get_q2 => Get_specific_humidity_2m
      procedure, public :: Get_rainc => Get_rain_convective
      procedure, public :: Get_rainnc => Get_rain_non_convective
      procedure, public :: Get_rho => Get_rho
      procedure, public :: Get_psfc => Get_surface_pressure
      procedure, public :: Get_t2 => Get_temperature_2m
      procedure, public :: Get_u3d => Get_zonal_wind_3d
      procedure, public :: Get_u3d_stag => Get_zonal_wind_stag_3d
      procedure, public :: Get_v3d => Get_meridional_wind_3d
      procedure, public :: Get_v3d_stag => Get_meridional_wind_stag_3d
      procedure, public :: Get_z0 => Get_z0
      procedure, public :: Get_z_at_w => Get_height_agl_at_walls
    end type wrf_t

    interface wrf_t
      module procedure Wrf_t_const
    end interface wrf_t

  contains

    subroutine Destroy_distance_between_vertical_layers (this)

      implicit none

      class (wrf_t), intent (in out) :: this

      if (allocated(this%dz8w)) deallocate (this%dz8w)

    end subroutine Destroy_distance_between_vertical_layers

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

    function Wrf_t_const (file_name) result (return_value)

      implicit none

      character (len = *), intent (in) :: file_name
      type (wrf_t) :: return_value

      return_value%file_name = trim (file_name)
      ! Centers
      call return_value%Get_latlons ()
      ! Corners
      call return_value%Get_latcloncs ()

      call Get_netcdf_att (trim (return_value%file_name), 'global', 'BOTTOM-TOP_PATCH_END_UNSTAG', return_value%bottom_top)
      call Get_netcdf_att (trim (return_value%file_name), 'global', 'BOTTOM-TOP_PATCH_END_STAG', return_value%bottom_top_stag)
      
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

  end module wrf_mod

