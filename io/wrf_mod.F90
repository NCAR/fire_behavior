  module wrf_mod

    use netcdf_mod, only : Get_netcdf_var, Get_netcdf_att
    use datetime_mod, only : datetime_t
    use proj_lc_mod, only : proj_lc_t

    implicit none

    private

    public :: wrf_t

    type :: wrf_t
      character (len = 300) :: file_name
      real, dimension(:, :), allocatable :: lats, lons, lats_c, lons_c, t2
    contains
      procedure, public :: Get_t2 => Get_temperature_2m
      procedure, public :: Get_datetime_index => Get_datetime_index
      procedure, public :: Get_latlons => Get_latlons
      procedure, public :: Get_latcloncs => Get_latcloncs
    end type wrf_t

    interface wrf_t
      module procedure Wrf_t_const
    end interface wrf_t

  contains

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

    function Wrf_t_const (file_name) result (return_value)

      implicit none

      character (len = *), intent (in) :: file_name
      type (wrf_t) :: return_value

      return_value%file_name = trim (file_name)
      ! Centers
      call return_value%Get_latlons ()
      ! Corners
      call return_value%Get_latcloncs ()
      
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

