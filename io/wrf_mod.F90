  module wrf_mod

    use netcdf_mod, only : Get_netcdf_var
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
      call return_value%Get_latlons ()
      ! Initialize lats_c and lons_c

    end function Wrf_t_const

  end module wrf_mod

