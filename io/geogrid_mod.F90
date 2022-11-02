  module geogrid_mod

    use netcdf_mod, only : Get_netcdf_var, Get_netcdf_att, Get_netcdf_dim

    implicit none

    private

    public :: geogrid_t

    type :: geogrid_t
      real, dimension (:, :), allocatable :: lats, lons, elevations, dz_dxs, dz_dys, fuel_cats
      real :: dx, dy, cen_lat, cen_lon
      integer :: ids = 1, jds = 1, ide, jde, sr_x, sr_y
        ! Atm vars
      real, dimension(:, :), allocatable :: xlat, xlong
    end type geogrid_t

    interface geogrid_t
      module procedure geogrid_t_const
    end interface geogrid_t

  contains

    function Geogrid_t_const (file_name) result (return_value)

      use, intrinsic :: iso_fortran_env, only : REAL32, INT32

      implicit none

      character (len = *), intent (in) :: file_name
      type (geogrid_t) :: return_value


      real (kind = REAL32), dimension(:, :, :), allocatable :: var_real32
      real (kind = REAL32) :: att_real32
      integer (kind = INT32) :: att_int32


        ! State vars
      call Get_netcdf_var (trim (file_name), 'ZSF', var_real32)
      return_value%elevations = var_real32(:, :, 1)
      deallocate (var_real32)

      call Get_netcdf_var (trim (file_name), 'DZDXF', var_real32)
      return_value%dz_dxs = var_real32(:, :, 1)
      deallocate (var_real32)

      call Get_netcdf_var (trim (file_name), 'DZDYF', var_real32)
      return_value%dz_dys = var_real32(:, :, 1)
      deallocate (var_real32)

      call Get_netcdf_var (trim (file_name), 'NFUEL_CAT', var_real32)
      return_value%fuel_cats = var_real32(:, :, 1)
      deallocate (var_real32)

      call Get_netcdf_dim (trim (file_name), 'south_north_stag', att_int32)
      return_value%jde = att_int32

      call Get_netcdf_dim (trim (file_name), 'west_east_stag', att_int32)
      return_value%ide = att_int32

      call Get_netcdf_att (trim (file_name), 'global', 'DX', att_real32)
      return_value%dx = att_real32

      call Get_netcdf_att (trim (file_name), 'global', 'DY', att_real32)
      return_value%dy = att_real32

        ! Other vars
      call Get_netcdf_att (trim (file_name), 'global', 'CEN_LAT', att_real32)
      return_value%cen_lat = att_real32

      call Get_netcdf_att (trim (file_name), 'global', 'CEN_LON', att_real32)
      return_value%cen_lon = att_real32

      call Get_netcdf_var (trim (file_name), 'XLAT_M', var_real32)
      return_value%xlat = var_real32(:, :, 1)
      deallocate (var_real32)

      call Get_netcdf_var (trim (file_name), 'XLONG_M', var_real32)
      return_value%xlong = var_real32(:, :, 1)
      deallocate (var_real32)

      call Get_netcdf_att (trim (file_name), 'global', 'sr_x', att_int32)
      return_value%sr_x = att_int32

      call Get_netcdf_att (trim (file_name), 'global', 'sr_y', att_int32)
      return_value%sr_y = att_int32

    end function geogrid_t_const

  end module geogrid_mod

