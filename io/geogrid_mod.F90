  module geogrid_mod

    use netcdf_mod, only : Get_netcdf_var, Get_netcdf_att, Get_netcdf_dim
    use state_mod, only : state_t

    implicit none

    private

    public :: Init_grid_from_geogrid

  contains

    subroutine Init_grid_from_geogrid (file_name, state, xlat, xlong, sr_x, sr_y)

      use, intrinsic :: iso_fortran_env, only : REAL32, INT32

      implicit none

      character (len = *), intent (in) :: file_name
      type (state_t), intent (out) :: state
      real, dimension(:, :), allocatable, intent (out) :: xlat, xlong
      integer, intent (out) :: sr_x, sr_y


      real (kind = REAL32), dimension(:, :, :), allocatable :: var_real32
      real (kind = REAL32) :: att_real32
      integer (kind = INT32) :: att_int32


        ! Fil in the state vars
      call Get_netcdf_var (trim (file_name), 'ZSF', var_real32)
      state%elevations = var_real32(:, :, 1)
      deallocate (var_real32)

      call Get_netcdf_var (trim (file_name), 'DZDXF', var_real32)
      state%dz_dxs = var_real32(:, :, 1)
      deallocate (var_real32)

      call Get_netcdf_var (trim (file_name), 'DZDYF', var_real32)
      state%dz_dys = var_real32(:, :, 1)
      deallocate (var_real32)

      call Get_netcdf_var (trim (file_name), 'NFUEL_CAT', var_real32)
      state%fuel_cats = var_real32(:, :, 1)
      deallocate (var_real32)

      call Get_netcdf_dim (trim (file_name), 'south_north_stag', state%jde)

      call Get_netcdf_dim (trim (file_name), 'west_east_stag', state%ide)

      call Get_netcdf_att (trim (file_name), 'global', 'DX', att_real32)
      state%dx = att_real32

      call Get_netcdf_att (trim (file_name), 'global', 'DY', att_real32)
      state%dy = att_real32

      call Get_netcdf_att (trim (file_name), 'global', 'CEN_LAT', att_real32)
      state%cen_lat = att_real32

      call Get_netcdf_att (trim (file_name), 'global', 'CEN_LON', att_real32)
      state%cen_lon = att_real32


        ! Other vars for WRF-Fire init
      call Get_netcdf_var (trim (file_name), 'XLAT_M', var_real32)
      xlat = var_real32(:, :, 1)
      deallocate (var_real32)

      call Get_netcdf_var (trim (file_name), 'XLONG_M', var_real32)
      xlong = var_real32(:, :, 1)
      deallocate (var_real32)

      call Get_netcdf_att (trim (file_name), 'global', 'sr_x', att_int32)
      sr_x = att_int32

      call Get_netcdf_att (trim (file_name), 'global', 'sr_y', att_int32)
      sr_y = att_int32

    end subroutine Init_grid_from_geogrid

  end module geogrid_mod

