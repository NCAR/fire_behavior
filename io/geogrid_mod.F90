  module geogrid_mod

    use netcdf_mod, only : Get_netcdf_var
    use grid_mod, only : grid_t

    implicit none

    private

    public :: Init_grid_from_geogrid

  contains

    subroutine Init_grid_from_geogrid ()

      use, intrinsic :: iso_fortran_env, only : REAL32

      implicit none

      type (grid_t) :: fire_grid

      real (kind = REAL32), dimension(:, :, :), allocatable :: var_real32


      call Get_netcdf_var ('geo_em.d01.nc', 'ZSF', var_real32)
      fire_grid%elevation = var_real32(:, :, 1)
      deallocate (var_real32)

      call Get_netcdf_var ('geo_em.d01.nc', 'DZDXF', var_real32)
      fire_grid%dz_dx = var_real32(:, :, 1)
      deallocate (var_real32)

      call Get_netcdf_var ('geo_em.d01.nc', 'DZDYF', var_real32)
      fire_grid%dz_dy = var_real32(:, :, 1)
      deallocate (var_real32)

      call Get_netcdf_var ('geo_em.d01.nc', 'NFUEL_CAT', var_real32)
      fire_grid%nfuel_cat = var_real32(:, :, 1)
      deallocate (var_real32)

    end subroutine Init_grid_from_geogrid

  end module geogrid_mod

