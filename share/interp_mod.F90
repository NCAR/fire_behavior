  module interp_mod

    implicit none

    private

    public :: Interp_profile

  contains

    subroutine Interp_profile (fire_lsm_zcoupling, fire_lsm_zcoupling_ref, fire_wind_height, kfds, kfde, &
        uin, vin, z_at_w, z0f, uout, vout)

      implicit none

      real, intent (in) :: fire_wind_height, fire_lsm_zcoupling_ref
      integer, intent (in) :: kfds, kfde
      real, dimension(:), intent (in) :: uin, vin, z_at_w
      real, intent (in) :: z0f
      logical, intent (in) :: fire_lsm_zcoupling
      real, intent (out) :: uout, vout


      real, parameter :: VK_KAPPA = 0.4
      real, dimension (kfds:kfde - 1) :: altw, hgt
      integer :: k, kdmax
      real :: loght, loglast, logz0, logfwh, ht, r_nan, fire_wind_height_local, z0fc, &
          ust_d, wsf, wsf1, uf_temp, vf_temp


        ! max layer to interpolate from, can be less
      kdmax = kfde - 2
      do k = kfds, kdmax + 1
          ! altitude of the bottom w-point
!        altw(k) = phl(k) / G
        altw(k) = z_at_w(k)
      end do

      do k = kfds, kdmax
          ! height of the mass point above the ground
        hgt(k) = 0.5 * (altw(k) + altw(k + 1)) - altw(kfds)
      end do

        ! extrapolate mid-flame height from fire_lsm_zcoupling_ref?
      if (fire_lsm_zcoupling) then
        logfwh = log (fire_lsm_zcoupling_ref)
        fire_wind_height_local = fire_lsm_zcoupling_ref
      else
        logfwh = log (fire_wind_height)
        fire_wind_height_local = fire_wind_height
      end if

        ! interpolate u
      if (fire_wind_height_local > z0f)then
        do k = kfds, kdmax
          ht = hgt(k)
          if (ht >= fire_wind_height_local) then
              ! found layer k this point is in
            loght = log(ht)
            if (k == kfds) then
                ! first layer, log linear interpolation from 0 at zr
              logz0 = log(z0f)
              uout = uin(k) * (logfwh - logz0) / (loght - logz0)
              vout = vin(k) * (logfwh - logz0) / (loght - logz0)
            else
                ! log linear interpolation
              loglast = log (hgt(k - 1))
              uout = uin(k - 1) + (uin(k) - uin(k - 1)) * (logfwh - loglast) / (loght - loglast)
              vout = vin(k - 1) + (vin(k) - vin(k - 1)) * (logfwh - loglast) / (loght - loglast)
            end if
            exit
          end if
          if (k == kdmax) then
              ! last layer, still not high enough
            uout = uin(k)
            vout = vin(k)
          end if
        end do
      else
          ! roughness higher than the fire wind height
        uout = 0.0
        vout = 0.0
      end if

        ! Extrapol wind to target height
      if (fire_lsm_zcoupling) then
        uf_temp = uout
        vf_temp = vout
        wsf = max (sqrt (uf_temp ** 2.0 + vf_temp ** 2.0), 0.1)
        z0fc = z0f
        ust_d = wsf * VK_KAPPA / log(fire_lsm_zcoupling_ref / z0fc)
        wsf1 = (ust_d / VK_KAPPA) * log((fire_wind_height + z0fc) / z0fc)
        uout = wsf1 * uf_temp / wsf
        vout = wsf1 * vf_temp / wsf
      end if

    end subroutine Interp_profile

  end module interp_mod
