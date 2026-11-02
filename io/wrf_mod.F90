  module wrf_mod

    use constants_mod, only : CP, XLV
    use emis_mod, only : Calc_smoke_aod
    use interp_mod, only : Interp_profile, WIND_HINTERP_NEAREST, WIND_HINTERP_BILINEAR
    use namelist_mod, only : namelist_t
    use proj_lc_mod, only : proj_lc_t
    use stderrout_mod, only : Stop_simulation

    implicit none

    private

    public :: Interp_wrf2dvar_to_cfbm, Interp_wrfwinds_to_cfbm, Interp_wrfstaggered_winds_to_cfbm, &
        Interp_wrfout_staggered_winds_to_cfbm, Provide_atm_feedback

  contains

    subroutine Interp_wrf2dvar_to_cfbm (wrfatm2dvar, ims, ime, jms, jme, ifms, ifme, jfms, jfme, ifps, ifpe, jfps, jfpe, &
        lats_in, lons_in, proj, vals_out)

      implicit none

      integer, intent (in) :: ims, ime, jms, jme, &
                              ifms, ifme, jfms, jfme, &
                              ifps, ifpe, jfps, jfpe
      real, dimension(ims:ime, jms:jme), intent (in) :: wrfatm2dvar
      real, dimension(ifms:ifme, jfms:jfme), intent (in) :: lats_in, lons_in
      type (proj_lc_t), intent (in) :: proj
      real, dimension(ifms:ifme, jfms:jfme), intent (in out) :: vals_out

      integer :: i, j, i_wrf, j_wrf
      real :: i_real, j_real


      do j = jfps, jfpe
        do i = ifps, ifpe
          call proj%Calc_ij (lats_in(i, j), lons_in(i, j), i_real, j_real)
          i_wrf = min (max (ims, nint (i_real)), ime)
          j_wrf = min (max (jms, nint (j_real)), jme)
          vals_out(i, j) = wrfatm2dvar(i_wrf, j_wrf)
        end do
      end do

    end subroutine Interp_wrf2dvar_to_cfbm

    subroutine Interp_wrfwinds_to_cfbm (u_phy, v_phy, z_at_w, ims, ime, kms, kme, jms, jme, ifms, ifme, jfms, jfme, ifps, ifpe, jfps, jfpe, &
        kfds, kfde, lats_in, lons_in, proj, z0f, fire_lsm_zcoupling, fire_lsm_zcoupling_ref, fire_wind_height, wind_hinterp_opt, &
        u_out, v_out)

      implicit none

      integer, intent (in) :: ims, ime, kms, kme, jms, jme, &
                              ifms, ifme, jfms, jfme, &
                              ifps, ifpe, jfps, jfpe, &
                              kfds, kfde, wind_hinterp_opt
      logical, intent (in) :: fire_lsm_zcoupling
      real, dimension(ims:ime, kms:kme, jms:jme), intent (in) :: u_phy, v_phy, z_at_w
      real, dimension(ifms:ifme, jfms:jfme), intent (in) :: lats_in, lons_in, z0f
      type (proj_lc_t), intent (in) :: proj
      real, intent (in) :: fire_wind_height, fire_lsm_zcoupling_ref
      real, dimension(ifms:ifme, jfms:jfme), intent (in out) :: u_out, v_out

      integer :: i, j, i0, i1, j0, j1
      real :: i_real, j_real, di, dj, u00, v00, u10, v10, u01, v01, u11, v11


      do j = jfps, jfpe
        do i = ifps, ifpe
          call proj%Calc_ij (lats_in(i, j), lons_in(i, j), i_real, j_real)
            ! Use mass-grid winds
            ! in WRF, winds have been destaggered them into u_phy/v_phy,
            ! or forcing source only provides mass-grid profiles. 
            ! The vertical log interpolation is done before horizontal averaging 
            ! so atmospheric columns keep their terrain-following height profile.
          select case (wind_hinterp_opt)
            case (WIND_HINTERP_NEAREST)
              i0 = min (max (ims, nint (i_real)), ime)
              j0 = min (max (jms, nint (j_real)), jme)
              call Interp_profile (fire_lsm_zcoupling, fire_lsm_zcoupling_ref, fire_wind_height, kfds, kfde, &
                  u_phy(i0, :, j0), v_phy(i0, :, j0), z_at_w(i0, :, j0), z0f(i, j), u_out(i, j), v_out(i, j))

            case (WIND_HINTERP_BILINEAR)
              i0 = max (ims, min (ime - 1, int (floor (i_real))))
              j0 = max (jms, min (jme - 1, int (floor (j_real))))
              i1 = i0 + 1
              j1 = j0 + 1
              di = max (0.0, min (1.0, i_real - real (i0)))
              dj = max (0.0, min (1.0, j_real - real (j0)))
              call Interp_profile (fire_lsm_zcoupling, fire_lsm_zcoupling_ref, fire_wind_height, kfds, kfde, &
                  u_phy(i0, :, j0), v_phy(i0, :, j0), z_at_w(i0, :, j0), z0f(i, j), u00, v00)
              call Interp_profile (fire_lsm_zcoupling, fire_lsm_zcoupling_ref, fire_wind_height, kfds, kfde, &
                  u_phy(i1, :, j0), v_phy(i1, :, j0), z_at_w(i1, :, j0), z0f(i, j), u10, v10)
              call Interp_profile (fire_lsm_zcoupling, fire_lsm_zcoupling_ref, fire_wind_height, kfds, kfde, &
                  u_phy(i0, :, j1), v_phy(i0, :, j1), z_at_w(i0, :, j1), z0f(i, j), u01, v01)
              call Interp_profile (fire_lsm_zcoupling, fire_lsm_zcoupling_ref, fire_wind_height, kfds, kfde, &
                  u_phy(i1, :, j1), v_phy(i1, :, j1), z_at_w(i1, :, j1), z0f(i, j), u11, v11)
              u_out(i, j) = (1.0 - di) * (1.0 - dj) * u00 + di * (1.0 - dj) * u10 + &
                  (1.0 - di) * dj * u01 + di * dj * u11
              v_out(i, j) = (1.0 - di) * (1.0 - dj) * v00 + di * (1.0 - dj) * v10 + &
                  (1.0 - di) * dj * v01 + di * dj * v11

            case default
              call Stop_simulation ('The horizontal wind interpolation option selected does not exist for mass-grid winds')
          end select
        end do
      end do

    end subroutine  Interp_wrfwinds_to_cfbm

    subroutine Interp_wrfstaggered_winds_to_cfbm (u_stag, v_stag, ph, phb, z0, ims, ime, kms, kme, jms, jme, &
        ifms, ifme, jfms, jfme, ifps, ifpe, jfps, jfpe, lats_in, lons_in, proj, z0f, fire_lsm_zcoupling, &
        fire_lsm_zcoupling_ref, fire_wind_height, u_out, v_out)

      implicit none

      integer, intent (in) :: ims, ime, kms, kme, jms, jme, ifms, ifme, jfms, jfme, ifps, ifpe, jfps, jfpe
      logical, intent (in) :: fire_lsm_zcoupling
      real, dimension(ims:ime, kms:kme, jms:jme), intent (in) :: u_stag, v_stag, ph, phb
      real, dimension(ims:ime, jms:jme), intent (in) :: z0
      real, dimension(ifms:ifme, jfms:jfme), intent (in) :: lats_in, lons_in, z0f
      type (proj_lc_t), intent (in) :: proj
      real, intent (in) :: fire_wind_height, fire_lsm_zcoupling_ref
      real, dimension(ifms:ifme, jfms:jfme), intent (in out) :: u_out, v_out

      real, dimension(ims:ime, jms:jme) :: ua, va
      real, dimension(kms:kme) :: z_profile
      integer :: i, j, k
      real :: i_real, j_real


      ua = 0.0
      va = 0.0

        ! WRF in-memory arrays in the coupled path use (i,k,j). U is staggered
        ! in i and V is staggered in j. Geopotential is mass-grid, so U-point
        ! and V-point height profiles are reconstructed by averaging the two
        ! adjacent mass-grid geopotential columns in the staggered direction.
      do j = jms, jme
        do i = ims + 1, ime
          do k = kms, kme
            z_profile(k) = 0.5 * (ph(i - 1, k, j) + phb(i - 1, k, j) + ph(i, k, j) + phb(i, k, j)) / 9.81
          end do
          call Interp_staggered_profile (u_stag(i, :, j), z_profile, 0.5 * (z0(i - 1, j) + z0(i, j)), &
              kms, kme, fire_lsm_zcoupling, fire_lsm_zcoupling_ref, fire_wind_height, ua(i, j))
        end do
        ua(ims, j) = ua(ims + 1, j)
      end do

      do j = jms + 1, jme
        do i = ims, ime
          do k = kms, kme
            z_profile(k) = 0.5 * (ph(i, k, j - 1) + phb(i, k, j - 1) + ph(i, k, j) + phb(i, k, j)) / 9.81
          end do
          call Interp_staggered_profile (v_stag(i, :, j), z_profile, 0.5 * (z0(i, j - 1) + z0(i, j)), &
              kms, kme, fire_lsm_zcoupling, fire_lsm_zcoupling_ref, fire_wind_height, va(i, j))
        end do
      end do
      va(:, jms) = va(:, jms + 1)

        ! Fire grid coordinates are evaluated in the mass-grid projection.
        ! The +0.5 offsets map the mass-grid cell center to the native U or V
        ! face locations before bilinear interpolation to the fire grid.
      do j = jfps, jfpe
        do i = ifps, ifpe
          call proj%Calc_ij (lats_in(i, j), lons_in(i, j), i_real, j_real)
          u_out(i, j) = Interp_2d_at_index (ua, ims, ime, jms, jme, i_real + 0.5, j_real)
          v_out(i, j) = Interp_2d_at_index (va, ims, ime, jms, jme, i_real, j_real + 0.5)
        end do
      end do

      call Apply_zcoupling_to_fire_grid (fire_lsm_zcoupling, fire_lsm_zcoupling_ref, fire_wind_height, &
          ifms, ifme, jfms, jfme, ifps, ifpe, jfps, jfpe, z0f, u_out, v_out)

    end subroutine Interp_wrfstaggered_winds_to_cfbm

    subroutine Interp_wrfout_staggered_winds_to_cfbm (u_stag, v_stag, phl, z0, ifms, ifme, jfms, jfme, ifps, ifpe, &
        jfps, jfpe, lats_in, lons_in, proj, z0f, fire_lsm_zcoupling, fire_lsm_zcoupling_ref, fire_wind_height, u_out, v_out)

      implicit none

      integer, intent (in) :: ifms, ifme, jfms, jfme, ifps, ifpe, jfps, jfpe
      logical, intent (in) :: fire_lsm_zcoupling
      real, dimension(:, :, :), intent (in) :: u_stag, v_stag, phl
      real, dimension(:, :), intent (in) :: z0
      real, dimension(ifms:ifme, jfms:jfme), intent (in) :: lats_in, lons_in, z0f
      type (proj_lc_t), intent (in) :: proj
      real, intent (in) :: fire_wind_height, fire_lsm_zcoupling_ref
      real, dimension(ifms:ifme, jfms:jfme), intent (in out) :: u_out, v_out

      real, dimension(size(u_stag, dim = 1), size(u_stag, dim = 2)) :: ua
      real, dimension(size(v_stag, dim = 1), size(v_stag, dim = 2)) :: va
      real, dimension(size(phl, dim = 3)) :: z_profile
      integer :: i, j, k, nx_mass, ny_mass, nz_stag
      real :: i_real, j_real


      nx_mass = size (z0, dim = 1)
      ny_mass = size (z0, dim = 2)
      nz_stag = size (phl, dim = 3)
        ! Offline wrfout reads retain NetCDF array order: U is
        ! (west_east_stag, south_north, bottom_top), V is
        ! (west_east, south_north_stag, bottom_top), and PH/PHB/ZNT are on
        ! the mass grid. Check these relationships explicitly before using
        ! C-grid indexing so non-WRF or pre-destaggered inputs fail clearly.
      if (size (u_stag, dim = 1) /= nx_mass + 1 .or. size (u_stag, dim = 2) /= ny_mass .or. &
          size (v_stag, dim = 1) /= nx_mass .or. size (v_stag, dim = 2) /= ny_mass + 1 .or. &
          size (phl, dim = 1) /= nx_mass .or. size (phl, dim = 2) /= ny_mass) &
          call Stop_simulation ('wind_hinterp_opt=3 requires WRF-staggered U/V and mass-grid PH/ZNT dimensions')
      ua = 0.0
      va = 0.0

        ! Interior U and V faces can be reconstructed from adjacent mass-grid
        ! geopotential and roughness. Boundary faces are extended from the
        ! nearest interior face, matching the practical treatment used by the
        ! WRF-Fire interpolation away from domain boundaries.
      do j = 1, ny_mass
        do i = 2, nx_mass
          do k = 1, nz_stag
            z_profile(k) = 0.5 * (phl(i - 1, j, k) + phl(i, j, k)) / 9.81
          end do
          call Interp_staggered_profile (u_stag(i, j, :), z_profile, 0.5 * (z0(i - 1, j) + z0(i, j)), &
              1, nz_stag, fire_lsm_zcoupling, fire_lsm_zcoupling_ref, fire_wind_height, ua(i, j))
        end do
        ua(1, j) = ua(2, j)
        ua(nx_mass + 1, j) = ua(nx_mass, j)
      end do

      do j = 2, ny_mass
        do i = 1, nx_mass
          do k = 1, nz_stag
            z_profile(k) = 0.5 * (phl(i, j - 1, k) + phl(i, j, k)) / 9.81
          end do
          call Interp_staggered_profile (v_stag(i, j, :), z_profile, 0.5 * (z0(i, j - 1) + z0(i, j)), &
              1, nz_stag, fire_lsm_zcoupling, fire_lsm_zcoupling_ref, fire_wind_height, va(i, j))
        end do
      end do
      va(:, 1) = va(:, 2)
      va(:, ny_mass + 1) = va(:, ny_mass)

      do j = jfps, jfpe
        do i = ifps, ifpe
          call proj%Calc_ij (lats_in(i, j), lons_in(i, j), i_real, j_real)
          u_out(i, j) = Interp_2d_at_index (ua, 1, size (ua, dim = 1), 1, size (ua, dim = 2), i_real + 0.5, j_real)
          v_out(i, j) = Interp_2d_at_index (va, 1, size (va, dim = 1), 1, size (va, dim = 2), i_real, j_real + 0.5)
        end do
      end do

      call Apply_zcoupling_to_fire_grid (fire_lsm_zcoupling, fire_lsm_zcoupling_ref, fire_wind_height, &
          ifms, ifme, jfms, jfme, ifps, ifpe, jfps, jfpe, z0f, u_out, v_out)

    end subroutine Interp_wrfout_staggered_winds_to_cfbm

    subroutine Interp_staggered_profile (wind_profile, z_w_profile, z0, kds, kde, fire_lsm_zcoupling, &
        fire_lsm_zcoupling_ref, fire_wind_height, wind_out)

      implicit none

      integer, intent (in) :: kds, kde
      logical, intent (in) :: fire_lsm_zcoupling
      real, dimension(kds:kde), intent (in) :: wind_profile, z_w_profile
      real, intent (in) :: z0, fire_lsm_zcoupling_ref, fire_wind_height
      real, intent (out) :: wind_out

      integer :: k, kdmax
      real :: hgt, hgt_last, logfwh, wind_height


      kdmax = kde - 1
        ! Interpolate to the reference height first when z-coupling is active.
        ! The vector wind is then reduced from that reference height to
        ! fire_wind_height after horizontal interpolation on the fire grid.
      if (fire_lsm_zcoupling) then
        wind_height = fire_lsm_zcoupling_ref
      else
        wind_height = fire_wind_height
      end if

      if (wind_height <= z0) then
        wind_out = 0.0
        return
      end if

      logfwh = log (wind_height)
      wind_out = wind_profile(kdmax)
      do k = kds, kdmax
        hgt = 0.5 * (z_w_profile(k) + z_w_profile(k + 1)) - z_w_profile(kds)
        if (hgt >= wind_height) then
          if (k == kds) then
            wind_out = wind_profile(k) * (logfwh - log (z0)) / (log (hgt) - log (z0))
          else
            hgt_last = 0.5 * (z_w_profile(k - 1) + z_w_profile(k)) - z_w_profile(kds)
            wind_out = wind_profile(k - 1) + (wind_profile(k) - wind_profile(k - 1)) * &
                (logfwh - log (hgt_last)) / (log (hgt) - log (hgt_last))
          end if
          exit
        end if
      end do

    end subroutine Interp_staggered_profile

    real function Interp_2d_at_index (data_in, ims, ime, jms, jme, i_real, j_real) result (value)

      implicit none

      integer, intent (in) :: ims, ime, jms, jme
      real, dimension(ims:ime, jms:jme), intent (in) :: data_in
      real, intent (in) :: i_real, j_real

      integer :: i0, i1, j0, j1
      real :: di, dj


      i0 = max (ims, min (ime - 1, int (floor (i_real))))
      j0 = max (jms, min (jme - 1, int (floor (j_real))))
      i1 = i0 + 1
      j1 = j0 + 1

      di = max (0.0, min (1.0, i_real - real (i0)))
      dj = max (0.0, min (1.0, j_real - real (j0)))

      value = (1.0 - di) * (1.0 - dj) * data_in(i0, j0) + &
          di * (1.0 - dj) * data_in(i1, j0) + &
          (1.0 - di) * dj * data_in(i0, j1) + &
          di * dj * data_in(i1, j1)

    end function Interp_2d_at_index

    subroutine Apply_zcoupling_to_fire_grid (fire_lsm_zcoupling, fire_lsm_zcoupling_ref, fire_wind_height, &
        ifms, ifme, jfms, jfme, ifps, ifpe, jfps, jfpe, z0f, u_out, v_out)

      implicit none

      logical, intent (in) :: fire_lsm_zcoupling
      integer, intent (in) :: ifms, ifme, jfms, jfme, ifps, ifpe, jfps, jfpe
      real, intent (in) :: fire_lsm_zcoupling_ref, fire_wind_height
      real, dimension(ifms:ifme, jfms:jfme), intent (in) :: z0f
      real, dimension(ifms:ifme, jfms:jfme), intent (in out) :: u_out, v_out

      real, parameter :: VK_KAPPA = 0.4
      integer :: i, j
      real :: uf_temp, vf_temp, wsf, ust_d, wsf1


      if (.not. fire_lsm_zcoupling) return

        ! The reference-height wind has already been interpolated to the fire
        ! grid. Apply the same log-profile reduction to the vector magnitude,
        ! then restore the original direction at each fire cell.
      do j = jfps, jfpe
        do i = ifps, ifpe
          uf_temp = u_out(i, j)
          vf_temp = v_out(i, j)
          wsf = max (sqrt (uf_temp ** 2.0 + vf_temp ** 2.0), 0.1)
          ust_d = wsf * VK_KAPPA / log (fire_lsm_zcoupling_ref / z0f(i, j))
          wsf1 = (ust_d / VK_KAPPA) * log ((fire_wind_height + z0f(i, j)) / z0f(i, j))
          u_out(i, j) = wsf1 * uf_temp / wsf
          v_out(i, j) = wsf1 * vf_temp / wsf
        end do
      end do

    end subroutine Apply_zcoupling_to_fire_grid

    subroutine Provide_atm_feedback (config_flags, &
            ifms, ifme, jfms, jfme,                &
            ifts, ifte, jfts, jfte,                &
            ifps, ifpe, jfps, jfpe,                &
            ids, ide, kds, kde, jds, jde,          &
            ims, ime, kms, kme, jms, jme,          &
            its, ite, kts, kte, jts, jte,          &
            sr_x, sr_y,                            &
            emis_smoke, smoke_tracer, tracer_opt,  &
            p_phy, t_phy, qv,                      &
            aod5502d_smoke,                        &
            fgrnhfx, fgrnqfx,                      &
            grnhfx, grnqfx, canhfx, canqfx,        &
            grnsmk,                                &
            alfg, alfc, z1can,                     &
            rho, dz8w, z_at_w,                     &
            mu, c1h, c2h,                          &
            rthfrten, rqvfrten)

      implicit none

      type (namelist_t), intent (in) :: config_flags
      integer, intent (in) :: ifms, ifme, jfms, jfme,       &
                              ifps, ifpe, jfps, jfpe,       &
                              ids, ide, kds, kde, jds, jde, &
                              ims, ime, kms, kme, jms, jme, &
                              its, ite, kts, kte, jts, jte, &
                              ifts, ifte, jfts, jfte, sr_x, sr_y

      real, dimension(ifms:ifme, jfms:jfme), intent (in) :: emis_smoke
      real, dimension(ims:ime, kms:kme, jms:jme), intent (in out), optional :: smoke_tracer
      integer, intent (in) :: tracer_opt
      real, dimension(ifms:ifme, jfms:jfme), intent (in) :: fgrnhfx, fgrnqfx
      real, dimension(ims:ime, kms:kme, jms:jme), intent (in) :: rho, dz8w, z_at_w, p_phy, t_phy, qv
      real, intent(in), dimension(ims:ime, jms:jme) :: mu   ! dry air mass (pa)
      real, intent(in), dimension(kms:kme) :: c1h, c2h      ! hybrid coordinate weights
      real, intent(in) :: alfg                              ! extinction depth surface fire heat (m)
      real, intent(in) :: alfc                              ! extinction depth crown  fire heat (m)
      real, intent(in) :: z1can                             ! height of crown fire heat release (m)
      real, dimension(ims:ime, jms:jme), intent (out) :: grnhfx, grnqfx, canhfx, canqfx, grnsmk, aod5502d_smoke
      real, intent(out), dimension(ims:ime, kms:kme, jms:jme) ::   &
           rthfrten, & ! theta tendency from fire (in mass units)
           rqvfrten    ! Qv tendency from fire (in mass units)

      logical, parameter :: DEBUG_LOCAL = .true.
      integer :: i, j, ibase, jbase, i_f, j_f, ioff, joff
      real :: avgw, convert_kg_m2_to_g_kg


      if (DEBUG_LOCAL) call Check_dims (its, ite, jts, jte, ifts, ifte, jfts, jfte, sr_x, sr_y)

      avgw = 1.0 / (sr_x * sr_y)
      do j = max (jds + 1, jts), min (jte, jde - 2)
        jbase = jfts + sr_y * (j - jts)
        do i = max (ids + 1, its), min (ite, ide - 2)
          ibase = ifts + sr_x * (i - its)
          canqfx(i, j) = 0.0
          canhfx(i, j) = 0.0
          grnsmk(i, j) = 0.0
          grnhfx(i, j) = 0.0
          grnqfx(i, j) = 0.0
          convert_kg_m2_to_g_kg = 1000.0 / (rho(i, kts, j) * dz8w(i, kts, j))
          do joff = 0, sr_y - 1
            j_f = joff + jbase
            do ioff = 0, sr_x - 1
              i_f = ioff + ibase
              grnsmk(i, j) = grnsmk(i, j) + emis_smoke(i_f, j_f)
              grnhfx(i, j) = grnhfx(i, j) + fgrnhfx(i_f, j_f) ! * config_flags%fire_atm_feedback
              grnqfx(i, j) = grnqfx(i, j) + fgrnqfx(i_f, j_f) ! * config_flags%fire_atm_feedback
            end do
          end do
          grnhfx(i, j) = grnhfx(i, j) * avgw
          grnqfx(i, j) = grnqfx(i, j) * avgw
          grnsmk(i, j) = grnsmk(i, j) * convert_kg_m2_to_g_kg * avgw
          if (tracer_opt == 3) smoke_tracer(i, kts, j) = smoke_tracer(i, kts, j) + grnsmk(i, j)
        end do
      end do

      call Fire_tendency (               &
            ids,ide - 1,kds,kde,jds,jde - 1,     & ! dimensions
            ims,ime,kms,kme,jms,jme,     &
            its,min (ite, ide-1),kts,kte,jts,min (jte, jde - 1),     &
            grnhfx,grnqfx,canhfx,canqfx, & ! heat fluxes summed up to  atm grid
            alfg,alfc,z1can,             & ! coeffients, properties, geometry
            z_at_w,dz8w,mu,c1h,c2h,rho,  &
            config_flags%fire_atm_feedback, &
            rthfrten,rqvfrten)             ! theta and Qv tendencies

      if (tracer_opt == 3) call Calc_smoke_aod (dz8w, p_phy, t_phy, qv, rho, smoke_tracer, aod5502d_smoke, &
           ids, ide, kds, kde, jds, jde,          &
           ims, ime, kms, kme, jms, jme,          &
           its, ite, kts, kte, jts, jte)

    contains

      subroutine Check_dims (its, ite, jts, jte, ifts, ifte, jfts, jfte, sr_x, sr_y)

        use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

        implicit none

        integer, intent (in) :: its, ite, jts, jte, ifts, ifte, jfts, jfte, sr_x, sr_y

        integer :: isz1, jsz1, isz2, jsz2, ir, jr
        logical, parameter :: DEBUG_LOCAL = .false.


        isz1 = ite - its + 1
        jsz1 = jte - jts + 1
        isz2 = ifte - ifts + 1
        jsz2 = jfte - jfts + 1
        ir = isz2 / isz1
        jr = jsz2 / jsz1

        if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'its, ite, jts, jte =', its, ite, jts, jte
        if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'ifts, ifte, jfts, jfte =', ifts, ifte, jfts, jfte 
        if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'isz1, jsz1, isz2, jsz2 =', isz1, jsz1, isz2, jsz2
        if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'ir, jz =', ir, jr

        if (ir /= sr_x .or. jr /= sr_y) call Stop_simulation ('Tile dims do not preserve fire/atm ratio')

      end subroutine Check_dims

    end subroutine Provide_atm_feedback

    subroutine Fire_tendency(   &
        ids,ide, kds,kde, jds,jde,   & ! dimensions
        ims,ime, kms,kme, jms,jme,   &
        its,ite, kts,kte, jts,jte,   &
        grnhfx,grnqfx,canhfx,canqfx, & ! heat fluxes summed up to  atm grid
        alfg,alfc,z1can,             & ! coeffients, properties, geometry
        z_at_w,dz8w,mu,c1h,c2h,rho,  &
        fire_atm_feedback,           &
        rthfrten,rqvfrten)             ! theta and Qv tendencies

    ! This routine is atmospheric physics

    ! --- this routine takes fire generated heat and moisture fluxes and
    !     calculates their influence on the theta and water vapor
    ! --- note that these tendencies are valid at the Arakawa-A location

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT
      implicit none

    ! --- incoming variables

      integer, intent (in) :: ids, ide, kds, kde, jds, jde, &
                              ims, ime, kms, kme, jms, jme, &
                              its, ite, kts, kte, jts, jte

      real, intent(in), dimension(ims:ime, jms:jme) :: grnhfx,grnqfx  ! w/m^2
      real, intent(in), dimension(ims:ime, jms:jme) :: canhfx,canqfx  ! w/m^2
      real, intent(in), dimension(ims:ime, jms:jme) :: mu             ! dry air mass (pa)
      real, intent(in), dimension(kms:kme) :: c1h, c2h       ! hybrid coordinate weights

      real, intent(in), dimension(ims:ime, kms:kme, jms:jme) :: z_at_w ! m abv sealvl
      real, intent(in), dimension(ims:ime, kms:kme, jms:jme) :: dz8w   ! dz across w-lvl
      real, intent(in), dimension(ims:ime, kms:kme, jms:jme) :: rho    ! density

      real, intent(in) :: alfg     ! extinction depth surface fire heat (m)
      real, intent(in) :: alfc     ! extinction depth crown  fire heat (m)
      real, intent(in) :: z1can    ! height of crown fire heat release (m)

      real, intent(in) :: fire_atm_feedback

    ! --- outgoing variables

      real, intent(out), dimension(ims:ime, kms:kme, jms:jme) ::   &
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


!      write (OUTPUT_UNIT, *) 'pajm: its, ite, jts, jte, kts, kte = ', its, ite, jts, jte, kts, kte
!      write (OUTPUT_UNIT, *) 'pajm: ids, ide, jds, jde, kds, kde = ', ids, ide, jds, jde, kds, kde
!      write (OUTPUT_UNIT, *) 'pajm: alfg,alfc,z1can = ', alfg,alfc,z1can

      do j=jts,jte
        do k=kts,min(kte+1,kde)
          do i=its,ite
            rthfrten(i,k,j)=0.
            rqvfrten(i,k,j)=0.
          enddo
        enddo
      enddo

      if (fire_atm_feedback <= 0.0) return

    ! --- set some local constants

      cp_i = 1.0 / CP     ! inverse of specific heat
      xlv_i = 1.0 / XLV   ! inverse of latent heat
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
            hfx(i,k,j) = fact_g * grnhfx(i,j) * fire_atm_feedback+ fact_c * canhfx(i,j)

            ! --- vapor flux

            fact_g = xlv_i * EXP( - alfg_i * z_w )
            if (z_w < z1can) then
                   fact_c = xlv_i
            else
                   fact_c = xlv_i * EXP( - alfc_i * (z_w - z1can) )
            end if
            qfx(i,k,j) = fact_g * grnqfx(i,j) * fire_atm_feedback + fact_c * canqfx(i,j)

!            if ((grnhfx(i,j) * fire_atm_feedback >0.) .and. (k == 1)) then
!              write (OUTPUT_UNIT, *) 'masih: grnhfx, grnqfx', grnhfx(i,j), grnqfx(i,j)
!              write (OUTPUT_UNIT, *) 'masih: hfx, qfx', hfx(i,1,j), qfx(i,1,j), hfx(i,1,j), qfx(i,1,j)
!            end if

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

  end module wrf_mod
