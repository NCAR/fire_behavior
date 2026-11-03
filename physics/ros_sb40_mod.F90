module ros_sb40_mod
  ! Dead/live Rothermel with Albini size-class weighting and live extinction.
  ! Equations: Andrews (2018), RMRS-GTR-371; fuel transfer: Scott and Burgan
  ! (2005), RMRS-GTR-153. Evaluated in the equations' customary units using
  ! double precision; interface values follow the existing CFBM ROS convention.
  use, intrinsic :: iso_fortran_env, only : REAL64
  use fuel_scott_burgan_mod, only : fuel_scott_burgan_t
  implicit none
  private
  public :: Calc_sb40_parameters
contains
  pure subroutine Calc_sb40_parameters(fuels, k, moisture, beta, b, wind_coefficient, r0, iboros, &
      reaction_dead, reaction_live, extinction_live)
    type(fuel_scott_burgan_t), intent(in) :: fuels
    integer, intent(in) :: k
    ! FMC order: 1h, 10h, 100h, 1000h, live herbaceous, live woody [kg kg-1].
    real, intent(in) :: moisture(6)
    real, intent(out) :: beta, b, wind_coefficient, r0, iboros
    real, intent(out), optional :: reaction_dead, reaction_live, extinction_live
    real(REAL64) :: w(6), sigma(6), mc(6), area(6), fraction(6), fine(6)
    real(REAL64) :: phase_area(2), phase_moisture(2), mx(2), net_load(2), ir(2)
    real(REAL64) :: sigma_bed, total_area, depth, packing, optimum, relative, gamma, a, gamma_max
    real(REAL64) :: heat_sink, flux, fine_dead, fine_live, fine_mc, damp, ratio, eta_s
    real(REAL64) :: grouped_fraction, bulk_density, total_load, c, e
    integer :: phase, lo, hi, i, j

    beta = 1.0
    b = 1.0
    wind_coefficient = 0.0
    r0 = 0.0
    iboros = 0.0
    if (present(reaction_dead)) reaction_dead = 0.0
    if (present(reaction_live)) reaction_live = 0.0
    if (present(extinction_live)) extinction_live = 0.0
    if (k == fuels%no_fuel_cat) return

    ! Tables are oven-dry: do not apply a wet-to-dry correction. Cured herb
    ! retains herbaceous SAV but takes the 1h dead moisture, conserving load.
    w = real(fuels%Component_loads(k, moisture(5)), REAL64) * (2000.0_REAL64/43560.0_REAL64)
    sigma = real([fuels%savr_1h(k),109.0,30.0,fuels%savr_lh(k),fuels%savr_lh(k), &
        fuels%savr_live_woody(k)], REAL64)
    mc = real([moisture(1:3),moisture(1),moisture(5:6)], REAL64)
    total_load = sum(w)
    if (total_load <= 0.0_REAL64) return
    area = w * sigma / real(fuels%fueldens(k),REAL64)
    total_area = sum(area)
    if (total_area <= 0.0_REAL64) return
    phase_area = [sum(area(1:4)),sum(area(5:6))]
    sigma_bed = sum(area*sigma)/total_area
    depth = real(fuels%fueldepthm(k),REAL64)/0.3048_REAL64
    bulk_density = total_load/depth
    packing = bulk_density/real(fuels%fueldens(k),REAL64)
    optimum = 3.348_REAL64 * sigma_bed**(-0.8189_REAL64)
    relative = packing/optimum

    ! Characteristic moisture uses within-life-state surface area. Net load
    ! instead uses Albini's grouped SAV bands, so identical fine particles do
    ! not lose reaction load merely because they occupy separate input slots.
    fraction = 0.0_REAL64
    net_load = 0.0_REAL64
    phase_moisture = 0.0_REAL64
    do phase = 1, 2
      lo = 1
      hi = 4
      if (phase == 2) then
        lo = 5
        hi = 6
      end if
      if (phase_area(phase) <= 0.0_REAL64) cycle
      fraction(lo:hi) = area(lo:hi)/phase_area(phase)
      phase_moisture(phase) = sum(fraction(lo:hi)*mc(lo:hi))
      do i = lo, hi
        grouped_fraction = 0.0_REAL64
        if (Sav_band(sigma(i)) == 0) cycle
        do j = lo, hi
          if (Sav_band(sigma(j)) == Sav_band(sigma(i))) grouped_fraction = grouped_fraction + fraction(j)
        end do
        net_load(phase) = net_load(phase) + grouped_fraction*w(i)*(1.0_REAL64-real(fuels%st(k),REAL64))
      end do
    end do

    mx(1) = real(fuels%fuelmce(k),REAL64)
    mx(2) = mx(1)
    fine = w*exp(-138.0_REAL64/sigma)
    fine_dead = sum(fine(1:4))
    fine_live = sum(w(5:6)*exp(-500.0_REAL64/sigma(5:6)))
    if (fine_live > tiny(fine_live) .and. fine_dead > tiny(fine_dead)) then
      fine_mc = sum(fine(1:4)*mc(1:4))/fine_dead
      mx(2) = max(mx(1), 2.9_REAL64*fine_dead/fine_live*(1.0_REAL64-fine_mc/mx(1))-0.226_REAL64)
    end if
    a = 133.0_REAL64/sigma_bed**0.7913_REAL64
    gamma_max = sigma_bed**1.5_REAL64/(495.0_REAL64+0.0594_REAL64*sigma_bed**1.5_REAL64)
    gamma = gamma_max*relative**a*exp(a*(1.0_REAL64-relative))
    eta_s = min(1.0_REAL64,0.174_REAL64*real(fuels%se(k),REAL64)**(-0.19_REAL64))
    ir = 0.0_REAL64
    do phase = 1, 2
      if (phase_area(phase) <= 0.0_REAL64 .or. phase_moisture(phase) >= mx(phase)) cycle
      ratio = phase_moisture(phase)/mx(phase)
      damp = max(0.0_REAL64,1.0_REAL64-2.59_REAL64*ratio+5.11_REAL64*ratio**2-3.52_REAL64*ratio**3)
      ir(phase) = gamma*net_load(phase)*real(fuels%fuelheat(k),REAL64)*eta_s*damp
    end do
    heat_sink = bulk_density*sum(area/total_area*exp(-138.0_REAL64/sigma)*(250.0_REAL64+1116.0_REAL64*mc))
    flux = exp((0.792_REAL64+0.681_REAL64*sqrt(sigma_bed))*(packing+0.1_REAL64)) / &
        (192.0_REAL64+0.2595_REAL64*sigma_bed)
    c = 7.47_REAL64*exp(-0.133_REAL64*sigma_bed**0.55_REAL64)
    e = 0.715_REAL64*exp(-3.59e-4_REAL64*sigma_bed)
    beta = real(packing)
    b = real(0.02526_REAL64*sigma_bed**0.54_REAL64)
    wind_coefficient = real(c*relative**(-e))
    r0 = real(sum(ir)*flux/heat_sink) ! ft min-1, as required by Calc_ros_wrffire.
    iboros = real(sum(ir)*(384.0_REAL64/sigma_bed)*1.055_REAL64/0.3048_REAL64**2) ! kJ m-2.
    if (present(reaction_dead)) reaction_dead = real(ir(1)) ! BTU ft-2 min-1.
    if (present(reaction_live)) reaction_live = real(ir(2))
    if (present(extinction_live)) extinction_live = real(mx(2))
  end subroutine

  pure integer function Sav_band(sigma) result(band)
    real(REAL64), intent(in) :: sigma
    real(REAL64), parameter :: limits(5) = [1200.0_REAL64,192.0_REAL64,96.0_REAL64,48.0_REAL64,16.0_REAL64]
    integer :: i
    band = 0
    do i = 1, 5
      if (sigma >= limits(i)) then
        band = i
        return
      end if
    end do
  end function
end module ros_sb40_mod
