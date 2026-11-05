module ros_sb40_mod

  ! Rothermel surface-fire parameters for one SB40 fuelbed and moisture state.
  ! Dead and live components contribute separately to the reaction intensity.
  ! Calc_ros_wrffire subsequently applies wind, slope, and spread direction.
  !
  ! Equations and Albini size-class weighting: Andrews (2018), RMRS-GTR-371.
  ! Herbaceous load transfer: Scott and Burgan (2005), RMRS-GTR-153.
  ! Intermediate calculations use REAL64 and the equations' customary units
  ! (ft, lb, min, BTU); outputs follow the existing CFBM ROS convention.

  use, intrinsic :: iso_fortran_env, only : REAL64
  use fuel_scott_burgan_mod, only : fuel_scott_burgan_t

  implicit none

  private

  public :: Calc_sb40_parameters

contains

  pure subroutine Calc_sb40_parameters(fuels, k, moisture, beta, b, wind_coefficient, r0, iboros, &
      reaction_dead, reaction_live, extinction_live)

    ! k is the validated internal fuel-table row, including the no-fuel row.
    type(fuel_scott_burgan_t), intent(in) :: fuels
    integer, intent(in) :: k

    ! Input FMC order: 1h, 10h, 100h, 1000h, live herbaceous, live woody.
    ! Moisture is water mass per unit oven-dry fuel mass [kg kg-1], not percent.
    ! SB40 has no 1000h reaction load, so moisture(4) is not used here.
    real, intent(in) :: moisture(6)

    ! beta: packing ratio; b: wind exponent; wind_coefficient multiplies U**b
    ! with midflame U in ft min-1. r0 is spread without wind or slope [ft min-1].
    ! iboros is reaction intensity times residence time [kJ m-2], used by the
    ! caller to obtain fireline intensity from the actual spread rate.
    real, intent(out) :: beta, b, wind_coefficient, r0, iboros

    ! Optional diagnostics: dead/live reaction [BTU ft-2 min-1] and the live
    ! moisture threshold for extinction [kg kg-1 dry fuel].
    real, intent(out), optional :: reaction_dead, reaction_live, extinction_live

    ! Internal component order differs from the input FMC order in slot 4:
    ! 1h, 10h, 100h, cured herbaceous, remaining live herbaceous, live woody.
    ! w is oven-dry load [lb ft-2]; sigma is surface-area-to-volume ratio
    ! (SAV) [ft-1]; mc is component moisture [kg kg-1 dry fuel].
    ! area is particle surface area per unit ground area [ft2 ft-2].
    real(REAL64) :: w(6), sigma(6), mc(6), area(6), fraction(6), fine(6)

    ! phase=1 contains dead components 1:4; phase=2 contains live components
    ! 5:6. net_load is weighted mineral-free load [lb ft-2]; ir is reaction
    ! intensity [BTU ft-2 min-1]; mx is extinction moisture [kg kg-1].
    real(REAL64) :: phase_area(2), phase_moisture(2), mx(2), net_load(2), ir(2)
    real(REAL64) :: sigma_bed, total_area, depth, packing, optimum, relative, gamma, a, gamma_max
    real(REAL64) :: heat_sink, flux, fine_dead, fine_live, fine_mc, damp, ratio, eta_s
    real(REAL64) :: grouped_fraction, bulk_density, total_load, c, e
    integer :: phase, lo, hi, i, j

    ! 1. Initialize a nonburning result.
    ! Positive beta and b keep the caller's powers finite for no-fuel cells;
    ! zero r0, wind coefficient, and iboros ensure zero spread and intensity.
    beta = 1.0
    b = 1.0
    wind_coefficient = 0.0
    r0 = 0.0
    iboros = 0.0
    if (present(reaction_dead)) reaction_dead = 0.0
    if (present(reaction_live)) reaction_live = 0.0
    if (present(extinction_live)) extinction_live = 0.0
    if (k == fuels%no_fuel_cat) return

    ! 2. Partition herbaceous load and align component properties.
    ! Component_loads uses live herbaceous moisture to transfer load to cured
    ! herbaceous fuel, conserving total dry mass. Cured herb retains herbaceous
    ! SAV and takes 1h dead moisture. Remaining herb and woody fuel stay live.
    ! Convert US short ton acre-1 to lb ft-2 (2000 lb per ton, 43560 ft2 per
    ! acre). Table loads are already oven-dry; no wet-to-dry factor is needed.
    w = real(fuels%Component_loads(k, moisture(5)), REAL64) * (2000.0_REAL64/43560.0_REAL64)
    sigma = real([fuels%savr_1h(k),fuels%savr_10h(k),fuels%savr_100h(k),fuels%savr_lh(k),fuels%savr_lh(k), &
        fuels%savr_live_woody(k)], REAL64)
    mc = real([moisture(1:3),moisture(1),moisture(5:6)], REAL64)

    ! 3. Derive fuelbed geometry from all dead and live components.
    ! Particle volume per ground area is w/density. Multiplying by SAV gives
    ! area, which weights the characteristic fuelbed SAV, sigma_bed [ft-1].
    total_load = sum(w)
    if (total_load <= 0.0_REAL64) return
    area = w * sigma / real(fuels%fueldens(k),REAL64)
    total_area = sum(area)
    if (total_area <= 0.0_REAL64) return
    phase_area = [sum(area(1:4)),sum(area(5:6))]
    sigma_bed = sum(area*sigma)/total_area

    ! Convert depth from m to ft. Bulk density [lb ft-3] includes void space;
    ! packing is its ratio to particle density. relative compares that packing
    ! with the optimum for the characteristic SAV and enters reaction and wind.
    depth = real(fuels%fueldepthm(k),REAL64)/0.3048_REAL64
    bulk_density = total_load/depth
    packing = bulk_density/real(fuels%fueldens(k),REAL64)
    optimum = 3.348_REAL64 * sigma_bed**(-0.8189_REAL64)
    relative = packing/optimum

    ! 4. Calculate characteristic moisture and net reaction load by life state.
    ! fraction sums to one within each populated dead/live phase. It weights
    ! phase moisture, while Albini SAV-band totals weight the net reaction load.
    ! Grouping prevents identical fine particles from losing reaction load
    ! merely because they occupy separate input slots, such as 1h and cured herb.
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

      ! Each component receives the summed surface fraction of its SAV band
      ! within the same life state. (1-st) removes the total mineral fraction.
      do i = lo, hi
        grouped_fraction = 0.0_REAL64
        if (Sav_band(sigma(i)) == 0) cycle
        do j = lo, hi
          if (Sav_band(sigma(j)) == Sav_band(sigma(i))) grouped_fraction = grouped_fraction + fraction(j)
        end do
        net_load(phase) = net_load(phase) + grouped_fraction*w(i)*(1.0_REAL64-real(fuels%st(k),REAL64))
      end do
    end do

    ! 5. Derive live extinction moisture from the current dead/live mixture.
    ! Dead extinction comes from the fuel table. Exponential factors give fine
    ! dead/live load for Albini's extinction relation; fine_mc uses the dead
    ! weights, which differ from the surface weights used for phase_moisture.
    ! The live threshold is bounded below by dead extinction. Retain that
    ! lower bound when either fine-load sum is too small to form the ratio.
    mx(1) = real(fuels%fuelmce(k),REAL64)
    mx(2) = mx(1)
    fine = w*exp(-138.0_REAL64/sigma)
    fine_dead = sum(fine(1:4))
    fine_live = sum(w(5:6)*exp(-500.0_REAL64/sigma(5:6)))
    if (fine_live > tiny(fine_live) .and. fine_dead > tiny(fine_dead)) then
      fine_mc = sum(fine(1:4)*mc(1:4))/fine_dead
      mx(2) = max(mx(1), 2.9_REAL64*fine_dead/fine_live*(1.0_REAL64-fine_mc/mx(1))-0.226_REAL64)
    end if

    ! 6. Calculate reaction velocity and mineral damping for the fuelbed.
    ! gamma_max and gamma have units min-1. The packing response reaches its
    ! maximum at relative=1. eta_s accounts for effective mineral content se;
    ! this damping is separate from removing total minerals st from net_load.
    a = 133.0_REAL64/sigma_bed**0.7913_REAL64
    gamma_max = sigma_bed**1.5_REAL64/(495.0_REAL64+0.0594_REAL64*sigma_bed**1.5_REAL64)
    gamma = gamma_max*relative**a*exp(a*(1.0_REAL64-relative))
    eta_s = min(1.0_REAL64,0.174_REAL64*real(fuels%se(k),REAL64)**(-0.19_REAL64))

    ! Apply moisture damping independently to dead and live reaction. A missing
    ! phase or moisture at/above its extinction threshold contributes zero.
    ! Below extinction, the cubic uses moisture divided by that threshold.
    ! The table supplies one heat content [BTU lb-1] for both life states.
    ir = 0.0_REAL64
    do phase = 1, 2
      if (phase_area(phase) <= 0.0_REAL64 .or. phase_moisture(phase) >= mx(phase)) cycle
      ratio = phase_moisture(phase)/mx(phase)
      damp = max(0.0_REAL64,1.0_REAL64-2.59_REAL64*ratio+5.11_REAL64*ratio**2-3.52_REAL64*ratio**3)
      ir(phase) = gamma*net_load(phase)*real(fuels%fuelheat(k),REAL64)*eta_s*damp
    end do

    ! 7. Form the heat sink and propagating flux for spread without wind/slope.
    ! Heat of preignition is 250+1116*mc [BTU lb-1]. The heating number
    ! exp(-138/sigma) and whole-bed surface fractions weight each component,
    ! including components whose moisture prevents them from contributing ir.
    ! Multiplying by bulk density gives heat_sink [BTU ft-3]; flux is the
    ! dimensionless fraction of reaction intensity that heats adjacent fuel.
    heat_sink = bulk_density*sum(area/total_area*exp(-138.0_REAL64/sigma)*(250.0_REAL64+1116.0_REAL64*mc))
    flux = exp((0.792_REAL64+0.681_REAL64*sqrt(sigma_bed))*(packing+0.1_REAL64)) / &
        (192.0_REAL64+0.2595_REAL64*sigma_bed)

    ! 8. Return the coefficients consumed by Calc_ros_wrffire.
    ! The wind multiplier there is c*relative**(-e)*U**b. Actual wind and slope
    ! are applied by that caller, so this routine needs neither as an input.
    c = 7.47_REAL64*exp(-0.133_REAL64*sigma_bed**0.55_REAL64)
    e = 0.715_REAL64*exp(-3.59e-4_REAL64*sigma_bed)
    beta = real(packing)
    b = real(0.02526_REAL64*sigma_bed**0.54_REAL64)
    wind_coefficient = real(c*relative**(-e))

    ! Sum dead/live reaction, then divide propagating heat by the heat sink.
    ! For iboros, residence time is 384/sigma_bed [min]; 1.055 converts BTU to
    ! kJ and 0.3048**2 converts ft2 to m2. The result is heat per ground area.
    r0 = real(sum(ir)*flux/heat_sink) ! ft min-1, as required by Calc_ros_wrffire.
    iboros = real(sum(ir)*(384.0_REAL64/sigma_bed)*1.055_REAL64/0.3048_REAL64**2) ! kJ m-2.

    if (present(reaction_dead)) reaction_dead = real(ir(1)) ! BTU ft-2 min-1.
    if (present(reaction_live)) reaction_live = real(ir(2))
    if (present(extinction_live)) extinction_live = real(mx(2))

  end subroutine

  pure integer function Sav_band(sigma) result(band)

    ! Albini reaction-load groups, ordered from fine to coarse particles.
    ! SAV limits are in ft-1; an exact boundary belongs to the finer group.
    ! Values below 16 return zero and receive no net reaction-load weight in
    ! Calc_sb40_parameters. This grouping does not alter the other averages.
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
