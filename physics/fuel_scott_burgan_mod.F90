  module fuel_scott_burgan_mod

    ! Native Scott and Burgan 40 dynamic fuel models for CFBM.
    ! Primary scientific source: Scott and Burgan (2005), USDA RMRS-GTR-153.
    ! Component tables follow Table 7; empirical consumption weights retain
    ! the WRF-Fire convention. No-fuel identity is preserved in external metadata.

    use fuel_mod, only : fuel_t, UNKNOWN_FUEL_CAT

    use constants_mod, only : CONVERT_TON_PER_ACRE_TO_KG_PER_M_SQ

    implicit none

    private

    public :: fuel_scott_burgan_t, Cured_fraction

    integer, parameter :: N_FUEL_CAT_SCOTT_BURGAN = 40
    integer, parameter :: NO_FUEL_CAT_SCOTT_BURGAN = N_FUEL_CAT_SCOTT_BURGAN + 1

    type, extends (fuel_t) :: fuel_scott_burgan_t
      ! Component loads [US short ton acre-1], separate from Anderson live load.
      real, allocatable :: fgi_lh(:), fgi_live_woody(:)
      ! Component SAV [ft-1]; 10h and 100h use 109 and 30 respectively.
      real, allocatable :: savr_1h(:), savr_lh(:), savr_live_woody(:)
      ! Low heat content [BTU lb-1], identical for dead/live in standard SB40.
      real, allocatable :: fuelheat(:)
      character (len = 80), dimension(N_FUEL_CAT_SCOTT_BURGAN + 1) :: fuel_name
    contains
      procedure :: Calc_effective_dead_load => Scott_burgan_dead_load
      procedure :: Moisture_loads => Scott_burgan_moisture_loads
      procedure :: Component_loads
      procedure, public :: Initialization => Init_scott_burgan_fuel_model
      procedure, public :: Resolve_fuel_index => Resolve_fuel_index_scott_burgan
    end type fuel_scott_burgan_t

  contains

    subroutine Init_scott_burgan_fuel_model (this, fuelmc_c)

      implicit none

      class (fuel_scott_burgan_t), intent(in out) :: this
      real, intent (in) :: fuelmc_c

      integer :: i
      real :: w(6), sigma(6)


      this%n_fuel_cat = N_FUEL_CAT_SCOTT_BURGAN
      this%n_moisture_classes = 6
      this%no_fuel_cat = NO_FUEL_CAT_SCOTT_BURGAN

      ! SB4 is length 40 by construction, independent of any namelist category
      ! count, so external code 204 cannot be lost as in legacy nfuelcats=53
      ! configurations. Row 41 is the explicit no-fuel row required by CFBM.
      ! Fuelbed depth [ft], converted to m below.
      this%fueldepthm = [ &
          0.4, 1.0, 2.0, 2.0, 1.5, 1.5, 3.0, 4.0, &
          5.0, 0.9, 1.5, 1.8, 2.1, 1.0, 1.0, 2.4, &
          3.0, 6.0, 2.0, 6.0, 3.0, 4.4, 0.6, 1.0, &
          1.3, 0.5, 1.0, 0.2, 0.2, 0.3, 0.4, 0.6, &
          0.3, 0.4, 0.3, 0.6, 1.0, 1.0, 1.2, 2.7, &
          1.0 ]
      this%fueldepthm = this%fueldepthm * 0.3048
      ! Dead 1h SAV [ft-1]; not the characteristic fuelbed SAV.
      this%savr_1h = [ &
          2200., 2000., 1500., 2000., 1800., 2200., 2000., 1500., 1800., &
          2000., 2000., 1800., 1800., &
          2000., 2000., 1600., 2000., 750., 750., 750., 750., 750., &
          2000., 2000., 1800., 2300., 1500., &
          2000., 2000., 2000., 2000., 2000., 2000., 2000., 1800., 1800., &
          2000., 2000., 2000., 2000., &
          3500. ]
      ! Dead extinction moisture [kg water kg-1 dry fuel].
      this%fuelmce = [ &
          0.15, 0.15, 0.3, 0.15, 0.4, 0.4, 0.15, 0.3, &
          0.4, 0.15, 0.15, 0.4, 0.4, 0.15, 0.15, 0.4, &
          0.3, 0.15, 0.3, 0.15, 0.4, 0.4, 0.2, 0.3, &
          0.3, 0.12, 0.25, 0.3, 0.25, 0.2, 0.25, 0.25, &
          0.25, 0.25, 0.35, 0.35, 0.25, 0.25, 0.25, 0.25, &
          0.12 ]
      ! Empirical fuel consumption time parameter [s].
      this%weight = [ &
          7., 7., 7., 7., 7., 7., 7., 7., 7., &
          7., 7., 7., 7., &
          100., 100., 100., 100., 180., 100., 180., 100., 100., &
          900., 900., 900., 900., 900., &
          900., 900., 900., 900., 900., 900., 900., 900., 900., &
          900., 900., 900., 900., &
          7. ]
      ! fgi_1h oven-dry load [US short ton acre-1].
      this%fgi_1h = [ &
          0.10, 0.10, 0.10, 0.25, 0.40, 0.10, 1.00, 0.50, 1.00, &
          0.20, 0.50, 0.30, 1.90, &
          0.25, 1.35, 0.45, 0.85, 3.60, 2.90, 3.50, 2.05, 4.50, &
          0.20, 0.95, 1.10, 4.50, 4.00, &
          1.00, 1.40, 0.50, 0.50, 1.15, 2.40, 0.30, 5.80, 6.65, &
          1.50, 4.50, 5.50, 5.25, &
          0.0 ]
      ! fgi_10h oven-dry load [US short ton acre-1].
      this%fgi_10h = [ &
          0.00, 0.00, 0.40, 0.00, 0.00, 0.00, 0.00, 1.00, 1.00, &
          0.00, 0.50, 0.25, 0.30, &
          0.25, 2.40, 3.00, 1.15, 2.10, 1.45, 5.30, 3.40, 2.45, &
          0.90, 1.80, 0.15, 0.00, 4.00, &
          2.20, 2.30, 2.20, 1.50, 2.50, 1.20, 1.40, 1.40, 3.30, &
          3.00, 4.25, 2.75, 3.50, &
          0.0 ]
      ! fgi_100h oven-dry load [US short ton acre-1].
      this%fgi_100h = [ &
          0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, &
          0.00, 0.00, 0.00, 0.10, &
          0.00, 0.75, 0.00, 0.20, 0.00, 0.00, 2.20, 0.85, 0.00, &
          1.50, 1.25, 0.25, 0.00, 3.00, &
          3.60, 2.20, 2.80, 4.20, 4.40, 1.20, 8.10, 1.10, 4.15, &
          11.00, 4.00, 3.00, 5.25, &
          0.0 ]
      ! fgi_1000h oven-dry load [US short ton acre-1].
      this%fgi_1000h = [ (0.0, i = 1, N_FUEL_CAT_SCOTT_BURGAN + 1) ]
      ! Herbaceous oven-dry load [short ton acre-1].
      this%fgi_lh = [ &
          0.3, 1.0, 1.5, 1.9, 2.5, 3.4, 5.4, 7.3, &
          9.0, 0.5, 0.6, 1.45, 3.4, 0.15, 0.0, 0.0, &
          0.0, 0.0, 0.0, 0.0, 0.0, 1.55, 0.2, 0.0, &
          0.65, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
          0.0 ]

      allocate (this%ichap(N_FUEL_CAT_SCOTT_BURGAN + 1))
      allocate (this%fueldens(N_FUEL_CAT_SCOTT_BURGAN + 1))
      allocate (this%st(N_FUEL_CAT_SCOTT_BURGAN + 1))
      allocate (this%se(N_FUEL_CAT_SCOTT_BURGAN + 1))
      allocate (this%fuelheat(N_FUEL_CAT_SCOTT_BURGAN + 1))
      this%ichap = 0
      this%fueldens = 32.0
      this%st = 0.0555
      this%se = 0.010
      ! Scott and Burgan use 8000 BTU lb-1 except GR6, where RMRS-GTR-153
      ! specifies 9000 BTU lb-1. CFBM keeps this per row instead of a module
      ! constant so GR6 code 106 follows the source table.
      this%fuelheat = 8000.0
      this%fuelheat(6) = 9000.0

      ! Woody oven-dry load [short ton acre-1].
      this%fgi_live_woody = [ &
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
          0.0, 0.65, 1.0, 1.25, 7.1, 1.3, 3.85, 6.2, &
          2.55, 2.9, 1.4, 3.4, 4.35, 7.0, 0.9, 0.2, &
          1.1, 2.0, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
          0.0 ]
      ! Particle SAV [ft-1]; absent components use a benign positive value.
      this%savr_lh = [ &
          2000.0, 1800.0, 1300.0, 1800.0, 1600.0, 2000.0, 1800.0, 1300.0, &
          1600.0, 1800.0, 1800.0, 1600.0, 1600.0, 1800.0, 1.0, 1.0, &
          1.0, 1.0, 1.0, 1.0, 1.0, 1800.0, 1800.0, 1.0, &
          1600.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
          1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
          1.0 ]
      ! Particle SAV [ft-1]; absent components use a benign positive value.
      this%savr_live_woody = [ &
          1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
          1.0, 1800.0, 1800.0, 1600.0, 1600.0, 1600.0, 1600.0, 1400.0, &
          1600.0, 1600.0, 1600.0, 1600.0, 1600.0, 1500.0, 1600.0, 1600.0, &
          1400.0, 2000.0, 750.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
          1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
          1.0 ]
      allocate(this%fgi(N_FUEL_CAT_SCOTT_BURGAN + 1), this%savr(N_FUEL_CAT_SCOTT_BURGAN + 1))
      this%savr = 1.0
      do i = 1, N_FUEL_CAT_SCOTT_BURGAN + 1
        w = this%Component_loads(i, 1.20)
        ! Total consumable oven-dry inventory [kg m-2], invariant under curing.
        this%fgi(i) = real(sum(w) * CONVERT_TON_PER_ACRE_TO_KG_PER_M_SQ)
        sigma = [this%savr_1h(i),109.0,30.0,this%savr_lh(i),this%savr_lh(i),this%savr_live_woody(i)]
        if (sum(w) > 0.0) this%savr(i) = sum(w*sigma*sigma)/sum(w*sigma)
      end do

      this%fuel_name = ' '

      this%fuel_name(1) = 'GR1 (101) Short, sparse, dry climate grass (D)'
      this%fuel_name(2) = 'GR2 (102) Low load, dry climate grass (D)'
      this%fuel_name(3) = 'GR3 (103) Low load, very coarse, humid climate grass (D)'
      this%fuel_name(4) = 'GR4 (104) Moderate load, dry climate grass (D)'
      this%fuel_name(5) = 'GR5 (105) Low load, humid climate grass (D)'
      this%fuel_name(6) = 'GR6 (106) Moderate load, humid climate grass (D)'
      this%fuel_name(7) = 'GR7 (107) High load, dry climate grass (D)'
      this%fuel_name(8) = 'GR8 (108) High load, very coarse, humid climate grass (D)'
      this%fuel_name(9) = 'GR9 (109) Very high load, humid climate grass (D)'
      this%fuel_name(10) = 'GS1 (121) Low load, dry climate grass-shrub (D)'
      this%fuel_name(11) = 'GS2 (122) Moderate load, dry climate grass-shrub (D)'
      this%fuel_name(12) = 'GS3 (123) Moderate load, humid climate grass-shrub (D)'
      this%fuel_name(13) = 'GS4 (124) High load, humid climate grass-shrub (D)'
      this%fuel_name(14) = 'SH1 (141) Low load, dry climate shrub (D)'
      this%fuel_name(15) = 'SH2 (142) Moderate load, dry climate shrub (S)'
      this%fuel_name(16) = 'SH3 (143) Moderate load, humid climate shrub (S)'
      this%fuel_name(17) = 'SH4 (144) Low load, humid climate timber-shrub (S)'
      this%fuel_name(18) = 'SH5 (145) High load, dry climate shrub (S)'
      this%fuel_name(19) = 'SH6 (146) Low load, humid climate shrub (S)'
      this%fuel_name(20) = 'SH7 (147) Very high load, dry climate shrub (S)'
      this%fuel_name(21) = 'SH8 (148) High load, humid climate shrub (S)'
      this%fuel_name(22) = 'SH9 (149) Very high load, humid climate shrub (D)'
      this%fuel_name(23) = 'TU1 (161) Light load, dry climate timber-grass-shrub (D)'
      this%fuel_name(24) = 'TU2 (162) Moderate load, humid climate timber-shrub (S)'
      this%fuel_name(25) = 'TU3 (163) Moderate load, humid climate timber-grass-shrub (D)'
      this%fuel_name(26) = 'TU4 (164) Dwarf conifer understory (S)'
      this%fuel_name(27) = 'TU5 (165) Very high load, dry climate timber-shrub (S)'
      this%fuel_name(28) = 'TL1 (181) Low load, compact conifer litter (S)'
      this%fuel_name(29) = 'TL2 (182) Low load broadleaf litter (S)'
      this%fuel_name(30) = 'TL3 (183) Moderate load conifer litter (S)'
      this%fuel_name(31) = 'TL4 (184) Small downed logs (S)'
      this%fuel_name(32) = 'TL5 (185) High load conifer litter (S)'
      this%fuel_name(33) = 'TL6 (186) High load broadleaf litter (S)'
      this%fuel_name(34) = 'TL7 (187) Large downed logs (S)'
      this%fuel_name(35) = 'TL8 (188) Long-needle litter (S)'
      this%fuel_name(36) = 'TL9 (189) Very high load broadleaf litter (S)'
      this%fuel_name(37) = 'SB1 (201) Low load activity fuel (S)'
      this%fuel_name(38) = 'SB2 (202) Moderate load activity or low load blowdown (S)'
      this%fuel_name(39) = 'SB3 (203) High load activity fuel or moderate load blowdown (S)'
      this%fuel_name(40) = 'SB4 (204) High load blowdown (S)'
      this%fuel_name(N_FUEL_CAT_SCOTT_BURGAN + 1) = 'Nonburnable'
      call this%Check_fuels()
      call this%Calc_wind_adjustment_factor()

    end subroutine Init_scott_burgan_fuel_model

    pure function Resolve_fuel_index_scott_burgan (this, external_code) result (idx)

      implicit none

      class (fuel_scott_burgan_t), intent (in) :: this
      integer, intent (in) :: external_code

      integer :: idx


      ! Native SB40 accepts Scott and Burgan external LANDFIRE-style ranges:
      ! 101..109 -> GR rows 1..9, 121..124 -> GS rows 10..13,
      ! 141..149 -> SH rows 14..22, 161..165 -> TU rows 23..27,
      ! 181..189 -> TL rows 28..36, and 201..204 -> SB rows 37..40.
      ! External 14 and named NB codes 91,92,93,98,99 map to no fuel. Legacy
      ! internal rows 15..54 are intentionally rejected here because accepting
      ! them would make nfuel_cat ambiguous unless a future alias mode is added.
      select case (external_code)
        case (101:109)
          idx = external_code - 100
        case (121:124)
          idx = external_code - 111
        case (141:149)
          idx = external_code - 127
        case (161:165)
          idx = external_code - 138
        case (181:189)
          idx = external_code - 153
        case (201:204)
          idx = external_code - 164
        case (14, 91, 92, 93, 98, 99)
          idx = this%no_fuel_cat
        case default
          idx = UNKNOWN_FUEL_CAT
      end select

    end function Resolve_fuel_index_scott_burgan

    pure real function Cured_fraction(mc_live) result(fraction)
      real, intent(in) :: mc_live
      ! Scott and Burgan: fully cured at 30%, no curing at 120% live moisture.
      fraction = max(0.0, min(1.0, (1.20 - mc_live)/0.90))
    end function

    pure function Component_loads(this, k, mc_live) result(loads)
      class(fuel_scott_burgan_t), intent(in) :: this
      integer, intent(in) :: k
      real, intent(in) :: mc_live
      real :: loads(6), cured
      cured = Cured_fraction(mc_live) * this%fgi_lh(k)
      ! Dead 1h, 10h, 100h, cured herb; remaining live herb and live woody.
      loads = [this%fgi_1h(k),this%fgi_10h(k),this%fgi_100h(k),cured,this%fgi_lh(k)-cured,this%fgi_live_woody(k)]
    end function

    pure function Scott_burgan_moisture_loads(this, k, mc_live) result(loads)
      class(fuel_scott_burgan_t), intent(in) :: this
      integer, intent(in) :: k
      real, intent(in) :: mc_live
      real :: loads(this%n_moisture_classes), w(6)
      w = this%Component_loads(k, mc_live)
      ! FMC keeps its 1000h slot; cured herb uses 1h moisture, woody uses slot 6.
      loads = [w(1)+w(4),w(2),w(3),0.0,w(5),w(6)]
    end function

    pure real function Scott_burgan_dead_load(this, k, mc_live) result(load)
      class(fuel_scott_burgan_t), intent(in) :: this
      integer, intent(in) :: k
      real, intent(in) :: mc_live
      real :: w(6)
      w = this%Component_loads(k, mc_live)
      load = real(sum(w(1:4)) * CONVERT_TON_PER_ACRE_TO_KG_PER_M_SQ)
    end function
  end module fuel_scott_burgan_mod
