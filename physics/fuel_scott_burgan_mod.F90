  module fuel_scott_burgan_mod

    ! Native Scott and Burgan 40 dynamic fuel models for CFBM.
    ! Primary scientific source: Scott and Burgan (2005), USDA RMRS-GTR-153.
    ! Metric values here are ported from the legacy WRF-Fire table in
    ! WRF/phys/module_fr_fire_phys.F and cross-checked against that source.

    use fuel_mod, only : fuel_t, UNKNOWN_FUEL_CAT

    implicit none

    private

    public :: fuel_scott_burgan_t

    integer, parameter :: N_FUEL_CAT_SCOTT_BURGAN = 40, NO_FUEL_CAT_SCOTT_BURGAN = 41

    type, extends (fuel_t) :: fuel_scott_burgan_t
      character (len = 80), dimension(N_FUEL_CAT_SCOTT_BURGAN + 1) :: fuel_name
    contains
      procedure, public :: Initialization => Init_scott_burgan_fuel_model
      procedure, public :: Resolve_fuel_index => Resolve_fuel_index_scott_burgan
    end type fuel_scott_burgan_t

  contains

    subroutine Init_scott_burgan_fuel_model (this, fuelmc_c)

      implicit none

      class (fuel_scott_burgan_t), intent(in out) :: this
      real, intent (in) :: fuelmc_c

      integer, parameter :: NF = N_FUEL_CAT_SCOTT_BURGAN + 1
      integer :: i


      this%n_fuel_cat = N_FUEL_CAT_SCOTT_BURGAN
      this%no_fuel_cat = NO_FUEL_CAT_SCOTT_BURGAN

      ! SB4 is row 40 by construction, independent of any namelist category
      ! count, so external code 204 cannot be lost as in legacy nfuelcats=53
      ! configurations. Row 41 is the explicit no-fuel row required by CFBM.
      this%fueldepthm = [ &
          0.1219, 0.3048, 0.6096, 0.6096, 0.4572, 0.4572, 0.9144, 1.2192, 1.5240, &
          0.2743, 0.4572, 0.5486, 0.6401, &
          0.3048, 0.3048, 0.7315, 0.9144, 1.8288, 0.6096, 1.8288, 0.9144, 1.3411, &
          0.1829, 0.3048, 0.3962, 0.1524, 0.3048, &
          0.0610, 0.0610, 0.0914, 0.1219, 0.1829, 0.0914, 0.1219, 0.0914, 0.1829, &
          0.3048, 0.3048, 0.3658, 0.8230, &
          0.3050 ]
      this%savr = [ &
          2200., 2000., 1500., 2000., 1800., 2200., 2000., 1500., 1800., &
          2000., 2000., 1800., 1800., &
          2000., 2000., 1600., 2000., 750., 750., 750., 750., 750., &
          2000., 2000., 1800., 2300., 1500., &
          2000., 2000., 2000., 2000., 2000., 2000., 2000., 1800., 1800., &
          2000., 2000., 2000., 2000., &
          3500. ]
      this%fuelmce = [ &
          0.15, 0.15, 0.30, 0.15, 0.40, 0.40, 0.15, 0.30, 0.40, &
          0.15, 0.15, 0.40, 0.40, &
          0.15, 0.15, 0.40, 0.30, 0.15, 0.30, 0.15, 0.40, 0.40, &
          0.20, 0.30, 0.30, 0.12, 0.25, &
          0.30, 0.25, 0.20, 0.25, 0.25, 0.25, 0.25, 0.35, 0.35, &
          0.25, 0.25, 0.25, 0.25, &
          0.12 ]
      this%weight = [ &
          7., 7., 7., 7., 7., 7., 7., 7., 7., &
          7., 7., 7., 7., &
          100., 100., 100., 100., 180., 100., 180., 100., 100., &
          900., 900., 900., 900., 900., &
          900., 900., 900., 900., 900., 900., 900., 900., 900., &
          900., 900., 900., 900., &
          7. ]
      this%fgi_1h = [ &
          0.10, 0.10, 0.10, 0.25, 0.40, 0.10, 1.00, 0.50, 1.00, &
          0.20, 0.50, 0.30, 1.90, &
          0.25, 1.35, 0.45, 0.85, 3.60, 2.90, 3.50, 2.05, 4.50, &
          0.20, 0.95, 1.10, 4.50, 4.00, &
          1.00, 1.40, 0.50, 0.50, 1.15, 2.40, 0.30, 5.80, 6.65, &
          1.50, 4.50, 5.50, 5.25, &
          0.0 ]
      this%fgi_10h = [ &
          0.00, 0.00, 0.40, 0.00, 0.00, 0.00, 0.00, 1.00, 1.00, &
          0.00, 0.50, 0.25, 0.30, &
          0.25, 2.40, 3.00, 1.15, 2.10, 1.45, 5.30, 3.40, 2.45, &
          0.90, 1.80, 0.15, 0.00, 4.00, &
          2.20, 2.30, 2.20, 1.50, 2.50, 1.20, 1.40, 1.40, 3.30, &
          3.00, 4.25, 2.75, 3.50, &
          0.0 ]
      this%fgi_100h = [ &
          0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, &
          0.00, 0.00, 0.00, 0.10, &
          0.00, 0.75, 0.00, 0.20, 0.00, 0.00, 2.20, 0.85, 0.00, &
          1.50, 1.25, 0.25, 0.00, 3.00, &
          3.60, 2.20, 2.80, 4.20, 4.40, 1.20, 8.10, 1.10, 4.15, &
          11.00, 4.00, 3.00, 5.25, &
          0.0 ]
      this%fgi_1000h = [ (0.0, i = 1, NF) ]
      this%fgi_live = [ &
          0.30, 1.00, 1.50, 1.90, 2.50, 3.40, 5.40, 7.30, 9.00, &
          0.50, 0.60, 1.45, 3.40, &
          0.15, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.55, &
          0.20, 0.00, 0.65, 0.00, 0.00, &
          0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, &
          0.00, 0.00, 0.00, 0.00, &
          0.0 ]
      this%fgi_lh = [ &
          0.0673, 0.2242, 0.3363, 0.4259, 0.5604, 0.7622, 1.2105, 1.6364, 2.0175, &
          0.1121, 0.1345, 0.3250, 0.7622, &
          0.0336, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.3475, &
          0.0448, 0.0000, 0.1457, 0.0000, 0.0000, &
          0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, &
          0.0000, 0.0000, 0.0000, 0.0000, &
          0.0 ]
      this%fgi = [ &
          0.0224, 0.0224, 0.1121, 0.0560, 0.0897, 0.0224, 0.2242, 0.3363, 0.4483, &
          0.0448, 0.2242, 0.1233, 0.5156, &
          0.1121, 1.0088, 0.7734, 0.4932, 1.2778, 0.9751, 2.4659, 1.4123, 1.5580, &
          0.5828, 0.8967, 0.3363, 1.0088, 2.4659, &
          1.5244, 1.3226, 1.2329, 1.3899, 1.8046, 1.0760, 2.1969, 1.8606, 3.1608, &
          3.4746, 2.8582, 2.5219, 3.1384, &
          1.e-7 ]

      allocate (this%ichap(NF))
      allocate (this%fueldens(NF))
      allocate (this%st(NF))
      allocate (this%se(NF))
      allocate (this%fgi_live_woody(NF))
      allocate (this%savr_live(NF))
      allocate (this%fuelmce_live(NF))
      allocate (this%fuelheat(NF))
      this%ichap = 0
      this%fueldens = 32.0
      this%st = 0.0555
      this%se = 0.010
      this%fgi_live_woody = 0.0
      this%savr_live = 0.0
      this%fuelmce_live = 0.0
      ! Scott and Burgan use 8000 BTU lb-1 except GR6, where RMRS-GTR-153
      ! specifies 9000 BTU lb-1. CFBM keeps this per row instead of a module
      ! constant so GR6 code 106 follows the source table.
      this%fuelheat = 8000.0
      this%fuelheat(6) = 9000.0

      this%fuel_name = ' '
      this%fuel_name(1) = 'GR1 (101)'
      this%fuel_name(6) = 'GR6 (106)'
      this%fuel_name(40) = 'SB4 (204)'
      this%fuel_name(41) = 'No fuel'

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
      ! External 14 and documented nonburnable 91..99 map to no fuel. Legacy
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
        case (14, 91:99)
          idx = this%no_fuel_cat
        case default
          idx = UNKNOWN_FUEL_CAT
      end select

    end function Resolve_fuel_index_scott_burgan

  end module fuel_scott_burgan_mod
