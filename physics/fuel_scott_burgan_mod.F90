  module fuel_scott_burgan_mod

    ! Native Scott and Burgan 40 dynamic fuel models for CFBM.
    ! Primary scientific source: Scott and Burgan (2005), USDA RMRS-GTR-153.
    ! Component tables follow Table 7; empirical consumption weights retain
    ! the WRF-Fire convention. No-fuel identity is preserved in external metadata.

    use fuel_mod, only : fuel_t, UNKNOWN_FUEL_CAT

    use constants_mod, only : CONVERT_TON_PER_ACRE_TO_KG_PER_M_SQ

    use fuel_table_mod, only: fuel_table_t, Read_fuel_table, Require_table

    implicit none

    private

    public :: fuel_scott_burgan_t

    integer, parameter :: N_FUEL_CAT_SCOTT_BURGAN = 40
    integer, parameter :: NO_FUEL_CAT_SCOTT_BURGAN = N_FUEL_CAT_SCOTT_BURGAN + 1

    type, extends (fuel_t) :: fuel_scott_burgan_t
      ! Component loads [US short ton acre-1], separate from Anderson live load.
      real, allocatable :: fgi_lh(:), fgi_live_woody(:)
      ! Component SAV [ft-1], including table-defined 10h and 100h values.
      real, allocatable :: savr_1h(:), savr_10h(:), savr_100h(:), savr_lh(:), savr_live_woody(:)
      ! Low heat content [BTU lb-1], identical for dead/live in standard SB40.
      real, allocatable :: fuelheat(:)
      real :: curing_none, curing_span
      character (len = 80), dimension(N_FUEL_CAT_SCOTT_BURGAN + 1) :: fuel_name
    contains
      procedure :: Calc_effective_dead_load => Scott_burgan_dead_load
      procedure :: Moisture_loads => Scott_burgan_moisture_loads
      procedure :: Cured_fraction
      procedure :: Component_loads
      procedure, public :: Initialization => Init_scott_burgan_fuel_model
      procedure, public :: Resolve_fuel_index => Resolve_fuel_index_scott_burgan
    end type fuel_scott_burgan_t

  contains

    subroutine Init_scott_burgan_fuel_model (this, fuelmc_c, filename, comm)

      implicit none

      class (fuel_scott_burgan_t), intent(in out) :: this
      real, intent (in) :: fuelmc_c
      character(len=*), intent(in), optional :: filename
      integer, intent(in), optional :: comm
      type(fuel_table_t) :: table

      integer :: i
      real :: w(6), sigma(6)


      this%n_fuel_cat = N_FUEL_CAT_SCOTT_BURGAN
      this%n_moisture_classes = 6
      this%no_fuel_cat = NO_FUEL_CAT_SCOTT_BURGAN

      call Read_fuel_table('SCOTT_BURGAN40', &
          [(i,i=101,109),(i,i=121,124),(i,i=141,149),(i,i=161,165),(i,i=181,189),(i,i=201,204),14], &
          'code depth[ft] weight[s] density[lb/ft3] st[1] se[1] savr1h[1/ft] savr10h[1/ft] savr100h[1/ft] ' // &
          'savrLH[1/ft] savrLW[1/ft] extinction[kg/kg] heat[BTU/lb] dead1h[ton/ac] dead10h[ton/ac] ' // &
          'dead100h[ton/ac] LH[ton/ac] LW[ton/ac] name', table, filename, comm)
      this%moisture = table%moisture
      this%table_fingerprint = table%fingerprint
      this%legacy_table_compatible = table%legacy_compatible
      this%fuel_name = table%names
      this%fueldepthm = table%properties(1,:)
      this%weight = table%properties(2,:)
      this%fueldens = table%properties(3,:)
      this%st = table%properties(4,:)
      this%se = table%properties(5,:)
      this%savr_1h = table%properties(6,:)
      this%savr_10h = table%properties(7,:)
      this%savr_100h = table%properties(8,:)
      this%savr_lh = table%properties(9,:)
      this%savr_live_woody = table%properties(10,:)
      this%fuelmce = table%properties(11,:)
      this%fuelheat = table%properties(12,:)
      this%fgi_1h = table%properties(13,:)
      this%fgi_10h = table%properties(14,:)
      this%fgi_100h = table%properties(15,:)
      this%fgi_lh = table%properties(16,:)
      this%fgi_live_woody = table%properties(17,:)
      call Require_table(all(this%fueldepthm > 0), 'fuel depth must be positive')
      call Require_table(all(this%weight > 0), 'consumption weight must be positive')
      call Require_table(all(this%fueldens > 0), 'particle density must be positive')
      call Require_table(all(this%st < 1), 'total mineral fraction must be below one')
      call Require_table(all(this%se > 0 .and. this%se <= this%st), 'invalid effective mineral fraction')
      call Require_table(all(this%fuelmce > 0), 'dead extinction moisture must be positive')
      this%fueldepthm = this%fueldepthm * 0.3048
      this%curing_none = table%curing_none
      this%curing_span = table%curing_span
      ! SB40 has no 1000-h reaction component or Anderson chaparral override.
      this%fgi_1000h = [(0.0,i=1,N_FUEL_CAT_SCOTT_BURGAN+1)]
      this%ichap = [(0,i=1,N_FUEL_CAT_SCOTT_BURGAN+1)]
      call Require_table(all(this%savr_1h > 0) .and. all(this%savr_10h > 0) .and. &
          all(this%savr_100h > 0) .and. all(this%savr_lh > 0) .and. all(this%savr_live_woody > 0), &
          'component SAV must be positive')
      call Require_table(all(this%fuelheat > 0), 'heat content must be positive')
      call Require_table(all(this%Component_loads(this%no_fuel_cat,this%curing_none) == 0), &
          'nonburnable component load must be zero')
      allocate(this%fgi(N_FUEL_CAT_SCOTT_BURGAN + 1), this%savr(N_FUEL_CAT_SCOTT_BURGAN + 1))
      this%savr = 1.0
      do i = 1, N_FUEL_CAT_SCOTT_BURGAN + 1
        w = this%Component_loads(i, this%curing_none)
        ! Total consumable oven-dry inventory [kg m-2], invariant under curing.
        this%fgi(i) = real(sum(w) * CONVERT_TON_PER_ACRE_TO_KG_PER_M_SQ)
        sigma = [this%savr_1h(i),this%savr_10h(i),this%savr_100h(i),this%savr_lh(i),this%savr_lh(i),this%savr_live_woody(i)]
        if (sum(w) > 0.0) this%savr(i) = sum(w*sigma*sigma)/sum(w*sigma)
      end do

      call Require_table(all(this%fgi(:this%n_fuel_cat) > 0), 'burnable fuel load must be positive')
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

    pure real function Cured_fraction(this, mc_live) result(fraction)
      real, intent(in) :: mc_live
      class(fuel_scott_burgan_t), intent(in) :: this
      ! Scott and Burgan: fully cured at 30%, no curing at 120% live moisture.
      fraction = max(0.0, min(1.0, (this%curing_none - mc_live)/this%curing_span))
    end function

    pure function Component_loads(this, k, mc_live) result(loads)
      class(fuel_scott_burgan_t), intent(in) :: this
      integer, intent(in) :: k
      real, intent(in) :: mc_live
      real :: loads(6), cured
      cured = this%Cured_fraction(mc_live) * this%fgi_lh(k)
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
