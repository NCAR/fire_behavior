  module fuel_anderson_mod

    ! =============================================================================
    ! Anderson 13 surface fire fuel models, along with some
    !          estimated canopy properties (for crown fire).
    ! =============================================================================
    !  --- Grass-dominated fuel models
    !  FUEL MODEL 1: Short grass (1 ft)
    !  FUEL MODEL 2: Timber (grass and understory)
    !  FUEL MODEL 3: Tall grass (2.5 ft)
    !  --- Shrub-dominated fuel models
    !  FUEL MODEL 4: Chaparral (6 ft)
    !  FUEL MODEL 5: Brush (2 ft)
    !  FUEL MODEL 6: Dormant brush, hardwood slash
    !  FUEL MODEL 7: Southern rough
    !  --- Forest litter-dominated fuel models
    !  FUEL MODEL 8: Closed timber litter
    !  FUEL MODEL 9: Hardwood litter
    !  FUEL MODEL 10: Timber (litter + understory)
    !  --- Logging debris-dominated fuel models
    !  FUEL MODEL 11: Light logging slash
    !  FUEL MODEL 12: Medium logging slash
    !  FUEL MODEL 13: Heavy logging slash
    !  --- Fuel-free
    !  FUEL MODEL 14: no fuel
    ! =============================================================================

    use fuel_mod, only : fuel_t, Crosswalk_from_scottburgan_to_anderson

    use fuel_table_mod, only: fuel_table_t, Read_fuel_table, Require_table

    implicit none

    private

    public :: fuel_anderson_t

    integer, parameter :: N_FUEL_CAT_ANDERSON = 13, NO_FUEL_CAT_ANDERSON = 14

    type, extends (fuel_t) :: fuel_anderson_t
      character (len = 80), dimension(N_FUEL_CAT_ANDERSON + 1) :: fuel_name
        ! Canopy
        ! fct: burn out time for canopy fuel, after dry [s]
      real, dimension(N_FUEL_CAT_ANDERSON + 1) :: fct
        ! fci_d: initial dry mass of canopy fuel
        ! ----- 1.12083 is 5 tons/acre.  5-50 tons/acre orig., 100-300 after blowdown
      real, dimension(N_FUEL_CAT_ANDERSON + 1) :: fci_d
      real, dimension(N_FUEL_CAT_ANDERSON + 1) :: windrf
        ! fci: initial total mass of canopy fuel
        ! fcbr: fuel canopy burn rate (kg/m2/s)
      real, dimension(N_FUEL_CAT_ANDERSON + 1) :: fcbr, fci
    contains
      procedure :: Resolve_fuel_index => Resolve_anderson_index
      procedure :: Calc_effective_dead_load => Anderson_dead_load
       procedure, public :: Initialization => Init_anderson_fuel_model
    end type fuel_anderson_t

  contains

    pure integer function Resolve_anderson_index(this, external_code) result(idx)
      class(fuel_anderson_t), intent(in) :: this
      integer, intent(in) :: external_code
      ! The crosswalk is degraded compatibility, not native SB40 physics.
      select case(external_code)
      case(14,91,92,93,98,99)
        idx = this%no_fuel_cat
      case default
        idx = Crosswalk_from_scottburgan_to_anderson(external_code)
      end select
    end function

    pure real function Anderson_dead_load(this, k, mc_live) result(load)
      class(fuel_anderson_t), intent(in) :: this
      integer, intent(in) :: k
      real, intent(in) :: mc_live
      load = this%fgi(k)
    end function

   subroutine Init_anderson_fuel_model (this, fuelmc_c, filename, comm)

      implicit none

      class (fuel_anderson_t), intent(in out) :: this
      real, intent (in) :: fuelmc_c
      character(len=*), intent(in), optional :: filename
      integer, intent(in), optional :: comm
      type(fuel_table_t) :: table

      integer :: i


      this%n_fuel_cat = N_FUEL_CAT_ANDERSON
      this%no_fuel_cat = NO_FUEL_CAT_ANDERSON

      call Read_fuel_table('ANDERSON13', &
          [(i,i=1,N_FUEL_CAT_ANDERSON+1)], &
          'code fgi[kg/m2] depth[m] weight[s] ichap[1] density[lb/ft3] st[1] se[1] savr[1/ft] ' // &
          'extinction[kg/kg] dead1h[ton/ac] dead10h[ton/ac] dead100h[ton/ac] dead1000h[ton/ac] live[ton/ac] ' // &
          'canopy[kg/m2] canopy_time[s] windrf[1] name', table, filename, comm)
      this%moisture = table%moisture
      this%table_fingerprint = table%fingerprint
      this%legacy_table_compatible = table%legacy_compatible
      this%fuel_name = table%names
      this%fgi = table%properties(1,:)
      this%fueldepthm = table%properties(2,:)
      this%weight = table%properties(3,:)
      call Require_table(all(table%properties(4,:) == 0 .or. table%properties(4,:) == 1), 'ichap must be 0 or 1')
      this%ichap = int(table%properties(4,:))
      this%fueldens = table%properties(5,:)
      this%st = table%properties(6,:)
      this%se = table%properties(7,:)
      this%savr = table%properties(8,:)
      this%fuelmce = table%properties(9,:)
      this%fgi_1h = table%properties(10,:)
      this%fgi_10h = table%properties(11,:)
      this%fgi_100h = table%properties(12,:)
      this%fgi_1000h = table%properties(13,:)
      this%fgi_live = table%properties(14,:)
      this%fci_d = table%properties(15,:)
      this%fct = table%properties(16,:)
      this%windrf = table%properties(17,:)
      call Require_table(all(this%fueldepthm > 0), 'fuel depth must be positive')
      call Require_table(all(this%weight > 0), 'consumption weight must be positive')
      call Require_table(all(this%fueldens > 0), 'particle density must be positive')
      call Require_table(all(this%st < 1), 'total mineral fraction must be below one')
      call Require_table(all(this%se > 0 .and. this%se <= this%st), 'invalid effective mineral fraction')
      call Require_table(all(this%fuelmce > 0), 'dead extinction moisture must be positive')
      call Require_table(all(this%savr > 0), 'SAV must be positive')
      call Require_table(all(this%Moisture_loads(this%no_fuel_cat,0.0) == 0), 'nonburnable component load must be zero')
      call Require_table(this%fgi(this%no_fuel_cat) <= 1.e-7, 'Anderson no-fuel sentinel exceeds legacy value')
      call Require_table(this%fci_d(this%no_fuel_cat) == 0, 'nonburnable canopy load must be zero')
      call Require_table(all(this%fgi(:this%n_fuel_cat) > 0), 'burnable fuel load must be positive')

      this%fci = 0.0
      this%fcbr = 0.0
      do i = 1, N_FUEL_CAT_ANDERSON
        this%fci(i) = (1.0 + fuelmc_c) * this%fci_d(i)
        if(this%fct(i) /= 0.0) then
          this%fcbr(i) = this%fci_d(i) / this%fct(i)
        else
          this%fcbr(i) = 0.0
        end if
      end do

      call this%Check_fuels ()

      ! Initialize wind adjustment factor
      call this%Calc_wind_adjustment_factor()

    end subroutine Init_anderson_fuel_model

  end module fuel_anderson_mod

