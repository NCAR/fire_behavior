  module fuel_mod

    implicit none

    private

    public :: fuel_t, FUEL_ANDERSON, FUEL_SCOTT_BURGAN, UNKNOWN_FUEL_CAT, Crosswalk_from_scottburgan_to_anderson

    ! In SB: explicit nonburnables map to no-fuel, unknown codes remain 0 and abort
    integer, parameter :: FUEL_ANDERSON = 1, FUEL_SCOTT_BURGAN = 2, UNKNOWN_FUEL_CAT = 0

    type, abstract :: fuel_t
       integer :: n_fuel_cat, no_fuel_cat
        ! total fuel loading kg/m2
        ! fgi: initial total mass of surface fuel [kg/m2]
        !     ranges from ~5 (fast burnup) to 1000 ( ~40% decr over 10 min) ????
      real, dimension(:), allocatable :: fgi
        ! fueldepthm: fuel depth [m]  (in feet in Anderson 1982)
      real, dimension(:), allocatable :: fueldepthm
        ! weight: weighting parameter that determines the slope of the mass loss curve
        ! ----- Notes on weight: (4) - best fit of data from D. Latham (pers. comm.);
        !              (5)-(7) could be 60-120; (8)-(10) could be 300-1600;
        !              (11)-(13) could be 300-1600
      real, dimension(:), allocatable :: weight
        ! ichap: set=1 if fuel is chaparral and want the rate of spread treated differently, 0 if not
      integer, dimension(:), allocatable :: ichap
        ! fueldens: ovendry particle density [lb/ft3]
      real, dimension(:), allocatable :: fueldens
        ! savr: fuel particle surface-area-to-volume ratio, [1/ft]
        !       grass: 3500., 10 hr fuel: 109., 100 hr fuel: 30.
      real, dimension(:), allocatable :: savr
        ! st: fuel particle total mineral content
      real, dimension(:), allocatable :: st
        ! se: fuel particle effective mineral content
      real, dimension(:), allocatable :: se
        ! fuelmce: moisture content of extinction
      real, dimension(:), allocatable :: fuelmce
        ! fuel loading 1-h, 10-h, 100-h, 1000-h, and live  [ton/acre]
      real, dimension(:), allocatable :: fgi_1h, fgi_10h, fgi_100h, fgi_1000h, fgi_live
        ! live herbaceous load [kg m-2] used by Tier A SB40 curing
      real, dimension(:), allocatable :: fgi_lh
        ! reserved Tier B live-fuel fields; Tier A retains the WRF-Fire aggregate ROS formulation
      real, dimension(:), allocatable :: fgi_live_woody, savr_live, fuelmce_live
        ! fuel particle low heat content [BTU lb-1]
      real, dimension(:), allocatable :: fuelheat
        ! wind adjustment factor
      real, dimension(:), allocatable :: waf
    contains
      procedure (Initialization), deferred :: Initialization
      procedure, public :: Calc_wind_adjustment_factor => Calc_wind_adjustment_factor
      procedure, public :: Effective_dead_load => Effective_dead_load
      procedure, public :: Resolve_fuel_index => Resolve_fuel_index_default
    end type fuel_t

    abstract interface
      subroutine Initialization (this, fuelmc_c)
        import :: fuel_t
        class (fuel_t), intent (in out) :: this
        real, intent(in) :: fuelmc_c
      end subroutine Initialization
    end interface

  contains

    pure function Crosswalk_from_scottburgan_to_anderson (fuel_cat) result (return_value)

      implicit none

      integer, intent (in) :: fuel_cat

      integer :: return_value


      select case (fuel_cat)
        case (1, 2, 3, 4, 5 ,6, 7, 8, 9, 10, 11, 12, 13, 14)
            ! notheing to do
          return_value = fuel_cat

        case (101, 104, 107)
             ! Scott & Burgan crosswalks
             ! Short grass -- 1
           return_value = 1

        case (102, 121, 122, 123, 124)
            ! Timber grass and understory -- 2
          return_value = 2

        case (103, 105, 106, 108, 109)
            ! Tall grass -- 3
          return_value = 3

        case (145, 147)
            ! Chaparral -- 4
          return_value = 4

        case (142)
            ! Brush -- 5
          return_value = 5

        case (141, 146)
            ! Dormant Brushi -- 6
          return_value = 6

        case (143, 144, 148, 149)
            ! Southern Rough -- 7
          return_value = 7

        case (181, 183, 184, 187)
            ! Compact Timber Litter -- 8
          return_value = 8

        case (182, 186, 188, 189)
            ! Hardwood Litter -- 9
          return_value = 9

        case (161, 162, 163, 164, 165)
            ! Timber (understory) -- 10
          return_value = 10

        case (185, 201)
            ! Light Logging Slash -- 11
          return_value = 11

        case (202)
            ! Medium Logging Slash -- 12
          return_value = 12

        case (203, 204)
            ! Heavy Logging Slash -- 13
          return_value = 13

        case default
          return_value = UNKNOWN_FUEL_CAT

        end select

    end function Crosswalk_from_scottburgan_to_anderson

    pure function Effective_dead_load (this, k, mc_lh) result (load)

      implicit none

      class (fuel_t), intent (in) :: this
      integer, intent (in) :: k
      real, intent (in) :: mc_lh

      real :: load


      ! Scott and Burgan dynamic herbaceous curing transfers live herbaceous
      ! load to the dead-equivalent pool below 120% live FMC, with complete
      ! transfer at 30%. Tier A applies that cured fraction to the aggregate
      ! WRF-Fire ROS load only; untransferred herbaceous fuel, live woody fuel,
      ! and dynamic live moisture of extinction require Tier B two-component
      ! Rothermel treatment.
      if (this%fgi_lh(k) == 0.0) then
        load = this%fgi(k)
      else if (mc_lh >= 1.20) then
        load = this%fgi(k)
      else if (mc_lh <= 0.30) then
        load = this%fgi(k) + this%fgi_lh(k)
      else
        load = this%fgi(k) + (1.0 - (mc_lh - 0.30) / 0.90) * this%fgi_lh(k)
      end if

    end function Effective_dead_load

    pure function Resolve_fuel_index_default (this, external_code) result (idx)

      implicit none

      class (fuel_t), intent (in) :: this
      integer, intent (in) :: external_code

      integer :: idx


      idx = UNKNOWN_FUEL_CAT

    end function Resolve_fuel_index_default

    subroutine Calc_wind_adjustment_factor (this)

      implicit none

      class (fuel_t), intent (in out) :: this

      ! local
      integer :: i
      real :: wh, flamelength, h
      real :: HF ! extent of the flame above the vegetation


      ! The validated physics index can be the no-fuel row. Allocate through
      ! n_fuel_cat + 1 so Apply_wafs and the NUOPC wind path remain branch-free
      ! and have a valid WAF entry for no-fuel cells in both Anderson and SB40.
      allocate (this%waf(this%n_fuel_cat + 1))
      wh = 10.0 ! input wind height in meter

      do i = 1,this%n_fuel_cat + 1
        h = this%fueldepthm(i)
        flamelength = 2.0 * h ! assume flamelength is double the fuel bed height
        if (flamelength > h) then
          hf = max(flamelength - h, 0.13 * h)
          this%waf(i) = (1.0 + 0.36 * h / hf) / log(((wh-h) + 0.36 * h) / (0.13 * h)) * (log( (hf/h + 0.36)/0.13) - 1.0)
        else
          this%waf(i) = 0.15
        endif
      end do
      this%waf(this%no_fuel_cat) = 1.e-7

    end subroutine Calc_wind_adjustment_factor

  end module fuel_mod
