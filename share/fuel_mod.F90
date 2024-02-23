  module fuel_mod

    implicit none

    private
    
    public :: fuel_t, FUEL_ANDERSON

    integer, parameter :: FUEL_ANDERSON = 1

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
    contains
      procedure (Initialization), deferred :: Initialization
    end type fuel_t

    abstract interface
      subroutine Initialization (this, fuelmc_c) 
        import :: fuel_t
        class (fuel_t), intent (in out) :: this
        real, intent(in) :: fuelmc_c
      end subroutine Initialization
    end interface

  end module fuel_mod
