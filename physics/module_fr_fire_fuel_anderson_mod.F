  module module_fr_fire_fuel_anderson_mod

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

    !  Data arrays indexed by fuel category:
    !       FGI            INITIAL TOTAL MASS OF SURFACE FUEL (KG/M**2)
    !       FUELDEPTHM     FUEL DEPTH, IN M  (CONVERTED TO FT)              
    !       SAVR           FUEL PARTICLE SURFACE-AREA-TO-VOLUME RATIO, 1/FT
    !                         GRASS: 3500., 10 hr fuel: 109.,  100 hr fuel: 30.
    !       FUELMCE        MOISTURE CONTENT OF EXTINCTION; 0.30 FOR MANY DEAD FUELS; 0.15 FOR GRASS
    !       FUELDENS       OVENDRY PARTICLE DENSITY, LB/FT^3
    !       ST             FUEL PARTICLE TOTAL MINERAL CONTENT
    !       SE             FUEL PARTICLE EFFECTIVE MINERAL CONTENT
    !       WEIGHT         WEIGHTING PARAMETER THAT DETERMINES THE SLOPE OF THE MASS LOSS CURVE
    !                        RANGES FROM ~5 (FAST BURNUP) TO 1000 ( ~40% DECR OVER 10 MIN).
    !       FCI_D          INITIAL DRY MASS OF CANOPY FUEL
    !       FCT            BURN OUT TIME FOR CANOPY FUEL, AFTER DRY (S)
    !       ichap          Set=1 if fuel is chaparral and want the rate of spread treated differently, 0 if not
    !D      FCI            INITIAL TOTAL MASS OF CANOPY FUEL
    !D      FCBR           FUEL CANOPY BURN RATE (KG/M**2/S)
    ! =============================================================================

    implicit none

    private

    public :: fuel_anderson_t, N_FUEL_CAT, NO_FUEL_CAT

    integer, parameter :: N_FUEL_CAT = 13, NO_FUEL_CAT = 14

    type :: fuel_anderson_t
      character (len = 80), dimension(N_FUEL_CAT + 1) :: fuel_name
      real, dimension(N_FUEL_CAT + 1) :: fueldepthm = [ 0.305,  0.305,  0.762, 1.829, 0.61, 0.762, 0.762, &
                                                        0.0610, 0.0610, 0.305, 0.305, 0.701, 0.914, 0.305 ]
      real, dimension(N_FUEL_CAT + 1) :: fuelmce = [ 0.12, 0.15, 0.25, 0.20, 0.20, 0.25, 0.40, &
                                                     0.30, 0.25, 0.25, 0.15, 0.20, 0.25, 0.12 ]
      real, dimension(N_FUEL_CAT + 1) :: savr = [ 3500., 2784., 1500., 1739., 1683., 1564., 1562.,  &
                                                  1889., 2484., 1764., 1182., 1145., 1159., 3500. ]
      real, dimension(N_FUEL_CAT + 1) :: fueldens = [ 32., 32., 32., 32., 32., 32., 32., &
                                                      32., 32., 32., 32., 32., 32., 32. ]
      real, dimension(N_FUEL_CAT + 1) :: st = [ 0.0555, 0.0555, 0.0555, 0.0555, 0.0555, 0.0555, 0.0555, &
                                                0.0555, 0.0555, 0.0555, 0.0555, 0.0555, 0.0555, 0.0555 ]
      real, dimension(N_FUEL_CAT + 1) :: se = [ 0.010, 0.010, 0.010, 0.010, 0.010, 0.010, 0.010, &
                                                0.010, 0.010, 0.010, 0.010, 0.010, 0.010, 0.010 ]
          ! ----- Notes on weight: (4) - best fit of data from D. Latham (pers. comm.);
          !              (5)-(7) could be 60-120; (8)-(10) could be 300-1600;
          !              (11)-(13) could be 300-1600
      real, dimension(N_FUEL_CAT + 1) :: weight = [ 7.,  7.,  7., 180., 100., 100., 100., &
                                                    900., 900., 900., 900., 900., 900., 7. ]
      integer, dimension(N_FUEL_CAT + 1) :: ichap = [ 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
       ! Canopy
      real, dimension(N_FUEL_CAT + 1) :: fct = [ 60., 60., 60., 60., 60., 60., 60., &
                                                 60., 120., 180., 180., 180., 180. , 60. ]
          ! ----- 1.12083 is 5 tons/acre.  5-50 tons/acre orig., 100-300 after blowdown
      real, dimension(N_FUEL_CAT + 1) :: fci_d = [ 0., 0., 0., 1.123, 0., 0., 0., &
                                                   1.121, 1.121, 1.121, 1.121, 1.121, 1.121, 0. ]
      real, dimension(N_FUEL_CAT + 1) :: windrf = [ 0.36, 0.36, 0.44,  0.55,  0.42,  0.44,  0.44,  &
                                                    0.36, 0.36, 0.36,  0.36,  0.43,  0.46,  1.0e-7 ]
    contains
       procedure, public :: Initialization => Init_anderson_fuel_model
    end type fuel_anderson_t

  contains

   subroutine Init_anderson_fuel_model (this)

      implicit none

      class (fuel_anderson_t), intent(in out) :: this


      this%fuel_name(1)  = '1: Short grass (1 ft)'
      this%fuel_name(2)  = '2: Timber (grass and understory)'
      this%fuel_name(3)  = '3: Tall grass (2.5 ft)'
      this%fuel_name(4)  = '4: Chaparral (6 ft)'
      this%fuel_name(5)  = '5: Brush (2 ft) '
      this%fuel_name(6)  = '6: Dormant brush, hardwood slash'
      this%fuel_name(7)  = '7: Southern rough'
      this%fuel_name(8)  = '8: Closed timber litter'
      this%fuel_name(9)  = '9: Hardwood litter'
      this%fuel_name(10) = '10: Timber (litter + understory)'
      this%fuel_name(11) = '11: Light logging slash'
      this%fuel_name(12) = '12: Medium logging slash'
      this%fuel_name(13) = '13: Heavy logging slash'
      this%fuel_name(14) = '14: no fuel'

    end subroutine Init_anderson_fuel_model

  end module module_fr_fire_fuel_anderson_mod

