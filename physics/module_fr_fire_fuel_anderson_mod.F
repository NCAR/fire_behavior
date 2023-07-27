module module_fr_fire_fuel_anderson_mod

    use module_fr_fire_fuel_moisture_model, only: nfuelcats

    implicit none

    private

    public :: fuel_anderson_t, Init_anderson_fuel_model

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

   type :: fuel_anderson_t
      ! fuel categorytables
      CHARACTER (len=80), DIMENSION(:), allocatable :: fuel_name
      INTEGER, DIMENSION(:), allocatable :: ichap
      REAL   , DIMENSION(:), allocatable :: windrf,weight,fci_d,fct,    &
                                            fueldepthm,fueldens,fuelmce,   &
                                            savr,st,se
   end type fuel_anderson_t

  contains

   subroutine Init_anderson_fuel_model (this)

      implicit none

      class (fuel_anderson_t), intent(in out) :: this

      allocate (this%fuel_name(nfuelcats+1))
      allocate (this%windrf(nfuelcats+1))
      allocate (this%fueldepthm(nfuelcats+1))
      allocate (this%savr(nfuelcats+1))
      allocate (this%fuelmce(nfuelcats+1))
      allocate (this%fueldens(nfuelcats+1))
      allocate (this%st(nfuelcats+1))
      allocate (this%se(nfuelcats+1))
      allocate (this%weight(nfuelcats+1))
      allocate (this%fci_d(nfuelcats+1))
      allocate (this%fct(nfuelcats+1))
      allocate (this%ichap(nfuelcats+1))

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

      this%windrf(1:nfuelcats+1) = (/ 0.36, 0.36, 0.44,  0.55,  0.42,  0.44,  0.44,  &
                                      0.36, 0.36, 0.36,  0.36,  0.43,  0.46,  1.0e-7 /)
      this%fueldepthm(1:nfuelcats+1) = (/ 0.305,  0.305,  0.762, 1.829, 0.61, 0.762, 0.762, &
                                          0.0610, 0.0610, 0.305, 0.305, 0.701, 0.914, 0.305 /)
      this%savr(1:nfuelcats+1) = (/ 3500., 2784., 1500., 1739., 1683., 1564., 1562.,  &
                                    1889., 2484., 1764., 1182., 1145., 1159., 3500.   /)
      this%fuelmce(1:nfuelcats+1) = (/ 0.12, 0.15, 0.25, 0.20, 0.20, 0.25, 0.40,  &
                                       0.30, 0.25, 0.25, 0.15, 0.20, 0.25, 0.12   /)
      this%fueldens(1:nfuelcats+1) = (/ 32., 32., 32., 32., 32., 32., 32., &
                                        32., 32., 32., 32., 32., 32., 32.  /)   ! 32 if solid, 19 if rotten.
      this%st(1:nfuelcats+1) = (/ 0.0555, 0.0555, 0.0555, 0.0555, 0.0555, 0.0555, 0.0555, &
                                  0.0555, 0.0555, 0.0555, 0.0555, 0.0555, 0.0555, 0.0555  /)
      this%se(1:nfuelcats+1) = (/ 0.010, 0.010, 0.010, 0.010, 0.010, 0.010, 0.010, &
                                  0.010, 0.010, 0.010, 0.010, 0.010, 0.010, 0.010  /)
   ! ----- Notes on weight: (4) - best fit of data from D. Latham (pers. comm.);
   !              (5)-(7) could be 60-120; (8)-(10) could be 300-1600;
   !              (11)-(13) could be 300-1600
      this%weight(1:nfuelcats+1) = (/ 7.,  7.,  7., 180., 100., 100., 100.,   &
                                      900., 900., 900., 900., 900., 900., 7.  /)
   ! ----- 1.12083 is 5 tons/acre.  5-50 tons/acre orig., 100-300 after blowdown
      this%fci_d(1:nfuelcats+1) = (/ 0., 0., 0., 1.123, 0., 0., 0.,               &
                                     1.121, 1.121, 1.121, 1.121, 1.121, 1.121, 0. /)
      this%fct(1:nfuelcats+1) = (/ 60., 60., 60., 60., 60., 60., 60.,      &
                                   60., 120., 180., 180., 180., 180. , 60. /)
      this%ichap(1:nfuelcats+1) = (/ 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)

   end subroutine Init_anderson_fuel_model

end module module_fr_fire_fuel_anderson_mod
