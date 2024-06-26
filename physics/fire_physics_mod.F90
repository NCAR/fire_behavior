
  module fire_physics_mod

    use constants_mod, only : XLV, CMBCNST
    use state_mod, only: state_fire_t
    use namelist_mod, only: namelist_t

    private

    public :: Calc_fire_fluxes, Calc_flame_length, Calc_smoke_emissions

      ! hfgl: surface fire heat flux threshold to ignite canopy [W/m2]
!    real, parameter :: HFGL = 17.0e4

  contains

    subroutine Calc_fire_fluxes (dt, grid, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, &
      iffs, iffe, jffs, jffe, fuel_load_g, fuel_frac_burnt_dt, grnhft, grnqft)

      implicit none

      type (state_fire_t), target :: grid
      real, intent(in) :: dt
      integer, intent(in) :: ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, iffs, iffe, jffs, jffe
      real, dimension(ifms:ifme, jfms:jfme), intent (in) :: fuel_load_g, fuel_frac_burnt_dt
      real, dimension(ifms:ifme, jfms:jfme), intent (out) :: grnhft, grnqft

      integer :: i, j
      real :: dmass, bmst


      do j = jfts, jfte
        do i = ifts, ifte
            ! surface fuel dry mass burnt this call [kg/m2]
          dmass = fuel_load_g(i, j) * fuel_frac_burnt_dt(i, j)
          bmst = grid%fmc_g(i, j)/(1.0 + grid%fmc_g(i, j))
            ! surface fire sensible heat flux [W/m2]
          grnhft(i, j) = (dmass / dt) * (1.0 - bmst) * CMBCNST
            ! surface fire latent heat flux [W/m2]
            ! Assume 56% of cellulose molecule mass is water.
          grnqft(i, j) = (bmst + (1.0 - bmst) * 0.56) * (dmass / dt) * XLV
        end do
      end do

    end subroutine Calc_fire_fluxes

    subroutine Calc_flame_length (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, &
        ros, iboros, flame_length, ros_fl, fire_area)

      ! flame length according to Byram (1959)

      implicit none

      integer, intent(in) :: ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme
      real, dimension(ifms:ifme, jfms:jfme), intent(in) :: ros, iboros, fire_area
      real, dimension(ifms:ifme, jfms:jfme), intent(out) :: flame_length, ros_fl

      integer :: i, j


      do j = jfts, jfte
        do i = ifts, ifte
          if (fire_area(i, j) > 0.0 .and. fire_area(i, j) < 1.0) then
            flame_length(i, j) = 0.0775 * (iboros(i, j) * ros(i, j)) ** 0.46 
            ros_fl(i, j) = ros(i, j)
          else
            flame_length(i, j) = 0.0
            ros_fl(i, j) = 0.0
          end if
        end do
      end do

    end subroutine Calc_flame_length

    subroutine Calc_smoke_emissions (grid, config_flags, ifts, ifte, jfts, jfte)

      implicit none

      type (state_fire_t), intent(in out) :: grid
      type (namelist_t), intent(in) :: config_flags
      integer, intent(in) :: ifts, ifte, jfts, jfte

      integer :: i, j


      do j = jfts, jfte
        do i = ifts, ifte
          grid%emis_smoke(i, j) = config_flags%frac_fburnt_to_smoke * grid%fuel_frac_burnt_dt(i, j) * grid%fuel_load_g(i, j) ! kg/m^2
        end do
      end do

    end subroutine Calc_smoke_emissions

  end module fire_physics_mod

