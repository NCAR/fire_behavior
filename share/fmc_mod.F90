  module fmc_mod

    use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
    use fuel_mod, only : fuel_t
    use ros_mod, only : ros_t
    use stderrout_mod, only : Stop_simulation

    implicit none

    private
    
    public :: fmc_t, FMC_WRFFIRE, Check_fmc

    integer, parameter :: FMC_WRFFIRE = -1

    type, abstract :: fmc_t
    contains
      procedure (Advance_fmc_model), deferred :: Advance_fmc_model
      procedure (Add_restart_vars), deferred :: Add_restart_vars
      procedure (Init), deferred :: Init
    end type fmc_t

    abstract interface
      subroutine Add_restart_vars (this, file_restart, cfbm_comm, nx, ny, ifps, ifpe, jfps, jfpe, rank)
        import :: fmc_t
        class (fmc_t), intent (in) :: this
        character (len = *), intent (in) :: file_restart
        integer, intent (in) :: cfbm_comm
        integer, intent (in) :: nx
        integer, intent (in) :: ny
        integer, intent (in) :: ifps
        integer, intent (in) :: ifpe
        integer, intent (in) :: jfps
        integer, intent (in) :: jfpe
        integer, intent (in) :: rank
      end subroutine Add_restart_vars

      subroutine Init (this, fuels, fuelmc_g, fuelmc_g_live, ifms, ifme, jfms, jfme, itimestep, dt)
        import :: fmc_t, fuel_t
        class (fmc_t), intent (in out) :: this
        class (fuel_t), intent (in) :: fuels
        real, intent (in) :: fuelmc_g, fuelmc_g_live, dt
        integer, intent (in) :: ifms, ifme, jfms, jfme, itimestep
      end subroutine Init

      subroutine Advance_fmc_model (this, fmoist_freq, fmoist_dt, itimestep, dt, ifms, ifme, jfms, jfme, i_start, i_end, &
            j_start, j_end, num_tiles, fire_rain, fire_t2, fire_q2, fire_psfc, fire_rain_old, fire_t2_old, fire_q2_old, &
            fire_psfc_old, fire_rh_fire, fuelmc_g, fmc_g, nfuel_cat, fuels, ros_param)
        import :: fmc_t, fuel_t, ros_t
        class (fmc_t), intent (in out) :: this
        class (fuel_t), intent (in) :: fuels
        class (ros_t), intent (in out) :: ros_param
        integer, intent (in) :: fmoist_freq, itimestep, ifms, ifme, jfms, jfme, num_tiles
        integer, dimension(num_tiles) :: i_start, i_end, j_start, j_end
        real, intent (in) ::  fmoist_dt, dt, fuelmc_g
        real, dimension (ifms:ifme, jfms:jfme), intent (in) :: nfuel_cat
        real, dimension (ifms:ifme, jfms:jfme), intent (in out) :: fire_rain, fire_t2, fire_q2, fire_psfc, fire_rain_old, &
            fire_t2_old, fire_q2_old, fire_psfc_old, fire_rh_fire, fmc_g
      end subroutine Advance_fmc_model
    end interface

  contains

    subroutine Check_fmc (ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, nfuel_cat, no_fuel_cat, fmc_g, source)

      implicit none

      integer, intent (in) :: ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, no_fuel_cat
      real, dimension(ifms:ifme, jfms:jfme), intent (in) :: nfuel_cat, fmc_g
      character (len = *), intent (in) :: source
      integer :: i, j
      character (len = 160) :: msg


      ! Check computed or restored moisture before it reaches spread physics.
      ! Only owned burnable cells require moisture; halo values may be unavailable.
      do j = jfts, jfte
        do i = ifts, ifte
          if (int (nfuel_cat(i, j)) == no_fuel_cat) cycle
          if (.not. ieee_is_finite (fmc_g(i, j))) then
            write (msg, '(a,a,2(1x,i0))') source, ' fmc_g must be finite at i,j:', i, j
            call Stop_simulation (msg)
          else if (fmc_g(i, j) < 0.0) then
            write (msg, '(a,a,2(1x,i0))') source, ' fmc_g must be nonnegative at i,j:', i, j
            call Stop_simulation (msg)
          end if
        end do
      end do

    end subroutine Check_fmc

  end module fmc_mod
