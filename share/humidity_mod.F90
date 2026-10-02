  module humidity_mod

    use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
    use stderrout_mod, only : Stop_simulation

    implicit none

    private

    public :: Mixing_ratio_to_specific_humidity, Specific_humidity_to_mixing_ratio, &
        Vapor_pressure_from_mixing_ratio, Is_valid_mixing_ratio, Is_valid_specific_humidity, Validate_mixing_ratio_2m

    real, parameter :: EPSILON_WATER_DRY_AIR = 0.622

  contains

    elemental function Is_valid_mixing_ratio (r2) result (is_valid)

      implicit none

      real, intent (in) :: r2
      logical :: is_valid


      is_valid = ieee_is_finite (r2) .and. r2 >= 0.0

    end function Is_valid_mixing_ratio

    elemental function Is_valid_specific_humidity (q2) result (is_valid)

      implicit none

      real, intent (in) :: q2
      logical :: is_valid


      is_valid = ieee_is_finite (q2) .and. q2 >= 0.0 .and. q2 < 1.0

    end function Is_valid_specific_humidity

    elemental function Mixing_ratio_to_specific_humidity (r2) result (q2)

      implicit none

      real, intent (in) :: r2
      real :: q2


      q2 = r2 / (1.0 + r2)

    end function Mixing_ratio_to_specific_humidity

    elemental function Specific_humidity_to_mixing_ratio (q2) result (r2)

      implicit none

      real, intent (in) :: q2
      real :: r2


      r2 = q2 / (1.0 - q2)

    end function Specific_humidity_to_mixing_ratio

    elemental function Vapor_pressure_from_mixing_ratio (r2, pressure) result (vapor_pressure)

      implicit none

      real, intent (in) :: r2, pressure
      real :: vapor_pressure


      vapor_pressure = r2 * pressure / (EPSILON_WATER_DRY_AIR + r2)

    end function Vapor_pressure_from_mixing_ratio

    subroutine Validate_mixing_ratio_2m (r2, ifms, ifme, jfms, jfme, ifps, ifpe, jfps, jfpe)

      implicit none

      integer, intent (in) :: ifms, ifme, jfms, jfme, ifps, ifpe, jfps, jfpe
      real, dimension(ifms:ifme, jfms:jfme), intent (in) :: r2

      integer :: i, j


      do j = jfps, jfpe
        do i = ifps, ifpe
          if (.not. Is_valid_mixing_ratio (r2(i, j))) &
              call Stop_simulation ('2 m water-vapor mixing ratio must be finite and nonnegative')
        end do
      end do

    end subroutine Validate_mixing_ratio_2m

  end module humidity_mod
