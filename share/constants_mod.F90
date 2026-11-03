  module constants_mod

    use, intrinsic :: iso_fortran_env, only : REAL64

    implicit none

    private

    public :: CONVERT_TON_PER_ACRE_TO_KG_PER_M_SQ
    public :: R_D, CP, XLV, PI, CMBCNST, CONVERT_J_PER_KG_TO_BTU_PER_POUND, FVIRT, G

    real, parameter :: R_D = 287.0      ! gas constant of dry air [J K-1 kg-1]
    real, parameter :: R_V = 461.6      ! gas constant for water vapor [J k-1 kg-1]
    real, parameter :: EPSI = R_D / R_V
    real, parameter :: FVIRT = 1.0 / EPSI - 1.0
    real, parameter :: CP = 7.0 * R_D / 2.0
    real, parameter :: XLV = 2.5E6      ! latent heat of vaporization of water at 0 degrees C [J kg-1]

    real, parameter :: G = 9.80665      ! standard gravity [m s-2]

    real, parameter :: PI = 3.1415926

    ! US short ton and international acre, using exact lb and ft definitions.
    real(REAL64), parameter :: CONVERT_TON_PER_ACRE_TO_KG_PER_M_SQ = &
        2000.0_REAL64 * 0.45359237_REAL64 / (43560.0_REAL64 * 0.3048_REAL64**2)

    real, parameter :: CMBCNST = 17.433e+06       ! cmbcnst: joules per kg of dry fuel
    real, parameter :: CONVERT_J_PER_KG_TO_BTU_PER_POUND = 4.30e-04 ! 1 / 2326

  end module constants_mod
