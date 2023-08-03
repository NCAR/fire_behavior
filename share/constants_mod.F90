  module constants_mod

    implicit none

    private

    public :: R_D, CP, XLV, PI, CMBCNST, CONVERT_J_PER_KG_TO_BTU_PER_PUND

    real, parameter :: R_D = 287.0                ! gas constant of dry air [J deg-1 kg-1]
    real, parameter :: CP = 7.0 * R_D / 2.0
    real, parameter :: XLV = 2.5E6                ! latent heat of vaporization of water at 0 degrees C [J kg-1]

    real, parameter :: PI = 3.1415926

    real, parameter :: CMBCNST = 17.433e+06       ! cmbcnst: joules per kg of dry fuel
    real, parameter :: CONVERT_J_PER_KG_TO_BTU_PER_PUND = 4.30e-04 ! 1 / 2326


  end module constants_mod
