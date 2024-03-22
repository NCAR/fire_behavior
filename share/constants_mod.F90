  module constants_mod

    implicit none

    private

    public :: R_D, CP, XLV, PI, CMBCNST, CONVERT_J_PER_KG_TO_BTU_PER_POUND
    public :: con_cp, con_rd, con_rv, con_g, con_fvirt

    real, parameter :: R_D = 287.0                ! gas constant of dry air [J deg-1 kg-1]
    real, parameter :: CP = 7.0 * R_D / 2.0
    real, parameter :: XLV = 2.5E6                ! latent heat of vaporization of water at 0 degrees C [J kg-1]

    real, parameter :: PI = 3.1415926

    real, parameter :: CMBCNST = 17.433e+06       ! cmbcnst: joules per kg of dry fuel
    real, parameter :: CONVERT_J_PER_KG_TO_BTU_PER_POUND = 4.30e-04 ! 1 / 2326

    real, parameter :: con_cp     =1.0046e+3          !< spec heat air at p (\f$J/kg/K\f$)
    real, parameter :: con_rd     =2.8705e+2          !< gas constant air (\f$J/kg/K\f$)
    real, parameter :: con_rv     =4.6150e+2          !< gas constant H2O (\f$J/kg/K\f$)
    real, parameter :: con_g      =9.8067e+0          !< gravity (\f$m/s^{2}\f$)
    real, parameter :: con_fvirt  =con_rv/con_rd-1.

  end module constants_mod
