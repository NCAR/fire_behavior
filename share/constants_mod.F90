  module constants_mod

    implicit none

    private

    public :: R_D, CP, XLV, PI

    real, parameter :: R_D = 287.0                ! gas constant of dry air [J deg-1 kg-1]
    real, parameter :: CP = 7.0 * R_D / 2.0
    real, parameter :: XLV = 2.5E6                ! latent heat of vaporization of water at 0 degrees C [J kg-1]
    real, parameter :: PI = 3.1415926

  end module constants_mod
