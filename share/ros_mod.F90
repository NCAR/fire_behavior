  module ros_mod

    use fuel_mod, only : fuel_t

    implicit none

    private
    
    public :: ros_t, ROS_WRFFIRE

    integer, parameter :: ROS_WRFFIRE = 0

    type, abstract :: ros_t
      real, dimension(:, :), allocatable :: iboros
    contains
      procedure (Calc_ros), deferred :: Calc_ros
      procedure (Init), deferred :: Init
      procedure (Set_params), deferred :: Set_params
    end type ros_t

    abstract interface
      subroutine Init (this, ifms, ifme, jfms, jfme)
        import :: ros_t
        class (ros_t), intent (in out) :: this
        integer, intent (in) :: ifms, ifme, jfms, jfme
      end subroutine Init

      subroutine Set_params (this, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, fuels, nfuel_cat, fmc_g)
        import :: ros_t, fuel_t
        class (ros_t), intent (in out) :: this
        integer, intent (in) :: ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte
        class (fuel_t), intent (in) :: fuels
        real, dimension(ifms:ifme, jfms:jfme), intent (in) :: nfuel_cat, fmc_g
      end subroutine Set_params

      subroutine Calc_ros (this, ifms, ifme, jfms, jfme, ros, nvx, nvy, i, j, uf, vf, dzdxf, dzdyf)
        import :: ros_t, fuel_t
        class (ros_t), intent (in) :: this
        real, intent (in) :: nvx, nvy
        integer, intent (in) :: ifms, ifme, jfms, jfme, i, j
        real, dimension(ifms:ifme, jfms:jfme), intent (in) :: uf, vf, dzdxf, dzdyf
        real, dimension(ifms:ifme, jfms:jfme), intent (out) :: ros
      end subroutine Calc_ros

    end interface

  end module ros_mod
