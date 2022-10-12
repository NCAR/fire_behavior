  module wrf_fire_test3_mod

    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

    use wrf_atmosphere_mod, only : domain, grid_config_rec_type
    use module_fr_fire_util, only : set_ideal_coord

    implicit none

    private

    public :: Set_wrf_fire_test3, n_steps_test3, read_wrf_input_test3, check_tends_test3
    integer :: n_steps_test3 = 0
    logical :: read_wrf_input_test3 = .true.
    logical :: check_tends_test3 = .true.

      ! Grid settins
    integer, parameter :: IDS = 1, IDE = 20, KDS = 1, KDE = 51, JDS = 1, JDE = 40 
    integer, parameter :: SR_X = 4, SR_Y = 4
    integer, parameter :: N_TIME_STEPS = 24

      ! Debuging
    integer, parameter :: I_TO_PRINT = 15, J_TO_PRINT = 15

  contains

    subroutine Set_wrf_fire_test3 (grid, config_flags)

      implicit none

      type (domain), intent (in out) :: grid
      type (grid_config_rec_type), intent (in out) :: config_flags

      logical, parameter :: DEBUG = .true.


      if (DEBUG) write (OUTPUT_UNIT, *) '  Entering subroutine Set_wrf_fire_test3'

        ! Set config_flags
      call Load_config_flags_test3 (config_flags)
      if (DEBUG) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) 'Contents of config_flags:'
        call config_flags%Print ()
      end if

        ! Set grid
      grid = domain (ids = IDS, ide = IDE, kds = KDS, kde = KDE, jds = JDS, jde = JDE, sr_x = SR_X, sr_y = SR_Y)
      if (DEBUG) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) 'Contents of grid:'
        call grid%Print ()
      end if

      call Load_domain_test3 (grid)

        ! Number of time steps
      n_steps_test3 = N_TIME_STEPS

      if (DEBUG) write (OUTPUT_UNIT, *) '  Leaving subroutine Set_wrf_fire_test3'

    end subroutine Set_wrf_fire_test3

    subroutine Load_config_flags_test3 (config_flags)

      implicit none

      type (grid_config_rec_type), intent (in out) :: config_flags

      logical, parameter :: DEBUG = .true.


      if (DEBUG) write (OUTPUT_UNIT, *) '  Entering subroutine Load_config_flags_test3'

      config_flags%fire_print_msg = 1

      config_flags%fire_fuel_read = 0
      config_flags%fire_fuel_cat = 1

      config_flags%fire_num_ignitions = 1
      config_flags%fire_ignition_ros1 = 0.05
      config_flags%fire_ignition_start_x1 = 700.0
      config_flags%fire_ignition_start_y1 = 500.0
      config_flags%fire_ignition_end_x1 = 500.0
      config_flags%fire_ignition_end_y1 = 1500.0
      config_flags%fire_ignition_radius1 = 80.0
      config_flags%fire_ignition_start_time1 = 1.0
      config_flags%fire_ignition_end_time1 = 15.0

      config_flags%fire_wind_height = 1.0

      config_flags%fire_fuel_left_irl = 2
      config_flags%fire_fuel_left_jrl = 2
      config_flags%fire_atm_feedback = 1.0
      config_flags%fire_grows_only = 1
      config_flags%fire_viscosity = 0.4
      config_flags%fire_upwinding = 9
      config_flags%fire_fuel_left_method = 1
      config_flags%fire_lfn_ext_up = 1.0

      if (DEBUG) write (OUTPUT_UNIT, *) '  Leaving subroutine Load_config_flags_test3'

    end subroutine Load_config_flags_test3

    subroutine Load_domain_test3 (grid)

      implicit none

      type (domain), intent (in out) :: grid

      logical, parameter  :: DEBUG = .true.
      real, parameter, dimension(KDE) :: PH_2_PROFILE = [ 0.00000000, 13.8204203, 26.5920029, 38.3797607, 49.2364426, 59.2161636, &
          68.3687134, 76.7347412, 84.2828140, 91.0704575, 97.1466751, 102.561920, 107.365410, 111.603745, 115.318642, 118.537750, &
          121.365837, 123.837875, 125.986755, 127.842049, 129.431870, 130.781860, 131.915421, 132.854492, 133.619125, 134.227219, &
          134.695877, 134.984268, 135.150009, 135.207733, 135.170517, 135.050400, 134.858658, 134.604935, 134.298111, 133.934662, &
          133.517365, 133.069000, 132.595490, 132.102081, 131.593399, 131.073563, 130.546265, 130.014786, 129.481873, 128.950027, &
          128.421371, 127.897804, 127.380852, 126.871918, 126.372375 ]
      real, parameter, dimension(KDE) :: PHB_PROFILE = [ 0.00000000, 1707.28320, 3361.90430, 4964.50537, 6515.84424, 8016.72852, &
          9467.99121, 10870.6191, 12225.7480, 13534.3027, 14797.2246, 16015.4951, 17190.0977, 18322.0605, 19412.4102, &
          20462.0918, 21471.9375, 22443.0254, 23376.4375, 24273.2539, 25134.5645, 25961.4453, 26754.9785, 27516.2227, &
          28246.2402, 28946.0723, 29616.7441, 30259.1211, 30874.1660, 31462.8828, 32026.2500, 32565.2148, 33080.7070, &
          33573.6367, 34044.8750, 34495.2852, 34925.6172, 35336.6758, 35729.2500, 36104.1055, 36461.9805, 36803.5859, &
          37129.6133, 37440.7266, 37737.5625, 38020.7383, 38290.8477, 38548.4609, 38794.1250, 39028.3711, 39251.6992 ]
      real, parameter, dimension(KDE) :: RHO_PROFILE = [ 1.20651519, 1.18431544, 1.16323423, 1.14317775, 1.12409139, 1.10593629, &
          1.08858371, 1.07191896, 1.05606472, 1.04098368, 1.02663994, 1.01299584, 1.00001323, 0.987663746, 0.976037860, &
          0.965157628, 0.954824507, 0.945008934, 0.935684621, 0.926825523, 0.918407857, 0.910409093, 0.902807295, &
          0.895582259, 0.888715386, 0.882187963, 0.876253724, 0.870619595, 0.865259767, 0.860160649, 0.855310082, &
          0.850694954, 0.846304417, 0.842127442, 0.838175058, 0.834568560, 0.831135690, 0.827868402, 0.824758351, &
          0.821798563, 0.818981647, 0.816300511, 0.813748956, 0.811320662, 0.809009790, 0.806810796, 0.804717839, &
          0.802726388, 0.800831199, 0.799025893, 0.00000000 ]
      real, parameter, dimension(KDE) :: ZATW_PROFILE = [ 0.00000000, 175.443787, 345.412445, 509.978088, 669.223267, 823.235901, &
          972.106018, 1115.93811, 1254.84509, 1388.92688, 1518.28442, 1643.02307, 1763.24792, 1879.06860, 1990.59399, 2097.92334, &
          2201.15210, 2300.39380, 2395.76172, 2487.36938, 2575.33081, 2659.75806, 2740.76392, 2818.45850, 2892.95190, 2964.35254, &
          3032.76636, 3098.27759, 3160.99023, 3221.00806, 3278.43213, 3333.36035, 3385.88843, 3436.11011, 3484.11523, 3529.99146, &
          3573.81567, 3615.67212, 3655.64136, 3693.80273, 3730.23169, 3765.00098, 3798.18140, 3829.84106, 3860.04492, 3888.85693, &
          3916.33716, 3942.54419, 3967.53345, 3991.35986, 4014.07422 ]
      real, parameter, dimension(KDE) :: DZ8W_PROFILE = [ 175.443787, 169.968658, 164.565643, 159.245178, 154.012634, 148.870117, &
          143.832092, 138.906982, 134.081787, 129.357544, 124.738647, 120.224854, 115.820679, 111.525391, 107.329346, 103.228760, &
          99.2416992, 95.3679199, 91.6076660, 87.9614258, 84.4272461, 81.0058594, 77.6945801, 74.4934082, 71.4006348, 68.4138184, &
          65.5112305, 62.7126465, 60.0178223, 57.4240723, 54.9282227, 52.5280762, 50.2216797, 48.0051270, 45.8762207, 43.8242188, &
          41.8564453, 39.9692383, 38.1613770, 36.4289551, 34.7692871, 33.1804199, 31.6596680, 30.2038574, 28.8120117, 27.4802246, &
          26.2070312, 24.9892578, 23.8264160, 22.7143555, 0.00000000 ]
      real, parameter :: HT = 0.0, Z0 = 0.1, MUT = 38739.3828, T2 = 0.0, Q2 = 0.0, PSFC = 0.0, RAINC = 0.0, RAINNC = 0.0, &
          ZSF = 0.0, DZDXF = 0.0, DZDYF = 0.0, DX = 50.0, DY = 50.0, DT = 0.5

      real :: fdx, fdy
      integer :: k


      if (DEBUG) write (OUTPUT_UNIT, *) '  Entering subroutine Load_domain_test3'

      grid%dx = DX
      grid%dy = DY
      grid%dt = DT

        ! 3D arrays
      do k = 1, KDE
        grid%ph_2(:, k, :) = PH_2_PROFILE(k)
        grid%phb(:, k, :) = PHB_PROFILE(k)
        grid%rho(:, k, :) = RHO_PROFILE(k)
        grid%z_at_w(:, k, :) = ZATW_PROFILE(k)
        grid%dz8w(:, k, :) = DZ8W_PROFILE(k)
      end do
      grid%u_2 = 3.0
      grid%v_2 = 0.0

        ! 2D arrays
      grid%ht = HT
      grid%z0 = Z0
      grid%mut = MUT
      grid%t2 = T2
      grid%q2 = Q2
      grid%psfc = PSFC
      grid%rainc = RAINC
      grid%rainnc = RAINNC

      grid%zsf = ZSF
      grid%dzdxf = DZDXF
      grid%dzdyf = DZDYF

        ! 1D arrays
      grid%c1h(:) = 1.0
      grid%c2h(:) = 0.0

        ! Ideal coordinates
      call set_ideal_coord( grid%dx,grid%dy, &
                  grid%ids, grid%ide, grid%jds, grid%jde, &
                  grid%ims, grid%ime, grid%jms, grid%jme, &
                  grid%ids, grid%ide, grid%jds, grid%jde, & ! originaly tile dims. domain dim are the same here
                  grid%xlong,grid%xlat)

      fdx = grid%dx / grid%sr_x
      fdy = grid%dy / grid%sr_y

      call set_ideal_coord (fdx, fdy, &
          grid%ifds, grid%ifde, grid%jfds, grid%jfde, &
          grid%ifms, grid%ifme, grid%jfms, grid%jfme, &
          grid%ifds, grid%ifde, grid%jfds, grid%jfde, & ! originaly tile dims. domain dim are the same here
          grid%fxlong,grid%fxlat)

      if (DEBUG) then
        write (OUTPUT_UNIT, *) 'xlat: i, j =', I_TO_PRINT, J_TO_PRINT
        write (OUTPUT_UNIT, *) grid%xlat (I_TO_PRINT, J_TO_PRINT)
        write (OUTPUT_UNIT, *) 'xlong: i, j =', I_TO_PRINT, J_TO_PRINT
        write (OUTPUT_UNIT, *) grid%xlong (I_TO_PRINT, J_TO_PRINT)
        write (OUTPUT_UNIT, *) 'fxlat: i, j =', I_TO_PRINT, J_TO_PRINT
        write (OUTPUT_UNIT, *) grid%fxlat (I_TO_PRINT, J_TO_PRINT)
        write (OUTPUT_UNIT, *) 'fxlong: i, j =', I_TO_PRINT, J_TO_PRINT
        write (OUTPUT_UNIT, *) grid%fxlong (I_TO_PRINT, J_TO_PRINT)
      end if

      if (DEBUG) write (OUTPUT_UNIT, *) '  Leaving subroutine Load_domain_test3'

    end subroutine Load_domain_test3

  end module wrf_fire_test3_mod

