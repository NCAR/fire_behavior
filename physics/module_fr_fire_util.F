  module module_fr_fire_util

    use, intrinsic :: iso_fortran_env, only : ERROR_UNIT, OUTPUT_UNIT

    private

    public :: REAL_MAX, REAL_SUM, RNRM_MAX, RNRM_SUM, Calc_domain_stats, &
        Message, Crash, Check_dims, Print_tile_stats

    integer, parameter:: REAL_SUM = 10, REAL_MAX = 20, RNRM_SUM = 30, RNRM_MAX = 40

  contains

    subroutine Check_for_nan (is, ie, js, je, ims, ime, jms, jme, var2d)

      implicit none

      integer, intent (in) :: is, ie, js, je, ims, ime, jms, jme
      real, intent (in) :: var2d(ims:ime, jms:jme)

      integer :: i, j
      real :: sum_var2d
      character (len = 128) :: msg


      sum_var2d = 0.0
      do j = js, je
        do i = is, ie
           sum_var2d = sum_var2d + var2d(i, j)
        end do
      end do

      if (sum_var2d /= sum_var2d) call Crash ('NaN detected')

    end subroutine Check_for_nan

    subroutine Crash (msg)

      implicit none

      character (len = *), intent(in) :: msg

      write (ERROR_UNIT, *) trim ('Crash: ' // msg)
      stop

    end subroutine Crash

    subroutine Message (msg, fire_print_msg, level)

      implicit none

      character (len = *), intent(in) :: msg
      integer, intent(in) :: fire_print_msg
      integer, intent(in), optional :: level

      integer :: mlevel


      if (present (level)) then
        mlevel = level
      else
        mlevel = 2
      endif

      if (fire_print_msg >= mlevel) write (OUTPUT_UNIT, *) 'FIRE:' // trim (msg)

    end subroutine Message

    subroutine Check_dims (ifs, ife, jfs, jfe, ifms, ifme, jfms, jfme, fire_print_msg)

      implicit none

      integer, intent(in) :: ifs, ife, jfs, jfe, ifms, ifme, jfms, jfme, fire_print_msg

      character (len = 128) msg
 

      if (ifs < ifms .or. ife > ifme .or. jfs < jfms .or. jfe > jfme) then
        !$OMP CRITICAL(FIRE_UTIL_CRIT)
        write (msg, *) 'mesh dimensions:  ', ifs, ife, jfs, jfe
        call Message (msg, fire_print_msg)
        write (msg, *) 'memory dimensions:', ifms, ifme, jfms, jfme
        !$OMP END CRITICAL(FIRE_UTIL_CRIT)
        call Message (msg, fire_print_msg)

        call Crash ('Check_dims: memory dimensions too small')
      end if

    end subroutine Check_dims

    subroutine Print_tile_stats (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, a, var_name, fire_print_msg)

      implicit none

      integer, intent (in) :: ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme
      integer, intent (in) :: fire_print_msg
      real, intent (in) :: a(ifms:ifme, jfms:jfme)
      character (len = *), intent (in) :: var_name

      integer :: i, j
      real :: avg_a, max_a, min_a, t, aa, bb
      character (len = 128) :: msg


      if (fire_print_msg == 0) return

        ! Calc min, max, and mean
      avg_a = 0
      max_a = - huge (max_a)
      min_a = huge (min_a)
      t = huge (t)
      do j = jfts, jfte
        do i = ifts, ifte
          aa = a(i, j)
          if (aa .ne. aa .or. .not. aa .le. t .or. .not. aa .ge. -t) call Handle_exception (var_name, i, j, aa)
          max_a = max (max_a, aa)
          min_a = min (min_a, aa)
          avg_a = avg_a + aa
        end do
      end do

      avg_a = avg_a / real ((ifte - ifts + 1) * (jfte - jfts + 1))

      call Print_stats (var_name, ifts, ifte, jfts, jfte, min_a, max_a, avg_a, fire_print_msg)

    contains

      subroutine Handle_exception (var_name, i, j, aa)

        implicit none

        character (len = *), intent(in) :: var_name
        integer, intent (in) :: i, j
        real, intent (in) :: aa


        !$OMP CRITICAL
        write (msg, 1) var_name, i, j, aa
        call Message (msg, fire_print_msg)
 1      format (a30, '(', i6, ',', i6, ') = ', g13.5)
        !$OMP END CRITICAL

        call Crash ('Invalid floating point number detected in ' // var_name)
        if (aa .ne. aa) call Crash ('NaN detected in ' // var_name)

      end subroutine Handle_exception

      subroutine Print_stats (var_name, ifts, ifte, jfts, jfte, min_a, max_a, avg_a, fire_print_msg)

        implicit none

        integer, intent (in) :: ifts, ifte, jfts, jfte, fire_print_msg
        character (len = *), intent(in) :: var_name
        real, intent(in) :: min_a, max_a, avg_a

        character (len = 128) :: msg
        character (len = 24) :: id


        id = trim (var_name)
        !$OMP CRITICAL(FIRE_UTIL_CRIT)
        write (msg,'(a, 4i4, 3g11.3)') id, ifts, ifte, jfts, jfte, min_a, max_a, avg_a
        !$OMP END CRITICAL(FIRE_UTIL_CRIT)
        call Message (msg, fire_print_msg)

      end subroutine Print_stats

    end subroutine Print_tile_stats

    real function Calc_domain_stats (fun, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, a, b) result (return_value)

#ifdef DM_PARALLEL
      USE module_dm , only : wrf_dm_sum_real , wrf_dm_max_real
#endif

      implicit none

      integer, intent(in)::  fun, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte
      real, intent(in), dimension(ifms:ifme, jfms:jfme) :: a, b

      real :: lsum, void
      integer :: i, j
      real, save :: psum, gsum
      logical :: dosum, domax
      character (len = 256) :: msg


      if(fun .eq. REAL_SUM) then
        void = 0.0
        lsum = void
        do j = jfts, jfte
          do i = ifts, ifte
            lsum = lsum + a(i,j)
          end do
        end do
      else if (fun .eq. RNRM_SUM) then
        void = 0.0
        lsum = void
        do j = jfts, jfte
          do i = ifts, ifte
            lsum = lsum + sqrt (a(i, j) * a(i, j) + b(i, j) * b(i, j))
          end do
        end do
      else if (fun .eq. REAL_MAX) then
        void = - huge (lsum)
        lsum = void
        do j = jfts, jfte
          do i = ifts, ifte
            lsum = max (lsum, a(i, j))
          end do
        end do
      else if (fun .eq. RNRM_MAX) then
        void = 0.0
        lsum = void
        do j = jfts, jfte
          do i = ifts, ifte
            lsum = max (lsum, sqrt (a(i, j) * a(i, j) + b(i, j) * b(i, j)))
          end do
        end do
      else
        call Crash ('fun_real: bad fun')
      end if

      if (lsum .ne. lsum) call Crash ('fun_real: NaN detected')

      dosum = fun .eq. REAL_SUM .or. fun .eq. RNRM_SUM
      domax = fun .eq. REAL_MAX .or. fun .eq. RNRM_MAX

      ! get process sum over all threads
      !$OMP SINGLE
      ! only one thread should write to shared variable
      psum = void
      !$OMP END SINGLE
      !$OMP BARRIER
      ! now all threads know psum

      !$OMP CRITICAL(RDSUM)
      ! each thread adds its own lsum
      if (dosum) psum = psum + lsum
      if (domax) psum = max (psum, lsum)
      !$OMP END CRITICAL(RDSUM)

      ! wait till all theads are done
      !$OMP BARRIER

      ! get global sum over all processes
      !$OMP SINGLE
      ! only one threads will do the mpi communication
#ifdef DM_PARALLEL
          if (dosum) gsum = wrf_dm_sum_real ( psum )
          if (domax) gsum = wrf_dm_max_real ( psum )
#else
          gsum = psum
#endif
      if (gsum .ne. gsum) call Crash ('fun_real: NaN detected')
      !$OMP END SINGLE

      !$OMP BARRIER
      ! now gsum is known to all threads

      return_value = gsum

    end function Calc_domain_stats

  end module module_fr_fire_util
