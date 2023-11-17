  module module_fr_fire_util

    use, intrinsic :: iso_fortran_env, only : ERROR_UNIT, OUTPUT_UNIT

    use stderrout_mod, only: Crash, Message

    private

    public :: REAL_MAX, REAL_SUM, RNRM_MAX, RNRM_SUM, Calc_domain_stats

    integer, parameter:: REAL_SUM = 10, REAL_MAX = 20, RNRM_SUM = 30, RNRM_MAX = 40

  contains

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
