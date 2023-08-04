  module module_fr_fire_util

    use, intrinsic :: iso_fortran_env, only : ERROR_UNIT, OUTPUT_UNIT

    private

    public :: REAL_MAX, REAL_SUM, RNRM_MAX, RNRM_SUM, fun_real, &
        Message, Crash, Check_mesh_2dim, Print_2d_stats

    integer, parameter:: REAL_SUM = 10, REAL_MAX = 20, RNRM_SUM = 30, RNRM_MAX = 40

  contains

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

    subroutine Check_mesh_2dim (ids, ide, jds, jde, ims, ime, jms, jme, fire_print_msg)

      implicit none

      integer, intent(in) :: ids, ide, jds, jde, ims, ime, jms, jme, fire_print_msg

      character (len = 128) msg
 

      if (ids < ims .or. ide > ime .or. jds < jms .or. jde > jme) then
        !$OMP CRITICAL(FIRE_UTIL_CRIT)
        write (msg, *) 'mesh dimensions:  ', ids, ide, jds, jde
        call Message (msg, fire_print_msg)
        write(msg, *) 'memory dimensions:',ims,ime,jms,jme
        !$OMP END CRITICAL(FIRE_UTIL_CRIT)
        call Message (msg, fire_print_msg)

        call Crash ('Check_mesh_2dim: memory dimensions too small')
      end if

    end subroutine Check_mesh_2dim

    subroutine Print_stat_line (name, ips, ipe, jps, jpe, min_a, max_a, avg_a, fire_print_msg)

      implicit none

      integer, intent (in) :: ips, ipe, jps, jpe, fire_print_msg
      character (len = *), intent(in) :: name
      real, intent(in) :: min_a, max_a, avg_a

      character (len = 128) :: msg
      character (len = 24) :: id


      if (fire_print_msg .eq. 0) return

      id = name
      !$OMP CRITICAL(FIRE_UTIL_CRIT)
      write(msg,'(a, 4i4, 3g11.3)') id, ips, ipe, jps, jpe, min_a, max_a, avg_a
      !$OMP END CRITICAL(FIRE_UTIL_CRIT)
      call Message (msg, fire_print_msg)

      if (.not. avg_a .eq. avg_a) call Crash ('NaN detected')

    end subroutine Print_stat_line

    subroutine Print_2d_stats(ips,ipe,jps,jpe, &
                         ims,ime,jms,jme, &
                         a,name,fire_print_msg)

      implicit none

      integer, intent (in) :: ips, ipe, jps, jpe, ims, ime, jms, jme
      integer, intent (in) :: fire_print_msg
      real, intent (in) :: a(ims:ime, jms:jme)
      character (len = *), intent (in) :: name

      integer :: i, j
      real :: avg_a, max_a, min_a, t, aa, bb
      character (len = 128) :: msg

        ! if(fire_print_msg.eq.0)return
        ! check for nans in any case
      bb = 0.0
      do j = jps, jpe
        do i = ips, ipe
           bb = bb + a(i, j)
        end do
      end do
      if(bb .eq. bb .and. fire_print_msg .eq. 0) return

        ! Calc min, max, and mean
      avg_a = 0
      max_a = - huge (max_a)
      min_a = huge (min_a)
      t = huge (t)
      do j = jps, jpe
        do i = ips, ipe
          aa = a(i, j)
          if (aa .ne. aa .or. .not. aa .le. t .or. .not. aa .ge. -t) goto 9
          max_a = max (max_a, aa)
          min_a = min (min_a, aa)
          avg_a = avg_a + aa
        end do
      end do

      if (bb .ne. bb) goto 10 ! should never happen 
      if (fire_print_msg .eq. 0) return

      avg_a = avg_a / ((ipe - ips + 1) * (jpe - jps + 1))

      call Print_stat_line (name, ips, ipe, jps, jpe, min_a, max_a, avg_a, fire_print_msg)

      return

 9    continue

      !$OMP CRITICAL(FIRE_UTIL_CRIT)
      write (msg, 1) name, i, j, aa
      call Message (msg, fire_print_msg)
 1    format (a30, '(', i6, ',', i6, ') = ', g13.5)
      !$OMP END CRITICAL(FIRE_UTIL_CRIT)

      call Print_stat_line (name, ips, ipe, jps, jpe, aa, aa, aa, fire_print_msg)
      if (aa .ne. aa) goto 10
      msg = 'Invalid floating point number detected in ' // name
      call Crash (msg)

 10   msg = 'NaN detected in '//name
      call Crash (msg)

    end subroutine Print_2d_stats

    pure integer function ifval(l,i,j)

      implicit none

      logical, intent (in) :: l
      integer, intent (in) :: i, j

      if (l) then
        ifval = i
      else
        ifval = j
      end if

    end function ifval

    real function fun_real(fun,  & 
        ims,ime,kms,kme,jms,jme, &                ! memory dims
        ids,ide,kds,kde,jds,jde, &                ! domain dims
        ips,ipe,kps,kpe,jps,jpe, &                ! patch or tile dims
        istag,kstag,jstag,       &                ! staggering
        a,b)                      

#ifdef DM_PARALLEL
      USE module_dm , only : wrf_dm_sum_real , wrf_dm_max_real
#endif

      implicit none

      integer, intent(in)::  fun, &
          ims, ime, kms, kme, jms, jme, &  ! memory dims
          ids, ide, kds, kde, jds, jde, &  ! domain dims
          ips, ipe, kps, kpe, jps, jpe, &  ! patch dims
          istag, kstag, jstag              ! staggering
      real, intent(in),dimension(ims:ime, kms:kme, jms:jme) :: a, b

      real :: lsum,void
      integer :: i,j,k,n,ipe1,jpe1,kpe1,iel,thread,is,js,ks
      real, save :: psum, gsum
      real :: rel
      logical :: dosum, domax
      character (len = 256) :: msg


      ipe1 = ifval (ipe .eq. ide .and. istag .ne. 0, ipe + 1, ipe)
      kpe1 = ifval (kpe .eq. kde .and. kstag .ne. 0, kpe + 1, kpe)
      jpe1 = ifval (jpe .eq. jde .and. jstag .ne. 0, jpe + 1, jpe)
      is = ifval (istag .ne. 0, 1, 0)
      ks = ifval (kstag .ne. 0, 1, 0)
      js = ifval (jstag .ne. 0, 1, 0)

      if(fun .eq. REAL_SUM) then
        void = 0.0
        lsum = void
        do j = jps, jpe1
          do k = kps, kpe1
            do i = ips, ipe1
              lsum = lsum + a(i,k,j)
            end do
          end do
        end do
      else if (fun .eq. RNRM_SUM) then
        void = 0.0
        lsum = void
        do j = jps, jpe1
          do k = kps, kpe1
            do i = ips, ipe1
              lsum = lsum + sqrt (a(i, k, j) * a(i, k, j) + b(i, k, j) * b(i, k, j))
            end do
          end do
        end do
      else if (fun .eq. REAL_MAX) then
        void = - huge (lsum)
        lsum = void
        do j = jps, jpe1
          do k = kps, kpe1
            do i = ips, ipe1
              lsum = max (lsum, a(i, k, j))
            end do
          end do
        end do
      else if (fun .eq. RNRM_MAX) then
        void = 0.0
        lsum = void
        do j = jps, jpe1
          do k = kps, kpe1
            do i = ips, ipe1
              lsum = max (lsum, sqrt (a(i, k, j) * a(i, k, j) + b(i, k, j) * b(i, k, j)))
            end do
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

      fun_real = gsum

    end function fun_real

  end module module_fr_fire_util
