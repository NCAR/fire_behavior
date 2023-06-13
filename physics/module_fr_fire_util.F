!
! This module contains general purpose utilities and WRF wrappers because some
! applications require this module to run standalone. No physics here.
! Some are dependent on WRF indexing scheme. Some violate WRF conventions but these
! are not called from the WRF dependent code. Some are not called at all.
! 

module module_fr_fire_util

  use, intrinsic :: iso_fortran_env, only : ERROR_UNIT, OUTPUT_UNIT

! various method selection parameters
! 1. add the parameter and its static default here
! optional:
! 2. add copy from config_flags in module_fr_fire_driver%%set_flags
! 3. add a line in Registry.EM to define the variable and set default value
! 4. add the variable and value in namelist.input

private

public :: ifval, &
          real_max, real_sum, rnrm_max, rnrm_sum, fun_real, &
          Open_text_file, Message, Crash, Check_mesh_2dim, &
          Print_2d_stats

integer, parameter:: REAL_SUM=10, REAL_MAX=20, RNRM_SUM=30, RNRM_MAX=40

contains

  subroutine crash (msg)

    implicit none

    character (len = *), intent(in) :: msg

    write (ERROR_UNIT, *) trim ('crash: ' // msg)
    stop

  end subroutine crash

  subroutine message (msg, fire_print_msg, level)

    implicit none

    character (len = *), intent(in) :: msg
    integer, intent(in) :: fire_print_msg
    integer, intent(in), optional :: level

    integer mlevel


    if (present (level)) then
      mlevel = level
    else
      mlevel = 2
    endif

    if (fire_print_msg >= mlevel) write (OUTPUT_UNIT, *) 'FIRE:' // trim (msg)

  end subroutine message

!
!****************
!


integer function open_text_file(filename,rw,fire_print_msg,allow_fail)
implicit none
character(len=*),intent(in):: filename,rw
integer, intent(in):: fire_print_msg
logical, intent(in), optional:: allow_fail
#ifdef _OPENMP
!integer, external:: OMP_GET_THREAD_NUM
#endif
character(len=128):: msg
character(len=1)::act
integer::iounit,ierr
logical::op,ok

#ifdef _OPENMP
!   if (OMP_GET_THREAD_NUM() .ne. 0)then
!      call crash('open_input_text_file: called from parallel loop')
!   endif
#endif
if(present(allow_fail))then
    ok=allow_fail
else
    ok=.false.  ! default
endif

    do iounit=19,99
       inquire(iounit,opened=op)
       if(.not.op)goto 1
    enddo
    call crash('open_text_file: Cannot find any available I/O unit')
1   continue
    act=rw(1:1)
    select case (act)
        case ('r','R')
            OPEN(iounit, FILE=filename,FORM='FORMATTED',STATUS='OLD',ACTION='READ',IOSTAT=ierr)
        case ('w','W')
            OPEN(iounit, FILE=filename,FORM='FORMATTED',STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=ierr)
        case default
            write(msg,*)'open_text_file: bad mode ',trim(rw),' for file ',trim(filename)
    end select
    
    if(ierr.ne.0)then 
	write(msg,*)'Cannot open file ',filename
        if(ok)then
            call message(msg,fire_print_msg)
            open_text_file=-1
        else
            call crash(msg)
        endif
    else
        open_text_file=iounit
    endif

end function open_text_file

!
!*****************************
!

subroutine check_mesh_2dim(ids,ide,jds,jde,ims,ime,jms,jme,fire_print_msg)
implicit none
integer, intent(in)::ids,ide,jds,jde,ims,ime,jms,jme,fire_print_msg
character(len=128)msg
if(ids<ims.or.ide>ime.or.jds<jms.or.jde>jme)then
!$OMP CRITICAL(FIRE_UTIL_CRIT)
    write(msg,*)'mesh dimensions:  ',ids,ide,jds,jde
    call message(msg,fire_print_msg)
    write(msg,*)'memory dimensions:',ims,ime,jms,jme
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
    call message(msg,fire_print_msg)
    call crash('check_mesh_2dim: memory dimensions too small')
endif
end subroutine check_mesh_2dim

!
!****************
!

subroutine sum_2d_cells(       &
       ims2,ime2,jms2,jme2,    &
       its2,ite2,jts2,jte2,    &
       v2,                     &  ! input       
       ims1,ime1,jms1,jme1,    &
       its1,ite1,jts1,jte1,    &
       fire_print_msg,         &
       v1)                        ! output
implicit none

!*** purpose
! sum cell values in mesh2 to cell values of coarser mesh1

!*** arguments
! the dimensions are in cells, not nodes!

integer, intent(in)::its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1
integer, intent(in)::fire_print_msg
real, intent(out)::v1(ims1:ime1,jms1:jme1)
integer, intent(in)::its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2
real, intent(in)::v2(ims2:ime2,jms2:jme2)

!*** local
integer:: i1,i2,j1,j2,ir,jr,isz1,isz2,jsz1,jsz2,ioff,joff,ibase,jbase
real t
character(len=128)msg

!*** executable

!check mesh dimensions and domain dimensions
call check_mesh_2dim(its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1,fire_print_msg)
call check_mesh_2dim(its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2,fire_print_msg)

! compute mesh sizes
isz1 = ite1-its1+1
jsz1 = jte1-jts1+1
isz2 = ite2-its2+1
jsz2 = jte2-jts2+1

! check mesh sizes
if(isz1.le.0.or.jsz1.le.0.or.isz2.le.0.or.jsz2.le.0)then
    call message('all mesh sizes must be positive',fire_print_msg)
    goto 9
endif

! compute mesh ratios
ir=isz2/isz1
jr=jsz2/jsz1

if(isz2.ne.isz1*ir .or. jsz2.ne.jsz1*jr)then
    call message('input mesh size must be multiple of output mesh size',fire_print_msg)
    goto 9
endif


! v1 = sum(v2)
do j1=jts1,jte1
    jbase=jts2+jr*(j1-jts1)
    do i1=its1,ite1
       ibase=its2+ir*(i1-its1)
       t=0.
       do joff=0,jr-1
           j2=joff+jbase
           do ioff=0,ir-1
               i2=ioff+ibase
               t=t+v2(i2,j2)
           enddo
       enddo
       v1(i1,j1)=t
    enddo
enddo

return

9 continue
!$OMP CRITICAL(FIRE_UTIL_CRIT)
write(msg,91)its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2
call message(msg,fire_print_msg)
write(msg,91)its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1
call message(msg,fire_print_msg)
write(msg,92)'input  mesh size:',isz2,jsz2
call message(msg,fire_print_msg)
91 format('dimensions: ',8i8)
write(msg,92)'output mesh size:',isz1,jsz1
call message(msg,fire_print_msg)
92 format(a,2i8)
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
call crash('module_fr_spread_util:sum_mesh_cells: bad mesh sizes')

end subroutine sum_2d_cells

!
!****************
!
                
subroutine print_stat_line(name,ips,ipe,jps,jpe,min_a,max_a,avg_a,fire_print_msg)
!*** encapsulate line with statistics
implicit none
!*** arguments
integer, intent(in)::ips,ipe,jps,jpe,fire_print_msg
character(len=*),intent(in)::name
real,intent(in)::min_a,max_a,avg_a
!*** local
character(len=128)::msg
character(len=24)::id
if(fire_print_msg.eq.0)return
id=name
!$OMP CRITICAL(FIRE_UTIL_CRIT)
write(msg,'(a,4i4,3g11.3)')id,ips,ipe,jps,jpe,min_a,max_a,avg_a
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
call message(msg,fire_print_msg)
if(.not.avg_a.eq.avg_a)call crash('NaN detected')
end subroutine print_stat_line


subroutine print_3d_stats(ips,ipe,kps,kpe,jps,jpe, &
                         ims,ime,kms,kme,jms,jme, &
                         a,name,fire_print_msg)
implicit none
integer, intent(in)::ips,ipe,jps,jpe,ims,ime,jms,jme,kms,kme,kps,kpe
integer, intent(in)::fire_print_msg
real, intent(in)::a(ims:ime,kms:kme,jms:jme)
character(len=*),intent(in)::name
integer:: i,j,k
real:: avg_a,max_a,min_a,t,aa,bb
character(len=128)::msg
! if(fire_print_msg.eq.0)return
! check for nans in any case
bb=0.
do j=jps,jpe
  do k=kps,kpe
    do i=ips,ipe
       bb=bb+a(i,k,j)
    enddo
  enddo
enddo
if(bb.eq.bb.and.fire_print_msg.eq.0)return
avg_a=0
max_a=-huge(max_a)
min_a= huge(min_a)
t=huge(t)
do j=jps,jpe
  do k=kps,kpe
    do i=ips,ipe
        aa=a(i,k,j)
        if(aa.ne.aa.or..not.aa.le.t.or..not.aa.ge.-t)goto 9
        max_a=max(max_a,aa)
        min_a=min(min_a,aa)
        avg_a=avg_a+aa
    enddo
  enddo
enddo
if(bb.ne.bb)goto 10 ! should never happen 
if(fire_print_msg.eq.0)return
avg_a = avg_a/((ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1))
call print_stat_line(name,ips,ipe,jps,jpe,min_a,max_a,avg_a,fire_print_msg)
return
9 continue
!$OMP CRITICAL(FIRE_UTIL_CRIT)
write(msg,1)name,i,k,j,aa
call message(msg,fire_print_msg)
1 format(a30,'(',i6,',',i6,',',i6,') = ',g13.5)
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
call print_stat_line(name,ips,ipe,jps,jpe,aa,aa,aa,fire_print_msg)
if(aa.ne.aa)goto 10
msg='Invalid floating point number detected in '//name
call crash(msg)
10 msg='NaN detected in '//name
call crash(msg)
end subroutine print_3d_stats

subroutine print_2d_stats(ips,ipe,jps,jpe, &
                         ims,ime,jms,jme, &
                         a,name,fire_print_msg)
implicit none
integer, intent(in)::ips,ipe,jps,jpe,ims,ime,jms,jme
integer, intent(in)::fire_print_msg
real, intent(in)::a(ims:ime,jms:jme)
character(len=*),intent(in)::name
!!character(len=128)::msg
!if(fire_print_msg.eq.0)return
call print_3d_stats(ips,ipe,1,1,jps,jpe, &
                         ims,ime,1,1,jms,jme, &
                         a,name,fire_print_msg)
!!write(msg,'(2a,z16)')name,' address =',loc(a)
!!call message(msg,fire_print_msg)
end subroutine print_2d_stats


! general conditional expression
pure integer function ifval(l,i,j)
implicit none
logical, intent(in)::l
integer, intent(in)::i,j
if(l)then
	ifval=i
else
	ifval=j
endif
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

integer, intent(in)::  fun, &
    ims,ime,kms,kme,jms,jme, &                ! memory dims
    ids,ide,kds,kde,jds,jde, &                ! domain dims
    ips,ipe,kps,kpe,jps,jpe, &                ! patch dims
    istag,kstag,jstag                         ! staggering
real, intent(in),dimension(ims:ime,kms:kme,jms:jme)::a,b

!*** local
real::lsum,void
integer::i,j,k,n,ipe1,jpe1,kpe1,iel,thread,is,js,ks
real, save::psum,gsum
real::rel
logical:: dosum,domax
character(len=256)msg

ipe1=ifval(ipe.eq.ide.and.istag.ne.0,ipe+1,ipe)
kpe1=ifval(kpe.eq.kde.and.kstag.ne.0,kpe+1,kpe)
jpe1=ifval(jpe.eq.jde.and.jstag.ne.0,jpe+1,jpe)
is=ifval(istag.ne.0,1,0)
ks=ifval(kstag.ne.0,1,0)
js=ifval(jstag.ne.0,1,0)

if(fun.eq.REAL_SUM)then
  void=0.
  lsum=void
  do j=jps,jpe1
    do k=kps,kpe1
      do i=ips,ipe1
        lsum=lsum+a(i,k,j)
      enddo
    enddo
  enddo
elseif(fun.eq.RNRM_SUM)then
  void=0.
  lsum=void
  do j=jps,jpe1
    do k=kps,kpe1
      do i=ips,ipe1
        lsum=lsum+sqrt(a(i,k,j)*a(i,k,j)+b(i,k,j)*b(i,k,j))
      enddo
    enddo
  enddo
elseif(fun.eq.REAL_MAX)then
  void=-huge(lsum)
  lsum=void
  do j=jps,jpe1
    do k=kps,kpe1
      do i=ips,ipe1
        lsum=max(lsum,a(i,k,j))
      enddo
    enddo
  enddo
elseif(fun.eq.RNRM_MAX)then
  void=0.
  lsum=void
  do j=jps,jpe1
    do k=kps,kpe1
      do i=ips,ipe1
        lsum=max(lsum,sqrt(a(i,k,j)*a(i,k,j)+b(i,k,j)*b(i,k,j)))
      enddo
    enddo
  enddo
else
  call crash('fun_real: bad fun')
endif

if(lsum.ne.lsum)call crash('fun_real: NaN detected')

dosum=fun.eq.REAL_SUM.or.fun.eq.RNRM_SUM
domax=fun.eq.REAL_MAX.or.fun.eq.RNRM_MAX

! get process sum over all threads
!$OMP SINGLE
! only one thread should write to shared variable
psum=void
!$OMP END SINGLE
!$OMP BARRIER
! now all threads know psum

!$OMP CRITICAL(RDSUM)
! each thread adds its own lsum
if(dosum)psum=psum+lsum
if(domax)psum=max(psum,lsum)
!$OMP END CRITICAL(RDSUM)

! wait till all theads are done
!$OMP BARRIER

! get global sum over all processes
!$OMP SINGLE
! only one threads will do the mpi communication
#ifdef DM_PARALLEL
    if(dosum) gsum = wrf_dm_sum_real ( psum )
    if(domax) gsum = wrf_dm_max_real ( psum )
#else
    gsum = psum
#endif
if(gsum.ne.gsum)call crash('fun_real: NaN detected')
!$OMP END SINGLE

!$OMP BARRIER
! now gsum is known to all threads

fun_real=gsum

end function fun_real

end module module_fr_fire_util
