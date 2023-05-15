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

public :: fire_advection, fire_fmc_read, fire_print_msg, fire_grows_only, fire_lfn_ext_up, &
          fire_lsm_band_ngp, fire_slope_factor, fire_upwind_split, fire_upwinding, ifval, &
          fire_viscosity, fire_viscosity_band, fire_viscosity_bg, fire_viscosity_ngp, &
          boundary_guard, fire_upwinding_reinit, fire_lsm_reinit_iter, fuel_left_irl, &
          fuel_left_jrl, fuel_left_method, fire_const_grnhfx, fire_const_grnqfx, &
          fire_const_time, fire_lsm_reinit, real_max, real_sum, rnrm_max, rnrm_sum, fun_real, &
          fire_atm_feedback, fire_lsm_zcoupling, fire_lsm_zcoupling_ref, &
          Open_text_file, Message, Crash, Check_mesh_2dim, &
          Print_2d_stats, Continue_at_boundary

integer,save::          &
 fire_print_msg=2,      &  ! print FIRE progress
 fuel_left_method=1,    &  ! 1=simple, 2=exact in linear case
 fuel_left_irl=2,       &  ! refinement for fuel calculation, must be even
 fuel_left_jrl=2,       &
 boundary_guard=-1,     &  ! crash if fire gets this many cells to domain boundary, -1=off
 fire_grows_only=1,     &  ! fire can spread out only (level set functions may not increase)
 fire_upwinding=9,      &  ! upwind normal spread: 1=standard, 2=godunov, 3=eno, 4=sethian, 5=2nd-order, 6=WENO3, 7=WENO5, 8=hybrid WENO3/ENO1, 9=hybrid WENO5/ENO1 
 fire_upwind_split=0,   &  ! 1=upwind advection separately from normal direction spread
 fire_topo_from_atm=1,  &  ! 0 = expect ZSF set correctly on entry, 1 = populate by interploating from atmosphere
 fire_advection=0,      &  ! 0 = fire spread from normal wind/slope (CAWFE), 1 = full speed projected
 fire_fmc_read=1,       &  ! fuel moisture: 0 from wrfinput, 1 from namelist.fire, 2 read from file in ideal
 fire_upwinding_reinit=4, &  ! spatial discretization for reinitialization PDE: 1=WENO3, 2=WENO5, 3=hybrid WENO3-ENO1, 4=hybrid WENO5-ENO1 
 fire_lsm_reinit_iter=1,  &  ! number of iterations for the reinitialization PDE 
 fire_lsm_band_ngp=4,     &  ! number of grid points around lfn=0 that the higher-order scheme WENO5/WENO3 is used (ENO1 elsewhere), for ire_upwinding=8,9 and fire_upwinding_reinit=3,4 and
 fire_viscosity_ngp=4        ! number of grid points around lfn=0 where low artificial viscosity is used = fire_viscosity_bg

real, save::            &
 fire_const_time=-1,    &  ! time from ignition to start constant heat output  <0=never
 fire_const_grnhfx=-1., &  ! if both >=0, the constant heat flux to output then
 fire_const_grnqfx=-1., &  ! if both >=0, the constant heat flux to output then
 fire_atm_feedback=1. , &  ! 1 = normal, 0. = one way coupling atmosphere -> fire only
 fire_viscosity=0.4,    &  ! artificial viscosity
 fire_lfn_ext_up=1.,    &  ! 0.=extend level set function at boundary by reflection, 1.=always up
 fire_lsm_zcoupling_ref=50., &  ! Reference height from wich u at fire_wind_hegiht is calculated using a logarithmic profile
 fire_viscosity_bg=0.4,      &  ! artificial viscosity in the near-front region 
 fire_viscosity_band=0.5,    &  ! number of times the hybrid advection band to transition from fire_viscosity_bg to fire_viscosity
 fire_slope_factor=1.0          ! slope correction factor

logical,save::              &
 fire_lsm_reinit=.true.,    &  ! flag to activate reinitialization of level set method
 fire_lsm_zcoupling=.false.     ! flag to activate reference velocity at a different height from fire_wind_height

integer, parameter:: REAL_SUM=10, REAL_MAX=20, RNRM_SUM=30, RNRM_MAX=40

contains

  subroutine crash (msg)

    implicit none

    character (len = *), intent(in) :: msg

    write (ERROR_UNIT, *) trim ('crash: ' // msg)
    stop

  end subroutine crash

  subroutine message (msg, level)

    implicit none

    character (len = *), intent(in) :: msg
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


integer function open_text_file(filename,rw,allow_fail)
implicit none
character(len=*),intent(in):: filename,rw
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
            call message(msg)
            open_text_file=-1
        else
            call crash(msg)
        endif
    else
        open_text_file=iounit
    endif

end function open_text_file

!
!****************
!

subroutine continue_at_boundary(ix,iy,bias, & ! do x direction or y direction
    ims,ime,jms,jme, &                ! memory dims
    ids,ide,jds,jde, &                ! domain dims
    its,ite,jts,jte, &                ! tile dims
    itso,iteo,jtso,jteo, &            ! tile dims where set
    lfn)                              ! array
implicit none
!*** description
! extend array by one beyond the domain by linear continuation
!*** arguments
integer, intent(in)::ix,iy              ! not 0 = do x or y (1 or 2) direction
real,intent(in)::bias                   ! 0=none, 1.=max
integer, intent(in)::ims,ime,jms,jme, & ! memory dims
    ids,ide,jds,jde,                  & ! domain dims
    its,ite,jts,jte                     ! tile dims
integer, intent(out)::itso,jtso,iteo,jteo ! where set
real,intent(inout),dimension(ims:ime,jms:jme)::lfn
!*** local
integer i,j
character(len=128)::msg
integer::its1,ite1,jts1,jte1
integer,parameter::halo=1           ! only 1 domain halo is needed since ENO1 is used near domain boundaries
!*** executable

! for dislay only
itso=its
jtso=jts
iteo=ite
jteo=jte
! go halo width beyond if at patch boundary but not at domain boudnary 
! assume we have halo need to compute the value we do not have 
! the next thread that would conveniently computer the extended values at patch corners
! besides halo may not transfer values outside of the domain
! 
its1=its
jts1=jts
ite1=ite
jte1=jte
if(.not.its.eq.ids)its1=its-halo
if(.not.jts.eq.jds)jts1=jts-halo
if(.not.ite.eq.ide)ite1=ite+halo
if(.not.jte.eq.jde)jte1=jte+halo
!$OMP CRITICAL(FIRE_UTIL_CRIT)
write(msg,'(a,2i5,a,f5.2)')'continue_at_boundary: directions',ix,iy,' bias ',bias 
call message(msg)
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
if(ix.ne.0)then
    if(its.eq.ids)then
        do j=jts1,jte1
            lfn(ids-1,j)=EX(lfn(ids,j),lfn(ids+1,j))
        enddo
        itso=ids-1
    endif
    if(ite.eq.ide)then
        do j=jts1,jte1
            lfn(ide+1,j)=EX(lfn(ide,j),lfn(ide-1,j))
        enddo
        iteo=ide+1
    endif
!$OMP CRITICAL(FIRE_UTIL_CRIT)
    write(msg,'(8(a,i5))')'continue_at_boundary: x:',its,':',ite,',',jts,':',jte,' ->',itso,':',iteo,',',jts1,':',jte1
    call message(msg)
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
endif
if(iy.ne.0)then
    if(jts.eq.jds)then
        do i=its1,ite1
            lfn(i,jds-1)=EX(lfn(i,jds),lfn(i,jds+1))
        enddo
        jtso=jds-1
    endif
    if(jte.eq.jde)then
        do i=its1,ite1
            lfn(i,jde+1)=EX(lfn(i,jde),lfn(i,jde-1))
        enddo
        jteo=jde+1
    endif
!$OMP CRITICAL(FIRE_UTIL_CRIT)
    write(msg,'(8(a,i5))')'continue_at_boundary: y:',its,':',ite,',',jts,':',jte,' ->',its1,':',ite1,',',jtso,':',jteo
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
    call message(msg)
endif
! corners of the domain
if(ix.ne.0.and.iy.ne.0)then
    if(its.eq.ids.and.jts.eq.jds)lfn(ids-1,jds-1)=EX(lfn(ids,jds),lfn(ids+1,jds+1))
    if(its.eq.ids.and.jte.eq.jde)lfn(ids-1,jde+1)=EX(lfn(ids,jde),lfn(ids+1,jde-1))
    if(ite.eq.ide.and.jts.eq.jds)lfn(ide+1,jds-1)=EX(lfn(ide,jds),lfn(ide-1,jds+1))
    if(ite.eq.ide.and.jte.eq.jde)lfn(ide+1,jde+1)=EX(lfn(ide,jde),lfn(ide-1,jde-1))
endif
return
contains
real function EX(a,b)
!*** statement function
real a,b
EX=(1.-bias)*(2.*a-b)+bias*max(2.*a-b,a,b)   ! extrapolation, max quarded
end function EX
end subroutine continue_at_boundary

!
!*****************************
!

subroutine check_mesh_2dim(ids,ide,jds,jde,ims,ime,jms,jme)
implicit none
integer, intent(in)::ids,ide,jds,jde,ims,ime,jms,jme
character(len=128)msg
if(ids<ims.or.ide>ime.or.jds<jms.or.jde>jme)then
!$OMP CRITICAL(FIRE_UTIL_CRIT)
    write(msg,*)'mesh dimensions:  ',ids,ide,jds,jde
    call message(msg)
    write(msg,*)'memory dimensions:',ims,ime,jms,jme
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
    call message(msg)
    call crash('check_mesh_2dim: memory dimensions too small')
endif
end subroutine check_mesh_2dim

!
!****************
!

subroutine check_mesh_3dim(ids,ide,kds,kde,jds,jde,ims,ime,kms,kme,jms,jme)
integer, intent(in)::ids,ide,jds,jde,ims,ime,jms,jme,kds,kde,kms,kme
if(ids<ims.or.ide>ime.or.jds<jms.or.jde>jme.or.kds<kms.or.kde>kme) then
    call crash('memory dimensions too small')
endif
end subroutine check_mesh_3dim

!
!****************
!

subroutine sum_2d_cells(       &
       ims2,ime2,jms2,jme2,    &
       its2,ite2,jts2,jte2,    &
       v2,                     &  ! input       
       ims1,ime1,jms1,jme1,    &
       its1,ite1,jts1,jte1,    &
       v1)                        ! output
implicit none

!*** purpose
! sum cell values in mesh2 to cell values of coarser mesh1

!*** arguments
! the dimensions are in cells, not nodes!

integer, intent(in)::its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1
real, intent(out)::v1(ims1:ime1,jms1:jme1)
integer, intent(in)::its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2
real, intent(in)::v2(ims2:ime2,jms2:jme2)

!*** local
integer:: i1,i2,j1,j2,ir,jr,isz1,isz2,jsz1,jsz2,ioff,joff,ibase,jbase
real t
character(len=128)msg

!*** executable

!check mesh dimensions and domain dimensions
call check_mesh_2dim(its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1)
call check_mesh_2dim(its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2)

! compute mesh sizes
isz1 = ite1-its1+1
jsz1 = jte1-jts1+1
isz2 = ite2-its2+1
jsz2 = jte2-jts2+1

! check mesh sizes
if(isz1.le.0.or.jsz1.le.0.or.isz2.le.0.or.jsz2.le.0)then
    call message('all mesh sizes must be positive')
    goto 9
endif

! compute mesh ratios
ir=isz2/isz1
jr=jsz2/jsz1

if(isz2.ne.isz1*ir .or. jsz2.ne.jsz1*jr)then
    call message('input mesh size must be multiple of output mesh size')
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
call message(msg)
write(msg,91)its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1
call message(msg)
write(msg,92)'input  mesh size:',isz2,jsz2
call message(msg)
91 format('dimensions: ',8i8)
write(msg,92)'output mesh size:',isz1,jsz1
call message(msg)
92 format(a,2i8)
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
call crash('module_fr_spread_util:sum_mesh_cells: bad mesh sizes')

end subroutine sum_2d_cells



! module_fr_fire_util%%interpolate_2d
subroutine interpolate_2d(  &
    ims2,ime2,jms2,jme2, & ! array coarse grid
    its2,ite2,jts2,jte2, & ! dimensions coarse grid
    ims1,ime1,jms1,jme1, & ! array coarse grid
    its1,ite1,jts1,jte1, & ! dimensions fine grid
    ir,jr,               & ! refinement ratio
    rip2,rjp2,rip1,rjp1, & ! (rip2,rjp2) on grid 2 lines up with (rip1,rjp1) on grid 1 
    v2, &                  ! in coarse grid  
    v1  )                  ! out fine grid
implicit none

!*** purpose
! interpolate nodal values in mesh2 to nodal values in mesh1
! interpolation runs over the mesh2 region its2:ite2,jts2:jte2 
! only the part of mesh 1 in the region its1:ite1,jts1:jte1 is modified

!*** arguments

integer, intent(in)::its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1
integer, intent(in)::its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2
integer, intent(in)::ir,jr
real,intent(in):: rjp1,rip1,rjp2,rip2
real, intent(out)::v1(ims1:ime1,jms1:jme1)
real, intent(in)::v2(ims2:ime2,jms2:jme2)

!*** local
integer:: i1,i2,j1,j2,is,ie,js,je
real:: tx,ty,rx,ry
real:: rio,rjo
intrinsic::ceiling,floor

!*** executable

!check mesh dimensions and domain dimensions
call check_mesh_2dim(its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1)
call check_mesh_2dim(its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2)

! compute mesh ratios
rx=1./ir
ry=1./jr

do j2=jts2,jte2-1               ! loop over mesh 2 cells
    rjo=rjp1+jr*(j2-rjp2)       ! mesh 1 coordinate of the mesh 2 patch start
    js=max(jts1,ceiling(rjo))   ! lower bound of mesh 1 patch for this mesh 2 cell
    je=min(jte1,floor(rjo)+jr)  ! upper bound of mesh 1 patch for this mesh 2 cell 
    do i2=its2,ite2-1
        rio=rip1+ir*(i2-rip2)
        is=max(its1,ceiling(rio))
        ie=min(ite1,floor(rio)+ir)
        do j1=js,je
            ty = (j1-rjo)*ry
            do i1=is,ie
                ! in case mesh 1 node lies on the boundary of several mesh 2 cells
                ! the result will be written multiple times with the same value
                ! up to a rounding error
                tx = (i1-rio)*rx
                !print *,'coarse ',i2,j2,'to',i2+1,j2+1,' fine ',is,js,' to ',ie,je
                v1(i1,j1)=                     &
                      (1-tx)*(1-ty)*v2(i2,j2)  &
                 +    (1-tx)*ty  *v2(i2,j2+1)  &
                 +      tx*(1-ty)*v2(i2+1,j2)  &
                 +        tx*ty  *v2(i2+1,j2+1)  
                !print *,'coarse ',i2,j2,' fine ',i1,j1, ' offset ',io,jo,' weights ',tx,ty, &
                ! 'in ',v2(i2,j2),v2(i2,j2+1),v2(i2+1,j2),v2(i2+1,j2+1),' out ',v1(i1,j1)
                !write(*,'(a,2i5,a,2f8.2,a,4f8.2,a,2i5,a,f8.2)') &
                !'coarse ',i2,j2,' coord',rio,rjo,' val',v2(i2,j2),v2(i2,j2+1),v2(i2+1,j2),v2(i2+1,j2+1),&
                !' fine ',i1,j1,' out ',v1(i1,j1)
           enddo
       enddo
    enddo
enddo

end subroutine interpolate_2d

!
!****************
!

subroutine interpolate_2d_cells2cells(              &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  ! in  
      ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,v1  )  ! out
implicit none

!*** purpose
! interpolate nodal values in mesh2 to nodal values in mesh1
! input mesh 2 is coarse output mesh 1 is fine

!*** arguments

integer, intent(in)::ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
real, intent(out)::v1(ims1:ime1,jms1:jme1)
integer, intent(in)::ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
real, intent(in)::v2(ims2:ime2,jms2:jme2)

! Example with mesh ratio=4. | = cell boundary,  x = cell center
!
!  mesh2   |-------x-------|-------x-------|
!  mesh1   |-x-|-x-|-x-|-x-|-x-|-x-|-x-|-x-| 
!

!*** local
integer:: ir,jr,isz1,isz2,jsz1,jsz2,ip,jp,ih,jh
character(len=128)msg

!*** executable

!check mesh dimensions and domain dimensions
call check_mesh_2dim(ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1)
call check_mesh_2dim(ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2)

! compute mesh sizes
isz1 = ide1-ids1+1
jsz1 = jde1-jds1+1
isz2 = ide2-ids2+1
jsz2 = jde2-jds2+1

! check mesh sizes
if(isz1.le.0.or.jsz1.le.0.or.isz2.le.0.or.jsz2.le.0)goto 9
if(mod(isz1,isz2).ne.0.or.mod(jsz1,jsz2).ne.0)goto 9

! compute mesh ratios
ir=isz1/isz2
jr=jsz1/jsz2
!
!  mesh2   |-------x-------|-------x-------|
!  mesh1   |-x-|-x-|-x-|-x-|-x-|-x-|-x-|-x-| 

!  mesh2   |-----x-----|-----x-----|  rx=3
!  mesh1   |-x-|-x-|-x-|-x-|-x-|-x-| 
!  i2            1   1   1   2
!  i1        1   2   3   4   5
!  ioff          0   1   2   0
!  tx            0  1/3 2/3

!  mesh2   |---x---|---x---| rx=2
!  mesh1   |-x-|-x-|-x-|-x-| 
!  i2            1   1   2  
!  i1            2   3   4
!  ioff          0   1   2   
!  tx           1/4 3/4


! offset of the last node in the 1st half of the cell
ih=ir/2
jh=jr/2
! 0 if coarse cell center coincides with fine, 1 if not
ip=mod(ir+1,2)
jp=mod(jr+1,2)

call interpolate_2d_w(ip,jp,ih,jh,ir,jr,              &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  ! in  
      ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,v1  )  ! out

return

9 continue
!$OMP CRITICAL(FIRE_UTIL_CRIT)
write(msg,91)ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
call message(msg)
write(msg,91)ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
call message(msg)
write(msg,92)'input  mesh size:',isz2,jsz2
call message(msg)
91 format('dimensions: ',8i8)
write(msg,92)'output mesh size:',isz1,jsz1
call message(msg)
92 format(a,2i8)
call crash("module_fr_fire_util:interpolate_2dmesh_cells: bad mesh sizes")
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
end subroutine interpolate_2d_cells2cells

!
!****************
!

subroutine interpolate_2d_cells2nodes(              &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  ! in  
      ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,v1  )  ! out
implicit none

!*** purpose
! interpolate nodal values in mesh2 to nodal values in mesh1
! input mesh 2 is coarse output mesh 1 is fine

!*** arguments

integer, intent(in)::ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
real, intent(out)::v1(ims1:ime1,jms1:jme1)
integer, intent(in)::ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
real, intent(in)::v2(ims2:ime2,jms2:jme2)

! Example with mesh ratio=4. | = cell boundary,  x = cell center
!
!  mesh2   |-------x-------|-------x-------|
!  mesh1   x-|-x-|-x-|-x-|-x-|-x-|-x-|-x-|-x 
!

!*** local
integer:: ir,jr,isz1,isz2,jsz1,jsz2,ip,jp,ih,jh
character(len=128)msg

!*** executable

!check mesh dimensions and domain dimensions
call check_mesh_2dim(ids1,ide1+1,jds1,jde1+1,ims1,ime1,jms1,jme1)
call check_mesh_2dim(ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2)

! compute mesh sizes
isz1 = ide1-ids1+1
jsz1 = jde1-jds1+1
isz2 = ide2-ids2+1
jsz2 = jde2-jds2+1

! check mesh sizes
if(isz1.le.0.or.jsz1.le.0.or.isz2.le.0.or.jsz2.le.0)goto 9
if(mod(isz1,isz2).ne.0.or.mod(jsz1,jsz2).ne.0)goto 9

! compute mesh ratios
ir=isz1/isz2
jr=jsz1/jsz2
!
!  mesh2   |-------x-------|-------x-------|
!  mesh1   x-|-x-|-x-|-x-|-x-|-x-|-x-|-x-|-x 

!  mesh2   |-----x-----|-----x-----|  rx=3
!  mesh1   x-|-x-|-x-|-x-|-x-|-x-|-x 

!  mesh2   |---x---|---x---| rx=2
!  mesh1   x-|-x-|-x-|-x-|-x 

! offset of the last node in the 1st half of the cell
ih=(ir+1)/2
jh=(jr+1)/2
! 0 if coarse cell center coincides with fine, 1 if not
ip=mod(ir,2)
jp=mod(jr,2)


call interpolate_2d_w(ip,jp,ih,jh,ir,jr,              &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  ! in  
      ids1,ide1+1,jds1,jde1+1,ims1,ime1,jms1,jme1,v1  )  ! out


return
9 continue
!$OMP CRITICAL(FIRE_UTIL_CRIT)
write(msg,91)ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
call message(msg)
write(msg,91)ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
call message(msg)
write(msg,92)'input  mesh size:',isz2,jsz2
call message(msg)
91 format('dimensions: ',8i8)
write(msg,92)'output mesh size:',isz1,jsz1
call message(msg)
92 format(a,2i8)
call crash("module_fr_fire_util:interpolate_2d_cells2nodes: bad mesh sizes")
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
end subroutine interpolate_2d_cells2nodes
!
!****************
!

subroutine interpolate_2d_w(ip,jp,ih,jh,ir,jr,             &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  ! in  
      ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,v1  )  ! out
implicit none
!*** EXCEPTION: THIS SUBROUTINE IS NEITHER CELL NOR NODE BASED.

integer, intent(in)::ip,jp,ih,jh,ir,jr
integer, intent(in)::ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
real, intent(out)::v1(ims1:ime1,jms1:jme1)
integer, intent(in)::ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
real, intent(in)::v2(ims2:ime2,jms2:jme2)

real:: tx,ty,rx,ry,half,xoff,yoff
integer:: i1,i2,j1,j2,ioff,joff
parameter(half=0.5)

rx=ir
ry=jr

xoff = ip*half
yoff = jp*half

! the inside, ids1+ih:ide1-ih,jds1+jh:jde1-jh 
do j2=jds2,jde2-1     ! interpolate from nodes j2 and j2+1
    do i2=ids2,ide2-1
        do ioff=0,ir-ip
            do joff=0,jr-jp
                ! compute fine mesh index
                i1=ioff+(ih+ids1)+ir*(i2-ids2)
                j1=joff+(jh+jds1)+jr*(j2-jds2)
                ! weights
                tx = (ioff+xoff)/rx
                ty = (joff+yoff)/ry
                ! interpolation
                v1(i1,j1)=                     &
                      (1-tx)*(1-ty)*v2(i2,j2)  &
                 +    (1-tx)*ty  *v2(i2,j2+1)  &
                 +      tx*(1-ty)*v2(i2+1,j2)  &
                 +        tx*ty  *v2(i2+1,j2+1)  
                !write(*,'(3(a,2i5),a,2f7.4)')'coarse ',i2,j2,' fine ',i1,j1, &
                ! ' offset ',ioff,joff,' weights ',tx,ty
                !write(*,'(a,4f7.4,a,f7.4)')'in ',v2(i2,j2),v2(i2,j2+1),v2(i2+1,j2), &
                !  v2(i2+1,j2+1),' out ',v1(i1,j1)
           enddo
       enddo
    enddo
enddo

! extend to the boundary strips from the nearest known
do ioff=0,ih-1  ! top and bottom strips
    do j2=jds2,jde2-1
        do joff=0,jr-jp
           j1=joff+(jh+jds1)+jr*(j2-jds2)
           ! weights
           ty = (joff+yoff)/ry
           ! interpolation
           v1(ids1+ioff,j1)=(1-ty)*v2(ids2,j2)+ty*v2(ids2,j2+1)
           v1(ide1-ioff,j1)=(1-ty)*v2(ide2,j2)+ty*v2(ide2,j2+1)
       enddo
    enddo
enddo
do joff=0,jh-1  ! left and right strips
    do i2=ids2,ide2-1
        do ioff=0,ir-ip
           i1=ioff+(ih+ids1)+ir*(i2-ids2)
           ! weights
           tx = (ioff+xoff)/rx
           ! interpolation
           v1(i1,jds1+joff)=(1-tx)*v2(i2,jds2)+tx*v2(i2+1,jds2)
           v1(i1,jde1-joff)=(1-tx)*v2(i2,jde2)+tx*v2(i2+1,jde2)
       enddo
    enddo
enddo
! extend to the 4 corner squares from the nearest known
do ioff=0,ih-1  
    do joff=0,jh-1
        v1(ids1+ioff,jds1+joff)=v2(ids2,jds2)
        v1(ide1-ioff,jds1+joff)=v2(ide2,jds2)
        v1(ids1+ioff,jde1-joff)=v2(ids2,jde2)
        v1(ide1-ioff,jde1-joff)=v2(ide2,jde2)
    enddo
enddo         
end subroutine interpolate_2d_w  

!
!****************
!
                
real function interp(ids,ide,jds,jde,ims,ime,jms,jme,x,y,v)
implicit none
!*** purpose
! general interpolation in a rectangular

!*** arguments

integer, intent(in)::ids,ide,jds,jde,ims,ime,jms,jme
real, intent(in)::x,y,v(ims:ime,jms:jme)
! the mesh is cell based so the used dimension of v is ids:ide+1,jds:jde+1

!*** calls
intrinsic floor,min,max

!*** local
integer i,j
real tx,ty

! executable

! indices of the lower left corner of the cell in the mesh that contains (x,y)
i = floor(x)
i=max(min(i,ide),ids)
j = floor(y)
j=max(min(j,jde),jds)

! the leftover
tx = x - real(i)
ty = y - real(j)

! interpolate the values
interp = &
                    (1-tx)*(1-ty)*v(i,j)    &
                 +    tx*(1-ty)  *v(i+1,j)  &
                 +    (1-tx)*ty  *v(i,j+1)  &
                 +        tx*ty  *v(i+1,j+1)  

!print *,'x,y=',x,y,'i1,i2=',i1,j1,'tx,ty=',tx,ty,' interp=',interp
end function interp

subroutine meshdiffc_2d(ids, ide, jds,jde , &    ! mesh area used (in cells, end +1)
                   ims1,ime1,jms1,jme1, &       ! memory dimensiuons 
                   dx,dy,               &       ! mesh spacing
                   lfn,                 &       ! input
                   diffCx,diffCy) ! output
implicit none

!*** purpose
! central differences on a 2d mesh

!*** arguments

integer, intent(in)::ids,ide,jds,jde,ims1,ime1,jms1,jme1
real, intent(in):: dx,dy
real, intent(in), dimension(ims1:ime1,jms1:jme1):: lfn
real, intent(out), dimension(ims1:ime1,jms1:jme1):: diffCx,diffCy

!*** local
integer:: i,j
real, dimension(ims1:ime1,jms1:jme1):: diffLx,diffRx,diffLy,diffRy

! get one-sided differences; dumb but had that already...
call meshdiff_2d(ids, ide, jds,jde , &    ! mesh area used (in cells, end +1)
                   ims1,ime1,jms1,jme1, &       ! dimensions of lfn 
                   dx,dy,               &       ! mesh spacing
                   lfn,                 &       ! input
                   diffLx,diffRx,diffLy,diffRy) ! output

! make into central
do j=jds,jde+1
    do i=ids,ide+1
        diffCx(i,j)=0.5*(diffLx(i,j) + diffRx(i,j))
        diffCy(i,j)=0.5*(diffLy(i,j) + diffRy(i,j))
    enddo
enddo
end subroutine meshdiffc_2d 

subroutine meshdiff_2d(ids, ide, jds,jde , &    ! mesh area used (in cells, end +1)
                   ims1,ime1,jms1,jme1, &       ! dimensions of lfn 
                   dx,dy,               &       ! mesh spacing
                   lfn,                 &       ! input
                   diffLx,diffRx,diffLy,diffRy) ! output
implicit none

!*** purpose
! one-sided differences on a 2d mesh

!*** arguments

integer, intent(in)::ids,ide,jds,jde,ims1,ime1,jms1,jme1
real, intent(in):: dx,dy
real, intent(in), dimension(ims1:ime1,jms1:jme1):: lfn
real, intent(out), dimension(ims1:ime1,jms1:jme1):: diffLx,diffRx,diffLy,diffRy

!*** local
integer:: i,j
real:: tmpx,tmpy

!*** executable

    call check_mesh_2dim(ids,ide+1,jds,jde+1,ims1,ime1,jms1,jme1)
  
    ! the bulk of the work
    do j=jds,jde
        do i=ids,ide
            tmpx = (lfn(i+1,j)-lfn(i,j))/dx
            diffLx(i+1,j) = tmpx
            diffRx(i,j)   = tmpx
            tmpy = (lfn(i,j+1)-lfn(i,j))/dy
            diffLy(i,j+1) = tmpy
            diffRy(i,j)   = tmpy
        enddo
        ! missing values - put there the other one
        diffLx(ids,j)  = diffLx(ids+1,j)
        diffRx(ide+1,j)= diffRx(ide,j)
    enddo
    ! cleanup
    ! j=jde+1 from above loop
    do i=ids,ide
        tmpx = (lfn(i+1,j)-lfn(i,j))/dx
        diffLx(i+1,j) = tmpx
        diffRx(i,j)   = tmpx
    enddo
    ! i=ide+1 from above loop
    do j=jds,jde
        tmpy = (lfn(i,j+1)-lfn(i,j))/dy
        diffLy(i,j+1) = tmpy
        diffRy(i,j)   = tmpy
    enddo
    ! missing values - put there the other one
    ! j=jde+1 from above loop, j=jds:jde done before in main bulk loop
    diffLx(ids,j)   = diffLx(ids+1,j)
    diffRx(ide+1,j) = diffRx(ide,j)
    do i=ids,ide+1
        diffLy(i,jds)   = diffLy(i,jds+1)
        diffRy(i,jde+1) = diffRy(i,jde)
    enddo    

end subroutine meshdiff_2d




real pure function sum_2darray( its,ite,jts,jte,               &
                                ims,ime,jms,jme,               &
                                a)
integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme
real, intent(in)::a(ims:ime,jms:jme)
!*** local
integer:: i,j
real:: t
t=0.
do j=jts,jte
    do i=its,ite
        t=t+a(i,j)
    enddo
enddo
sum_2darray = t
end function sum_2darray

real pure function max_2darray( its,ite,jts,jte,               &
                                ims,ime,jms,jme,               &
                                a)
integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme
real, intent(in)::a(ims:ime,jms:jme)
!*** local
integer:: i,j
real:: t
t=0.
do j=jts,jte
    do i=its,ite
        t=max(t,a(i,j))
    enddo
enddo
max_2darray = t
end function max_2darray

subroutine print_2d_stats_vec(ips,ipe,jps,jpe, &
                         ims,ime,jms,jme, &
                         ax,ay,name)
implicit none
integer, intent(in)::ips,ipe,jps,jpe,ims,ime,jms,jme
real, intent(in), dimension(ims:ime,jms:jme)::ax,ay
character(len=*),intent(in)::name
integer:: i,j
real:: t 
real:: avg_a,max_a,min_a 
character(len=25)::id
id=name
call print_2d_stats(ips,ipe,jps,jpe, &
                         ims,ime,jms,jme, &
                         ax,id//'/x ')
call print_2d_stats(ips,ipe,jps,jpe, &
                         ims,ime,jms,jme, &
                         ay,id//'/y ')
avg_a=0
max_a=-huge(max_a)
min_a= huge(min_a)
do j=jps,jpe
    do i=ips,ipe
        t=sqrt(ax(i,j)**2+ay(i,j)**2)
        max_a=max(max_a,t)
        min_a=min(min_a,t)
        avg_a=avg_a+t
    enddo
enddo
avg_a = avg_a/((ipe-ips+1)*(jpe-jps+1))
call print_stat_line(id//'/sz',ips,ipe,jps,jpe,min_a,max_a,avg_a)
end subroutine print_2d_stats_vec


subroutine print_stat_line(name,ips,ipe,jps,jpe,min_a,max_a,avg_a)
!*** encapsulate line with statistics
implicit none
!*** arguments
integer, intent(in)::ips,ipe,jps,jpe
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
call message(msg)
if(.not.avg_a.eq.avg_a)call crash('NaN detected')
end subroutine print_stat_line


subroutine print_3d_stats_by_slice(ips,ipe,kps,kpe,jps,jpe, &
                         ims,ime,kms,kme,jms,jme, &
                         a,name)
implicit none
integer, intent(in)::ips,ipe,jps,jpe,ims,ime,jms,jme,kms,kme,kps,kpe
real, intent(in)::a(ims:ime,kms:kme,jms:jme)
character(len=*),intent(in)::name
integer::k
character(len=128)::msg
do k=kps,kpe
! print 3d stats for each horizontal slice separately
!$OMP CRITICAL(SFIRE_UTIL_CRIT)
    write(msg,'(i2,1x,a)')k,name
!$OMP END CRITICAL(SFIRE_UTIL_CRIT)
    call print_3d_stats(ips,ipe,k,k,jps,jpe, &
                         ims,ime,kms,kme,jms,jme, &
                         a,msg)
enddo
end subroutine print_3d_stats_by_slice 


subroutine print_3d_stats(ips,ipe,kps,kpe,jps,jpe, &
                         ims,ime,kms,kme,jms,jme, &
                         a,name)
implicit none
integer, intent(in)::ips,ipe,jps,jpe,ims,ime,jms,jme,kms,kme,kps,kpe
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
call print_stat_line(name,ips,ipe,jps,jpe,min_a,max_a,avg_a)
return
9 continue
!$OMP CRITICAL(FIRE_UTIL_CRIT)
write(msg,1)name,i,k,j,aa
call message(msg)
1 format(a30,'(',i6,',',i6,',',i6,') = ',g13.5)
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
call print_stat_line(name,ips,ipe,jps,jpe,aa,aa,aa)
if(aa.ne.aa)goto 10
msg='Invalid floating point number detected in '//name
call crash(msg)
10 msg='NaN detected in '//name
call crash(msg)
end subroutine print_3d_stats

subroutine print_2d_stats(ips,ipe,jps,jpe, &
                         ims,ime,jms,jme, &
                         a,name)
implicit none
integer, intent(in)::ips,ipe,jps,jpe,ims,ime,jms,jme
real, intent(in)::a(ims:ime,jms:jme)
character(len=*),intent(in)::name
!!character(len=128)::msg
!if(fire_print_msg.eq.0)return
call print_3d_stats(ips,ipe,1,1,jps,jpe, &
                         ims,ime,1,1,jms,jme, &
                         a,name)
!!write(msg,'(2a,z16)')name,' address =',loc(a)
!!call message(msg)
end subroutine print_2d_stats

real pure function avg_2darray( its,ite,jts,jte,               &
                                ims,ime,jms,jme,               &
                                a)
integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme
real, intent(in)::a(ims:ime,jms:jme)
!*** local
!*** executable
avg_2darray = sum_2darray( its,ite,jts,jte,               &
                           ims,ime,jms,jme,               &
                           a)/((ite-its+1)*(jte-jts+1))
end function avg_2darray

real pure function avg_2darray_vec( its,ite,jts,jte,           &
                                ims,ime,jms,jme,               &
                                ax,ay)
integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme
real, intent(in), dimension(ims:ime,jms:jme):: ax,ay
!*** local
integer:: i,j
real:: t
t=0.
do j=jts,jte
    do i=its,ite
        t=t+sqrt(ax(i,j)**2+ay(i,j)**2)
    enddo
enddo
t = t/((ite-its+1)*(jte-jts+1))
avg_2darray_vec = t
end function avg_2darray_vec


subroutine print_array(its,ite,jts,jte,           &
                         ims,ime,jms,jme,               &
                         a,name,id)
! debug
!*** arguments
integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme,id
real, intent(in), dimension(ims:ime,jms:jme):: a
character(len=*),intent(in)::name
!****
integer i,j
character(len=128)::msg
!****
!$OMP CRITICAL(FIRE_UTIL_CRIT)
write(msg,*)name,' start ',id,' dim ',its,ite,jts,jte
call message(msg)
do j=jts,jte
    do i=its,ite
         write(msg,*)i,j,a(i,j)
         call message(msg)
    enddo
enddo
write(msg,*)name,' end ',id
call message(msg)
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
end subroutine print_array

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
