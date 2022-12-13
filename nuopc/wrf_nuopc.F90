
#include "Fire_behavior_NUOPC_Macros.h"

module wrf_nuopc

  !-----------------------------------------------------------------------------
  ! offline WRF component
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS    => SetServices
  use wrf_mod, only : wrf_t


  implicit none

  private

  public SetServices

  type (wrf_t) :: state
  real(ESMF_KIND_R8), pointer     :: ptr_t2(:,:)
  integer                         :: clb(2), cub(2)

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Model
    call NUOPC_CompDerive(model, modelSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! specialize model
    call NUOPC_CompSpecialize(model, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, &
      specRoutine=Realize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=Advance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advertise(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Eventually, you will advertise your model's import and
    ! export fields in this phase.  For now, however, call
    ! your model's initialization routine(s).

    state = wrf_t(file_name = 'wrf.nc')
    allocate(state%t2(size(state%lats, dim=1), size(state%lats, dim=2)))
    state%t2 = 300.0
    ! Import/ Export Variables -----------------------------------------------------

    ! Disabling the following macro, e.g. renaming to WITHIMPORTFIELDS_disable,
    ! will result in a model component that does not advertise any importable
    ! Fields. Use this if you want to drive the model independently.

#define WITHEXPORTFIELDS
#ifdef WITHEXPORTFIELDS
    ! exportable field: inst_temp_height2m
    call NUOPC_Advertise(exportState, &
      StandardName="inst_temp_height2m", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    ! type(ESMF_TimeInterval) :: stabilityTimeStep
    type(ESMF_Field)        :: field
    type(ESMF_DistGrid)     :: distgrid
    type(ESMF_Grid)         :: grid

    ! type(ESMF_VM)                   :: vm

    ! working local variables
    integer                        :: lbnd(2),ubnd(2)
    real(ESMF_KIND_COORD), pointer :: coordXcenter(:,:)
    real(ESMF_KIND_COORD), pointer :: coordYcenter(:,:)
    real(ESMF_KIND_COORD), pointer :: coordXcorner(:,:)
    real(ESMF_KIND_COORD), pointer :: coordYcorner(:,:)
    integer                        :: i, j, nx, ny

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    nx = size(state%lats, dim=1)
    ny = size(state%lons, dim=2)

    ! Create distgrid based on the state grid
    distgrid = ESMF_DistGridCreate( &
      minIndex=(/1,1/), maxIndex=(/nx,ny/), &
      rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    grid = ESMF_GridCreate(name='ATM', & 
      distgrid=distgrid, coordSys = ESMF_COORDSYS_SPH_DEG, &
      rc = rc)
    if(ESMF_STDERRORCHECK(rc)) return

    ! Add Center Coordinates to Grid
    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=coordXcenter, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=coordYcenter, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    do j = lbnd(2),ubnd(2)
    do i = lbnd(1),ubnd(1)
      coordXcenter(i,j) = state%lons(i,j)
      coordYcenter(i,j) = state%lats(i,j)
    enddo
    enddo

    ! CORNERS

    ! Add Corner Coordinates to Grid
    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CORNER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=coordXcorner, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=coordYcorner, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    do j = lbnd(2),ubnd(2)
      do i = lbnd(1),ubnd(1)
        coordXcorner(i,j) = state%lons_c(i,j)
        coordYcorner(i,j) = state%lats_c(i,j)
      enddo
    enddo

#ifdef WITHEXPORTFIELDS
    ! exportable field on Grid: inst_temp_height2m
    field = ESMF_FieldCreate(name="inst_temp_height2m", grid=grid, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Get Field memory
    call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_t2, &
      computationalLBound=clb, computationalUBound=cub, rc=rc)

#endif


  end subroutine

  ! !-----------------------------------------------------------------------------

  ! subroutine SetClock(model, rc)
  !   type(ESMF_GridComp)  :: model
  !   integer, intent(out) :: rc

  !   ! local variables
  !   type(ESMF_Clock)              :: clock
  !   type(ESMF_TimeInterval)       :: stabilityTimeStep

  !   rc = ESMF_SUCCESS

  !   ! query for clock
  !   call NUOPC_ModelGet(model, modelClock=clock, rc=rc)
  !   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  !     line=__LINE__, &
  !     file=__FILE__)) &
  !     return  ! bail out

  !   ! initialize internal clock
  !   ! here: parent Clock and stability timeStep determine actual model timeStep
  !   !TODO: stabilityTimeStep should be read in from configuation
  !   !TODO: or computed from internal Grid information
  !   call ESMF_TimeIntervalSet(stabilityTimeStep, m=5, rc=rc) ! 5 minute steps
  !   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  !     line=__LINE__, &
  !     file=__FILE__)) &
  !     return  ! bail out
  !   call NUOPC_CompSetClock(model, clock, stabilityTimeStep, rc=rc)
  !   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  !     line=__LINE__, &
  !     file=__FILE__)) &
  !     return  ! bail out

  ! end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advance(model, rc)

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    ! type(ESMF_Time)             :: currTime
    ! type(ESMF_TimeInterval)     :: timeStep
    ! type(ESMF_VM)               :: vm
    ! integer                     :: currentSsiPe
     character(len=160)          :: msgString

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! ! Query for VM
    ! call ESMF_GridCompGet(model, vm=vm, rc=rc)
    ! if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !   line=__LINE__, &
    !   file=__FILE__)) &
    !   return  ! bail out

    ! call ESMF_VMLog(vm, "LUMO Advance(): ", ESMF_LOGMSG_INFO, rc=rc)
    ! if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !   line=__LINE__, &
    !   file=__FILE__)) &
    !   return  ! bail out

    
    ! call Run_atmospheric_model ()

    ! Set field data
    ptr_t2(clb(1):cub(1),clb(2):cub(2))= & ! Just set it to this for testing 
      state%t2(1:size(state%lats, dim=1),1:size(state%lats, dim=2)) 

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing Fire model from: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="---------------------> to: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

!  subroutine Init_fire_behavior_model ()
!
!    implicit none
!    call Init_state (grid, config_flags)
!
!  end subroutine Init_fire_behavior_model

  !-----------------------------------------------------------------------------

!  subroutine Run_fire_behavior_model ()
!
!    implicit none
!
!    do n = 1, config_flags%n_steps
!       call Advance_state (grid, config_flags)
!    end do
!  end subroutine Run_fire_behavior_model

  !-----------------------------------------------------------------------------

end module wrf_nuopc
