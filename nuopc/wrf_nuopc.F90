
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
  use namelist_mod, only : namelist_t
  use datetime_mod, only : datetime_t


  implicit none

  private

  public SetServices

  type (wrf_t) :: state
  real(ESMF_KIND_R8), pointer     :: ptr_z0(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_q2(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_t2(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_u3d(:,:,:)
  integer                         :: clb(2), cub(2), clb3(3), cub3(3)
  type (namelist_t) :: config_flags

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

    call NUOPC_CompSpecialize (model, specLabel = label_SetClock, specRoutine = SetClock, rc = rc)
    if (ESMF_LogFoundError (rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line = __LINE__, file = __FILE__)) &
        return

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

    call config_flags%Init_time_block ('namelist.input')
    call config_flags%Init_atm_block ('namelist.input')

    state = wrf_t(file_name = 'wrf.nc')
    allocate(state%t2(size(state%lats, dim=1), size(state%lats, dim=2)))
    allocate(state%z0(size(state%lats, dim=1), size(state%lats, dim=2)))
    allocate(state%u3d(size(state%lats, dim=1), size(state%lats, dim=2), state%bottom_top))
    ! Import/ Export Variables -----------------------------------------------------

    ! Disabling the following macro, e.g. renaming to WITHIMPORTFIELDS_disable,
    ! will result in a model component that does not advertise any importable
    ! Fields. Use this if you want to drive the model independently.

#define WITHEXPORTFIELDS
#ifdef WITHEXPORTFIELDS
    ! exportable field: inst_surface_roughness
    call NUOPC_Advertise(exportState, &
      StandardName="inst_zonal_wind_levels", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: inst_surface_roughness
    call NUOPC_Advertise(exportState, &
      StandardName="inst_surface_roughness", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: inst_spec_humid_height2m
    call NUOPC_Advertise(exportState, &
      StandardName="inst_spec_humid_height2m", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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
     ! exportable field on Grid: inst_zonal_wind_levels
     field = ESMF_FieldCreate(name="inst_zonal_wind_levels", grid=grid, &
       gridToFieldMap=(/1,2/), ungriddedLBound=(/1/), &
       ungriddedUBound=(/state%bottom_top/), &
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
     call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_u3d, &
       computationalLBound=clb3, computationalUBound=cub3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field on Grid: inst_surface_roughness
    field = ESMF_FieldCreate(name="inst_surface_roughness", grid=grid, &
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
    call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_z0, &
      computationalLBound=clb, computationalUBound=cub, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field on Grid: inst_spec_humid_height2m
    field = ESMF_FieldCreate(name="inst_spec_humid_height2m", grid=grid, &
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
    call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_q2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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
    call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_t2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#endif

  end subroutine

  subroutine SetClock (model, rc)

    implicit none

    type(ESMF_GridComp) :: model
    integer, intent(out) :: rc

    type (ESMF_Clock) :: modelClock
    type (ESMF_Time) :: startTime
    type (ESMF_Time) :: stopTime
    type (ESMF_TimeInterval) :: timeStep


    rc = ESMF_SUCCESS

    call ESMF_TimeIntervalSet (timeStep, s = config_flags%interval_atm, rc = rc)
    if (ESMF_LogFoundError (rcToCheck = rc, msg = ESMF_LOGERR_PASSTHRU, line = __LINE__, file = __FILE__)) &
        return

    call ESMF_TimeSet (startTime, yy = config_flags%start_year, mm = config_flags%start_month, &
        dd = config_flags%start_day, h = config_flags%start_hour, m = config_flags%start_minute, &
        s = config_flags%start_second, calkindflag = ESMF_CALKIND_GREGORIAN, rc = rc)
    if (ESMF_LogFoundError (rcToCheck = rc, msg = ESMF_LOGERR_PASSTHRU, line = __LINE__, file = __FILE__)) &
        return

    call ESMF_TimeSet (stopTime, yy = config_flags%end_year, mm = config_flags%end_month, &
        dd = config_flags%end_day, h = config_flags%end_hour, m = config_flags%end_minute, &
        s = config_flags%end_second, calkindflag = ESMF_CALKIND_GREGORIAN, rc = rc)
    if (ESMF_LogFoundError (rcToCheck = rc, msg = ESMF_LOGERR_PASSTHRU, line = __LINE__, file = __FILE__)) &
        return

    modelClock = ESMF_ClockCreate (name = "WRFdata Clock", timeStep = timeStep, startTime = startTime, stopTime = stopTime, rc = rc)
    if (ESMF_LogFoundError (rcToCheck = rc, msg = ESMF_LOGERR_PASSTHRU, line = __LINE__, file = __FILE__)) &
        return

    call ESMF_GridCompSet (model, clock = modelClock, rc = rc)
    if (ESMF_LogFoundError (rcToCheck = rc, msg = ESMF_LOGERR_PASSTHRU, line = __LINE__, file = __FILE__)) &
        return

  end subroutine

  subroutine Advance(model, rc)

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: currTime
    ! type(ESMF_TimeInterval)     :: timeStep
    ! type(ESMF_VM)               :: vm
    ! integer                     :: currentSsiPe
     character(len=160)          :: msgString
    integer :: yy_now, mm_now, dd_now, h_now, m_now, s_now, ms
    real (kind = ESMF_KIND_R8) :: s_r8
    type (datetime_t) :: datetime_now

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

      ! Get current time stamp
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_TimeGet(time=currTime, yy  = yy_now, mm = mm_now, dd = dd_now, h = h_now, m = m_now, s = s_now, s_r8 = s_r8, ms = ms, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    datetime_now = datetime_t (yy_now, mm_now, dd_now, h_now, m_now, s_now)
    call datetime_now%Print_datetime ()

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

    
      ! "Run" atmospheric model
    call state%Get_t2(datetime_now)
    call state%Get_q2(datetime_now)
    call state%Get_z0(datetime_now)
    call state%Get_u3d(datetime_now)

    ! Set field data
    ptr_z0(clb(1):cub(1),clb(2):cub(2))= & ! Just set it to this for testing 
      state%z0(1:size(state%lats, dim=1),1:size(state%lats, dim=2)) 
    ptr_q2(clb(1):cub(1),clb(2):cub(2))= & ! Just set it to this for testing
      state%q2(1:size(state%lats, dim=1),1:size(state%lats, dim=2))
    ptr_t2(clb(1):cub(1),clb(2):cub(2))= & ! Just set it to this for testing
      state%t2(1:size(state%lats, dim=1),1:size(state%lats, dim=2))
    ptr_u3d(clb3(1):cub3(1),clb3(2):cub3(2),clb3(3):cub3(3))= & ! Just set it to this for testing
      state%u3d(1:size(state%lats, dim=1),1:size(state%lats, dim=2), 1:state%bottom_top)
!    call state%Destroy_t2 ()

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing WRFdata model from: ", unit=msgString, rc=rc)
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

end module wrf_nuopc
