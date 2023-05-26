
#include "Fire_behavior_NUOPC_Macros.h"

module fire_behavior_nuopc

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS    => SetServices

  use state_mod, only : state_fire_t
  use namelist_mod, only : namelist_t
  use initialize_mod, only : Init_fire_state
  use advance_mod, only : Advance_state

  implicit none

  private

  public SetVM, SetServices

  type (state_fire_t) :: grid
  type (namelist_t) :: config_flags
  real(ESMF_KIND_R8), pointer     :: ptr_z0(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_t2(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_psfc(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_rain(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_q2(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_u3d(:,:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_v3d(:,:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_ph(:,:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_pres(:,:,:)
  integer :: clb(2), cub(2), clb3(3), cub3(3)

  contains

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

      ! Read namelist
    call config_flags%Initialization (file_name = 'namelist.input')

    call Init_fire_state (grid, config_flags)

    ! Import/ Export Variables -----------------------------------------------------

    ! Disabling the following macro, e.g. renaming to WITHIMPORTFIELDS_disable,
    ! will result in a model component that does not advertise any importable
    ! Fields. Use this if you want to drive the model independently.

#define WITHIMPORTFIELDS
#ifdef WITHIMPORTFIELDS
    ! 3D fields

    ! importable field: inst_zonal_wind_levels
    call NUOPC_Advertise(importState, &
      StandardName="inst_zonal_wind_levels", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: inst_merid_wind_levels
    call NUOPC_Advertise(importState, &
      StandardName="inst_merid_wind_levels", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: inst_geop_levels
    call NUOPC_Advertise(importState, &
      StandardName="inst_geop_levels", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: inst_pres_levels
    call NUOPC_Advertise(importState, &
      StandardName="inst_pres_levels", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

!    !  2D fields

    ! importable field: inst_surface_roughness
    call NUOPC_Advertise(importState, &
      StandardName="inst_surface_roughness", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: inst_rainfall_amount
    call NUOPC_Advertise(importState, &
      StandardName="inst_rainfall_amount", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: inst_spec_humid_height2m
    call NUOPC_Advertise(importState, &
      StandardName="inst_spec_humid_height2m", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: inst_pres_height_surface
    call NUOPC_Advertise(importState, &
      StandardName="inst_pres_height_surface", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: inst_temp_height2m
    call NUOPC_Advertise(importState, &
      StandardName="inst_temp_height2m", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

!#define WITHEXPORTFIELDS_disable
!#ifdef WITHEXPORTFIELDS
!    ! exportable field: fire_temperature_tendency
!    call NUOPC_Advertise(exportState, &
!      StandardName="fire_temperature_tendency", name="fire_temperature_tendency", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!#endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    ! type(ESMF_TimeInterval) :: stabilityTimeStep
    type(ESMF_Field)        :: field
    type(ESMF_DistGrid)     :: fire_distgrid
    type(ESMF_Grid)         :: fire_grid

    ! integer, parameter              :: totalNumPoints=100
    ! integer(ESMF_KIND_I4), pointer  :: mask(:)
    ! real(ESMF_KIND_R8), pointer     :: lon(:), lat(:)
    ! type(ESMF_VM)                   :: vm

    ! working local variables
    integer                        :: lbnd(2),ubnd(2)
    real(ESMF_KIND_COORD), pointer :: coordXcenter(:,:)
    real(ESMF_KIND_COORD), pointer :: coordYcenter(:,:)
    real(ESMF_KIND_COORD), pointer :: coordXcorner(:,:)
    real(ESMF_KIND_COORD), pointer :: coordYcorner(:,:)
    integer                        :: i, j

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Create distgrid based on the state grid
    fire_distgrid = ESMF_DistGridCreate( &
      minIndex=(/1,1/), maxIndex=(/grid%nx,grid%ny/), &
      rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    fire_grid = ESMF_GridCreate(name='FIRE_BEHAVIOR', & 
      distgrid=fire_distgrid, coordSys = ESMF_COORDSYS_SPH_DEG, &
!      coordTypeKind=ESMF_TYPEKIND_COORD, & ?
!      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
      rc = rc)
    if(ESMF_STDERRORCHECK(rc)) return

    if (allocated(grid%lats) .and. allocated (grid%lons)) then

      ! CENTERS

      ! Add Center Coordinates to Grid
      call ESMF_GridAddCoord(fire_grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
      call ESMF_GridGetCoord(fire_grid, coordDim=1, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        computationalLBound=lbnd, computationalUBound=ubnd, &
        farrayPtr=coordXcenter, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_GridGetCoord(fire_grid, coordDim=2, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=coordYcenter, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      do j = lbnd(2),ubnd(2)
      do i = lbnd(1),ubnd(1)
        coordXcenter(i,j) = grid%lons(i,j)
        coordYcenter(i,j) = grid%lats(i,j)
      enddo
      enddo

      ! CORNERS

      ! Add Corner Coordinates to Grid
      call ESMF_GridAddCoord(fire_grid, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
      call ESMF_GridGetCoord(fire_grid, coordDim=1, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CORNER, &
        computationalLBound=lbnd, computationalUBound=ubnd, &
        farrayPtr=coordXcorner, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_GridGetCoord(fire_grid, coordDim=2, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=coordYcorner, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      do j = lbnd(2),ubnd(2)
      do i = lbnd(1),ubnd(1)
        coordXcorner(i,j) = grid%lons_c(i,j)
        coordYcorner(i,j) = grid%lats_c(i,j)
      enddo
      enddo
    end if

#ifdef WITHIMPORTFIELDS
     !  3D fields

     ! importable field on Grid: inst_zonal_wind_levels
     field = ESMF_FieldCreate(name="inst_zonal_wind_levels", grid=fire_grid, &
       gridToFieldMap=(/1,2/), ungriddedLBound=(/1/), &
       ungriddedUBound=(/grid%kfde - 1/), &
       typekind=ESMF_TYPEKIND_R8, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call NUOPC_Realize(importState, field=field, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     ! Get Field memory
     call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_u3d, &
       computationalLBound=clb3, computationalUBound=cub3, rc=rc)

     ! importable field on Grid: inst_zonal_wind_levels
     field = ESMF_FieldCreate(name="inst_merid_wind_levels", grid=fire_grid, &
       gridToFieldMap=(/1,2/), ungriddedLBound=(/1/), &
       ungriddedUBound=(/grid%kfde - 1/), &
       typekind=ESMF_TYPEKIND_R8, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call NUOPC_Realize(importState, field=field, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     ! Get Field memory
     call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_v3d, rc=rc)

     ! importable field on Grid: inst_geop_levels
     field = ESMF_FieldCreate(name="inst_geop_levels", grid=fire_grid, &
       gridToFieldMap=(/1,2/), ungriddedLBound=(/1/), &
       ungriddedUBound=(/grid%kfde - 1/), &
       typekind=ESMF_TYPEKIND_R8, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call NUOPC_Realize(importState, field=field, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     ! Get Field memory
     call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_ph, rc=rc)

     ! importable field on Grid: inst_pres_levels
     field = ESMF_FieldCreate(name="inst_pres_levels", grid=fire_grid, &
       gridToFieldMap=(/1,2/), ungriddedLBound=(/1/), &
       ungriddedUBound=(/grid%kfde - 1/), &
       typekind=ESMF_TYPEKIND_R8, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call NUOPC_Realize(importState, field=field, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     ! Get Field memory
     call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_pres, rc=rc)

     !  2D fields

     ! importable field on Grid: inst_surface_roughness
     field = ESMF_FieldCreate(name="inst_surface_roughness", grid=fire_grid, &
       typekind=ESMF_TYPEKIND_R8, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call NUOPC_Realize(importState, field=field, rc=rc)
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

     ! importable field on Grid: inst_rainfall_amount
     field = ESMF_FieldCreate(name="inst_rainfall_amount", grid=fire_grid, &
       typekind=ESMF_TYPEKIND_R8, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call NUOPC_Realize(importState, field=field, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     ! Get Field memory
     call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_rain, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out


     ! importable field on Grid: inst_spec_humid_height2m
     field = ESMF_FieldCreate(name="inst_spec_humid_height2m", grid=fire_grid, &
       typekind=ESMF_TYPEKIND_R8, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call NUOPC_Realize(importState, field=field, rc=rc)
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

     ! importable field on Grid: inst_pres_height_surface
     field = ESMF_FieldCreate(name="inst_pres_height_surface", grid=fire_grid, &
       typekind=ESMF_TYPEKIND_R8, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call NUOPC_Realize(importState, field=field, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     ! Get Field memory
     call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_psfc, rc=rc) 
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

     ! importable field on Grid: inst_temp_height2m
     field = ESMF_FieldCreate(name="inst_temp_height2m", grid=fire_grid, &
       typekind=ESMF_TYPEKIND_R8, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call NUOPC_Realize(importState, field=field, rc=rc)
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

! #ifdef WITHEXPORTFIELDS
!     ! exportable field on Grid: fire_temperature_tendency
!     field = ESMF_FieldCreate(name="fire_temperature_tendency", grid=gridOut, &
!       typekind=ESMF_TYPEKIND_R8, rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out
!     call NUOPC_Realize(exportState, field=field, rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out
! #endif

  end subroutine

  subroutine SetClock(model, rc)

    implicit none

    type(ESMF_GridComp) :: model
    integer, intent(out) :: rc

    type (ESMF_Clock) :: modelClock
    type (ESMF_Time) :: startTime
    type (ESMF_Time) :: stopTime
    type (ESMF_TimeInterval) :: timeStep


    rc = ESMF_SUCCESS

    call ESMF_TimeIntervalSet (timeStep, s_r8 = real (config_flags%dt, kind = ESMF_KIND_R8), rc = rc)
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

    modelClock = ESMF_ClockCreate (name = "Fire Clock", timeStep = timeStep, startTime = startTime, stopTime = stopTime, rc = rc)
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
    ! type(ESMF_Time)             :: currTime
    ! type(ESMF_TimeInterval)     :: timeStep
    ! type(ESMF_VM)               :: vm
    ! integer                     :: currentSsiPe
    integer                     :: i, j
    character(len=160)          :: msgString
    real, dimension(:, :, :), allocatable :: atm_u3d, atm_v3d, atm_ph, atm_pres

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

#ifdef WITHIMPORTFIELDS
    ! Update atmospheric fields
    grid%fz0(1:grid%nx,1:grid%ny) = ptr_z0(clb(1):cub(1),clb(2):cub(2))
    grid%fire_q2(1:grid%nx,1:grid%ny) = ptr_q2(clb(1):cub(1),clb(2):cub(2))
    grid%fire_t2(1:grid%nx,1:grid%ny) = ptr_t2(clb(1):cub(1),clb(2):cub(2))
    grid%fire_psfc(1:grid%nx,1:grid%ny) = ptr_psfc(clb(1):cub(1),clb(2):cub(2))
    grid%fire_rain(1:grid%nx,1:grid%ny) = ptr_rain(clb(1):cub(1),clb(2):cub(2))

    do j = grid%jfds, grid%jfde
      do i = grid%ifds, grid%ifde
        grid%fire_q2(i,j) = max (grid%fire_q2(i,j), .001)
        grid%fire_t2(i,j) = max (grid%fire_t2(i,j), .001)
        grid%fire_psfc(i,j) = max (grid%fire_psfc(i,j), .001)
!        grid%fire_rain(i,j) = max (grid%fire_t2(i,j), .001)
      end do
    end do 


    allocate (atm_u3d(1:grid%nx,1:grid%ny,1:grid%kfde - 1))
    allocate (atm_v3d(1:grid%nx,1:grid%ny,1:grid%kfde - 1))
    allocate (atm_ph(1:grid%nx,1:grid%ny,1:grid%kfde - 1))
    allocate (atm_pres(1:grid%nx,1:grid%ny,1:grid%kfde - 1))

    atm_u3d(1:grid%nx,1:grid%ny,1:grid%kfde - 1)  = ptr_u3d(clb3(1):cub3(1),clb3(2):cub3(2),clb3(3):cub3(3))
    atm_v3d(1:grid%nx,1:grid%ny,1:grid%kfde - 1)  = ptr_v3d(clb3(1):cub3(1),clb3(2):cub3(2),clb3(3):cub3(3))
    atm_ph(1:grid%nx,1:grid%ny,1:grid%kfde - 1)   = ptr_ph(clb3(1):cub3(1),clb3(2):cub3(2),clb3(3):cub3(3))
    atm_pres(1:grid%nx,1:grid%ny,1:grid%kfde - 1) = ptr_pres(clb3(1):cub3(1),clb3(2):cub3(2),clb3(3):cub3(3))
#endif
    
    do j = 1, grid%jfde
      do i = 1, grid%ifde
        call grid%Interpolate_wind3d (config_flags,  & ! for debug output, <= 0 no output
            config_flags%fire_wind_height,           & ! interpolation height
            grid%ifds, grid%ifde, grid%kfds, grid%kfde, grid%jfds, grid%jfde,    & ! fire grid dimensions
            atm_u3d(i,j,:),atm_v3d(i,j,:),           & ! atm grid arrays in
            atm_ph(i,j,:),                           &
            grid%uf(i,j),grid%vf(i,j),grid%fz0(i,j))
      enddo
    enddo

    deallocate (atm_u3d, atm_v3d, atm_ph, atm_pres)

    if (grid%datetime_now == grid%datetime_start) call grid%Save_state ()

    call Advance_state (grid, config_flags)

    call grid%Handle_output (config_flags)

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

end module

