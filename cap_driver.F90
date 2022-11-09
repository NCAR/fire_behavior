module cap_driver

  !-----------------------------------------------------------------------------
  ! Fire behavior component
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS    => SetServices

  !use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, INPUT_UNIT

  use state_mod, only : domain
  use namelist_mod, only : namelist_t
  use initialize_mod, only : Init_state
  use advance_mod, only : Advance_state

  implicit none

  private

  public SetVM, SetServices

  !integer, parameter :: CASE_WRF_FIRE_TEST1 = 1
  type (domain) :: grid
  type (namelist_t) :: config_flags
  integer :: n, j
  !logical, parameter :: DEBUG = .true., WRITE_OUTPUT = .false.

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

    call Init_fire_behavior_model()

    ! Import/ Export Variables -----------------------------------------------------

    ! Disabling the following macro, e.g. renaming to WITHIMPORTFIELDS_disable,
    ! will result in a model component that does not advertise any importable
    ! Fields. Use this if you want to drive the model independently.
#define WITHIMPORTFIELDS_disable
#ifdef WITHIMPORTFIELDS

    ! importable field: air_pressure_at_sea_level
    call NUOPC_Advertise(importState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(importState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: precipitation_flux
    call NUOPC_Advertise(importState, &
      StandardName="precipitation_flux", name="precip", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

#define WITHEXPORTFIELDS_disable
#ifdef WITHEXPORTFIELDS
    ! exportable field: sea_surface_temperature
    call NUOPC_Advertise(exportState, &
      StandardName="sea_surface_temperature", name="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_salinity
    call NUOPC_Advertise(exportState, &
      StandardName="sea_surface_salinity", name="sss", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_height_above_sea_level
    call NUOPC_Advertise(exportState, &
      StandardName="sea_surface_height_above_sea_level", name="ssh", rc=rc)
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
    ! type(ESMF_Field)        :: field
    ! type(ESMF_Grid)         :: gridIn, gridOut
    ! type(ESMF_Mesh)         :: meshIn, meshOut
    ! type(ESMF_LocStream)    :: locsIn, locsOut

    ! integer, parameter              :: totalNumPoints=100
    ! integer(ESMF_KIND_I4), pointer  :: mask(:)
    ! real(ESMF_KIND_R8), pointer     :: lon(:), lat(:)
    ! real(ESMF_KIND_R8), pointer     :: fptr(:)
    ! integer                         :: clb(1), cub(1), i
    ! type(ESMF_VM)                   :: vm

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

!     ! create Grid objects for Fields
!     gridIn = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/100, 20/), &
!       minCornerCoord=(/10._ESMF_KIND_R8, 20._ESMF_KIND_R8/), &
!       maxCornerCoord=(/100._ESMF_KIND_R8, 200._ESMF_KIND_R8/), &
!       coordSys=ESMF_COORDSYS_CART, &
!       staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
!       rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out
!     gridOut = gridIn ! for now out same as in

!     ! create Mesh objects for Fields
!     meshIn = ESMF_MeshCreate(grid=gridIn, rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out
!     meshOut = ESMF_MeshCreate(grid=gridOut, rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out

!     ! create LocStream objects for Fields
!     locsIn=ESMF_LocStreamCreate(name="Equatorial Measurements", &
!         maxIndex=totalNumPoints, coordSys=ESMF_COORDSYS_SPH_DEG, &
!         indexFlag=ESMF_INDEX_GLOBAL, rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out
!     ! Add key data (internally allocating memory).
!     call ESMF_LocStreamAddKey(locsIn,                 &
!          keyName="ESMF:Lat",           &
!          KeyTypeKind=ESMF_TYPEKIND_R8, &
!          keyUnits="Degrees",           &
!          keyLongName="Latitude", rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out
!     call ESMF_LocStreamAddKey(locsIn,                 &
!          keyName="ESMF:Lon",           &
!          KeyTypeKind=ESMF_TYPEKIND_R8, &
!          keyUnits="Degrees",           &
!          keyLongName="Longitude", rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out
!     call ESMF_LocStreamAddKey(locsIn,                 &
!          keyName="ESMF:Mask",           &
!          KeyTypeKind=ESMF_TYPEKIND_I4, &
!          keyUnits="none",           &
!          keyLongName="mask values", rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out
!     ! Get coordinate memory
!     call ESMF_LocStreamGetKey(locsIn,                 &
!          localDE=0,                    &
!          keyName="ESMF:Lat",           &
!          farray=lat,                   &
!          rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out
!     call ESMF_LocStreamGetKey(locsIn,                 &
!          localDE=0,                    &
!          keyName="ESMF:Lon",           &
!          farray=lon,                   &
!          rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out
!     ! Get mask memory
!     call ESMF_LocStreamGetKey(locsIn,                 &
!          localDE=0,                    &
!          keyName="ESMF:Mask",           &
!          farray=mask,                   &
!          rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out
!     locsOut = locsIn ! for now out same as in

! #ifdef WITHIMPORTFIELDS
!     ! importable field on Grid: air_pressure_at_sea_level
!     field = ESMF_FieldCreate(name="pmsl", grid=gridIn, &
!       typekind=ESMF_TYPEKIND_R8, rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out
!     call NUOPC_Realize(importState, field=field, rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out

!     ! importable field on Grid: surface_net_downward_shortwave_flux
!     field = ESMF_FieldCreate(name="rsns", grid=gridIn, &
!       typekind=ESMF_TYPEKIND_R8, rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out
!     call NUOPC_Realize(importState, field=field, rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out

!     ! importable field on Mesh: precipitation_flux
!     field = ESMF_FieldCreate(name="precip", mesh=meshIn, &
!       typekind=ESMF_TYPEKIND_R8, rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out
!     call NUOPC_Realize(importState, field=field, rc=rc)
!     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!       line=__LINE__, &
!       file=__FILE__)) &
!       return  ! bail out
! #endif

! #ifdef WITHEXPORTFIELDS
!     ! exportable field on Grid: sea_surface_temperature
!     field = ESMF_FieldCreate(name="sst", grid=gridOut, &
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

!     ! exportable field on Mesh: sea_surface_salinity
!     field = ESMF_FieldCreate(name="sss", mesh=meshOut, &
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

!     ! exportable field on LocStream: sea_surface_height_above_sea_level
!     field = ESMF_FieldCreate(name="ssh", locstream=locsOut, &
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
!     ! Get Field memory
!     call ESMF_FieldGet(field, localDe=0, farrayPtr=fptr, &
!       computationalLBound=clb, computationalUBound=cub, rc=rc)
!     ! Set coordinate data and field data
!     do i=clb(1),cub(1)
!        lon(i)=(i-1)*360.0/REAL(totalNumPoints)
!        lat(i)=0.0
!        fptr(i)=lon(i)/360.0 ! Just set it to this for testing
!        mask(i)=0
!        ! Mask out range and make data bad
!        ! (Same range as in atm.F90)
!        if ((lon(i) > 10.0) .and. (lon(i) < 20.0)) then
!           mask(i)=1
!           fptr(i)=-10000.0 ! Bad value to check that mask works
!        endif
!     enddo
! #endif


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
    ! character(len=160)          :: msgString

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

    
    call Run_fire_behavior_model ()

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Init_fire_behavior_model ()

    implicit none
    call Init_state (grid, config_flags)

  end subroutine Init_fire_behavior_model

  !-----------------------------------------------------------------------------

  subroutine Run_fire_behavior_model ()

    implicit none

    do n = 1, config_flags%n_steps
       call Advance_state (grid, config_flags)
    end do
  end subroutine Run_fire_behavior_model

  !-----------------------------------------------------------------------------

end module
