!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module ESM

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driverSS             => SetServices

  use fire_behavior_nuopc, only: fireSS => SetServices
  use wrf_nuopc, only: wrfSS => SetServices

  use NUOPC_Connector, only: cplSS => SetServices

  use namelist_mod, only : namelist_t

  implicit none

  private

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Driver
    call NUOPC_CompDerive(driver, driverSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! specialize driver
    call NUOPC_CompSpecialize(driver, specLabel=label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set driver verbosity
    call NUOPC_CompAttributeSet(driver, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Grid)               :: grid
    type(ESMF_Field)              :: field
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: connector

    type (namelist_t) :: fire_nml
    integer :: start_year, start_month, start_day, start_hour, start_minute, start_second, &
        end_year, end_month, end_day, end_hour, end_minute, end_second
    real :: dt


    rc = ESMF_SUCCESS

    ! SetServices for FIRE
    call NUOPC_DriverAddComp(driver, "FIRE", fireSS, comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! SetServices for WRF
    call NUOPC_DriverAddComp(driver, "WRF", wrfSS, comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Disabling the following macro, e.g. renaming to WITHCONNECTORS_disable,
    ! will result in a driver that does not call connectors between the model
    ! components. This mode can be used if all model components are driven
    ! as independent models. However, even for independent models the
    ! connectors can be set here, but will turn into no-ops.
#define WITHCONNECTORS
#ifdef WITHCONNECTORS
    ! SetServices for wrf2fire
    call NUOPC_DriverAddComp(driver, srcCompLabel="WRF", dstCompLabel="FIRE", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_DriverAddComp(driver, srcCompLabel="FIRE", dstCompLabel="WRF", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#endif

      ! set the driver clock
    call fire_nml%Init_time_block ('namelist.input')

    call ESMF_TimeIntervalSet (timeStep, s_r8 = real (fire_nml%dt, kind = ESMF_KIND_R8), rc = rc)
    if (ESMF_LogFoundError (rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line = __LINE__, file = __FILE__)) &
        return

    call ESMF_TimeSet (startTime, yy = fire_nml%start_year, mm = fire_nml%start_month, &
        dd = fire_nml%start_day, h = fire_nml%start_hour, m = fire_nml%start_minute,  &
        s = fire_nml%start_second, calkindflag = ESMF_CALKIND_GREGORIAN, rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) &
        return

    call ESMF_TimeSet (stopTime, yy = fire_nml%end_year, mm = fire_nml%end_month, &
        dd = fire_nml%end_day, h = fire_nml%end_hour, m = fire_nml%end_minute, &
        s = fire_nml%end_second, calkindflag = ESMF_CALKIND_GREGORIAN, rc = rc)
    if (ESMF_LogFoundError (rcToCheck = rc, msg = ESMF_LOGERR_PASSTHRU, line = __LINE__, file = __FILE__)) &
        return

    internalClock = ESMF_ClockCreate (name = "Application Clock", timeStep = timeStep, startTime = startTime, &
        stopTime = stopTime, rc = rc)
    if (ESMF_LogFoundError (rcToCheck = rc, msg = ESMF_LOGERR_PASSTHRU, line=__LINE__, file = __FILE__)) &
        return

    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

end module
