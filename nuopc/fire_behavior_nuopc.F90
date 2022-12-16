
#include "Fire_behavior_NUOPC_Macros.h"

module fire_behavior_nuopc

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS    => SetServices

  use state_mod, only : domain
  use namelist_mod, only : namelist_t
  use initialize_mod, only : Init_state
  use advance_mod, only : Advance_state

  implicit none

  private

  public SetVM, SetServices

  type (domain) :: grid
  type (namelist_t) :: config_flags
  real(ESMF_KIND_R8), pointer     :: ptr_t2(:,:)
  integer :: clb(2), cub(2)

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

    call Init_state (grid, config_flags)

    ! Import/ Export Variables -----------------------------------------------------

    ! Disabling the following macro, e.g. renaming to WITHIMPORTFIELDS_disable,
    ! will result in a model component that does not advertise any importable
    ! Fields. Use this if you want to drive the model independently.

#define WITHIMPORTFIELDS
#ifdef WITHIMPORTFIELDS
!    ! 3D fields
!
!    ! importable field: inst_zonal_wind_levels
!    call NUOPC_Advertise(importState, &
!      StandardName="inst_zonal_wind_levels", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!
!    ! importable field: inst_merid_wind_levels
!    call NUOPC_Advertise(importState, &
!      StandardName="inst_merid_wind_levels", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!
!    ! importable field: inst_geop_levels
!    call NUOPC_Advertise(importState, &
!      StandardName="inst_geop_levels", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!
!    ! importable field: inst_geop_interface
!    call NUOPC_Advertise(importState, &
!      StandardName="inst_geop_interface", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!
!    ! importable field: inst_pres_interface
!    call NUOPC_Advertise(importState, &
!      StandardName="inst_pres_interface", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!
!    ! importable field: inst_pres_levels
!    call NUOPC_Advertise(importState, &
!      StandardName="inst_pres_levels", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!
!    ! importable field: inst_temp_levels
!    call NUOPC_Advertise(importState, &
!      StandardName="inst_temp_levels", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!
!    ! importable field: sphum
!    call NUOPC_Advertise(importState, &
!      StandardName="sphum", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!
!    !  2D fields
!
!    ! importable field: inst_surface_roughness
!    call NUOPC_Advertise(importState, &
!      StandardName="inst_surface_roughness", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!
!    ! importable field: inst_rainfall_amount
!    call NUOPC_Advertise(importState, &
!      StandardName="inst_rainfall_amount", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!
!    ! importable field: inst_spec_humid_height2m
!    call NUOPC_Advertise(importState, &
!      StandardName="inst_spec_humid_height2m", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!
!    ! importable field: inst_pres_height_surface
!    call NUOPC_Advertise(importState, &
!      StandardName="inst_pres_height_surface", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out

    ! importable field: inst_temp_height2m
    call NUOPC_Advertise(importState, &
      StandardName="inst_temp_height2m", rc=rc)
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
    type(ESMF_Field)        :: field
    type(ESMF_DistGrid)     :: fire_distgrid
    type(ESMF_Grid)         :: fire_grid
    ! type(ESMF_Grid)         :: gridIn, gridOut
    ! type(ESMF_Mesh)         :: meshIn, meshOut
    ! type(ESMF_LocStream)    :: locsIn, locsOut

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


#ifdef WITHIMPORTFIELDS
     !  3D fields

!     ! importable field on Grid: inst_zonal_wind_levels
!     field = ESMF_FieldCreate(name="inst_zonal_wind_levels", grid=fire_grid, &
!       gridToFieldMap=(/1,2/), ungriddedLBound=(/1/), &
!       ungriddedUBound=(/grid%kde/), &
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
!
!     ! importable field on Grid: inst_zonal_wind_levels
!     field = ESMF_FieldCreate(name="inst_zonal_wind_levels", grid=fire_grid, &
!       gridToFieldMap=(/1,2/), ungriddedLBound=(/1/), &
!       ungriddedUBound=(/grid%kde/), &
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
!
!     ! importable field on Grid: inst_geop_levels
!     field = ESMF_FieldCreate(name="inst_geop_levels", grid=fire_grid, &
!       gridToFieldMap=(/1,2/), ungriddedLBound=(/1/), &
!       ungriddedUBound=(/grid%kde/), &
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
!
!     ! importable field on Grid: inst_geop_interface
!     field = ESMF_FieldCreate(name="inst_geop_interface", grid=fire_grid, &
!       gridToFieldMap=(/1,2/), ungriddedLBound=(/1/), &
!       ungriddedUBound=(/grid%kde/), &
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
!
!     ! importable field on Grid: inst_pres_interface
!     field = ESMF_FieldCreate(name="inst_pres_interface", grid=fire_grid, &
!       gridToFieldMap=(/1,2/), ungriddedLBound=(/1/), &
!       ungriddedUBound=(/grid%kde/), &
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
!
!     ! importable field on Grid: inst_pres_levels
!     field = ESMF_FieldCreate(name="inst_pres_levels", grid=fire_grid, &
!       gridToFieldMap=(/1,2/), ungriddedLBound=(/1/), &
!       ungriddedUBound=(/grid%kde/), &
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
!
!     ! importable field on Grid: inst_temp_levels
!     field = ESMF_FieldCreate(name="inst_temp_levels", grid=fire_grid, &
!       gridToFieldMap=(/1,2/), ungriddedLBound=(/1/), &
!       ungriddedUBound=(/grid%kde/), &
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
!
!     ! importable field on Grid: sphum
!     field = ESMF_FieldCreate(name="sphum", grid=fire_grid, &
!       gridToFieldMap=(/1,2/), ungriddedLBound=(/1/), &
!       ungriddedUBound=(/grid%kde/), &
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
!
!     !  2D fields
!
!     ! importable field on Grid: inst_surface_roughness
!     field = ESMF_FieldCreate(name="inst_surface_roughness", grid=fire_grid, &
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
!
!     ! importable field on Grid: inst_rainfall_amount
!     field = ESMF_FieldCreate(name="inst_rainfall_amount", grid=fire_grid, &
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
!
!     ! importable field on Grid: inst_spec_humid_height2m
!     field = ESMF_FieldCreate(name="inst_spec_humid_height2m", grid=fire_grid, &
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
!
!     ! importable field on Grid: inst_pres_height_surface
!     field = ESMF_FieldCreate(name="inst_pres_height_surface", grid=fire_grid, &
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
     call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_t2, &
       computationalLBound=clb, computationalUBound=cub, rc=rc) 
#endif

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

#ifdef WITHIMPORTFIELDS
    ! Update atmospheric fields
    grid%fire_t2(1:grid%nx,1:grid%ny) = ptr_t2(clb(1):cub(1),clb(2):cub(2))
#endif

    call Advance_state (grid, config_flags)

    if (grid%datetime_now == grid%datetime_end) call grid%Save_state ()

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

