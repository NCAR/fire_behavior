
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
  use constants_mod, only : G, XLV, CP, FVIRT, R_D

  implicit none

  private

  public SetVM, SetServices

  type (state_fire_t) :: grid
  type (namelist_t) :: config_flags
  real(ESMF_KIND_R8), pointer     :: ptr_z0(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_t2(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_psfc(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_rainrte(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_rainacc(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_q2(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_lowest_q(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_lowest_t(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_lowest_pres(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_u3d(:,:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_v3d(:,:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_ph(:,:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_hflx_fire(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_evap_fire(:,:)
  real(ESMF_KIND_R8), pointer     :: ptr_smoke_fire(:,:)
  integer :: clb(2), cub(2), clb3(3), cub3(3)
  logical :: imp_rainrte = .FALSE.
  logical :: imp_rainacc = .FALSE.

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

      ! Read namelist
    call config_flags%Initialization (file_name = 'namelist.fire')

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

    !  2D fields

    ! importable field: inst_surface_roughness
    call NUOPC_Advertise(importState, &
      StandardName="inst_surface_roughness", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: accumulated_lwe_thickness_of_precipitation_amount
    call NUOPC_Advertise(importState, &
      StandardName="accumulated_lwe_thickness_of_precipitation_amount", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: mean_prec_rate
    call NUOPC_Advertise(importState, &
      StandardName="mean_prec_rate", rc=rc)
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

    ! importable field: inst_pres_height_lowest_from_phys
    call NUOPC_Advertise(importState, &
      StandardName="inst_pres_height_lowest_from_phys", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: inst_spec_humid_height_lowest_from_phys
    call NUOPC_Advertise(importState, &
      StandardName="inst_spec_humid_height_lowest_from_phys", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: inst_temp_height_lowest_from_phys
    call NUOPC_Advertise(importState, &
      StandardName="inst_temp_height_lowest_from_phys", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#endif

!#define WITHEXPORTFIELDS_disable
!#ifdef WITHEXPORTFIELDS
    ! exportable field: hflx_fire
    call NUOPC_Advertise(exportState, &
      StandardName="hflx_fire", name="hflx_fire", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: evap_fire
    call NUOPC_Advertise(exportState, &
      StandardName="evap_fire", name="evap_fire", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: smoke_fire
    call NUOPC_Advertise(exportState, &
      StandardName="smoke_fire", name="smoke_fire", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!#endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Field)        :: field
    type(ESMF_DistGrid)     :: fire_distgrid
    type(ESMF_Grid)         :: fire_grid

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

     if (NUOPC_IsConnected(importState, fieldName="mean_prec_rate")) then
       imp_rainrte = .TRUE.
       ! importable field on Grid: mean_prec_rate
       field = ESMF_FieldCreate(name="mean_prec_rate", grid=fire_grid, &
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
       call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_rainrte, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
     else
       imp_rainrte = .FALSE.
       call ESMF_StateRemove(importState, (/"mean_prec_rate"/), rc=rc)
     endif

     if (NUOPC_IsConnected(importState, fieldName="accumulated_lwe_thickness_of_precipitation_amount")) then
       imp_rainacc = .TRUE.
       ! importable field on Grid: inst_rainfall_amount
       field = ESMF_FieldCreate(name="accumulated_lwe_thickness_of_precipitation_amount", grid=fire_grid, &
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
       call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_rainacc, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
     else
       imp_rainacc = .FALSE.
       call ESMF_StateRemove(importState, (/"accumulated_lwe_thickness_of_precipitation_amount"/), rc=rc)
     endif

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

     ! importable field on Grid: inst_pres_height_lowest_from_phys
     field = ESMF_FieldCreate(name="inst_pres_height_lowest_from_phys", grid=fire_grid, &
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
     call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_lowest_pres, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

     ! importable field on Grid: inst_spec_humid_height_lowest_from_phys
     field = ESMF_FieldCreate(name="inst_spec_humid_height_lowest_from_phys", grid=fire_grid, &
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
     call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_lowest_q, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

     ! importable field on Grid: inst_temp_height_lowest_from_phys
     field = ESMF_FieldCreate(name="inst_temp_height_lowest_from_phys", grid=fire_grid, &
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
     call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_lowest_t, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

#endif

! #ifdef WITHEXPORTFIELDS
     ! exportable field on Grid: hflx_fire
     field = ESMF_FieldCreate(name="hflx_fire", grid=fire_grid, &
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
     call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_hflx_fire, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

     ! exportable field on Grid: evap_fire
     field = ESMF_FieldCreate(name="evap_fire", grid=fire_grid, &
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
     call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_evap_fire, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

     ! exportable field on Grid: smoke_fire
     field = ESMF_FieldCreate(name="smoke_fire", grid=fire_grid, &
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
     call ESMF_FieldGet(field, localDe=0, farrayPtr=ptr_smoke_fire, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
! #endif

    ! Initialize fire behavior exports
    ptr_hflx_fire = 0.
    ptr_evap_fire = 0.
    ptr_smoke_fire = 0.

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
    type(ESMF_TimeInterval)     :: timeStep
    real(ESMF_KIND_R8)          :: ts
    type(ESMF_State)            :: importState, exportState
    integer                     :: i, j
    real                        :: wspd, q0, rho
    character(len=160)          :: msgString
    real, dimension(:, :, :), allocatable :: atm_u3d, atm_v3d, atm_ph
    real, dimension(:, :), allocatable :: atm_lowest_t, atm_lowest_q, atm_lowest_pres
    real, dimension(:, :), allocatable :: grnhfx_kinematic, grnqfx_kinematic, smoke
    real :: dtratio


    rc = ESMF_SUCCESS

    ! ratio of fire to atmosphere time step
    dtratio = config_flags%dt / config_flags%interval_atm

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_TimeIntervalGet(timeStep, s_r8=ts, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#ifdef WITHIMPORTFIELDS
    ! Update atmospheric fields
    ! convert cm to m
    grid%fz0(1:grid%nx,1:grid%ny) = ptr_z0(clb(1):cub(1),clb(2):cub(2)) * 0.01
    grid%fire_q2(1:grid%nx,1:grid%ny) = ptr_q2(clb(1):cub(1),clb(2):cub(2))
    grid%fire_t2(1:grid%nx,1:grid%ny) = ptr_t2(clb(1):cub(1),clb(2):cub(2))
    grid%fire_psfc(1:grid%nx,1:grid%ny) = ptr_psfc(clb(1):cub(1),clb(2):cub(2))
    if (imp_rainrte) then
      ! convert m s-1 to m and accumulate
      grid%fire_rain(1:grid%nx,1:grid%ny) = grid%fire_rain(1:grid%nx,1:grid%ny) + ( ptr_rainrte(clb(1):cub(1),clb(2):cub(2)) * ts )
    elseif (imp_rainacc) then
      grid%fire_rain(1:grid%nx,1:grid%ny) = ptr_rainacc(clb(1):cub(1),clb(2):cub(2))
    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="missing rainfall import", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    do j = grid%jfds, grid%jfde
      do i = grid%ifds, grid%ifde
        grid%fire_q2(i,j) = max (grid%fire_q2(i,j), .001)
        grid%fire_t2(i,j) = max (grid%fire_t2(i,j), 123.4) ! avoid arithmatic error
        grid%fire_psfc(i,j) = max (grid%fire_psfc(i,j), .001)
      end do
    end do

    allocate (atm_u3d(1:grid%nx,1:grid%ny,1:grid%kfde - 1))
    allocate (atm_v3d(1:grid%nx,1:grid%ny,1:grid%kfde - 1))
    allocate (atm_ph(1:grid%nx,1:grid%ny,1:grid%kfde - 1))

    atm_u3d(1:grid%nx,1:grid%ny,1:grid%kfde - 1)  = ptr_u3d(clb3(1):cub3(1),clb3(2):cub3(2),clb3(3):cub3(3))
    atm_v3d(1:grid%nx,1:grid%ny,1:grid%kfde - 1)  = ptr_v3d(clb3(1):cub3(1),clb3(2):cub3(2),clb3(3):cub3(3))
    atm_ph(1:grid%nx,1:grid%ny,1:grid%kfde - 1)   = ptr_ph(clb3(1):cub3(1),clb3(2):cub3(2),clb3(3):cub3(3))

#endif

    do j = 1, grid%jfde
      do i = 1, grid%ifde
        call grid%Interpolate_profile (config_flags,  & ! for debug output, <= 0 no output
            config_flags%fire_wind_height,           & ! interpolation height
            grid%kfds, grid%kfde,                    & ! fire grid dimensions
            atm_u3d(i,j,:),atm_v3d(i,j,:),           & ! atm grid arrays in
            atm_ph(i,j,:),                           &
            grid%uf(i,j),grid%vf(i,j),grid%fz0(i,j))

        ! avoid arithmatic error
        wspd = (grid%uf(i,j) ** 2. + grid%vf(i,j) ** 2.) ** .5
        if (wspd < 0.001) then
          grid%uf(i,j) = sign(0.001, grid%uf(i,j))
          grid%vf(i,j) = sign(0.001, grid%vf(i,j))
        endif

      enddo
    enddo

    if (grid%datetime_now == grid%datetime_start) call grid%Save_state ()

    If_reset_fluxes: if (grid%datetime_now == grid%datetime_next_atm_update) then

      call grid%datetime_now%Print_datetime ()
      call grid%datetime_next_atm_update%Add_seconds (config_flags%interval_atm)

      ptr_hflx_fire = 0.
      ptr_evap_fire = 0.
      ptr_smoke_fire = 0.

    end if If_reset_fluxes

    call Advance_state (grid, config_flags)

    allocate (grnhfx_kinematic(1:grid%nx,1:grid%ny))
    allocate (grnqfx_kinematic(1:grid%nx,1:grid%ny))
    allocate (smoke(1:grid%nx,1:grid%ny))
    allocate (atm_lowest_t(1:grid%nx,1:grid%ny))
    allocate (atm_lowest_q(1:grid%nx,1:grid%ny))
    allocate (atm_lowest_pres(1:grid%nx,1:grid%ny))

    atm_lowest_t(1:grid%nx,1:grid%ny)    = ptr_lowest_t(clb(1):cub(1),clb(2):cub(2))
    atm_lowest_q(1:grid%nx,1:grid%ny)    = ptr_lowest_q(clb(1):cub(1),clb(2):cub(2))
    atm_lowest_pres(1:grid%nx,1:grid%ny) = ptr_lowest_pres(clb(1):cub(1),clb(2):cub(2))

    do j = 1, grid%jfde
      do i = 1, grid%ifde
        q0   = max(atm_lowest_q(i,j)/(1.-atm_lowest_q(i,j)), 1.e-8)
        rho = atm_lowest_pres(i,j) / (R_D * atm_lowest_t(i,j) * &
            (1.0 + FVIRT * q0))
        if (rho > 0.) then ! avoid unpredictable behavior on the edges
           ! convert [W m-2] to [K m s-1]
          grnhfx_kinematic(i,j)  = grid%fgrnhfx(i,j) / (CP * rho)
           ! convert [W m-2] to [kg kg-1 m s-1]
          grnqfx_kinematic(i,j) = grid%fgrnqfx(i,j) / (XLV * rho)
           ! convert [kg smoke m-2] to [kg smoke kg-1 air]
          smoke(i,j) = grid%emis_smoke(i,j) / ((atm_ph(i,j,2) - atm_ph(i,j,1)) / G * rho)
        end if
      enddo
    enddo

    deallocate (atm_u3d, atm_v3d, atm_ph) !, atm_pres)

    ptr_hflx_fire(clb(1):cub(1),clb(2):cub(2)) = ptr_hflx_fire(clb(1):cub(1),clb(2):cub(2)) \
                 + grnhfx_kinematic(1:grid%nx,1:grid%ny) * config_flags%fire_atm_feedback * dtratio
    ptr_evap_fire(clb(1):cub(1),clb(2):cub(2)) = ptr_evap_fire(clb(1):cub(1),clb(2):cub(2)) \
                 + grnqfx_kinematic(1:grid%nx,1:grid%ny) * config_flags%fire_atm_feedback * dtratio
    ptr_smoke_fire(clb(1):cub(1),clb(2):cub(2)) = ptr_smoke_fire(clb(1):cub(1),clb(2):cub(2)) \
                 + smoke(1:grid%nx,1:grid%ny)

    deallocate(grnhfx_kinematic, grnqfx_kinematic, smoke)

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

