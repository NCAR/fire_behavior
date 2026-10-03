program coordinate_precision_unit

  ! Write small NetCDF coordinate fixtures in single and double precision, then
  ! read them through the atmospheric reader. Check that double-precision
  ! coordinates retain their detail and single-precision values are preserved
  ! when promoted to the reader's double-precision storage.

  use, intrinsic :: iso_fortran_env, only : REAL32, REAL64, INT64
  use netcdf
  use wrfdata_mod, only : wrfdata_t
#ifdef DM_PARALLEL
  use mpi
#endif

  implicit none

  type (wrfdata_t) :: atmosphere
  real(kind=REAL64) :: latitude(2,1,1), longitude(2,1,1)
  real(kind=REAL32) :: lat32(2,1,1), lon32(2,1,1)
  integer :: file_id, lat_id, lon_id, dims(3), storage_type, ierr
  integer(kind=INT64) :: tick
  character(len=100) :: filename

#ifdef DM_PARALLEL
  call MPI_Init (ierr)
#endif

  ! These coordinate separations are smaller than a float32 longitude quantum.
  latitude(:,1,1) = [40.0000001_REAL64, 40.0000002_REAL64]
  longitude(:,1,1) = [-105.0000001_REAL64, -105.0000002_REAL64]
  lat32 = real(latitude, REAL32)
  lon32 = real(longitude, REAL32)

  do storage_type = NF90_FLOAT, NF90_DOUBLE
    call system_clock(count=tick)
    write(filename, '(a,i0,a,i0,a)') 'coordinates-', tick, '-', storage_type, '.nc'
    call checked(nf90_create(trim(filename), NF90_NOCLOBBER, file_id))
    call checked(nf90_def_dim(file_id, 'west_east', 2, dims(1)))
    call checked(nf90_def_dim(file_id, 'south_north', 1, dims(2)))
    call checked(nf90_def_dim(file_id, 'Time', 1, dims(3)))
    call checked(nf90_def_var(file_id, 'XLAT', storage_type, dims, lat_id))
    call checked(nf90_def_var(file_id, 'XLONG', storage_type, dims, lon_id))
    call checked(nf90_enddef(file_id))
    call checked(nf90_put_var(file_id, lat_id, latitude))
    call checked(nf90_put_var(file_id, lon_id, longitude))
    call checked(nf90_close(file_id))

    atmosphere%file_name = trim(filename)
    call atmosphere%Get_latlons()
    if (storage_size(atmosphere%lats) /= 64 .or. storage_size(atmosphere%lons) /= 64) &
        error stop 'atmospheric coordinates must retain 64-bit storage'
    if (storage_type == NF90_DOUBLE) then
      if (any(atmosphere%lats /= latitude(:,:,1))) error stop 'double latitude precision lost'
      if (any(atmosphere%lons /= longitude(:,:,1))) error stop 'double longitude precision lost'
    else
      if (any(atmosphere%lats /= real(lat32(:,:,1), REAL64))) error stop 'float latitude changed'
      if (any(atmosphere%lons /= real(lon32(:,:,1), REAL64))) error stop 'float longitude changed'
    end if
  end do

  print *, 'coordinate precision checks passed'
#ifdef DM_PARALLEL
  call MPI_Finalize (ierr)
#endif

contains

  subroutine checked(status)
    integer, intent(in) :: status
    if (status /= NF90_NOERR) then
      print *, trim(nf90_strerror(status))
      error stop 'coordinate fixture I/O failed'
    end if
  end subroutine checked

end program coordinate_precision_unit
