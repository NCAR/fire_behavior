  module netcdf_mod

    implicit none

    private

    public :: Get_netcdf_var, Get_netcdf_att, Get_netcdf_dim

    interface Get_netcdf_var
      module procedure Get_netcdf_var_real32_3d
      module procedure Get_netcdf_var_int32_3d
    end interface Get_netcdf_var

    interface Get_netcdf_att
      module procedure Get_netcdf_att_real32
      module procedure Get_netcdf_att_int32
    end interface Get_netcdf_att

  contains

    subroutine Check_status (status)

      use netcdf

      implicit none

      integer, intent (in) :: status


      if (status /= NF90_NOERR) then
        print *, trim (nf90_strerror (status))
        stop
      end if

    end subroutine Check_status

    subroutine Get_netcdf_att_int32 (file_name, var_name, att_name, att_value)

      use netcdf

      use, intrinsic :: iso_fortran_env, only :  INT32

      implicit none

      character (len = *), intent(in) :: file_name, var_name, att_name
      integer (kind = INT32), intent(out) :: att_value

      integer :: status, ncid, varid


        ! Open file
      status = nf90_open (trim(file_name), NF90_NOWRITE, ncid)
      call Check_status (status)

        ! Get att length
      if (var_name == 'global') then
        status = nf90_get_att (ncid, NF90_GLOBAL, att_name, att_value)
        call Check_status (status)
      else
        status = nf90_inq_varid (ncid, var_name, varid)
        call Check_status (status)

        status = nf90_get_att (ncid, varid, att_name, att_value)
        call Check_status (status)
      end if

        ! Close file
      status = nf90_close (ncid)
      call Check_status (status)

    end subroutine Get_netcdf_att_int32

    subroutine Get_netcdf_dim (file_name, dim_name, dim_value)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !                                                         !
    !  Purpose: Get the value of a dimension                  !
    !                                                         !
    !  Author: Pedro A. Jimenez                               !
    !                                                         !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use netcdf
      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      character (len = *), intent(in) :: file_name, dim_name
      integer, intent(out) :: dim_value

      integer :: status, ncid, dimid


        ! Opens file
      status = nf90_open (trim(file_name), NF90_NOWRITE, ncid)
      call Check_status (status)

        ! Get dim ID
      status = nf90_inq_dimid (ncid, dim_name, dimid)
      call Check_status (status)

        ! Get dim len
      status = nf90_inquire_dimension (ncid, dimid, len = dim_value)
      call Check_status (status)

        ! Closing file
      status = nf90_close (ncid)
      call Check_status (status)

    end subroutine Get_netcdf_dim

    subroutine Get_netcdf_att_real32 (file_name, var_name, att_name, att_value)

      use netcdf

      use, intrinsic :: iso_fortran_env, only :  REAL32

      implicit none

      character (len = *), intent(in) :: file_name, var_name, att_name
      real (kind = REAL32), intent(out) :: att_value

      integer :: status, ncid, varid


        ! Open file
      status = nf90_open (trim(file_name), NF90_NOWRITE, ncid)
      call Check_status (status)

        ! Get att length
      if (var_name == 'global') then
        status = nf90_get_att (ncid, NF90_GLOBAL, att_name, att_value)
        call Check_status (status)
      else
        status = nf90_inq_varid (ncid, var_name, varid)
        call Check_status (status)

        status = nf90_get_att (ncid, varid, att_name, att_value)
        call Check_status (status)
      end if

        ! Close file
      status = nf90_close (ncid)
      call Check_status (status)

    end subroutine Get_netcdf_att_real32

    subroutine Get_netcdf_var_int32_3d (file_name, var_name, output)

      use netcdf
      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, INT32

      implicit none

      character (len = *), intent(in) :: file_name, var_name
      integer (kind = INT32), dimension (:, :, :), allocatable :: output

      integer :: status, ncid, ivar, nf_type, nvdims
      integer :: i, n1, n2, n3, io_stat
      integer, dimension(NF90_MAX_VAR_DIMS) :: dimids, idims


        ! Open file
      status = nf90_open (trim(file_name), NF90_NOWRITE, ncid)
      call Check_status (status)

        ! Get var
      status = nf90_inq_varid (ncid, trim (var_name), ivar)
      call Check_status (status)
      status = nf90_inquire_variable (ncid, ivar, xtype = nf_type, ndims = nvdims, dimids = dimids)
      call Check_status (status)

      if (nf_type == NF90_INT) then
        if (nvdims == 3) then
          do i = 1, nvdims
            status = nf90_inquire_dimension (ncid, dimids(i), len = idims(i))
            call Check_status (status)
          end do

          n1 = idims(1)
          n2 = idims(2)
          n3 = idims(3)
          allocate (output(n1, n2, n3), stat = io_stat)
          if (io_stat /= 0) then
            write (OUTPUT_UNIT, *) 'Problems allocating output variable'
            stop
          end if

          status = nf90_get_var (ncid, ivar, output)
          call Check_status (status)
        else
          write (OUTPUT_UNIT, *)
          write (OUTPUT_UNIT, *) 'FATAL ERROR: ', trim (var_name), ' is not a 3D array'
          stop
        end if
      else
        write (OUTPUT_UNIT, *)
        write (OUTPUT_UNIT, *) 'FATAL ERROR: ', trim (var_name), ' is not an int32 variable'
        stop
      end if

        ! Closing file
      status = nf90_close (ncid)
      call Check_status (status)

    end subroutine Get_netcdf_var_int32_3d

    subroutine Get_netcdf_var_real32_3d (file_name, var_name, output)

      use netcdf
      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, REAL32

      implicit none

      character (len = *), intent(in) :: file_name, var_name
      real (kind = REAL32), dimension (:, :, :), allocatable :: output

      integer :: status, ncid, ivar, nf_type, nvdims
      integer :: i, n1, n2, n3, io_stat
      integer, dimension(NF90_MAX_VAR_DIMS) :: dimids, idims


        ! Open file
      status = nf90_open (trim(file_name), NF90_NOWRITE, ncid)
      call Check_status (status)

        ! Get var
      status = nf90_inq_varid (ncid, trim (var_name), ivar)
      call Check_status (status)
      status = nf90_inquire_variable (ncid, ivar, xtype = nf_type, ndims = nvdims, dimids = dimids)
      call Check_status (status)

      if (nf_type == NF90_FLOAT) then
        if (nvdims == 3) then
          do i = 1, nvdims
            status = nf90_inquire_dimension (ncid, dimids(i), len = idims(i))
            call Check_status (status)
          end do

          n1 = idims(1)
          n2 = idims(2)
          n3 = idims(3)
          allocate (output(n1, n2, n3), stat = io_stat)
          if (io_stat /= 0) then
            write (OUTPUT_UNIT, *) 'Problems allocating output variable'
            stop
          end if

          status = nf90_get_var (ncid, ivar, output)
          call Check_status (status)
        else
          write (OUTPUT_UNIT, *)
          write (OUTPUT_UNIT, *) 'FATAL ERROR: ', trim (var_name), ' is not a 3D array'
          stop
        end if
      else
        write (OUTPUT_UNIT, *)
        write (OUTPUT_UNIT, *) 'FATAL ERROR: ', trim (var_name), ' is not a real32 variable'
        stop
      end if

        ! Closing file
      status = nf90_close (ncid)
      call Check_status (status)

    end subroutine Get_netcdf_var_real32_3d

  end module netcdf_mod

