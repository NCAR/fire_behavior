module fuel_table_mod
  ! Read once before physics initialization. Only communicator rank zero opens
  ! the file; every rank validates the same broadcast text outside OpenMP.
  use, intrinsic :: iso_fortran_env, only: REAL64, INT64, iostat_end
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
#ifdef DM_PARALLEL
  use mpi
#endif
  use stderrout_mod, only: Stop_simulation, Print_message
  use fuel_table_paths_mod, only: BUILD_TABLE_DIR, INSTALL_TABLE_DIR
  implicit none
  private
  public :: fuel_table_t, moisture_parameters_t, Read_fuel_table, Require_table

  integer, parameter :: LINE_LENGTH = 4096, MAX_LINES = 512
  type :: moisture_parameters_t
    real, allocatable :: drying_lag(:), wetting_lag(:), saturation_moisture(:)
    real, allocatable :: saturation_rain(:), rain_threshold(:)
    integer, allocatable :: drying_model(:), wetting_model(:), fmc_gc_initialization(:)
    real :: decay_lag
  end type

  type :: fuel_table_t
    real, allocatable :: properties(:,:)
    character(len=80), allocatable :: names(:)
    type(moisture_parameters_t) :: moisture
    real :: curing_none, curing_span
    character(len=16) :: fingerprint
    logical :: legacy_compatible
  end type

contains

  subroutine Require_table(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message
    if (.not. condition) call Stop_simulation('Fuel table: '//trim(message))
  end subroutine

  subroutine Read_fuel_table(family, codes, columns, table, filename, comm)
    character(len=*), intent(in) :: family, columns
    integer, intent(in) :: codes(:)
    type(fuel_table_t), intent(out) :: table
    character(len=*), intent(in), optional :: filename
    integer, intent(in), optional :: comm
    character(len=LINE_LENGTH), allocatable :: lines(:)
    character(len=LINE_LENGTH+1) :: buffer
    character(len=LINE_LENGTH) :: path, directory
    character(len=80) :: name
    character(len=32) :: number
    integer :: rank, ierr, unit, ios, nlines, pos, nfields, nclasses, i, j, k, idx, code
    integer, parameter :: aliases(6) = [14,91,92,93,98,99]
    logical :: exists, seen(size(codes)), seen_alias(6), seen_class(6)
    real(REAL64), allocatable :: values(:), moisture(:,:)
    real(REAL64) :: lower, upper
    integer(INT64) :: checksum

    rank = 0
#ifdef DM_PARALLEL
    if (present(comm)) then
      call MPI_Comm_rank(comm, rank, ierr)
      call Require_table(ierr == MPI_SUCCESS, 'cannot determine communicator rank')
    end if
#endif
    allocate(lines(MAX_LINES))
    lines = ''
    nlines = 0
    path = ''
    if (rank == 0) then
      if (present(filename)) path = filename
      if (len_trim(path) == 0) then
        call get_environment_variable('CFBM_FUEL_TABLE_DIR', directory, status=ios)
        call Require_table(ios /= -1, 'CFBM_FUEL_TABLE_DIR is too long')
        if (ios == 0 .and. len_trim(directory) > 0) then
          path = trim(directory)//'/'//family//'.TBL'
        else
          path = family//'.TBL'
          inquire(file=trim(path), exist=exists)
          if (.not. exists) path = BUILD_TABLE_DIR//'/'//family//'.TBL'
          inquire(file=trim(path), exist=exists)
          if (.not. exists) path = INSTALL_TABLE_DIR//'/'//family//'.TBL'
        end if
      end if
      open(newunit=unit, file=trim(path), status='old', action='read', iostat=ios)
      call Require_table(ios == 0, 'cannot open '//trim(path))
      do
        read(unit, '(a)', iostat=ios) buffer
        if (ios == iostat_end) exit
        call Require_table(ios == 0, 'read failure in '//trim(path))
        call Require_table(len_trim(buffer) <= LINE_LENGTH, 'line exceeds 4096 characters')
        buffer = adjustl(buffer)
        do while (buffer(1:1) == achar(9))
          buffer = adjustl(buffer(2:))
        end do
        if (len_trim(buffer) == 0) cycle
        if (buffer(1:1) == '#' .or. buffer(1:1) == '!') cycle
        nlines = nlines + 1
        call Require_table(nlines <= MAX_LINES, 'too many records')
        lines(nlines) = trim(buffer)
      end do
      close(unit)
    end if
#ifdef DM_PARALLEL
    if (present(comm)) then
      call MPI_Bcast(nlines, 1, MPI_INTEGER, 0, comm, ierr)
      call Require_table(ierr == MPI_SUCCESS, 'record-count broadcast failed')
      call MPI_Bcast(lines, LINE_LENGTH*nlines, MPI_CHARACTER, 0, comm, ierr)
      call Require_table(ierr == MPI_SUCCESS, 'data broadcast failed')
    end if
#endif
    ! CRC64-ECMA of non-comment records, stripped at the ends, with LF separators.
    ! This is a reproducibility identifier, not a cryptographic authenticity check.
    checksum = 0_INT64
    do i = 1, nlines
      do j = 1, len_trim(lines(i))+1
        code = 10
        if (j <= len_trim(lines(i))) code = iachar(lines(i)(j:j))
        checksum = ieor(checksum, shiftl(int(code,INT64),56))
        do k = 1, 8
          if (btest(checksum,63)) then
            checksum = ieor(shiftl(checksum,1),int(z'42F0E1EBA9EA3693',INT64))
          else
            checksum = shiftl(checksum,1)
          end if
        end do
      end do
    end do
    write(table%fingerprint,'(z16.16)') checksum
    table%legacy_compatible = .false.
    if (family == 'ANDERSON13') table%legacy_compatible = table%fingerprint == 'C3E03B875D191B13'
    if (family == 'SCOTT_BURGAN40') table%legacy_compatible = table%fingerprint == 'EE9E2DDFA76801AC'
    pos = 0
    call Expect('CFBM_FUEL_TABLE 1')
    call Expect('FAMILY '//family)
    write(number,'(i0)') size(codes)
    call Expect('FUEL_MODELS '//trim(number))
    call Expect(columns)
    ! Each numeric column is a single whitespace-delimited token. The final
    ! quoted name may contain spaces; null values, repetition and '/' are rejected.
    nfields = 0
    do i = 1, len_trim(columns)
      if (columns(i:i) == ' ') nfields = nfields + 1
    end do
    allocate(values(nfields), table%properties(nfields-1,size(codes)), table%names(size(codes)))
    seen = .false.
    do i = 1, size(codes)
      call Row(nfields, values, name)
      idx = 0
      do j = 1, size(codes)
        if (values(1) == real(codes(j),REAL64)) idx = j
      end do
      call Require_table(idx > 0, 'unknown property-row code')
      call Require_table(.not. seen(idx), 'duplicate property-row code')
      seen(idx) = .true.
      call Require_table(all(values(2:) >= 0), 'negative fuel property')
      table%properties(:,idx) = real(values(2:))
      table%names(idx) = name
    end do
    call Require_table(all(seen), 'missing property-row code')
    call Expect('NONBURNABLE_ALIASES 6')
    call Expect('code name')
    seen_alias = .false.
    do i = 1, 6
      call Row(1, values(:1), name)
      idx = 0
      do j = 1, 6
        if (values(1) == real(aliases(j),REAL64)) idx = j
      end do
      call Require_table(idx > 0, 'unknown nonburnable alias')
      call Require_table(.not. seen_alias(idx), 'duplicate nonburnable alias')
      seen_alias(idx) = .true.
    end do
    nclasses = 5
    if (family == 'SCOTT_BURGAN40') nclasses = 6
    write(number,'(i0)') nclasses
    call Expect('MOISTURE_CLASSES '//trim(number))
    call Expect('class drying[h] wetting[h] saturation[kg/kg] saturation_rain[mm/h] '// &
        'rain_threshold[mm/h] drying_model[1] wetting_model[1] initialization[1] name')
    allocate(moisture(8,nclasses))
    seen_class = .false.
    do i = 1, nclasses
      call Row(9, values(:9), name)
      call Require_table(values(1) >= 1 .and. values(1) <= nclasses, 'unknown moisture class')
      idx = int(values(1))
      call Require_table(values(1) == idx, 'fractional moisture class')
      call Require_table(.not. seen_class(idx), 'duplicate moisture class')
      seen_class(idx) = .true.
      call Require_table(all(values(2:5) > 0), 'moisture lags and saturation must be positive')
      call Require_table(values(6) >= 0, 'rain threshold must be nonnegative')
      call Require_table(all(values(7:8) == 1), 'unsupported drying/wetting model')
      call Require_table(any(values(9) == [1.0_REAL64,2.0_REAL64,3.0_REAL64]), 'initialization must be 1, 2 or 3')
      moisture(:,idx) = values(2:9)
    end do
    table%moisture%drying_lag = real(moisture(1,:))
    table%moisture%wetting_lag = real(moisture(2,:))
    table%moisture%saturation_moisture = real(moisture(3,:))
    table%moisture%saturation_rain = real(moisture(4,:))
    table%moisture%rain_threshold = real(moisture(5,:))
    table%moisture%drying_model = int(moisture(6,:))
    table%moisture%wetting_model = int(moisture(7,:))
    table%moisture%fmc_gc_initialization = int(moisture(8,:))
    call Scalar('MOISTURE_ADJUSTMENT_DECAY_H', lower)
    call Require_table(lower > 0, 'adjustment decay time must be positive')
    table%moisture%decay_lag = real(lower)
    if (nclasses == 6) then
      call Scalar('CURING_FULL_MOISTURE', lower)
      call Scalar('CURING_NONE_MOISTURE', upper)
      call Require_table(lower >= 0 .and. upper > lower, 'invalid curing moisture interval')
      table%curing_none = real(upper)
      ! Subtract the decimal table endpoints in double precision before storage:
      ! the standard interval remains the original default-real literal 0.90.
      table%curing_span = real(upper-lower)
      call Require_table(table%curing_span > 0, 'curing interval is too small')
    end if
    call Expect('END')
    call Require_table(pos == nlines, 'unexpected records after END')
    if (rank == 0) call Print_message('Fuel table '//trim(path)//' CRC64='//table%fingerprint)

  contains

    subroutine Next()
      pos = pos + 1
      call Require_table(pos <= nlines, 'unexpected end of file')
    end subroutine

    subroutine Expect(expected)
      character(len=*), intent(in) :: expected
      call Next()
      call Require_table(trim(lines(pos)) == expected, 'expected '//expected)
    end subroutine

    subroutine Numeric(token, value)
      character(len=*), intent(in) :: token
      real(REAL64), intent(out) :: value
      integer :: status
      call Require_table(len_trim(token) > 0, 'missing numeric value')
      call Require_table(verify(trim(token),'0123456789+-.eEdD') == 0, 'invalid numeric token '//trim(token))
      read(token,*,iostat=status) value
      call Require_table(status == 0, 'invalid numeric value '//trim(token))
      call Require_table(ieee_is_finite(value), 'nonfinite numeric value')
      call Require_table(abs(value) <= real(huge(1.0),REAL64), 'value exceeds model precision')
      if (value /= 0) call Require_table(abs(value) >= real(tiny(1.0),REAL64), 'value below model precision')
    end subroutine

    subroutine Row(n, result, label)
      integer, intent(in) :: n
      real(REAL64), intent(out) :: result(n)
      character(len=*), intent(out) :: label
      character(len=LINE_LENGTH) :: rest
      integer :: m, boundary, length
      call Next()
      rest = lines(pos)
      do m = 1, n
        boundary = scan(trim(rest),' '//achar(9))
        call Require_table(boundary > 1, 'missing row field')
        call Numeric(rest(:boundary-1), result(m))
        rest = adjustl(rest(boundary+1:))
        do while (rest(1:1) == achar(9))
          rest = adjustl(rest(2:))
        end do
      end do
      length = len_trim(rest)
      call Require_table(length >= 3 .and. length <= len(label)+2, 'invalid row name')
      call Require_table(rest(1:1) == '"' .and. rest(length:length) == '"', 'row must end with a quoted name')
      call Require_table(index(rest(2:length-1),'"') == 0, 'extra row fields')
      label = rest(2:length-1)
    end subroutine

    subroutine Scalar(key, value)
      character(len=*), intent(in) :: key
      real(REAL64), intent(out) :: value
      call Next()
      call Require_table(index(lines(pos),key//' ') == 1, 'expected '//key)
      call Numeric(trim(adjustl(lines(pos)(len(key)+2:))), value)
    end subroutine
  end subroutine Read_fuel_table
end module fuel_table_mod
