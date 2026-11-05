program fuel_table_unit
  ! Runtime tables must reproduce the distributed parameters and admit explicit
  ! experiments without rebuilding. A changed GR1 load and 1-h drying lag test
  ! propagation into inventory and the moisture time constant. Reordered rows
  ! must retain external-code identity; malformed files must stop before physics.
  ! MPI tests reverse the communicator: only its root receives a usable path.
  ! This detects accidental world-root assumptions and non-root file access.
  use fuel_anderson_mod, only: fuel_anderson_t
  use fuel_scott_burgan_mod, only: fuel_scott_burgan_t
  use fmc_wrffire_mod, only: fmc_wrffire_t
  use netcdf_mod, only: Create_netcdf_file, Add_netcdf_att, Get_netcdf_att
#ifdef DM_PARALLEL
  use mpi
#endif
  implicit none
  type(fuel_anderson_t) :: anderson
  type(fuel_scott_burgan_t) :: fuels, standard
  type(fmc_wrffire_t) :: moisture
  character(len=4096) :: source, file, line, saved, directory
  character(len=16) :: fingerprint
  character(len=32) :: scenario
  integer :: rank, world_rank, comm, ierr, unit, output, status, position

  call get_command_argument(1, scenario)
  call get_command_argument(2, directory)
  rank = 0
  comm = 0
#ifdef DM_PARALLEL
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, world_rank, ierr)
  call MPI_Comm_split(MPI_COMM_WORLD, 0, -world_rank, comm, ierr)
  call MPI_Comm_rank(comm, rank, ierr)
#endif
  source = trim(directory)//'/SCOTT_BURGAN40.TBL'
  file = source
  if (rank /= 0) file = 'nonroot_must_not_open_this_file'
  call standard%Initialization(1.0, filename=trim(file), comm=comm)
  if (.not. standard%legacy_table_compatible) error stop 'distributed SB40 identity changed'
  file = trim(directory)//'/ANDERSON13.TBL'
  if (rank /= 0) file = 'nonroot_must_not_open_this_file'
  call anderson%Initialization(1.0, filename=trim(file), comm=comm)
  if (.not. anderson%legacy_table_compatible) error stop 'distributed Anderson identity changed'
  if (anderson%fgi(13) /= 13.024 .or. anderson%fueldepthm(4) /= 1.829) error stop 'Anderson rounding changed'
  if (size(anderson%moisture%drying_lag) /= 5) error stop 'wrong Anderson moisture classes'
  if (size(standard%moisture%drying_lag) /= 6) error stop 'wrong SB40 moisture classes'
  if (scenario == 'defaults') then
    ! Text attributes identify the fuel table in restart files. Exact roundtrip
    ! preserves all sixteen hexadecimal digits, including leading zeroes.
    if (rank == 0) then
      call Create_netcdf_file('identity.nc')
      call Add_netcdf_att('identity.nc','global','fuel_table_crc64',standard%table_fingerprint)
      call Get_netcdf_att('identity.nc','global','fuel_table_crc64',fingerprint)
      if (fingerprint /= standard%table_fingerprint) error stop 'table identity roundtrip failed'
    end if
  else
    file = 'modified.TBL'
    if (rank == 0) then
      open(newunit=unit, file=trim(source), status='old', action='read')
      open(newunit=output, file=trim(file), status='replace', action='write')
      do
        read(unit,'(a)',iostat=status) line
        if (status /= 0) exit
        if (index(line,'101 ') == 1) then
          select case(trim(scenario))
          case('custom')
            call Column(line,14,'0.20') ! Double only the 1-h dry load [ton/ac].
          case('unknown')
            call Column(line,1,'15')
          case('negative')
            call Column(line,2,'-0.4')
          case('nonfinite')
            call Column(line,2,'NaN')
          case('extra_column')
            call Column(line,2,'0.4 0.4')
          case('missing')
            cycle
          case('reordered')
            saved = line
            cycle
          end select
        end if
        if (index(line,'102 ') == 1) then
          if (scenario == 'duplicate') call Column(line,1,'101')
          if (scenario == 'reordered') then
            write(output,'(a)') trim(line)
            line = saved
          end if
        end if
        if (index(line,'14 ') == 1 .and. index(line,'Nonburnable') > 0) then
          if (scenario == 'nb_load') call Column(line,14,'0.1')
        end if
        if (index(line,'94 ') == 1) error stop 'reserved code in distributed table'
        if (index(line,'93 ') == 1 .and. scenario == 'nb_alias') call Column(line,1,'94')
        if (index(line,'1 1.0 1.4 ') == 1) then
          if (scenario == 'custom') call Column(line,2,'2.0')
          if (scenario == 'zero_lag') call Column(line,2,'0.0')
          if (scenario == 'bad_init') call Column(line,9,'0')
        end if
        if (scenario == 'custom') then
          if (index(line,'CURING_FULL_MOISTURE') == 1) line = 'CURING_FULL_MOISTURE 0.40'
          if (index(line,'CURING_NONE_MOISTURE') == 1) line = 'CURING_NONE_MOISTURE 1.30'
        end if
        if (scenario == 'units') then
          position = index(line,'depth[ft]')
          if (position > 0) line(position:position+8) = 'depth[cm]'
        end if
        if (scenario == 'family' .and. index(line,'FAMILY ') == 1) line = 'FAMILY ANDERSON13'
        if (scenario == 'truncated' .and. line == 'END') cycle
        write(output,'(a)') trim(line)
      end do
      if (scenario == 'trailing') write(output,'(a)') 'UNEXPECTED'
      close(unit)
      close(output)
    end if
    if (scenario == 'missing_file') file = 'missing_table_file.TBL'
    if (rank /= 0) file = 'nonroot_must_not_open_this_file'
    call fuels%Initialization(1.0, filename=trim(file), comm=comm)
    if (fuels%legacy_table_compatible) error stop 'modified table accepted as legacy parameters'
    select case(trim(scenario))
    case('custom')
      if (fuels%fgi_1h(1) /= 0.20) error stop 'custom load ignored'
      if (fuels%fgi(1) <= standard%fgi(1)) error stop 'inventory did not respond to table'
      if (abs(fuels%Cured_fraction(0.85)-0.5) > 2.e-7) error stop 'custom curing endpoints ignored'
      call moisture%Init(fuels,0.08,0.30,0,3,0,3,0,4.0)
      if (moisture%rec_drying_lag_sec(1) /= 1.0/7200.0) error stop 'custom moisture lag ignored'
    case('reordered')
      if (any(fuels%fgi /= standard%fgi)) error stop 'row ordering changed model identity'
      if (any(fuels%fuel_name /= standard%fuel_name)) error stop 'row names lost identity'
    case default
      error stop 'invalid table reached physics initialization'
    end select
  end if
  if (rank == 0) print *, 'Fuel table checks passed: ', trim(scenario)
#ifdef DM_PARALLEL
  call MPI_Comm_free(comm,ierr)
  call MPI_Finalize(ierr)
#endif
contains
  subroutine Column(record, n, value)
    character(len=*), intent(inout) :: record
    integer, intent(in) :: n
    character(len=*), intent(in) :: value
    integer :: start, finish, i
    start = 1
    do i = 1, n-1
      start = start + scan(record(start:),' ')
    end do
    finish = start + scan(record(start:),' ') - 1
    record = record(:start-1)//value//record(finish:)
  end subroutine
end program
