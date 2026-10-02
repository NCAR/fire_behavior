program tile_namelist_unit

  use namelist_mod, only : namelist_t

  implicit none

  type (namelist_t) :: config
  integer :: unit_namelist


  open (newunit=unit_namelist, file='tile_strategy_unit.nml', status='replace', action='write')
  write (unit_namelist, '(a)') '&time'
  write (unit_namelist, '(a)') '  num_tiles = 4,'
  write (unit_namelist, '(a)') '  tile_strategy = 3,'
  write (unit_namelist, '(a)') '/'
  close (unit_namelist)

  call config%Init_time_block ('tile_strategy_unit.nml')
  if (config%num_tiles /= 4) error stop 'num_tiles was not read from the time namelist'
  if (config%tile_strategy /= 3) error stop 'tile_strategy was not read from the time namelist'

end program tile_namelist_unit
