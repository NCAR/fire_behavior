program tiles_unit

  ! Divide an 8-by-6 domain into four tiles and check that tiled work covers
  ! the complete domain. In an OpenMP build, also require four distinct threads
  ! to process nonempty tiles, so a serial execution cannot satisfy that check.

  use tiles_mod, only : Calc_tiles_dims
#ifdef _OPENMP
  use omp_lib, only : omp_get_thread_num
#endif

  implicit none

  integer, dimension(:), allocatable :: i_start, i_end, j_start, j_end
  integer, dimension(4) :: tile_thread
  integer, dimension(1:8, 1:6) :: work
  integer :: i, j, num_tiles, tile


  num_tiles = 4
  call Calc_tiles_dims (1, 8, 1, 6, num_tiles, 3, i_start, i_end, j_start, j_end)
  if (num_tiles /= 4) error stop 'four-tile layout was not retained'

  work = 0
  tile_thread = -1
  !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(tile, i, j) SCHEDULE(STATIC, 1)
  do tile = 1, num_tiles
#ifdef _OPENMP
    tile_thread(tile) = omp_get_thread_num ()
#else
    tile_thread(tile) = 0
#endif
    do j = j_start(tile), j_end(tile)
      do i = i_start(tile), i_end(tile)
        work(i, j) = tile
      end do
    end do
  end do
  !$OMP END PARALLEL DO

  if (any (work == 0)) error stop 'tiled work did not cover the complete domain'
#ifdef _OPENMP
  if (Count_unique (tile_thread) /= 4) error stop 'OpenMP-4 did not assign nonempty tiles to four threads'
#endif

  write (*, '(a,i0,a,4(1x,i0))') 'resolved tiles=', num_tiles, ' thread ids=', tile_thread

contains

  function Count_unique (values) result (number_unique)

    implicit none

    integer, dimension(:), intent (in) :: values
    integer :: i, number_unique


    number_unique = 0
    do i = 1, size (values)
      if (i == 1) then
        number_unique = 1
      else if (.not. any (values(1:i - 1) == values(i))) then
        number_unique = number_unique + 1
      end if
    end do

  end function Count_unique

end program tiles_unit
