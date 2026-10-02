program namelist_broadcast_mpi

  use mpi
  use namelist_mod, only : namelist_t

  implicit none

  integer, parameter :: FMC_SENTINEL = 41729
  type (namelist_t) :: config
  integer :: ierr, local_comm, local_rank, world_rank, world_size


  call MPI_Init (ierr)
  if (ierr /= MPI_SUCCESS) error stop 'MPI_Init failed'
  call MPI_Comm_rank (MPI_COMM_WORLD, world_rank, ierr)
  call MPI_Comm_size (MPI_COMM_WORLD, world_size, ierr)

  call MPI_Comm_split (MPI_COMM_WORLD, 0, world_size - world_rank, local_comm, ierr)
  if (ierr /= MPI_SUCCESS) error stop 'MPI_Comm_split failed'
  call MPI_Comm_rank (local_comm, local_rank, ierr)

  config%fmc_opt = -1
  if (local_rank == 0) config%fmc_opt = FMC_SENTINEL
  call config%Broadcast_nml (local_comm)

  if (config%fmc_opt /= FMC_SENTINEL) error stop 'fmc_opt was not broadcast on the supplied communicator'
  if (local_rank == 0 .and. world_rank == 0) error stop 'split-communicator root unexpectedly equals world rank zero'

  call MPI_Comm_free (local_comm, ierr)
  call MPI_Finalize (ierr)

end program namelist_broadcast_mpi
