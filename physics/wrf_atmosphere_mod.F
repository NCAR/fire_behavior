  module wrf_atmosphere_mod

    use state_mod, only : domain

    implicit none

    private

    public :: Get_ijk_from_subgrid

  contains

    subroutine Get_ijk_from_subgrid (  grid ,                &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0,    &
                           its0, ite0, jts0, jte0, kts0, kte0     )

    ! return the values for subgrid whose refinement is in grid%sr
    ! note when using this routine, it does not affect K. For K 
    ! (vertical), it just returns what get_ijk_from_grid does

      type (domain), intent (in) :: grid
      integer, intent(out) ::                                 &
                       ids0, ide0, jds0, jde0, kds0, kde0,    &
                       ims0, ime0, jms0, jme0, kms0, kme0,    &
                       ips0, ipe0, jps0, jpe0, kps0, kpe0,    &
                       its0, ite0, jts0, jte0, kts0, kte0
        ! Local
      integer ::                              &
                ids, ide, jds, jde, kds, kde, &
                ims, ime, jms, jme, kms, kme, &
                ips, ipe, jps, jpe, kps, kpe, &
                its, ite, jts, jte, kts, kte


      ids0 = grid%ids
      ide0 = grid%ide * grid%sr_x
      ims0 = (grid%ims - 1) * grid%sr_x + 1
      ime0 = grid%ime * grid%sr_x
      ips0 = (grid%ips - 1) * grid%sr_x + 1
      ipe0 = grid%ipe * grid%sr_x
      its0 = (grid%its - 1) * grid%sr_x + 1
      ite0 = (grid%ite - grid%ids + 1) * grid%sr_x + ids0 - 1

      jds0 = grid%jds
      jde0 = grid%jde * grid%sr_y
      jms0 = (grid%jms - 1) * grid%sr_y + 1
      jme0 = grid%jme * grid%sr_y
      jps0 = (grid%jps - 1) * grid%sr_y + 1
      jpe0 = grid%jpe * grid%sr_y
      jts0 = (grid%jts - 1) * grid%sr_y + 1
      jte0 = (grid%jte - grid%jds + 1) * grid%sr_y + jds0 - 1

      kds0 = grid%kds
      kde0 = grid%kde
      kms0 = grid%kms
      kme0 = grid%kme
      kps0 = grid%kps
      kpe0 = grid%kpe
      kts0 = grid%kts
      kte0 = grid%kte

      return

    end subroutine Get_ijk_from_subgrid

  end module wrf_atmosphere_mod
