  module fuel_mod

    use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
    use stderrout_mod, only : Stop_simulation, Print_message
    use fuel_table_mod, only: moisture_parameters_t
    use mpi_mod, only : Sum_across_mpi_tasks

    implicit none

    private

    public :: fuel_t, FUEL_ANDERSON, FUEL_SCOTT_BURGAN, UNKNOWN_FUEL_CAT, Crosswalk_from_scottburgan_to_anderson

    integer, parameter :: FUEL_ANDERSON = 1, FUEL_SCOTT_BURGAN = 2, UNKNOWN_FUEL_CAT = 0

    type, abstract :: fuel_t
       integer :: n_fuel_cat, no_fuel_cat
      integer :: n_moisture_classes = 5
      type(moisture_parameters_t) :: moisture
      character(len=16) :: table_fingerprint
      logical :: legacy_table_compatible
        ! Initial consumable surface fuel [kg m-2]. Anderson keeps its legacy
        ! table; SB40 includes all dead and live oven-dry component loads.
      real, dimension(:), allocatable :: fgi
        ! fueldepthm: fuel depth [m]  (in feet in Anderson 1982)
      real, dimension(:), allocatable :: fueldepthm
        ! weight: weighting parameter that determines the slope of the mass loss curve
        ! ----- Notes on weight: (4) - best fit of data from D. Latham (pers. comm.);
        !              (5)-(7) could be 60-120; (8)-(10) could be 300-1600;
        !              (11)-(13) could be 300-1600
      real, dimension(:), allocatable :: weight
        ! ichap: set=1 if fuel is chaparral and want the rate of spread treated differently, 0 if not
      integer, dimension(:), allocatable :: ichap
        ! fueldens: ovendry particle density [lb/ft3]
      real, dimension(:), allocatable :: fueldens
        ! savr: fuel particle surface-area-to-volume ratio, [1/ft]
        !       grass: 3500., 10 hr fuel: 109., 100 hr fuel: 30.
      real, dimension(:), allocatable :: savr
        ! st: fuel particle total mineral content
      real, dimension(:), allocatable :: st
        ! se: fuel particle effective mineral content
      real, dimension(:), allocatable :: se
        ! fuelmce: moisture content of extinction
      real, dimension(:), allocatable :: fuelmce
        ! Component loads [US short ton acre-1]. fgi_live belongs to the
        ! Anderson five-class representation and is unallocated for SB40.
      real, dimension(:), allocatable :: fgi_1h, fgi_10h, fgi_100h, fgi_1000h, fgi_live
        ! wind adjustment factor
      real, dimension(:), allocatable :: waf
    contains
      procedure (Initialization), deferred :: Initialization
      procedure (Resolve_fuel_index), deferred :: Resolve_fuel_index
      procedure (Calc_effective_dead_load), deferred :: Calc_effective_dead_load
      procedure, public :: Resolve_fuel_indices
      procedure, public :: Moisture_loads
      procedure, public :: Check_fuels => Check_fuels
      procedure, public :: Calc_wind_adjustment_factor => Calc_wind_adjustment_factor
    end type fuel_t

    abstract interface
      subroutine Initialization (this, fuelmc_c, filename, comm)
        import :: fuel_t
        class (fuel_t), intent (in out) :: this
        real, intent(in) :: fuelmc_c
        character(len=*), intent(in), optional :: filename
        integer, intent(in), optional :: comm
      end subroutine Initialization
      pure integer function Resolve_fuel_index(this, external_code) result(idx)
        import :: fuel_t
        class(fuel_t), intent(in) :: this
        integer, intent(in) :: external_code
      end function

      pure real function Calc_effective_dead_load(this, k, mc_live) result(load)
        import :: fuel_t
        class(fuel_t), intent(in) :: this
        integer, intent(in) :: k
        real, intent(in) :: mc_live
      end function
    end interface

  contains

    pure function Moisture_loads(this, k, mc_live) result(loads)
      class(fuel_t), intent(in) :: this
      integer, intent(in) :: k
      real, intent(in) :: mc_live
      real :: loads(this%n_moisture_classes)
      ! Anderson retains its original five-class weighting in short tons/acre.
      loads = [this%fgi_1h(k), this%fgi_10h(k), this%fgi_100h(k), this%fgi_1000h(k), this%fgi_live(k)]
    end function

    subroutine Resolve_fuel_indices(this, ifms, ifme, jfms, jfme, num_tiles, &
        i_start, i_end, j_start, j_end, nfuel_cat, fuel_index, comm)
      class(fuel_t), intent(in) :: this
      integer, intent(in) :: ifms, ifme, jfms, jfme, num_tiles, comm
      integer, intent(in) :: i_start(num_tiles), i_end(num_tiles), j_start(num_tiles), j_end(num_tiles)
      real, intent(in) :: nfuel_cat(ifms:ifme,jfms:jfme)
      integer, intent(out) :: fuel_index(ifms:ifme,jfms:jfme)
      integer :: tile, i, j, idx, unknown, samples
      real :: global_unknown
      character(len=200) :: msg

      ! Preserve external map codes. Only owned tiles contain valid categories;
      ! leave halos explicitly unresolved rather than silently marking no fuel.
      fuel_index = UNKNOWN_FUEL_CAT
      unknown = 0
      !$omp parallel do private(tile,i,j,idx) reduction(+:unknown)
      do tile = 1, num_tiles
        do j = j_start(tile), j_end(tile)
          do i = i_start(tile), i_end(tile)
            idx = UNKNOWN_FUEL_CAT
            ! Validate before INT: missing, nonfinite and fractional map values
            ! must not truncate to valid categories or reach a table subscript.
            if (ieee_is_finite(nfuel_cat(i,j))) then
              if (nfuel_cat(i,j) >= 1.0 .and. nfuel_cat(i,j) <= 1000000.0) then
                if (nfuel_cat(i,j) == real(int(nfuel_cat(i,j)))) &
                    idx = this%Resolve_fuel_index(int(nfuel_cat(i,j)))
              end if
            end if
            fuel_index(i,j) = idx
            if (idx <= 0 .or. idx > this%n_fuel_cat + 1) unknown = unknown + 1
          end do
        end do
      end do
      !$omp end parallel do
      ! MPI and diagnostics remain on the initial thread after tile completion.
      call Sum_across_mpi_tasks(real(unknown), comm, global_unknown)
      if (global_unknown > 0.0) then
        write(msg,'(a,i0,a,i0)') 'Unknown fuel categories: local_count=',unknown,' global_count=',nint(global_unknown)
        call Print_message(trim(msg))
        samples = 0
        do tile = 1, num_tiles
          do j = j_start(tile), j_end(tile)
            do i = i_start(tile), i_end(tile)
              idx = fuel_index(i,j)
              if ((idx <= 0 .or. idx > this%n_fuel_cat + 1) .and. samples < 8) then
                write(msg,'(a,es16.8,a,i0,a,i0)') '  invalid external fuel code ',nfuel_cat(i,j),' at i=',i,' j=',j
                call Print_message(trim(msg))
                samples = samples + 1
              end if
            end do
          end do
        end do
        call Stop_simulation('Unknown nfuel_cat values require explicit fuel or nonburnable mapping')
      end if
    end subroutine Resolve_fuel_indices

    subroutine Check_fuels (this)

      implicit none

      class (fuel_t), intent (in) :: this
      integer :: k
      character (len = 128) :: msg


      ! Extinction moisture is a fixed table property, checked once at initialization.
      do k = 1, this%n_fuel_cat
        if (k == this%no_fuel_cat) cycle
        if (.not. ieee_is_finite (this%fuelmce(k))) then
          write (msg, '(a,1x,i0)') 'fuelmce must be finite for fuel category:', k
          call Stop_simulation (msg)
        else if (this%fuelmce(k) <= 0.0) then
          write (msg, '(a,1x,i0)') 'fuelmce must be positive for fuel category:', k
          call Stop_simulation (msg)
        end if
      end do

    end subroutine Check_fuels

    pure function Crosswalk_from_scottburgan_to_anderson (fuel_cat) result (return_value)

      implicit none

      integer, intent (in) :: fuel_cat

      integer :: return_value


      select case (fuel_cat)
        case (1, 2, 3, 4, 5 ,6, 7, 8, 9, 10, 11, 12, 13, 14)
            ! notheing to do
          return_value = fuel_cat

        case (101, 104, 107)
             ! Scott & Burgan crosswalks
             ! Short grass -- 1
           return_value = 1

        case (102, 121, 122, 123, 124)
            ! Timber grass and understory -- 2
          return_value = 2

        case (103, 105, 106, 108, 109)
            ! Tall grass -- 3
          return_value = 3

        case (145, 147)
            ! Chaparral -- 4
          return_value = 4

        case (142)
            ! Brush -- 5
          return_value = 5

        case (141, 146)
            ! Dormant Brushi -- 6
          return_value = 6

        case (143, 144, 148, 149)
            ! Southern Rough -- 7
          return_value = 7

        case (181, 183, 184, 187)
            ! Compact Timber Litter -- 8
          return_value = 8

        case (182, 186, 188, 189)
            ! Hardwood Litter -- 9
          return_value = 9

        case (161, 162, 163, 164, 165)
            ! Timber (understory) -- 10
          return_value = 10

        case (185, 201)
            ! Light Logging Slash -- 11
          return_value = 11

        case (202)
            ! Medium Logging Slash -- 12
          return_value = 12

        case (203, 204)
            ! Heavy Logging Slash -- 13
          return_value = 13

        case default
          return_value = UNKNOWN_FUEL_CAT

        end select

    end function Crosswalk_from_scottburgan_to_anderson

    subroutine Calc_wind_adjustment_factor (this)

      implicit none

      class (fuel_t), intent (in out) :: this

      ! local
      integer :: i
      real :: wh, flamelength, h
      real :: HF ! extent of the flame above the vegetation


      ! Both wind paths can index the no-fuel row; it must be allocated.
      allocate (this%waf(this%n_fuel_cat + 1))
      wh = 10.0 ! input wind height in meter

      do i = 1,this%n_fuel_cat + 1
        h = this%fueldepthm(i)
        flamelength = 2.0 * h ! assume flamelength is double the fuel bed height
        if (flamelength > h) then
          hf = max(flamelength - h, 0.13 * h)
          this%waf(i) = (1.0 + 0.36 * h / hf) / log(((wh-h) + 0.36 * h) / (0.13 * h)) * (log( (hf/h + 0.36)/0.13) - 1.0)
        else
          this%waf(i) = 0.15
        endif
      end do

      this%waf(this%no_fuel_cat) = 1.e-7

    end subroutine Calc_wind_adjustment_factor

  end module fuel_mod
