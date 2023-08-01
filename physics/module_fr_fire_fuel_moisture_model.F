!
module module_fr_fire_fuel_moisture_model

use constants_mod, only : XLV
use module_fr_fire_util, only: Crash, Check_mesh_2dim, Print_2d_stats, Message

use state_mod, only: state_fire_t
use namelist_mod, only: namelist_t
    use module_fr_fire_fuel_anderson_mod, only: N_FUEL_CAT


PRIVATE

! subroutines and functions 
PUBLIC:: init_fuel_moisture,fuel_moisture,advance_moisture,check_for_moisture
PUBLIC::mfuelcats,nf,zf,fgi

!***************************************************************************************!
!***                                  Fuel moisture model                            ***!
!***************************************************************************************!
!*** subroutine advance_moisure: a step of time-delay differential equation for each ***!
!***    fuel class (10h, 100h, 1000h) on Earth surface atmospheric mesh, towards     ***!
!***    fuel moisture content equilibrium from the atmosphere state on the surface   ***!
!*** subroutine fuel_moisture: interpolate to fire mesh and average the fuel         ***!
!***    moisture contents from the fuel classes to the fuel present in the fire mesh ***!
!***    cell with weights given in fuel description                                  ***!
!*** See https://www.openwfm.org/wiki/Fuel_moisture_model for further details        ***!
!***    including additional namelist.input and namelist.fire variables.             ***!
!*** Developed 09/2011-09/2012 and merged from https://github.com/openwfm/wrf-fire   ***!
!***    with common ancestor as submitted to V3.3 in 01/2017 commit 3f78267b38e0f0a3 ***!
!*** Reference: J. Mandel, S. Amram, J.D. Beezley, G. Kelman, A.K. Kochanski, V.Y.   ***!
!***    Kondratenko, B.H. Lynn, B. Regev, M. Vejmelka, Recent advances and           ***!
!***    applications of WRF-SFIRE. Natural Hazards and Earth System Science, 14,     ***!
!***    2829-2845, 2014, doi:10.5194/nhess-14-2829-2014                              ***!
!***************************************************************************************!

!! To add moisture classes:
!  1. change parameter max_moisture_classes below
!  2. change the default of nfmc to the same value in Registry/registry.fire
!  3. add the appropriate lines real::fmc_gw<number>= <default values>
!  4. add default

!*** dimensions
  INTEGER, PARAMETER :: mfuelcats = 30     ! allowable number of fuel categories
  INTEGER, PARAMETER ::max_moisture_classes=5
!***

  integer, save:: moisture_classes=5
  real, dimension(max_moisture_classes), save:: drying_lag,wetting_lag,saturation_moisture,saturation_rain, &
         rain_threshold,rec_drying_lag_sec,rec_wetting_lag_sec,fmc_gc_initial_value
  integer, dimension(max_moisture_classes), save:: drying_model,wetting_model,fmc_gc_initialization
   ! relative weights of moisture class for each fuel category
   integer::itmp
   CHARACTER (len=80), DIMENSION(max_moisture_classes), save :: moisture_class_name
  REAL, save:: fmc_1h, fmc_10h, fmc_100h, fmc_1000h, fmc_live

  data moisture_class_name /'1-h','10-h','100-h','1000-h','Live'/
  data drying_lag          /1., 10., 100., 1000.,1e9/  ! time lag (h) approaching equilibrium moisture
  data wetting_lag         /1.4,14., 140., 1400.,1e9/  ! time lag (h) for approaching saturation in rain
  data saturation_moisture /2.5, 2.5, 2.5 ,2.5, 2.5/  ! saturation moisture contents (1) in rain
  data saturation_rain     /8.0, 8.0, 8.0, 8.0, 8.0/  ! stronger rain matters only in duration (mm/h)
  data rain_threshold      /0.05,0.05,0.05,0.05,0.05/ ! rain intensity this small is same as nothing
  data drying_model        /1,   1,   1,   1,   1 / ! future proofing
  data wetting_model       /1,   1,   1,   1,   1 / ! future proofing
  data fmc_gc_initialization/2,  2,   2,   2,   3 / ! initialization 0=input, 1=from fuelmc_g,
                                                    ! 2=from equilibrium, 3=from fmc_1h,...,fmc_live
  data fmc_gc_initial_value/0.,  0.,  0.,  0.,  0./ ! initial value used in the model, to be the actual value fmc_1h,...,fmc_live or read in
  data fmc_1h /0.08/, fmc_10h/0.08/, fmc_100h/0.08/, fmc_1000h/0.08/, fmc_live/0.3/
  ! live fuel moisture following https://www.nwcg.gov/publications/pms437/fuel-moisture/live-fuel-moisture-content
  ! from Rothermel, 1983 Table II-2 p.13 https://www.fs.usda.gov/treesearch/pubs/24635
  ! 300% Fresh foliage, annuals developing early in the growing cycle
  ! 200% Maturing foliage, still developing, with full turgor
  ! 100% Mature foliage, new growth complete and comparable to older perennial foliage
  ! 50%  Entering dormancy, coloration starting, some leaves may have dropped from stem
  ! 30%  Completely cured, treat as dead fuel

! =========================================================================

! fuel categorytables
   INTEGER, PARAMETER :: nf=14              ! number of fuel categories in data stmts
   INTEGER, PARAMETER :: zf = mfuelcats-nf  ! number of zero fillers in data stmt
   REAL   , DIMENSION( mfuelcats ), save :: fgi_1h,fgi_10h,fgi_100h,fgi_1000h,fgi_live, fgi, &
                                            fgi_t,fmc_gwt
   REAL,   DIMENSION(mfuelcats,max_moisture_classes), save :: fgi_c, fmc_gw ! fuel moisture class weights

! fuel loading 1-h, 10-h, 100-h, 1000-h, live following Albini 1976 as reprinted in Anderson 1982 Table 1 (for proportions only)
!                     1      2      3      4      5      6      7      8      9     10     11     12     13     14
  DATA fgi_1h    / 0.74,   2.00,  3.01,  5.01,  1.00,  1.50,  1.13,  1.50,  2.92,  3.01,  1.50,  4.01,  7.01,   0.0,   zf*0./
  DATA fgi_10h   / 0.00,   1.00,  0.00,  4.01,  0.50,  2.50,  1.87,  1.00,  0.41,  2.00,  4.51, 14.03, 23.04,   0.0,   zf*0./
  DATA fgi_100h  / 0.00,   0.50,  0.00,  2.00,  0.00,  2.00,  1.50,  2.50,  0.15,  5.01,  5.51, 16.53, 28.05,   0.0,   zf*0./
  DATA fgi_1000h / 0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,    0.0,   zf*0./
  DATA fgi_live  / 0.00,   0.50,  0.000, 5.01,  2.00,  0.00,  0.37,  0.00,  0.00,  2.00,  0.00,  2.3,   0.00,   0.0,   zf*0./

! total fuel loading kg/m^2
  DATA fgi       / 0.166,  0.896, 0.674, 3.591, 0.784, 1.344, 1.091, 1.120, 0.780, 2.692, 2.582, 7.749, 13.024, 1.e-7, zf*0.  /

contains

subroutine init_fuel_moisture( grid, config_flags )

    implicit none

    type (state_fire_t), target :: grid
    type (namelist_t), intent (in) :: config_flags

    !*** local
    integer:: i, k
    character(len=128):: msg
    real:: rat

    grid%fmoist_lasttime = grid%itimestep * grid%dt
    grid%fmoist_nexttime = grid%fmoist_lasttime

    ! From read_namelist_fire
    if (N_FUEL_CAT > mfuelcats) then
        write(msg,*)'N_FUEL_CAT =',N_FUEL_CAT,' is too large, increase mfuelcats'
        call crash(msg)
    endif

    ! convert fuel loads in the fuel classes to internal as weights adding up to one
    !
    ! copy the fuel weights and scale to 1

    ! just checking
    if (max_moisture_classes.ne.5)then
        call crash('Must have 5 fuel classes, modify source code if not')
    endif
    !**************************************************************************************************
    ! WARNING: initialization from scalars tied to a particular model with 5 fuel moisture classes
    ! the rest of the code can be used for various models with different number of fuel moisture classes
    !
    fgi_c(1:mfuelcats,1)=fgi_1h
    fgi_c(1:mfuelcats,2)=fgi_10h
    fgi_c(1:mfuelcats,3)=fgi_100h
    fgi_c(1:mfuelcats,4)=fgi_1000h
    fgi_c(1:mfuelcats,5)=fgi_live
    fmc_gc_initial_value(1)=fmc_1h
    fmc_gc_initial_value(2)=fmc_10h
    fmc_gc_initial_value(3)=fmc_100h
    fmc_gc_initial_value(4)=fmc_1000h
    fmc_gc_initial_value(5)=fmc_live
    !
    ! end initialization from scalars tied to a particular model with 5 fuel moisture classes
    ! WARNING: delete this and set fgi_c and fmc_gc_initial_value directly for more general models!
    !**************************************************************************************************

    call message('Scaling fuel loads within each fuel category to averaging weights of fuel moisture classes', &
        config_flags%fire_print_msg)
    do i=1,mfuelcats
            fgi_t(i) = 0.
            do k=1,max_moisture_classes
                if(fgi_c(i,k).ge.0.)then
                    fgi_t(i) = fgi_t(i) + fgi_c(i,k)
                else ! no need for OMP CRITICAL this needs to run in master thread anyway
                    write(msg,*)'fuel load in category',i,' fuel class ',k,' is ',fgi_c(i,k),',must be nonegative.'
                    call crash(msg)
                endif
            enddo
            if (fgi_t(i)>0. .or. fgi(i)>0.)then
                if (fgi_t(i)>0.) then
                     rat = fgi(i)/fgi_t(i)
                else
                     rat = 0.
                endif
                write(msg,'(a,i4,1x,a,g13.6,1x,a,g13.6,1x,a,g13.6)') &
                    'fuel category',i,'fuel load',fgi(i),'total by class',fgi_t(i), 'ratio',rat
                call message(msg,config_flags%fire_print_msg)
            endif
            ! fuel moisture averaging weights for fuel classes in category i
            fmc_gwt(i)=0.
            do k=1,max_moisture_classes
               if (fgi_t(i) > 0.) then
                   fmc_gw(i,k) = fgi_c(i,k) / fgi_t(i)
                   fmc_gwt(i) = fmc_gwt(i) + fmc_gw(i,k)
               else
                   fmc_gw(i,k) = 0.
               endif
            enddo
    enddo

    ! moisture model derived scalars
    do i=1,moisture_classes
        rec_drying_lag_sec(i)  = 1.0/(3600.0*drying_lag(i))
        rec_wetting_lag_sec(i) = 1.0/(3600.0*wetting_lag(i))
    enddo

end subroutine init_fuel_moisture

subroutine check_for_moisture( grid, config_flags )

implicit none

type (state_fire_t), target :: grid
type (namelist_t), intent (in) :: config_flags

character(len=128)msg
real:: time_start
real:: moisture_time

    ! time - assume dt does not change
    time_start = grid%itimestep * grid%dt

    ! decide what to run
    if(config_flags%fire_fmc_read.ne.0.and.config_flags%fmoist_run)call crash('fmoist_run=T requires fire_fmc_read=0')

    grid%run_advance_moisture = .false. ! default
    moisture_time = time_start

    if(config_flags%fmoist_run)then
        if(config_flags%fmoist_freq > 0)then  ! regular timestep. go by multiples?
            if(mod(grid%itimestep,config_flags%fmoist_freq) .eq. 0)then
                write(msg,'(a,i10,a,i10)')'moisture model runs because timestep ',grid%itimestep,' is a multiple of ', &
                    config_flags%fmoist_freq
                call message(msg,config_flags%fire_print_msg)
                grid%run_advance_moisture = .true.
            endif
        else
            if(.not. moisture_time  < grid%fmoist_nexttime) then ! no, by time interval
                write(msg,'(a,f12.2,a)')'moisture model runs because time ',grid%fmoist_nexttime,'s has arrived'
                call message(msg,config_flags%fire_print_msg)
                grid%run_advance_moisture = .true.
            endif
        endif
        if(grid%run_advance_moisture)then ! decide on timing
            grid%dt_moisture  = moisture_time - grid%fmoist_lasttime  ! Time since moisture model run the last time. Should be long.
            grid%fmoist_lasttime = moisture_time
            if(config_flags%fmoist_freq > 0)then
                write(msg,'(a,f12.2,a,i10,a)')'moisture time step is ',grid%dt_moisture,'s running every ', &
                    config_flags%fmoist_freq,' steps'
                call message(msg,config_flags%fire_print_msg)
            else
                grid%fmoist_nexttime = moisture_time + config_flags%fmoist_dt
                write(msg,'(a,f12.2,a,f12.2,a)')'moisture time step is ',grid%dt_moisture,'s next run at ',grid%fmoist_nexttime,'s'
                call message(msg,config_flags%fire_print_msg)
            endif
        endif
    endif

end subroutine check_for_moisture

subroutine fuel_moisture(                &
    nfmc,                                &
    ifds, ifde, jfds, jfde,              & ! fire grid dimensions
    ifms, ifme, jfms, jfme,              &
    ifts,ifte,jfts,jfte,                 &
    fire_print_msg,                      &
    nfuel_cat,                           & ! fuel data
    fmc_gc,                              & ! moisture contents by class on atmospheric grid
    fmc_g                                & ! weighted fuel moisture contents on fire grid
    )

implicit none

!**** arguments
integer, intent(in)::                    &
    nfmc,                                &
    ifds, ifde, jfds, jfde,              & ! fire grid dimensions
    ifms, ifme, jfms, jfme,              &
    ifts,ifte,jfts,jfte,                 &
    fire_print_msg

real,intent(in),dimension(ifms:ifme,jfms:jfme):: nfuel_cat ! fuel data
real,intent(in),dimension(ifms:ifme,nfmc,jfms:jfme):: fmc_gc
real,intent(out),dimension(ifms:ifme,jfms:jfme):: fmc_g ! fuel data

!**** local
integer::i,j,k,n
character(len=128)::msg

call check_mesh_2dim(ifts,ifte,jfts,jfte,ifds,ifde,jfds,jfde,fire_print_msg) ! check if fire tile fits into domain
call check_mesh_2dim(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fire_print_msg) ! check if fire tile fits into memory

do j=jfts,jfte
    do i=ifts,ifte
        fmc_g(i,j)=0.               ! initialize sum over classes
    enddo
enddo

do k=1,moisture_classes

    ! add moisture contents for class k to the fuel moisture
    do j=jfts,jfte
        do i=ifts,ifte
            n = nfuel_cat(i,j)
            if(n > 0)then
                fmc_g(i,j)=fmc_g(i,j)+fmc_gw(n,k)*fmc_gc(i,k,j)      ! add to sum over classes
            endif
        enddo
    enddo

    call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fmc_g,'fuel_moisture: fmc_g',fire_print_msg)

enddo


end subroutine fuel_moisture

subroutine advance_moisture(    &
    initialize,                 & ! initialize timestepping. true on the first call at time 0, then false
    ifms,ifme,  jfms,jfme,      & ! fire memory dimensions
    ifts,ifte,  jfts,jfte,      & ! fire tile dimensions
    fire_print_msg,             &
    nfmc,                       & ! dimension of moisture fields
    moisture_dt,                & ! timestep = time step time elapsed from the last call
    fmep_decay_tlag,            & ! moisture extended model assimilated diffs. decay time lag
    rain,                       & ! accumulated rain 
    t2, q2, psfc,               & ! temperature (K), vapor contents (kg/kg), pressure (Pa) at the surface
    rain_old,                   & ! previous value of accumulated rain
    t2_old, q2_old, psfc_old,   & ! previous values of the atmospheric state at surface
    rh_fire,                    & ! relative humidity at surface, for diagnostic only
    fmc_gc,                     & ! fuel moisture by class, updated
    fmep,                       & ! fuel moisture extended model parameters
    fmc_equi,                   & ! fuel moisture equilibrium by class, for diagnostics only
    fmc_lag,                    & ! fuel moisture tendency by classe, for diagnostics only
    fuelmc_g                    & ! fuel moisture content ground from namelist
    )

implicit none

!*** arguments
logical, intent(in):: initialize
integer, intent(in)::           &
    ifms,ifme,  jfms,jfme,      & ! fire memory dimensions
    ifts,ifte,  jfts,jfte,      & ! fire tile dimensions
    nfmc,                       & ! number of moisture fields
    fire_print_msg
real, intent(in):: moisture_dt, fmep_decay_tlag
real, intent(in), dimension(ifms:ifme,jfms:jfme):: t2, q2, psfc, rain
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: t2_old, q2_old, psfc_old, rain_old 
real, intent(inout), dimension(ifms:ifme,nfmc,jfms:jfme):: fmc_gc
real, intent(inout), dimension(ifms:ifme,2,jfms:jfme):: fmep
real, intent(out), dimension(ifms:ifme,nfmc,jfms:jfme):: fmc_equi, fmc_lag
real, intent(out), dimension(ifms:ifme,jfms:jfme)::rh_fire 
real, intent(in) :: fuelmc_g

!*** global
! fuel properties moisture set by init_fuel_cats

!*** local
integer:: i,j,k
real::rain_int, T, P, Q, QRS, ES, RH, tend, EMC_d, EMC_w, EMC, R, rain_diff, fmc, rlag, equi, &
    d, w, rhmax, rhmin, change, rainmax,rainmin, fmc_old, H, deltaS, deltaE
real, parameter::tol=1e-2 ! relative change larger than that will switch to exponential ode solver 
character(len=256)::msg
integer::msglevel=2
logical, parameter::check_data=.true.,check_rh=.false.
real::epsilon,Pws,Pw

!*** executable

! check arguments

if(msglevel>1)then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
    write(msg,'(a,f10.2,a,i4,a,i4)')'advance moisture dt=',moisture_dt,'s using ',moisture_classes,' classes from possible ',nfmc
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
    call message(msg,fire_print_msg)
endif

if(moisture_classes > nfmc .or. moisture_classes > max_moisture_classes)then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
    write(msg,*)'advance_moisture: moisture_classes=',moisture_classes, &
       ' > nfmc=',nfmc,' or >  max_moisture_classes=',max_moisture_classes
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
    call crash(msg)
endif

if(initialize) then 
    call message('advance_moisture: initializing, copying surface variables to old',fire_print_msg)
    call copy2old
endif

if(check_data)then
    do j=jfts,jfte
        do i=ifts,ifte
            if( .not.(t2(i,j)>0.0 .and. psfc(i,j)>0.0 .and. .not. q2(i,j) < 0.0 ))then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
                 write(msg,'(a,2i4,a,3e12.2)')'At i j',i,j,' t2 psfc q2 are ',t2(i,j),psfc(i,j),q2(i,j)
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
                 call message(msg,fire_print_msg) 
                 call crash('invalid data passed from WRF, must have t2 psfc>0, q2 >= 0')
            endif
        enddo
    enddo
endif

! one time step

rhmax=-huge(rhmax)
rhmin=huge(rhmin)
rainmax=-huge(rainmax)
rainmin= huge(rainmin)
do j=jfts,jfte
    do k=1,moisture_classes
        do i=ifts,ifte
            ! old fuel moisture contents
            ! compute the rain intensity from the difference of accumulated rain
            rain_diff = rain(i,j) - rain_old(i,j)
            if(moisture_dt > 0.)then
                rain_int  = 3600. * rain_diff / moisture_dt 
            else
                rain_int  = 0.
            endif
            rainmax = max(rainmax,rain_int)
            rainmin = min(rainmin,rain_int)
            R = rain_int - rain_threshold(k)

            ! average the inputs for second order accuracy
            T = 0.5 * (t2_old(i,j) + t2(i,j))
            P = 0.5 * (psfc_old(i,j) + psfc(i,j))
            Q = 0.5 * (q2_old(i,j) + q2(i,j))

            ! compute the relative humidity
            ! ES=610.78*exp(17.269*(T-273.161)/(T-35.861))
            ! QRS=0.622*ES/(P-0.378*ES)
            ! RH = Q/QRS
            ! function rh_from_q from Adam Kochanski following Murphy and Koop, Q.J.R. Meteorol. Soc (2005) 131 1539-1565 eq. (10)
            epsilon = 0.622 ! Molecular weight of water (18.02 g/mol) to molecular weight of dry air (28.97 g/mol)
            ! vapor pressure [Pa]
            Pw=q*P/(epsilon+(1-epsilon)*q); 
            ! saturation vapor pressure [Pa]
            Pws= exp( 54.842763 - 6763.22/T - 4.210 * log(T) + 0.000367*T + &
                tanh(0.0415*(T - 218.8)) * (53.878 - 1331.22/T - 9.44523 * log(T) + 0.014025*T))
            !realtive humidity [1]
            RH = Pw/Pws
            rh_fire(i,j)=RH
            rhmax=max(RH,rhmax)         
            rhmin=min(RH,rhmin)         

            deltaE = fmep(i,1,j)
            deltaS = fmep(i,2,j)

            if(.not.check_rh)then
                RH = min(RH,1.0)
            else
                if(RH < 0.0 .or. RH > 1.0 .or. RH .ne. RH )then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
                    write(msg,'(a,2i6,5(a,f10.2))')'At i,j ',i,j,' RH=',RH, &
                        ' from T=',T,' P=',P,' Q=',Q
                    call message(msg,fire_print_msg) 
                    call crash('Relative humidity must be between 0 and 1, saturated water contents must be >0')
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
                endif
            endif 
            !print *,'ADV_MOIST i=',i,' j=',j,' T=',T,' P=',P,' Q=',Q,' ES=',ES,' QRS=',QRS,' RH=',RH

            if (R > 0.) then
                select case(wetting_model(k))
                case(1) ! saturation_moisture=2.5 wetting_lag=14h saturation_rain=8 mm/h calibrated to VanWagner&Pickett 1985 per 24 hours
                    EMC_w=saturation_moisture(k) + deltaS
                    EMC_d=saturation_moisture(k) + deltaS
                    rlag=rec_wetting_lag_sec(k) * (1. - exp(-R/saturation_rain(k)))
                end select
            else ! not raining
                select case(drying_model(k))
                case(1) ! Van Wagner and Pickett (1972) per Viney (1991) eq (7) (8)
                    H = RH * 100.
                    d=0.942*H**0.679 + 0.000499*exp(0.1*H) + 0.18*(21.1+273.15-T)*(1-exp(-0.115*H)) ! equilibrium moisture for drying
                    w=0.618*H**0.753 + 0.000454*exp(0.1*H) + 0.18*(21.1+273.15-T)*(1-exp(-0.115*H)) ! equilibrium moisture for adsorbtion
                    if(d.ne.d.or.w.ne.w)call crash('equilibrium moisture calculation failed, result is NaN')
                    d = d*0.01
                    w = w*0.01
                    EMC_d = max(max(d,w)+deltaE,0.0)
                    EMC_w = max(min(d,w)+deltaE,0.0)
                    rlag=rec_drying_lag_sec(k)
                end select
            endif
            !*** MODELS THAT ARE NOT OF THE EXPONENTIAL TIME LAG KIND 
            ! ARE RESPONSIBLE FOR THEIR OWN LOGIC, THESE MODELS
            ! SHOULD COMPUTE fmc_gc(i,k,j) DIRECTLY AND SET TLAG < 0
            !
            if(rlag > 0.0)then

                if(.not.initialize .or. fmc_gc_initialization(k).eq.0)then ! take old from before, no initialization
                    fmc_old = fmc_gc(i,k,j)
                elseif(fmc_gc_initialization(k).eq.1)then ! from scalar fuelmc_g
                    fmc_old = fuelmc_g
                elseif(fmc_gc_initialization(k).eq.2)then ! from computed equilibrium
                    fmc_old=0.5*(EMC_d+EMC_w)
                elseif(fmc_gc_initialization(k).eq.3)then ! from scalar parameter
                    fmc_old = fmc_gc_initial_value(k)
                else
                    call crash('bad value of fmc_gc_initialization(k), must be between 0 and 2')
                endif
                equi = max(min(fmc_old, EMC_d),EMC_w) ! take lower or upper equilibrium value 

                change = moisture_dt * rlag 

                if(change  < tol)then
                     if(fire_print_msg.ge.3)call message('midpoint method',fire_print_msg)
                     fmc = fmc_old + (equi - fmc_old)*change*(1.0 - 0.5*change)  ! 2nd order Taylor
                else
                     if(fire_print_msg.ge.3)call message('exponential method',fire_print_msg)
                     fmc = fmc_old + (equi - fmc_old)*(1 - exp(-change))
                endif
                fmc_gc(i,k,j) = fmc

                ! diagnostics out
                fmc_equi(i,k,j)=equi
                fmc_lag(i,k,j)=1.0/(3600.0*rlag)
                 
                ! diagnostic prints
                if(fire_print_msg.ge.3)then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
		    write(msg,*)'i=',i,' j=',j,'EMC_w=',EMC_w,' EMC_d=',EMC_d
                    call message(msg,fire_print_msg)
                    write(msg,*)'fmc_old=',fmc,' equi=',equi,' change=',change,' fmc=',fmc
                    call message(msg,fire_print_msg)
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
                endif

            endif
      enddo
   enddo
enddo


! assimilated differences decay
do j=jfts,jfte
  do k=1,2
    do i=ifts,ifte
      change = moisture_dt / (fmep_decay_tlag * 3600.)
      if(change < tol) then
        fmep(i,k,j) = fmep(i,k,j)*(1.0 - change * (1.0 - 0.5 * change))
      else
        fmep(i,k,j) = fmep(i,k,j)*exp(-change)
      endif
    enddo
  enddo
enddo


if(fire_print_msg.ge.2)then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
    write(msg,2)'Rain intensity    min',rainmin,  ' max',rainmax,' mm/h'
    call message(msg,fire_print_msg) 
    if(rainmin <0.)then
        call message('WARNING rain accumulation must increase',fire_print_msg)
    endif
    write(msg,2)'Relative humidity min',100*rhmin,' max',100*rhmax,'%'
    call message(msg,fire_print_msg) 
    if(.not.(rhmax<=1.0 .and. rhmin>=0))then
        call message('WARNING Relative humidity must be between 0 and 100%',fire_print_msg)
    endif
2   format(2(a,f10.2),a)
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
endif

call copy2old

return

contains

subroutine copy2old

do j=jfts,jfte
    do i=ifts,ifte
        rain_old(i,j) = rain(i,j)
        t2_old(i,j) = t2(i,j)
        q2_old(i,j) = q2(i,j)
        psfc_old(i,j) = psfc(i,j)
    enddo
enddo

end subroutine copy2old

subroutine get_equi_moist
end subroutine get_equi_moist

end subroutine advance_moisture

end module module_fr_fire_fuel_moisture_model
