!
module module_fr_fire_fuel_moisture_model

use constants_mod, only : XLV
use module_fr_fire_util, only: Crash, Check_mesh_2dim, Print_2d_stats, Message
use module_fr_fire_phys, only: fuelmc_g, max_moisture_classes, moisture_classes, &
                               rain_threshold, saturation_moisture, saturation_rain, &
                               wetting_model, rec_drying_lag_sec, rec_wetting_lag_sec, &
                               drying_model, fmc_gc_initialization, fmc_gc_initial_value, fmc_gw

PRIVATE

! subroutines and functions 
PUBLIC:: fuel_moisture,advance_moisture

contains

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
    fmc_lag                     & ! fuel moisture tendency by classe, for diagnostics only
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
