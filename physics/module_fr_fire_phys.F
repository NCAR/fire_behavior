!
module module_fr_fire_phys

use constants_mod, only : XLV
use module_fr_fire_util, only: Crash, Check_mesh_2dim, Print_2d_stats, Message

PRIVATE

! subroutines and functions 
PUBLIC:: init_fuel_cats,fire_ros,heat_fluxes,set_fire_params
PUBLIC:: read_namelist_fire

! variables
PUBLIC:: moisture_classes, max_moisture_classes, rain_threshold, saturation_moisture, &
         saturation_rain, wetting_model, rec_drying_lag_sec, rec_wetting_lag_sec, &
         drying_model, fmc_gc_initialization, fmc_gc_initial_value, fmc_gw
PUBLIC::fuelmc_g

PUBLIC::fire_params

! arrays passed to fire_ros
type fire_params
real,pointer,dimension(:,:):: vx,vy                ! wind velocity (m/s)
real,pointer,dimension(:,:):: zsf                  ! terrain height (m) 
real,pointer,dimension(:,:):: dzdxf,dzdyf          ! terrain grad (1) 
real,pointer,dimension(:,:):: bbb,betafl,phiwc,r_0 ! spread formula coefficients
real,pointer,dimension(:,:):: fgip                 ! init mass of surface fuel (kg/m^2)
real,pointer,dimension(:,:):: ischap               ! if fuel is chaparral and want rate of spread treated differently
real,pointer,dimension(:,:):: iboros               ! fire instensity over rate of spread -> avialable fuel x heat of combustion
real,pointer,dimension(:,:):: fuel_time            ! time to burn to 1/e (s) 
real,pointer,dimension(:,:):: fmc_g                ! fuel moisture contents, ground (1) 
end type fire_params

! use as
! type(fire_params)::fp

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

!D in col 2 means quantity derived from the others
!
!  Scalar constants (data same for all fuel categories):
!       HFGL           SURFACE FIRE HEAT FLUX THRESHOLD TO IGNITE CANOPY (W/m^2)
!       CMBCNST        JOULES PER KG OF DRY FUEL
!       FUELHEAT       FUEL PARTICLE LOW HEAT CONTENT, BTU/LB
!       FUELMC_G       FUEL PARTICLE (SURFACE) MOISTURE CONTENT
!D      BMST           RATIO OF LATENT TO SENSIBLE HEAT FROM SFC BURN:
!                        % of total fuel mass that is water (not quite
!                        = % fuel moisture).    BMST= (H20)/(H20+DRY)
!                        so BMST = FUELMC_G / (1 + FUELMC_G)  where
!                        FUELMC_G = moisture content of surface fuel
!
!  Data arrays indexed by fuel category:
!       FGI            INITIAL TOTAL MASS OF SURFACE FUEL (KG/M**2)
!       FUELDEPTHM     FUEL DEPTH, IN M  (CONVERTED TO FT)              
!       SAVR           FUEL PARTICLE SURFACE-AREA-TO-VOLUME RATIO, 1/FT
!                         GRASS: 3500., 10 hr fuel: 109.,  100 hr fuel: 30.
!       FUELMCE        MOISTURE CONTENT OF EXTINCTION; 0.30 FOR MANY DEAD FUELS; 0.15 FOR GRASS
!       FUELDENS       OVENDRY PARTICLE DENSITY, LB/FT^3
!       ST             FUEL PARTICLE TOTAL MINERAL CONTENT
!       SE             FUEL PARTICLE EFFECTIVE MINERAL CONTENT
!       WEIGHT         WEIGHTING PARAMETER THAT DETERMINES THE SLOPE OF THE MASS LOSS CURVE
!                        RANGES FROM ~5 (FAST BURNUP) TO 1000 ( ~40% DECR OVER 10 MIN).
!       FCI_D          INITIAL DRY MASS OF CANOPY FUEL
!       FCT            BURN OUT TIME FOR CANOPY FUEL, AFTER DRY (S)
!       ichap          Set=1 if fuel is chaparral and want the rate of spread treated differently, 0 if not
!D      FCI            INITIAL TOTAL MASS OF CANOPY FUEL
!D      FCBR           FUEL CANOPY BURN RATE (KG/M**2/S) 

! =============================================================================
! Anderson 13 surface fire fuel models, along with some
!          estimated canopy properties (for crown fire).
! =============================================================================
!  --- Grass-dominated fuel models
!  FUEL MODEL 1: Short grass (1 ft)
!  FUEL MODEL 2: Timber (grass and understory)
!  FUEL MODEL 3: Tall grass (2.5 ft)
!  --- Shrub-dominated fuel models
!  FUEL MODEL 4: Chaparral (6 ft)
!  FUEL MODEL 5: Brush (2 ft) 
!  FUEL MODEL 6: Dormant brush, hardwood slash
!  FUEL MODEL 7: Southern rough
!  --- Forest litter-dominated fuel models
!  FUEL MODEL 8: Closed timber litter
!  FUEL MODEL 9: Hardwood litter
!  FUEL MODEL 10: Timber (litter + understory)
!  --- Logging debris-dominated fuel models
!  FUEL MODEL 11: Light logging slash
!  FUEL MODEL 12: Medium logging slash
!  FUEL MODEL 13: Heavy logging slash
!  --- Fuel-free 
!  FUEL MODEL 14: no fuel

! scalar fuel coefficients
   REAL, SAVE:: cmbcnst,hfgl,fuelmc_g,fuelmc_c
! computed values
   REAL, SAVE:: fuelheat

! defaults, may be changed in init_fuel_cats
   DATA cmbcnst  / 17.433e+06/             ! J/kg dry fuel
   DATA hfgl     / 17.e4 /                ! W/m^2
   DATA fuelmc_g / 0.08  /                ! set = 0 for dry surface fuel
   DATA fuelmc_c / 1.00  /                ! set = 0 for dry canopy
!  REAL, PARAMETER :: bmst     = fuelmc_g/(1+fuelmc_g)
!  REAL, PARAMETER :: fuelheat = cmbcnst * 4.30e-04     ! convert J/kg to BTU/lb
!  real, parameter :: xlv      = 2.5e6                  ! to make it selfcontained
!  real, parameter :: cp      =  7.*287./2              ! to make it selfcontained


! fuel categorytables
   INTEGER, PARAMETER :: nf=14              ! number of fuel categories in data stmts
   INTEGER, SAVE      :: nfuelcats = 13     ! number of fuel categories that are specified
   INTEGER, PARAMETER :: zf = mfuelcats-nf  ! number of zero fillers in data stmt 
   INTEGER, SAVE      :: no_fuel_cat = 14   ! special category outside of 1:nfuelcats
   CHARACTER (len=80), DIMENSION(mfuelcats ), save :: fuel_name
   INTEGER, DIMENSION( mfuelcats ), save :: ichap
   REAL   , DIMENSION( mfuelcats ), save :: windrf,weight,fgi,fci,fci_d,fct,fcbr, &
                                            fueldepthm,fueldens,fuelmce,   &
                                            savr,st,se, &
                                            fgi_1h,fgi_10h,fgi_100h,fgi_1000h,fgi_live, &
                                            fgi_t,fmc_gwt
   REAL,   DIMENSION(mfuelcats,max_moisture_classes), save :: fgi_c, fmc_gw ! fuel moisture class weights
   DATA fuel_name /'1: Short grass (1 ft)', &
     '2: Timber (grass and understory)', &
     '3: Tall grass (2.5 ft)', &
     '4: Chaparral (6 ft)', &
     '5: Brush (2 ft) ', &
     '6: Dormant brush, hardwood slash', &
     '7: Southern rough', &
     '8: Closed timber litter', &
     '9: Hardwood litter', &
     '10: Timber (litter + understory)', &
     '11: Light logging slash', &
     '12: Medium logging slash', &
     '13: Heavy logging slash', &
     '14: no fuel', zf* ' '/

   DATA windrf /0.36, 0.36, 0.44,  0.55,  0.42,  0.44,  0.44, &     
                0.36, 0.36, 0.36,  0.36,  0.43,  0.46,  1e-7, zf*0 /
   DATA fueldepthm /0.305,  0.305,  0.762, 1.829, 0.61,  0.762,0.762, &
                    0.0610, 0.0610, 0.305, 0.305, 0.701, 0.914, 0.305,zf*0. /
   DATA savr / 3500., 2784., 1500., 1739., 1683., 1564., 1562.,  &
               1889., 2484., 1764., 1182., 1145., 1159., 3500., zf*0. /
   DATA fuelmce / 0.12, 0.15, 0.25, 0.20, 0.20, 0.25, 0.40,  &
                  0.30, 0.25, 0.25, 0.15, 0.20, 0.25, 0.12 , zf*0. / 
   DATA fueldens / nf * 32., zf*0. /   ! 32 if solid, 19 if rotten.
   DATA st / nf* 0.0555 , zf*0./
   DATA se / nf* 0.010 , zf*0./
! ----- Notes on weight: (4) - best fit of data from D. Latham (pers. comm.);
!              (5)-(7) could be 60-120; (8)-(10) could be 300-1600;
!              (11)-(13) could be 300-1600
   DATA weight / 7.,  7.,  7., 180., 100., 100., 100.,  &
              900., 900., 900., 900., 900., 900., 7. , zf*0./ 
! ----- 1.12083 is 5 tons/acre.  5-50 tons/acre orig., 100-300 after blowdown
   DATA fci_d / 0., 0., 0., 1.123, 0., 0., 0.,  &
            1.121, 1.121, 1.121, 1.121, 1.121, 1.121, 0., zf*0./
   DATA fct / 60., 60., 60., 60., 60., 60., 60.,  &
            60., 120., 180., 180., 180., 180. , 60. , zf*0.   /
   DATA ichap / 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 , zf*0/
!  DATA fmc_gw05 / 0.000, 0.023, 0.000, 0.230, 0.092, 0.000, 0.017, 0.000, 0.000, 0.092, 0.000, 0.000, 0.000, zf*0/

! fuel loading 1-h, 10-h, 100-h, 1000-h, live following Albini 1976 as reprinted in Anderson 1982 Table 1 (for proportions only)
!                     1      2      3      4      5      6      7      8      9     10     11     12     13     14
  DATA fgi_1h    / 0.74,   2.00,  3.01,  5.01,  1.00,  1.50,  1.13,  1.50,  2.92,  3.01,  1.50,  4.01,  7.01,   0.0,   zf*0./
  DATA fgi_10h   / 0.00,   1.00,  0.00,  4.01,  0.50,  2.50,  1.87,  1.00,  0.41,  2.00,  4.51, 14.03, 23.04,   0.0,   zf*0./
  DATA fgi_100h  / 0.00,   0.50,  0.00,  2.00,  0.00,  2.00,  1.50,  2.50,  0.15,  5.01,  5.51, 16.53, 28.05,   0.0,   zf*0./
  DATA fgi_1000h / 0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,    0.0,   zf*0./
  DATA fgi_live  / 0.00,   0.50,  0.000, 5.01,  2.00,  0.00,  0.37,  0.00,  0.00,  2.00,  0.00,  2.3,   0.00,   0.0,   zf*0./

! total fuel loading kg/m^2
  DATA fgi       / 0.166,  0.896, 0.674, 3.591, 0.784, 1.344, 1.091, 1.120, 0.780, 2.692, 2.582, 7.749, 13.024, 1.e-7, zf*0.  /

! =========================================================================
contains

subroutine read_namelist_fire(init_fuel_moisture,fire_print_msg)

implicit none

logical, intent(in)::init_fuel_moisture
integer, intent(in)::fire_print_msg
!*** local
integer:: i, k
character(len=128):: msg
real:: rat

    if (nfuelcats>mfuelcats) then
        write(msg,*)'nfuelcats=',nfuelcats,' is too large, increase mfuelcats'
        call crash(msg)
    endif
    if (no_fuel_cat >= 1 .and. no_fuel_cat <= nfuelcats)then
        write(msg,*)'no_fuel_cat=',no_fuel_cat,' may not be between 1 and nfuelcats=',nfuelcats
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

    call message('Scaling fuel loads within each fuel category to averaging weights of fuel moisture classes',fire_print_msg)
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
                call message(msg,fire_print_msg)
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

end subroutine read_namelist_fire


subroutine init_fuel_cats(init_fuel_moisture,fire_print_msg)
implicit none
!*** purpose: initialize fuel tables and variables by constants
!*** arguments: 
logical, intent(in)::init_fuel_moisture
integer, intent(in)::fire_print_msg
!*** local
integer:: i,j,k,ii
integer:: kk
character(len=128):: msg
!*** executable

call read_namelist_fire(init_fuel_moisture,fire_print_msg)

! moisture model derived scalars
do i=1,moisture_classes
    rec_drying_lag_sec(i)  = 1.0/(3600.0*drying_lag(i))
    rec_wetting_lag_sec(i) = 1.0/(3600.0*wetting_lag(i))
enddo

! compute derived scalars

fuelheat = cmbcnst * 4.30e-04     ! convert J/kg to BTU/lb

! compute derived fuel category coefficients 

DO i = 1,nfuelcats
    fci(i) = (1.+fuelmc_c)*fci_d(i)
    if(fct(i) .ne.  0.)then
        fcbr(i) = fci_d(i)/fct(i) !  avoid division by zero
    else
        fcbr(i) = 0
    endif
END DO

end subroutine init_fuel_cats


subroutine set_fire_params( &
                           ifds,ifde,jfds,jfde, &
                           ifms,ifme,jfms,jfme, &
                           ifts,ifte,jfts,jfte, &
                           fdx,fdy,             &
                           nfuel_cat,fuel_time, &
                           fp, fire_fmc_read, fire_print_msg ) 

implicit none

!*** purpose: Set all fire model params arrays, constant values.

!*** arguments
integer, intent(in)::ifds,ifde,jfds,jfde                        ! fire domain bounds
integer, intent(in)::ifts,ifte,jfts,jfte                        ! fire tile bounds
integer, intent(in)::ifms,ifme,jfms,jfme                        ! memory array bounds
real, intent(in):: fdx,fdy                                      ! fire mesh spacing
integer,intent(in)::fire_fmc_read, fire_print_msg
real, intent(in),dimension(ifms:ifme, jfms:jfme)::nfuel_cat  ! fuel data
real, intent(out), dimension(ifms:ifme, jfms:jfme)::fuel_time   ! fire params arrays
type(fire_params),intent(inout)::fp

!*** local

real::  fuelload, fueldepth, rtemp1, rtemp2, &
        qig, epsilon, rhob, wn, betaop, e, c, &
        xifr, etas, etam, a, gammax, gamma, ratio, ir, &
        fuelloadm,fdxinv,fdyinv
real:: bmst
integer:: i,j,k
character(len=128)::msg

integer :: kk
integer,parameter :: nf_sb = 204 ! maximum category on 
integer,dimension(1:nf_sb) :: ksb ! Anderson82 + S&B2005 fuel categories array

!*** executable

!*** Scott & Burgan ***! 
! assign no fuel by default to all the categories
do kk=1,nf_sb
   ksb(kk)=14
enddo
! Anderson 1982
ksb(1)=1
ksb(2)=2
ksb(3)=3
ksb(4)=4
ksb(5)=5
ksb(6)=6
ksb(7)=7
ksb(8)=8
ksb(9)=9
ksb(10)=10
ksb(11)=11
ksb(12)=12
ksb(13)=13
! Scott & Burgan crosswalks
! Short grass -- 1
ksb(101)=1
ksb(104)=1
ksb(107)=1
! Timber grass and understory -- 2
ksb(102)=2
ksb(121)=2
ksb(122)=2
ksb(123)=2
ksb(124)=2
! Tall grass -- 3
ksb(103)=3
ksb(105)=3
ksb(106)=3
ksb(108)=3
ksb(109)=3
! Chaparral -- 4
ksb(145)=4
ksb(147)=4
! Brush -- 5
ksb(142)=5
! Dormant Brushi -- 6
ksb(141)=6
ksb(146)=6
! Southern Rough -- 7
ksb(143)=7
ksb(144)=7
ksb(148)=7
ksb(149)=7
! Compact Timber Litter -- 8
ksb(181)=8
ksb(183)=8
ksb(184)=8
ksb(187)=8
! Hardwood Litter -- 9
ksb(182)=9
ksb(186)=9
ksb(188)=9
ksb(189)=9
! Timber (understory) -- 10
ksb(161)=10
ksb(162)=10
ksb(163)=10
ksb(164)=10
ksb(165)=10
! Light Logging Slash -- 11
ksb(185)=11
ksb(201)=11
! Medium Logging Slash -- 12
ksb(202)=12
! Heavy Logging Slash -- 13
ksb(203)=13
ksb(204)=13

! ****** !

do j=jfts,jfte
   do i=ifts,ifte
     ! fuel category 
     k=ksb(int(nfuel_cat(i,j))) ! DME S&B05
     if(k.eq.no_fuel_cat)then   ! no fuel 
        fp%fgip(i,j)=0.            ! no mass 
        fp%ischap(i,j)=0.
        fp%betafl(i,j)=1.          ! DME: set to 1.0 to prevent fp%betafl(i,j)**(-0.3) to be Inf in fire_ros
        fp%bbb(i,j)=1.             !
        fuel_time(i,j)=7./0.85  ! does not matter, just what was there before
        fp%phiwc(i,j)=0.
        fp%r_0(i,j)=0.             ! no fuel, no spread.
        fp%iboros(i,j)=0.   ! DME Ib/ROS zero for no fuel
     else
        if(k.lt.1.or.k.gt.nfuelcats)then
!$OMP CRITICAL(FIRE_PHYS_CRIT)
            write(msg,'(3(a,i5))')'nfuel_cat(', i ,',',j,')=',k
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
            call message(msg,fire_print_msg)
            call crash('set_fire_params: fuel category out of bounds')
        endif

        fuel_time(i,j)=weight(k)/0.85 ! cell based
        
        ! set fuel time constant: weight=1000 => 40% decrease over 10 min
        ! fuel decreases as exp(-t/fuel_time) 
        ! exp(-600*0.85/1000) = approx 0.6 

        fp%ischap(i,j)=ichap(k)
        fp%fgip(i,j)=fgi(k)
        ! PAJM: is it possible that if fire_fmc_read == 0, needed by the FMC model, fp%fmc_g is not initialized?
        if(fire_fmc_read.eq.1)then
           fp%fmc_g(i,j)=fuelmc_g
        endif

        !     ...Settings of fire spread parameters from Rothermel follows. These
        !        don't need to be recalculated later.
        
        bmst     = fp%fmc_g(i,j) / (1.+fp%fmc_g(i,j))
        fuelloadm= (1.-bmst) * fgi(k)  !  fuelload without moisture
        fuelload = fuelloadm * (.3048)**2 * 2.205    ! to lb/ft^2
        fueldepth = fueldepthm(k)/0.3048               ! to ft
        fp%betafl(i,j) = fuelload/(fueldepth * fueldens(k))! packing ratio
        betaop = 3.348 * savr(k)**(-0.8189)     ! optimum packing ratio
        qig = 250. + 1116.*fp%fmc_g(i,j)            ! heat of preignition, btu/lb
        epsilon = exp(-138./savr(k) )    ! effective heating number
        rhob = fuelload/fueldepth    ! ovendry bulk density, lb/ft^3

        c = 7.47 * exp( -0.133 * savr(k)**0.55)    ! const in wind coef
        fp%bbb(i,j) = 0.02526 * savr(k)**0.54      ! const in wind coef
        e = 0.715 * exp( -3.59e-4 * savr(k))       ! const in wind coef
        fp%phiwc(i,j) = c * (fp%betafl(i,j)/betaop)**(-e)

        rtemp2 = savr(k)**1.5
        gammax = rtemp2/(495. + 0.0594*rtemp2)              ! maximum rxn vel, 1/min
        a = 1./(4.774 * savr(k)**0.1 - 7.27)   ! coef for optimum rxn vel
        ratio = fp%betafl(i,j)/betaop
        gamma = gammax *(ratio**a) *exp(a*(1.-ratio)) !optimum rxn vel, 1/min

        wn = fuelload/(1 + st(k))       ! net fuel loading, lb/ft^2
        rtemp1 = fp%fmc_g(i,j)/fuelmce(k)
        etam = 1.-2.59*rtemp1 +5.11*rtemp1**2 -3.52*rtemp1**3  !moist damp coef
        etas = 0.174* se(k)**(-0.19)                ! mineral damping coef
        ir = gamma * wn * fuelheat * etam * etas  !rxn intensity,btu/ft^2 min
        ! irm = ir * 1055./( 0.3048**2 * 60.) * 1.e-6     !for mw/m^2
        fp%iboros(i,j) = ir * 1055./( 0.3048**2 * 60.) * 1.e-3 * (60.*12.6/savr(k))     ! I_R x t_r (kJ m^-2)

        xifr = exp( (0.792 + 0.681*savr(k)**0.5) &
            * (fp%betafl(i,j)+0.1)) /(192. + 0.2595*savr(k)) ! propagating flux ratio

!        ... r_0 is the spread rate for a fire on flat ground with no wind.
        fp%r_0(i,j) = ir*xifr/(rhob * epsilon *qig)    ! default spread rate in ft/min

     endif
  enddo
enddo

end subroutine set_fire_params

!
!*******************
!

subroutine heat_fluxes(dt,fp,                     &
        ifms,ifme,jfms,jfme,                      &  ! memory dims
        ifts,ifte,jfts,jfte,                      &  ! tile dims
        iffs,iffe,jffs,jffe,                      &  ! fuel_frac_burnt dims
        fgip,fuel_frac_burnt,                     & !in
        grnhft,grnqft)                              !out
implicit none

!*** purpose        
! compute the heat fluxes on the fire grid cells

!*** arguments
type(fire_params), intent(in)::fp
real, intent(in)::dt          ! dt  the fire time step (the fire model advances time by this)
integer, intent(in)::ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,iffs,iffe,jffs,jffe   ! dimensions                   
real, intent(in),dimension(ifms:ifme,jfms:jfme):: fgip
real, intent(in),dimension(iffs:iffe,jffs:jffe):: fuel_frac_burnt
real, intent(out),dimension(ifms:ifme,jfms:jfme):: grnhft,grnqft

!*** local
integer::i,j
real:: dmass,bmst
logical::latent

!*** executable        
do j=jfts,jfte
    do i=ifts,ifte
         dmass =                     &     ! surface fuel dry mass burnt this call (kg/m^2)
             fgip(i,j)               &     ! init mass from fuel model no (kg/m^2) = fgi(nfuel_cat(i,j)
             * fuel_frac_burnt(i,j)        ! fraction burned this call    (1)
         bmst     = fp%fmc_g(i,j)/(1.+fp%fmc_g(i,j))        ! bmst varies by (i,j)
         grnhft(i,j) = (dmass/dt)*(1.-bmst)*cmbcnst         ! surface fire sensible heat flux W/m^2
         grnqft(i,j) = (bmst+(1.-bmst)*.56)*(dmass/dt)*XLV  ! surface fire latent heat flux W/m
         ! xlv is defined in module_model_constants.. Assume 56% of cellulose molecule mass is water.
    enddo
enddo

end subroutine heat_fluxes

!
!**********************
!            

  subroutine fire_ros(ros_base,ros_wind,ros_slope, &
    nvx,nvy,i,j,fp,fire_advection)
  
    implicit none
  
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !     ... calculates fire spread rate with McArthur formula or Rothermel
  !           using fuel type of fuel cell
  !      
  !         m/s =(ft/min) *.3048/60. =(ft/min) * .00508   ! conversion rate
  !         ft/min = m/s * 2.2369 * 88. = m/s *  196.850 ! conversion rate
  !      
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  
    !*** arguments
    real, intent(out) :: ros_base, ros_wind, ros_slope ! rate of spread contribution due to fuel, wind, and slope
    real, intent(in) :: nvx, nvy
    integer, intent(in) :: i,j         ! node mesh coordinates
    integer, intent(in) :: fire_advection
    type(fire_params), intent(in) :: fp
    
    !*** local
    real :: speed, tanphi ! windspeed and slope in the direction normal to the fireline
    real :: umid, phis, phiw, spdms, umidm, excess
    real :: ros_back
    real, parameter::ros_max=6.
    real ::cor_wind, cor_slope
  
  
    !*** executable
  
    if (fire_advection.ne.0) then ! from flags in module_fr_fire_util
      ! wind speed is total speed 
      speed = sqrt(fp%vx(i,j) * fp%vx(i,j) + fp%vy(i,j) * fp%vy(i,j)) +tiny(speed)
      ! slope is total slope
      tanphi = sqrt(fp%dzdxf(i,j) * fp%dzdxf(i,j) + fp%dzdyf(i,j) * fp%dzdyf(i,j)) + tiny(tanphi)
      ! cos of wind and spread, if >0
      cor_wind =  max(0., (fp%vx(i,j) * nvx + fp%vy(i,j) * nvy) / speed)
      ! cos of slope and spread, if >0
      cor_slope = max(0., (fp%dzdxf(i,j) * nvx + fp%dzdyf(i,j) * nvy) / tanphi)
    else
      ! wind speed in spread direction
      speed = fp%vx(i,j) * nvx + fp%vy(i,j) * nvy
      ! slope in spread direction
      tanphi = fp%dzdxf(i,j) * nvx + fp%dzdyf(i,j) * nvy
      cor_wind = 1.
      cor_slope = 1.
    endif
  
    if (.not. fp%ischap(i,j) > 0.) then      
      ! Fuel is not chaparral, calculate rate of spread using Rothermel formula
      ! ... if wind is 0 or into fireline, phiw = 0, &this reduces to backing ros.
      spdms = max(speed,0.)            ! 
      umidm = min(spdms,30.)           ! max input wind spd is 30 m/s   !param!
      umid = umidm * 196.850           ! m/s to ft/min
      !  eqn.: phiw = c * umid**bbb(i,j) * (fp%betafl(i,j)/betaop)**(-e) ! wind coef
      phiw = umid**fp%bbb(i,j) * fp%phiwc(i,j) ! wind coef
      phis=0.
      if (tanphi .gt. 0.) then
      phis = 5.275 *(fp%betafl(i,j))**(-0.3) *tanphi**2   ! slope factor
      endif
      ! rosm = fp%r_0(i,j)*(1. + phiw + phis)  * .00508 ! spread rate, m/s
      ros_base = fp%r_0(i,j) * .00508
      ros_wind = ros_base*phiw
      ros_slope= ros_base*phis!
    else 
      ! Chaparral fuel, spread rate only depends on windspeed, not fuel character
      spdms = max(speed,0.)      
      ! rosm = 1.2974 * spdms**1.41       ! spread rate, m/s
      ! note: backing ros is 0 for chaparral without setting nozero value below
      ! sp_n=.03333  
      ! chaparral backing fire spread rate 0.033 m/s   ! param!
      ! rosm= max(rosm, sp_n)   ! no less than backing r.o.s.
  
      ros_back=.03333    ! chaparral backing fire spread rate 0.033 m/s   ! param!
      ros_wind = 1.2974 * spdms**1.41       ! spread rate, m/s
      ros_wind = max(ros_wind, ros_back)
      ros_slope =0.
    endif
  
    ! if advection, multiply by the cosines
    ros_wind = ros_wind * cor_wind
    ros_slope = ros_slope * cor_slope
  
    !     ----------note!  put an 6 m/s cap on max spread rate -----------
    excess = ros_base + ros_wind + ros_slope - ros_max
  
    if (excess > 0.)then
      ! take it out of wind and slope in proportion
      ros_wind = ros_wind - excess * ros_wind / (ros_wind + ros_slope)
      ros_slope = ros_slope - excess * ros_slope/ (ros_wind + ros_slope)
    endif
  end subroutine fire_ros 

end module module_fr_fire_phys
