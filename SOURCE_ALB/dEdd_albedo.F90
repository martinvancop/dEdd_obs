!=====================================================================================================================
!
!     dEdd_obs.F     
!
!     Test program for Delta-Eddington using observations
!
!     M. Vancoppenolle, M. Lebrun, LOCEAN-CNRS, Paris, July 2020
!
!======================================================================================================================
!
!======================================================================================================================
!     
      PROGRAM dEdd_obs

      USE icepack_shortwave
      USE icepack_parameters

      IMPLICIT NONE

!======================================================================================================================
!
      INTEGER (kind=4) :: &
         N_hi   , &  ! number of ice thickness observations
         N_hs        ! number of snow depth observations

      INTEGER (kind=4) :: &
         ncat   , & ! number of ice thickness categories
         nilyr  , & ! number of ice layers
         nslyr      ! number of snow layers

      INTEGER (kind=4) :: &
         i_sta  , & ! loop index
         i      , & ! loop index
         j      , & ! loop index
         k          ! loop index
         
      REAL (kind=8)    ::  &
         zhi_max  , &
         zhs_max  , &
         zdhi     , &
         zdhs   

!     LOGICAL (kind=4) :: &
!         heat_capacity, & ! if true, ice has nonzero heat capacity
!         dEdd_algae,    & ! .true. use prognostic chla in dEdd
!         modal_aero       ! .true. use modal aerosol treatment

      ! dEdd tuning parameters, set in namelist
!     REAL  (kind=8) :: &
!         dT_mlt, & ! change in temp for non-melt to melt snow grain radius change (C)
!         hs0      , & ! snow depth for transition to bare sea ice (m)
!         pndaspect, & ! ratio of pond depth to pond fraction
!         hs1      , & ! tapering parameter for snow on pond ice
!         hp1      , & ! critical parameter for pond ice thickness
!         kalg         ! algae absorption coefficient

      REAL  (kind=8), DIMENSION(1,1)                 :: & 
          kaer_tab, & ! aerosol mass extinction cross section (m2/kg)
          waer_tab, & ! aerosol single scatter albedo (fraction)
          gaer_tab    ! aerosol asymmetry parameter (cos(theta))

      REAL  (kind=8), DIMENSION(1,1)                 :: & ! Modal aerosol treatment
          kaer_bc_tab, & ! aerosol mass extinction cross section (m2/kg)
          waer_bc_tab, & ! aerosol single scatter albedo (fraction)
          gaer_bc_tab    ! aerosol asymmetry parameter (cos(theta))

      REAL  (kind=8), DIMENSION(1,1,1) :: & ! Modal aerosol treatment
          bcenh          ! BC absorption enhancement factor

      CHARACTER  (len=80) :: &
          calendar_type       ! differentiates Gregorian from other calendars

      INTEGER  (kind=4) :: & 
          days_per_year, &    ! number of days in one year
          sec                 ! elapsed seconds into date
          
      INTEGER (kind=4) ::  &
          ji           ,   &
          jj

      REAL  (kind=8) :: &
          nextsw_cday     , & ! julian day of next shortwave calculation
          yday                ! day of the year

      REAL (kind=8) :: &
          dt,    & ! time step (s)
          tlat,  & ! latitude of temp pts (radians)
          tlon,  & ! longitude of temp pts (radians)
          fsw ,  & ! observed shortwave flux (W/m^2)
          swvdr, & ! sw down, visible, direct  (W/m^2)
          swvdf, & ! sw down, visible, diffuse (W/m^2)
          swidr, & ! sw down, near IR, direct  (W/m^2)
          swidf, & ! sw down, near IR, diffuse (W/m^2)
          fsnow    ! snowfall rate (kg/m^2 s)

      REAL (kind=8), ALLOCATABLE, DIMENSION(:) :: &
          aicen, & ! concentration of ice
          vicen, & ! volume per unit area of ice (m)
          vsnon, & ! volume per unit area of snow (m)
          Tsfcn, & ! surface temperature (deg C)
          alvln, & ! level-ice area fraction
          apndn, & ! pond area fraction
          hpndn, & ! pond depth (m)
          ipndn    ! pond refrozen lid thickness (m)

      REAL (kind=8), DIMENSION(1,1) :: &
          aeron, & ! aerosols (kg/m^3)
          trcrn_bgcsw ! zaerosols (kg/m^3) + chlorophyll on shorthwave grid
 
      REAL (kind=8), ALLOCATABLE, DIMENSION(:)  :: &
            ffracn,    &  ! fraction of fsurfn used to melt ipond
            dhsn          ! depth difference for snow on sea ice and pond ice

      REAL (kind=8) :: &
            coszen        ! cosine solar zenith angle, < 0 for sun below horizon 

      REAL (kind=8), ALLOCATABLE, DIMENSION(:) :: &
            alvdrn,   & ! visible direct albedo (fraction)
            alvdfn,   & ! near-ir direct albedo (fraction)
            alidrn,   & ! visible diffuse albedo (fraction)
            alidfn,   & ! near-ir diffuse albedo (fraction)
            fswsfcn,  & ! SW absorbed at ice/snow surface (W m-2)
            fswintn,  & ! SW absorbed in ice interior, below surface (W m-2)
            fswthrun, & ! SW through ice to ocean (W/m^2) 
            albicen,  & ! albedo bare ice 
            albsnon,  & ! albedo snow 
            albpndn,  & ! albedo pond 
            apeffn,   & ! effective pond area used for radiation calculation
            snowfracn   ! snow fraction on each category used for radiation

      REAL (kind=8), ALLOCATABLE, DIMENSION(:) :: &
            fswthrun_vdr, & ! vis dir SW through ice to ocean (W/m^2) 
            fswthrun_vdf, & ! vis dif SW through ice to ocean (W/m^2) 
            fswthrun_idr, & ! nir dir SW through ice to ocean (W/m^2) 
            fswthrun_idf    ! nir dif SW through ice to ocean (W/m^2) 

      REAL (kind=8), ALLOCATABLE, DIMENSION(:,:) :: &
            Sswabsn , & ! SW radiation absorbed in snow layers (W m-2)
            Iswabsn , & ! SW radiation absorbed in ice layers (W m-2) 
            fswpenln    ! visible SW entering ice layers (W m-2)

      LOGICAL  (kind=4) :: &
            l_print_point
 
      LOGICAL  (kind=4) :: &
            initonly    ! flag to indicate init only, default is false

      ! CCSM3 variables
      REAL (kind=8), ALLOCATABLE, DIMENSION(:) :: &
            albin,   &   ! ccsm3 variable
            albsn,   &   ! ccsm3 variable
            fswsfc,  &   ! ccsm3 variable
            fswthru, &   ! ccsm3 variable
            fswthru_vdr, &! ccsm3 variable
            fswthru_vdf, &! ccsm3 variable
            fswthru_idr, &! ccsm3 variable
            fswthru_idf, &! ccsm3 variable
            fswint       ! ccsm3 variable

      ! CCSM3 variables
      REAL (kind=8), ALLOCATABLE, DIMENSION(:,:) :: &
            fswpenl, &   ! ccsm3 variable
            Iswabs , &   ! ccsm3 variable
            Sswabs       ! ccsm3 variable

      ! Dummy arrays
      REAL (kind=8)                :: zscalar
      REAL (kind=8), DIMENSION(10) :: zarray, zarray_out

      ! Input arrays from file
      REAL (kind=8) :: &
         jday, & ! julian day
         lat,  & ! latitude
         lon,  & ! longitude
         tair, & ! air temperature
         a_p , & ! pond fraction
         F_sw    ! downwelling solar radiation
         
      REAL(kind=8), ALLOCATABLE, DIMENSION(:,:) :: &
         h_i,  & ! ice thickness
         h_s     ! snow depth

!======================================================================================================================

      N_hi  = 3  ! number of ice thicknesses
      N_hs  = 3  ! number of snow depths
      zhi_max    = 2.      ! max ice thickness
      zhs_max    = 0.15    ! max snow depth

      ncat   = 1 ! number of ice thickness categories
      nilyr  = 5 ! number of ice layers
      nslyr  = 1 ! number of snow layers

      ALLOCATE( aicen(ncat) )
      ALLOCATE( vicen(ncat) )
      ALLOCATE( vsnon(ncat) )
      ALLOCATE( Tsfcn(ncat) )
      ALLOCATE( alvln(ncat) )
      ALLOCATE( apndn(ncat) )
      ALLOCATE( hpndn(ncat) )
      ALLOCATE( ipndn(ncat) )
      
      ALLOCATE( ffracn(ncat) )
      ALLOCATE( dhsn(ncat) )
      
      ALLOCATE( alvdrn(ncat) )
      ALLOCATE( alvdfn(ncat) )
      ALLOCATE( alidrn(ncat) )
      ALLOCATE( alidfn(ncat) )
      ALLOCATE( fswsfcn(ncat) )
      ALLOCATE( fswintn(ncat) )
      ALLOCATE( fswthrun(ncat) )
      ALLOCATE( albicen(ncat) )
      ALLOCATE( albsnon(ncat) )
      ALLOCATE( albpndn(ncat) )
      ALLOCATE( apeffn(ncat) )
      ALLOCATE( snowfracn(ncat) )

      ALLOCATE( fswthrun_vdr(ncat) )
      ALLOCATE( fswthrun_vdf(ncat) )
      ALLOCATE( fswthrun_idr(ncat) )
      ALLOCATE( fswthrun_idf(ncat) )
      ALLOCATE( Sswabsn(nilyr+1,ncat) )
      ALLOCATE( Iswabsn(nilyr+1,ncat) )
      ALLOCATE( fswpenln(nilyr+1,ncat) )

      ! Extra CCSM3 variables
      ALLOCATE( albin(ncat) )
      ALLOCATE( albsn(ncat) )
      ALLOCATE( fswsfc(ncat) )
      ALLOCATE( fswthru(ncat) )
      ALLOCATE( fswthru_vdr(ncat) )
      ALLOCATE( fswthru_vdf(ncat) )
      ALLOCATE( fswthru_idr(ncat) )
      ALLOCATE( fswthru_idf(ncat) )
      ALLOCATE( fswint(ncat) )

      ! Extra CCSM3 variables
      ALLOCATE( Sswabs(nilyr+1,ncat) )
      ALLOCATE( Iswabs(nilyr+1,ncat) )
      ALLOCATE( fswpenl(nilyr+1,ncat) )

      ALLOCATE( h_i (N_hi, N_hs) )
      ALLOCATE( h_s (N_hi, N_hs) )

      !----------------------------------------------------------------------------------------------------------------

      WRITE(*,*) ' Comparison challenge of DE and CCSM3 albedos'

      !------------------
      ! Other parameters 
      !------------------
      ! For these parameters, we used default CICE values

!     heat_capacity = .FALSE. ! if true, ice has nonzero heat capacity
!     dEdd_algae    = .FALSE. ! .true. use prognostic chla in dEdd ! CHANGE 
!     modal_aero    = .FALSE. ! .true. use modal aerosol treatment

      l_print_point = .FALSE.
      initonly      = .FALSE.

!     dT_mlt = 1.5d0          ! change in temp for non-melt to melt snow grain radius change (C)
!     hs0      = 0.03d0       ! snow depth for transition to bare sea ice (m)
!     pndaspect = 0.8d0       ! ratio of pond depth to pond fraction
!     hs1       = 0.03d0      ! tapering parameter for snow on pond ice
!     hp1       = 0.01d0      ! critical parameter for pond ice thickness
!     kalg      = 0.6d0       ! algae absorption coefficient

      kaer_tab  = 0.d0        ! OBSOLETE aerosol mass extinction cross section (m2/kg)
      waer_tab  = 0.d0        ! OBSOLETE aerosol single scatter albedo (fraction)
      gaer_tab  = 0.d0        ! OBSOLETE aerosol asymmetry parameter (cos(theta))

      kaer_bc_tab = 0.0d0     ! OBSOLETE aerosol mass extinction cross section (m2/kg)
      waer_bc_tab = 0.0d0     ! OBSOLETE aerosol single scatter albedo (fraction)
      gaer_bc_tab = 0.0d0     ! OBSOLETE aerosol asymmetry parameter (cos(theta))

      bcenh = 0.0d0           ! BC absorption enhancement factor

      calendar_type = "GREGORIAN" ! OBSOLETE calendar type

      days_per_year = 365      ! number of days in one year
      sec           = 86400./2 ! elapsed seconds into date
      ! MV we assume it is noon, as it is complicated to recheck the time
      ! as it probably does not affect the zenithal angle unless we are close to sunrise or sunset,
      ! which is unlikely in the context of our measurements

      dt = 3600.               ! time step (s)
      ! MV dt does not matter since it is only used to fix a patho case, irrelevant in the real world

      fsnow = 0.   ! snowfall rate (kg/m^2 s)
      ! MV fsnow does not matter since it is only used to fix a patho case, irrelevant in the real world

      aicen(1) = 1. ! 1 since we are 1D
      alvln(1) = 1.   ! level-ice area fraction
      ipndn(1) = 0.0  ! pond refrozen lid thickness (m)

      aeron(1,1) = 0.0 ! aerosols (kg/m^3)
      trcrn_bgcsw(1,1) = 0.0 ! zaerosols (kg/m^3) + chlorophyll on shorthwave grid

      ffracn(1) = 0. ! fraction of fsurfn used to melt ipond 
      ! MV ffracn does not matter since it is only used to fix a patho case, irrelevant in the real world
      dhsn(1) = 0.0 ! depth difference for snow on sea ice and pond ice
      ! MV dhsn does not matter since it is only used to fix a patho case, irrelevant in the real world
      apeffn(1)    = 0.   ! effective pond area used for radiation calculation
      snowfracn(1) = 0.   ! snow fraction on each category used for radiation

      !----------------------------------------------------------------------------------------------------------------
      ! Do loop starts here
      !----------------------------------------------------------------------------------------------------------------
      OPEN ( UNIT = 11, FILE ="dEdd_albedo.txt", ACTION = 'WRITE')

      WRITE(11,*) "  hi(m)  hs(m) alvdrn alidrn alvdfn alidfn "
      
      zdhi       = zhi_max / ( REAL(N_hi) - 1. )
      zdhs       = zhs_max / ( REAL(N_hs) - 1. )

      DO ji = 1, N_hi
         DO jj = 1, N_hs


           h_i(ji,jj) = zdhi * REAL(ji-1)
           h_s(ji,jj) = zdhs * REAL(jj-1)
!          READ(10,*) zarray
!          WRITE(*,*) zarray
! 
!          jday(i_sta) = zarray(1)
!          lat(i_sta)  = zarray(2)
!          lon(i_sta)  = zarray(3)
!          tair(i_sta)  = zarray(4)
!          F_sw(i_sta)  = zarray(5)
!          h_i (i_sta)  = zarray(6)
!          h_s (i_sta)  = zarray(7)
!          a_p (i_sta)  = zarray(8)
! 
!          WRITE(*,*) ' jday : ', jday(i_sta)
!          WRITE(*,*) ' lat  : ', lat(i_sta)
!          WRITE(*,*) ' lon  : ', lon(i_sta)
!          WRITE(*,*) ' tair : ', tair(i_sta)
!          WRITE(*,*) ' F_sw : ', F_sw(i_sta)
!          WRITE(*,*) ' h_i  : ', h_i (i_sta)
!          WRITE(*,*) ' h_s  : ', h_s (i_sta)
!          WRITE(*,*) ' a_p  : ', a_p (i_sta)

         !--------------------------
         ! Station-dependent arrays
         !--------------------------
         yday = 150               ! take June 1st as default
         nextsw_cday   = yday     ! julian day of next shortwave calculation 
         ! MV we assume it is the same as we do know what to do, since this sounds like
         ! nextsw_cday should be time-step dependent, which is a bit absurd in the real world
   
         tlat = 80. * 3.14159267 / 180.  ! dummy latitude
         tlon = 0.  * 3.14159267 / 180.  ! dummy longitude
         fsw  = 150.                     ! dummy Fsw in
   
         ! split shortwave incident radiation into its components, assuming same fractions
         ! as in CICE standalone configuration
         swvdr = fsw * 0.28              ! sw down, visible, direct  (W/m^2)
         swvdf = fsw * 0.24              ! sw down, visible, diffuse (W/m^2)
         swidr = fsw * 0.31              ! sw down, near IR, direct  (W/m^2)
         swidf = fsw * 0.17              ! sw down, near IR, diffuse (W/m^2)
         
         ! MV test
!        swvdr = fsw * 0.10              ! sw down, visible, direct  (W/m^2)
!        swvdf = fsw * 0.40              ! sw down, visible, diffuse (W/m^2)
!        swidr = fsw * 0.10              ! sw down, near IR, direct  (W/m^2)
!        swidf = fsw * 0.40              ! sw down, near IR, diffuse (W/m^2)

         ! Surface temperature might be important
         Tsfcn(1) = -10.      ! surface temperature (deg C) - equal to air temperature
            
         vicen(1) = h_i(i,j)  ! volume per unit area of ice (m) - equal to ice thickness
         vsnon(1) = h_s(i,j)  ! volume per unit area of snow (m) - equal to snow depth

         apndn(1) = 0.        ! pond area fraction
         hpndn(1) = 0.        ! pond depth (m)
   
         !---------------------
         ! Set outputs to zero
         !---------------------
    
         coszen = 0.  ! cosine solar zenith angle, < 0 for sun below horizon 
         alvdrn(1) = 0.  ! visible direct albedo (fraction)
         alvdfn(1) = 0.  ! near-ir direct albedo (fraction)
         alidrn(1) = 0.  ! visible diffuse albedo (fraction)
         alidfn(1) = 0.  ! near-ir diffuse albedo (fraction)
         fswsfcn(1) = 0. ! SW absorbed at ice/snow surface (W m-2)
         fswintn(1) = 0. ! SW absorbed in ice interior, below surface (W m-2)
         fswthrun(1) = 0. ! SW through ice to ocean (W/m^2) 
         albicen(1) = 0.  ! albedo bare ice 
         albsnon(1) = 0.  ! albedo snow 
         albpndn(1) = 0.  ! albedo pond 
   
         fswthrun_vdr(1) = 0. ! vis dir SW through ice to ocean (W/m^2) 
         fswthrun_vdf(1) = 0. ! vis dif SW through ice to ocean (W/m^2) 
         fswthrun_idr(1) = 0. ! nir dir SW through ice to ocean (W/m^2) 
         fswthrun_idf(1) = 0. ! nir dif SW through ice to ocean (W/m^2) 
    
         Sswabsn (:,1)     = 0. ! SW radiation absorbed in snow layers (W m-2)
         Iswabsn (:,1)     = 0. ! SW radiation absorbed in ice layers (W m-2) 
         fswpenln(:,1)     = 0. ! visible SW entering ice layers (W m-2)
   
         !----------------------------------------------------------------------------------------------------------------
    
          WRITE(*,*) ' Delta-Eddington Scheme '

          CALL       run_dEdd(dt,       ncat,      &
                           dEdd_algae,          &
                           nilyr,    nslyr,     &
                           aicen,    vicen,     &
                           vsnon,    Tsfcn,     &
                           alvln,    apndn,     &
                           hpndn,    ipndn,     &
                           aeron,    kalg,      &
                           trcrn_bgcsw,         &
                           heat_capacity,       &
                           tlat,     tlon,      & 
                           calendar_type,       &
                           days_per_year,       &
                           nextsw_cday,   yday, &
                           sec,      R_ice,     &
                           R_pnd,    R_snw,     &
                           dT_mlt,   rsnw_mlt,  &
                           hs0,      hs1,  hp1, &
                           pndaspect,           &
                           kaer_tab, waer_tab,  &
                           gaer_tab,            &
                           kaer_bc_tab,         &
                           waer_bc_tab,         &
                           gaer_bc_tab,         &
                           bcenh,               &
                           modal_aero,          &
                           swvdr,    swvdf,     &
                           swidr,    swidf,     &
                           coszen,   fsnow,     &
                           alvdrn,   alvdfn,    &
                           alidrn,   alidfn,    &
                           fswsfcn,  fswintn,   &
                           fswthrun,            &
                           fswthrun_vdr,        &
                           fswthrun_vdf,        &
                           fswthrun_idr,        &
                           fswthrun_idf,        &
                           fswpenln,            &
                           Sswabsn,  Iswabsn,   &
                           albicen,  albsnon,   &
                           albpndn,  apeffn,    &
                           snowfracn,           &
                           dhsn,     ffracn,    &
                           l_print_point,       &
                           initonly)

!          WRITE(*,*) ' CCSM3 Scheme           '

!           CALL    shortwave_ccsm3 (aicen,    vicen,    & ! ok
!                                   vsnon,    Tsfcn,    & ! ok
!                                   swvdr,    swvdf,    & ! ok
!                                   swidr,    swidf,    & ! ok
!                                   heat_capacity,      & ! ok
!                                   albedo_type,        &
!                                   albicev,  albicei,  &
!                                   albsnowv, albsnowi, &
!                                   ahmax,              &
!                                   alvdrn,   alidrn,   & ! ok
!                                   alvdfn,   alidfn,   & ! ok
!                                   fswsfc,   fswint,   & 
!                                   fswthru,            &
!                                   fswthru_vdr,        &
!                                   fswthru_vdf,        &
!                                   fswthru_idr,        &
!                                   fswthru_idf,        &
!                                   fswpenl,            &
!                                   Iswabs,   SSwabs,   &
!                                   albin,    albsn,    &
!                                   coszen,   ncat,     & ! ok
!                                   nilyr)                ! ok

         !----------------------------------------------------------------------------------------------------------------
   
         
         zarray_out(:) = 0.d0
         zarray_out(1) = h_i(i,j)
         zarray_out(2) = h_s(i,j)
         zarray_out(3) = alvdrn(1)
         zarray_out(4) = alidrn(1)
         zarray_out(5) = alvdfn(1)
         zarray_out(6) = alidfn(1)

         WRITE(*,'(10F7.2)') zarray_out

      END DO
      END DO

      CLOSE(10) ! CLOSE file
      CLOSE(11) ! CLOSE file

      END PROGRAM
