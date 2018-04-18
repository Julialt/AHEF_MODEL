!=====================================================================
!     global.h
!=====================================================================
!  Definition of GLOBAL parameters and variables
!=====================================================================

      INTEGER,PARAMETER :: minyear = 1930
      INTEGER,PARAMETER :: maxyear =  2150
      INTEGER,PARAMETER :: maxlats =     7
!      INTEGER,PARAMETER :: maxcty  =  3143        ! entire US
      INTEGER,PARAMETER :: maxcty  =   524         ! REGION 1
      INTEGER,PARAMETER :: maxstate = 15           ! max states per region

! JMLT TODO : find numcty by reading?
!      INTEGER,PARAMETER :: numcty  =  3111        ! continental US
!      INTEGER,PARAMETER :: numcty  = 524           ! REGION 1

      INTEGER,PARAMETER :: numcty  = 32            ! REGION 7
      INTEGER,PARAMETER :: maxage  = 85            ! maximum age cohort
      INTEGER,PARAMETER :: step    = 5             ! step in cohort ages
      INTEGER,PARAMETER :: maxages = maxage/step+1 ! number of ages
      INTEGER,PARAMETER :: maxcohorts =    50
!      INTEGER,PARAMETER :: maxpops = 6             ! WM,WF,BM,BF,NWM,NWF
      INTEGER,PARAMETER :: maxpops = 4   ! WM,WF,BM,BF (treats Hispanic as WH)
!  (original note) : problem with baseline at 4

      INTEGER,PARAMETER :: maxeps  = 10            ! max end points
      INTEGER,PARAMETER :: lcols   = 21            ! lookup table columns
      INTEGER,PARAMETER :: lrows   = 51            ! lookup table rows
      INTEGER,PARAMETER :: topage  = 90            ! New def maxage
      INTEGER,PARAMETER :: mmo  = 12               ! months in a year

      INTEGER runcount
      INTEGER startyr, endyr, maxyr, numlats
      INTEGER iyear, imonth, iday, itime
      INTEGER ilat, icty
      INTEGER icohort, iage, ipop, iagey
      INTEGER mincohort,maxcohort

      LOGICAL emiflag, oznflag, expflag, effflag
      LOGICAL errflag, returned, eof
      LOGICAL expblflag

! for *.f
      CHARACTER(len=3),DIMENSION(mmo),PARAMETER :: monthname = 
     &                (/'Jan','Feb','Mar','Apr','May','Jun',    
     &                  'Jul','Aug','Sep','Oct','Nov','Dec'/)

! for  .f90
!      CHARACTER(len=3),DIMENSION(mmo),PARAMETER :: monthname =  &
!     &                (/'Jan','Feb','Mar','Apr','May','Jun',    &
!     &                  'Jul','Aug','Sep','Oct','Nov','Dec'/)

      CHARACTER(len=12) :: eminame, oznname, expname, expblname, effname
      CHARACTER(len=12) :: xageblname, xagename, effagename
      CHARACTER(len=12) :: atmrunname, exprunname, effrunname
      CHARACTER(len=12) :: drtype

      INTEGER,DIMENSION(maxcty) :: cty_fip  ! fip number for each US county
      REAL,DIMENSION(maxcty) :: cty_lat   ! associated lat for each US county

