C=====================================================================
C     global.h
C=====================================================================
C  Definition of GLOBAL parameters and variables
C=====================================================================

      INTEGER,PARAMETER :: minyear = 1930
      INTEGER,PARAMETER :: maxyear =  2150
      INTEGER,PARAMETER :: maxlats =     7       
!      INTEGER,PARAMETER :: maxcty  =  3111        ! continental US
      INTEGER,PARAMETER :: maxcty  =   524         ! REGION 1
! JMLT TODO : find numcty by reading?
!      INTEGER,PARAMETER :: numcty  =  3111        ! continental US
!      INTEGER,PARAMETER :: numcty  = 524           ! REGION 1
      INTEGER,PARAMETER :: numcty  = 32            ! REGION 7
      INTEGER,PARAMETER :: maxage  = 85            ! maximum age cohort
      INTEGER,PARAMETER :: step    = 5             ! step in cohort ages
      INTEGER,PARAMETER :: maxages = maxage/step+1 ! number of ages
      INTEGER,PARAMETER :: maxcohorts =    50
!      INTEGER,PARAMETER :: maxpops = 6             ! WM,WF,BM,BF,NWM,NWF
      INTEGER,PARAMETER :: maxpops = 4             ! WM,WF,BM,BF (treats Hispanic as WH)
c                                                     problem with baseline at 4
      INTEGER,PARAMETER :: maxeps  = 10            ! max end points
      INTEGER,PARAMETER :: lcols   = 21            ! lookup table columns
      INTEGER,PARAMETER :: lrows   = 51            ! lookup table rows
      INTEGER,PARAMETER :: topage  = 90            ! New def maxage

      INTEGER runcount
      INTEGER startyr, endyr, maxyr, numlats
      INTEGER iyear, imonth, iday, itime
      INTEGER ilat, icty
      INTEGER icohort, iage, ipop, iagey
      INTEGER mincohort,maxcohort

      LOGICAL emiflag, oznflag, expflag, effflag
      LOGICAL errflag, returned, eof
      LOGICAL expblflag

      CHARACTER(len=3) :: monthname(12)

      CHARACTER(len=12) :: eminame, oznname, expname, expblname, effname
      CHARACTER(len=12) :: xageblname, xagename, effagename
      CHARACTER(len=12) :: atmrunname, exprunname, effrunname
      CHARACTER(len=12) :: drtype

      INTEGER,DIMENSION(maxcty) :: cty_fip  ! fip number for each US county
      REAL,   DIMENSION(maxcty) :: cty_lat   ! associated lat for each US county
