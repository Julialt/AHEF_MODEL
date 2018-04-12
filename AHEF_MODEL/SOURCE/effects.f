!C=====================================================================
      SUBROUTINE effects
C=====================================================================
C   This subroutine contains the effects model.
C   Input:    exposure by cohort,age,latitude,measure
C   Output:   cases by year,measure
C   Modified to compute incidence for scenarios using BAFs and baseline
C   incidence
C=====================================================================
      IMPLICIT NONE

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'effects.fi'
      INCLUDE 'setup.h'
!---------------------------------------------------------------

      LOGICAL first, byyear, byage
      INTEGER count,yrlplo,yrlphi,coh1,coh2,iy,iageg
      INTEGER cohyrtmp,colotmp,cohitmp,ilattmp
      INTEGER rlat
      CHARACTER*12 age_pfn, cohort_pfn, coeff_pfn, popname
      CHARACTER*3  baftype
      CHARACTER*4  drtmp
      CHARACTER*1  chrpops
      CHARACTER*23  text
      CHARACTER*23  tempchar
      REAL coh_wght,tmp1,tmp2,wtd_incid, cmflag
      REAL expwgt(maxages*step)
      REAL expbase,expscen
      INTEGER year,iageall
      INTEGER,DIMENSION(maxpops) :: poptmp
      INTEGER i,ii,icomp,ic,iwrite,itmp,imatch,imatch2,ictymatch,tmp
c
      REAL::  t1,t2,tca,tcb

      REAL st_tot_base(15,maxpops)  ! by 15 state, 4 pop
      REAL st_tot_scen(15,maxpops)
      REAL st_tot_diff(15,maxpops)
      REAL st_coh_scen(15,4,maxpops)
      REAL st_coh_base(15,4,maxpops)
      REAL st_coh_diff(15,4,maxpops)
      REAL st_tot_b(15)  ! by 15 state, 4 pop
      REAL st_tot_s(15)
      REAL st_tot_d(15)
c 
!      REAL ct_tot_diff_l(numcty), ct_tot_diff_d(numcty)
      REAL ct_tot_diff_l(maxcty), ct_tot_diff_d(maxcty)
c
      INTEGER group1(15)
c      DATA group1 /1,5,12,22,28,29,40/
      DATA group1 /9,10,11,23,24,25,33,34,36,39,42,44,50,51,54/
      INTEGER ireg  ! total number of states for casest counter

      CHARACTER(10) adummy
      CHARACTER(20) bdummy
      CHARACTER(8) endpttmp
      CHARACTER(8) runname
      CHARACTER(1) c1,c2,c3,c4,c5,c6,c7,c8
!---------------------------------------------------------------

      WRITE (*,*) 'Running effects model . . . .'

! INITIALIZE SOME QUANTITIES

      ireg = 15
      count = 1
      total = 0
      numreg = 3
      first = .true.

      WRITE(chrpops,'(i1)')maxpops

cc      mrlm - debug
cc      WRITE(*,*)'debug ',expage(2,3,1)

      WRITE(*,*)'**************  lats(1) = ',lats(1)
c
! OPEN RESULTS FILES FROM EXPOSURE MODULE 
! IDEA ! case switches for which kind of input is available

! ... by cohort ...
! open exposure file *.EXP
!      OPEN(scratch,file=dir_io//expname)     
!      WRITE(*,*)'exposure file = ',expname

! open baseline exposure file *.XBL
!      OPEN(scratchbl,file=dir_io//expblname) 
!      WRITE(*,*)'baseline exposure file = ',expblname

! ... by age ...
! open age exposure file *.XSA
      OPEN(scratchage,file=dir_io//xagename)     
      WRITE(*,*)'age exposure file = ',xagename

! open baseline age exposure file *.XBA
      OPEN(scratchagebl,file=dir_io//xageblname) 
      WRITE(*,*)'baseline age exposure file = ',xageblname

! output file  for effective aggregate age is opened in
! write_eff_agg_age.f
!      WRITE(*,*)'effagename = ',effagename

! OPEN & READ EFFECTS RUN SETUP FILE

      OPEN(effrun,file=dir_in//effrunname,status='OLD',err=1080)
      WRITE (logfile,*) 'Reading effects runfile'
      WRITE (*,*) 'Reading effects runfile ',effrunname

      CALL skip(effrun, eof)
!      READ( effrun, 100 ) popname, outputlo, outputhi 
100   FORMAT(t19,a8,t39,i4,t47,i4)
      READ( effrun, 103 ) runname,popname, outputlo, outputhi 
103   FORMAT(t5,a8,t19,a8,t39,i4,t47,i4)

      CALL skip(effrun, eof)
      READ( effrun, 101 ) text,tempchar
      IF ((tempchar .EQ. 'Y') .OR. (tempchar .EQ. 'y') .OR.
     +    (tempchar .EQ. 'T') .OR. (tempchar .EQ. 't') .OR.
     +    (tempchar .EQ. 'X') .OR. (tempchar .EQ. 'x')) THEN
        byyear = .true.
      ELSE
        byyear = .false.
      ENDIF

      CALL skip(effrun, eof)
      READ( effrun, 101 ) text,tempchar
      IF ((tempchar .EQ. 'Y') .OR. (tempchar .EQ. 'y') .OR.
     +    (tempchar .EQ. 'T') .OR. (tempchar .EQ. 't') .OR.
     +    (tempchar .EQ. 'X') .OR. (tempchar .EQ. 'x')) THEN
        byage = .true.
      ELSE
        byage = .false.
      ENDIF

101   FORMAT(a23,a1)

!===============================================================
! READ POPULATION DISTRIBUTION
!===============================================================

!      WRITE(*,*)'Reading population...'

      CALL read_population(dir_pop//TRIM(runname)//'/'//popname)
      IF (errflag) GOTO 999

      !WRITE (*,*) 'Returned from read_pop...'

C**  Write population to an output file **
c
      WRITE(*,*)'numcty = ',numcty
c
      OPEN(o1unit, file=dir_io//'popout.txt')

      WRITE(o1unit,*)"year, # in each of "//chrpops//
     &               " population categories"

      DO year = outputlo, outputhi
        DO ipop = 1,maxpops
          poptmp(ipop)=0
          DO icty = 1,numcty
            DO iage=1,maxages
              poptmp(ipop) = poptmp(ipop) + pop(year,iage,icty,ipop)
c mlrm debug between codes - ---------
c            IF ((icty.EQ.1).AND.(ipop.EQ.1).AND.(year.EQ.2005)) THEN
c            WRITE(99,*)'year,iage,icty,ipop',year,iage,icty,ipop
c            WRITE(99,*)' pop = ',pop(year,iage,icty,ipop),'  ',poptmp
c            ENDIF
c--------------------------------------
            ENDDO ! icty
          ENDDO ! iage
        ENDDO ! ipop
        WRITE(o1unit,'(2x,i4,'//chrpops//'(2x,i12))') 
     &                     year,poptmp(1:maxpops)
      ENDDO ! year
c
cc      WRITE(*,*)'********pop', pop(2005,1,1,1)
c
120   FORMAT(t2,a5,t7,i2,t12,a5,t17,i2)
125   FORMAT(t2,i4,t8,i12,)
      CLOSE(o1unit)

      CALL skip(effrun, eof)

!===============================================================
! LOOP THROUGH DISEASE-TYPE SELECTIONS IN EFFRUN FILE
!===============================================================

      DO WHILE (.NOT. eof)
        READ( effrun, 200 ) endpoint,indname,age_pfn,cohort_pfn,
     &                      coeff_pfn, drtype
        WRITE(logfile, 200 ) endpoint,indname,age_pfn,cohort_pfn,
     &                       coeff_pfn,drtype
200     FORMAT(t5,a8,t19,a8,t33,a8,t47,a8,t61,a8,t75,a4)

        cohortname = cohort_pfn(1:len_trim(cohort_pfn))//'.COH'
        coeffname  = coeff_pfn(1:len_trim(coeff_pfn))//'.COE'
        agename  = age_pfn(1:len_trim(age_pfn))//'.AGE'


! JMLT: new
        DO i=1,2 ! loop projection, baseline

          WRITE(*,*) '..........................................'
          SELECT CASE (i)
            CASE (1)
              WRITE(*,*)'Reading projection exposure'
              expblflag=.FALSE.
              CALL read_exposure_age(indname)

            CASE (2)
              expblflag=.TRUE.
              WRITE (*,*) 'Reading baseline exposure'
              CALL read_exposure_age(indname)

          END SELECT

C=====================================================================
C  Calculate incidence using BAFs as a percentage change in
C  incidence for a given percent change in exposure
C=====================================================================
C  Read BAFs
C=====================================================================

        OPEN(iunit, FILE = dir_eff//'BAF.DAT')
        WRITE(*,*) "Reading file ", dir_eff//'BAF.DAT'
        CALL skip(iunit,eof)

        READ(iunit, 215) baftype
215     FORMAT(t26,a3)
        CALL skip(iunit,eof)

        WRITE(*,*) 'drtype   = ', drtype
        WRITE(*,*) 'endpoint = ', endpoint

        DO WHILE (.NOT. eof)
          READ (iunit, 220) endpttmp,drtmp,(baf(ipop), ipop=1,maxpops)
!220       FORMAT(t3,a12,t17,a4,t28,30(:,f6.4,4x))
220       FORMAT(t3,a8,t17,a4,t28,30(:,f6.4,4x))
          CALL skip(iunit,eof)

          WRITE(*,*) 'drtmp    = ', drtmp
          WRITE(*,*) 'endpttmp = ', endpttmp

          IF((endpttmp.EQ.endpoint).AND.(drtmp.EQ.drtype)) EXIT

        ENDDO ! WHILE loop for eof(iunit)

        CLOSE(iunit)

      ENDDO ! WHILE loop for eof(effrun)

      WRITE(*,*) ".........................................."
c
C=====================================================================
C  Read Exposure Weights 
C  JMLT TODO - REWRITE THIS TO ACCEPT INPUT FROM RUNFILES
C=====================================================================

      OPEN (iunit,file=dir_eff//'EXPWGTS.DAT')
      WRITE(*,*) "Reading file ",dir_eff//'EXPWGTS.DAT'
      CALL skip (iunit,eof)

      WRITE(*,*) 'endpoint = ', endpoint 

      DO WHILE (.NOT.eof)

        endpttmp=" "

! JMLT ! 
! Length of read-in field and # of leading blanks is unknown.
! This format finds the end-of-line character so we can parse the input.
        READ(iunit,'(Q,A)') i,bdummy
        CALL skip (iunit,eof)

        adummy=ADJUSTL(bdummy(1:i-1))
        i=INDEX(adummy," ")
        endpttmp(1:i-1) = adummy(1:i-1)


        DO iagey=1,maxages*step
          READ (iunit,250) expwgt(iagey)
        ENDDO ! iagey

250     FORMAT (t8,f4.2)
        CALL skip (iunit,eof)

        WRITE(*,*) 'endpttmp = ', (endpttmp) 

        IF(endpttmp.EQ.endpoint) EXIT

      ENDDO !  WHILE loop for eof(iunit)

      CLOSE (iunit)

      WRITE(*,*) ".........................................."

C=====================================================================
C  Read Baseline Incidence
C=====================================================================

      OPEN(iunit, FILE = dir_eff//'BASEINC.TXT')
      WRITE(*,*) "Reading file ", dir_eff//'BASEINC.TXT'
      CALL skip(iunit,eof)

      READ(iunit, 325) colotmp, cohitmp
325   FORMAT(t47,i4,t55,i4)
      CALL skip(iunit,eof)

      WRITE(*,*) 'endpoint = ', endpoint 

      DO WHILE (.NOT.eof)

        DO ilat = 1, numlats
          DO ipop = 1, maxpops

            endpttmp=" "
            popseg=" "

! read in string with unknown length, leading blanks
            READ(iunit,'(Q,A)') i,bdummy

            bdummy=ADJUSTL(bdummy(1:i-1))

            i=INDEX(bdummy," ")

            endpttmp(1:i-1) = bdummy(1:i-1)

            WRITE(*,*) 'endptmp = ', endpttmp 

            READ(iunit,332) ilattmp
332         FORMAT(t5,i1)
            WRITE(*,*) ilattmp

! read in string with unknown length, one integral blank
            READ(iunit,'(Q,A)') i,bdummy

            bdummy=ADJUSTL(bdummy(1:i-1))

            i=INDEX(bdummy," ")
            ii=INDEX(bdummy(i+1:LEN(bdummy))," ")

            popseg(1:i+ii-1) = bdummy(1:i+ii-1)

            WRITE(*,*) popseg

            READ(iunit,336) adummy   ! Skip a line
336         FORMAT(t1,a3)

! JMLT TODO: write error lines

            IF (((popseg .EQ. 'WH MALE'   ).AND.(ipop .NE. 1)) .OR.
     &          ((popseg .EQ. 'WH FEMALE' ).AND.(ipop .NE. 2)) .OR.
     &          ((popseg .EQ. 'NWH MALE'  ).AND.(ipop .NE. 3)) .OR.
     &          ((popseg .EQ. 'NWH FEMALE').AND.(ipop .NE. 4))) THEN
              WRITE(6,*)"popseg mis-defined for ipop ",ipop,":",popseg
              STOP
            ENDIF

! find first year to read
305         READ (iunit, 340) cohyrtmp
340         FORMAT(t1,i4)

            IF (cohyrtmp.LT.colo_year) GOTO 305

            BACKSPACE(iunit)

!... and read all years in target range
            DO icohort = colo, ((cohitmp-colo_year)/step)+1

C --------------------------------------
C  Debugging tool
c               WRITE (*,'(t1,a7,t9,i3,t13,a4,t18,i3)')
c    +                'cohort=', icohort, 'age=', iage
c               WRITE (*,'(t1,a4,t9,i3,t13,a4,t18,i3)')
c    +                'lat=', ilat, 'pop=', ipop
C --------------------------------------

              READ(iunit, 310)  (incid_bl(icohort,iage,ilat,ipop),
     +              iage=1, maxages)

310           FORMAT(t5,50(:,4x,e8.2))

            ENDDO ! icohort
          ENDDO ! ipop
        ENDDO ! ilat

        IF (endpttmp .EQ. endpoint) EXIT

      ENDDO ! WHILE loop for eof(iunit)

      CLOSE(iunit)
      WRITE(*,*) "............................"

C=====================================================================
C Print baseline and scenario incidence matrices and the difference between
C them.
C NOTE: This routine changed to print only for ipop=1 (whm), in order to
C       limit size of the AHEF.LOG file.
C=====================================================================

       WRITE (logfile, '(A)') '==========================='
       WRITE (logfile,*) endpttmp
       WRITE (logfile, '(A)') 'Baseline incidence matrices'

        DO ilat = 1,numlats
          DO ipop = 1,1  ! (Just whm, use maxpops if all ipops desired)
            DO icohort = colo, cohi

!               WRITE (logfile, 500)
!     &               "lat=",ilat,"pop=",ipop,"year=",icohort
!               WRITE (logfile, 502) (incid_bl(icohort,iage,ilat,ipop),
!     +                       iage = 1, maxages)

               WRITE (logfile, 503)
     &               "lat=",ilat,"pop=",ipop,"year=",icohort,
     &               (incid_bl(icohort,iage,ilat,ipop),iage=1,maxages)

            ENDDO ! icohort
          ENDDO ! ipop
        ENDDO ! ilat

500    FORMAT (t1,a4,t5,i3,t9,a4,t14,i3,t20,a5,t26,i4)
502    FORMAT(t5,1p,50(:,4x,e8.2))
503    FORMAT (a4,i3,1x,a4,1x,i3,1x,a5,1x,i4,20(1x,e8.2))

C==========================================================================
C  Calculate cases using exposure by age information
C==========================================================================

C=============================================
C  Make incidence baseline matrix by age
C=============================================
c      mrlm
c      WRITE(*,*)'numlats = ',numlats
c      WRITE(*,*)'maxpops = ',maxpops
c      WRITE(*,*)'maxages = ',maxages
c      WRITE(*,*)'poplo = ',poplo
c      WRITE(*,*)'pophi = ',pophi
c      WRITE(*,*)'step = ',step
c

        DO ilat = 1,numlats
          DO ipop = 1,maxpops
            DO icohort = colo,cohi
              DO iage = 1,maxages
                DO iy = 1,5

                  iageall = (iage - 1) * step + iy

                  incage_bl(icohort,iageall,ilat,ipop) =
     +              incid_bl(icohort,iage,ilat,ipop)

                ENDDO ! iy
              ENDDO ! iage
            ENDDO ! icohort
          ENDDO ! ipop
        ENDDO ! ilat

C===========================================================
C  Write incidence by age to the logfile (for lat=1, pop=1)
C==========================================================

        WRITE(logfile,*)'================================='
        WRITE(logfile,*)'Incidence by age for lat=1, pop=1'

        DO icohort = colo,cohi

          WRITE (logfile,601) icohort, 
     &          (incage_bl(icohort,iagey,2,1),iagey=1,maxages*step)

        ENDDO ! icohort

601     FORMAT (i4,t8,100(1x,e8.2))

! JMLT DONE TO HERE ! GETTING REAL VALUES OUT !
C==================================================================
C  Get cases by cohort and cohort time using age-specific exposures
C  NOTE this is by each age for each of the 5 ages in a cohort
C  NOTE uses real weights by age for cumulative exposures to date
C==================================================================

        cmflag = 0
        IF ((drtype.EQ."CMYR") .OR. (drtype.EQ."CMPK")) cmflag = 1

        caseca=0
        casecab=0
c
        DO icty = 1,numcty
          DO ipop = 1,maxpops
            DO icohort = colo,cohi
             DO iy = 0,4

                expbase = 0.0
                expscen = 0.0
cc
c                  i get the iagey, i would think iy loop comes into play
c                   IF iagey = 1,maxages (without the step) but the iy represent
c                   the stepping through <-- question - incid_bl which is by 5 years
c                   however, here, is incage_bl which is by each year...
c
                DO iagey = 1, maxages * step
c
c                   so this is for going through the 5 years around the icohort*step 
c                   so colo_year = 1890, icohort = 1:43, step = 5, iagey = 1,90, iy=5 steps
c                   maxages = 18

                  iyear = colo_year + (icohort-1)*step - int(step/2) 
     &                  + iagey - 1 + iy

                  itmp = iagey + iy

cc                  mrlm comment - expage(icohort,itmp,ilat) - how much exposure a person gets
c                  living at that location given when you are born and how old you are now
c                   - i.e. ilat = 1 (so you live at 50 degrees North)
c                     itmp = 50 so you are 50 years old
c                     icohort = 20 so you were born in 1890+20*5 = 1990
c                     so this is equivalent to 2040 (born in 1990 + 50 years old)
c---------
c                  so this next part THEN for a given latitude, type of population, and
c                  for each birth year (icohort), it THEN sums up the lifetime of exposure
c                  weighted by the amount of exposure respective to that age (i.e. younger
c                  ages tend to get more exposure than the older ages)
c                   = ends up with total lifetime exposure for a person born in icohort year
c
                  expbase = expbase*cmflag 
     &                    + expagebl(icohort,itmp,icty) * expwgt(iagey)

                  expscen = expscen*cmflag 
     &                    + expage(icohort,itmp,icty) * expwgt(iagey)

c mrlm
c      IF (((icohort.EQ.20).OR.(icohort.EQ.40)).AND.
c     +      ((iagey.EQ.20).OR.(iagey.EQ.40))) THEN
c            WRITE(91,*)'icty,ipop,icohort,iy,iagey = ',icty,ipop,icohort,iy,
c     +            iagey
c            WRITE(91,*)'exbl,exage,expwgt,cmflag=',
c     +            expagebl(icohort,itmp,icty),expage(icohort,itmp,icty),
c     +        expwgt(iagey),cmflag
c      ENDIF
c
                  iage = int(iagey/step) + 1

                  tmp = mod(iagey,step)

                  IF (tmp .EQ. 0) iage = iage - 1

cc-----------------------
cNEW - determine latitude to use
                   DO imatch = 1,numcty
                     ictymatch = cty(imatch)
                     DO imatch2 = 1,numcty
                       IF (ictymatch.EQ.(cty_fip(imatch2))) THEN
                         rlat = aint(cty_lat(imatch2))
                         GOTO 801
                       ENDIF
                    ENDDO
                  ENDDO
c
 801              IF ((rlat.ge.20).AND.(rlat.le.30)) THEN
                    ilat = 3
                  ELSEIF ((rlat.ge.30).AND.(rlat.le.40)) THEN
                    ilat = 2
                  ELSEIF ((rlat.ge.40).AND.(rlat.le.50)) THEN
                    ilat = 1
                  ENDIF        
c
                  casecab(icohort, itmp, icty, ipop) =
     +            casecab(icohort, itmp, icty, ipop) +
     +            incage_bl(icohort, iagey, ilat, ipop) *
     +            pop(min(max(iyear, poplo), pophi), iage, icty, ipop)
     +            / (step*100000)
cc 
                  IF(baftype .EQ. 'PWR') THEN

                    caseca(icohort, itmp, icty, ipop) =
     +              caseca(icohort, itmp, icty, ipop) +
     +              incage_bl(icohort, iagey, ilat, ipop) * 
     +              (1 + (BAF(ipop)*(expscen-expbase)/expbase) ) *
     +              pop(min(max(iyear, poplo), pophi), iage, icty, ipop)
     +              / (step*100000)

                  ELSE IF(baftype .EQ. 'EXP') THEN

                    caseca(icohort, itmp, icty, ipop) =
     +              caseca(icohort, itmp, icty, ipop) +
     +              incage_bl(icohort, iagey, ilat, ipop) * 
     +              (1 + (BAF(ipop)*(expscen-expbase)) ) *
     +              pop(min(max(iyear, poplo), pophi), iage, icty, ipop)
     +              / (step*100000)

                  ENDIF

                ENDDO ! iagey
              ENDDO ! iy
            ENDDO ! icohort
          ENDDO ! ipop
        ENDDO ! icty
C----------------------------------------------------------------------
c  MRLM for testing print out to make graphs of a 10 year old thread
c-----------
c      OPEN(21,file=dir_io//"pop_10.txt",status="unknown")
c      OPEN(22,file=dir_io//"scbs_exp_10.txt",status="unknown")
c      OPEN(24,file=dir_io//"bsince_10.txt",status="unknown")
c      OPEN(25,file=dir_io//"case_10.txt",status="unknown")
c
c      icty = 1
c      ilat = 2
c      ipop = 1
c
c      WRITE(21,*)endpttmp
c      WRITE(22,*)endpttmp
c      WRITE(24,*)endpttmp
c      WRITE(25,*)endpttmp
c
cc      WRITE(*,*)'IS IT 5-32?',incage_bl(1,56,2,1)
c
c      DO icohort = colo,cohi
c
c      iyear = colo_year + (icohort-1)*step - int(step/2) + 10 - 1
c      iyear = colo_year + (icohort-1)*step 
cc      WRITE(*,*)'icohort, iyear =',icohort,iyear
c
c        WRITE(21,111)colo_year+(icohort-1)*step,
c     +        pop(min(max(iyear, poplo), pophi),3, 
c     +        icty, ipop)
c       WRITE(22,121)colo_year+(icohort-1)*step,
c     +          expagebl(icohort,10,icty),
c     +      expage(icohort,10,icty)
c       WRITE(24,*)colo_year+(icohort-1)*step,
c     +       incage_bl(icohort,10,ilat,ipop),
c     +     icohort,ilat,ipop
c       WRITE(25,141)colo_year+(icohort-1)*step,
c     +        casecab(icohort,10,icty,ipop),
c     +        caseca(icohort,10,icty,ipop)
c      ENDDO
c
c111      FORMAT(I4,2x,I10)
c121      FORMAT(I4,2x,2(f10.4,2x))
c131      FORMAT(I4,2x,f10.7)
c141      FORMAT(I4,2x,2(f10.7,2x))
C
C=====================================================================
C  Writes Scenario Inc/Mort by cohort, ilat, and ipop over cohort time
C=====================================================================

      IF (first) THEN
        OPEN(ounit, file=dir_out//'eachpop_state.txt')
        OPEN(ounit21, file=dir_out//'sumpop_state.txt')
      ELSE
        OPEN(ounit, file=dir_out//'eachpop_state.txt', status = 'OLD',
     +     access = 'APPEND')
        OPEN(ounit21, file=dir_out//'sumpop_state.txt',status='OlD',
     +      access='APPEND')
      ENDIF
c
      WRITE(ounit, 700) 'Measure:   >',endpttmp,'<'
      WRITE(ounit, 700) 'D-RType:   >',drtmp,'<'

      WRITE(ounit21, 700) 'Measure:   >',endpttmp,'<'
      WRITE(ounit21, 700) 'D-RType:   >',drtmp,'<'

! initialize
      st_tot_scen(:,:)=0.0
      st_tot_base(:,:)=0.0
      st_tot_diff(:,:)=0.0
      st_tot_s(:)=0.0
      st_tot_b(:)=0.0
      st_tot_d(:)=0.0

      DO ipop = 1,maxpops
        DO icty = 1,numcty

          icomp = int(cty_fip(icty)/1000)
c
          DO ii = 1,ireg
            IF (icomp.EQ.group1(ii)) THEN
c             icg1=icg1 + 1

              DO icohort = colo,cohi
                DO iagey = 1,maxages*step+4

                  st_tot_scen(ii,ipop)= st_tot_scen(ii,ipop)
     &                                + caseca(icohort,iagey,icty,ipop) 
                  st_tot_base(ii,ipop)= st_tot_base(ii,ipop)
     &                                + casecab(icohort,iagey,icty,ipop)
                  st_tot_diff(ii,ipop)= st_tot_diff(ii,ipop) 
     &                                + caseca(icohort,iagey,icty,ipop)
     &                                - casecab(icohort,iagey,icty,ipop)
                ENDDO ! iagey
              ENDDO ! icohort

            ENDIF
          ENDDO ! ii
        ENDDO ! icty
      ENDDO ! ipop

c
c sum for each population over all state(/regions)
c      WRITE(*,*)' summing up for state: total counties = ',icg1
c             
      DO ipop=1,maxpops
         WRITE(ounit,*)'Population =',ipop
         DO ii=1,ireg
           WRITE(ounit,*)group1(ii),st_tot_base(ii,ipop),
     +                   st_tot_scen(ii,ipop),st_tot_diff(ii,ipop)
         ENDDO ! ii
      ENDDO !ipop

      CLOSE(ounit)
c
c sum for each state(/region) over all populations
c           WRITE(ounit21,*)'Population =',ipop

      DO ii=1,ireg
        DO ipop=1,maxpops
            st_tot_b(ii) = st_tot_base(ii,ipop)+st_tot_b(ii)
            st_tot_s(ii) = st_tot_scen(ii,ipop)+st_tot_s(ii)
            st_tot_d(ii) = st_tot_diff(ii,ipop)+st_tot_d(ii)
        ENDDO ! ipop

        WRITE(ounit21,*)group1(ii),st_tot_b(ii),
     +                             st_tot_s(ii),
     +                             st_tot_d(ii)
      ENDDO ! ii

      CLOSE(ounit21)

c
c---------------------------------------------------------------------
c  WRITE out by population by state summed up for cohort groups
c      1890-1980; 1985-2010; 2015-2050; 2055-2100
c
      IF (first) THEN
        OPEN(ounit31, file=dir_out//'eachpop_state_cohort.txt',
     &                status = 'NEW')
      ELSE
        OPEN(ounit31, file=dir_out//'eachpop_state_cohort.txt', 
     &                status = 'OLD',
     &                access = 'APPEND')
      ENDIF
c
      WRITE(ounit, 700) 'Measure:   >',endpttmp,'<'
      WRITE(ounit, 700) 'D-RType:   >',drtmp,'<'
c      
      DO ii = 1,ireg
        DO ic = 1,4
          DO ipop = 1,maxpops
                 st_coh_scen(ii,ic,ipop)=0.0
            ENDDO
        ENDDO
      ENDDO
c
      DO ipop=1, maxpops
        DO icty = 1,numcty

          icomp = int(cty_fip(icty)/1000)

          DO ii=1, ireg
            IF (icomp.EQ.group1(ii)) THEN
c              icg1=icg1 + 1
              DO icohort = colo,cohi
c
                iyear = 1890+ (icohort-1)*5
                IF (iyear.le.1980) THEN   ! 1890 - 1980
                  ic=1 
                ELSEIF (iyear.le.2010) THEN    ! 1985-2010
                  ic =2
                ELSEIF (iyear.le.2050) THEN    ! 2015-2050
                  ic = 3
                ELSEIF (iyear.le.2100) THEN    ! 2055-2100
                  ic = 4
                ELSE
                  WRITE(*,*)' ERROR in WRITING OUT line 704'
                ENDIF

                DO iagey=1,maxages*step +4

                  st_coh_scen(ii,ic,ipop)= 
     &            st_coh_scen(ii,ic,ipop)+
     &            caseca(icohort,iagey,icty,ipop) 

                  st_coh_base(ii,ic,ipop)= 
     &            st_coh_base(ii,ic,ipop)+
     &            casecab(icohort,iagey,icty,ipop) 

                  st_coh_diff(ii,ic,ipop)= 
     &            st_coh_diff(ii,ic,ipop)+
     &            caseca(icohort,iagey,icty,ipop)-
     &            casecab(icohort,iagey,icty,ipop)
c
                ENDDO  ! iagey
              ENDDO  ! icohort
            ENDIF  ! icomp
          ENDDO ! ii
        ENDDO ! icty
      ENDDO ! ipop      
c
      DO ic = 1,4  ! 4 cohort groups
        DO ipop=1,maxpops   ! 4 population types
          WRITE(ounit31,*)'Population =',ipop
          WRITE(ounit31,*)'Cohort group = ',ic
          DO ii=1,ireg
            WRITE(ounit31,*)group1(ii),st_coh_base(ii,ic,ipop),
     +                                 st_coh_scen(ii,ic,ipop),
     &                                 st_coh_diff(ii,ic,ipop)
          ENDDO ! ii
        ENDDO !ipop
      ENDDO
c
      CLOSE(ounit31)
c

c        DO ipop=1, maxpops
c            DO icty=1,numcty

c      WRITE(ounit, 710) ipop, icty
c
C DECOMMENT THE FOLLOWING FOR DETAILED COHORT-TIME SCENARIO MATRIX

c               DO icohort=colo,cohi

c                    WRITE(ounit, 604) icohort,
c     +                          (colo_year+(icohort-1)*step),
c     +                  (caseca(icohort,
c     +                  iagey, icty, ipop), iagey=1,maxages * step+4)
c               ENDDO ! icohort
c             ENDDO ! icty
c        ENDDO ! ipop


603   FORMAT (3x,i4)
604   FORMAT (i4,2x,i4,100(:,f14.6))

c     CLOSE(ounit)

c-------------------------------------------------------------------
c by county
c------------------------------------------------------------------
      IF (first) THEN
        OPEN(ounit441, file=dir_out//'sum_cty.txt')
      ELSE
        OPEN(ounit441, file=dir_out//'sum_cty.txt', status = 'OLD',
     +       access = 'APPEND')
      ENDIF
c
! initialize            
      ct_tot_diff_l(:)=0.0
      ct_tot_diff_d(:)=0.0

      DO icty=1, numcty
        icomp = int(cty_fip(icty)/1000)
c
        DO ii=1, ireg
          IF (icomp.EQ.group1(ii)) THEN
c
            DO icohort = colo,cohi
              DO iagey=1,maxages*step +4
                DO ipop = 1,maxpops
c
                  IF ((ipop.EQ.1).OR.(ipop.EQ.2)) THEN  ! light skinned
                    ct_tot_diff_l(icty)= 
     &              caseca(icohort,iagey,icty,ipop) -
     +              casecab(icohort,iagey,icty,ipop) + 
     +              ct_tot_diff_l(icty)
c      
c
cc      IF ((icomp.EQ.10).AND.(ipop.EQ.3)) THEN
cc            WRITE(*,*)'ipop =',icty,ipop,caseca(icohort,iagey,icty,ipop)
cc      ENDIF
c
                  ELSEIF ((ipop.EQ.3).OR.(ipop.EQ.4)) THEN  ! dark skinned
                    ct_tot_diff_d(icty)=
     &              caseca(icohort,iagey,icty,ipop) -
     +              casecab(icohort,iagey,icty,ipop) + 
     +              ct_tot_diff_d(icty)
                  ENDIF

                ENDDO ! ipop
              ENDDO ! iagey
            ENDDO ! icohort

          ENDIF
        ENDDO ! ii
      ENDDO ! icty

      DO icty=1, numcty
        WRITE(ounit441,*)icty,cty_fip(icty),
     +                        ct_tot_diff_l(icty),
     +                        ct_tot_diff_d(icty),
     +                        ct_tot_diff_l(icty)+ct_tot_diff_d(icty)
            
      ENDDO ! icty
c
      CLOSE(ounit441)
            

C=====================================================================
C  Writes Baseline Inc/Mort by cohort, ilat, and ipop over cohort time
C=====================================================================

c      IF (first) THEN
c        OPEN(ounit, file=dir_out//'casecab.txt')
c      ELSE
c        OPEN(ounit, file=dir_out//'casecab.txt', status = 'OLD',
c     +       access = 'APPEND')
c      ENDIF

c      WRITE(ounit, 700) 'Measure:   >',endpttmp,'<'
c      WRITE(ounit, 700) 'D-RType:   >',drtmp,'<'

700   FORMAT(t4,a,a8,a)

c        DO ipop=1, maxpops

c            DO icty = 1,numcty

c      WRITE(ounit, 710) ipop, icty
710   FORMAT(i3,3x,i3)

C DECOMMENT THE FOLLOWING FOR DETAILED COHORT-TIME BASELINE MATRIX

c               DO icohort=colo,cohi
c                    WRITE(ounit, 603) icohort
c                    WRITE(ounit, 604) icohort,
c     +                          (colo_year+(icohort-1)*step),
c     +                  (casecab(icohort,
c     +                  iagey, icty, ipop), iagey=1,maxages * step+4)
c               ENDDO ! icohort
c             ENDDO ! icty
c        ENDDO ! ipop

c        CLOSE(ounit)

C=======================================================================
C  Writes Incremental Inc/Mort by cohort, ipop, ilat, over cohort time
C=======================================================================

c      IF (first) THEN
c        OPEN(ounit, file=dir_out//'casecan.txt')
c      ELSE
c        OPEN(ounit, file=dir_out//'casecan.txt', status = 'OLD',
c     +       access = 'APPEND')
c      ENDIF

c      WRITE(*,'(a11)') 'casecan.txt'

c      WRITE(ounit, 700) 'Measure:   >',endpttmp,'<'
c      WRITE(ounit, 700) 'D-RType:   >',drtmp,'<'

c      DO ipop=1, maxpops

c        DO icty = 1,numcty

c      WRITE(ounit, 710) ipop, ilat

c          DO icohort=colo,cohi

c             WRITE(ounit, 604) (colo_year+(icohort-1)*step),
c     +                  ((caseca(icohort,
c     +                  iagey, icty, ipop)
c     +                  - casecab(icohort, iagey, icty, ipop)),
c     +                  iagey= 1, maxages * step + 4)
c          ENDDO ! icohort
c        ENDDO ! icty
c      ENDDO ! ipop

c      CLOSE(ounit)


C=====================================================================
C  Calculates and Writes Total Cases by cohort, ilat, ipop
C=====================================================================
      iWRITE=0
      IF (iWRITE.EQ.1) THEN  ! WRITE out the following files

      IF (first) THEN
        OPEN(ounit, file=dir_out//'tcolp.txt')
      ELSE
        OPEN(ounit, file=dir_out//'tcolp.txt', 
     &              status = 'OLD',
     &              access = 'APPEND')
      ENDIF

      WRITE(ounit, 700) 'Measure:   >',endpttmp,'<'
      WRITE(ounit, 700) 'D-RType:   >',drtmp,'<'

cc      WRITE(*,*)'colo_year =',colo_year
cc
      DO ipop=1, maxpops
        DO icty = 1,numcty

          WRITE(ounit, 710) ipop, icty

          DO icohort = colo, cohi

            t1=0
            t2=0

            DO iagey = 1, maxages * step + 4

              t1 = t1 + caseca(icohort, iagey, icty, ipop)
              t2 = t2 + casecab(icohort, iagey, icty, ipop)

            ENDDO ! iagey

            WRITE (ounit, 713) (colo_year + (icohort - 1) * step),
     +                         t1, t2, (t1-t2)

713         FORMAT (i4,3(:,f14.5))

          ENDDO ! icohort
        ENDDO ! icty
      ENDDO ! ipop

       CLOSE(ounit)

      ENDIF ! writing out files

C=================================================================
C  Calculates and Writes Totals by cohort for all ipops and ilats
C=================================================================

      IF (first) THEN
        OPEN(ounit, file=dir_out//'tcoall.txt')
      ELSE
        OPEN(ounit, file=dir_out//'tcoall.txt', status = 'OLD',
     +       access = 'APPEND')
      ENDIF

      WRITE(ounit, 700) 'Measure:   >',endpttmp,'<'
      WRITE(ounit, 700) 'D-RType:   >',drtmp,'<'

      DO icohort = colo, cohi

        tca=0
        tcb=0

        DO ipop=1, maxpops
          DO icty = 1,numcty
            DO iagey = 1, maxages * step + 4

            tca = tca + caseca(icohort, iagey, icty, ipop)
            tcb = tcb + casecab(icohort, iagey, icty, ipop)

            ENDDO ! iagey
          ENDDO ! icty
        ENDDO ! ipop

        WRITE(ounit, 713) (colo_year + (icohort - 1) * step),
     +                    tca, tcb, (tca-tcb)

      ENDDO ! icohort

      CLOSE(ounit)

C====================================================
C  Call other by-year and lat write routines
C====================================================

cc     CALL write_eff_agg_age(endpoint,first)

      first = .false.

      CALL skip(effrun,eof)
      count = count + 1
 
!===============================================================
! END LOOP THROUGH DISEASE-TYPE SELECTIONS IN EFFRUN FILE
!===============================================================
      ENDDO ! effrun
!===============================================================

c RLM commented out on 11/21/2014
cxc      CALL write_effects


999   CLOSE(effrun)
      CLOSE(scratch)
      RETURN

1070  CALL error(70, *999)
1080  CALL error(80, *999)

      END SUBROUTINE effects

