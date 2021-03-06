C=====================================================================
      SUBROUTINE read_population(Filename)
C=====================================================================
C   Read population definition file
C=====================================================================

      INCLUDE 'files.fi'
c      INCLUDE 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
      INCLUDE 'global.fi'
      INCLUDE 'effects.fi'

c mlrm
      CHARACTER*5 ctypl
c
      CHARACTER*12 filename
      CHARACTER tempchar
!      LOGICAL eof, expinterp, last
      LOGICAL expinterp, last
      INTEGER col, year, lastyear, yrlp
      REAL totpop(minyear:maxyear), temp
      REAL regbrk(minyear:maxyear, maxlats)
      REAL popbrk(minyear:maxyear, 6)
      REAL agebrk(minyear:maxyear, maxages, 6)
c
      REAL popt(minyear:maxyear, maxages, numcty, 6)
      INTEGER ctyint
c
      filename = filename(1:len_trim(filename))//'.POP'
      OPEN(iunit, file = filename, status = 'OLD', err = 1090)
      WRITE(errfile,*) 'Reading Population'

cc      WRITE(*,*) 'Entered READ_pop'
cc      WRITE(*,*) 'num cty = ',numcty
c
      expinterp = .true.
c
      pophi = 0
      poplo = 0
c mlrm add
      popminyr = 1985
c for testing
      popmaxyr = 2025
c      popmaxyr = 2005      
c
      yrstep = 5
C=====================================================================
C   Read total population and regional breakout
C=====================================================================

cc      WRITE(*,*) 'read_pop 1'

cmrlm      CALL skip( iunit, eof )

      last = .false.
      lastyear = 0

cc      WRITE(*,*) 'read_pop 1b'

      DO WHILE (.NOT. last)
          DO iyear = 1,((popmaxyr-popminyr)/yrstep)+1

            DO icty = 1,numcty

            year = popminyr + (iyear -1)*yrstep
c110     FORMAT(t4,i4,t11,18(i9))
cc      WRITE(*,*)'year - ',year,iage,icty, maxages
c
            READ (iunit,*)year,ctyint, 
     +            (popt(year,iage,icty,1), iage=1,maxages),
     +            (popt(year,iage,icty,2), iage=1,maxages),
     +            (popt(year,iage,icty,3), iage=1,maxages),
     +            (popt(year,iage,icty,4), iage=1,maxages),
     +            (popt(year,iage,icty,5), iage=1,maxages),
     +            (popt(year,iage,icty,6), iage=1,maxages)
c
c  convert character string to integer for state/county fip
c
cc              cty(icty)=ichar(ctypl(5:5))-48
cc          cty(icty)= cty(icty)+ (ichar(ctypl(4:4))-48)*10
cc          cty(icty)=cty(icty)+(ichar(ctypl(3:3))-48)*100
cc          cty(icty)=cty(icty)+(ichar(ctypl(2:2))-48)*1000
cc          cty(icty)=cty(icty)+(ichar(ctypl(1:1))-48)*10000
c
cc      WRITE(*,*)'icty,iyear,year = ',icty,iyear, year
c------------ match up counties
            DO ii=1,522     !CHANGE BY REGION!!!!!!!!!!
                  IF (ctyint.EQ.(cty_fip(ii))) THEN
                        DO iage=1,maxages
                              pop(year,iage,ii,1)=popt(year,iage,icty,1)
     +            + popt(year,iage,icty,5)
                              pop(year,iage,ii,2)=popt(year,iage,icty,2)
     +        + popt(year,iage,icty,6)
                              pop(year,iage,ii,3)=popt(year,iage,icty,3)
                              pop(year,iage,ii,4)=popt(year,iage,icty,4)
c                              pop(year,iage,ii,5)=popt(year,iage,icty,5)
c                              pop(year,iage,ii,6)=popt(year,iage,icty,6)
                        ENDDO
                        GOTO 677
                  ENDIF
            ENDDO

c
c--------------------- interpolate between years -------------
677     IF ((lastyear .NE. 0) .AND. (year .GT. (lastyear + 1))) THEN
          DO yrlp = lastyear + 1, year - 1
            IF (expinterp) THEN    ! exponential or linear interp
            DO ipop=1,maxpops
              DO iage=1, maxages
c      
cc      IF ((icty.EQ.1).AND.(lastyear.EQ.2005)) THEN
cc      WRITE(*,*)lastyear,year,iage,icty,ipop,
cc     + pop(lastyear,iage,icty,ipop),pop(year,iage,icty,ipop),
cc     + (einterp(REAL(pop(lastyear,
cc     +         iage,icty,ipop)), REAL(pop(year,iage,icty,ipop)),
cc     +         lastyear, yrlp, year))
cc      ENDIF
c
cc      WRITE(*,*)' interpolating...'
c
               pop(yrlp, iage, icty, ipop) = (einterp(REAL(pop(lastyear,
     +         iage,icty,ipop)), REAL(pop(year,iage,icty,ipop)),
     +         lastyear, yrlp, year))
              ENDDO ! iage
            ENDDO ! ipop

            ENDIF
          ENDDO ! yrlp
        ENDIF
c------------------------------------------------------------
        ENDDO ! icty
c
        IF (lastyear .EQ. 0) poplo = year
        lastyear = year
c
cc      WRITE(*,*)'last year, poplo =',lastyear,poplo,year
c
        ENDDO ! iyear

        CALL check(iunit,last)
      ENDDO ! not last

      pophi = year



999   CLOSE (iunit)

      RETURN

1070  CALL error(70,*999)
1090  CALL error(90,*999)
1100  CALL error(100,*999)

      END SUBROUTINE read_population

