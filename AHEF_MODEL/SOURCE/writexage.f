C=====================================================================
      SUBROUTINE writexage(indexname,first)
C=====================================================================
C  Subroutine to write exposure output file
C=====================================================================
      IMPLICIT NONE
C$DEBUG: 'D'

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
c      INCLUDE 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
      INCLUDE 'exposure.fi'
      INCLUDE 'setup.h'

!---------------------------------------------------------------
      LOGICAL first
      INTEGER iloop, idummy, iageymax
      CHARACTER*8 indexname
      CHARACTER*12 expfilename

!---------------------------------------------------------------
      IF (expblflag) THEN
        expfilename = xageblname
      ELSE
        expfilename = xagename
      ENDIF

      WRITE(errfile,*) "Writing to exposure file : ",
!     &             expfilename,
     &             dir_io//expfilename
      WRITE(*,*) "Writing to exposure file : ",
!     &             expfilename,
     &             dir_io//expfilename

c  mrlm
c      WRITE(*,*)'xageblname,xagename,expfilname',xageblname,
c     +  xagename,expfilename

      IF (first) THEN
        OPEN(ounit,file=dir_io//expfilename)
      ELSE
        OPEN(ounit,file=dir_io//expfilename, 
     &             status='OLD',access='APPEND')
      ENDIF

      WRITE(ounit,'(100(:,a))')'*',('=', iloop = 1, 69)
      WRITE(ounit,'(a)')'*  EXPOSURE(cohort,age,county)'
      WRITE(ounit,'(100(:,a))')'*',('=', iloop = 1, 69)
      WRITE(ounit,'(a)')'*'

      WRITE(ounit,100)'Measure:   >',indexname,'<'
      WRITE(ounit,101)'Min Cohort:  >',colo_year,'<'
c      WRITE(ounit, 102)'Latitudes:    >',numlats,'<'
      WRITE(ounit,103)'Max Cohort:  >',cohi_year,'<'

! loop over counties: write county header
!      DO icty = 1, numcty
! DEBUG !
      DO icty = 1, 5
! END DEBUG !

        WRITE(ounit,'(a)') '*'
        WRITE(ounit,104)'County:    >',cty_fip(icty),'<'
        WRITE(ounit,'(a)') '*'
        WRITE(ounit,110)'*iyr',((idummy-1)*step,idummy*step-1,
     +                        idummy=1,maxages-1),(maxages-1)*step
        WRITE(ounit,111)'*',('---------', idummy = 1, maxages)

! loop over cohorts: write data line for each cohort (i.e. year)
        DO icohort = colo,cohi

          iyear    = colo_year+(icohort-1)*step
          iageymax = maxages*step+4
          WRITE(ounit,120)iyear,
     &         (expos_age(icohort,iagey,icty),iagey=1,iageymax)

        ENDDO

        WRITE(ounit,111)'*', ('---------', idummy = 1, maxages)
        WRITE(ounit,'(a)')'*'

      ENDDO

      CLOSE(ounit)
      RETURN

100   FORMAT(t4,a,a8,a)
101   FORMAT(t8,a,i4,a)
102   FORMAT(t4,a,i2,a)
103   FORMAT(t11,a,i4,a)
104   FORMAT(t4,a,t18,i5,a)
110   FORMAT(a4,t12,100(:,i2.2,'-',i2.2,7x))
111   FORMAT(a,t5,100(:,3x,a9))
120   FORMAT(i4,t5,100(:,e12.4))

      END SUBROUTINE writexage

