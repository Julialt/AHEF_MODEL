C=====================================================================
      SUBROUTINE writexage(indexname,first)
C=====================================================================
C  Subroutine to write exposure output file
C=====================================================================

C$DEBUG: 'D'

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
c      INCLUDE 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
      INCLUDE 'exposure.fi'

      LOGICAL first
      CHARACTER*8 indexname
      CHARACTER*12 expfilename

      IF (expblflag) THEN
        expfilename = xageblname
      ELSE
        expfilename = xagename
      ENDIF
c  mrlm
c      WRITE(*,*)'xageblname,xagename,expfilname',xageblname,
c     +  xagename,expfilename
c

      IF (first) THEN
        OPEN(ounit, file = expfilename)
      ELSE
        OPEN(ounit, file = expfilename, status = 'OLD',
     +             access = 'APPEND')

      ENDIF

      WRITE(ounit, '(100(:,A))') '*',
     +             ('=', iloop = 1, 69)
      WRITE(ounit, '(A)') '*  EXPOSURE(cohort,age,county)'
      WRITE(ounit, '(100(:,A))') '*',
     +             ('=', iloop = 1, 69)
      WRITE(ounit, '(A)') '*'

      WRITE(ounit, 100) 'Measure:   >',indexname,'<'
      WRITE(ounit, 101) 'Min Cohort:  >',colo_year,'<'
c      WRITE(ounit, 102) 'Latitudes:    >',numlats,'<'
      WRITE(ounit, 103) 'Max Cohort:  >',cohi_year,'<'

100   FORMAT(t4,a,a8,a,\)
101   FORMAT(t8,a,i4,a)
102   FORMAT(t4,a,i2.2,a,\)
103   FORMAT(t11,a,i4,a)

      DO icty = 1, numcty

        WRITE(ounit, '(A)') '*'
c        WRITE (ounit,'(t4,a,t18,f5.2,a)')
            WRITE(ounit,'(t4,a,t18,i5,a)')
     +         'County:    >',cty_fip(icty),'<'
        WRITE(ounit, '(A)') '*'
        WRITE(ounit, 110) '*', ((idummy-1)*step,idummy*step-1,
     +                          idummy=1,maxages-1),(maxages-1)*step
110     FORMAT(a,t12,100(:,i2.2,'-',i2.2,7x))
        WRITE(ounit, 111) '*', ('---------', idummy = 1, maxages)
111     FORMAT(a,t5,100(:,3x,a9))

        DO icohort = colo, cohi

          iyear = colo_year + (icohort - 1)*step

          WRITE(ounit, 120) iyear, (expos_age(icohort,iagey,icty),
     +          iagey = 1, maxages * step + 4)
120       FORMAT(i4,t5,100(:,e12.4))

        ENDDO

        WRITE(ounit, 111) '*', ('---------', idummy = 1, maxages)
        WRITE(ounit, '(A)') '*'

      ENDDO

      CLOSE(ounit)
      RETURN

      END SUBROUTINE writexage

