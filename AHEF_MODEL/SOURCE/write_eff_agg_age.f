C=====================================================================
      SUBROUTINE write_eff_agg_age(indexname,first)
C=====================================================================
C  Subroutine to write effects output file - for the age routine
C=====================================================================
      IMPLICIT NONE

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
c      INCLUDE 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
      INCLUDE 'effects.fi'

      LOGICAL first
      CHARACTER*8 indexname
      REAL caseout(maxlats)
      REAL caseoutb(maxlats)
      REAL casesan(2100 + topage + 4 - 1887,maxlats,maxpops)
!----------------------------------------------------------------

      IF (first) THEN
        OPEN(ounit, file=effagename)
      ELSE
        OPEN(ounit, file=effagename, status = 'OLD', access = 'APPEND')
      ENDIF

      WRITE(ounit, '(100(:,A))') '*',
     +             ('=', iloop = 1, 69)
c      WRITE(ounit, '(A)') '*  EFFECTS(year,latitude)'
      WRITE(ounit, '(A)') '*  EFFECTS(year,county)'
      WRITE(ounit, '(100(:,A))') '*',
     +             ('=', iloop = 1, 69)
      WRITE(ounit, '(A)') '*'

      WRITE(ounit, 100) 'Measure:   >',indexname,'<'
      WRITE(ounit,101)   ' County:      >',numcty,'<'
c      WRITE(ounit, 101) 'Latitudes:    >',numlats,'<'

100   FORMAT(t4,a,a8,a)
101   FORMAT(t8,a,i2,a)

      WRITE(ounit, '(A)') '*'
      WRITE(ounit, 110) '*', (lats(ilat), ilat = 1, numlats)
c      WRITE(punit,110)'*', (cnty
110   FORMAT(a,t5,100(:,f14.1))
      WRITE(ounit, 111) '*', ('    --------', idummy = 1, numlats)
111   FORMAT(a,t5,100(:,a14))

C=============================================================
C  Get cases by year by ipop, by ilat
C=============================================================

        casesa=0
        casesab=0

c        DO ilat = 1, numlats
        DO icty = 1, numcty

          DO ipop = 1, maxpops

            DO icohort = colo, cohi

              DO iagey = 1, maxages * step + 4
c mrlm - icohort starts at 1 and year starts at 1, then iagey adds 1 through 90s with each addition 
c making iyear increase by 1;  with the next icohort loop, the iyear starts at 2 etc. (so each
c cohort loop shIFts the values for the iyear up  
c
              iyear = (icohort - 1) * step + iagey

              casesa(iyear, icty, ipop) = casesa(iyear, icty, ipop) +
     +              caseca(icohort, iagey, icty, ipop)

              casesab(iyear, icty, ipop) = casesab(iyear, icty, ipop) +
     +              casecab(icohort, iagey, icty, ipop)
c      mrlm debug
c      IF ((ipop.eq.1).and.(ilat.eq.1)) THEN
c      WRITE(95,*)'iyear - ',iyear,icohort,iagey
c      ENDIF
              ENDDO ! iagey
            ENDDO ! icohort
          ENDDO ! ipop
        ENDDO ! icty

C=====================================================
C  Writes Scenario Cases by year, ilat, ipop
C=====================================================

      IF (first) THEN
        OPEN(o1unit, file='casesa.txt')
      ELSE
        OPEN(o1unit, file='casesa.txt', status = 'OLD',
     +       access = 'APPEND')
      ENDIF

      WRITE(o1unit, 200) 'Measure:   >',indexname,'<'

      WRITE(o1unit, 200) 'D-RType:   >',drtype,'<'

      DO ipop = 1, maxpops

      WRITE(o1unit, 202) ipop

        DO iyear=1, (cohi-1) * step + maxages * step + 4
        WRITE(o1unit,115) iyear+colo_year-1-int(step/2),
     +    (casesa(iyear, icty, ipop), icty=1, numcty)
        ENDDO ! iyear
      ENDDO ! ipop

115     FORMAT(i4,t10,3(:,f14.2))
202     FORMAT(i3)

        CLOSE(o1unit)

C=====================================================
C  Writes Baseline Cases by year, ilat, ipop
C=====================================================

      IF (first) THEN
        OPEN(o2unit, file='casesab.txt')
      ELSE
        OPEN(o2unit, file='casesab.txt', status = 'OLD',
     +       access = 'APPEND')
      ENDIF

      WRITE(o2unit, 200) 'Measure:   >',indexname,'<'

      WRITE(o2unit, 200) 'D-RType:   >',drtype,'<'

      DO ipop = 1, maxpops

      WRITE(o2unit, 202) ipop

        DO iyear=1, (cohi-1) * step + maxages * step + 4
        WRITE(o2unit,115) iyear+colo_year-1-int(step/2),
     +     (casesab(iyear, icty, ipop), ilat=1, numlats)
        ENDDO ! iyear
      ENDDO ! ipop

        CLOSE(o2unit)

C=====================================================
C  Writes Incremental Cases by year, ilat, ipop
C=====================================================

      IF (first) THEN
        OPEN(o2unit, file='casesan.txt')
      ELSE
        OPEN(o2unit, file='casesan.txt', status = 'OLD',
     +       access = 'APPEND')
      ENDIF

      WRITE(o2unit, 200) 'Measure:   >',indexname,'<'

      WRITE(o2unit, 200) 'D-RType:   >',drtype,'<'

      DO ipop = 1, maxpops

      WRITE(o2unit, 202) ipop

        DO iyear=1, (cohi-1) * step + maxages * step + 4
        WRITE(o2unit,115) iyear+colo_year-1-int(step/2),
     +    ((casesa(iyear, icty, ipop) - casesab(iyear, icty, ipop)),
     +    icty=1,numcty)
        ENDDO ! iyear
      ENDDO ! ipop

        CLOSE(o2unit)

C=========================================================
C  Computes and Writes Scenario, Baseline, and Incremental
C  Cases by year for all lats and pops
C=======================================================

      IF (first) THEN
        OPEN(o1unit, file='run_test.efn')
      ELSE
        OPEN(o1unit, file='run_test.efn', status = 'OLD',
     +       access = 'APPEND')
      ENDIF

      WRITE(o1unit, 200) 'Measure:   >',indexname,'<'

      WRITE(o1unit, 200) 'D-RType:   >',drtype,'<'

      IF (first) THEN
        OPEN(o3unit, file='run_test.efb')
      ELSE
        OPEN(o3unit, file='run_test.efb', status = 'OLD',
     +       access = 'APPEND')
      ENDIF

      WRITE(o3unit, 200) 'Measure:   >',indexname,'<'

      WRITE(o3unit, 200) 'D-RType:   >',drtype,'<'

      WRITE(ounit, 200) 'D-RType:   >',drtype,'<'
c
      WRITE(*,*)'colo_year, outputlo,outputhi =',colo_year,outputlo, 
     + outputhi
c
      DO iyear = outputlo, outputhi
c            mrlm - this starts the index at the outputlo (i.e. for 
c          1950, the corresponding index for caseout would be 63
c          for 2150, this corresponds to a year index for caseout of 2150
        iyri = iyear - colo_year + int(step/2) + 1

        caseout = 0
        caseoutb = 0
          DO ipop = 1, maxpops
            DO icty = 1, numcty
              caseout(icty) = caseout(icty) +
     +                    casesa(iyri,icty,ipop)

              caseoutb(icty) = caseoutb(icty) +
     +                    casesab(iyri,icty,ipop)
cc            WRITE(95,*)'ipop,ilat,iyear,iyri',ipop,ilat,iyear,iyri
            ENDDO ! icty
          ENDDO ! ipop

        WRITE(ounit, 120) iyear, (caseout(icty), icty = 1, numcty)

        WRITE(o3unit, 120) iyear, (caseoutb(icty), icty = 1, numcty)

        WRITE(o1unit, 120) iyear, ((caseout(icty) -
     +                            caseoutb(icty)), icty = 1, numcty)

120     FORMAT(i4,t5,100(:,f14.2))

      ENDDO ! iyear

      WRITE(ounit, 111) '*', ('    --------', idummy = 1, numlats)
      WRITE(ounit, '(A)') '*'

200   FORMAT(t4,a,a8,a)
      CLOSE(ounit)
      CLOSE(o3unit)
      CLOSE(o1unit)
      RETURN

      END SUBROUTINE write_eff_agg_age

