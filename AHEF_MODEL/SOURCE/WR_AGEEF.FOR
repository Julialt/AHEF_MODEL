C=====================================================================
      SUBROUTINE WRITE_EFF_AGG_AGE(indexname,first)
C=====================================================================
C  Subroutine to write effects output file - for the age routine
C=====================================================================

      include 'files.fi'
	include 'global.fi'
c      include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
      include 'effects.fi'

      logical first
      character*8 indexname
      real caseout(maxlats)
      real caseoutb(maxlats)
      real casesan(2100 + topage + 4 - 1887,maxlats,maxpops)

      if (first) then
        open(ounit, file = effagename)
      else
        open(ounit, file=effagename, status = 'OLD', access = 'APPEND')
      endif

      write(ounit, '(100(:,A))') '*',
     +             ('=', iloop = 1, 69)
c      write(ounit, '(A)') '*  EFFECTS(year,latitude)'
	write(ounit, '(A)') '*  EFFECTS(year,county)'
      write(ounit, '(100(:,A))') '*',
     +             ('=', iloop = 1, 69)
      write(ounit, '(A)') '*'

      write(ounit, 100) 'Measure:   >',indexname,'<'
	write(ounit,101)   ' County:      >',numcty,'<'
c      write(ounit, 101) 'Latitudes:    >',numlats,'<'

100   format(t4,a,a8,a,\)
101   format(t8,a,i2.2,a)

      write(ounit, '(A)') '*'
      write(ounit, 110) '*', (lats(ilat), ilat = 1, numlats)
c	write(punit,110)'*', (cnty
110   format(a,t5,100(:,f14.1))
      write(ounit, 111) '*', ('    --------', idummy = 1, numlats)
111   format(a,t5,100(:,a14))

C=============================================================
C  Get cases by year by ipop, by ilat
C=============================================================

        casesa=0
        casesab=0

c        do ilat = 1, numlats
	  do icty = 1, numcty

          do ipop = 1, maxpops

            do icohort = colo, cohi

              do iagey = 1, maxages * step + 4
c mrlm - icohort starts at 1 and year starts at 1, then iagey adds 1 through 90s with each addition 
c making iyear increase by 1;  with the next icohort loop, the iyear starts at 2 etc. (so each
c cohort loop shifts the values for the iyear up  
c
              iyear = (icohort - 1) * step + iagey

              casesa(iyear, icty, ipop) = casesa(iyear, icty, ipop) +
     +              caseca(icohort, iagey, icty, ipop)

              casesab(iyear, icty, ipop) = casesab(iyear, icty, ipop) +
     +              casecab(icohort, iagey, icty, ipop)
c	mrlm debug
c	if ((ipop.eq.1).and.(ilat.eq.1)) then
c	write(95,*)'iyear - ',iyear,icohort,iagey
c	end if
              end do ! iagey
            end do ! icohort
          end do ! ipop
        end do ! icty

C=====================================================
C  Writes Scenario Cases by year, ilat, ipop
C=====================================================

      if (first) then
        open(o1unit, file='casesa.txt')
      else
        open(o1unit, file='casesa.txt', status = 'OLD',
     +       access = 'APPEND')
      endif

      write(o1unit, 200) 'Measure:   >',indexname,'<'

      write(o1unit, 200) 'D-RType:   >',drtype,'<'

      do ipop = 1, maxpops

      write(o1unit, 202) ipop

        do iyear=1, (cohi-1) * step + maxages * step + 4
        write(o1unit,115) iyear+colo_year-1-int(step/2),
     +    (casesa(iyear, icty, ipop), icty=1, numcty)
        end do ! iyear
      end do ! ipop

115     format(i4,t10,3(:,f14.2))
202     format(i3)

        close(o1unit)

C=====================================================
C  Writes Baseline Cases by year, ilat, ipop
C=====================================================

      if (first) then
        open(o2unit, file='casesab.txt')
      else
        open(o2unit, file='casesab.txt', status = 'OLD',
     +       access = 'APPEND')
      endif

      write(o2unit, 200) 'Measure:   >',indexname,'<'

      write(o2unit, 200) 'D-RType:   >',drtype,'<'

      do ipop = 1, maxpops

      write(o2unit, 202) ipop

        do iyear=1, (cohi-1) * step + maxages * step + 4
        write(o2unit,115) iyear+colo_year-1-int(step/2),
     +     (casesab(iyear, icty, ipop), ilat=1, numlats)
        end do ! iyear
      end do ! ipop

        close(o2unit)

C=====================================================
C  Writes Incremental Cases by year, ilat, ipop
C=====================================================

      if (first) then
        open(o2unit, file='casesan.txt')
      else
        open(o2unit, file='casesan.txt', status = 'OLD',
     +       access = 'APPEND')
      endif

      write(o2unit, 200) 'Measure:   >',indexname,'<'

      write(o2unit, 200) 'D-RType:   >',drtype,'<'

      do ipop = 1, maxpops

      write(o2unit, 202) ipop

        do iyear=1, (cohi-1) * step + maxages * step + 4
        write(o2unit,115) iyear+colo_year-1-int(step/2),
     +    ((casesa(iyear, icty, ipop) - casesab(iyear, icty, ipop)),
     +    icty=1,numcty)
        end do ! iyear
      end do ! ipop

        close(o2unit)

C=========================================================
C  Computes and Writes Scenario, Baseline, and Incremental
C  Cases by year for all lats and pops
C=======================================================

      if (first) then
        open(o1unit, file='run_test.efn')
      else
        open(o1unit, file='run_test.efn', status = 'OLD',
     +       access = 'APPEND')
      endif

      write(o1unit, 200) 'Measure:   >',indexname,'<'

      write(o1unit, 200) 'D-RType:   >',drtype,'<'

      if (first) then
        open(o3unit, file='run_test.efb')
      else
        open(o3unit, file='run_test.efb', status = 'OLD',
     +       access = 'APPEND')
      endif

      write(o3unit, 200) 'Measure:   >',indexname,'<'

      write(o3unit, 200) 'D-RType:   >',drtype,'<'

      write(ounit, 200) 'D-RType:   >',drtype,'<'
c
	write(*,*)'colo_year, outputlo,outputhi =',colo_year,outputlo, 
     + outputhi
c
      do iyear = outputlo, outputhi
c		mrlm - this starts the index at the outputlo (i.e. for 
c	    1950, the corresponding index for caseout would be 63
c	    for 2150, this corresponds to a year index for caseout of 2150
        iyri = iyear - colo_year + int(step/2) + 1

        caseout = 0
        caseoutb = 0
          do ipop = 1, maxpops
            do icty = 1, numcty
              caseout(icty) = caseout(icty) +
     +                    casesa(iyri,icty,ipop)

              caseoutb(icty) = caseoutb(icty) +
     +                    casesab(iyri,icty,ipop)
cc		write(95,*)'ipop,ilat,iyear,iyri',ipop,ilat,iyear,iyri
            end do ! icty
          end do ! ipop

        write(ounit, 120) iyear, (caseout(icty), icty = 1, numcty)

        write(o3unit, 120) iyear, (caseoutb(icty), icty = 1, numcty)

        write(o1unit, 120) iyear, ((caseout(icty) -
     +                            caseoutb(icty)), icty = 1, numcty)

120     format(i4,t5,100(:,f14.2))

      end do ! iyear

      write(ounit, 111) '*', ('    --------', idummy = 1, numlats)
      write(ounit, '(A)') '*'

200   format(t4,a,a8,a)
      close(ounit)
      close(o3unit)
      close(o1unit)
      return
      end

