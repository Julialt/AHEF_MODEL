C=====================================================================
      SUBROUTINE WRITEXAGE(indexname,first)
C=====================================================================
C  Subroutine to write exposure output file
C=====================================================================

C$DEBUG: 'D'

      include 'files.fi'
	include 'global.fi'
c      include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
      include 'exposure.fi'

      logical first
      character*8 indexname
      character*12 expfilename

      if (expblflag) then
        expfilename = xageblname
      else
        expfilename = xagename
      endif
c  mrlm
c	write(*,*)'xageblname,xagename,expfilname',xageblname,
c     +  xagename,expfilename
c

      if (first) then
        open(ounit, file = expfilename)
      else
        open(ounit, file = expfilename, status = 'OLD',
     +             access = 'APPEND')

      endif

      write(ounit, '(100(:,A))') '*',
     +             ('=', iloop = 1, 69)
      write(ounit, '(A)') '*  EXPOSURE(cohort,age,county)'
      write(ounit, '(100(:,A))') '*',
     +             ('=', iloop = 1, 69)
      write(ounit, '(A)') '*'

      write(ounit, 100) 'Measure:   >',indexname,'<'
      write(ounit, 101) 'Min Cohort:  >',colo_year,'<'
c      write(ounit, 102) 'Latitudes:    >',numlats,'<'
      write(ounit, 103) 'Max Cohort:  >',cohi_year,'<'

100   format(t4,a,a8,a,\)
101   format(t8,a,i4,a)
102   format(t4,a,i2.2,a,\)
103   format(t11,a,i4,a)

      do icty = 1, numcty

        write(ounit, '(A)') '*'
c        write (ounit,'(t4,a,t18,f5.2,a)')
		write(ounit,'(t4,a,t18,i5,a)')
     +         'County:    >',cty_fip(icty),'<'
        write(ounit, '(A)') '*'
        write(ounit, 110) '*', ((idummy-1)*step,idummy*step-1,
     +                          idummy=1,maxages-1),(maxages-1)*step
110     format(a,t12,100(:,i2.2,'-',i2.2,7x))
        write(ounit, 111) '*', ('---------', idummy = 1, maxages)
111     format(a,t5,100(:,3x,a9))

        do icohort = colo, cohi

          iyear = colo_year + (icohort - 1)*step

          write(ounit, 120) iyear, (expos_age(icohort,iagey,icty),
     +          iagey = 1, maxages * step + 4)
120       format(i4,t5,100(:,e12.4))

        end do

        write(ounit, 111) '*', ('---------', idummy = 1, maxages)
        write(ounit, '(A)') '*'

      end do

      close(ounit)
      return
      end

