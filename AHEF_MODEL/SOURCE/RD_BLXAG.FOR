C=====================================================================
      SUBROUTINE READ_BLEXPOSURE_AGE(indexname)
C=====================================================================
C  Subroutine to read baseline exposure age file
C=====================================================================

      include 'files.fi'
	include 'global.fi'
c      include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
      include 'effects.fi'

      logical eof
      character*8 indexname, indextmp

      write (errfile, *) 'Reading Baseline Exposure'
cc mrlm 10/2008 debug
cc	write(*,*)'scratchagebl - ',scratchagebl
cc
      rewind(scratchagebl)
      call skip( scratchagebl, eof )

      do while (.not. eof)

	  read(scratchagebl,100)indextmp,colo_year
cc	write(*,*)'in lo = ',indextmp,colo_year
	  read(scratchagebl,103)cohi_year
cc	write(*,*)'hi = ',cohi_year
c
100     format(t16,a8,t46,i4)
103     format(t25,i4)

        colo = 1
        cohi = (cohi_year - colo_year)/step + 1

c        do ilat = 1, numlats
	   do icty = 1,numcty

          call skip( scratchagebl, eof )
		read (scratchagebl,'(t18,i5)') cty(icty)
cc	write(*,*)' cty(icty) = ',cty(icty)
c
          call skip( scratchagebl, eof )

          do icohort = colo, cohi

            read(scratchagebl, 120) (expagebl(icohort,iagey,icty),
     +          iagey = 1, maxages * step + 4)
ccmrlm 10/2008 debug - uncommented write statement below - put comment back
cc           write(errfile, 120) (expagebl(icohort,iagey,ilat),
cc     +          iagey = 1, maxages * step + 4)
120         format(t5,100(:,e12.4))

          end do ! icohort
        end do ! icty

c       write (*,*) indexname,indextmp
        if (indexname .eq. indextmp) goto 999

        write (errfile, *) 'Skipping to Next'

        call skip( scratchagebl, eof )

      end do

      goto 1130

999   return

1130  call error(130, *999)
      end

