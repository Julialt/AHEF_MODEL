C=====================================================================
      SUBROUTINE READ_EXPOSURE_AGE(indexname)
C=====================================================================
C  Subroutine to read exposure by age file
C=====================================================================

      include 'files.fi'
	include 'global.fi'
c      include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
      include 'effects.fi'

      logical eof
      character*8 indexname, indextmp

      write (errfile, *) 'Reading Exposure'

      rewind(scratchage)
      call skip( scratchage, eof )

      do while (.not. eof)

	read(scratchage,100)indextmp,colo_year
	read(scratchage,103)cohi_year
c
100   format(t16,a8,t46,i4)
103   format(t25,i4)

        colo = 1
        cohi = (cohi_year - colo_year)/step + 1

   	  do icty = 1,numcty

           call skip( scratchage, eof )
		read (scratchage,'(t18,i5)') cty(icty)
cc		write(*,*)'cty = ',cty(icty)
          call skip( scratchage, eof )
cc mrlm - make sure lats aren't revered on read-in
cc 1/2008 - it's works fine!
cc	write(91,*)ilat, lats(ilat)
cc mrlm - end
          do icohort = colo, cohi

            read(scratchage, 120) (expage(icohort,iagey,icty),
     +          iagey = 1, maxages * step + 4)
cc mrlm - added in for check
cc	      write(91,120) (expage(icohort,iagey,icty),
cc     +          iagey = 1, maxages * step + 4)
cc mlrm - end
c           write(errfile, 120) (expage(icohort,iagey,ilat),
c    +          iagey = 1, maxages * step + 4)
120         format(t5,100(:,e12.4))

          end do ! icohort
        end do ! icty

c       write (*,*) indexname,indextmp
        if (indexname .eq. indextmp) goto 999

        write (errfile, *) 'Skipping to Next'

        call skip( scratchage, eof )

      end do

      goto 1130

999   return

1130  call error(130, *999)
      end

