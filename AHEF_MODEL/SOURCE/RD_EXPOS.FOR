C=====================================================================
      SUBROUTINE READ_EXPOSURE(indexname)
C=====================================================================
C  Subroutine to read exposure file
C=====================================================================

      include 'files.fi'
	include 'global.fi'
c      include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'

      include 'effects.fi'

      logical eof
      character*8 indexname, indextmp

      write (errfile, *) 'Reading Exposure'

      rewind(scratch)
      call skip( scratch, eof )

      do while (.not. eof)

        read(scratch, 100) indextmp, colo_year
cc	write(*,*)'indextmp, colo_year = ',indextmp,colo_year
	  read(scratch, 102) cohi_year
c        read(scratch, 102) numlats, cohi_year
102   format(t23,a,i4,a)
        colo = 1
        cohi = (cohi_year - colo_year)/step + 1

100     format(t14,a,a8,a,t45,i4)
c102     format(t19,i2,t46,i4)

c        do ilat = 1, numlats
	   do icty = 1, numcty

          call skip( scratch, eof )
c          read (scratch,'(t18,f5.2)') lats(ilat)
		read (scratch,'(t18,f5.2)') cty(icty)
          call skip( scratch, eof )

          do icohort = colo, cohi

           read(scratch, 120) (expage(icohort,iage,icty),
     +          iage = 1, maxages*step + 4)
c           write(errfile, 120) (expos1(icohort,iage,ilat),
c    +          iage = 1, maxages)
120         format(t5,100(:,e12.4))

          end do ! icohort
        end do ! icty

c       write (*,*) indexname,indextmp
        if (indexname .eq. indextmp) goto 999

        write (errfile, *) 'Skipping to Next'

        call skip( scratch, eof )

      end do

      goto 1130

999   return

1130  call error(130, *999)
      end

