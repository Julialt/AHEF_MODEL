C=====================================================================
      SUBROUTINE READ_AGE_COEFFS
C=====================================================================
C   Read age file
c=====================================================================

      include 'files.fi'
	include 'global.fi'
c      include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'

      include 'effects.fi'

      logical eof
      integer agelp, lastage, age

      open(iunit, file = agename, status = 'OLD', err = 1140)
      write(errfile,*) 'Reading Age Coefficients'

      call skip( iunit, eof )

      lastage = -1

      do while (.not. eof)
        read( iunit, 100 ) age, age_coeffs(age/step + 1)
        write( errfile, 100 ) age, age_coeffs(age/step + 1)
100     format(t5,i2,t13,f5.2)

        if ( (lastage .ne. -1) .and.
     +       (age .ge. (lastage + 2 * step)) ) then
          do agelp = lastage + step, age - step, step
            age_coeffs(agelp/step + 1) =
     +         xinterp(age_coeffs(lastage/step + 1),
     +                 age_coeffs(age/step + 1),
     +                 lastage, agelp, age)
            write(errfile,100) agelp,
     +             age_coeffs(agelp/step + 1)
          end do ! agelp
        endif

        lastage = age

        call skip( iunit, eof )
      end do

999   close (iunit)
      return

1140  call error(140,*999)
      end

