C=====================================================================
      SUBROUTINE READ_COEFFICIENTS
C=====================================================================
C   Read coefficients file
C=====================================================================

      include 'files.fi'
	include 'global.fi'
c      include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'

      include 'effects.fi'

      logical eof
      integer ilp

      open(iunit, file = coeffname, status = 'OLD', err = 1110)
      write(errfile,*) 'Reading Coefficients'

      call skip( iunit, eof )

      do ipop = 1, maxpops
        read( iunit, 100, end=1070 ) (coeff(ipop,ilp), ilp=1,5)
        write( errfile, 100 ) (coeff(ipop,ilp), ilp=1,5)
100     format(t15,f6.3,t25,100(f6.3,7x))

        call skip( iunit, eof )
      end do

999   close (iunit)
      return

1070  call error(70,*999)
1110  call error(110,*999)
      end

