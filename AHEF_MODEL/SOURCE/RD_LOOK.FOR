C=====================================================================
      SUBROUTINE READLOOKUP(filename)
C=====================================================================
C   Read lookup table for given action spectrum
C   (i.e., irradiance by zenith angle and dobson units)
C=====================================================================

C$DEBUG: 'D'

      include 'files.fi'
	include 'global.fi'
c      include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'

      include 'exposure.fi'

      character*12 filename
      integer row,col
      logical eof

      open(iunit, file = filename,Defaultfile=
     +'\\tsclient\C\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data',
c     +	'C:\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data\',
     +status = 'OLD', err = 1040)
      write(errfile,*) 'Reading LOOKUP'
c
c	mrlm 
cc	write(*,*)'lrows, lcols = ',lrows,lcols
c
      call skip( iunit, eof )
      do row = 1, lrows
        read(iunit, *, err=1070) idummy,
     +                           (lookup(row,col), col=1, lcols)
      end do

999   close (iunit)
      return

1040  call error(40,*999)
1070  call error(70,*999)
      end

