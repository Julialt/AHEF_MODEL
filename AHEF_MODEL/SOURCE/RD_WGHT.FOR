C=====================================================================
      SUBROUTINE READWEIGHT(filename)
C=====================================================================
C   Read age weighting function
C=====================================================================

C$DEBUG: 'D'

      include 'files.fi'
	include 'global.fi'
c      include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
      include 'exposure.fi'

      character*12 filename
      logical eof
      integer row, col, idummy

      open(iunit, file = filename, Defaultfile=
     +'\\tsclient\C\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data\'
c     +	'C:\Users\rawlings\Desktop\ahef\input data\',
     +,status = 'OLD', err = 1060)
      write(errfile,*) 'Reading WEIGHTS'

      call skip( iunit, eof )
      do row = 1, maxages
        read(iunit, 100, err=1070) idummy,
     +                           (weights(row,col), col=1, maxages)
        write(errfile,100) idummy, (weights(row,col), col=1, maxages)
100     format(i3,100(:,2x,f5.1))
      end do

999   close (iunit)
      return

1060  call error(60,*999)
1070  call error(70,*999)
      end

