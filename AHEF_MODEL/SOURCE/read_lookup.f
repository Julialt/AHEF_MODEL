C=====================================================================
      SUBROUTINE read_lookup(filename)
C=====================================================================
C   Read lookup table for given action spectrum
C   (i.e., irradiance by zenith angle and DObson units)
C=====================================================================
      IMPLICIT NONE

      INCLUDE 'files.h'
      INCLUDE 'global.h'
      INCLUDE 'exposure.h'
      INCLUDE 'setup.h'

      CHARACTER(len=*),INTENT(IN) :: filename

      INTEGER :: irow,icol,idummy
! lrows & lcols are defined in file global.fi
!-------------------------------------------------

      WRITE(logfile,*) 'Reading lookup : ',filename
      WRITE(*,*) 'Reading lookup : ',filename

      OPEN(iunit,file=dir_act//filename,status='OLD',err=1040)

      CALL skip(iunit,eof)
      DO irow = 1,lrows
        READ(iunit,*,err=1070,end=999) 
     &           idummy,(lookup(irow,icol),icol=1,lcols)
        CALL skip(iunit,eof)
      ENDDO

999   CLOSE (iunit)
      RETURN

1040  CALL error(40,*999)
1070  CALL error(70,*999)

      END SUBROUTINE read_lookup

