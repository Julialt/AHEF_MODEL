C=====================================================================
      SUBROUTINE check(iounit,last)
C=====================================================================
C  Subroutine to check IF we have reached the end of
C  an input block
C=====================================================================

      INTEGER iounit
      LOGICAL last
      CHARACTER*1 col1

      last = .false.

100   READ(iounit,'(a1)', end=150) col1
      IF (col1 .NE. '*') GOTO 200
150   last = .true.

200   BACKSPACE iounit
      RETURN

      END SUBROUTINE check

C=====================================================================
C=====================================================================
      SUBROUTINE error(number,*)
C=====================================================================
! NOT CURRENTLY USED : JMLT, Feb 27, 2018 !
C=====================================================================
C   This subroutine handles errors.
C
C   Error  10:    Runfile not found
C   Error  20:    No action specified
C   Error  30:    Ozone file not found
C   Error  40:    Lookup file not found
C   Error  50:    Exposure run file not found
C   Error  60:    Age weighting file not found
C   Error  70:    Unexpected file format or EOF
C   Error  80:    Effects run file not found
C   Error  90:    Population file not found
C   Error 100:    Exposure and Effects Regions Differ
C   Error 110:    Coefficient file not found
C   Error 120:    Cohort risk file not found
C   Error 130:    Exposure measure not found
C   Error 140:    Age coefficient file not found
C
C=====================================================================

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
c       INCLUDE 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'


      INTEGER number

      errflag = .true.

      SELECT CASE (number)

        CASE(10)
          WRITE(*,*)         'ERROR 010: Runfile not found'
          WRITE(errfile,*)   'ERROR 010: Runfile not found'

        CASE(20)
          WRITE(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  No action specified'
          WRITE(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  No action specified'

        CASE(30)
          WRITE(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Ozone file not found'
          WRITE(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Ozone file not found'

        CASE(40)
          WRITE(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Lookup file not found'
          WRITE(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Lookup file not found'

        CASE(50)
          WRITE(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Exposure run file not found'
          WRITE(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Exposure run file not found'

        CASE(60)
          WRITE(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Age weighting file not found'
          WRITE(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Age weighting file not found'

        CASE(70)
          WRITE(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Unexpected format or end of file'
          WRITE(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Unexpected file format or EOF'

        CASE(80)
          WRITE(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Effects run file not found'
          WRITE(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Effects run file not found'

        CASE(90)
          WRITE(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Population file not found '
          WRITE(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Population file not found '

        CASE(100)
          WRITE(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Exposure and Effects Regions Differ'
          WRITE(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Exposure and Effects Regions Differ'

        CASE(110)
          WRITE(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Coefficient file not found'
          WRITE(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Coefficient file not found'

        CASE(120)
          WRITE(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Cohort risk file not found'
          WRITE(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Cohort risk file not found'

        CASE(130)
          WRITE(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Exposure measure not found'
          WRITE(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Exposure measure not found'

        CASE(140)
          WRITE(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Age coefficients file not found'
          WRITE(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Age coefficients file not found'

      END SELECT

      RETURN 1

100   FORMAT (a,i3.3,a,i3.3,a)
999   RETURN  

      END SUBROUTINE error

C=====================================================================
C=====================================================================
      REAL FUNCTION einterp (valu1,valu2,idx1,idx2,idx3)
C=====================================================================
C     Exponential Interpolation function
C=====================================================================

      INCLUDE 'files.fi'

      REAL valu1,valu2,tmp1,tmp2
      INTEGER idx1,idx2,idx3

      IF (valu1.NE.0) THEN
      tmp1 = log(valu1)
      ELSE
      tmp1 = 0
      ENDIF

      IF (valu2.NE.0) THEN
      tmp2 = log(valu2)
      ELSE
      tmp2 = 0
      ENDIF
c
      IF (idx3 .NE. idx1) THEN
        einterp = exp( tmp1 + (tmp2-tmp1) *
     +                        (idx2-idx1+0.0)/(idx3-idx1) )
      ELSE
        einterp = 0
        WRITE (errfile,*) 'Interpolation Error'
      ENDIF
c mlrm 2/2009 testing
c      If ((idx1.EQ.1985).AND.(idx2.EQ.1986).AND.(idx3.EQ.1990)) THEN
c      WRITE(99,*)'valu1 valu2 idx1 idx2 idx3', valu1, valu2, idx1, idx2, idx3
c      WRITE(99,*)'tmp1 tmp2 = ', tmp1, tmp2
c      WRITE(99,*)'later = ', (idx2 - idx1 + 0) / (idx3 - idx1)
c      WRITE(99,*)'in paran =', 
c    +      tmp1 + (tmp2 - tmp1) * (idx2 - idx1 + 0) / (idx3 - idx1)
c      WRITE(99,*)'einterp = ', einterp
c      ENDIF

      RETURN

      END FUNCTION einterp

C=====================================================================
C=====================================================================
      REAL FUNCTION xinterp (valu1,valu2,idx1,idx2,idx3)
C=====================================================================
C     Linear Interpolation function
C=====================================================================

      INCLUDE 'files.fi'

      REAL valu1,valu2
      INTEGER idx1,idx2,idx3

      IF (idx3 .NE. idx1) THEN
        xinterp = valu1 + (valu2-valu1) * (idx2-idx1+0.0)/(idx3-idx1)
      ELSE
        xinterp = 0
        WRITE (errfile,*) 'Interpolation Error'
      ENDIF

      RETURN

      END FUNCTION xinterp

C=====================================================================
C=====================================================================
      SUBROUTINE skip(iounit,eof)
C=====================================================================
C  Subroutine to skip comments while reading data files
C  Positions file at next data record by skipping those comment
C  records denoted by an asterisk in column 1.  The flag indicates
C  when the end of file has been reached.
C=====================================================================
      IMPLICIT NONE

! INCLUDING global.fi requires subroutine call with NO ARGUMENTS !
!      INCLUDE 'global.fi'    

      INTEGER iounit
      LOGICAL eof     ! only if global.fi not invoked
      CHARACTER*1 col1

      eof = .false.

100   READ(iounit,'(a1)',END=200) col1
      IF (col1 .NE. '*') GOTO 300
      GOTO 100

200   eof = .true.

300   BACKSPACE iounit

      RETURN
   
      END SUBROUTINE skip
