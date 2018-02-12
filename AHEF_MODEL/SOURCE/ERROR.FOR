C=====================================================================
      SUBROUTINE ERROR(number,*)
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

      include 'files.fi'
	include 'global.fi'
c       include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'


      integer number

      errflag = .true.

      select case (number)

        case(10)
          write(*,*)         'ERROR 010: Runfile not found'
          write(errfile,*)   'ERROR 010: Runfile not found'

        case(20)
          write(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  No action specified'
          write(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  No action specified'

        case(30)
          write(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Ozone file not found'
          write(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Ozone file not found'

        case(40)
          write(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Lookup file not found'
          write(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Lookup file not found'

        case(50)
          write(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Exposure run file not found'
          write(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Exposure run file not found'

        case(60)
          write(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Age weighting file not found'
          write(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Age weighting file not found'

        case(70)
          write(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Unexpected format or end of file'
          write(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Unexpected file format or EOF'

        case(80)
          write(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Effects run file not found'
          write(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Effects run file not found'

        case(90)
          write(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Population file not found '
          write(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Population file not found '

        case(100)
          write(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Exposure and Effects Regions Differ'
          write(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Exposure and Effects Regions Differ'

        case(110)
          write(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Coefficient file not found'
          write(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Coefficient file not found'

        case(120)
          write(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Cohort risk file not found'
          write(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Cohort risk file not found'

        case(130)
          write(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Exposure measure not found'
          write(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Exposure measure not found'

        case(140)
          write(*,100)       ' ERROR ',number,' (Run ',runcount,
     +                       '):  Age coefficients file not found'
          write(errfile,100) 'ERROR ',number,' (Run ',runcount,
     +                       '):  Age coefficients file not found'

      end select

      return 1

100   format (a,i3.3,a,i3.3,a)
999   return  
      end
