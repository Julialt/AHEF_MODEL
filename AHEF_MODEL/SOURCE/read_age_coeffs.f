C=====================================================================
      SUBROUTINE READ_age_coeffs
C=====================================================================
C   Read age file
c=====================================================================

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'effects.fi'

      INTEGER agelp, lastage, age
!--------------------------------------------------

      OPEN(iunit, file = agename, status = 'OLD', err = 1140)
      WRITE(logfile,*) 'Reading Age Coefficients'

      CALL skip( iunit, eof )

      lastage = -1

      DO WHILE (.not. eof)
        READ( iunit, 100 ) age, age_coeffs(age/step + 1)
        WRITE( logfile, 100 ) age, age_coeffs(age/step + 1)
100     FORMAT(t5,i2,t13,f5.2)

        IF ( (lastage .ne. -1) .and.
     +       (age .ge. (lastage + 2 * step)) ) THEN
          DO agelp = lastage + step, age - step, step
            age_coeffs(agelp/step + 1) =
     +         xinterp(age_coeffs(lastage/step + 1),
     +                 age_coeffs(age/step + 1),
     +                 lastage, agelp, age)
            WRITE(logfile,100) agelp,
     +             age_coeffs(agelp/step + 1)
          ENDDO ! agelp
        ENDIF

        lastage = age

        CALL skip( iunit, eof )
      ENDDO

999   CLOSE (iunit)
      RETURN

1140  CALL error(140,*999)

      END SUBROUTINE read_age_coeffs

