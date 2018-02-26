C=====================================================================
      PROGRAM AHEF
C=====================================================================
C  Atmospheric and Health Effects Framework          File: AHEF.f
C  A tool for estimating the impact of emissions of ozone-depleting
C  chemicals on human health and the environment.
C
C  This framework has three major components:
C     1. Atmospheric model:  ODS emis -> Ozone depletion    : AHEF.f
C     2. Exposure model:     Ozone depletion -> UV exposure : exposure.f
C     3. Effects model:      UV Exposure -> Health effects  : effects.f
C=====================================================================

C$DEBUG: 'D'

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      character*1 temp1,temp2,temp3,temp4
      integer flagcount, endpoint
      logical eof

      character*3 emi_ext, ozn_ext, exp_ext, eff_ext
      character*3 atmrun_ext, exprun_ext, effrun_ext
      character*8 name, outname

      data (monthname(i), i=1, 12) / 'Jan', 'Feb', 'Mar', 'Apr',
     +      'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /

C=====================================================================
C  Open global runfile and global default names and extensions
C=====================================================================

      WRITE (*,*) 'Atmospheric and Health Effects Framework V4.0'
      WRITE (*,*) 'ICF Incorporated, 1997'
      WRITE (*,*)

      OPEN(errfile, FILE = 'AHEF.ERR')
C     OPEN(runfile, FILE = 'AHEF.RUN', status = 'OLD', ERR = 1010)
      OPEN(runfile, FILE = 'AHEF.RUN', status = 'OLD')

      CALL skip( runfile, eof )
      READ( runfile, 100, err=1070 )
     +                      emi_ext, ozn_ext, exp_ext, eff_ext
      WRITE( errfile, 100 )
     +                      emi_ext, ozn_ext, exp_ext, eff_ext
100   FORMAT(t26,4(a3,3x))

      CALL skip( runfile, eof )
      runcount = 0

C=====================================================================
C  Loop over specified runs
C=====================================================================

      DO WHILE (.not. eof)

        runcount = runcount + 1
        WRITE (*,'(A,i3.3)') ' Executing run ',runcount

        READ( runfile, 200, err=1071 )
     +                   name, outname, temp1, temp2, temp3, temp4,
     +                   atmrun_ext, exprun_ext, effrun_ext
        WRITE( errfile, 200 )
     +                   name, outname, temp1, temp2, temp3, temp4,
     +                   atmrun_ext, exprun_ext, effrun_ext
200     FORMAT(t4,a8,t15,a8,t27,4(a1,5x),t52,3(a3,6x))

C=====================================================================
C  Parse filenames for module runfiles
C=====================================================================

        atmrunname = 'ATM_RUN.'//atmrun_ext
        exprunname = 'EXP_RUN.'//exprun_ext
        effrunname = 'EFF_RUN.'//effrun_ext

C=====================================================================
C  Determine entry and exit points
C  Parse filenames for communication files
C
C  Framework enters with module whose input file corresponds to the
C  first X in the runfile, exits with module whose output file corresponds
C  to the last X in the runfile, WRITEs intermediate files only IF marked
C
C  Output file PFN assumed same as input file PFN unless specified
C=====================================================================

        flagcount = 0

        eminame = name(1:len_trim(name))//'.'//emi_ext
        IF (temp1 .ne. ' ') THEN
            emiflag = .true.
            flagcount = flagcount + 1
            IF (outname .ne. '        ') name = outname
        ELSE
            emiflag = .false.
        ENDIF

        oznname = name(1:len_trim(name))//'.'//ozn_ext
        IF (temp2 .ne. ' ') THEN
            oznflag = .true.
            flagcount = flagcount + 1
            endpoint = 1
            IF (outname .ne. '        ') name = outname
        ELSE
            oznflag = .false.
            oznname = 'SCRATCH.TMP'
        ENDIF

        expname = name(1:len_trim(name))//'.'//exp_ext
        expblname = name(1:len_trim(name))//'.'//'XBL'    ! Baseline
        xageblname = name(1:len_trim(name))//'.'//'XBA'   ! BL age
        xagename = name(1:len_trim(name))//'.'//'XSA'     ! SC age

        IF (temp3 .ne. ' ') THEN
            expflag = .true.
            flagcount = flagcount + 1
            endpoint = 2
            IF (outname .ne. '        ') name = outname
        ELSE
            expflag = .false.
            expname = 'scratch.tmp'
        ENDIF

        effname = name(1:len_trim(name))//'.'//eff_ext
        effagename = name(1:len_trim(name))//'.'//'efa' ! age spec file
        IF (temp4 .ne. ' ') THEN
            effflag = .true.
            flagcount = flagcount + 1
            endpoint = 3
        ELSE
            effflag = .false.
        ENDIF

        IF ( flagcount .lt. 2 ) goto 1020        ! No action specified

C=====================================================================
C  Call respective modules
C=====================================================================

        errflag  = .false.
        returned = .false.

        IF (emiflag) THEN
          CALL solomon
          returned = .true.
        ENDIF

        IF (( endpoint .ge. 2      )  .and.
     +      ( oznflag .or. returned)  .and.
     +      ( .not. errflag        )) THEN
          CALL exposure
        ENDIF
c
       IF (( endpoint .eq. 3 )  .and.
     +      ( .not. errflag   )) THEN
         CALL effects
        ENDIF

300     CALL skip( runfile, eof )

      ENDDO

      CLOSE(errfile)
      CLOSE(runfile)


C999   return
      STOP     

1010  WRITE (*,*) 'Error 1010'      
1020  WRITE (*,*) 'Error 1020'
1070  WRITE (*,*) 'Error 1070'
1071  WRITE (*,*) 'Error 1071'



C1010  CALL error(10, 999)
C1020  CALL error(20, 300)
C1070  CALL error(70, 999)
C1071  CALL error(70, 300)
      END PROGRAM AHEF

