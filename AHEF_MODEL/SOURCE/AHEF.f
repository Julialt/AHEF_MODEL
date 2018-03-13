C=====================================================================
      PROGRAM AHEF
C=====================================================================
C  Atmospheric and Health Effects Framework          File: AHEF.f
C  A tool for estimating the impact of emissions of ozone-depleting
C  chemicals on human health and the environment.
C
C  This framework has three major components:
C     1. Atmospheric model:  ODS emis -> Ozone depletion    : AHEF.f
C     2. Exposure model   :  Ozone depletion -> UV exposure : exposure.f
C     3. Effects model    :  UV Exposure -> Health effects  : effects.f
C
C=====================================================================
C PURPOSE OF THIS PROGRAM UNIT:
C     - read run file AHEF.RUN
C     - extract names of input files
C     - extract flags for simulation type
C     - based on flags, call exposure or effects subroutine
C     - pass file names to subroutines
C
C=====================================================================
C NAMES USED IN THIS PROGRAM UNIT:
C
C FILENAMES:   
C IN/OUT UNIT          WHERE_USED           DESCRIPTION
C ------
C INPUT  iunit(30)     readozone.f          DU matrix for lat/month
C    oznname    = name(1:len_trim(name))//'.'//ozn_ext
C INPUT  exprun(50)    exposure.f           exposure run file (input pointers)
C    exprunname = 'EXP_RUN.'//exprun_ext
C INPUT  effrun(60)    effects.f            effects run file (input pointers)
C    effrunname = 'EFF_RUN.'//effrun_ext
C ------
C Both the following are also OUTPUT: as expfilename on ounit(40)
C    written by routine writexage.f (exposure model)
C             then used as input to (effects model)
C INPUT  scratchage(71) read_exposure_age.f age exposure scratch file
C    xagename   = name(1:len_trim(name))//'.'//'XSA'
C INPUT  scratchagebl(81) read_blexposure_age.f b/l age exposure file
C    xageblname = name(1:len_trim(name))//'.'//'XBA 
C ------
C OUTPUT ounit(40)     write_eff_agg_age.f  age spec file
C    effagename = name(1:len_trim(name))//'.'//'efa'
C ------
C APPARENTLY UNUSED scratchbl(80) effects.f baseline exposure scratch file
C    expblname  = name(1:len_trim(name))//'.'//'XBL'
C ------
C (following names are defined but not currently used)
C    atmrunname = 'ATM_RUN.'//atmrun_ext
C    effname = name(1:len_trim(name))//'.'//eff_ext
C    eminame = name(1:len_trim(name))//'.'//emi_ext
C    expname = name(1:len_trim(name))//'.'//exp_ext -or- scratch.tmp
C
C OTHER CHARACTER VARIABLES:
C    outname = read from AHEF.RUN on unit runfile(10)
C    name    = outname
C
C    emi_ext, ozn_ext, exp_ext, eff_ext = file extensions in AHEF.RUN
C    atmrun_ext, exprun_ext, effrun_ext = file extensions in AHEF.RUN
C NB: currently using only ozn_ext, atmrun_ext, exprun_ext, effrun_ext
C
C    temp1,2,3,4 = io selector " "/"X" for each file type in AHEF.RUN
C
C LOGICAL VARIABLES:
C    errflag initialised T, enabling CALL effects or exposure
C    following are readin fields from AHEF.RUN : set input filenames
C    emiflag = T IF (temp1 .ne. ' ') THEN -> CALL solomon
C    oznflag = T IF (temp2 .ne. ' ') THEN -> set o3 filename
C    expflag = T IF (temp3 .ne. ' ') THEN -> set modules = 2
C    effflag = T IF (temp4 .ne. ' ') THEN -> reset modules = 3
C    returned = T if we came back from Solomon
C             (enables bypassing atmos model)
C
C INTEGER VARIABLES:
C    i = 1,12
C    flagcount = # of TRUE flags
C    runcount  = # of requested runs
C    modules = 2 -> exposure only; = 3 -> also effects
C    ...formerly designated "endpoint" but that caused a type conflict
C
C REAL VARIABLES:
C    (none)
C
C=====================================================================
      !USE setup

      IMPLICIT NONE

      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'setup.h'

      INTEGER flagcount, modules

      CHARACTER*1 temp1,temp2,temp3,temp4
      CHARACTER*3 emi_ext, ozn_ext, exp_ext, eff_ext
      CHARACTER*3 atmrun_ext, exprun_ext, effrun_ext
      CHARACTER*8 name, outname

      DATA (monthname(i), i=1, 12) / 'Jan', 'Feb', 'Mar', 'Apr',
     +      'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /

C=====================================================================
C  Initialize flags
      eof = .false.
      errflag = .false.
      emiflag = .false.
      oznflag = .false.
      effflag = .false.
      expflag = .false.
C  Initialize counters
      flagcount = 0
      runcount = 0
C=====================================================================
C  Open global runfile and global default names and extensions
C=====================================================================

      WRITE (*,*) 'Atmospheric and Health Effects Framework V4.0'
      WRITE (*,*) 'ICF Incorporated, 1997'
      WRITE (*,*)

      OPEN(errfile,FILE=dir_io//'AHEF.ERR')
      OPEN(runfile,FILE=dir_io//'AHEF.RUN',status='OLD',ERR = 1010)
C      OPEN(runfile,FILE=dir_io//'AHEF.RUN',status = 'OLD')

      CALL skip( runfile, eof )

! read file extensions : table header line from file AHEF.RUN
      READ(runfile,100,err=1070) emi_ext, ozn_ext, exp_ext, eff_ext
      WRITE(errfile,100)         emi_ext, ozn_ext, exp_ext, eff_ext

100   FORMAT(t26,4(a3,3x))

      CALL skip(runfile,eof)

      runcount = 0
      
C=====================================================================
C  Loop over runs specified in AHEF.RUN
C=====================================================================

      DO WHILE (.NOT. eof)

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
C  to the last X in the runfile, writes intermediate files only if marked
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
            modules = 1
            IF (outname .ne. '        ') name = outname
        ELSE
            oznflag = .false.
            oznname = 'SCRATCH.TMP'
        ENDIF
        WRITE(*,*)oznname

        expname = name(1:len_trim(name))//'.'//exp_ext    ! Exposure
        expblname = name(1:len_trim(name))//'.'//'XBL'    ! Baseline exposure
        xagename = name(1:len_trim(name))//'.'//'XSA'     ! SC expos by age 
        xageblname = name(1:len_trim(name))//'.'//'XBA'   ! BL expos by age

        IF (temp3 .ne. ' ') THEN
            expflag = .true.
            flagcount = flagcount + 1
            modules = 2
            IF (outname .ne. '        ') name = outname
        ELSE
            expflag = .false.
            expname = 'scratch.tmp'
        ENDIF

        effname = name(1:len_trim(name))//'.'//eff_ext
        !effagename = name(1:len_trim(name))//'.'//'EFA' ! age spec file
        IF (temp4 .ne. ' ') THEN
            effflag = .true.
            flagcount = flagcount + 1
            modules = 3
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

        IF (( modules .GE. 2      )  .and.
     +      ( oznflag .OR. returned)  .and.
     +      ( .NOT. errflag        )) THEN
          CALL exposure
!           WRITE(*,*) "bypassing exposure module for DEBUG"
        ENDIF
c
       IF (( modules .EQ. 3 )  .and.
     +      ( .NOT. errflag   )) THEN
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

