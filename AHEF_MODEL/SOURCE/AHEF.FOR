C=====================================================================
      PROGRAM AHEF
C=====================================================================
C  Atmospheric and Health Effects Framework          File: AHEF.FOR
C  A tool for estimating the impact of emissions of ozone-depleting
C  chemicals on human health and the environment.
C
C  This framework has three major components:
C     1. Atmospheric model:  ODS emis -> Ozone depletion
C     2. Exposure model:     Ozone depletion -> UV exposure
C     3. Effects model:      UV Exposure -> Health effects
C=====================================================================

C$DEBUG: 'D'

      include 'files.fi'
	include 'global.fi'
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

      write (*,*) 'Atmospheric and Health Effects Framework V4.0'
      write (*,*) 'ICF Incorporated, 1997'
      write (*,*)

      open(errfile, FILE = 'AHEF.ERR')
C     open(runfile, FILE = 'AHEF.RUN', status = 'OLD', ERR = 1010)
      open(runfile, FILE = 'AHEF.RUN', status = 'OLD')

      call skip( runfile, eof )
      read( runfile, 100, err=1070 )
     +                      emi_ext, ozn_ext, exp_ext, eff_ext
      write( errfile, 100 )
     +                      emi_ext, ozn_ext, exp_ext, eff_ext
100   format(t26,4(a3,3x))

      call skip( runfile, eof )
      runcount = 0

C=====================================================================
C  Loop over specified runs
C=====================================================================

      do while (.not. eof)

        runcount = runcount + 1
        write (*,'(A,i3.3)') ' Executing run ',runcount

        read( runfile, 200, err=1071 )
     +                   name, outname, temp1, temp2, temp3, temp4,
     +                   atmrun_ext, exprun_ext, effrun_ext
        write( errfile, 200 )
     +                   name, outname, temp1, temp2, temp3, temp4,
     +                   atmrun_ext, exprun_ext, effrun_ext
200     format(t4,a8,t15,a8,t27,4(a1,5x),t52,3(a3,6x))

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
        if (temp1 .ne. ' ') then
            emiflag = .true.
            flagcount = flagcount + 1
            if (outname .ne. '        ') name = outname
        else
            emiflag = .false.
        endif

        oznname = name(1:len_trim(name))//'.'//ozn_ext
        if (temp2 .ne. ' ') then
            oznflag = .true.
            flagcount = flagcount + 1
            endpoint = 1
            if (outname .ne. '        ') name = outname
        else
            oznflag = .false.
            oznname = 'SCRATCH.TMP'
        endif

        expname = name(1:len_trim(name))//'.'//exp_ext
        expblname = name(1:len_trim(name))//'.'//'XBL'    ! Baseline
        xageblname = name(1:len_trim(name))//'.'//'XBA'   ! BL age
        xagename = name(1:len_trim(name))//'.'//'XSA'     ! SC age

        if (temp3 .ne. ' ') then
            expflag = .true.
            flagcount = flagcount + 1
            endpoint = 2
            if (outname .ne. '        ') name = outname
        else
            expflag = .false.
            expname = 'scratch.tmp'
        endif

        effname = name(1:len_trim(name))//'.'//eff_ext
        effagename = name(1:len_trim(name))//'.'//'efa' ! age spec file
        if (temp4 .ne. ' ') then
            effflag = .true.
            flagcount = flagcount + 1
            endpoint = 3
        else
            effflag = .false.
        endif

        if ( flagcount .lt. 2 ) goto 1020        ! No action specified

C=====================================================================
C  Call respective modules
C=====================================================================

        errflag  = .false.
        returned = .false.

        if (emiflag) then
          call solomon
          returned = .true.
        endif

        if (( endpoint .ge. 2      )  .and.
     +      ( oznflag .or. returned)  .and.
     +      ( .not. errflag        )) then
          call exposure
        endif
c
       if (( endpoint .eq. 3 )  .and.
     +      ( .not. errflag   )) then
         call effects
        endif

300     call skip( runfile, eof )

      end do

      close(errfile)
      close(runfile)


C999   return
      stop     

1010  write (*,*) 'Error 1010'	
1020  write (*,*) 'Error 1020'
1070  write (*,*) 'Error 1070'
1071  write (*,*) 'Error 1071'



C1010  call error(10, 999)
C1020  call error(20, 300)
C1070  call error(70, 999)
C1071  call error(70, 300)
      end

