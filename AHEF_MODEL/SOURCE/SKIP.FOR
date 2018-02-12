C=====================================================================
      SUBROUTINE SKIP(IOUNIT,EOF)
C=====================================================================
C  Subroutine to skip comments while reading data files
C  Positions file at next data record by skipping those comment
C  records denoted by an asterisk in column 1.  The flag indicates
C  when the end of file has been reached.
C=====================================================================

      integer iounit
      logical eof
      character*1 col1
      eof = .false.

100   read(iounit,'(a1)', end=200) col1
      if (col1 .ne. '*') goto 300
      goto 100

200   eof = .true.

300   backspace iounit
      return
      end
