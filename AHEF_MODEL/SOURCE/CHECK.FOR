C=====================================================================
      SUBROUTINE CHECK(IOUNIT,LAST)
C=====================================================================
C  Subroutine to check if we've reached the end of
C  an input block
C=====================================================================

      integer iounit
      logical last
      character*1 col1

      last = .false.

100   read(iounit,'(a1)', end=150) col1
      if (col1 .ne. '*') goto 200
150   last = .true.

200   backspace iounit
      return
      end
