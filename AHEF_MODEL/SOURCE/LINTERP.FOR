C=====================================================================
      REAL FUNCTION XINTERP (VALU1,VALU2,IDX1,IDX2,IDX3)
C=====================================================================
C     Linear Interpolation function
C=====================================================================

      include 'files.fi'

      real valu1,valu2
      integer idx1,idx2,idx3

      if (idx3 .ne. idx1) then
        xinterp = valu1 + (valu2-valu1) * (idx2-idx1+0.0)/(idx3-idx1)
      else
        xinterp = 0
        write (errfile,*) 'Interpolation Error'
      endif

      return
      end

