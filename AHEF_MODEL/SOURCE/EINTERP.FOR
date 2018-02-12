C=====================================================================
      REAL FUNCTION EINTERP (VALU1,VALU2,IDX1,IDX2,IDX3)
C=====================================================================
C     Exponential Interpolation function
C=====================================================================

      include 'files.fi'

      real valu1,valu2,tmp1,tmp2
      integer idx1,idx2,idx3

	if (valu1.ne.0) then
      tmp1 = log(valu1)
	else
	tmp1 = 0
	endif

	if (valu2.ne.0) then
      tmp2 = log(valu2)
	else
	tmp2 = 0
	endif
c
      if (idx3 .ne. idx1) then
        einterp = exp( tmp1 + (tmp2-tmp1) *
     +                        (idx2-idx1+0.0)/(idx3-idx1) )
      else
        einterp = 0
        write (errfile,*) 'Interpolation Error'
      endif
c mlrm 2/2009 testing
c	If ((idx1.eq.1985).and.(idx2.eq.1986).and.(idx3.eq.1990)) then
c	write(99,*)'valu1 valu2 idx1 idx2 idx3', valu1, valu2, idx1, idx2, idx3
c	write(99,*)'tmp1 tmp2 = ', tmp1, tmp2
c	write(99,*)'later = ', (idx2 - idx1 + 0) / (idx3 - idx1)
c	write(99,*)'in paran =', 
c    +	tmp1 + (tmp2 - tmp1) * (idx2 - idx1 + 0) / (idx3 - idx1)
c	write(99,*)'einterp = ', einterp
c	End If

      return
      end

