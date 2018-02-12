	Subroutine Read_CTYLAT
c  MRLM - to read in county file with state/coounty fip and the associated latitude
c  at the county centroid location
c
      include 'files.fi'
      include 'global.fi'
      include 'exposure.fi'
c
	integer lat
c
	open(unit=67,file="stcntyfip_lat.txt", status="old")
c
	call skip(67, eof )
c
	do icty = 1, numcty  ! of while not end of file
142		read(67,188)cty_fip(icty),cty_lat(icty)
c FOR ONE COUNTY RUN

	if (cty_fip(icty).eq.1001) then
	 write(*,*)' right one = ',cty_lat(icty)
	else
	  goto 142
	end if
c	    
 188	format(I5,1x,f9.6)
c
	close (67)
	end do
c
	return 
	end
		