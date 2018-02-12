	Subroutine Read_CTYLAT
c  MRLM - to read in county file with state/coounty fip and the associated latitude
c  at the county centroid location
c18959\Desktop\AHEF_Runs_2014\
	include 'files.fi'
	include 'global.fi'
	include 'exposure.fi'
c	include 'C:\Users\18959\Desktop\AHEF_Runs_2014\ahef\run group r1\
c     +source\files.fi'
c      include 'C:\Users\18959\Desktop\AHEF_Runs_2014\ahef\run group r1\
c     +source\global.fi'
c
c      include 'C:\Users\18959\Desktop\AHEF_Runs_2014\ahef\run group r1\
c     +source\exposure.fi'
c
	integer lat
c
	open(unit=67,file="cntyfip_g1.txt", status="old")
c
	call skip(67, eof )
c
	do icty = 1, numcty  ! of while not end of file
142		read(67,188)cty_fip(icty),cty_lat(icty)
c FOR ONE COUNTY RUN

c	if (cty_fip(icty).eq.1001) then
c	 write(*,*)' right one = ',cty_lat(icty)
c	else
c	  goto 142
c	end if
c	    
 188	format(I5,1x,f9.6)
c
c	close (67)
	end do
	close(67)
c
	return 
	end
		