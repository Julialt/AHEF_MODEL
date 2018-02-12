C=====================================================================
      SUBROUTINE READOZONE(filename)
C=====================================================================
C  Read Dobson Unit matrix (D.U. for given latitude and month)
C=====================================================================

C$DEBUG: 'D'

      include 'files.fi'
	include 'global.fi'
c      include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
      include 'exposure.fi'

      logical eof
      character*12 filename

      errflag = .false.
C MRLM ozone debugging 10/2008
cc       write(*,*)'in readozone - filename is:',filename
C
      open(iunit, file = filename, Defaultfile=
     +'\\tsclient\C\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data\'
c     +	'C:\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data\',
     + ,status = 'OLD', 
     + err = 1030)
      write (errfile,*) 'Reading OZONE'

      call skip( iunit, eof)
cc	write(*,*)'MADE IT'
      read(iunit, *, err=1070) numlats
c mrlm write out
cc	write(*,*)'MADE IT 2'
cc	write(*,*)'numlats',numlats
c
	write(errfile,*)'numlats = ',numlats
c
      call skip( iunit, eof)
      read(iunit,*) (lats(ilat), ilat = 1,numlats)

      call skip( iunit, eof)

      do while (.not. eof)

        do imonth = 1, 12

cc	 write(errfile,*)'imonth = ',imonth
c for 110 format for no cntl.ozn
c	if (filename.eq.'NOCNTL.OZN') then 
c for the following, use 120 format
	if ((filename.eq.'MA98N2.OZN').or.
     +    (filename.eq.'MP1987.OZN').or.
     + (filename.eq.'MP2007.OZN').or.
     + (filename.eq.'MPAMEN.OZN').or.
     + (filename.eq.'MA98O2.OZN').or.
     +	 (filename.eq.'WMOA1C.OZN').or.
     +	 (filename.eq.'WMOMPC.OZN').or.
     +	 (filename.eq.'WMONOC.OZN').or.
     + (filename.eq.'WMO_NP.OZN').or.
     + (filename.eq.'WMO_A1.OZN').or.
c WMO baseline
     +     (filename.eq.'BASELINE.OZN').or.
c     +     (filename.eq.'MA98NR.OZN').or.
     +     (filename.eq.'EP1NvB.OZN').or.
     +    (filename.eq.'MA98NU.OZN'))	  then
cc		write(*,*)'non control ozone'
ccx		read(iunit,110,err=1070) iyear,adummy,
ccx     +          (dobson(iyear,imonth,ilat), ilat=1, numlats)
ccx         write(errfile,110) iyear, adummy,
ccx     +          (dobson(iyear,imonth,ilat), ilat=1, numlats)
		read(iunit,120,err=1070) iyear,adummy,
     +          (dobson(iyear,imonth,ilat), ilat=1, numlats)
         write(errfile,120) iyear, adummy,
     +          (dobson(iyear,imonth,ilat), ilat=1, numlats)
	end if
c commented out below - since using the WMO baseline
cx	if (filename.eq.'BASELINE.OZN') then
c	.or.
c     +	(filename.eq.'MA98N2.OZN')) then
c          read(iunit,100,err=1070) iyear, adummy,
c		read(iunit,*,err=1070) iyear, adummy,
c     +          (dobson(iyear,imonth,ilat), ilat=1, numlats)
c         write(errfile,100) iyear, adummy,
c     +          (dobson(iyear,imonth,ilat), ilat=1, numlats)
c MRLM FOR TESTING!!!!!  REMOVE!!!!!  
c  100		  format(1x,i4,a3,3(f6.2))
120		format(1x,i4,4x,a3,3(f13.3))
110       format (i4,8x,a3,3x,3(f6.2,2x))
  100       format (1x,i4,4x,a3,100(:,f12.2)) ! NOTE: for reading decimal
C This format statement is for scientific notation
c 100       format (1x,i4,4x,a3,100(:,e12.4))
c	end if
          call skip( iunit, eof )

        end do

        if ((yrlo .lt. 1) .or. (iyear .lt. yrlo)) then
          yrlo = iyear
        endif
c	mrlm write out iyear
c	write(*,*)'iyear =',iyear
c

      end do
c	mrlm test
c	write(*,*)'iyear = ',iyear
c	write(*,*)'imonth = ',imonth
c	write(*,*)'ilat = ',ilat
c	write(*,*)'dobson of 1,1,1',dobson(1985,1,1)
c	write(*,*)'dobson of 1,12,1',dobson(1985,12,1)
c	write(*,*)'dobson of 10,12,3',dobson(2050,12,3)
c
      if ((yrhi .lt. 1) .or. (iyear .gt. yrhi)) then
        yrhi = iyear
      endif

999   close(iunit)
      return

1030  call error(30,*999)
1070  call error(70,*999)
      end

