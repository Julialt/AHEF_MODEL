C=====================================================================
      SUBROUTINE READ_POPULATION(filename)
C=====================================================================
C   Read population definition file
C=====================================================================

      include 'files.fi'
c      include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
	include 'global.fi'
      include 'effects.fi'

c mlrm
	character*5 ctypl
c
      character*12 filename
      character tempchar
      logical eof, expinterp, last
      integer col, year, lastyear, yrlp
      real totpop(minyear:maxyear), temp
      real regbrk(minyear:maxyear, maxlats)
      real popbrk(minyear:maxyear, 6)
      real agebrk(minyear:maxyear, maxages, 6)
c
	real popt(minyear:maxyear, maxages, numcty, 6)
	integer ctyint
c
      filename = filename(1:len_trim(filename))//'.POP'
      open(iunit, file = filename, status = 'OLD', err = 1090)
      write(errfile,*) 'Reading Population'

cc      write(*,*) 'Entered read_pop'
cc	write(*,*) 'num cty = ',numcty
c
	expinterp = .true.
c
      pophi = 0
      poplo = 0
c mlrm add
	popminyr = 1985
c for testing
	popmaxyr = 2025
c	popmaxyr = 2005	
c
	yrstep = 5
C=====================================================================
C   Read total population and regional breakout
C=====================================================================

cc      write(*,*) 'read_pop 1'

cmrlm      call skip( iunit, eof )

      last = .false.
      lastyear = 0

cc      write(*,*) 'read_pop 1b'

      do while (.not. last)
          do iyear = 1,((popmaxyr-popminyr)/yrstep)+1

	      do icty = 1,numcty

		year = popminyr + (iyear -1)*yrstep
c110     format(t4,i4,t11,18(i9))
cc	write(*,*)'year - ',year,iage,icty, maxages
c
		read (iunit,*)year,ctyint, 
     +		(popt(year,iage,icty,1), iage=1,maxages),
     +		(popt(year,iage,icty,2), iage=1,maxages),
     +		(popt(year,iage,icty,3), iage=1,maxages),
     +		(popt(year,iage,icty,4), iage=1,maxages),
     +		(popt(year,iage,icty,5), iage=1,maxages),
     +		(popt(year,iage,icty,6), iage=1,maxages)
c
c  convert character string to integer for state/county fip
c
cc	  	cty(icty)=ichar(ctypl(5:5))-48
cc	    cty(icty)= cty(icty)+ (ichar(ctypl(4:4))-48)*10
cc	    cty(icty)=cty(icty)+(ichar(ctypl(3:3))-48)*100
cc	    cty(icty)=cty(icty)+(ichar(ctypl(2:2))-48)*1000
cc	    cty(icty)=cty(icty)+(ichar(ctypl(1:1))-48)*10000
c
cc	write(*,*)'icty,iyear,year = ',icty,iyear, year
c------------ match up counties
		do ii=1,522     !CHANGE BY REGION!!!!!!!!!!
			if (ctyint.eq.(cty_fip(ii))) then
				do iage=1,maxages
					pop(year,iage,ii,1)=popt(year,iage,icty,1)
     +		+ popt(year,iage,icty,5)
					pop(year,iage,ii,2)=popt(year,iage,icty,2)
     +        + popt(year,iage,icty,6)
					pop(year,iage,ii,3)=popt(year,iage,icty,3)
					pop(year,iage,ii,4)=popt(year,iage,icty,4)
c					pop(year,iage,ii,5)=popt(year,iage,icty,5)
c					pop(year,iage,ii,6)=popt(year,iage,icty,6)
				end do
				go to 677
			end if
		end do

c
c--------------------- interpolate between years -------------
677     if ((lastyear .ne. 0) .and. (year .gt. (lastyear + 1))) then
          do yrlp = lastyear + 1, year - 1
            if (expinterp) then    ! exponential or linear interp
            do ipop=1,maxpops
              do iage=1, maxages
c	
cc	if ((icty.eq.1).and.(lastyear.eq.2005)) then
cc	write(*,*)lastyear,year,iage,icty,ipop,
cc     + pop(lastyear,iage,icty,ipop),pop(year,iage,icty,ipop),
cc     + (einterp(real(pop(lastyear,
cc     +         iage,icty,ipop)), real(pop(year,iage,icty,ipop)),
cc     +         lastyear, yrlp, year))
cc	end if
c
cc	write(*,*)' interpolating...'
c
               pop(yrlp, iage, icty, ipop) = (einterp(real(pop(lastyear,
     +         iage,icty,ipop)), real(pop(year,iage,icty,ipop)),
     +         lastyear, yrlp, year))
              end do ! iage
	      end do ! ipop

            endif
          end do ! yrlp
        endif
c------------------------------------------------------------
            end do ! icty
c
        if (lastyear .eq. 0) poplo = year
        lastyear = year
c
cc	write(*,*)'last year, poplo =',lastyear,poplo,year
c
        end do ! iyear

        call check(iunit,last)
      end do ! not last

      pophi = year



999   close (iunit)
      return

1070  call error(70,*999)
1090  call error(90,*999)
1100  call error(100,*999)
      end

