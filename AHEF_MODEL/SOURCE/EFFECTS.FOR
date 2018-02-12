C=====================================================================
      SUBROUTINE EFFECTS
C=====================================================================
C   This subroutine contains the effects model.
C   Input:    exposure by cohort,age,latitude,measure
C   Output:   cases by year,measure
C   Modified to compute incidence for scenarios using BAFs and baseline
C   incidence
C=====================================================================

      include 'files.fi'
	include 'global.fi'
c      include 'C:\Documents and Settings\18959\Desktop\AHEF\
c     +countyAHEF\miniruns\run group 1\global.fi'
      include 'effects.fi'

      logical eof, first, byyear, byage
      integer count,yrlplo,yrlphi,coh1,coh2,iy,iageg
      integer cohyrtmp,colotmp,cohitmp,ilattmp
	integer rlat
      character*12 age_pfn, cohort_pfn, coeff_pfn, popname
      character*12 endpttmp
      character*3  baftype
      character*4  drtmp
      real coh_wght,tmp1,tmp2,wtd_incid, cmflag
      real expwgt(maxages*step)
      integer year, poptmp
c
	real st_tot_base(15,maxpops)  ! by 15 state, 4 pop
	real st_tot_scen(15,maxpops)
	real st_tot_diff(15,maxpops)
	real st_coh_scen(15,4,maxpops)
	real st_coh_base(15,4,maxpops)
	real st_coh_diff(15,4,maxpops)
	real st_tot_b(15)  ! by 15 state, 4 pop
	real st_tot_s(15)
	real st_tot_d(15)
c 
	real ct_tot_diff_l(numcty), ct_tot_diff_d(numcty)
c
	integer group1(15)
c	data group1 /1,5,12,22,28,29,40/
	data group1 /9,10,11,23,24,25,33,34,36,39,42,44,50,51,54/
	integer ireg  ! total number of states for casest counter
c
	ireg = 15
c
      count = 1
      cases = 0
      total = 0
	numreg = 3
      first = .true.
cc	mrlm - debug
cc	write(*,*)'debug ',expage(2,3,1)
	write(*,*)'**************  lats(1) = ',lats(1)
c
      write (*,1)
1     format ('+','Running effects model  . . . .')

      open(scratch, file = expname)     ! open exposure file

      open(scratchbl, file = expblname) ! open baseline exposure file

      open(scratchage, file = xagename)     ! open age exposure file

      open(scratchagebl, file = xageblname) ! open baseline age exposure file
cc	mrlm debug 10/2008
	write(*,*)'exposure file = ',expname
	write(*,*)'baseline exposure file = ',expblname
	write(*,*)'age exposure file =',xagename
	write(*,*)'baseline age exposure file =',xageblname
	write(*,*)'effagename = ',effagename

      open(effrun, file = effrunname, status = 'OLD', err = 1080)
      write (errfile,*) 'Reading effects runfile'

      call skip(effrun, eof)
      read( effrun, 100 ) popname, outputlo, outputhi 
100   format(t19,a8,t39,i4,t47,i4)

      call skip(effrun, eof)
      read( effrun, 101 ) tempchar
      if ((tempchar .eq. 'Y') .or. (tempchar .eq. 'y') .or.
     +    (tempchar .eq. 'T') .or. (tempchar .eq. 't') .or.
     +    (tempchar .eq. 'X') .or. (tempchar .eq. 'x')) then
        byyear = .true.
      else
        byyear = .false.
      endif

      call skip(effrun, eof)
      read( effrun, 101 ) tempchar
      if ((tempchar .eq. 'Y') .or. (tempchar .eq. 'y') .or.
     +    (tempchar .eq. 'T') .or. (tempchar .eq. 't') .or.
     +    (tempchar .eq. 'X') .or. (tempchar .eq. 'x')) then
        byage = .true.
      else
        byage = .false.
      endif

101   format(t23,a1)

      write (*,*) 'Reading population...'
C MRLM  10/2008 debug
	write(*,*)' population file is:',popname

      call read_population(popname)

      if (errflag) goto 999

      write (*,*) 'Returned from read_pop...'

C**  Write population to an output file **
c
		write(*,*)'numcty = ',numcty
c
      open(o1unit, file='popout.txt')

        do year = 1985, 2025
          do ipop = 1, maxpops
            poptmp=0
            do iage=1, maxages
 	        do icty = 1, numcty
              poptmp = poptmp + pop(year,iage,icty,ipop)
c mlrm debug between codes - ---------
c		if ((icty.eq.1).and.(ipop.eq.1).and.(year.eq.2005)) then
c		write(99,*)'year,iage,icty,ipop',year,iage,icty,ipop
c		write(99,*)' pop = ',pop(year,iage,icty,ipop),'  ',poptmp
c		end if
c--------------------------------------
            end do ! icty
		 end do ! iage
          write(o1unit,125) year,poptmp
        end do ! ipop
      end do ! year
c
cc	write(*,*)'********pop', pop(2005,1,1,1)
c
120   format(t2,a5,t7,i2,t12,a5,t17,i2)
125   format(t2,i4,t8,i12)
      close(o1unit)

      call skip(effrun, eof)

      do while (.not. eof)
        read( effrun, 200 ) endpoint,index,age_pfn,cohort_pfn,coeff_pfn,
     +          drtype
        write(errfile, 200 ) endpoint,index,age_pfn,cohort_pfn,
     +          coeff_pfn,drtype
200     format(t5,a8,t19,a8,t33,a8,t47,a8,t61,a8,t75,a4)

        cohortname = cohort_pfn(1:len_trim(cohort_pfn))//'.COH'
        coeffname  = coeff_pfn(1:len_trim(coeff_pfn))//'.COE'
        agename  = age_pfn(1:len_trim(age_pfn))//'.AGE'


        write (*,*) 'Reading exposure by age...'
        call read_exposure_age(index)
        call read_blexposure_age(index)

C=====================================================================
C  Calculate incidence using BAFs as a percentage change in
C  incidence for a given percent change in exposure
C=====================================================================
C  Read BAFs
C=====================================================================

        open(iunit, FILE = 'BAF.DAT',Defaultfile=
     +'\\tsclient\C\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data')
c     +	'C:\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data')
        call skip(iunit, eof)

        read(iunit, 215) baftype
215     format(t26,a3)
        call skip(iunit, eof)

        write(*,'(a8,t9,a4)') 'drtype=', drtype
        write(*,'(a10,t12,a12)') 'endpoint=', endpoint

        do while (.not. eof)
          read (iunit, 220) endpttmp,drtmp,(baf(ipop), ipop=1,maxpops)

        write(*,'(a8,t12,a4)') 'drtmp=', drtmp
        write(*,'(a10,t12,a12)') 'endpttmp=', endpttmp

220       format(t3,a12,t17,a4,t28,30(:,f6.4,4x))

          if ((endpttmp .eq. endpoint) .and. (drtmp .eq. drtype))
     +    goto 230

        end do ! while loop

230     continue

        close(iunit)
c
C=====================================================================
C  Read Exposure Weights
C=====================================================================

        open (iunit,file='expwgts.dat',Defaultfile=
     +'\\tsclient\C\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data')
c     +	'C:\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data')

        call skip (iunit, eof)

        do while (.not. eof)

          read (iunit, 240)  endpttmp

240     format (t3,a12)

          read (iunit, 250) (expwgt(iagey), iagey=1, maxages*step)
          call skip (iunit, eof)

250     format (t8,f4.2)

          if (endpttmp .eq. endpoint) goto 260

        end do !  while loop

260     continue

        close (iunit)

C=====================================================================
C  Read Baseline Incidence
C=====================================================================

        open(iunit, FILE = 'baseinc.txt',Defaultfile=
     +'\\tsclient\C\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data')
c     +	'C:\Users\18959\Desktop\AHEF_Runs_2014\ahef\input data')
        call skip( iunit, eof )
        read(iunit, 325) colotmp, cohitmp
        call skip( iunit, eof )

        do while (.not. eof)

c        do ilat = 1, numlats
c        do ipop = 1, maxpops
		do ilat =1,3
		do ipop=1,4

          read(iunit, 330) endpttmp
          read(iunit, 332) ilattmp
          read(iunit, 334) popseg
          read(iunit, 336) adummy   ! Skip a line

          if ((popseg .eq. 'WH MALE').and.(ipop .ne. 1)) then
            ! make an error for this
          else if ((popseg .eq. 'WH FEMALE').and.(ipop .ne. 2)) then
            ! make an error for this
          else if ((popseg .eq. 'NWH MALE').and.(ipop .ne. 3)) then
            ! make an error for this
          else if ((popseg .eq. 'NWH FEMALE').and.(ipop .ne. 4)) then
            ! make an error for this
          else
            ! make an error for this
          endif

305       read (iunit, 340) cohyrtmp
            if (cohyrtmp.lt.colo_year) goto 305

              backspace(iunit)

          do icohort = colo, ((cohitmp-colo_year)/step)+1
C --------------------------------------
C  Debugging tool
c               write (*,'(t1,a7,t9,i3,t13,a4,t18,i3)')
c    +                'cohort=', icohort, 'age=', iage
c               write (*,'(t1,a4,t9,i3,t13,a4,t18,i3)')
c    +                'lat=', ilat, 'pop=', ipop
C --------------------------------------

              read(iunit, 310)  (incid_bl(icohort,iage,ilat,ipop),
     +              iage=1, maxages)

310           format(t5,50(:,4x,e8.2))
312           format(t5,1p,50(:,4x,e8.2))

          end do ! icohort

        end do ! ipop
        end do ! ilat

          if (endpttmp .eq. endpoint) goto 350

        end do ! while loop

325       format(t47,i4,t55,i4)
330       format(t1,a8)
332       format(t5,i1)
334       format(t1,a12)
336       format(t1,a3)
340       format(t1,i4)


350     continue
c
        close(iunit)
C=====================================================================
C Print baseline and scenario incidence matrices and the difference between
C them.
C NOTE: This routine changed to print only for ipop=1 (whm), in order to
C       limit size of the AHEF.ERR file.
C=====================================================================
c
cc
       write (errfile, '(A)') '==========================='
       write (errfile,*) endpttmp
       write (errfile, '(A)') 'Baseline incidence matrices'

        do ilat = 1, numreg
          do ipop = 1, 1  ! (Just whm, use maxpops for all ipops)
            do icohort = colo, cohi

               write (errfile, 500) "lat=", ilat, "pop=", ipop
               write (errfile, 502) (incid_bl(icohort,iage,ilat,ipop),
     +                       iage = 1, maxages)

            end do ! icohort
          end do ! ipop
        end do ! ilat

500    format (t1,a4,t5,i3,t9,a5,t14,i3)
502    format(t5,1p,50(:,4x,e8.2))


C==========================================================================
C  Calculate cases using exposure by age information
C==========================================================================

C=============================================
C  Make incidence baseline matrix by age
C=============================================
c	mrlm
c	write(*,*)'numreg = ',numreg
c	write(*,*)'maxpops = ',maxpops
c	write(*,*)'maxages = ',maxages
c	write(*,*)'poplo = ',poplo
c	write(*,*)'pophi = ',pophi
c	write(*,*)'step = ',step
c

        do ilat = 1, numreg

          do ipop = 1, maxpops

            do icohort = colo, cohi

              do iage = 1, maxages

                do iy = 1, 5

                  iageall = (iage - 1) * step + iy

                  incage_bl(icohort,iageall,ilat,ipop) =
     +              incid_bl(icohort,iage,ilat,ipop)
c
c	if ((ilat.eq.2).and.(iage.eq.12).and.(iageall.eq.56)) then
c	write(errfile,*)'icohort,iageall,iage,ilat,ipop',icohort,iageall,
c     +  iage,ilat,ipop,
c	write(errfile,*)'   incid,  incage_bl=',
c     +  incid_bl(icohort,iage,ilat,ipop),	
c     + incage_bl(icohort,iageall,ilat,ipop)
c	end if
c
                end do ! iy

              end do ! iage

            end do ! icohort

          end do ! ipop

        end do ! ilat

C=========================================
C  Write it to the Errfile (lat and pop=1)
C=========================================
c mrlm debug
cc	write(errfile,*)' ****************************************'
c
        do icohort = colo, cohi

        write (errfile,601) icohort, (incage_bl(icohort,iagey,2,1),
     +                      iagey = 1, maxages * step)

        end do ! icohort

c       icohort=10

c        write (errfile,601) icohort, (incage_bl(icohort,iagey,2,1),
c     +                      iagey = 1, maxages * step)

601      format (i4,t5,100(:,4x,e8.2))

C==================================================================
C  Get cases by cohort and cohort time using age-specific exposures
C  NOTE this is by each age for each of the 5 ages in a cohort
C  NOTE uses real weights by age for cumulative exp's to date
C==================================================================

        if ((drtype .eq. "CMYR") .or. (drtype .eq. "CMPK")) then
           cmflag = 1
        else
           cmflag = 0
        endif

        caseca=0
        casecab=0
c
		do icty = 1,numcty

          do ipop = 1, maxpops

            do icohort = colo, cohi

             do iy = 0, 4

                expbase = 0.0

                expscen = 0.0
cc
c			i get the iagey, i would think iy loop comes into play
c			 if iagey = 1,maxages (without the step) but the iy represent
c			 the stepping through <-- question - incid_bl which is by 5 years
c			 however, here, is incage_bl which is by each year...
c
                do iagey = 1, maxages * step
c
c			 so this is for going through the 5 years around the icohort*step 
c			 so colo_year = 1890, icohort = 1:43, step = 5, iagey = 1,90, iy=5 steps
c			 maxages = 18

       iyear = colo_year + (icohort-1)*step - int(step/2) + iagey - 1
     +          + iy

                itmp = iagey + iy

cc			mrlm comment - expage(icohort,itmp,ilat) - how much exposure a person gets
c			living at that location given when you are born and how old you are now
c			 - i.e. ilat = 1 (so you live at 50 degrees North)
c			   itmp = 50 so you are 50 years old
c			   icohort = 20 so you were born in 1890+20*5 = 1990
c			   so this is equivalent to 2040 (born in 1990 + 50 years old)
c---------
c			so this next part then for a given latitude, type of population, and
c			for each birth year (icohort), it then sums up the lifetime of exposure
c			weighted by the amount of exposure respective to that age (i.e. younger
c			ages tend to get more exposure than the older ages)
c			 = ends up with total lifetime exposure for a person born in icohort year
c
                expbase = expbase*cmflag + expagebl(icohort,itmp,icty)
     +                    * expwgt(iagey)

                expscen = expscen*cmflag + expage(icohort,itmp,icty)
     +                    * expwgt(iagey)
c mrlm
c	if (((icohort.eq.20).or.(icohort.eq.40)).and.
c     +	((iagey.eq.20).or.(iagey.eq.40))) then
c		write(91,*)'icty,ipop,icohort,iy,iagey = ',icty,ipop,icohort,iy,
c     +		iagey
c		write(91,*)'exbl,exage,expwgt,cmflag=',
c     +		expagebl(icohort,itmp,icty),expage(icohort,itmp,icty),
c     +        expwgt(iagey),cmflag
c	end if
c
                iage = int(iagey/step) + 1

                tmp = mod(iagey,step)

                if (tmp .eq. 0) then
                iage = iage - 1
                endif

cc-----------------------
cNEW - determine latitude to use
			 do imatch = 1,numcty
				ictymatch = cty(imatch)
				do imatch2 = 1,numcty
				    if (ictymatch.eq.(cty_fip(imatch2))) then
						rlat = aint(cty_lat(imatch2))
						goto 801
					end if
				end do
			end do
c
 801			if ((rlat.ge.20).and.(rlat.le.30)) then
				ilat = 3
			elseif ((rlat.ge.30).and.(rlat.le.40)) then
				ilat = 2
			elseif ((rlat.ge.40).and.(rlat.le.50)) then
				ilat = 1
			end if	  
c
                casecab(icohort, itmp, icty, ipop) =
     +          casecab(icohort, itmp, icty, ipop) +
     +          incage_bl(icohort, iagey, ilat, ipop) *
     +        pop(min(max(iyear, poplo), pophi), iage, icty, ipop)
c MRLM REMOVE - for 1 county
cc     +           /step
     +         / (step*100000)
cc 
                if(baftype .eq. 'PWR') then

                caseca(icohort, itmp, icty, ipop) =
     +          caseca(icohort, itmp, icty, ipop) +
     +  incage_bl(icohort, iagey, ilat, ipop) * (1 + (BAF(ipop) *
     +         ((expscen - expbase)/expbase))) *
     +        pop(min(max(iyear, poplo), pophi), iage, icty, ipop)
c MRLM REMOVE - for 1 county
c     +           /step
     +         / (step*100000)

                else if(baftype .eq. 'EXP') then

                caseca(icohort, itmp, icty, ipop) =
     +          caseca(icohort, itmp, icty, ipop) +
     +  incage_bl(icohort, iagey, ilat, ipop) * (1 + (BAF(ipop) *
     +         (expscen - expbase))) *
     +        pop(min(max(iyear, poplo), pophi), iage, icty, ipop)
c MRLM REMOVE - for 1 county
c     +           /step
     +         / (step*100000)
                else

                  ! ERROR

                end if
c

605           Continue

                enddo ! iagey

              enddo ! iy
            enddo ! icohort
          enddo ! ipop
        enddo ! icty
C----------------------------------------------------------------------
c  MRLM for testing print out to make graphs of a 10 year old thread
c-----------
c	open(21,file="pop_10.txt",status="unknown")
c	open(22,file="scbs_exp_10.txt",status="unknown")
c	open(24,file="bsince_10.txt",status="unknown")
c	open(25,file="case_10.txt",status="unknown")
c
c	icty = 1
c	ilat = 2
c	ipop = 1
c
c	write(21,*)endpttmp
c	write(22,*)endpttmp
c	write(24,*)endpttmp
c	write(25,*)endpttmp
c
cc	write(*,*)'IS IT 5-32?',incage_bl(1,56,2,1)
c
c	do icohort = colo,cohi
c
c	iyear = colo_year + (icohort-1)*step - int(step/2) + 10 - 1
c	iyear = colo_year + (icohort-1)*step 
cc	write(*,*)'icohort, iyear =',icohort,iyear
c
c	  write(21,111)colo_year+(icohort-1)*step,
c     +	  pop(min(max(iyear, poplo), pophi),3, 
c     +	  icty, ipop)
c	 write(22,121)colo_year+(icohort-1)*step,
c     +  	  expagebl(icohort,10,icty),
c     +      expage(icohort,10,icty)
c	 write(24,*)colo_year+(icohort-1)*step,
c     +	 incage_bl(icohort,10,ilat,ipop),
c     +     icohort,ilat,ipop
c	 write(25,141)colo_year+(icohort-1)*step,
c     +	  casecab(icohort,10,icty,ipop),
c     +	  caseca(icohort,10,icty,ipop)
c	end do
c
c111	format(I4,2x,I10)
c121	format(I4,2x,2(f10.4,2x))
c131	format(I4,2x,f10.7)
c141	format(I4,2x,2(f10.7,2x))
C
C=====================================================================
C  Writes Scenario Inc/Mort by cohort, ilat, and ipop over cohort time
C=====================================================================

      if (first) then
        open(ounit, file='eachpop_state1.txt')
	  open(ounit21, file='sumpop_state1.txt')
      else
        open(ounit, file='eachpop_state1.txt', status = 'OLD',
     +       access = 'APPEND')
	  open(ounit21, file='sumpop_state1.txt',status='OlD',
     +        access='APPEND')
      endif
c
      write(ounit, 700) 'Measure:   >',endpttmp,'<'
      write(ounit, 700) 'D-RType:   >',drtmp,'<'
c
      write(ounit21, 700) 'Measure:   >',endpttmp,'<'
      write(ounit21, 700) 'D-RType:   >',drtmp,'<'
c
	do ipop=1,maxpops
	  do ii=1,ireg               
		st_tot_scen(ii,ipop)=0.0
		st_tot_base(ii,ipop)=0.0
		st_tot_diff(ii,ipop)=0.0
		st_tot_s(ii)=0.0
		st_tot_b(ii)=0.0
		st_tot_d(ii)=0.0
	  end do
	end do

          do ipop=1, maxpops

			do icty = 1,numcty

			     icomp = int(cty_fip(icty)/1000)
c
		     do ii=1, ireg
		     if (icomp.eq.group1(ii)) then
c			    icg1=icg1 + 1

				do icohort = colo,cohi
				 do iagey=1,maxages*step +4

			     st_tot_scen(ii,ipop)= caseca(icohort,iagey,icty,ipop) +
     +              st_tot_scen(ii,ipop)
		         st_tot_base(ii,ipop)=casecab(icohort,iagey,icty,ipop) +
     +	              st_tot_base(ii,ipop)
			      st_tot_diff(ii,ipop)=caseca(icohort,iagey,icty,ipop) -
     +               casecab(icohort,iagey,icty,ipop) + 
     +               st_tot_diff(ii,ipop)
c
c		if ((cty_fip(icty).eq.11001).and.(ipop.eq.1)) then
c		 if (caseca(icohort,iagey,icty,ipop).ne.0) then
c	  write(911,*)iagey,caseca(icohort,iagey,icty,ipop),
c     +    casecab(icohort,iagey,icty,ipop) 
c		 end if
c	    end if
				end do
			    end do

			 end if
			 end do ! ii
c
c	if ((cty_fip(icty).eq.11001).and.(ipop.eq.1)) then
c		write(911,*)'icohort,iagey=',icohort,' ',iagey,
c     +		 caseca(icohort,iagey,icty,ipop),
c     +        st_tot_scen(ii,ipop), casecab(icohort,iagey,icty,ipop),
c     +	    st_tot_base(ii,ipop),caseca(icohort,iagey,icty,ipop) -
c     +               casecab(icohort,iagey,icty,ipop),
c     +               st_tot_diff(ii,ipop)
c	end if
c
           end do ! icty
c
	if (ipop.eq.1) then
c	 if (st_tot_base(10,ipop).eq.(0.0)) then
c	write(671,*)' here'
c	else
c	write(911,*)'FINAL =',st_tot_base(10,ipop),st_tot_scen(10,ipop),
c     +    st_tot_diff(10,ipop)
c	 end if
	end if
c
      end do ! ipop

c
c	write(*,*)' summing up for state: total counties = ',icg1
c             
	  do ipop=1,maxpops
 	    write(ounit,*)'Population =',ipop
		do ii=1,ireg
		 write(ounit,*)group1(ii),st_tot_base(ii,ipop),
     +       st_tot_scen(ii,ipop),st_tot_diff(ii,ipop)
           end do ! ii
	  end do !ipop

        close(ounit)
c
c add up for all populations
	 do ii=1,ireg
	  do ipop=1,maxpops
		st_tot_b(ii) = st_tot_base(ii,ipop)+st_tot_b(ii)
		st_tot_s(ii) = st_tot_scen(ii,ipop)+st_tot_s(ii)
		st_tot_d(ii) = st_tot_diff(ii,ipop)+st_tot_d(ii)
	  end do
	 end do
c 	    write(ounit21,*)'Population =',ipop
	  do ii=1,ireg
		 write(ounit21,*)group1(ii),st_tot_b(ii),
     +       st_tot_s(ii),st_tot_d(ii)
        end do ! ii

        close(ounit21)

c
c---------------------------------------------------------------------
c  write out by population by state summed up for cohort groups
c	1890-1980; 1985-2010; 2015-2050; 2055-2100
c
      if (first) then
        open(ounit31, file='eachpop_state_cohort1.txt')
      else
        open(ounit31, file='eachpop_state_cohort1.txt', status = 'OLD',
     +       access = 'APPEND')
      endif
c
      write(ounit, 700) 'Measure:   >',endpttmp,'<'
      write(ounit, 700) 'D-RType:   >',drtmp,'<'
c	
	do ii = 1,ireg
	  do ic = 1,4
	    do ipop = 1,maxpops
  		   st_coh_scen(ii,ic,ipop)=0.0
		end do
	  end do
	end do
c
         do ipop=1, maxpops

			do icty = 1,numcty

			     icomp = int(cty_fip(icty)/1000)
c
		     do ii=1, ireg
		     if (icomp.eq.group1(ii)) then
c			    icg1=icg1 + 1

				do icohort = colo,cohi
c
				  iyear = 1890+ (icohort-1)*5
				  if (iyear.le.1980) then   ! 1890 - 1980
						ic=1 
				  elseif (iyear.le.2010) then    ! 1985-2010
						ic =2
	              elseif (iyear.le.2050) then    ! 2015-2050
					    ic = 3
	              elseif (iyear.le.2100) then    ! 2055-2100
	                     ic = 4
	              else
					write(*,*)' ERROR in WRITING OUT line 704'
	              end if
c
				 do iagey=1,maxages*step +4

			     st_coh_scen(ii,ic,ipop)= caseca(icohort,iagey,icty,ipop) +
     +              st_coh_scen(ii,ic,ipop)
		         st_coh_base(ii,ic,ipop)=casecab(icohort,iagey,icty,ipop) +
     +	              st_coh_base(ii,ic,ipop)
			      st_coh_diff(ii,ic,ipop)=caseca(icohort,iagey,icty,ipop) -
     +               casecab(icohort,iagey,icty,ipop) + 
     +               st_coh_diff(ii,ic,ipop)
c
				end do  ! iagey
			    end do  ! icohort

			 end if  ! icomp
			 end do ! ii
c
           end do ! icty
c
      end do ! ipop	
c
	 do ic = 1,4  ! 4 cohort groups
	  do ipop=1,maxpops   ! 6 population types
 	    write(ounit31,*)'Population =',ipop
		write(ounit31,*)'Cohort group = ',ic
		do ii=1,ireg
		 write(ounit31,*)group1(ii),st_coh_base(ii,ic,ipop),
     +       st_coh_scen(ii,ic,ipop),st_coh_diff(ii,ic,ipop)
           end do ! ii
	  end do !ipop
	end do
c
        close(ounit31)
c

c        do ipop=1, maxpops
c		do icty=1,numcty

c      write(ounit, 710) ipop, icty
c
C DECOMMENT THE FOLLOWING FOR DETAILED COHORT-TIME SCENARIO MATRIX

c               do icohort=colo,cohi

c                    write(ounit, 604) icohort,
c     +				  (colo_year+(icohort-1)*step),
c     +                  (caseca(icohort,
c     +                  iagey, icty, ipop), iagey=1,maxages * step+4)
c               end do ! icohort
c             end do ! icty
c        end do ! ipop


603       format (3x,i4)
604       format (i4,2x,i4,100(:,f14.6))

c        close(ounit)
c-------------------------------------------------------------------
c by county
c------------------------------------------------------------------
      if (first) then
	  open(ounit441, file='sum_cty1.txt')
      else
        open(ounit441, file='sum_cty1.txt', status = 'OLD',
     +       access = 'APPEND')
	endif
c
	do icty=1,numcty
            
		ct_tot_diff_l(icty)=0.0
		ct_tot_diff_d(icty)=0.0

	end do

         do icty=1, numcty
			     icomp = int(cty_fip(icty)/1000)
c
		     do ii=1, ireg
		      if (icomp.eq.group1(ii)) then
c
				do icohort = colo,cohi
				 do iagey=1,maxages*step +4
		 	          do ipop = 1,maxpops
c
			if ((ipop.eq.1).or.(ipop.eq.2)) then  ! light skinned
			     ct_tot_diff_l(icty)= caseca(icohort,iagey,icty,ipop) -
     +               casecab(icohort,iagey,icty,ipop) + 
     +              ct_tot_diff_l(icty)
c	
		     end if			! dark skinned
			if ((ipop.eq.3).or.(ipop.eq.4)) then  !
c
cc	if ((icomp.eq.10).and.(ipop.eq.3)) then
cc		write(*,*)'ipop =',icty,ipop,caseca(icohort,iagey,icty,ipop)
cc	end if
c
		         ct_tot_diff_d(icty)=caseca(icohort,iagey,icty,ipop) -
     +               casecab(icohort,iagey,icty,ipop) + 
     +	              ct_tot_diff_d(icty)
			end if

				end do
			    end do
				end do

			 end if
			 end do ! ii
		end do
c
		do icty = 1,numcty
		write(ounit441,*)icty,cty_fip(icty),
     +   ct_tot_diff_l(icty),ct_tot_diff_d(icty),
     +  ct_tot_diff_l(icty)+ct_tot_diff_d(icty)
		
	end do
c
        close(ounit441)
		

C=====================================================================
C  Writes Baseline Inc/Most by cohort, ilat, and ipop over cohort time
C=====================================================================

c      if (first) then
c        open(ounit, file='casecab.txt')
c      else
c        open(ounit, file='casecab.txt', status = 'OLD',
c     +       access = 'APPEND')
c      endif

c      write(ounit, 700) 'Measure:   >',endpttmp,'<'
c      write(ounit, 700) 'D-RType:   >',drtmp,'<'

700   format(t4,a,a8,a)

c        do ipop=1, maxpops

c		do icty = 1,numcty

c      write(ounit, 710) ipop, icty
710   format(i3,3x,i3)

C DECOMMENT THE FOLLOWING FOR DETAILED COHORT-TIME BASELINE MATRIX

c               do icohort=colo,cohi
c                    write(ounit, 603) icohort
c                    write(ounit, 604) icohort,
c     +				  (colo_year+(icohort-1)*step),
c     +                  (casecab(icohort,
c     +                  iagey, icty, ipop), iagey=1,maxages * step+4)
c               end do ! icohort
c             end do ! icty
c        end do ! ipop

c        close(ounit)

C=======================================================================
C  Writes Incremental Inc/Mort by cohort, ipop, ilat, over cohort time
C=======================================================================

c      if (first) then
c        open(ounit, file='casecan.txt')
c      else
c        open(ounit, file='casecan.txt', status = 'OLD',
c     +       access = 'APPEND')
c      endif

c      write(*,'(a11)') 'casecan.txt'

c      write(ounit, 700) 'Measure:   >',endpttmp,'<'
c      write(ounit, 700) 'D-RType:   >',drtmp,'<'

c        do ipop=1, maxpops

c		do icty = 1,numcty

c      write(ounit, 710) ipop, ilat

c                do icohort=colo,cohi

c                     write(ounit, 604) (colo_year+(icohort-1)*step),
c     +                  ((caseca(icohort,
c     +                  iagey, icty, ipop)
c     +                  - casecab(icohort, iagey, icty, ipop)),
c     +                  iagey= 1, maxages * step + 4)
c                end do ! icohort
c             end do ! icty
c        end do ! ipop

c        close(ounit)


C=====================================================================
C  Calculates and Writes Total Cases by cohort, ilat, ipop
C=====================================================================
	iwrite=0
	if (iwrite.eq.1) then  ! write out the following files

      if (first) then
        open(ounit, file='tcolp.txt')
      else
        open(ounit, file='tcolp.txt', status = 'OLD',
     +       access = 'APPEND')
      endif

      write(ounit, 700) 'Measure:   >',endpttmp,'<'
      write(ounit, 700) 'D-RType:   >',drtmp,'<'

cc	write(*,*)'colo_year =',colo_year
cc
        do ipop=1, maxpops

		do icty = 1,numcty

          write(ounit, 710) ipop, icty

            do icohort = colo, cohi

              t1=0
              t2=0

              do iagey = 1, maxages * step + 4

              t1 = t1 + caseca(icohort, iagey, icty, ipop)
              t2 = t2 + casecab(icohort, iagey, icty, ipop)

              end do ! iagey

              write (ounit, 713) (colo_year + (icohort - 1) * step),
     +                         t1, t2, (t1-t2)

            end do ! icohort

713   format (i4,3(:,f14.5))

          end do ! icty
        end do ! ipop

        close(ounit)

	end if ! writing out files
C=================================================================
C  Calculates and Writes Totals by cohort for all ipops and ilats
C=================================================================

      if (first) then
        open(ounit, file='tcoall.txt')
      else
        open(ounit, file='tcoall.txt', status = 'OLD',
     +       access = 'APPEND')
      endif

      write(ounit, 700) 'Measure:   >',endpttmp,'<'
      write(ounit, 700) 'D-RType:   >',drtmp,'<'

        do icohort = colo, cohi

          tca=0
          tcb=0

          do ipop=1, maxpops

		  do icty = 1,numcty
              do iagey = 1, maxages * step + 4

              tca = tca + caseca(icohort, iagey, icty, ipop)
              tcb = tcb + casecab(icohort, iagey, icty, ipop)

              end do ! iagey
            end do ! icty
          end do ! ipop

        write(ounit, 713) (colo_year + (icohort - 1) * step),
     +                    tca, tcb, (tca-tcb)

        end do ! icohort

        close(ounit)
C====================================================
C  Call other by-year and lat write routines
C====================================================

cc       call write_eff_agg_age(endpoint,first)

        first = .false.

        call skip(effrun,eof)
        count = count + 1

      end do ! effrun

c RLM commented out on 11/21/2014
cxc      call write_effects


999   close(effrun)
      close(scratch)
      return

1070  call error(70, *999)
1080  call error(80, *999)
      end

