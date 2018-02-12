c	Version 10.23.2012
c MRLM replaced satellite linear regression coefficients
c provided by Mark Wagner to obtain same ozone results
c
c  updated lifetimes, fractional release and alpha based
c  on WMO(2010) and WMO (2007) - see notes
c
c      Version 08.06.99
c      clbr55rg.for - to redo EESC for new regressions

cc--  Equivalent stratospheric Cl release fraction for Brominated Compounds,
cc    ALFA = 60 (New Case) used by WMO (2007,2010)

c     This program calculates the  Cl and Br loading for WMO (1994) scenarios
CAMK  The model was modified to work with .ati emission files from ICF Model 
CAMK  The model was modified to predict total ozone in D.U. for different 
CAMK  (after year 1989!!!) emission scenarious for each month of year for 
CAMK  following latitudes: 70-60N, 60-50N, 50-40N, 40-30N, 30-20N, N.H., 
CAMK                       Global.
 
  
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(icf=19,iati=25,iyr=300, mlat=7, mm=12)
CC---icf: total no. of gases-19
c --  f11(1),f12(2),f113(3),f114(4),f115(5),H-1211(6), H-1301(7)
C --- H-2402(8),ccl4(9),CH3CCl3(10),HCFC-22(11),HCFC-123(12),HCFC-124(13)
C --- HCFC-141(14),HCFC-142(15),
C---- h-225ca(16),h-225cb(17),ch3cl(18),CH3Br_an(19)
	real emis(icf,iyr), emisati(iati, iyr)
	real cfcc(icf,iyr)
	real xncl(icf),xnbr(icf),fclr(icf),xlt(icf),cf(icf)
	real stclr(icf,iyr),stbr(icf,iyr),trclr(icf,iyr),trbr(icf,iyr)
	real totrcl(iyr),tostcl(iyr)
cicf      real*8 ef22(57),eex(12)
      real*8 ef22(57)
	real a(mm,mlat), b(mm,mlat), O3(mm,mlat)
	character*50 inpf, outf
	character*3 month(mm)


cc --- number_Cl for 19 gases listed above
      data xncl/3.,2.,3.,2.,1.,1.,0.,1.,4.,3.,1.,2.,1.,2.,1.,2.,
     *          2.,1.,0./

cc-- Equivalet strat. Cl release Fac. for Brominated Compounds (New Case)
cc-- used by WMO (2007,2010)
cc	data xnbr/5*0,3*55.,10*0,55./
      data xnbr/5*0.0,3*60.,10*0.,60./
cc-- Equivalet strat. Cl release Fac. for Brominated Compounds (High Case)
CC -- Highly recomended by Don Wuebbles
cc	data xnbr/5*0.0,3*100.,10*0.,100./
c100      data xnbr/5*0.0,3*100.,10*0.,100./    !MRLM COMMENT _ TESTING NOW
c --- stat_Cl-Release factors
c      data fclr/0.9,0.45,0.5,0.58,0.12,
c     *       .99,0.72,0.0,.95,0.9,
c     * 0.315,1.0,.4707, .65,.324,
c     * 2*1., 0.99,.97/
cc --- stat_Cl-Release factors (WMO(2007))
c      data fclr/1.0,0.6,0.75,0.28,0.12,
c     *       1.18,0.62,0.0,1.06,1.08,
c     * 0.35,1.11,0.52,0.65,0.36,
c     * 2*1., 0.99,1.12/
c adjust to identify difference
      data fclr/1.0,0.6,0.75,0.28,0.12,
     *       0.99,0.72,0.0,0.95,0.9,
     * 0.315,1.0,0.4707,0.65,0.3240,
c     * 0.315,1.0,0.47,0.65,0.36,
     * 2*1.,0.99,0.97/
c correct below  - 4
c     * 2*1., 0.99,1.12/
cc --- Lifetime (yr)
c      data xlt/ 50.,102.,85.,300.,1700.,
c     *          20.,65.,77.,42.,4.9,
c     *        12.1,1.4,6.1,9.4,18.4,2.1,6.2,1.5,1.2/
c lifetime below based on WMO (1999/1998) assessment report on ozone
c	data xlt/ 45.0,100.0,85.0,300.0,1700.0,11.,65.0,
c     *    77.0,42.0,4.8,12.3,1.4,6.1,9.4,18.4,2.1,6.2,
c     *    1.3,0.7/
c lifetime below based on WMO (2007) assessment report on ozone
c      data xlt/ 45.0,100.0,85.0,300.0,1700.0,16.,65.0,
c     *    77.0,26.0,5.0,12.0,1.3,5.8,50000.0,17.9,2.1,6.2,
c     *    1.0,0.7/
c  lifetime below based on WMO(2010) assessment report on ozone
      data xlt/ 45.0,100.0,85.0,190.0,1020.0,16.0,65.0,
     *    77.0,26.0,5.0,11.9,1.3,5.9,50000.0,17.2,2.1,6.2,
     *    1.0,0.8/
cc --- Conversion Factors (kt/ppt)
      data cf/23.2,20.4,31.6,28.9,26.1,27.9,25.1,25.1,25.9,22.4,
     *        14.6,25.8,23.,19.7,16.9,34.2,34.2,8.5,16./
cc --- Initial concentration of CFCs and other compounds
      data (cfcc(i,1),i=1,19)/19*0.0/

cc -- Historical F-22 emissions
      data ef22/4*0.0,10*.12,
     *  0.3,0.4,0.5,0.7,0.9,1.2,1.6,2.2,2.9,4.0,
     *  5.4,7.2,23.3,25.1,27.0,29.1,31.3,33.6,36.2,38.9,
     *  41.4,45.8,50.1,57.5,65.0,65.7,74.0,82.4,90.1,96.7,
     *  104.3,110.3,111.9,122.8,131.8,137.2,145.6,153.7,171.4,188.7,
     *  195.2,204.3,235.0/

	 print*,'TRY'
Cicf       data eex/10,15,20,25,30,35,40,45,50,55,60,70/
c --  f11(1),f12(2),f113(3),f114(4),f115(5),h-1211(6), H-1301(7)
C --- h-2402(8),ccl4(9),CH3CCl3(10),f-22(11),h-123(12),h-124(13)
C --- h-141b(14),h-142b(15), h-225ca(16),h-225cb(17),ch3cl(18),CH3Br_an(19)

C  Linear regression coefficients: O3 =a +b*EESC
	data month/"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
     *           "Oct","Nov","Dec"/
C Month: 1,2,3,4,5,6,7,8,9,10,11,12
C Latitudes: 70-60, 60-50, 50-40, 40-30, 30-20, N.h., Global
C
C a(mm,mlat)
C
c      data a/456.86,508.23,599.33,582.20,504.69,434.97,394.45,354.54,
c     *357.63,390.90,416.32,437.33,
c     *501.11,547.48,563.84,528.58,505.81,447.78,389.79,358.91,363.97,
c     *389.88,426.53,465.35,
c     *470.35,525.16,511.33,463.98,457.12,412.83,362.92,351.64,345.43,
c     *346.00,396.44,440.89,
c     *369.46,382.83,399.05,381.63,369.36,342.29,331.33,329.55,318.30,
c     *314.13,334.20,365.67,
c     *286.46,281.21,287.88,309.47,309.13,304.41,303.19,307.23,302.98,
c     *303.15,300.96,311.78,
c     *375.31,403.08,456.41,446.95,412.60,376.17,345.04,334.30,324.03,
c     *332.09,349.73,365.81,
c     *363.69,364.03,390.48,393.47,381.33,362.86,349.24,354.69,364.66,
c     *373.03,383.31,375.47/
	data a/530.81,530.81,653.43,597.41,488.83,426.07,385.62,346.33,
     +	341.04,361.33,357.72,357.72,
     +472.79,565.00,574.55,548.35,489.74,442.02,381.81,351.64,352.12,
     + 	371.78,347.74,380.49,
     +477.02,541.47,525.66,487.51,449.78,406.26,356.93,341.66,335.64,
     + 	331.00,332.95,374.63,
     +401.50,413.25,429.39,407.88,368.74,332.00,325.58,324.51,314.74,
     + 	300.33,309.58,337.77,
     +304.71,304.21,319.99,333.53,307.40,291.91,298.47,304.46,301.11,
     +    291.31,296.26,304.46,
     +351.60,373.67,385.04,380.57,355.63,330.67,318.42,315.68,311.92,
     +    303.88,303.39,315.75,
     +333.70,335.53,339.71,341.00,333.96,321.88,323.73,329.88,336.34,
     +    333.73,324.56,324.67/

C b(mm,mlat)
c      data b/-0.0239,-0.0324,-0.0589,-0.0546,-0.0368,-0.0261,-0.0215,
c     *-0.0131,-0.0186,-0.0309,-0.0343,-0.0310,
c     *-0.0428,-0.0503,-0.0521,-0.0423,-0.0406,-0.0279,-0.0149,-0.0110,
c     *-0.0179,-0.0284,-0.0373,-0.0428,
c     *-0.0404,-0.0526,-0.0449,-0.0292,-0.0305,-0.0204,-0.0104,-0.0123,
c     *-0.0143,-0.0172,-0.0322,-0.0404,
c     *-0.0224,-0.0218,-0.0237,-0.0151,-0.0116,-0.0073,-0.0078,-0.0099,
c     *-0.0093,-0.0116,-0.0200,-0.0274,
c     *-0.0081,-0.0025, 0.0000,-0.0027,-0.0005,-0.0007,-0.0018,-0.0052,
c     *-0.0067,-0.0105,-0.0137,-0.0190,
c     *-0.0235,-0.0240,-0.0344,-0.0296,-0.0220,-0.0155,-0.0106,-0.0118,
c     *-0.0119,-0.0161,-0.0227,-0.0265,
c     *-0.0220,-0.0195,-0.0252,-0.0242,-0.0212,-0.0172,-0.0149,-0.0190,
c     *-0.0256,-0.0291,-0.0305,-0.0270/
	data b/-0.0437,-0.0437,-0.0724,-0.0608,-0.0318,-0.0239,-0.0192,	
     +	-0.0110,-0.0127,-0.0193,-0.0133,-0.0133,
     + -0.0334,-0.0574,-0.0572,-0.0510,-0.0361,-0.0273,-0.0138,-0.0099,	
     +  -0.0142,-0.0217,-0.0090,-0.0119,
     +-0.0431,-0.0586,-0.0512,-0.0396,-0.0296,-0.0201,-0.0103,
     +	-0.0104,-0.0118,-0.0122,-0.0097,-0.0165,
     + -0.0344,-0.0337,-0.0359,-0.0268,-0.0137,-0.0062,-0.0080,-0.0100,
     +	-0.0095,-0.0076,-0.0116,-0.0176,
     +-0.0159,-0.0127,-0.0140,-0.0142,-0.0030,0.0004,-0.0032,-0.0068,
     +	-0.0081,-0.0077,-0.0129,-0.0169,
     + -0.0214,-0.0246,-0.0251,-0.0221,-0.0138,-0.0081,-0.0066,-0.0080,	
     + -0.0089,-0.0085,-0.0094,-0.0128,
     + -0.0166,-0.0160,-0.0163,-0.0159,-0.0132,-0.0097,-0.0104,-0.0123,	
     + -0.0141,-0.0138,-0.0126,-0.0142/
	READ (*,*) inpf
	READ (*,*) outf
cinpf="2007_EP1N.txt"
coutf="2007_EP1N.OZN"

CC--Scenarios for futute chlorine and bromine loding
cc-- See WMO (1994) report for detail description of this scenario
      open(unit=4,file=inpf)
      open(unit=6,file=outf)
	write(6,*) 'The emission file is: ',inpf
ccc -- j(1)=1935; j(266)=2200
	j=2
camk      do 10 j=2,266
cicf  5   read(4,300,END=10)ix,(emis(i,j),i=1,19)
  5   read(4,*,end=10)ix,(emisati(i,j),i=1,25)
c     read(4,302,END=10)ix,(emisati(i,j),i=1,25)
     	write(6,303)ix,(emisati(i,j),i=1,25)
	
	if ((j .eq. 2) .and. (ix .ne. 1936)) then
	  write(6,*) 'Emission file should start from year 1936!!!'
	  STOP 10
      endif

camk	write(6,300)ix,(emis(i,j),i=1,19)  

      
Cicf      if(j.le.12)then

cicf      emis(19,j)= eex(j)
cicf      else
cicf      emis(19,j)=emis(19,j)*.44
cicf      endif

      emis(1,j)=emisati(4,j)
      emis(2,j)=emisati(5,j)
      emis(3,j)=emisati(7,j)
      emis(4,j)=0.
      emis(5,j)=0.
      emis(6,j)=emisati(12,j)
      emis(7,j)=emisati(11,j)
      emis(8,j)=0.
	emis(9,j)=emisati(8,j)
      emis(10,j)=emisati(9,j)
      emis(11,j)=emisati(6,j)
      emis(12,j)=emisati(16,j)
      emis(13,j)=emisati(17,j)
      emis(14,j)=emisati(20,j)
      emis(15,j)=emisati(21,j)
      emis(16,j)=0.
      emis(17,j)=0.
      emis(18,j)=emisati(10,j)
      emis(19,j)=emisati(13,j)

      if(j.le.58)emis(11,j)=ef22(j-1)

	j = j + 1
      jend = j
	
      goto 5

c      write(6,300)ix,(emis(i,j),i=1,19)
c300	format(i5,19(f8.2,1x))
c   10 continue

CICF  300 format (i10,0p,19f9.3)
  302 format (i5,25f8.0)
  303 format (i5,25f8.2)

camk  300 format (i10,0p,7f9.3/f4.3,7f9.3/f4.3,3f9.3,f10.3)

      
camk	do 50 j=2,266
 10	do 50 j=2,jend

      do 20 i=1,19
      ex1=exp(-1./xlt(i))
cc--concentration calculations for CFCs and other compounds
      cfcc(i,j)=ex1*cfcc(i,j-1)+((1-ex1)*xlt(i)*emis(i,j))/cf(i)
   20 continue
c      write(6,300)1936+(J-1),(cfcc(k,j),k=1,19)
c      write(6,*)1936+(J-1),emis(12,j),cfcc(12,j),xlt(12),cf(12)
cc      write(6,151)1936+(J-1),emis(12,j),cfcc(12,j)
   50 continue
  151 format(i6,2f10.4)
	write(6,*)
      write(6,*) '               TOTAL OZONE COLUMN IN DOBSON UNITS '
CAMK      write(6,*)'                     Copenhagen Scenario'
      write(6,*)
      write(6,*)'Year  Month  EESC     70-60   60-50   50-40   40-30 
     *30-20   N.H.   Global'
      write(6,*)
      do 40 j=4,166
      do 30 i=1,19
       if(i.eq.18)then
        trclr(i,j)=0.6
        stclr(i,j)=0.54
        trbr (i,j)=(cfcc(i,j)*xnbr(i))/1000.
       stbr (i,j)=(cfcc(i,j-3)*xnbr(i)*fclr(i))/1000.
       else
        trclr(i,j)=(cfcc(i,j)*xncl(i))/1000.
        stclr(i,j)=(cfcc(i,j-3)*xncl(i)*fclr(i))/1000.
        trbr (i,j)=(cfcc(i,j)*xnbr(i))/1000.
        stbr (i,j)=(cfcc(i,j-3)*xnbr(i)*fclr(i))/1000.
      endif
   30 continue
c -- alfa = 40
      totrcl(j)=0.25
cc      tostcl(j)=0.21
c -- alfa = 100
c100       totrcl(j)=0.625
CICF       tostcl(j)=0.525
       tostcl(j)=0.

       do 70 k=1,19
       totrcl(j)=totrcl(j)+trclr(k,j)+trbr(k,j)
       tostcl(j)=tostcl(j)+stclr(k,j)+stbr(k,j)
   70  continue
c      write(6,200)1935+(j-1),totrcl(j),tostcl(j)
c      write(6,400)1935+(j-1),(trclr(m,j),m=1,3),
c     * trbr(6,j)+trclr(6,j),trbr(7,j),
c     * (trclr(l,j),l=9,15),
c     * trclr(18,j),trbr(19,j)
c      write(6,400)1935+(j-1),(stclr(m,j),m=1,3),
c     * stbr(6,j)+stclr(6,j),stbr(7,j),
c     * (stclr(l,j),l=9,15),
c     * stclr(18,j),stbr(19,j)

      cfc= trclr(1,j)+trclr(2,j)+trclr(3,j)+
     *     trclr(4,j)+trclr(5,j)
      ccl4=trclr(9,j)
      ch3cl=trclr(10,j)
      hcfcs=trclr(11,j)+trclr(12,j)+trclr(13,j)+trclr(14,j)+trclr(15,j)
      halon=trclr(6,j)+trclr(7,j)+trclr(8,j)+
     *       trbr(6,j)+ trbr(7,j)+ trbr(8,j)
      ch3br=trbr(19,j)
      ch3br=trbr(19,j)
c -- alfa=40
c --     write(6,800)1935+(j-1),0.6,0.25,
cc      write(6,800)1935+(j-1),0.6,0.625,
cc     * cfc,ccl4,ch3cl,hcfcs,halon,ch3br,totrcl(j)
      cfcs= stclr(1,j)+stclr(2,j)+stclr(3,j)+
     *      stclr(4,j)+stclr(5,j)
      ccl4s=stclr(9,j)
      ch3cls=stclr(10,j)
      hcfcss=stclr(11,j)+stclr(12,j)+stclr(13,j)+stclr(14,j)+stclr(15,j)
      halons=stclr(6,j)+stclr(7,j)+stclr(8,j)+
     *        stbr(6,j)+ stbr(7,j)+ stbr(8,j)
      ch3brs=stbr(19,j)
c MRLM - added for troubleshooting
	write(81,78)1935+(j-1),cfcs,cc14s,cg3cls,hcfcss,halons,ch3brs,
     + cfc,ccl4,ch3cl,hcfcs,halon,ch3br
78    format(i6,12f8.5)
c -- alfa=40
cc      write(6,800)1935+(j-1),0.54,0.21,
c --alfa = 100

CICF      write(6,800)1935+(j-1),0.54,0.525,
	if (1935+(j-1) .ge. 1978) then
	   EESC = 1000.*tostcl(j)
	   do 37 m=1,mm
	      do 35 ml=1, mlat
c update the equation to below to update the b based on new EESC/O3 obs relationship from 1980 to 1990
c  0.89 = [EESC1(1980)-EESC1(1990)]/[EESC2(1980)-EESC2(1990)]
c	         O3(m,ml) = a(m,ml) + b(m,ml)*EESC
               O3(m,ml) = a(m,ml) + b(m,ml)*EESC*0.89
   35       continue
            write(6,800)1935+(j-1),month(m),EESC,(O3(m,ml), ml=1,mlat)
   37    continue
      endif
   40 continue
  800 format(i5,2x,a3,3x,8f8.2)
CDEB      do 110 ml=1,mlat
CDEB	      write(6,900) (a(m,ml), m=1,mm)
CDEB  110 continue
CDEB      do 120 ml=1,mlat
CDEB	      write(6,950) (b(m,ml), m=1,mm)
CDEB  120 continue

  900 format(12(f6.2,1x))
  950 format(12(f6.4,1x))

c --  f11(1),f12(2),f113(3),f114(4),f115(5),h-1211(6), H-1301(7)
C --- h-2402(8),ccl4(9),CH3CCl3(10),f-22(11),h-123(12),h-124(13)
C --- h-141b(14),h-142b(15), h-225ca(16),h-225cb(17),ch3cl(18),CH3Br_an(19)


  100 format(i8,19f10.3)
  200 format(i8,2f10.3)
  400 format(i8,19f10.3)
  500 format('Enter the name of emission file:', a50)
  600 format('Enter the name of output file:', a50)
      stop
      end


