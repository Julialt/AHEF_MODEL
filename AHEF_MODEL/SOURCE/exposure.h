C=====================================================================
C     exposure.h
C=====================================================================
C  Definition of shared variables for EXPOSURE SUBROUTINE
C=====================================================================

      INTEGER minmon, maxmon, yrhi, yrlo
      INTEGER measures, colo, cohi, colo_year, cohi_year

      REAL,DIMENSION(maxlats) :: lats
      REAL,DIMENSION(lrows,lcols) :: lookup
      REAL,DIMENSION(maxages,maxages) :: weights

c      REAL,DIMENSION(minyear:maxyear,maxcty) :: yearlyirrad
      REAL,DIMENSION(minyear:maxyear,numcty) :: yearlyirrad
      REAL,DIMENSION(minyear:maxyear,12,maxlats) :: dobson

C      REAL,DIMENSION(maxcohorts,maxages,maxlats) :: expos_out,expos_outbl
c      REAL,DIMENSION(maxcohorts,topage+4,maxcty) :: expos_age,expos_agebl
      REAL,DIMENSION(maxcohorts,topage+4,numcty) :: expos_age,expos_agebl

!      LOGICAL :: expblflag

