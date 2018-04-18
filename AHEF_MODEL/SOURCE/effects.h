C=====================================================================
C     effects.h
C=====================================================================
C  Definition of shared variables for EFFECTS SUBROUTINE
C=====================================================================
c change all maxcty to numcty to save memory!
      REAL,DIMENSION(maxlats) :: lats
      REAL,DIMENSION(maxages) :: age_coeffs
      REAL,DIMENSION(maxpops) :: baf
      REAL,DIMENSION(maxpops,5) :: coeff
      REAL,DIMENSION(maxcohorts) :: cohort_risk 
      REAL,DIMENSION(maxcohorts,topage+4, numcty) :: expage,expagebl
      REAL,DIMENSION(maxcohorts,maxages,maxlats,maxpops) :: incid_bl
      REAL,DIMENSION(maxcohorts,topage,maxlats,maxpops) :: incage_bl
      REAL,DIMENSION(2100+topage+4-1887,numcty,maxpops) :: casesa,casesab
      REAL,DIMENSION(maxcohorts,topage+4,numcty,maxpops) :: caseca,casecab
      REAL,DIMENSION(minyear:maxyear,maxeps) :: total

      CHARACTER*12 endpoint, indname
      CHARACTER*12 agename, cohortname, coeffname, popseg

      INTEGER colo, cohi, colo_year, cohi_year
      INTEGER poplo, pophi, outputlo, outputhi
      INTEGER popmaxyr, popminyr, yrstep

      INTEGER,DIMENSION(numcty) :: cty
      INTEGER,DIMENSION(minyear:maxyear,maxages,numcty,maxpops) :: pop

      INTEGER,DIMENSION(maxstate),PARAMETER :: group1 =
     &           (/9,10,11,23,24,25,33,34,36,39,42,44,50,51,54/)

C     REAL,DIMENSION(maxcohorts,maxages,maxlats) :: expos, exposbl
C     REAL,DIMENSION(maxcohorts,maxages,maxlats,maxpops) :: incid,incid2
C     REAL,DIMENSION(maxcohorts,topage,maxlats,maxpops) :: incage2
C     REAL,DIMENSION(minyear:maxyear,maxages,maxlats,maxpops) :: cases



