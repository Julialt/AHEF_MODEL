      SUBROUTINE read_ctylat_gp1

c  MRLM - to read in county file with state/coounty fip and the associated latitude
c  at the county centroid location
c18959\Desktop\AHEF_Runs_2014\
      INCLUDE 'files.h'
      INCLUDE 'global.h'
      INCLUDE 'exposure.h'

      INTEGER lat
c
      OPEN(unit=67,file="cntyfip_g1.txt", status="old")
c
      CALL skip(67, eof )
c
      DO icty = 1, numcty  ! of while not end of file
142            READ(67,188)cty_fip(icty),cty_lat(icty)
c FOR ONE COUNTY RUN

c      IF (cty_fip(icty).eq.1001) THEN
c       WRITE(*,*)' right one = ',cty_lat(icty)
c      ELSE
c        GOTO 142
c      ENDIF
c          
 188      FORMAT(I5,1x,f9.6)
c
c      CLOSE (67)
      ENDDO
      CLOSE(67)
c
      RETURN 

      END SUBROUTINE read_ctylat_gp1
            
