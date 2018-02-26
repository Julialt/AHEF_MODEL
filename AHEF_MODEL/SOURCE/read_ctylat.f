      SUBROUTINE read_ctylat
c  MRLM - to read in county file with state/coounty fip and the associated latitude
c  at the county centroid location
c
      INCLUDE 'files.fi'
      INCLUDE 'global.fi'
      INCLUDE 'exposure.fi'
c
      INTEGER lat
c
      OPEN(unit=67,file="stcntyfip_lat.txt", status="old")
c
      CALL skip(67, eof )
c
      DO icty = 1, numcty  ! of while not end of file
142            READ(67,188)cty_fip(icty),cty_lat(icty)
c FOR ONE COUNTY RUN

      IF (cty_fip(icty).EQ.1001) THEN
       WRITE(*,*)' right one = ',cty_lat(icty)
      ELSE
        GOTO 142
      ENDIF
c          
 188      FORMAT(I5,1x,f9.6)
c
      CLOSE (67)
      ENDDO
c
      RETURN 

      END SUBROUTINE read_ctylat
            
