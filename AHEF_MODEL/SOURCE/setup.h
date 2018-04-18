!      MODULE setup
!-------------------------------------------------------!
! module containing directory information for AHEF i/o
!-------------------------------------------------------!
! location of run-specific setup/instructions files
! References are relative to directory AHEF_MODEL/SOURCE

! location of ODS parameter files 
      CHARACTER(LEN=*),PARAMETER :: dir_ods="../DATA_EMISSION/"

! location of emission input data files 
      CHARACTER(LEN=*),PARAMETER :: dir_ems="../DATA_EMISSION/"

! location of generated EESC data files
      CHARACTER(LEN=*),PARAMETER :: dir_esc="../DATA_EESC/"

! location of generated ozone data files
      CHARACTER(LEN=*),PARAMETER :: dir_ozn="../DATA_OZONE/"

! duplicate location of ozone scenario and irradiance data files
      CHARACTER(LEN=*),PARAMETER :: dir_dat="../INPUT_DATA/"

! location of action spectra input data files
      CHARACTER(LEN=*),PARAMETER :: dir_act="../DATA_SPECTRA/"

! location of biological weighting function input data files
      CHARACTER(LEN=*),PARAMETER :: dir_eff="../DATA_HEALTH/"

! location of county and population input data files
      CHARACTER(LEN=*),PARAMETER :: dir_pop="../DATA_POPULATION/"

! location of exposure & effects run instruction files
      CHARACTER(LEN=*),PARAMETER :: dir_in="../RUN_INPUT/"

! location of exposure & effects run intermediate output/input 
      CHARACTER(LEN=*),PARAMETER :: dir_io="../RUN_IO/"

! location of final output 
      CHARACTER(LEN=*),PARAMETER :: dir_out="../RUN_OUTPUT/"

!      END MODULE setup
