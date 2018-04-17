C=====================================================================
C     files.h
C=====================================================================
C  This file contains definitions of unit numbers for files used
C  throughout the program.
C=====================================================================

      INTEGER,PARAMETER :: runfile   = 10     ! global run file
      INTEGER,PARAMETER :: logfile   = 20     ! global log (error) file
      INTEGER,PARAMETER :: iunit     = 30     ! generic input
      INTEGER,PARAMETER :: iounit    = 32     ! generic input
      INTEGER,PARAMETER :: ounit     = 35     ! generic output
      INTEGER,PARAMETER :: ounit21   = 21     ! generic output
      INTEGER,PARAMETER :: ounit31   = 31     ! generic output
      INTEGER,PARAMETER :: ounit441  = 441    ! generic output
      INTEGER,PARAMETER :: o1unit    = 41     ! generic output
      INTEGER,PARAMETER :: o2unit    = 42     ! generic output
      INTEGER,PARAMETER :: o3unit    = 43     ! generic output
      INTEGER,PARAMETER :: atmrun    = 40     ! atmosphere run file
      INTEGER,PARAMETER :: exprun    = 50     ! exposure run file
      INTEGER,PARAMETER :: effrun    = 60     ! effects run file
      INTEGER,PARAMETER :: scratch   = 70     ! exposure scratch file
      INTEGER,PARAMETER :: scratchbl  = 80     ! baseline exposure scratch file
      INTEGER,PARAMETER :: scratchage  = 71     ! exp by age file
      INTEGER,PARAMETER :: scratchagebl  = 81     ! exp by age bl file
C=====================================================================
C  Note: to avoid unit conflicts, do not use generic i/o units for
C        operations that require the file to stay open.
C=====================================================================

