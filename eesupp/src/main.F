
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

      PROGRAM MAIN
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !LOCAL VARIABLES:
C     msgBuf       :: I/O message buffer
C     I            :: loop counter
C     myThid       :: thread Id number
      INTEGER :: myThid = 1
      INTEGER :: dummyComm = -1
      _RL     :: secchiD = 20. _d 0

      CALL EEBOOT( dummyComm )
      CALL THE_MODEL_MAIN(myThid, secchiD)

      END

