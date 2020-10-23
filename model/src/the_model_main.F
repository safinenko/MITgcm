
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

      SUBROUTINE THE_MODEL_MAIN(myThid)
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

      INTEGER myThid
      _RL    :: secchiD = 20. _d 0

      INTEGER myIter
      _RL     myTime

      CALL INITIALISE_FIXED( myThid )
      myTime = startTime
      myIter = nIter0

      CALL THE_MAIN_LOOP( myTime, myIter, myThid, secchiD )
      CALL MNC_FILE_CLOSE_ALL( myThid )

      RETURN
      END
