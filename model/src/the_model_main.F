
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

      SUBROUTINE THE_MODEL_MAIN(myThid, secchiD)
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

      INTEGER myThid
      INTEGER myIter
      _RL     secchiD
      _RL     myTime

      CALL INITIALISE_FIXED( myThid )
      myTime = startTime
      myIter = nIter0

      CALL THE_MAIN_LOOP( myTime, myIter, myThid, secchiD )
      CALL MNC_FILE_CLOSE_ALL( myThid )

      RETURN
      END
  
