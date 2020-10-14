#include "CPP_EEOPTIONS.h"
#include "PACKAGES_CONFIG.h"

CBOP
C     !ROUTINE: INI_THREADING_ENVIRONMENT

C     !INTERFACE:
      SUBROUTINE INI_THREADING_ENVIRONMENT

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE INI\_THREADING\_ENVIRONMENT
C     | o Initialise multi-threaded environment.
C     *==========================================================*
C     | Generally we do not start separate threads here.
C     | The separate threads a spawned at later on.
C     | Here we perform initialisation of data-structures
C     | that indicate which of the nSx x nSy tiles a thread is
C     | responsible for.
C     | The multiple threads are spawned in the top level MAIN
C     | routine.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bXPerThread - Blocks of size sNx per thread.
C     byPerThread - Blocks of size sNy per thread.
C     thId        - Thread index. Temporary used in loops
C                   which set per. thread values on a
C                   cartesian grid.
C     bxLo, bxHi  - Work vars. for thread index
C     byLo, byHi    range. bxLo is the lowest i index
C                   that a thread covers, bxHi is the
C                   highest i index. byLo is the lowest
C                   j index, byHi is the highest j index.
C     I, J        - Loop counter
C     msgBuf      - I/O buffer for reporting status information.
C     myThid      - Dummy thread id for use in printed messages
C                   ( this routine "INI_THREADING_ENVIRONMENT" is
C                     called before multi-threading has started.)
      INTEGER bxPerThread
      INTEGER byPerThread
      INTEGER thId
      INTEGER bxLo, bxHi
      INTEGER byLo, byHi
      INTEGER I, J
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER myThid
#ifndef ALLOW_EXCH2
      LOGICAL flag
#endif
CEOP

C--   Set default for all threads of having no blocks to
C--   work on - except for thread 1.
      myBxLo(1) = 1
      myBxHi(1) = nSx
      myByLo(1) = 1
      myByHi(1) = nSy
      DO I = 2, MAX_NO_THREADS
       myBxLo(I) = 0
       myBxHi(I) = 0
       myByLo(I) = 0
       myByHi(I) = 0
      ENDDO
      myThid = 1
      commName(COMM_NONE) = 'none'
      commName(COMM_MSG ) = 'messages'
      commName(COMM_PUT ) = 'put'
      commName(COMM_GET ) = 'get'

C--   If there are multiple threads allocate different range of the
C--   nSx*nSy blocks to each thread.
C     For now handle simple case of no. blocks nSx = n*nTx and
C     no. blocks nSy = m*nTy ( where m and n are integer ). This
C     is handled by simply mapping threads to blocks in sequence
C     with the x thread index moving fastest.
C     Later code which sets the thread number of neighboring blocks
C     needs to be consistent with the code here.
      nThreads = nTx * nTy
      IF   ( nThreads .GT. MAX_NO_THREADS ) THEN
       WRITE(msgBuf,'(2A,2I6)')
     &  'S/R INI_THREADING_ENVIRONMENT:',
     &  ' Total number of threads exceeds MAX_NO_THREADS',
     &   nTx*nTy, MAX_NO_THREADS
       CALL PRINT_ERROR(msgBuf, myThid)
       WRITE(msgBuf,'(2A)')
     &    ' Needs to increase MAX_NO_THREADS',
     &    ' in file "EEPARAMS.h" and to re-compile'
       CALL PRINT_ERROR(msgBuf, myThid)
       eeBootError = .TRUE.
       STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
      ENDIF

C--   Initialise the barrier mechanisms
C     BAR2 will eventually replace barrier everywhere.
      CALL BARRIER_INIT
      DO I=1, MAX_NO_THREADS
       CALL BAR2_INIT(I)
      ENDDO

C--   Initialise exchange mechanism
      CALL EXCH_INIT

      IF   ( nThreads .NE. nTx*nTy ) THEN
       WRITE(msgBuf,'(A,A,A,I5,A,I5)')
     &  'S/R INI_THREADING_ENVIRONMENT:',
     &  ' Total number of threads is not the same as nTx*nTy.',
     &  ' nTx * nTy = ',nTx*nTy,' nThreads = ',nThreads
       CALL PRINT_ERROR(msgBuf, myThid)
       eeBootError = .TRUE.
       STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
      ENDIF
      bxPerThread = nSx/nTx
      IF ( bxPerThread*nTx .NE. nSx ) THEN
       WRITE(msgBuf,'(A,A,A)')
     &  'S/R INI_THREADING_ENVIRONMENT:',
     &  ' Number of blocks in X (nSx)',
     &  ' must be exact multiple of threads in X (nTx).'
       CALL PRINT_ERROR(msgBuf, myThid)
       eeBootError = .TRUE.
       STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
      ENDIF
      byPerThread = nSy/nTy
      IF ( byPerThread*nTy .NE. nSy ) THEN
       WRITE(msgBuf,'(A,A,A)')
     &  'S/R INI_THREADING_ENVIRONMENT:',
     &  ' Number of blocks in Y (nSy)',
     &  ' must be exact multiple of threads in Y (nTy).'
       CALL PRINT_ERROR(msgBuf, myThid)
       eeBootError = .TRUE.
       STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
      ENDIF
      IF ( .NOT. eeBootError ) THEN
       byLo = 1
       DO J=1,nTy
        byHi = byLo+byPerThread-1
        bxLo = 1
        DO I=1,nTx
         thId = (J-1)*nTx+I
         bxHi = bxLo+bxPerThread-1
         myBxLo(thId) = bxLo
         myBxHi(thId) = bxHi
         myByLo(thId) = byLo
         myByHi(thId) = byHi
         bxLo = bxHi+1
        ENDDO
        byLo = byHi+1
       ENDDO
      ENDIF

      DO thId=1,nThreads
       CALL INI_COMMUNICATION_PATTERNS( thId )
      ENDDO

C--   Print mapping of threads to grid points.
C      WRITE(msgBuf,'(A)')
C     &'// ======================================================'
C      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &  SQUEEZE_RIGHT , 1)
C      WRITE(msgBuf,'(A)') '// Mapping of tiles to threads'
C      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &  SQUEEZE_RIGHT , 1)
C     o Write list of tiles each thread is responsible for
C      WRITE(msgBuf,'(A)')
C     &'// ======================================================'
C      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &  SQUEEZE_RIGHT , 1)
C      DO I=1,nThreads
C       WRITE(msgBuf,'(A,I4,A,4(I4,A1))')
C     & '// -o- Thread',I,', tiles (',
C     & myBxLo(I),':',myBxHi(I),',',myByLo(I),':',myByHi(I),')'
C       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,SQUEEZE_BOTH , 1)
C      ENDDO
C      WRITE(msgBuf,'(A)')  ' '
C      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,SQUEEZE_RIGHT , 1)

C#ifndef ALLOW_EXCH2
C     o For each tile print its communication method(s)
C      WRITE(msgBuf,'(A)')
C     &'// ======================================================'
C      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &  SQUEEZE_RIGHT , 1)
C      WRITE(msgBuf,'(A)') '// Tile <-> Tile connectvity table'
C      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &  SQUEEZE_RIGHT , 1)
C      WRITE(msgBuf,'(A)')
C     &'// ======================================================'
C      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &  SQUEEZE_RIGHT , 1)
C      DO J=1,nSy
C       DO I=1,nSx
C        WRITE(msgBuf,'(A,A,I6.6,A,I6.6,A)')
C     &   '//',' Tile number: ',tileNo(I,J),
C     &   ' (process no. = ',myPid,')'
C        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT , 1)
C       o West communication details
C        IF ( tileNoW(I,J).NE. NULL_TILE ) THEN
C         WRITE(msgBuf,'(A,A,I6.6,A,I6.6,A,A)')
C     &   '//        WEST: ',
C     &   'Tile = ',tileNoW(I,J),
C     &   ', Process = ',tilePidW(I,J),
C     &   ', Comm = ',commName(tileCommModeW(I,J))
C         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
C         WRITE(msgBuf,'(A,A,I6.6,A,I6.6)')
C     &   '//              ',
C     &   '  bi = ',tileBiW(I,J),
C     &   ', bj = ',tileBjW(I,J)
C         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
C        ELSE
C         WRITE(msgBuf,'(A)')
C     &   '//         WEST: no neighbor'
C         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
C        ENDIF
C       o East communication details
C        IF ( tileNoE(I,J).NE. NULL_TILE ) THEN
C         WRITE(msgBuf,'(A,A,I6.6,A,I6.6,A,A)')
C     &   '//        EAST: ',
C     &   'Tile = ',tileNoE(I,J),
C     &   ', Process = ',tilePidE(I,J),
C     &   ', Comm = ',commName(tileCommModeE(I,J))
C         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
C         WRITE(msgBuf,'(A,A,I6.6,A,I6.6)')
C     &   '//              ',
C     &   '  bi = ',tileBiE(I,J),
C     &   ', bj = ',tileBjE(I,J)
C         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
C        ELSE
C         WRITE(msgBuf,'(A)')
C     &   '//         EAST: no neighbor'
C         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
C        ENDIF
C       o South communication method
C        IF ( tileNoS(I,J).NE. NULL_TILE ) THEN
C         WRITE(msgBuf,'(A,A,I6.6,A,I6.6,A,A)')
C     &   '//       SOUTH: ',
C     &   'Tile = ',tileNoS(I,J),
C     &   ', Process = ',tilePidS(I,J),
C     &   ', Comm = ',commName(tileCommModeS(I,J))
C         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
C         WRITE(msgBuf,'(A,A,I6.6,A,I6.6)')
C     &   '//              ',
C     &   '  bi = ',tileBiS(I,J),
C     &   ', bj = ',tileBjS(I,J)
C         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
C        ELSE
C         WRITE(msgBuf,'(A)')
C     &   '//        SOUTH: no neighbor'
C         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
C        ENDIF
C       o North communication method
C        IF ( tileNoN(I,J).NE. NULL_TILE ) THEN
C         WRITE(msgBuf,'(A,A,I6.6,A,I6.6,A,A)')
C     &   '//       NORTH: ',
C     &   'Tile = ',tileNoN(I,J),
C     &   ', Process = ',tilePidN(I,J),
C     &   ', Comm = ',commName(tileCommModeN(I,J))
C         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
C         WRITE(msgBuf,'(A,A,I6.6,A,I6.6)')
C     &   '//              ',
C     &   '  bi = ',tileBiN(I,J),
C     &   ', bj = ',tileBjN(I,J)
C         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
C        ELSE
C         WRITE(msgBuf,'(A)')
C     &   '//        NORTH: no neighbor'
C         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
C        ENDIF
C       ENDDO
C      ENDDO
C      WRITE(msgBuf,'(A)')  ' '
C      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
C#endif /* ndef ALLOW_EXCH2 */

C--   Check EXCH-1 options
#ifndef ALLOW_EXCH2
      IF ( usingMPI .AND. useCubedSphereExchange ) THEN
C-    not working with multi-procs (checked within EXCH1-CUBE S/R) and
C-    if compiled with MPI (without EXCH2) safer to set usingMPI to False.
        WRITE(msgBuf,'(2A)') 'EXCH-1 useCubedSphereExchange',
     &                       ' unsafe with usingMPI=True'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
      ENDIF
      IF ( nThreads.GT.1 .AND. useCubedSphereExchange ) THEN
C-    multi-threads not working for local arrays; could remove the stop if
C     we are sure that only shared array (=in common blocks) are exchanged.
        WRITE(msgBuf,'(2A)') 'EXCH-1 useCubedSphereExchange',
     &                       ' unsafe with multi-threads'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
      ENDIF
      IF ( nThreads.GT.1 ) THEN
       flag = .FALSE.
       DO J=1,nSy
        DO I=1,nSx
          flag = flag
     &       .OR. tileCommModeW(I,J).EQ.COMM_GET
     &       .OR. tileCommModeE(I,J).EQ.COMM_GET
     &       .OR. tileCommModeS(I,J).EQ.COMM_GET
     &       .OR. tileCommModeN(I,J).EQ.COMM_GET
        ENDDO
       ENDDO
       IF ( flag ) THEN
C-    multi-threads not working for local arrays; not safe neither for shared arrays
        WRITE(msgBuf,'(3A)') 'EXCH-1 using Comm = ',
     &   commName(COMM_GET), ' unsafe with multi-threads'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
       ENDIF
      ENDIF
#endif /* ndef ALLOW_EXCH2 */

      RETURN
      END
