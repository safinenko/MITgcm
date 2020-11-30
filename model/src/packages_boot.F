#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: PACKAGES_BOOT
C     !INTERFACE:
      SUBROUTINE PACKAGES_BOOT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PACKAGES_BOOT
C     | o Routine to parse runtime package selection file
C     *==========================================================*
C     | Routine reads a flat file which contains a single flag
C     | for each known "package". Flag can enables runtime
C     | activation or deactivation of a package. The data is read
C     | from a file called data.pkg.
C     | Note - If a runtime activation flag is set on but the
C     |        package code was not included in the build then
C     |        the default behavior is to stop with an error.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid   :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf   :: Informational/error message buffer
C     iUnit    :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      LOGICAL locFlag
CEOP

C     data.pkg namelists
      NAMELIST /PACKAGES/
     &          useOBCS,
     &          useSHAP_FILT,
     &          useZONAL_FILT,
     &          useOPPS,
     &          usePP81,
     &          useKL10,
     &          useMY82,
     &          useGGL90,
     &          useKPP,
     &          useGMRedi,
     &          useDOWN_SLOPE,
     &          useBBL,
     &          useCAL,
     &          useEXF,
     &          useBulkForce,
     &          useEBM,
     &          useCheapAML,
     &          useAUTODIFF,
     &          useGrdchk,
     &          useSMOOTH,
     &          usePROFILES,
     &          useECCO,
     &          useCTRL,
     &          useSBO,
     &          useFLT,
     &          usePTRACERS,
     &          useGCHEM,
     &          useRBCS,
     &          useOffLine,
     &          useMATRIX,
     &          useFRAZIL,
     &          useSEAICE,
     &          useSALT_PLUME,
     &          useShelfIce,
     &          useStreamIce,
     &          useICEFRONT,
     &          useThSIce,
     &          useLand,
     &          useATM2D,
     &          useAIM,
     &          useAtm_Phys,
     &          useFizhi,
     &          useGridAlt,
     &          useDiagnostics,
     &          useREGRID,
     &          useLayers,
     &          useMNC,
     &          useRunClock,
     &          useEMBED_FILES,
     &          useMYPACKAGE

      _BEGIN_MASTER(myThid)

C      WRITE(msgBuf,'(A)') ' PACKAGES_BOOT: opening data.pkg'
C      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &                    SQUEEZE_RIGHT , myThid )

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.pkg', 'PACKAGES_BOOT',
     O                          iUnit,
     I                          myThid )

C--   Default package configuration
c     useGAD          =.FALSE.
      useOBCS         =.FALSE.
      useSHAP_FILT    =.FALSE.
      useZONAL_FILT   =.FALSE.
      useOPPS         =.FALSE.
      usePP81         =.FALSE.
      useKL10         =.FALSE.
      useMY82         =.FALSE.
      useGGL90        =.FALSE.
      useKPP          =.FALSE.
      useGMRedi       =.FALSE.
      useDOWN_SLOPE   =.FALSE.
      useBBL          =.FALSE.
      useCAL          =.FALSE.
      useEXF          =.FALSE.
      useBulkForce    =.FALSE.
      useEBM          =.FALSE.
      useCheapAML     =.FALSE.
      useAUTODIFF     =.FALSE.
      useGrdchk       =.FALSE.
      useSMOOTH       =.FALSE.
      usePROFILES     =.FALSE.
      useECCO         =.FALSE.
      useCTRL         =.FALSE.
      useSBO          =.FALSE.
      useFLT          =.FALSE.
      usePTRACERS     =.FALSE.
      useGCHEM        =.FALSE.
      useRBCS         =.FALSE.
      useOffLine      =.FALSE.
      useMATRIX       =.FALSE.
      useFRAZIL       =.FALSE.
      useSEAICE       =.FALSE.
      useSALT_PLUME   =.FALSE.
      useShelfIce     =.FALSE.
      useStreamIce    =.FALSE.
      useICEFRONT     =.FALSE.
      useThSIce       =.FALSE.
      useLand         =.FALSE.
      useATM2D        =.FALSE.
      useAIM          =.FALSE.
      useAtm_Phys     =.FALSE.
      useFizhi        =.FALSE.
      useGridAlt      =.FALSE.
      useDiagnostics  =.FALSE.
      useREGRID       =.FALSE.
      useLayers       =.FALSE.
      useMNC          =.FALSE.
      useRunClock     =.FALSE.
      useEMBED_FILES  =.FALSE.
      useMYPACKAGE    =.FALSE.

C-    Adjust default to account for pkgs without run-time On/Off switch
#ifdef ALLOW_AUTODIFF
      useAUTODIFF     =.TRUE.
# ifdef ALLOW_ECCO
      useECCO         =.TRUE.
# endif
# ifdef ALLOW_CTRL
      useCTRL         =.TRUE.
# endif
#endif /* ALLOW_AUTODIFF */

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=PACKAGES)

C      WRITE(msgBuf,'(A)') ' PACKAGES_BOOT: finished reading data.pkg'
C      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &                    SQUEEZE_RIGHT , myThid )

C--   Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C--   packages with hard-coded switch
#ifndef HAVE_NETCDF
      IF (usePROFILES) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** pkg/profiles uses NetCDF',
     &   ' which is unavailable (genmake2 test failed)'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** PACKAGES_BOOT:',
     &                       ' ==> Reset usePROFILES to FALSE'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') ' PACKAGES_BOOT: undef HAVE_NETCDF',
     &                       ' ==> Reset usePROFILES to FALSE'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        usePROFILES=.FALSE.
      ENDIF
#endif

#ifdef ALLOW_CAL
      IF (usePROFILES) useCAL = .TRUE.
      IF (useECCO) useCAL = .TRUE.
#endif

#ifdef ALLOW_CTRL
      IF (useGrdchk) useCTRL = .TRUE.
#endif

C--   packages with an implied switch
      useGAD = tempStepping .OR. saltStepping .OR. usePTRACERS

#ifdef ALLOW_CAL
c     IF (useEXF) useCAL = .TRUE.
C--   Double warning (StdErr & StdOut) after changing useCAL setting
      IF ( useEXF .AND. .NOT. useCAL ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** PACKAGES_BOOT: ',
     &    'useCAL no longer set to T when using EXF (useEXF=T)'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** PACKAGES_BOOT: ',
     &    ' as it used to be before checkpoint66d (2017/02/13)'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** PACKAGES_BOOT: ',
     &    'To continue to use pkg/cal with EXF, need to add:'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** PACKAGES_BOOT: ',
     &    '> useCAL=.TRUE., < in file "data.pkg"'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
#endif /* ALLOW_CAL */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Short summary of which package is compiled and used
C      WRITE(msgBuf,'(2A)') ' PACKAGES_BOOT: On/Off package Summary'
C      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &                    SQUEEZE_RIGHT, myThid )

C----- pkgs with a standard "usePKG" On/Off switch in "data.pkg":
C      WRITE(standardMessageUnit,'(2A)') ' --------  pkgs with a',
C     & ' standard "usePKG" On/Off switch in "data.pkg":  --------'
C#ifdef ALLOW_OBCS
C      CALL PACKAGES_PRINT_MSG( useOBCS,       'OBCS',        ' ' )
C#endif
C#ifdef ALLOW_SHAP_FILT
C      CALL PACKAGES_PRINT_MSG( useSHAP_FILT,  'SHAP_FILT',   ' ' )
C#endif
C#ifdef ALLOW_ZONAL_FILT
C      CALL PACKAGES_PRINT_MSG( useZONAL_FILT, 'ZONAL_FILT',  ' ' )
C#endif
C#ifdef ALLOW_OPPS
C      CALL PACKAGES_PRINT_MSG( useOPPS,       'OPPS',        ' ' )
C#endif
C#ifdef ALLOW_PP81
C      CALL PACKAGES_PRINT_MSG( usePP81,       'PP81',        ' ' )
C#endif
C#ifdef ALLOW_KL10
C      CALL PACKAGES_PRINT_MSG( useKL10,       'KL10',        ' ' )
C#endif
C#ifdef ALLOW_MY82
C      CALL PACKAGES_PRINT_MSG( useMY82,       'MY82',        ' ' )
C#endif
C#ifdef ALLOW_GGL90
C      CALL PACKAGES_PRINT_MSG( useGGL90,      'GGL90',       ' ' )
C#endif
C#ifdef ALLOW_KPP
C      CALL PACKAGES_PRINT_MSG( useKPP,        'KPP',         ' ' )
C#endif
C#ifdef ALLOW_GMREDI
C      CALL PACKAGES_PRINT_MSG( useGMRedi,     'GMRedi',      ' ' )
C#endif
C#ifdef ALLOW_DOWN_SLOPE
C      CALL PACKAGES_PRINT_MSG( useDOWN_SLOPE, 'DOWN_SLOPE',  ' ' )
C#endif
C#ifdef ALLOW_BBL
C      CALL PACKAGES_PRINT_MSG( useBBL,        'BBL',         ' ' )
C#endif
C#ifdef ALLOW_CAL
C      CALL PACKAGES_PRINT_MSG( useCAL,        'CAL',         ' ' )
C#endif
C#ifdef ALLOW_EXF
C      CALL PACKAGES_PRINT_MSG( useEXF,        'EXF',         ' ' )
C#endif
C#ifdef ALLOW_BULK_FORCE
C      CALL PACKAGES_PRINT_MSG(useBulkForce,'BULK_FORCE','useBulkForce')
C#endif
C#ifdef ALLOW_EBM
C      CALL PACKAGES_PRINT_MSG( useEBM,        'EBM',         ' ' )
C#endif
C#ifdef ALLOW_CHEAPAML
C      CALL PACKAGES_PRINT_MSG( useCheapAML,   'CheapAML',    ' ' )
C#endif
C#ifdef ALLOW_AUTODIFF
C      CALL PACKAGES_PRINT_MSG( useAUTODIFF,   'AUTODIFF',    ' ' )
C#endif
C#ifdef ALLOW_GRDCHK
C      CALL PACKAGES_PRINT_MSG( useGrdchk,     'Grdchk',      ' ' )
C#endif
C#ifdef ALLOW_SMOOTH
C      CALL PACKAGES_PRINT_MSG( useSMOOTH,     'SMOOTH',      ' ' )
C#endif
C#ifdef ALLOW_PROFILES
C      CALL PACKAGES_PRINT_MSG( usePROFILES,   'PROFILES',    ' ' )
C#endif
C#ifdef ALLOW_ECCO
C      CALL PACKAGES_PRINT_MSG( useECCO,       'ECCO',        ' ' )
C#endif
C#ifdef ALLOW_CTRL
C      CALL PACKAGES_PRINT_MSG( useCTRL,       'CTRL',        ' ' )
C#endif
C#ifdef ALLOW_SBO
C      CALL PACKAGES_PRINT_MSG( useSBO,        'SBO',         ' ' )
C#endif
C#ifdef ALLOW_FLT
C      CALL PACKAGES_PRINT_MSG( useFLT,        'FLT',         ' ' )
C#endif
C#ifdef ALLOW_PTRACERS
C      CALL PACKAGES_PRINT_MSG( usePTRACERS,   'PTRACERS',    ' ' )
C#endif
C#ifdef ALLOW_LONGSTEP
C      CALL PACKAGES_PRINT_MSG( usePTRACERS,'LONGSTEP','usePTRACERS' )
C#endif
C#ifdef ALLOW_GCHEM
C      CALL PACKAGES_PRINT_MSG( useGCHEM,      'GCHEM',       ' ' )
C#endif
C#ifdef ALLOW_RBCS
C      CALL PACKAGES_PRINT_MSG( useRBCS,       'RBCS',        ' ' )
C#endif
C#ifdef ALLOW_OFFLINE
C      CALL PACKAGES_PRINT_MSG( useOffLine,    'OffLine',     ' ' )
C#endif
C#ifdef ALLOW_MATRIX
C      CALL PACKAGES_PRINT_MSG( useMATRIX,     'MATRIX',      ' ' )
C#endif
C#ifdef ALLOW_FRAZIL
C      CALL PACKAGES_PRINT_MSG( useFRAZIL,     'FRAZIL',      ' ' )
C#endif
C#ifdef ALLOW_SEAICE
C      CALL PACKAGES_PRINT_MSG( useSEAICE,     'SEAICE',      ' ' )
C#endif
C#ifdef ALLOW_SALT_PLUME
C      CALL PACKAGES_PRINT_MSG( useSALT_PLUME, 'SALT_PLUME',  ' ' )
C#endif
C#ifdef ALLOW_SHELFICE
C      CALL PACKAGES_PRINT_MSG( useShelfIce,   'ShelfIce',    ' ' )
C#endif
C#ifdef ALLOW_STREAMICE
C      CALL PACKAGES_PRINT_MSG( useStreamIce,  'StreamIce',   ' ' )
C#endif
C#ifdef ALLOW_ICEFRONT
C      CALL PACKAGES_PRINT_MSG( useICEFRONT,   'ICEFRONT',    ' ' )
C#endif
C#ifdef ALLOW_THSICE
C      CALL PACKAGES_PRINT_MSG( useThSIce,     'ThSIce',      ' ' )
C#endif
C#ifdef ALLOW_LAND
C      CALL PACKAGES_PRINT_MSG( useLand,       'Land',        ' ' )
C#endif
C#ifdef ALLOW_ATM2D
C      CALL PACKAGES_PRINT_MSG( useATM2D,      'ATM2D',       ' ' )
C#endif
C#ifdef ALLOW_AIM_V23
C      CALL PACKAGES_PRINT_MSG( useAIM,        'aim_v23', 'useAIM' )
C#endif
C#ifdef ALLOW_ATM_PHYS
C      CALL PACKAGES_PRINT_MSG( useAtm_Phys,   'Atm_Phys',    ' ' )
C#endif
C#ifdef ALLOW_FIZHI
C      CALL PACKAGES_PRINT_MSG( useFizhi,      'Fizhi',       ' ' )
C#endif
C#ifdef ALLOW_GRIDALT
C      CALL PACKAGES_PRINT_MSG( useGridAlt,    'GridAlt',     ' ' )
C#endif
C#ifdef ALLOW_DIAGNOSTICS
C      CALL PACKAGES_PRINT_MSG( useDiagnostics,'Diagnostics', ' ' )
C#endif
C#ifdef ALLOW_REGRID
C      CALL PACKAGES_PRINT_MSG( useREGRID,     'REGRID',      ' ' )
C#endif
C#ifdef ALLOW_LAYERS
C      CALL PACKAGES_PRINT_MSG( useLayers,     'Layers',      ' ' )
C#endif
C#ifdef ALLOW_MNC
C      CALL PACKAGES_PRINT_MSG( useMNC,        'MNC',         ' ' )
C#endif
C#ifdef ALLOW_RUNCLOCK
C      CALL PACKAGES_PRINT_MSG( useRunClock,   'RunClock',    ' ' )
C#endif
C#ifdef ALLOW_EMBED_FILES
C      CALL PACKAGES_PRINT_MSG( useEMBED_FILES,'EMBED_FILES', ' ' )
C#endif
C#ifdef ALLOW_MYPACKAGE
C      CALL PACKAGES_PRINT_MSG( useMYPACKAGE,  'MYPACKAGE',   ' ' )
C#endif

C----- pkgs without standard "usePKG" in "data.pkg":
C      WRITE(standardMessageUnit,'(2A)') ' -------- pkgs without',
C     & ' standard "usePKG" On/Off switch in "data.pkg":  --------'
C#ifdef ALLOW_GENERIC_ADVDIFF
C      CALL PACKAGES_PRINT_MSG( useGAD, 'GENERIC_ADVDIFF', 'useGAD' )
C#endif
C#ifdef ALLOW_MOM_COMMON
C      CALL PACKAGES_PRINT_MSG( momStepping,'MOM_COMMON','momStepping' )
C#endif
C#ifdef ALLOW_MOM_VECINV
C      locFlag = momStepping .AND. vectorInvariantMomentum
C      CALL PACKAGES_PRINT_MSG( locFlag, 'MOM_VECINV',
C     &                                  '+vectorInvariantMomentum' )
C#endif
C#ifdef ALLOW_MOM_FLUXFORM
C      locFlag = momStepping .AND. .NOT.vectorInvariantMomentum
C      CALL PACKAGES_PRINT_MSG( locFlag, 'MOM_FLUXFORM',
C     &                                  '& not vectorInvariantMom' )
C#endif
C#ifdef ALLOW_CD_CODE
C      CALL PACKAGES_PRINT_MSG( useCDscheme, 'CD_CODE', 'useCDscheme' )
C#endif
C#ifdef ALLOW_MONITOR
C      locFlag = monitorFreq.GT.0.
C      CALL PACKAGES_PRINT_MSG( locFlag, 'MONITOR', 'monitorFreq > 0.' )
C#endif
C#ifdef ALLOW_TIMEAVE
C      locFlag = taveFreq.GT.0.
C      CALL PACKAGES_PRINT_MSG( locFlag,  'TIMEAVE', 'taveFreq > 0.' )
C#endif
C#ifdef ALLOW_DEBUG
C      CALL PACKAGES_PRINT_MSG( debugMode, 'DEBUG', 'debugMode' )
C#endif
C#ifdef ALLOW_COMPON_COMMUNIC
C      CALL PACKAGES_PRINT_MSG( useCoupler, 'COMPON_COMMUNIC',
C     &                         'useCoupler' )
C#endif
C#ifdef ALLOW_ATM_COMPON_INTERF
C      CALL PACKAGES_PRINT_MSG( useCoupler, 'ATM_COMPON_INTERF',
C     &                         'useCoupler' )
C#endif
C#ifdef ALLOW_OCN_COMPON_INTERF
C      CALL PACKAGES_PRINT_MSG( useCoupler, 'OCN_COMPON_INTERF',
C     &                         'useCoupler' )
C#endif
CC----- pkgs without a run-time switch:
C      locFlag = .TRUE.
C#ifdef ALLOW_ATM_OCN_COUPLER
C      CALL PACKAGES_PRINT_MSG( locFlag, 'ATM_OCN_COUPLER', 'Used' )
C#endif
C#ifdef ALLOW_EXCH2
C      CALL PACKAGES_PRINT_MSG( locFlag, 'EXCH2',      'Used' )
C#endif
C#ifdef ALLOW_RW
C      CALL PACKAGES_PRINT_MSG( locFlag, 'RW',        'Used' )
C#endif
C#ifdef ALLOW_MDSIO
C      CALL PACKAGES_PRINT_MSG( locFlag, 'MDSIO',     'Used' )
C#endif
C#ifdef ALLOW_SHOWFLOPS
C      CALL PACKAGES_PRINT_MSG( locFlag, 'SHOWFLOPS', 'Used' )
C#endif
CC----- AD related pkgs:
C#ifdef ALLOW_ADMTLM
C      CALL PACKAGES_PRINT_MSG( locFlag, 'ADMTLM',     'Used' )
C#endif
C#ifdef ALLOW_AUTODIFF
C      CALL PACKAGES_PRINT_MSG( locFlag, 'AUTODIFF',   'Used' )
C#endif
C#ifdef ALLOW_OPENAD
C      CALL PACKAGES_PRINT_MSG( locFlag, 'OPENAD',     'Used' )
C#endif
C#ifdef ALLOW_COST
C      CALL PACKAGES_PRINT_MSG( locFlag, 'COST',       'Used' )
C#endif
CC----- mysterious pkgs:
C#ifdef ALLOW_SPHERE
C      CALL PACKAGES_PRINT_MSG( locFlag, 'SPHERE',     'None' )
C#endif
C#ifdef ALLOW_CHRONOS
C      CALL PACKAGES_PRINT_MSG( locFlag, 'CHRONOS',    'None' )
C#endif
C#ifdef ALLOW_ATM_COMMON
C      CALL PACKAGES_PRINT_MSG( locFlag, 'ATM_COMMON', 'None' )
C#endif
C
C      WRITE(msgBuf,'(2A)') ' PACKAGES_BOOT: End of package Summary'
C      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &                    SQUEEZE_RIGHT, myThid )
C      WRITE(msgBuf,'(A)') ' '
C      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &                    SQUEEZE_RIGHT, myThid )
C
      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
