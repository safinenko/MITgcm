CBOP
C !ROUTINE: GAD.h

C !INTERFACE:
C #include "GAD.h"

C !DESCRIPTION:
C Contains enumerated constants for distinguishing between different
C advection schemes and tracers.
C
C Unfortunately, there is no easy way to make use of the
C tokens in namelist input so for now we have to enter the
C tokens value into "data" (ie. 2 for 2nd order etc.)

C !USES:

C !DEFINED PARAMETERS:

C ENUM_UPWIND_1RST :: 1rst Order Upwind
      INTEGER, PARAMETER :: ENUM_UPWIND_1RST = 1

C ENUM_CENTERED_2ND :: Centered 2nd order
      INTEGER, PARAMETER :: ENUM_CENTERED_2ND = 2

C ENUM_UPWIND_3RD :: 3rd order upwind
      INTEGER, PARAMETER :: ENUM_UPWIND_3RD = 3

C ENUM_CENTERED_4TH :: Centered 4th order
      INTEGER, PARAMETER :: ENUM_CENTERED_4TH = 4

C ENUM_DST2 :: 2nd Order Direct Space and Time (= Lax-Wendroff)
      INTEGER, PARAMETER :: ENUM_DST2 = 20

C ENUM_FLUX_LIMIT :: Non-linear flux limiter
      INTEGER, PARAMETER :: ENUM_FLUX_LIMIT = 77

C ENUM_DST3 :: 3rd Order Direst Space and Time
      INTEGER, PARAMETER :: ENUM_DST3 = 30

C ENUM_DST3_FLUX_LIMIT :: 3-DST flux limited
      INTEGER, PARAMETER :: ENUM_DST3_FLUX_LIMIT = 33

C ENUM_OS7MP :: 7th Order One Step method with Monotonicity Preserving Limiter
      INTEGER, PARAMETER :: ENUM_OS7MP = 7

C ENUM_SOM_PRATHER :: 2nd Order-Moment Advection Scheme, Prather, 1986
      INTEGER, PARAMETER :: ENUM_SOM_PRATHER = 80

C ENUM_SOM_LIMITER :: 2nd Order-Moment Advection Scheme, Prather Limiter
      INTEGER, PARAMETER :: ENUM_SOM_LIMITER = 81

C ENUM_PPM_NULL :: piecewise parabolic method with "null" limiter
      INTEGER, PARAMETER :: ENUM_PPM_NULL_LIMIT = 40

C ENUM_PPM_MONO :: piecewise parabolic method with "mono" limiter
      INTEGER, PARAMETER :: ENUM_PPM_MONO_LIMIT = 41

C ENUM_PPM_WENO :: piecewise parabolic method with "weno" limiter
      INTEGER, PARAMETER :: ENUM_PPM_WENO_LIMIT = 42

C ENUM_PQM_NULL :: piecewise quartic method with "null" limiter
      INTEGER, PARAMETER :: ENUM_PQM_NULL_LIMIT = 50

C ENUM_PQM_MONO :: piecewise quartic method with "mono" limiter
      INTEGER, PARAMETER :: ENUM_PQM_MONO_LIMIT = 51

C ENUM_PQM_WENO :: piecewise quartic method with "weno" limiter
      INTEGER, PARAMETER :: ENUM_PQM_WENO_LIMIT = 52

C GAD_Scheme_MaxNum :: maximum possible number for an advection scheme
      INTEGER, PARAMETER :: GAD_Scheme_MaxNum = 100 

C nSOM :: number of 1rst & 2nd Order-Moments: 1+1 (1D), 2+3 (2D), 3+6 (3D)
      INTEGER, PARAMETER :: nSOM = 3+6 

C oneSixth :: Third/fourth order interpolation factor
      _RL, PARAMETER :: oneSixth = 1.D0/6.D0

C loop range for computing vertical advection tendency
C  iMinAdvR,iMaxAdvR  :: 1rst index (X-dir) loop range for vertical advection
C  jMinAdvR,jMaxAdvR  :: 2nd  index (Y-dir) loop range for vertical advection
      INTEGER, PARAMETER :: iMinAdvR = 1, iMaxAdvR = sNx
      INTEGER, PARAMETER :: jMinAdvR = 1, jMaxAdvR = sNy

C Differentiate between tracers (needed for KPP - arrgh!!!)
cph                              and GMRedi arrgh*arrgh!!!)
cph  indices are used for TAF key computations, so need to
cph  running from 1, 2, ...
c
C GAD_TEMPERATURE :: temperature
      INTEGER, PARAMETER :: GAD_TEMPERATURE = 1
C GAD_SALINITY :: salinity
      INTEGER, PARAMETER :: GAD_SALINITY = 2
C GAD_TR1 :: passive tracer 1
      INTEGER, PARAMETER :: GAD_TR1 = 3
CEOP

C--   COMMON /GAD_PARM_C/ Character parameters for GAD pkg routines
C      somSfx       :: 1rst & 2nd Order moment suffix
      CHARACTER*2 somSfx(nSOM)
      COMMON /GAD_PARM_C/
     & somSfx

C--   COMMON /GAD_PARM_I/ Integer parameters for GAD pkg routines
C GAD_OlMinSize     :: overlap minimum size for GAD routines
C           1: min required; 2: to add to current min; 3: factor to apply
      INTEGER GAD_OlMinSize(3)
      COMMON /GAD_PARM_I/
     &        GAD_OlMinSize

C--   COMMON /GAD_PARM_L/ Logical parameters for GAD pkg routines
C tempSOM_Advection :: set to T if using 2nd-Order Moment advection for Temp
C saltSOM_Advection :: set to T if using 2nd-Order Moment advection for Salt
C tempMultiDimAdvec :: set to T if using multi-dim advection for Temp
C saltMultiDimAdvec :: set to T if using multi-dim advection for Salt
C AdamsBashforthGt  :: apply Adams-Bashforth extrapolation on T tendency (=Gt)
C AdamsBashforthGs  :: apply Adams-Bashforth extrapolation on S tendency (=Gs)
C AdamsBashforth_T  :: apply Adams-Bashforth extrapolation on Pot.Temp.
C AdamsBashforth_S  :: apply Adams-Bashforth extrapolation on Salinity
      LOGICAL tempSOM_Advection
      LOGICAL saltSOM_Advection
      LOGICAL tempMultiDimAdvec
      LOGICAL saltMultiDimAdvec
      LOGICAL AdamsBashforthGt
      LOGICAL AdamsBashforthGs
      LOGICAL AdamsBashforth_T
      LOGICAL AdamsBashforth_S
      COMMON /GAD_PARM_L/
     & tempSOM_Advection, saltSOM_Advection,
     & tempMultiDimAdvec, saltMultiDimAdvec,
     & AdamsBashforthGt, AdamsBashforthGs,
     & AdamsBashforth_T, AdamsBashforth_S

      _RL SmolarkiewiczMaxFrac
      COMMON /GAD_SMOL/ SmolarkiewiczMaxFrac

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
