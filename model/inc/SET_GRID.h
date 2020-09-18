C

CBOP
C     !ROUTINE: SET_GRID.h
C     !INTERFACE:
C     #include SET_GRID.h

C     !DESCRIPTION:
C     Header file holding some 1-D arrays which are used to set-up the model grid.
C==========================================================================
C  IMPORTANT : This header file can only be included after the options
C              file PACKAGES_CONFIG.h and the header file W2_EXCH2_SIZE.h
C==========================================================================

CEOP

C    grid_maxNx :: Maximum length of delX vector
C    grid_maxNy :: Maximum length of delY vector
#ifdef ALLOW_EXCH2
      INTEGER, PARAMETER :: grid_maxNx = W2_maxXStackNx
      INTEGER, PARAMETER :: grid_maxNy = W2_maxYStackNy
#else  /* ALLOW_EXCH2 */
      INTEGER, PARAMETER :: grid_maxNx = Nx
      INTEGER, PARAMETER :: grid_maxNy = Ny
#endif /* ALLOW_EXCH2 */

C--   COMMON /SET_GRID_R/ "Real" valued parameter used to set-up the model gri
C     delX      :: Separation between cell faces (m) or (deg), depending on type
C     delY         type of horizontal grid choice (cartesian/spherical-polar ...)
      COMMON /SET_GRID_R/
     & delX, delY

      _RL delX(grid_maxNx)
      _RL delY(grid_maxNy)

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
