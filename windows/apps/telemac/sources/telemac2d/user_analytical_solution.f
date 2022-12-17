!                   ***********************************
                    SUBROUTINE USER_ANALYTICAL_SOLUTION
!                   ***********************************
!
     &(F,MESH,FINEMESH,NELMAX,NPOIN,CORRESP,RLEVELS,NLEVEL,
     & IKLE,FINEIKLE,ANALYTICAL)
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!Brief    Analytical solution for error_computation
!
!history  A. LEROY (LNHE) & J-M HERVOUET (jubilado)
!+        26/09/2017
!+        V7P3
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |<--| FIELD FOR ERROR COMPUTATION
!| FINEMESH       |<--| FINE MESH ON WHICH ERRORS ARE COMPUTED
!| MESH           |<--| COARSER MESH CONTAINING THE SIMULATION RESULTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)       :: NPOIN
      DOUBLE PRECISION, INTENT(IN)       :: F(NPOIN)
      TYPE(BIEF_MESH) , INTENT(IN)       :: MESH
      TYPE(BIEF_MESH) , INTENT(INOUT)    :: FINEMESH
      INTEGER         , INTENT(IN)       :: NELMAX
      INTEGER         , INTENT(IN)       :: RLEVELS
      INTEGER         , INTENT(IN)       :: NLEVEL
      INTEGER         , INTENT(IN)       :: CORRESP(NELMAX,RLEVELS)
!     WARNING, THERE SHOULD BE NDP INSTEAD OF 3
      INTEGER         , INTENT(IN)       :: IKLE(NELMAX*3)
      INTEGER         , INTENT(IN)       :: FINEIKLE(NELMAX*3)
      DOUBLE PRECISION, INTENT(INOUT)    :: ANALYTICAL(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!

      WRITE(LU,*) 'ANALYTICAL SOLUTION MUST BE COMPUTED USING USER',
     &            'FORTRAN SUBROUTINE USER_ANALYTICAL_SOLUTION.'
      WRITE(LU,*) 'SEE TELEMAC2D/CONVERGENCE FOR AN EXAMPLE'
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
