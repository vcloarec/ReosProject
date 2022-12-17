!                   *****************
                    SUBROUTINE HPROPA
!                   *****************
!
     &(HPROP ,HN,H,PROLIN,HAULIN,TETA,NSOUSI)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES PROPAGATION DEPTH, I.E. DEPTH IN DIVERGENCE TERM
!         OF CONTINUITY EQUATION.
!
!history  J-M HERVOUET (LNHE)
!+        16/07/2007
!+        V5P8
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| H              |<--| WATER DEPTH AT TIME T(N+1)
!| HAULIN         |-->| MEAN DEPTH FOR LINEARISATION
!| HN             |<--| WATER DEPTH AT TIME T(N)
!| HPROP          |<--| WATER DEPTH FOR PROPAGATION
!| NSOUSI         |-->| NUMBER OF SUB-ITERATIONS
!| PROLIN         |-->| KEY-WORD "LINEARISED PROPAGATON"
!| TETA           |-->| IMPLICITATION COEFFICIENT ON DEPTH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NSOUSI
      LOGICAL, INTENT(IN)           :: PROLIN
      DOUBLE PRECISION, INTENT(IN)  :: TETA,HAULIN
      TYPE(BIEF_OBJ), INTENT(IN)    :: HN,H
      TYPE(BIEF_OBJ), INTENT(INOUT) :: HPROP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(PROLIN) THEN
        CALL OS( 'X=C     ' , X=HPROP , C=HAULIN    )
      ELSEIF(NSOUSI.EQ.1) THEN
        CALL OS( 'X=Y     ' , X=HPROP , Y=HN )
      ELSE
        CALL OS( 'X=CY    ' , X=HPROP , Y=HN , C=1.D0-TETA )
        CALL OS( 'X=X+CY  ' , X=HPROP , Y=H  , C= TETA )
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CLIPS HPROP
!
      IF(.NOT.PROLIN) THEN
        CALL OS('X=+(Y,C)',X=HPROP,Y=HPROP,C=0.D0)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
