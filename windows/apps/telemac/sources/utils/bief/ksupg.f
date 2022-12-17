!                   ****************
                    SUBROUTINE KSUPG
!                   ****************
!
     &(KX,KY,XMUL,U,V,MESH)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES A VECTOR, USED BY THE METHOD:
!+                STREAMLINE UPWIND PETROV GALERKIN (SUPG)
!+                WITH AN OFF-CENTERING OF 1.
!code
!+                    DX   U
!+             KX = -----------
!+                   2 NORM(U)
!+
!+                    DY   V
!+             KY = -----------
!+                   2 NORM(U)
!
!history  J-M HERVOUET (LNH)
!+        08/12/94
!+        V5P6
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
!| KX             |-->| FIRST COMPONENT OF RESULTING VECTOR
!| KY             |-->| SECOND COMPONENT OF RESULTING VECTOR
!| MESH           |-->| MESH STRUCTURE
!| U              |-->| FIRST COMPONENT OF VELOCITY
!| V              |-->| SECOND COMPONENT OF VELOCITY
!| XMUL           |-->| MULTIPLICATION COEFICIENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_KSUPG => KSUPG
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: KX,KY
      TYPE(BIEF_OBJ), INTENT(IN)    :: U,V
!
      DOUBLE PRECISION, INTENT(IN)  :: XMUL
!
!-----------------------------------------------------------------------
!
      IF(U%ELM.EQ.11.OR.U%ELM.EQ.12.OR.U%ELM.EQ.13) THEN
!
        CALL KSPG11(KX%R,KY%R,MESH%XEL%R,MESH%YEL%R,U%R,V%R,
     &              MESH%IKLE%I,MESH%NELEM,MESH%NELMAX,XMUL)
!
!  ELEMENT NOT IMPLEMENTED: ERROR
!
      ELSE
        WRITE(LU,101) U%ELM
101     FORMAT(1X,'KSUPG (BIEF): U%ELM = ',1I6,' ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
