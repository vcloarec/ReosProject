!                   *****************
                    SUBROUTINE SOLAUX
!                   *****************
!
     &(IPT, TB,TBB,ITB,ITBB,S)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    TB IS A BLOCK OF VECTORS AND TBB A BLOCK OF BLOCKS.
!+                SOLAUX PREPARES A TBB BLOCK BY FILLING IT IN WITH
!+                MAX(1,S) VECTORS FROM TB.
!+
!+            THE OUTPUT ADDRESS IS AN ADDRESS RELATIVE TO TBB.
!+
!+            IN PRACTICE TB OBJECTS ARE VECTORS.
!
!history  J.M. HERVOUET (LNH)
!+        01/02/95
!+        V5P1
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
!| IPT            |<--| POINTER IN TBB OF THE BIEF_OBJ STRUCTURE ASKED
!| ITB            |-->| FIRST FREE VECTOR IN TB
!| ITBB           |-->| FIRST FREE BLOCK IN TBB
!| S              |-->| SIZE OF SYSTEM 1: 1 MATRIX
!|                |   |                2: 4 MATRICES
!|                |   |                3: 9 MATRICES
!| TB             |-->| BLOCK OF WORKING BIEF_OBJ STRUCTURES
!| TBB            |-->| BLOCK OF BLOCKS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SOLAUX => SOLAUX
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: S
      INTEGER, INTENT(INOUT) :: ITB,IPT,ITBB
!
!-----------------------------------------------------------------------
!
!  STRUCTURES OF BLOCKS OF WORKING ARRAYS
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TB,TBB
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,IAD,MAXTB,MAXTBB
!
!-----------------------------------------------------------------------
!
      MAXTBB = TBB%N
      IF(ITBB.GT.MAXTBB) THEN
        WRITE(LU,31) ITBB
        CALL PLANTE(1)
        STOP
      ENDIF
      IPT = ITBB
      MAXTB = TB%N
      ITBB = ITBB + 1
!     REINITIALISES THE BLOCK
      DO K=1,TBB%ADR(IPT)%P%N
        NULLIFY(TBB%ADR(IPT)%P%ADR(K)%P)
      ENDDO
      TBB%ADR(IPT)%P%N=0
      DO K = 1 , MAX(S,1)
        IF(ITB.GT.MAXTB) THEN
          WRITE(LU,11) ITB + MAX(S,1) - K + 1
          CALL PLANTE(1)
          STOP
        ENDIF
        IAD=ITB
        CALL ADDBLO(TBB%ADR(IPT)%P,TB%ADR(IAD)%P)
        ITB = ITB + 1
      ENDDO ! K
!
!-----------------------------------------------------------------------
!
11    FORMAT(1X,'SOLAUX (BIEF): INSUFFICIENT NUMBER OF ARRAYS',/,
     &       1X,'IN BLOCK TB. MINIMUM REQUIRED:',1I4)
31    FORMAT(1X,'SOLAUX (BIEF): INSUFFICIENT NUMBER OF BLOCKS',/,
     &       1X,'IN BLOCK TBB. MINIMUM REQUIRED:',1I4)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
