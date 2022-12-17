!                   *****************
                    SUBROUTINE VECLE4
!                   *****************
!
     & (LV,IKLE,NELEM,NELMAX,NPOIN,V)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DETERMINES THE LENGTH OF A VECTOR WITHOUT BACK
!+                DEPENDENCIES FOR LOOPS ON THE ELEMENTS.
!+
!+            ONLY LOOKS FOR VALUES :
!+                1 , 64 , 128 , 256 , 512 , 1024.
!+
!+            THE PRINCIPLE IS TO PERFORM, IN SCALAR AND VECTOR
!+                MODE, AN ALGORITHM WHICH COMPUTES THE NUMBER OF
!+                ADJACENT ELEMENTS AT EACH POINT.
!+
!+            ELEMENT CONSIDERED: QUADRILATERAL.
!
!warning  IN VECTOR MODE WITH DEPENDENCIES, THE RESULT IS WRONG
!
!history  J-M HERVOUET (LNH)
!+        11/03/94
!+        V5P1
!+   ORIGINAL IDEA FROM J.-P. GREGOIRE
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
!| IKLE           |-->| CONNECTIVITY TABLE.
!| LV             |-->| VECTOR LENGTH OF THE MACHINE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| V              |-->| ARRAY OF SIZE NPOIN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VECLE4 => VECLE4
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(INOUT) :: LV
      INTEGER, INTENT(IN)    :: NELEM,NELMAX,NPOIN
      INTEGER, INTENT(IN)    :: IKLE(NELMAX,4)
!
      DOUBLE PRECISION, INTENT(INOUT) :: V(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IB
!
!-----------------------------------------------------------------------
!
      LV = 1024
!
5     CONTINUE
!
!  INITIALISES V AT 0
!
      CALL OV('X=C     ', X=V, C=0.D0, DIM1=NPOIN)
!
!  SCALAR MODE
!
      DO IELEM = 1 , NELEM
        V(IKLE(IELEM,1)) = V(IKLE(IELEM,1)) + 1.D0
        V(IKLE(IELEM,2)) = V(IKLE(IELEM,2)) + 1.D0
        V(IKLE(IELEM,3)) = V(IKLE(IELEM,3)) + 1.D0
        V(IKLE(IELEM,4)) = V(IKLE(IELEM,4)) + 1.D0
      ENDDO ! IELEM
!
!  VECTOR MODE WITH FORCED VECTORISATION
! (FUJITSU COMMANDS, THEN CRAY COMMANDS)
!
      DO IB = 1,(NELEM+LV-1)/LV
!VOCL LOOP,NOVREC
!DIR$ IVDEP
      DO IELEM = 1+(IB-1)*LV , MIN(NELEM,IB*LV)
        V(IKLE(IELEM,1)) = V(IKLE(IELEM,1)) - 1.D0
        V(IKLE(IELEM,2)) = V(IKLE(IELEM,2)) - 1.D0
        V(IKLE(IELEM,3)) = V(IKLE(IELEM,3)) - 1.D0
        V(IKLE(IELEM,4)) = V(IKLE(IELEM,4)) - 1.D0
      ENDDO ! IELEM
      ENDDO ! IB
!
!-----------------------------------------------------------------------
!
      IF(DOT(NPOIN,V,V).GT.0.5D0.AND.LV.NE.1) THEN
        LV = LV/2
        IF(LV.LT.64) THEN
          LV = 1
          GO TO 1000
        ENDIF
        GO TO 5
      ENDIF
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
