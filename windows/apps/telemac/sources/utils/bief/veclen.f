!                   *****************
                    SUBROUTINE VECLEN
!                   *****************
!
     & (LV,NDP,IKLE,NELEM,NELMAX,NPOIN,V)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DETERMINES THE LENGTH OF A VECTOR WITHOUT BACK
!+                DEPENDENCIES FOR LOOPS ON THE ELEMENTS.
!+
!+            ONLY LOOKS FOR VALUES :
!+                1, 64, 128, 256, 512, OR 1024.
!+
!+            THE PRINCIPLE IS TO PERFORM, IN SCALAR AND VECTOR
!+                MODE, AN ALGORITHM WHICH COMPUTES THE NUMBER OF
!+                ADJACENT ELEMENTS AT EACH POINT.
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
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| V              |-->| ARRAY OF SIZE NPOIN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VECLEN => VECLEN
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(INOUT) :: LV
      INTEGER, INTENT(IN)    :: NELEM,NELMAX,NDP,NPOIN
      INTEGER, INTENT(IN)    :: IKLE(NELMAX,NDP)
!
      DOUBLE PRECISION, INTENT(INOUT) :: V(NPOIN)
!
!-----------------------------------------------------------------------
!
      IF(NDP.EQ.3) THEN
        CALL VECLE3(LV,IKLE,NELEM,NELMAX,NPOIN,V)
      ELSEIF(NDP.EQ.4) THEN
        CALL VECLE4(LV,IKLE,NELEM,NELMAX,NPOIN,V)
      ELSEIF(NDP.EQ.6) THEN
        CALL VECLE6(LV,IKLE,NELEM,NELMAX,NPOIN,V)
      ELSE
        WRITE(LU,60) NDP
60      FORMAT(1X,'VECLEN : UNEXPECTED VALUE OF NDP: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
