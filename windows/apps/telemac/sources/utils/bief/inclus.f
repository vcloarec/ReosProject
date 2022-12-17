!                   ***********************
                    LOGICAL FUNCTION INCLUS
!                   ***********************
!
     &( C1 , C2 )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CHECKS IF A CHARACTER STRING IS COMPRISED IN ANOTHER.
!+                INCLUS=.TRUE. MEANS 'C2 IS COMPRISED IN C1'.
!
!history  J.M. HERVOUET (LNH)
!+        17/08/94
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
!| C1             |-->| CHARACTER STRING WHERE C2 IS LOOKED FOR
!| C2             |-->| CHARACTER STRING LOOKED FOR INTO C1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      CHARACTER(LEN=*) C1 , C2
!
      INTEGER I,LC1,LC2
!
      INTRINSIC LEN
!
!-----------------------------------------------------------------------
!
      INCLUS = .FALSE.
!
      LC1 = LEN(C1)
      LC2 = LEN(C2)
      IF(LC2.GT.LC1) GO TO 1000
!
      I = 0
10    I = I + 1
      IF(I.GT.LC1-LC2+1) GO TO 1000
!
      IF(C1(I:I+LC2-1).NE.C2(1:LC2)) GO TO 10
!
      INCLUS = .TRUE.
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
