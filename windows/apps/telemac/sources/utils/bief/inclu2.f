!                   ***********************
                    LOGICAL FUNCTION INCLU2
!                   ***********************
!
     &( C1 , C2 )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CHECKS IF A WORD IS COMPRISED IN A LIST OF WORDS.
!+                INCLU2=.TRUE. MEANS 'WORD C2 IS COMPRISED IN LIST C1'.
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
!| C1             |-->| LIST OF WORDS SEPARATED BY A CHARACTER ELSE THAN
!|                |   | A-Z AND 0-9
!| C2             |-->| WORD LOOKED FOR IN C1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      CHARACTER(LEN=*) C1 , C2
!
      INTEGER I,IC1,LC1,LC2,IMAX
!
      LOGICAL FLAG
!
      INTRINSIC LEN
!
!-----------------------------------------------------------------------
!
      INCLU2 = .FALSE.
!
      LC1 = LEN(C1)
      LC2 = LEN(C2)
      IMAX = LC1-LC2
!
      IF(IMAX.GE.0) THEN
!
        DO I = 0,IMAX
          IF(C1(I+1:I+LC2).EQ.C2(1:LC2)) THEN
            FLAG = .TRUE.
            IF (I.NE.0) THEN
              IC1 = ICHAR(C1(I:I))
              IF ((IC1.GE.48.AND.IC1.LE.57).OR.
     &            (IC1.GE.65.AND.IC1.LE.90)) FLAG = .FALSE.
            ENDIF
            IF (I.NE.IMAX) THEN
              IC1 = ICHAR(C1(I+LC2+1:I+LC2+1))
              IF ((IC1.GE.48.AND.IC1.LE.57).OR.
     &            (IC1.GE.65.AND.IC1.LE.90)) FLAG = .FALSE.
            ENDIF
            INCLU2 = INCLU2.OR.FLAG
          ENDIF
        ENDDO ! I
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
