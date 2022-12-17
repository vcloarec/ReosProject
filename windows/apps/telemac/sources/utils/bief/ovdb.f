!                   ***************
                    SUBROUTINE OVDB
!                   ***************
!
     & ( OP , X , Y , Z , C , NBOR , NPTFR )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON VECTORS.
!code
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON VECTORS X,Y AND Z AND CONSTANT C.
!+
!+   HERE X IS A VECTOR DEFINED IN THE DOMAIN.
!+   Y AND Z ARE VECTORS DEFINED ON THE BOUNDARY.
!+
!+   THE RESULT IS VECTOR X.
!+
!+   OP = 'X=Y     '     :  COPIES Y IN X
!+   OP = 'X=+Y    '     :  IDEM
!+   OP = 'X=X+Y   '     :  ADDS Y TO X
!+   OP = 'X=X+CY  '     :  ADDS C.Y TO X
!+   OP = 'X=X-Y   '     :  SUBTRACTS Y FROM X
!+   OP = 'X=CY    '     :  MULTIPLIES Y BY C
!+   OP = 'X=XY    '     :  MULTIPLIES Y BY X
!
!history  J-M HERVOUET (LNH)
!+        06/01/04
!+        V5P4
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
!| C              |-->| A GIVEN CONSTANT
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| OP             |-->| STRING INDICATING THE OPERATION TO BE DONE
!| X              |<--| RESULT (A BIEF_OBJ STRUCTURE)
!| Y              |-->| TO BE USED IN THE OPERATION
!| Z              |-->| TO BE USED IN THE OPERATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPTFR,NBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN)    :: Y(*),Z(*),C
!
      CHARACTER(LEN=8), INTENT(IN) :: OP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K
!
!-----------------------------------------------------------------------
!
      IF(OP(1:8).EQ.'X=Y     '.OR.
     &   OP(1:8).EQ.'X=+Y    ') THEN
!
        DO K=1,NPTFR
          X(NBOR(K)) = Y(K)
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+Y   ') THEN
!
        DO K=1,NPTFR
          X(NBOR(K)) = X(NBOR(K)) + Y(K)
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=Y+Z   ') THEN
!
        DO K=1,NPTFR
          X(NBOR(K)) = Y(K) + Z(K)
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+CY  ') THEN
!
        DO K=1,NPTFR
          X(NBOR(K)) = X(NBOR(K)) + C * Y(K)
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X-Y   ') THEN
!
        DO K=1,NPTFR
          X(NBOR(K)) = X(NBOR(K)) - Y(K)
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X-YZ  ') THEN
!
        DO K=1,NPTFR
          X(NBOR(K)) = X(NBOR(K)) - Y(K)*Z(K)
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=XY    ') THEN
!
        DO K=1,NPTFR
          X(NBOR(K)) = X(NBOR(K)) * Y(K)
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=CY    ') THEN
!
        DO K=1,NPTFR
          X(NBOR(K)) = C * Y(K)
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,1001) OP
1001    FORMAT(1X,'OVDB (BIEF) : UNKNOWN OPERATION: ',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
