!                   ******************************
                    INTEGER FUNCTION BIEF_DIM2_EXT
!                   ******************************
!
     &(IELM1,IELM2,STO,TYPEXT,MESH)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GIVES THE SECOND DIMENSION OF A MATRICE'S EXTRA-DIAGONAL
!+                TERMS.
!
!history  J-M HERVOUET (LNH)
!+        12/12/94
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
!| IELM1          |-->| TYPE OF LINE ELEMENT
!| IELM2          |-->| TYPE OF COLUMN ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| STO            |-->| TYPE OF STORAGE
!| TYPEXT         |-->| TYPE OF OFF-DIAGONAL TERMS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_DIM2_EXT => BIEF_DIM2_EXT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN) :: IELM1,IELM2,STO
      CHARACTER(LEN=1), INTENT(IN) :: TYPEXT
      TYPE(BIEF_MESH) , INTENT(IN) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NDIM
!
!-----------------------------------------------------------------------
!
      IF(TYPEXT.EQ.'0') THEN
!
        BIEF_DIM2_EXT = 1
!
      ELSEIF(STO.EQ.1) THEN
!
!       NDIM IS HERE THE SECOND DIMENSION OF VECTOR
        NDIM =     BIEF_NBPEL(IELM1,MESH)*BIEF_NBPEL(IELM2,MESH)
     &       - MIN(BIEF_NBPEL(IELM1,MESH),BIEF_NBPEL(IELM2,MESH))
!       SYMMETRICAL MATRIX
        IF(IELM1.EQ.IELM2.AND.TYPEXT.EQ.'S') THEN
          NDIM = NDIM / 2
        ENDIF
        BIEF_DIM2_EXT = NDIM
!
      ELSEIF(STO.EQ.3) THEN
!
!       ASSEMBLED EBE STORAGE OR EDGE BASED
        BIEF_DIM2_EXT = 1
!
      ELSE
!
        WRITE(LU,101) STO
101     FORMAT(1X,'BIEF_DIM2_EXT : UNKNOWN TYPE OF STORAGE: ',1I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
