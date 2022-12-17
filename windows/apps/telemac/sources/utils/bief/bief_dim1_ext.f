!                   ******************************
                    INTEGER FUNCTION BIEF_DIM1_EXT
!                   ******************************
!
     &(IELM1,IELM2,STO,TYPEXT,MESH)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GIVES THE FIRST DIMENSION OF A MATRICE'S EXTRA-DIAGONAL
!+                TERMS.
!
!history  J-M HERVOUET (LNH)
!+        05/02/2010
!+        V6P0
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
      USE BIEF, EX_BIEF_DIM1_EXT => BIEF_DIM1_EXT
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
      INTEGER IELMX,N
!
!-----------------------------------------------------------------------
!
      IELMX = 10*(IELM1/10)
!
      IF(TYPEXT.EQ.'0') THEN
!
!       NOT 0 TO ENABLE BOUND CHECKING
        BIEF_DIM1_EXT = 1
!
      ELSEIF(STO.EQ.1) THEN
!
!       CLASSICAL EBE STORAGE
!
        BIEF_DIM1_EXT =BIEF_NBMPTS(IELMX,MESH)
!
      ELSEIF(STO.EQ.3) THEN
!
!       EDGE-BASED STORAGE
!
        IF(TYPEXT.EQ.'S') THEN
          BIEF_DIM1_EXT=BIEF_NBSEG(IELM1,MESH)
        ELSE
          BIEF_DIM1_EXT=BIEF_NBSEG(IELM1,MESH)+BIEF_NBSEG(IELM2,MESH)
          N=MAX(BIEF_NBPEL(IELM1,MESH),BIEF_NBPEL(IELM2,MESH))
     &     -MIN(BIEF_NBPEL(IELM1,MESH),BIEF_NBPEL(IELM2,MESH))
          IF(N.GE.2) THEN
!           SOME SEGMENTS LINK ONLY E.G. QUADRATIC POINTS AND
!           WILL NOT BE CONSIDERED IN A RECTANGULAR MATRIX
!           THIS IS THE CASE WITH 3 SEGMENTS IN QUADRATIC TRIANGLE
            BIEF_DIM1_EXT=BIEF_DIM1_EXT
     &                   -N*(N-1)*BIEF_NBMPTS(IELMX,MESH)/2
          ENDIF
        ENDIF
!
      ELSE
!
        WRITE(LU,101) STO
101     FORMAT(1X,'BIEF_DIM1_EXT : UNKNOWN TYPE OF STORAGE: ',1I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
