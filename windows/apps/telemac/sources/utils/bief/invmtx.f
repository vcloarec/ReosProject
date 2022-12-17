!                   *****************
                    SUBROUTINE INVMTX
!                   *****************
!
     &(AM,BM,NP)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INVERTS A MATRIX OF NP BY NP.
!+
!+            BM IS THE INVERSION OF AM.
!
!note     THIS SUBROUTINE CALLS LUDCMP AND LUBKSB, WHICH WERE
!+         COPIED FROM "NUMERIC RECIPES" -- A WELL-KNOWN BOOK.
!
!history  CHUN WANG
!+        28/07/2006
!+        V5P7
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
!| AM             |-->| MATRIX TO BE INVERTED
!| BM             |<--| INVERTED MATRIX
!| NP             |-->| RANK OF MATRICES AM AND BM.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NP
      DOUBLE PRECISION, INTENT(INOUT) :: AM(NP,NP),BM(NP,NP)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER INDX(500),N,I,J
!
!-----------------------------------------------------------------------
!
      IF(NP.GT.500) THEN
        WRITE(LU,*) 'NP MUST BE LESS THAN 500 IN INVMTX'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      N = NP
      DO I=1,N !SET UP IDENTITY MATRIX
        DO J=1,N
          BM(I,J)=0.D0
        ENDDO
        BM(I,I)=1.D0
      ENDDO
!
!     DECOMPOSES THE MATRIX JUST ONCE
!
      CALL LUDCMP(AM,N,NP,INDX)
!
!     FINDS INVERSE BY COLUMNS
!
      DO J=1,N
        CALL LUBKSB(AM,N,NP,INDX,BM(1,J))
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
