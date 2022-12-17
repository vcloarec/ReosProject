!                   *****************
                    SUBROUTINE ERRMAX
!                   *****************
!
     &(X1,X2,ERR,IERR)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES MAX DIFFERENCES BETWEEN 2 COMPUTED ARRAYS.
!
!history  E. BARROS
!+        27/04/93
!+
!+
!
!history  A. LEOPARDI (UNINA)
!+        02/10/00
!+        V5P1
!+   UPGRADE
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
!| ERR            |<--| MAXIMUM ABSOLUTE DIFFERENCE
!| IERR           |<--| POINT WHERE THE DIFFERENCE OCCURS
!| X1             |-->| ARRAY TO COMPARE WITH X2
!| X2             |-->| ARRAY TO COMPARE WITH X1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(OUT) :: IERR
      DOUBLE PRECISION , INTENT(OUT) :: ERR
      TYPE (BIEF_OBJ)  , INTENT(IN)  :: X1,X2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      INTRINSIC ABS
!
!---------------------------------------------------------------------
!
      IERR=1
      ERR=-1.D0
      DO I=1,X1%DIM1
!
        IF(ABS(X1%R(I)-X2%R(I)).GT.ERR) THEN
          ERR=ABS(X1%R(I)-X2%R(I))
          IERR=I
        ENDIF
!
      ENDDO
!
!---------------------------------------------------------------------
!
      RETURN
      END
