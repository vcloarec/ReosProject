!                   *****************
                    SUBROUTINE MT01CC
!                   *****************
!
     &( A11 , A12 , A13 , A14 , A15 , A16 ,
     &        A22 , A23 , A24 , A25 , A26 ,
     &              A33 , A34 , A35 , A36 ,
     &                    A44 , A45 , A46 ,
     &                          A55 , A56 ,
     &                                A66 ,
     &  XMUL,SURFAC,NELEM,NELMAX )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE MASS MATRIX FOR P2 TRIANGLES.
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  ALGIANE FROEHLY (MATMECA)
!+        29/02/08
!+        V5P9
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
!| A11            |<--| ELEMENTS OF MATRIX
!| A12            |<--| ELEMENTS OF MATRIX
!| A13            |<--| ELEMENTS OF MATRIX
!| A14            |<--| ELEMENTS OF MATRIX
!| A15            |<--| ELEMENTS OF MATRIX
!| A16            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A24            |<--| ELEMENTS OF MATRIX
!| A25            |<--| ELEMENTS OF MATRIX
!| A26            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| A34            |<--| ELEMENTS OF MATRIX
!| A35            |<--| ELEMENTS OF MATRIX
!| A36            |<--| ELEMENTS OF MATRIX
!| A44            |<--| ELEMENTS OF MATRIX
!| A45            |<--| ELEMENTS OF MATRIX
!| A46            |<--| ELEMENTS OF MATRIX
!| A55            |<--| ELEMENTS OF MATRIX
!| A56            |<--| ELEMENTS OF MATRIX
!| A66            |<--| ELEMENTS OF MATRIX
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SURFAC         |-->| AREA OF TRIANGLES
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF !, EX_MT01CC => MT01CC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*),A14(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A15(*),A16(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A25(*),A26(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A33(*),A34(*),A35(*),A36(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A44(*),A45(*),A46(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A55(*),A56(*),A66(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELEM
      DOUBLE PRECISION XSUR180
!
!=======================================================================
!
      XSUR180 = XMUL / 180.D0
!
      DO IELEM = 1 , NELEM
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM) = - SURFAC(IELEM) * XSUR180
        A13(IELEM) =          A12(IELEM)
        A14(IELEM) =   0.D0
        A15(IELEM) =   4.D0 * A12(IELEM)
        A16(IELEM) =   0.D0
        A23(IELEM) =          A12(IELEM)
        A24(IELEM) =   0.D0
        A25(IELEM) =   0.D0
        A26(IELEM) =          A15(IELEM)
        A34(IELEM) =          A15(IELEM)
        A35(IELEM) =   0.D0
        A36(IELEM) =   0.D0
        A45(IELEM) = - 4.D0 * A15(IELEM)
        A46(IELEM) =          A45(IELEM)
        A56(IELEM) =          A45(IELEM)
!
!  DIAGONAL TERMS
!
        A11(IELEM) = - 6.D0 * A12(IELEM)
        A22(IELEM) =          A11(IELEM)
        A33(IELEM) =          A11(IELEM)
        A44(IELEM) = - 8.D0 * A15(IELEM)
        A55(IELEM) =          A44(IELEM)
        A66(IELEM) =          A44(IELEM)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
