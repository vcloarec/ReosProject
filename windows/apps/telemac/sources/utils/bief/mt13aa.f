!                   *****************
                    SUBROUTINE MT13AA
!                   *****************
!
     &(  A11 , A12 , A13 ,
     &   A21 , A22 , A23 ,
     &   A31 , A32 , A33 ,
     &   XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+                    /            D
!+    A(I,J)=   XMUL /  PSI2(I) *  --( PSI1(J) ) D(OMEGA)
!+                  /OMEGA         DX
!+
!+  ICOORD=2 WOULD GIVE A DERIVATIVE WRT Y
!+  ICOORD=3 WOULD GIVE A DERIVATIVE WRT Z
!+
!+  PSI1: LINEAR
!+  PSI2: LINEAR
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  SIGN CHANGED COMPARED TO 3.0
!warning  TRANSPOSITION COMPARED TO 3.0
!
!history  J-M HERVOUET (LNH)    ; C MOULIN (LNH)
!+        09/12/94
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
!| A11            |<--| ELEMENTS OF MATRIX
!| A12            |<--| ELEMENTS OF MATRIX
!| A13            |<--| ELEMENTS OF MATRIX
!| A21            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A31            |<--| ELEMENTS OF MATRIX
!| A32            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT13AA => MT13AA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
      DOUBLE PRECISION XSUR6
!
!-----------------------------------------------------------------------
!
      XSUR6 = XMUL/6.D0
!
!================================
!  DERIVATIVE WRT X  =
!================================
!
      IF(ICOORD.EQ.1) THEN
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
!   DIAGONAL TERMS
!
      A22(IELEM) =    YEL(IELEM,3) * XSUR6
      A33(IELEM) = -  YEL(IELEM,2) * XSUR6
      A11(IELEM) = - A22(IELEM) - A33(IELEM)
!
!   EXTRADIAGONAL TERMS
!
      A21(IELEM) = A11(IELEM)
      A31(IELEM) = A11(IELEM)
      A32(IELEM) = A22(IELEM)
      A12(IELEM) = A22(IELEM)
      A13(IELEM) = A33(IELEM)
      A23(IELEM) = A33(IELEM)
!
      ENDDO ! IELEM
!
      ELSEIF(ICOORD.EQ.2) THEN
!
!================================
!  DERIVATIVE WRT Y  =
!================================
!
      DO IELEM = 1 , NELEM
!
!   DIAGONAL TERMS
!
      A22(IELEM) = - XEL(IELEM,3) * XSUR6
      A33(IELEM) =   XEL(IELEM,2) * XSUR6
      A11(IELEM) = - A22(IELEM) - A33(IELEM)
!
!   EXTRADIAGONAL TERMS
!
      A21(IELEM) = A11(IELEM)
      A31(IELEM) = A11(IELEM)
      A32(IELEM) = A22(IELEM)
      A12(IELEM) = A22(IELEM)
      A13(IELEM) = A33(IELEM)
      A23(IELEM) = A33(IELEM)
!
      ENDDO ! IELEM
!
        ELSE
!
          WRITE(LU,201) ICOORD
201       FORMAT(1X,'MT13AA (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
