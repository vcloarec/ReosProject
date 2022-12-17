!                   *****************
                    SUBROUTINE VC00FF
!                   *****************
!
     &(XMUL,X,Y,Z,IKLE1,IKLE2,IKLE3,IKLE4,NBOR,NELEB,NELEBX,W1,W2,W3,W4,
     & NELBOR,NULONE,NELMAX)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /
!+    VEC(I) = XMUL  /    PSI(I)  D(OMEGA)
!+                  /OMEGA
!+
!+    PSI(I) IS A BASE OF TYPE P1 SEGMENT
!+
!+    F IS A VECTOR OF TYPE IELMF
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        05/02/91
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        11/012013
!+        V6P3
!+   Last 3 arguments added, use of XEL, YEL instead of XPT,YPT.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLE1          |-->| FIRST POINT OF QUADRILATERAL
!| IKLE2          |-->| SECOND POINT OF QUADRILATERAL
!| IKLE3          |-->| THIRD POINT OF QUADRILATERAL
!| IKLE4          |-->| FOURTH POINT OF QUADRILATERAL
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELBOR         |-->| ADJACENT ELEMENT NUMBER
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NULONE         |-->| LOCAL NUMBERING OF BOUNDARY ELEMENT IN ADJACENT
!|                |   | ELEMENT.
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Y              |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH, PER POINT !!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEB,NELEBX,NELMAX
      INTEGER, INTENT(IN) :: NELBOR(NELEBX),NULONE(NELEBX,4),NBOR(*)
      INTEGER, INTENT(IN) :: IKLE1(NELEBX),IKLE2(NELEBX)
      INTEGER, INTENT(IN) :: IKLE3(NELEBX),IKLE4(NELEBX)
!
      DOUBLE PRECISION, INTENT(IN)    :: X(NELMAX,6),Y(NELMAX,6),Z(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELEBX),W2(NELEBX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELEBX),W4(NELEBX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,I4,IEL,J1,J2
      DOUBLE PRECISION XSUR24,H1,H2,AL
!
      INTRINSIC SQRT
!
!***********************************************************************
!
! NOTE ON PARALLELISM : IN PARALLEL MODE NELEM MAY BE WRONG AS BOUNDARY
!                       ELEMENTS MAY BE IN ANOTHER SUB-DOMAIN. IN THIS
!                       CASE WE HAVE I1=I2 AND THUS AL=0.D0, SO WRONG
!                       ELEMENTS DO NOT CONTRIBUTE.
!
!***********************************************************************
!
      XSUR24 = XMUL/24.D0
!
!   LOOP ON THE BOUNDARY SIDES
!
      DO IELEM = 1,NELEB
!
        IEL=NELBOR(IELEM)
!
        IF(IEL.GT.0) THEN
!
!         ELEMENT IN THE DOMAIN
!
!         GLOBAL NUMBERING OF THE SIDE NODES
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
!
          J1=NULONE(IELEM,1)
          J2=NULONE(IELEM,2)
          AL = SQRT((X(IEL,J2)-X(IEL,J1))**2
     &             +(Y(IEL,J2)-Y(IEL,J1))**2) * XSUR24
!
          H1 = Z(NBOR(I4)) - Z(NBOR(I1))
          H2 = Z(NBOR(I3)) - Z(NBOR(I2))
!
          W1(IELEM) = (3.D0*H1+H2)*AL
          W2(IELEM) = (3.D0*H2+H1)*AL
          W3(IELEM) = (3.D0*H2+H1)*AL
          W4(IELEM) = (3.D0*H1+H2)*AL
!
        ELSE
!
!         ELEMENT NOT IN THE DOMAIN (PARALLELISM)
!
          W1(IELEM) = 0.D0
          W2(IELEM) = 0.D0
          W3(IELEM) = 0.D0
          W4(IELEM) = 0.D0
!
        ENDIF
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
