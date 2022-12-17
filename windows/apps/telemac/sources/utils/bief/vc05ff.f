!                   *****************
                    SUBROUTINE VC05FF
!                   *****************
!
     &(XMUL,SU,SV,U,V,X,Y,Z,
     & IKLE1,IKLE2,IKLE3,IKLE4,NBOR,NELEB,NELEBX,W1,W2,W3,W4,
     & NELBOR,NULONE,NELMAX)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /         ->
!+    VEC(I) = XMUL  /    (U,V).N  PSI(I)  D(GAMMA)
!+                  /GAMMA
!+
!+    PSI(I) IS A BASE OF TYPE P1 QUADRILATERAL
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM - REAL MESH
!
!history  J-M HERVOUET (LNH)
!+        24/07/2009
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
!| NELEB          |-->| NUMBER OF BOUNDARY ELEMENTS
!| NELEBX         |-->| MAXIMUM NUMBER OF BOUNDARY ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NULONE         |-->| LOCAL NUMBERING OF BOUNDARY ELEMENT IN ADJACENT
!|                |   | ELEMENT.
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Y              |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH, PER POINT !!!!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC05FF => VC05FF
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
!     STRUCTURES OF U, V AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN)   :: SU,SV
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELMU,IELMV,IELEM,N1,N2,N3,N4,I1,I2,I3,I4,J1,J2,IEL
!
      DOUBLE PRECISION XSUR72,H1,H2,HT,AX,AY
      DOUBLE PRECISION U1,U2,U3,U4,V1,V2,V3,V4
!
!-----------------------------------------------------------------------
!
      IELMU=SU%ELM
      IELMV=SV%ELM
!
!-----------------------------------------------------------------------
!
      XSUR72 = XMUL/72.D0
!
!     U LINEAR BY PRISMS
!
!-----------------------------------------------------------------------
!
      IF(IELMU.EQ.71.AND.IELMV.EQ.71) THEN
!
!-----------------------------------------------------------------------
!
!        LOOP ON THE BOUNDARY SIDES
!
        DO IELEM = 1,NELEB
!
          IEL=NELBOR(IELEM)
!
          IF(IEL.GT.0) THEN
!
!           ELEMENT IN THE DOMAIN
!
!           LOCAL NUMBERING OF THE SIDE NODES
!
            I1 = IKLE1(IELEM)
            I2 = IKLE2(IELEM)
            I3 = IKLE3(IELEM)
            I4 = IKLE4(IELEM)
!
!           GLOBAL NUMBERING OF THE SIDE NODES
!
            N1 = NBOR(I1)
            N2 = NBOR(I2)
            N3 = NBOR(I3)
            N4 = NBOR(I4)
!
            H1 = Z(N4) - Z(N1)
            H2 = Z(N3) - Z(N2)
            HT = H1 + H2
            H1 = H1 + H1 + HT
            H2 = H2 + H2 + HT
!
            J1=NULONE(IELEM,1)
            J2=NULONE(IELEM,2)
            AX = (Y(IEL,J2)-Y(IEL,J1)) * XSUR72
            AY = (X(IEL,J1)-X(IEL,J2)) * XSUR72
!           AX = (Y(N2)-Y(N1)) * XSUR72
!           AY = (X(N1)-X(N2)) * XSUR72
!
            U1 = U(I1) + U(I1) + U(I4)
            U2 = U(I2) + U(I2) + U(I3)
            U3 = U(I2) + U(I3) + U(I3)
            U4 = U(I1) + U(I4) + U(I4)
!
            V1 = V(I1) + V(I1) + V(I4)
            V2 = V(I2) + V(I2) + V(I3)
            V3 = V(I2) + V(I3) + V(I3)
            V4 = V(I1) + V(I4) + V(I4)
!
            W1(IELEM) = (U1*H1+U2*HT)*AX + (V1*H1+V2*HT)*AY
            W2(IELEM) = (U1*HT+U2*H2)*AX + (V1*HT+V2*H2)*AY
            W3(IELEM) = (U4*HT+U3*H2)*AX + (V4*HT+V3*H2)*AY
            W4(IELEM) = (U4*H1+U3*HT)*AX + (V4*H1+V3*HT)*AY
!
          ELSE
!
!           ELEMENT NOT IN THE DOMAIN (POSSIBLE IN PARALLELISM)
!
            W1(IELEM) = 0.D0
            W2(IELEM) = 0.D0
            W3(IELEM) = 0.D0
            W4(IELEM) = 0.D0
!
          ENDIF
!
        ENDDO
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELMU.EQ.41.AND.IELMV.EQ.41) THEN
!
!-----------------------------------------------------------------------
!
!       LOOP ON THE BOUNDARY SIDES
!
        DO IELEM = 1,NELEB
!
          IEL=NELBOR(IELEM)
!
          IF(IEL.GT.0) THEN
!
!           GLOBAL NUMBERING OF THE SIDE NODES
!
            N1 = NBOR(IKLE1(IELEM))
            N2 = NBOR(IKLE2(IELEM))
            N3 = NBOR(IKLE3(IELEM))
            N4 = NBOR(IKLE4(IELEM))
!
            H1 = Z(N4) - Z(N1)
            H2 = Z(N3) - Z(N2)
            HT = H1 + H2
            H1 = H1 + H1 + HT
            H2 = H2 + H2 + HT
!
            J1=NULONE(IELEM,1)
            J2=NULONE(IELEM,2)
            AX = (Y(IEL,J2)-Y(IEL,J1)) * XSUR72
            AY = (X(IEL,J1)-X(IEL,J2)) * XSUR72
!           AX = (Y(N2)-Y(N1)) * XSUR72
!           AY = (X(N1)-X(N2)) * XSUR72
!
            U1 = U(N1) + U(N1) + U(N4)
            U2 = U(N2) + U(N2) + U(N3)
            U3 = U(N2) + U(N3) + U(N3)
            U4 = U(N1) + U(N4) + U(N4)
!
            V1 = V(N1) + V(N1) + V(N4)
            V2 = V(N2) + V(N2) + V(N3)
            V3 = V(N2) + V(N3) + V(N3)
            V4 = V(N1) + V(N4) + V(N4)
!
            W1(IELEM) = (U1*H1+U2*HT)*AX + (V1*H1+V2*HT)*AY
            W2(IELEM) = (U1*HT+U2*H2)*AX + (V1*HT+V2*H2)*AY
            W3(IELEM) = (U4*HT+U3*H2)*AX + (V4*HT+V3*H2)*AY
            W4(IELEM) = (U4*H1+U3*HT)*AX + (V4*H1+V3*HT)*AY
!
          ELSE
!
!           ELEMENT NOT IN THE DOMAIN (PARALLELISM)
!
            W1(IELEM) = 0.D0
            W2(IELEM) = 0.D0
            W3(IELEM) = 0.D0
            W4(IELEM) = 0.D0
!
          ENDIF
!
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,101) IELMU,SU%NAME
101     FORMAT(1X,'VC05FF (BIEF) :',/,
     &         1X,'DISCRETIZATION OF U NOT AVAILABLE:',1I6,
     &         1X,'REAL NAME: ',A6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
