!                   *****************
                    SUBROUTINE MT02AA
!                   *****************
!
     &( A11 , A12 , A13 ,
     &        A22 , A23 ,
     &              A33 ,
     &  XMUL,SU,U,SV,V,
     &  XEL,YEL,SURFAC,IKLE1,IKLE2,IKLE3,NELEM,NELMAX,FORMUL)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE DIFFUSION MATRIX FOR P1 TRIANGLES.
!+
!+            VISCOSITY CAN BE ISOTROPIC IF U IS A VECTOR WITH 1
!+                DIMENSION. IT CAN BE ALSO TENSORIAL IF U IS A VECTOR
!+                WITH 3 DIMENSIONS, THEN REPRESENTING NUXX, NUYY, NUXY.
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        16/07/07
!+        V5P8
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
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| FORMUL         |-->| FORMULA DESCRIBING THE MATRIX
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SURFAC         |-->| AREA OF TRIANGLES
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| U              |-->| FUNCTION U USED IN THE FORMULA
!| V              |-->| FUNCTION V USED IN THE FORMULA
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT02AA => MT02AA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
      DOUBLE PRECISION, INTENT(IN)    :: U(*),V(*)
!     STRUCTURE OF U
      TYPE(BIEF_OBJ), INTENT(IN)      :: SU,SV
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      CHARACTER(LEN=16), INTENT(IN)   :: FORMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELMNU,IELMNV,IELEM,ISO,IAD2,IAD3,I1,I2,I3
!
      DOUBLE PRECISION X2,X3,Y2,Y3,S2D,VISC1,VISC2,VISC3,BPE,CPF,APD
      DOUBLE PRECISION AUX,X2X3,Y2Y3,X2AUX,Y2AUX,X3AUX,Y3AUX,X2MX3,Y2MY3
      DOUBLE PRECISION SOMVX,SOMVY,SOMVZ,XSUR12,XSUR48
      DOUBLE PRECISION G1,G2,G3,COEF1,COEF2,G123
!
!=======================================================================
!
      XSUR12 = XMUL / 12.D0
      XSUR48 = XMUL / 48.D0
!
!     EXTRACTS THE TYPE OF ELEMENT FOR VISCOSITY
!
      IELMNU=SU%ELM
      IELMNV=SV%ELM
      ISO = SU%DIM2
!
      IF(IELMNU.EQ.10.AND.ISO.EQ.1) THEN
!
!-----------------------------------------------------------------------
!
!  P0 DISCRETISATION FOR ISOTROPIC VISCOSITY:
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
!
!   INITIALISES THE INTERMEDIATE VARIABLES
!
        S2D   = XMUL*0.25D0/SURFAC(IELEM)
        VISC1 = U(IELEM)*S2D
        BPE   = VISC1*(Y3**2 + X3**2)
        CPF   = VISC1*(Y2*Y3 + X2*X3)
        APD   = VISC1*(Y2**2 + X2**2)
!
!  DIAGONAL TERMS
!
        A11(IELEM)= APD+BPE-2*CPF
        A22(IELEM)= BPE
        A33(IELEM)= APD
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM)= CPF-BPE
        A13(IELEM)= CPF-APD
        A23(IELEM)=-CPF
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!  P0 DISCRETISATIO FOR NONISOTROPIC VISCOSITY
!
      ELSEIF(IELMNU.EQ.10.AND.ISO.EQ.3) THEN
!
!-----------------------------------------------------------------------
!
!  P0 DISCRETISATION FOR ISOTROPIC VISCOSITY:
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
!
!  INITIALISES THE INTERMEDIATE VARIABLES
!
        S2D   = XMUL*0.25D0/SURFAC(IELEM)
        VISC1 = U(         IELEM)*S2D
        VISC2 = U(  NELMAX+IELEM)*S2D
        VISC3 = U(2*NELMAX+IELEM)*S2D
        BPE   = VISC1*Y3**2 + VISC2*X3**2
        CPF   = VISC1*Y2*Y3 + VISC2*X2*X3
        APD   = VISC1*Y2**2 + VISC2*X2**2
        X2MX3  = X2-X3
        Y2MY3  = Y2-Y3
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM)= CPF-BPE   - ( Y2MY3*X3 + X2MX3*Y3 ) * VISC3
        A13(IELEM)= CPF-APD   + ( Y2MY3*X2 + X2MX3*Y2 ) * VISC3
        A23(IELEM)= -CPF      + ( X2*Y3    + X3*Y2    ) * VISC3
!
!  DIAGONAL TERMS
!
        A11(IELEM) = - A12(IELEM) - A13(IELEM)
        A22(IELEM) = - A12(IELEM) - A23(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELMNU.EQ.11.AND.ISO.EQ.1) THEN
!
!-----------------------------------------------------------------------
!
      IF(FORMUL(7:8).EQ.'UV'.AND.
     &   IELMNU.EQ.11.AND.IELMNV.EQ.11) THEN
!
!-----------------------------------------------------------------------
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
!
!   INITIALISES THE INTERMEDIATE VARIABLES
!
        I1=IKLE1(IELEM)
        I2=IKLE2(IELEM)
        I3=IKLE3(IELEM)
!
        G1=V(I1)
        G2=V(I2)
        G3=V(I3)
!
        COEF1=XSUR48/SURFAC(IELEM)
        G123=G1+G2+G3
        COEF2=U(I1)*(G1+G123)+U(I2)*(G2+G123)+U(I3)*(G3+G123)
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM)= (Y3*(Y2-Y3)+X3*(X2-X3))*COEF2*COEF1
        A13(IELEM)=-(Y2*(Y2-Y3)+X2*(X2-X3))*COEF2*COEF1
        A23(IELEM)=-(Y2*    Y3 +X2*    X3 )*COEF2*COEF1
!
!  DIAGONAL TERMS
!
        A11(IELEM) = - A12(IELEM) - A13(IELEM)
        A22(IELEM) = - A12(IELEM) - A23(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
      ENDDO
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ELSE
!
!  P1 DISCRETISATION FOR VISCOSITY:
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
!
!   INITIALISES THE INTERMEDIATE VARIABLES
!
        SOMVX = ( U(IKLE1(IELEM))
     &           +U(IKLE2(IELEM))
     &           +U(IKLE3(IELEM)) ) * XSUR12 / SURFAC(IELEM)
        X2X3  = X2 * X3  + Y2 * Y3
        X2AUX = X2*(-X2+X3)+Y2*(-Y2+Y3)
        X3AUX = X3*(-X2+X3)+Y3*(-Y2+Y3)
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM) = - SOMVX * X3AUX
        A13(IELEM) =   SOMVX * X2AUX
        A23(IELEM) = - SOMVX * X2X3
!
!  DIAGONAL TERMS
!
        A11(IELEM) = - A12(IELEM) - A13(IELEM)
        A22(IELEM) = - A12(IELEM) - A23(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
      ENDDO ! IELEM
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  LINEAR DISCRETISATION FOR NONISOTROPIC VISCOSITY
!
      ELSEIF(IELMNU.EQ.11.AND.ISO.EQ.3) THEN
!
!-----------------------------------------------------------------------
!
!  P1 DISCRETISATION FOR VISCOSITY:
!
      IAD2 = SU%MAXDIM1
      IAD3 = 2*IAD2
!
      DO IELEM = 1 , NELEM
!
!  INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
!
!  INITIALISES THE INTERMEDIATE VARIABLES
!
        SOMVX = U(IKLE1(IELEM)     )
     &        + U(IKLE2(IELEM)     )
     &        + U(IKLE3(IELEM)     )
        SOMVY = U(IKLE1(IELEM)+IAD2)
     &        + U(IKLE2(IELEM)+IAD2)
     &        + U(IKLE3(IELEM)+IAD2)
        SOMVZ = U(IKLE1(IELEM)+IAD3)
     &        + U(IKLE2(IELEM)+IAD3)
     &        + U(IKLE3(IELEM)+IAD3)
!
!   INITIALISES THE INTERMEDIATE VARIABLES
!
        AUX = XSUR12 / SURFAC(IELEM)
        X2X3 = X2 * X3
        Y2Y3 = Y2 * Y3
        X2AUX = X2*(-X2+X3)
        Y2AUX = Y2*(-Y2+Y3)
        X3AUX = X3*(-X2+X3)
        Y3AUX = Y3*(-Y2+Y3)
        X2MX3 = X2-X3
        Y2MY3 = Y2-Y3
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM) = (   - SOMVX * Y3AUX
     &                   - SOMVY * X3AUX
     &                   - Y2MY3*X3*SOMVZ
     &                   - X2MX3*Y3*SOMVZ ) * AUX
!
        A13(IELEM) = (     SOMVX * Y2AUX
     &                   + SOMVY * X2AUX
     &                   + Y2MY3*X2*SOMVZ
     &                   + X2MX3*Y2*SOMVZ ) * AUX
!
        A23(IELEM) = (   - SOMVX * Y2Y3
     &                   - SOMVY * X2X3
     &                   + SOMVZ*X3*Y2+SOMVZ*X2*Y3 ) * AUX
!
!  DIAGONAL TERMS
!
        A11(IELEM) = - A12(IELEM) - A13(IELEM)
        A22(IELEM) = - A12(IELEM) - A23(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,11) IELMNU,ISO
11      FORMAT(1X,
     &  'MT02AA (BIEF) : TYPE OF VISCOSITY NOT AVAILABLE : ',2I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(FORMUL(14:16).EQ.'MON') THEN
        IF(XMUL.GT.0.D0) THEN
          DO IELEM=1,NELEM
            A12(IELEM)=MIN(A12(IELEM),0.D0)
            A13(IELEM)=MIN(A13(IELEM),0.D0)
            A23(IELEM)=MIN(A23(IELEM),0.D0)
!           DIAGONAL TERMS REDONE
            A11(IELEM) = - A12(IELEM) - A13(IELEM)
            A22(IELEM) = - A12(IELEM) - A23(IELEM)
            A33(IELEM) = - A13(IELEM) - A23(IELEM)
          ENDDO
        ELSE
          DO IELEM=1,NELEM
            A12(IELEM)=MAX(A12(IELEM),0.D0)
            A13(IELEM)=MAX(A13(IELEM),0.D0)
            A23(IELEM)=MAX(A23(IELEM),0.D0)
!           DIAGONAL TERMS REDONE
            A11(IELEM) = - A12(IELEM) - A13(IELEM)
            A22(IELEM) = - A12(IELEM) - A23(IELEM)
            A33(IELEM) = - A13(IELEM) - A23(IELEM)
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
