!                   **********************
                    SUBROUTINE VC02PP_STAR
!                   **********************
!
     &( XMUL,SF,SG,SH,SU,F,G,H,U,X,Y,Z,SURFAC,
     &  IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,NELEM,NELMAX,
     &  W1,W2,W3,W4,W5,W6,FORMUL)
!
!***********************************************************************
! BIEF   V6P2                                         23/06/2011
!***********************************************************************
!
!brief    COMPUTES THE PRODUCT OF THE DIFFUSION MATRIX BY FUNCTION U
!         CORRESPONDS TO MATRIX COMPUTED IN MT02PP_STAR
!         F, G AND H ARE THE DIFFUSION COEFFICIENTS ALONG X, Y AND Z
!
!history  J-M HERVOUET (LNHE)
!+        23/06/2011
!+        V6P1
!+   First version
!
!history  U.H. MERKEL
!+        18/07/2012
!+        V6P2
!+   Replaced EPSILON with CHOUIA due to nag compiler problems
!
!history  J-M HERVOUET (EDF R&D LNHE)
!+        07/01/2013
!+        V6P3
!+   X and Y are now given per element.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| G              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| H              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| IKLE1          |-->| FIRST POINT OF PRISMS
!| IKLE2          |-->| SECOND POINT OF PRISMS
!| IKLE3          |-->| THIRD POINT OF PRISMS
!| IKLE4          |-->| FOURTH POINT OF PRISMS
!| IKLE5          |-->| FIFTH POINT OF PRISMS
!| IKLE6          |-->| SIXTH POINT OF PRISMS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SG             |-->| BIEF_OBJ STRUCTURE OF G
!| SH             |-->| BIEF_OBJ STRUCTURE OF H
!| SURFAC         |-->| AREA OF TRIANGLES
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| W5             |<--| RESULT IN NON ASSEMBLED FORM
!| W6             |<--| RESULT IN NON ASSEMBLED FORM
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| Y              |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Z              |-->| ELEVATIONS OF POINTS, PER POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF  !  , EX_VC04PP => VC04PP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      INTEGER, INTENT(IN) :: IKLE4(NELMAX),IKLE5(NELMAX),IKLE6(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN)   ::X(NELMAX,6),Y(NELMAX,6),Z(*)
      DOUBLE PRECISION, INTENT(IN)   ::SURFAC(*)
      DOUBLE PRECISION, INTENT(INOUT)::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT)::W4(NELMAX),W5(NELMAX),W6(NELMAX)
      DOUBLE PRECISION, INTENT(IN)   ::XMUL
!
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
!
!     STRUCTURES DE U,V ET DONNEES REELLES
!
      TYPE(BIEF_OBJ),   INTENT(IN) :: SU,SF,SG,SH
      DOUBLE PRECISION, INTENT(IN) :: U(*),F(*),G(*),H(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION NF1,NF2,NF3,NG1,NG2,NG3,NH1,NH2,NH3,XS03
      DOUBLE PRECISION XS06,X2,X3,Y2,Y3,H1,H2,H3,SOMVX,SOMVY,AUX
      DOUBLE PRECISION X2X3,Y2Y3,X2AUX,X3AUX,Y2AUX,Y3AUX,GX(3),GY(3)
      DOUBLE PRECISION NPX,NPY,NUXMOY,NUYMOY,XS24,NPXL,NPXU,NPYL,NPYU
      DOUBLE PRECISION XM01,XM02,XM03,XM04,XM05,XM06,XM07,XM08,XM09,XM10
      DOUBLE PRECISION XM11,XM12,XM13,XM14,XM15,XM16,XM17,XM18,XM19,XM20
      DOUBLE PRECISION XM21,XM22,XM23,XM24,XM25,XM26,XM27,XM28,XM29,XM30
      DOUBLE PRECISION NPX2,NPY2
      INTEGER I1,I2,I3,I4,I5,I6,IELEM
      LOGICAL INCHYD
!
      DOUBLE PRECISION :: CHOUIA = 1.D-4
!
!***********************************************************************
!
      INCHYD=.FALSE.
      IF(FORMUL(7:7).EQ.'2') INCHYD=.TRUE.
      XS03 = XMUL / 3.D0
      XS06 = XMUL / 6.D0
      XS24 = XMUL /24.D0
!
!-----------------------------------------------------------------------
!
      IF(SF%ELM.NE.41) THEN
        WRITE(LU,1001) SF%ELM
1001    FORMAT(1X,'VC02PP_STAR (BIEF) : TYPE OF F NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(SG%ELM.NE.41) THEN
        WRITE(LU,2001) SG%ELM
2001    FORMAT(1X,'VC02PP_STAR (BIEF) : TYPE OF G NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(SH%ELM.NE.41) THEN
        WRITE(LU,3001) SH%ELM
3001    FORMAT(1X,'VC02PP_STAR (BIEF) : TYPE OF H NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(SU%ELM.NE.41) THEN
        WRITE(LU,4001) SU%ELM
4001    FORMAT(1X,'VC02PP_STAR (BIEF) : TYPE OF U NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     TERMES HORIZONTAUX
!
      IF(FORMUL(14:16).EQ.'HOR') THEN
!
!-----------------------------------------------------------------------
!
      DO IELEM = 1 , NELEM
!
        I1 = IKLE1(IELEM)
        I2 = IKLE2(IELEM)
        I3 = IKLE3(IELEM)
        I4 = IKLE4(IELEM)
        I5 = IKLE5(IELEM)
        I6 = IKLE6(IELEM)
!
        H1 = Z(I4) - Z(I1)
        H2 = Z(I5) - Z(I2)
        H3 = Z(I6) - Z(I3)
!
!       X2 = X(I2)-X(I1)
!       X3 = X(I3)-X(I1)
!       Y2 = Y(I2)-Y(I1)
!       Y3 = Y(I3)-Y(I1)
!
        X2=X(IELEM,2)
        X3=X(IELEM,3)
        Y2=Y(IELEM,2)
        Y3=Y(IELEM,3)
!
        IF((INCHYD.AND.MAX(Z(I1),Z(I2),Z(I3)).GT.
     &                 MIN(Z(I4),Z(I5),Z(I6)))    .OR.
     &      H1.LT.CHOUIA.OR.H2.LT.CHOUIA.OR.H3.LT.CHOUIA ) THEN
          AUX = 0.D0
          NUXMOY=0.D0
          NUYMOY=0.D0
        ELSE
          AUX = XS24 / SURFAC(IELEM)
!         TAKING INTO ACCOUNT AN AVERAGE DIFFUSION ON THE PRISM
          NUXMOY=(F(I1)+F(I2)+F(I3)+F(I4)+F(I5)+F(I6))/6.D0
          NUYMOY=(G(I1)+G(I2)+G(I3)+G(I4)+G(I5)+G(I6))/6.D0
        ENDIF
!
        X2X3  = X2*X3
        Y2Y3  = Y2*Y3
        X2AUX = X2X3-X2**2
        X3AUX = X3**2-X2X3
        Y2AUX = Y2Y3-Y2**2
        Y3AUX = Y3**2-Y2Y3
!
!       2D DIFFUSION, LOWER LEVEL (SEE MT02AA)
!       LIKE IN 2D BUT MULTIPLIED BY DELTA(Z)/2
        SOMVX = ( F(I1)*H1+F(I2)*H2+F(I3)*H3 ) * AUX
        SOMVY = ( G(I1)*H1+G(I2)*H2+G(I3)*H3 ) * AUX
!
!       OFF-DIAGONAL TERMS FOR POINTS OF LOWER LEVEL
!       WITH MONOTONICITY ENSURED
!
!       TERM 1
!
        XM01 = - SOMVY * X3AUX - SOMVX * Y3AUX
        XM02 =   SOMVY * X2AUX + SOMVX * Y2AUX
        XM06 = - SOMVY * X2X3  - SOMVX * Y2Y3
        XM16 = XM01
        XM17 = XM02
        XM21 = XM06
!
!       2D DIFFUSION, UPPER LEVEL
        SOMVX = ( F(I4)*H1+F(I5)*H2+F(I6)*H3 ) * AUX
        SOMVY = ( G(I4)*H1+G(I5)*H2+G(I6)*H3 ) * AUX
!
!       OFF-DIAGONAL TERMS FOR POINTS OF UPPER LEVEL
!       WITH MONOTONICITY ENSURED
!
!       TERM 1
!
        XM13 = - SOMVY * X3AUX - SOMVX * Y3AUX
        XM14 =   SOMVY * X2AUX + SOMVX * Y2AUX
        XM15 = - SOMVY * X2X3  - SOMVX * Y2Y3
        XM28 = XM13
        XM29 = XM14
        XM30 = XM15
!
!       AVERAGE OF NORMAL VECTOR TO PLANES (NOT NORMED)
!       ONE CAN CHECK THAT WE GET -1 0 OR 0 -1 WITH Z=X OR Z=Y
!       NPX=-DZ/DX   NPY=-DZ/DY
!
        NPXL=-0.5D0*(Y2*(Z(I1)-Z(I3))+Y3*(Z(I2)-Z(I1)))
        NPXU=-0.5D0*(Y2*(Z(I4)-Z(I6))+Y3*(Z(I5)-Z(I4)))
        NPYL=-0.5D0*(X2*(Z(I3)-Z(I1))+X3*(Z(I1)-Z(I2)))
        NPYU=-0.5D0*(X2*(Z(I6)-Z(I4))+X3*(Z(I4)-Z(I5)))
        NPX=0.5D0*(NPXL+NPXU)/SURFAC(IELEM)
        NPY=0.5D0*(NPYL+NPYU)/SURFAC(IELEM)
!
        NPX=NPX*NUXMOY
        NPY=NPY*NUYMOY
!
!       2D GRADIENT MATRIX  GX(3,3) AND GY(3,3)
!       BUT GX(1,J)=GX(2,J)=GX(3,J)
!       AND GY(1,J)=GY(2,J)=GY(3,J), HENCE ONLY GX(3) AND GY(3)
!
        GX(2) =   Y3*XS06
        GX(3) = - Y2*XS06
        GX(1) = - GX(2) - GX(3)
!
        GY(2) = - X3 * XS06
        GY(3) =   X2 * XS06
        GY(1) = - GY(2) - GY(3)
!
!       TERM 3
!
!       TERM 1-2 (01)
        XM01=XM01        +0.5D0*( - ( NPX*GX(1)+NPY*GY(1)) )
!       TERM 1-3 (02)
        XM02=XM02        +0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 1-4 (03)
        XM03=            +0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 1-5 (04)
        XM04=            +0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 1-6 (05)
        XM05=            +0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 2-3 (06)
        XM06=XM06        +0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 2-4 (07)
        XM07=            +0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 2-5 (08)
        XM08=            +0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 2-6 (09)
        XM09=            +0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 3-4 (10)
        XM10=            +0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 3-5 (11)
        XM11=            +0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 3-6 (12)
        XM12=            +0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERME 4-5 (13)
        XM13=XM13        +0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 4-6 (14)
        XM14=XM14        +0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 5-6 (15)
        XM15=XM15        +0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!
!       TERM 2-1 (16)
        XM16=XM16        +0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 3-1 (17)
        XM17=XM17        +0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 4-1 (18)
        XM18=             0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 5-1 (19)
        XM19=             0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 6-1 (20)
        XM20=             0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 3-2 (21)
        XM21=XM21        +0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 4-2 (22)
        XM22=             0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 5-2 (23)
        XM23=             0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 6-2 (24)
        XM24=             0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 4-3 (25)
        XM25=             0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 5-3 (26)
        XM26=             0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 5-4 (28)
        XM28=XM28        +0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 6-3 (27)
        XM27=             0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 6-4 (29)
        XM29=XM29        +0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 6-5 (30)
        XM30=XM30        +0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!
        W1(IELEM) =  + XM01 * ( U(I2)-U(I1) )
     &               + XM02 * ( U(I3)-U(I1) )
     &               + XM03 * ( U(I4)-U(I1) )
     &               + XM04 * ( U(I5)-U(I1) )
     &               + XM05 * ( U(I6)-U(I1) )
        W2(IELEM) =  + XM16 * ( U(I1)-U(I2) )
     &               + XM06 * ( U(I3)-U(I2) )
     &               + XM07 * ( U(I4)-U(I2) )
     &               + XM08 * ( U(I5)-U(I2) )
     &               + XM09 * ( U(I6)-U(I2) )
        W3(IELEM) =  + XM17 * ( U(I1)-U(I3) )
     &               + XM21 * ( U(I2)-U(I3) )
     &               + XM10 * ( U(I4)-U(I3) )
     &               + XM11 * ( U(I5)-U(I3) )
     &               + XM12 * ( U(I6)-U(I3) )
        W4(IELEM) =  + XM18 * ( U(I1)-U(I4) )
     &               + XM22 * ( U(I2)-U(I4) )
     &               + XM25 * ( U(I3)-U(I4) )
     &               + XM13 * ( U(I5)-U(I4) )
     &               + XM14 * ( U(I6)-U(I4) )
        W5(IELEM) =  + XM19 * ( U(I1)-U(I5) )
     &               + XM23 * ( U(I2)-U(I5) )
     &               + XM26 * ( U(I3)-U(I5) )
     &               + XM28 * ( U(I4)-U(I5) )
     &               + XM15 * ( U(I6)-U(I5) )
        W6(IELEM) =  + XM20 * ( U(I1)-U(I6) )
     &               + XM24 * ( U(I2)-U(I6) )
     &               + XM27 * ( U(I3)-U(I6) )
     &               + XM29 * ( U(I4)-U(I6) )
     &               + XM30 * ( U(I5)-U(I6) )
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(14:16).EQ.'VER') THEN
!
!-----------------------------------------------------------------------
!
      DO IELEM = 1 , NELEM
!
        I1 = IKLE1(IELEM)
        I2 = IKLE2(IELEM)
        I3 = IKLE3(IELEM)
        I4 = IKLE4(IELEM)
        I5 = IKLE5(IELEM)
        I6 = IKLE6(IELEM)
!
        H1 = Z(I4) - Z(I1)
        H2 = Z(I5) - Z(I2)
        H3 = Z(I6) - Z(I3)
!
!       X2 = X(I2)-X(I1)
!       X3 = X(I3)-X(I1)
!       Y2 = Y(I2)-Y(I1)
!       Y3 = Y(I3)-Y(I1)
!
        X2=X(IELEM,2)
        X3=X(IELEM,3)
        Y2=Y(IELEM,2)
        Y3=Y(IELEM,3)
!
        IF((INCHYD.AND.MAX(Z(I1),Z(I2),Z(I3)).GT.
     &                 MIN(Z(I4),Z(I5),Z(I6)))    .OR.
     &      H1.LT.CHOUIA.OR.H2.LT.CHOUIA.OR.H3.LT.CHOUIA ) THEN
          NF1=0.D0
          NF2=0.D0
          NF3=0.D0
          NG1=0.D0
          NG2=0.D0
          NG3=0.D0
          NH1=0.D0
          NH2=0.D0
          NH3=0.D0
          NUXMOY=0.D0
          NUYMOY=0.D0
        ELSE
!         SUIVANT LES CAS (II4=I1 OU I4, ETC.)
!         VARIANTE AVEC VISCOSITE VERTICALE LINEAIRE (II4=I4)
!         VARIANTE AVEC VISCOSITE VERTICALE P0 SUR LA VERTICALE (II4=I1)
          NF1=0.5D0*(F(I1)+F(I4))/H1
          NF2=0.5D0*(F(I2)+F(I5))/H2
          NF3=0.5D0*(F(I3)+F(I6))/H3
          NG1=0.5D0*(G(I1)+G(I4))/H1
          NG2=0.5D0*(G(I2)+G(I5))/H2
          NG3=0.5D0*(G(I3)+G(I6))/H3
          NH1=0.5D0*(H(I1)+H(I4))/H1
          NH2=0.5D0*(H(I2)+H(I5))/H2
          NH3=0.5D0*(H(I3)+H(I6))/H3
!         TAKING INTO ACCOUNT AN AVERAGE DIFFUSION ON THE PRISM
          NUXMOY=(F(I1)+F(I2)+F(I3)+F(I4)+F(I5)+F(I6))/6.D0
          NUYMOY=(G(I1)+G(I2)+G(I3)+G(I4)+G(I5)+G(I6))/6.D0
        ENDIF
!
!       AVERAGE OF NORMAL VECTOR TO PLANES (NOT NORMED)
!       ONE CAN CHECK THAT WE GET -1 0 OR 0 -1 WITH Z=X OR Z=Y
!       NPX=-DZ/DX   NPY=-DZ/DY
!
        NPXL=-0.5D0*(Y2*(Z(I1)-Z(I3))+Y3*(Z(I2)-Z(I1)))
        NPXU=-0.5D0*(Y2*(Z(I4)-Z(I6))+Y3*(Z(I5)-Z(I4)))
        NPYL=-0.5D0*(X2*(Z(I3)-Z(I1))+X3*(Z(I1)-Z(I2)))
        NPYU=-0.5D0*(X2*(Z(I6)-Z(I4))+X3*(Z(I4)-Z(I5)))
        NPX=0.5D0*(NPXL+NPXU)/SURFAC(IELEM)
        NPY=0.5D0*(NPYL+NPYU)/SURFAC(IELEM)
        NPX2=NPX**2
        NPY2=NPY**2
!
        NPX=NPX*NUXMOY
        NPY=NPY*NUYMOY
!
!       2D GRADIENT MATRIX  GX(3,3) AND GY(3,3)
!       BUT GX(1,J)=GX(2,J)=GX(3,J)
!       AND GY(1,J)=GY(2,J)=GY(3,J), HENCE ONLY GX(3) AND GY(3)
!
        GX(2) =   Y3*XS06
        GX(3) = - Y2*XS06
        GX(1) = - GX(2) - GX(3)
!
        GY(2) = - X3 * XS06
        GY(3) =   X2 * XS06
        GY(1) = - GY(2) - GY(3)
!
!
!       TERMS 2
!
!       TERM 1-2 (01)
        XM01=0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 1-3 (02)
        XM02=0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 1-4 (03)
        XM03=0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 1-5 (04)
        XM04=0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 1-6 (05)
        XM05=0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 2-3 (06)
        XM06=0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 2-4 (07)
        XM07=0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 2-5 (08)
        XM08=0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 2-6 (09)
        XM09=0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 3-4 (10)
        XM10=0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 3-5 (11)
        XM11=0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 3-6 (12)
        XM12=0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 4-5 (13)
        XM13=0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 4-6 (14)
        XM14=0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 5-6 (15)
        XM15=0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!
!       TERM 2-1 (16)
        XM16=0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 3-1 (17)
        XM17=0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 4-1 (18)
        XM18=0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 5-1 (19)
        XM19=0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 6-1 (20)
        XM20=0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 3-2 (21)
        XM21=0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 4-2 (22)
        XM22=0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 5-2 (23)
        XM23=0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 6-2 (24)
        XM24=0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 4-3 (25)
        XM25=0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 5-3 (26)
        XM26=0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 6-3 (27)
        XM27=0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 5-4 (28)
        XM28=0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 6-4 (29)
        XM29=0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 6-5 (30)
        XM30=0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!
!  TERM 4
!
        XM03=XM03-(NPX2*NF1+NPY2*NG1+NH1)*SURFAC(IELEM)*XS03
        XM08=XM08-(NPX2*NF2+NPY2*NG2+NH2)*SURFAC(IELEM)*XS03
        XM12=XM12-(NPX2*NF3+NPY2*NG3+NH3)*SURFAC(IELEM)*XS03
        XM18=XM18-(NPX2*NF1+NPY2*NG1+NH1)*SURFAC(IELEM)*XS03
        XM23=XM23-(NPX2*NF2+NPY2*NG2+NH2)*SURFAC(IELEM)*XS03
        XM27=XM27-(NPX2*NF3+NPY2*NG3+NH3)*SURFAC(IELEM)*XS03
!
        W1(IELEM) =  + XM01 * ( U(I2)-U(I1) )
     &               + XM02 * ( U(I3)-U(I1) )
     &               + XM03 * ( U(I4)-U(I1) )
     &               + XM04 * ( U(I5)-U(I1) )
     &               + XM05 * ( U(I6)-U(I1) )
        W2(IELEM) =  + XM16 * ( U(I1)-U(I2) )
     &               + XM06 * ( U(I3)-U(I2) )
     &               + XM07 * ( U(I4)-U(I2) )
     &               + XM08 * ( U(I5)-U(I2) )
     &               + XM09 * ( U(I6)-U(I2) )
        W3(IELEM) =  + XM17 * ( U(I1)-U(I3) )
     &               + XM21 * ( U(I2)-U(I3) )
     &               + XM10 * ( U(I4)-U(I3) )
     &               + XM11 * ( U(I5)-U(I3) )
     &               + XM12 * ( U(I6)-U(I3) )
        W4(IELEM) =  + XM18 * ( U(I1)-U(I4) )
     &               + XM22 * ( U(I2)-U(I4) )
     &               + XM25 * ( U(I3)-U(I4) )
     &               + XM13 * ( U(I5)-U(I4) )
     &               + XM14 * ( U(I6)-U(I4) )
        W5(IELEM) =  + XM19 * ( U(I1)-U(I5) )
     &               + XM23 * ( U(I2)-U(I5) )
     &               + XM26 * ( U(I3)-U(I5) )
     &               + XM28 * ( U(I4)-U(I5) )
     &               + XM15 * ( U(I6)-U(I5) )
        W6(IELEM) =  + XM20 * ( U(I1)-U(I6) )
     &               + XM24 * ( U(I2)-U(I6) )
     &               + XM27 * ( U(I3)-U(I6) )
     &               + XM29 * ( U(I4)-U(I6) )
     &               + XM30 * ( U(I5)-U(I6) )
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,202) FORMUL
202     FORMAT(1X,'VC02PP_STAR (BIEF) :',/,
     &         1X,'HOR OR VER LACKING AT THE END OF THE FORMULA : ',A16)
        CALL PLANTE(1)
        STOP
!
!-----------------------------------------------------------------------
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
