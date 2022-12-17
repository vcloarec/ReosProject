!                   **********************
                    SUBROUTINE MT02PP_STAR
!                   **********************
!
     &(T,XM,XMUL,SF,SG,SH,F,G,H,X,Y,Z,SURFAC,IKLE,NELEM,NELMAX,INCHYD,
     & FORMUL,NPLAN)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE DIFFUSION MATRIX AS IN MT02PP BUT HERE
!+               WITH A PRIOR DECOMPOSITION OF GRADIENTS ALOG PLANES
!+               AND ON THE VERTICAL, THIS GIVES 4 TERMS WHICH ARE
!+               SUCCESSIVELY COMPUTED HERE, AND MAY BE COMPUTED
!+               SEPARATELY. AS THERE ARE APPROXIMATIONS WHICH ARE
!+               DIFFERENT, THE RESULT IS SLIGHTLY DIFFERENT FROM MT02PP
!+               THIS IS USED E.G. FOR CORRECTING HORIZONTAL AND VERTICAL
!+               TO GET DIVERGENCE FREE FLUXES.
!+
!+            THE FUNCTION DIFFUSION COEFFICIENT IS HERE A P1
!+                DIAGONAL TENSOR.
!+
!+            CASE OF THE PRISM.
!code
!+     STORAGE CONVENTION FOR EXTRA-DIAGONAL TERMS:
!+
!+     XM(IELEM, 1)  ---->  M(1,2) = M(2,1)
!+     XM(IELEM, 2)  ---->  M(1,3) = M(3,1)
!+     XM(IELEM, 3)  ---->  M(1,4) = M(4,1)
!+     XM(IELEM, 4)  ---->  M(1,5) = M(5,1)
!+     XM(IELEM, 5)  ---->  M(1,6) = M(6,1)
!+     XM(IELEM, 6)  ---->  M(2,3) = M(3,2)
!+     XM(IELEM, 7)  ---->  M(2,4) = M(4,2)
!+     XM(IELEM, 8)  ---->  M(2,5) = M(5,2)
!+     XM(IELEM, 9)  ---->  M(2,6) = M(6,2)
!+     XM(IELEM,10)  ---->  M(3,4) = M(4,3)
!+     XM(IELEM,11)  ---->  M(3,5) = M(5,3)
!+     XM(IELEM,12)  ---->  M(3,6) = M(6,3)
!+     XM(IELEM,13)  ---->  M(4,5) = M(5,4)
!+     XM(IELEM,14)  ---->  M(4,6) = M(6,4)
!+     XM(IELEM,15)  ---->  M(5,6) = M(6,5)
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history
!+        22/08/07
!+
!+   CAN OPTIONALLY TREAT (USES DIMDISC) A P0 VERTICAL
!
!history
!+        06/02/07
!+
!+   IF FORMUL(14:16)='MON' :
!
!history  JMH
!+        15/03/2010
!+
!+   PARAMETER EPSILON ADDED
!
!history  J-M HERVOUET (LNHE)     ; F  LEPEINTRE (LNH)
!+        20/05/2010
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
!history  U.H.MErkel
!+        18/07/2012
!+        V6P2
!+   Replaced EPSILON with CHOUIA due to nag compiler problems
!
!history  J-M HERVOUET (LNHE)
!+        11/01/2013
!+        V6P3
!+   XEL and YEL sent instead of XPT and YPT.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE FORMULA
!| FORMUL         |-->| FORMULA DESCRIBING THE RESULTING MATRIX
!| G              |-->| FUNCTION USED IN THE FORMULA
!| H              |-->| FUNCTION USED IN THE FORMULA
!| IKLE           |-->| CONNECTIVITY TABLE.
!| INCHYD         |-->| IF YES, TREATS HYDROSTATIC INCONSISTENCIES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPLAN          |-->| NUMBER OF PLANES IN THE MESH OF PRISMS
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| SG             |-->| STRUCTURE OF FUNCTIONS G
!| SH             |-->| STRUCTURE OF FUNCTIONS H
!| SURFAC         |-->| AREA OF 2D ELEMENTS
!| T              |<->| WORK ARRAY FOR ELEMENT BY ELEMENT DIAGONAL
!| X              |-->| ABSCISSAE OF POINTS, PER ELEMENT
!| Y              |-->| ORDINATES OF POINTS, PER ELEMENT
!| Z              |-->| ELEVATIONS OF POINTS, PER POINT
!| XM             |<->| OFF-DIAGONAL TERMS
!| XMUL           |-->| COEFFICIENT FOR MULTIPLICATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT02PP_STAR => MT02PP_STAR
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPLAN
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
!
!                                                              15 OR 30
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6),XM(NELMAX,*)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
      DOUBLE PRECISION, INTENT(IN)    :: F(*),G(*),H(*)
!
!     STRUCTURES DE F,G,H
!
      TYPE(BIEF_OBJ), INTENT(IN)      :: SF,SG,SH
!
      DOUBLE PRECISION, INTENT(IN)    :: X(NELMAX,6),Y(NELMAX,6),Z(*)
!
      LOGICAL, INTENT(IN)             :: INCHYD
      CHARACTER(LEN=16), INTENT(IN)   :: FORMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIQUES A CE SOUS-PROGRAMME
!
      DOUBLE PRECISION H1,H2,H3
      DOUBLE PRECISION NF1,NF2,NF3,NG1,NG2,NG3,NH1,NH2,NH3
      DOUBLE PRECISION XS06,XS24,NPX,NPY,NPXL,NPXU,XS03
      DOUBLE PRECISION NPYL,NPYU,GX(3),GY(3),NUXMOY,NUYMOY
!
      DOUBLE PRECISION X2,X3,Y2,Y3,SOMVX,X2X3,X2AUX,X3AUX,NPX2,NPY2
      DOUBLE PRECISION SOMVY,Y2Y3,Y2AUX,Y3AUX,AUX
!
      INTEGER I1,I2,I3,I4,I5,I6,IELEM,II4,II5,II6,NPOU0
!
      DOUBLE PRECISION, PARAMETER :: CHOUIA = 1.D-4
!
!***********************************************************************
!
      XS03  =XMUL/3.D0
      XS06  =XMUL/6.D0
      XS24  =XMUL/24.D0
!
      IF(SF%ELM.NE.41) THEN
        WRITE(LU,1001) SF%ELM
1001    FORMAT(1X,'MT02PP_STAR (BIEF) : TYPE OF F NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(SG%ELM.NE.41) THEN
        WRITE(LU,2001) SG%ELM
2001    FORMAT(1X,'MT02PP_STAR (BIEF) : TYPE OF G NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(SH%ELM.NE.41) THEN
        WRITE(LU,3001) SH%ELM
3001    FORMAT(1X,'MT02PP_STAR (BIEF) : TYPE OF H NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     POUR LE TRAITEMENT DE LA VISCOSITE VERTICALE P0 SUR LA VERTICALE
!     VOIR VISCLM DE TELEMAC-3D
!
      IF(SH%DIMDISC.EQ.0) THEN
!       VISCOSITE VERTICALE P1
        NPOU0=SH%DIM1/NPLAN
      ELSEIF(SH%DIMDISC.EQ.4111) THEN
!       VISCOSITE VERTICALE P0 SUR LA VERTICALE (VOIR II4,5,6)
        NPOU0=0
      ELSE
        WRITE(LU,4001) SH%DIMDISC
4001    FORMAT(1X,'MT02PP_STAR (BIEF): DIMDISC NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(FORMUL(11:13).EQ.'MON') THEN
!
        IF(XMUL.LT.0.D0) THEN
          WRITE(LU,4003) XMUL
4003      FORMAT(1X,'MT02PP (BIEF): NEGATIVE XMUL EXCLUDED: ',G16.7)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
!     VERSION WITH MONOTONICITY
!
!-----------------------------------------------------------------------
!
      DO IELEM=1,NELEM
!
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
        I4=IKLE(IELEM,4)
        I5=IKLE(IELEM,5)
        I6=IKLE(IELEM,6)
!       SUIVANT NPOU0, II4 SERA I4 OU I1, ETC.
        II4=I1+NPOU0
        II5=I2+NPOU0
        II6=I3+NPOU0
!
        H1=Z(I4)-Z(I1)
        H2=Z(I5)-Z(I2)
        H3=Z(I6)-Z(I3)
!
        IF((INCHYD.AND.MAX(Z(I1),Z(I2),Z(I3)).GT.
     &                 MIN(Z(I4),Z(I5),Z(I6)))    .OR.
     &      H1.LT.CHOUIA.OR.H2.LT.CHOUIA.OR.H3.LT.CHOUIA ) THEN
          NH1=0.D0
          NH2=0.D0
          NH3=0.D0
        ELSE
!         SUIVANT LES CAS (II4=I1 OU I4, ETC.)
!         VARIANTE AVEC VISCOSITE VERTICALE LINEAIRE (II4=I4)
!         VARIANTE AVEC VISCOSITE VERTICALE P0 SUR LA VERTICALE (II4=I1)
          NH1=0.5D0*(H(I1)+H(II4))/H1
          NH2=0.5D0*(H(I2)+H(II5))/H2
          NH3=0.5D0*(H(I3)+H(II6))/H3
        ENDIF
!
!       DIFFUSION ALONG PLANES
!
!       X2 = X(I2)-X(I1)
!       X3 = X(I3)-X(I1)
!       Y2 = Y(I2)-Y(I1)
!       Y3 = Y(I3)-Y(I1)
        X2 = X(IELEM,2)
        X3 = X(IELEM,3)
        Y2 = Y(IELEM,2)
        Y3 = Y(IELEM,3)
!
        X2X3  = X2*X3
        Y2Y3  = Y2*Y3
        X2AUX = X2X3-X2**2
        X3AUX = X3**2-X2X3
        Y2AUX = Y2Y3-Y2**2
        Y3AUX = Y3**2-Y2Y3
!
        AUX = XS24 / SURFAC(IELEM)
!
!       2D DIFFUSION, LOWER LEVEL (SEE MT02AA)
!       LIKE IN 2D BUT MULTIPLIED BY DELTA(Z)/2
        SOMVX = ( F(I1)*H1+F(I2)*H2+F(I3)*H3 ) * AUX
        SOMVY = ( G(I1)*H1+G(I2)*H2+G(I3)*H3 ) * AUX
!
!       OFF-DIAGONAL TERMS FOR POINTS OF LOWER LEVEL
!       WITH MONOTONICITY ENSURED
!
        XM(IELEM, 1) = MIN(0.D0,- SOMVY * X3AUX - SOMVX * Y3AUX)
        XM(IELEM, 2) = MIN(0.D0,  SOMVY * X2AUX + SOMVX * Y2AUX)
        XM(IELEM, 6) = MIN(0.D0,- SOMVY * X2X3  - SOMVX * Y2Y3 )
!
!       2D DIFFUSION, UPPER LEVEL
        SOMVX = ( F(I4)*H1+F(I5)*H2+F(I6)*H3 ) * AUX
        SOMVY = ( G(I4)*H1+G(I5)*H2+G(I6)*H3 ) * AUX
!
!       OFF-DIAGONAL TERMS FOR POINTS OF UPPER LEVEL
!       WITH MONOTONICITY ENSURED
!
        XM(IELEM,13) = MIN(0.D0,- SOMVY * X3AUX - SOMVX * Y3AUX)
        XM(IELEM,14) = MIN(0.D0,  SOMVY * X2AUX + SOMVX * Y2AUX)
        XM(IELEM,15) = MIN(0.D0,- SOMVY * X2X3  - SOMVX * Y2Y3 )
!
!       HERE TERMS 2 AND 3 HAVE BEEN REMOVED FOR MONOTONICITY
!       CROSSED TERMS CANCELLED
!
        XM(IELEM,04)=0.D0
        XM(IELEM,05)=0.D0
        XM(IELEM,07)=0.D0
        XM(IELEM,09)=0.D0
        XM(IELEM,10)=0.D0
        XM(IELEM,11)=0.D0
!
!       TERM 4
!
!       HERE SOME HORIZONTAL TERMS HAVE BEEN REMOVED BECAUSE
!       HORIZONTAL FLUXES THROUGH BOTTOM AND TOP OF PRISM
!       ARE NEGLECTED FOR MONOTONICITY
!       SEE THE WHOLE TERM IN THE OPTION WITHOUT MONOTONICITY
!
        XM(IELEM,03)=-NH1*SURFAC(IELEM)*XS03
        XM(IELEM,08)=-NH2*SURFAC(IELEM)*XS03
        XM(IELEM,12)=-NH3*SURFAC(IELEM)*XS03
!
!-----------------------------------------------------------------------
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(10:13).EQ.'1234') THEN
!
!-----------------------------------------------------------------------
!
!     VERSION WITHOUT MONOTONICITY AND ALL TERMS
!
!-----------------------------------------------------------------------
!
      DO IELEM=1,NELEM
!
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
        I4=IKLE(IELEM,4)
        I5=IKLE(IELEM,5)
        I6=IKLE(IELEM,6)
!       SUIVANT NPOU0, II4 SERA I4 OU I1, ETC.
        II4=I1+NPOU0
        II5=I2+NPOU0
        II6=I3+NPOU0
!
        H1=Z(I4)-Z(I1)
        H2=Z(I5)-Z(I2)
        H3=Z(I6)-Z(I3)
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
          AUX=0.D0
          NUXMOY=0.D0
          NUYMOY=0.D0
        ELSE
!         SUIVANT LES CAS (II4=I1 OU I4, ETC.)
!         VARIANTE AVEC VISCOSITE VERTICALE LINEAIRE (II4=I4)
!         VARIANTE AVEC VISCOSITE VERTICALE P0 SUR LA VERTICALE (II4=I1)
          NF1=0.5D0*(F(I1)+F(II4))/H1
          NF2=0.5D0*(F(I2)+F(II5))/H2
          NF3=0.5D0*(F(I3)+F(II6))/H3
          NG1=0.5D0*(G(I1)+G(II4))/H1
          NG2=0.5D0*(G(I2)+G(II5))/H2
          NG3=0.5D0*(G(I3)+G(II6))/H3
          NH1=0.5D0*(H(I1)+H(II4))/H1
          NH2=0.5D0*(H(I2)+H(II5))/H2
          NH3=0.5D0*(H(I3)+H(II6))/H3
          AUX = XS24 / SURFAC(IELEM)
!         TAKING INTO ACCOUNT AN AVERAGE DIFFUSION ON THE PRISM
          NUXMOY=(F(I1)+F(I2)+F(I3)+F(I4)+F(I5)+F(I6))/6.D0
          NUYMOY=(G(I1)+G(I2)+G(I3)+G(I4)+G(I5)+G(I6))/6.D0
        ENDIF
!
!       X2 = X(I2)-X(I1)
!       X3 = X(I3)-X(I1)
!       Y2 = Y(I2)-Y(I1)
!       Y3 = Y(I3)-Y(I1)
        X2 = X(IELEM,2)
        X3 = X(IELEM,3)
        Y2 = Y(IELEM,2)
        Y3 = Y(IELEM,3)
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
        XM(IELEM, 1) = - SOMVY * X3AUX - SOMVX * Y3AUX
        XM(IELEM, 2) =   SOMVY * X2AUX + SOMVX * Y2AUX
        XM(IELEM, 6) = - SOMVY * X2X3  - SOMVX * Y2Y3
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
        XM(IELEM,13) = - SOMVY * X3AUX - SOMVX * Y3AUX
        XM(IELEM,14) =   SOMVY * X2AUX + SOMVX * Y2AUX
        XM(IELEM,15) = - SOMVY * X2X3  - SOMVX * Y2Y3
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
!       TERMS 2 AND 3
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
!       OFF-DIAGONAL TERMS, SOME (UP AND DOWN) ALREADY INITIALISED,
!                           SOME (CROSSED TERMS) NOT
!
!       TERMS 2
!
!       TERM 1-2 (01)
        XM(IELEM,01)=XM(IELEM,01)+0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 1-3 (02)
        XM(IELEM,02)=XM(IELEM,02)+0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 1-4 (03)
        XM(IELEM,03)=            +0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 1-5 (04)
        XM(IELEM,04)=            +0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 1-6 (05)
        XM(IELEM,05)=            +0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 2-3 (06)
        XM(IELEM,06)=XM(IELEM,06)+0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 2-4 (07)
        XM(IELEM,07)=            +0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 2-5 (08)
        XM(IELEM,08)=            +0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 2-6 (09)
        XM(IELEM,09)=            +0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 3-4 (10)
        XM(IELEM,10)=            +0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 3-5 (11)
        XM(IELEM,11)=            +0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 3-6 (12)
        XM(IELEM,12)=            +0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERME 4-5 (13)
        XM(IELEM,13)=XM(IELEM,13)+0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 4-6 (14)
        XM(IELEM,14)=XM(IELEM,14)+0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 5-6 (15)
        XM(IELEM,15)=XM(IELEM,15)+0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!
!       TERMS 3
!
!       TERM 1-2 (01)
        XM(IELEM,01)=XM(IELEM,01)+0.5D0*( - ( NPX*GX(1)+NPY*GY(1)) )
!       TERM 1-3 (02)
        XM(IELEM,02)=XM(IELEM,02)+0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 1-4 (03)
        XM(IELEM,03)=XM(IELEM,03)+0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 1-5 (04)
        XM(IELEM,04)=XM(IELEM,04)+0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 1-6 (05)
        XM(IELEM,05)=XM(IELEM,05)+0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 2-3 (06)
        XM(IELEM,06)=XM(IELEM,06)+0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 2-4 (07)
        XM(IELEM,07)=XM(IELEM,07)+0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 2-5 (08)
        XM(IELEM,08)=XM(IELEM,08)+0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 2-6 (09)
        XM(IELEM,09)=XM(IELEM,09)+0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 3-4 (10)
        XM(IELEM,10)=XM(IELEM,10)+0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 3-5 (11)
        XM(IELEM,11)=XM(IELEM,11)+0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 3-6 (12)
        XM(IELEM,12)=XM(IELEM,12)+0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERME 4-5 (13)
        XM(IELEM,13)=XM(IELEM,13)+0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 4-6 (14)
        XM(IELEM,14)=XM(IELEM,14)+0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 5-6 (15)
        XM(IELEM,15)=XM(IELEM,15)+0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!
!       TERM 4
!
        XM(IELEM,03)=XM(IELEM,03)
     &              -(NPX2*NF1+NPY2*NG1+NH1)*SURFAC(IELEM)*XS03
        XM(IELEM,08)=XM(IELEM,08)
     &              -(NPX2*NF2+NPY2*NG2+NH2)*SURFAC(IELEM)*XS03
        XM(IELEM,12)=XM(IELEM,12)
     &              -(NPX2*NF3+NPY2*NG3+NH3)*SURFAC(IELEM)*XS03
!
!-----------------------------------------------------------------------
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(10:13).EQ.'1 3 ') THEN
!
!-----------------------------------------------------------------------
!
!     VERSION WITHOUT MONOTONICITY AND ONLY TERMS 1 AND 3
!
!-----------------------------------------------------------------------
!
      DO IELEM=1,NELEM
!
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
        I4=IKLE(IELEM,4)
        I5=IKLE(IELEM,5)
        I6=IKLE(IELEM,6)
!       SUIVANT NPOU0, II4 SERA I4 OU I1, ETC.
        II4=I1+NPOU0
        II5=I2+NPOU0
        II6=I3+NPOU0
!
        H1=Z(I4)-Z(I1)
        H2=Z(I5)-Z(I2)
        H3=Z(I6)-Z(I3)
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
          AUX=0.D0
          NUXMOY=0.D0
          NUYMOY=0.D0
        ELSE
!         SUIVANT LES CAS (II4=I1 OU I4, ETC.)
!         VARIANTE AVEC VISCOSITE VERTICALE LINEAIRE (II4=I4)
!         VARIANTE AVEC VISCOSITE VERTICALE P0 SUR LA VERTICALE (II4=I1)
          NF1=0.5D0*(F(I1)+F(II4))/H1
          NF2=0.5D0*(F(I2)+F(II5))/H2
          NF3=0.5D0*(F(I3)+F(II6))/H3
          NG1=0.5D0*(G(I1)+G(II4))/H1
          NG2=0.5D0*(G(I2)+G(II5))/H2
          NG3=0.5D0*(G(I3)+G(II6))/H3
          NH1=0.5D0*(H(I1)+H(II4))/H1
          NH2=0.5D0*(H(I2)+H(II5))/H2
          NH3=0.5D0*(H(I3)+H(II6))/H3
          AUX = XS24 / SURFAC(IELEM)
!         TAKING INTO ACCOUNT AN AVERAGE DIFFUSION ON THE PRISM
          NUXMOY=(F(I1)+F(I2)+F(I3)+F(I4)+F(I5)+F(I6))/6.D0
          NUYMOY=(G(I1)+G(I2)+G(I3)+G(I4)+G(I5)+G(I6))/6.D0
        ENDIF
!
!       X2 = X(I2)-X(I1)
!       X3 = X(I3)-X(I1)
!       Y2 = Y(I2)-Y(I1)
!       Y3 = Y(I3)-Y(I1)
        X2 = X(IELEM,2)
        X3 = X(IELEM,3)
        Y2 = Y(IELEM,2)
        Y3 = Y(IELEM,3)
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
        XM(IELEM, 1) = - SOMVY * X3AUX - SOMVX * Y3AUX
        XM(IELEM, 2) =   SOMVY * X2AUX + SOMVX * Y2AUX
        XM(IELEM, 6) = - SOMVY * X2X3  - SOMVX * Y2Y3
        XM(IELEM,16) = XM(IELEM, 1)
        XM(IELEM,17) = XM(IELEM, 2)
        XM(IELEM,21) = XM(IELEM, 6)
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
        XM(IELEM,13) = - SOMVY * X3AUX - SOMVX * Y3AUX
        XM(IELEM,14) =   SOMVY * X2AUX + SOMVX * Y2AUX
        XM(IELEM,15) = - SOMVY * X2X3  - SOMVX * Y2Y3
        XM(IELEM,28) = XM(IELEM,13)
        XM(IELEM,29) = XM(IELEM,14)
        XM(IELEM,30) = XM(IELEM,15)
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
!       TERM 3
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
!       OFF-DIAGONAL TERMS, SOME (UP AND DOWN) ALREADY INITIALISED,
!                           SOME (CROSSED TERMS) NOT
!
!       TERMS 3
!
!       TERM 1-2 (01)
        XM(IELEM,01)=XM(IELEM,01)+0.5D0*( - ( NPX*GX(1)+NPY*GY(1)) )
!       TERM 1-3 (02)
        XM(IELEM,02)=XM(IELEM,02)+0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 1-4 (03)
        XM(IELEM,03)=            +0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 1-5 (04)
        XM(IELEM,04)=            +0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 1-6 (05)
        XM(IELEM,05)=            +0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 2-3 (06)
        XM(IELEM,06)=XM(IELEM,06)+0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 2-4 (07)
        XM(IELEM,07)=            +0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 2-5 (08)
        XM(IELEM,08)=            +0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 2-6 (09)
        XM(IELEM,09)=            +0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 3-4 (10)
        XM(IELEM,10)=            +0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 3-5 (11)
        XM(IELEM,11)=            +0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 3-6 (12)
        XM(IELEM,12)=            +0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERME 4-5 (13)
        XM(IELEM,13)=XM(IELEM,13)+0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 4-6 (14)
        XM(IELEM,14)=XM(IELEM,14)+0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 5-6 (15)
        XM(IELEM,15)=XM(IELEM,15)+0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!
!       TERM 2-1 (16)
        XM(IELEM,16)=XM(IELEM,16)+0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 3-1 (17)
        XM(IELEM,17)=XM(IELEM,17)+0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 4-1 (18)
        XM(IELEM,18)=             0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 5-1 (19)
        XM(IELEM,19)=             0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 6-1 (20)
        XM(IELEM,20)=             0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 3-2 (21)
        XM(IELEM,21)=XM(IELEM,21)+0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 4-2 (22)
        XM(IELEM,22)=             0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 5-2 (23)
        XM(IELEM,23)=             0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 6-2 (24)
        XM(IELEM,24)=             0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 4-3 (25)
        XM(IELEM,25)=             0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 5-3 (26)
        XM(IELEM,26)=             0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 5-4 (28)
        XM(IELEM,28)=XM(IELEM,28)+0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 6-3 (27)
        XM(IELEM,27)=             0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 6-4 (29)
        XM(IELEM,29)=XM(IELEM,29)+0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 6-5 (30)
        XM(IELEM,30)=XM(IELEM,30)+0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!
!-----------------------------------------------------------------------
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(10:13).EQ.' 2 4') THEN
!
!-----------------------------------------------------------------------
!
!     VERSION WITHOUT MONOTONICITY AND ONLY TERMS 2 AND 4
!
!-----------------------------------------------------------------------
!
      DO IELEM=1,NELEM
!
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
        I4=IKLE(IELEM,4)
        I5=IKLE(IELEM,5)
        I6=IKLE(IELEM,6)
!       SUIVANT NPOU0, II4 SERA I4 OU I1, ETC.
        II4=I1+NPOU0
        II5=I2+NPOU0
        II6=I3+NPOU0
!
        H1=Z(I4)-Z(I1)
        H2=Z(I5)-Z(I2)
        H3=Z(I6)-Z(I3)
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
          NF1=0.5D0*(F(I1)+F(II4))/H1
          NF2=0.5D0*(F(I2)+F(II5))/H2
          NF3=0.5D0*(F(I3)+F(II6))/H3
          NG1=0.5D0*(G(I1)+G(II4))/H1
          NG2=0.5D0*(G(I2)+G(II5))/H2
          NG3=0.5D0*(G(I3)+G(II6))/H3
          NH1=0.5D0*(H(I1)+H(II4))/H1
          NH2=0.5D0*(H(I2)+H(II5))/H2
          NH3=0.5D0*(H(I3)+H(II6))/H3
!         TAKING INTO ACCOUNT AN AVERAGE DIFFUSION ON THE PRISM
          NUXMOY=(F(I1)+F(I2)+F(I3)+F(I4)+F(I5)+F(I6))/6.D0
          NUYMOY=(G(I1)+G(I2)+G(I3)+G(I4)+G(I5)+G(I6))/6.D0
        ENDIF
!
!       X2 = X(I2)-X(I1)
!       X3 = X(I3)-X(I1)
!       Y2 = Y(I2)-Y(I1)
!       Y3 = Y(I3)-Y(I1)
        X2 = X(IELEM,2)
        X3 = X(IELEM,3)
        Y2 = Y(IELEM,2)
        Y3 = Y(IELEM,3)
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
!       TERMS 2
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
!       OFF-DIAGONAL TERMS
!
!       TERMS 2
!
!       TERM 1-2 (01)
        XM(IELEM,01)=0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 1-3 (02)
        XM(IELEM,02)=0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 1-4 (03)
        XM(IELEM,03)=0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 1-5 (04)
        XM(IELEM,04)=0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 1-6 (05)
        XM(IELEM,05)=0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 2-3 (06)
        XM(IELEM,06)=0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 2-4 (07)
        XM(IELEM,07)=0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 2-5 (08)
        XM(IELEM,08)=0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 2-6 (09)
        XM(IELEM,09)=0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 3-4 (10)
        XM(IELEM,10)=0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 3-5 (11)
        XM(IELEM,11)=0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 3-6 (12)
        XM(IELEM,12)=0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 4-5 (13)
        XM(IELEM,13)=0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 4-6 (14)
        XM(IELEM,14)=0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 5-6 (15)
        XM(IELEM,15)=0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!
!       TERM 2-1 (16)
        XM(IELEM,16)=0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 3-1 (17)
        XM(IELEM,17)=0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 4-1 (18)
        XM(IELEM,18)=0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 5-1 (19)
        XM(IELEM,19)=0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 6-1 (20)
        XM(IELEM,20)=0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 3-2 (21)
        XM(IELEM,21)=0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 4-2 (22)
        XM(IELEM,22)=0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 5-2 (23)
        XM(IELEM,23)=0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 6-2 (24)
        XM(IELEM,24)=0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!       TERM 4-3 (25)
        XM(IELEM,25)=0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 5-3 (26)
        XM(IELEM,26)=0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 6-3 (27)
        XM(IELEM,27)=0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
!       TERM 5-4 (28)
        XM(IELEM,28)=0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 6-4 (29)
        XM(IELEM,29)=0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
!       TERM 6-5 (30)
        XM(IELEM,30)=0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
!
!       TERM 4
!
        XM(IELEM,03)=XM(IELEM,03)
     &              -(NPX2*NF1+NPY2*NG1+NH1)*SURFAC(IELEM)*XS03
        XM(IELEM,08)=XM(IELEM,08)
     &              -(NPX2*NF2+NPY2*NG2+NH2)*SURFAC(IELEM)*XS03
        XM(IELEM,12)=XM(IELEM,12)
     &              -(NPX2*NF3+NPY2*NG3+NH3)*SURFAC(IELEM)*XS03
        XM(IELEM,18)=XM(IELEM,18)
     &              -(NPX2*NF1+NPY2*NG1+NH1)*SURFAC(IELEM)*XS03
        XM(IELEM,23)=XM(IELEM,23)
     &              -(NPX2*NF2+NPY2*NG2+NH2)*SURFAC(IELEM)*XS03
        XM(IELEM,27)=XM(IELEM,27)
     &              -(NPX2*NF3+NPY2*NG3+NH3)*SURFAC(IELEM)*XS03
!
!-----------------------------------------------------------------------
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,*) 'MT02PP_STAR (BIEF): UNKNOWN FORMULA'
        WRITE(LU,*) FORMUL
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     DIAGONAL TERMS OBTAINED BY THE FACT THAT THE SUM OF TERMS IN A
!     LINE IS 0
!
!-----------------------------------------------------------------------
!
!     IN SYMMETRIC MODE
!
      IF(FORMUL(11:13).EQ.'MON'.OR.FORMUL(10:13).EQ.'1234') THEN
!
      DO IELEM=1,NELEM
!
        T(IELEM,1)= -XM(IELEM,01)
     &              -XM(IELEM,02)
     &              -XM(IELEM,03)
     &              -XM(IELEM,04)
     &              -XM(IELEM,05)
        T(IELEM,2)= -XM(IELEM,01)
     &              -XM(IELEM,06)
     &              -XM(IELEM,07)
     &              -XM(IELEM,08)
     &              -XM(IELEM,09)
        T(IELEM,3)= -XM(IELEM,02)
     &              -XM(IELEM,06)
     &              -XM(IELEM,10)
     &              -XM(IELEM,11)
     &              -XM(IELEM,12)
        T(IELEM,4)= -XM(IELEM,03)
     &              -XM(IELEM,07)
     &              -XM(IELEM,10)
     &              -XM(IELEM,13)
     &              -XM(IELEM,14)
        T(IELEM,5)= -XM(IELEM,04)
     &              -XM(IELEM,08)
     &              -XM(IELEM,11)
     &              -XM(IELEM,13)
     &              -XM(IELEM,15)
        T(IELEM,6)= -XM(IELEM,05)
     &              -XM(IELEM,09)
     &              -XM(IELEM,12)
     &              -XM(IELEM,14)
     &              -XM(IELEM,15)
!
      ENDDO
!
!     IN NON SYMMETRIC MODE
!
      ELSEIF(FORMUL(10:13).EQ.'1 3 '.OR.FORMUL(10:13).EQ.' 2 4') THEN
!
      DO IELEM=1,NELEM
!
        T(IELEM,1)= -XM(IELEM,01)
     &              -XM(IELEM,02)
     &              -XM(IELEM,03)
     &              -XM(IELEM,04)
     &              -XM(IELEM,05)
        T(IELEM,2)= -XM(IELEM,16)
     &              -XM(IELEM,06)
     &              -XM(IELEM,07)
     &              -XM(IELEM,08)
     &              -XM(IELEM,09)
        T(IELEM,3)= -XM(IELEM,17)
     &              -XM(IELEM,21)
     &              -XM(IELEM,10)
     &              -XM(IELEM,11)
     &              -XM(IELEM,12)
        T(IELEM,4)= -XM(IELEM,18)
     &              -XM(IELEM,22)
     &              -XM(IELEM,25)
     &              -XM(IELEM,13)
     &              -XM(IELEM,14)
        T(IELEM,5)= -XM(IELEM,19)
     &              -XM(IELEM,23)
     &              -XM(IELEM,26)
     &              -XM(IELEM,28)
     &              -XM(IELEM,15)
        T(IELEM,6)= -XM(IELEM,20)
     &              -XM(IELEM,24)
     &              -XM(IELEM,27)
     &              -XM(IELEM,29)
     &              -XM(IELEM,30)
!
      ENDDO
!
      ELSE
        WRITE(LU,*) 'MT02PP_STAR (BIEF): UNKNOWN FORMULA'
        WRITE(LU,*) FORMUL
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
