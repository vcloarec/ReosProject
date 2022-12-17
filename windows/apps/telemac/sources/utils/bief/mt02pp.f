!                   *****************
                    SUBROUTINE MT02PP
!                   *****************
!
     &(T,XM,XMUL,SF,SG,SH,F,G,H,X,Y,Z,SURFAC,IKLE,NELEM,NELMAX,INCHYD,
     & FORMUL,NPLAN)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE DIFFUSION MATRIX.
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
!warning  JMH : THIS IS NOT DONE AS IN 2D, TO BE MODIFIED
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        11/03/2014
!+        V7P0
!+   Now SH%TYPR checked to cancel vertical diffusion.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE FORMULA
!| FORMUL         |-->| FORMULA DESCRIBING THE RESULTING MATRIX
!| G              |-->| FUNCTION USED IN THE FORMULA
!| H              |-->| FUNCTION USED IN THE FORMULA
!| IKLE           |-->| CONNECTIVITY TABLE.
!| INCHYD         |---| IF YES, TREATS HYDROSTATIC INCONSISTENCIES
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
      USE BIEF, EX_MT02PP => MT02PP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPLAN
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
!
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6),XM(NELMAX,15)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
      DOUBLE PRECISION, INTENT(IN)    :: F(*),G(*),H(*)
!
!     STRUCTURES OF F, G, H
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
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      DOUBLE PRECISION H1,H2,H3,RI,RS,R,RRI,RRS,RRRI,RRRS
      DOUBLE PRECISION D1,D2,D3,D12,D13,D23,D
      DOUBLE PRECISION SH1,SHH,SNI,SNS,SNHI,SNHS,SNHHI,SNHHS,SNHH
      DOUBLE PRECISION SNHI1,SNHS1,SNHI2,SNHS2,SNHI3,SNHS3
      DOUBLE PRECISION HHI12,HHS12,HH12,HHI13,HHS13,HH13
      DOUBLE PRECISION HHI23,HHS23,HH23
      DOUBLE PRECISION HRI1,HRS1,HRI2,HRS2,HRI3,HRS3
      DOUBLE PRECISION RR11,RR22,RR33,RR12,RR13,RR23
      DOUBLE PRECISION NF1,NF2,NF3,NF4,NF5,NF6
      DOUBLE PRECISION NG1,NG2,NG3,NG4,NG5,NG6
      DOUBLE PRECISION NH1,NH2,NH3,XS06,XS2880
!
      INTEGER I1,I2,I3,I4,I5,I6,IELEM,II4,II5,II6,NPOU0
!
      DOUBLE PRECISION, PARAMETER :: CHOUIA = 1.D-4
!
!-----------------------------------------------------------------------
!
      XS06=XMUL/6.D0
      XS2880=XMUL/2880.D0
!
!-----------------------------------------------------------------------
!
!     VERY IMPORTANT !!!!!!!!!
!
!     SH%TYPR CHECKED TO CANCEL DIFFUSION ALONG Z
!
!     NOTE: XS06 ONLY USED WITH DIFFUSION ALONG Z !!!!!!!!!
!
      IF(SH%TYPR.EQ.'0') THEN
        XS06=0.D0
      ENDIF
!
!     COULD BE OPTIMISED MORE BUT WOULD REQUEST SPLITTING LOOPS
!     INTO HORIZONTAL AND VERTICAL
!
!-----------------------------------------------------------------------
!
      IF(SF%ELM.NE.41) THEN
        WRITE(LU,1001) SF%ELM
1001    FORMAT(1X,'MT02PP (BIEF) : TYPE OF F NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(SG%ELM.NE.41) THEN
        WRITE(LU,2001) SG%ELM
2001    FORMAT(1X,'MT02PP (BIEF) : TYPE OF G NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(SH%ELM.NE.41) THEN
        WRITE(LU,3001) SH%ELM
3001    FORMAT(1X,'MT02PP (BIEF) : TYPE OF H NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     SEE VISCLM OF TELEMAC-3D
!     FOR THE TREATMENT OF P0 VERTICAL VISCOSITY ON THE VERTICAL
!
      IF(SH%DIMDISC.EQ.0) THEN
!       P1 VERTICAL VISCOSITY
        NPOU0=SH%DIM1/NPLAN
      ELSEIF(SH%DIMDISC.EQ.4111) THEN
!       P0 VERTICAL VISCOSITY ON THE VERTICAL (SEE II4,5,6)
        NPOU0=0
      ELSE
        WRITE(LU,4001) SH%DIMDISC
4001    FORMAT(1X,'MT02PP (BIEF): DIMDISC OF H NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     VERSION WITH TREATMENT OF HYDROSTATIC INCONSISTENCIES
!
      IF(INCHYD) THEN
!
!-----------------------------------------------------------------------
!
!     DIFFUSION ALONG X
!
!-----------------------------------------------------------------------
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM=1,NELEM
!
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
        I4=IKLE(IELEM,4)
        I5=IKLE(IELEM,5)
        I6=IKLE(IELEM,6)
!       DEPENDING ON NPOU0, II4 WILL BE I4 OR I1, ETC
        II4=I1+NPOU0
        II5=I2+NPOU0
        II6=I3+NPOU0
!
        H1=Z(I4)-Z(I1)
        H2=Z(I5)-Z(I2)
        H3=Z(I6)-Z(I3)
!
        SH1=H1+H2+H3
        SHH=H1*H1+H2*H2+H3*H3
!
        D1=Y(IELEM,2)-Y(IELEM,3)
!       D2=Y(IELEM,3)-Y(IELEM,1)
        D2=Y(IELEM,3)
!       D3=Y(IELEM,1)-Y(IELEM,2)
        D3=          -Y(IELEM,2)
!
        D=XS2880/SURFAC(IELEM)
!
        RI=-(Z(I1)*D1+Z(I2)*D2+Z(I3)*D3)
        RS=-(Z(I4)*D1+Z(I5)*D2+Z(I6)*D3)
        R=RI+RS
        RRI=R+RI+RI
        RRS=R+RS+RS
        RRRI=R*R+2*RI*RI
        RRRS=R*R+2*RS*RS
!
        D12=D1*D2
        D13=D1*D3
        D23=D2*D3
!
        IF(MAX(Z(I1),Z(I2),Z(I3)).GT.MIN(Z(I4),Z(I5),Z(I6)).OR.
     &     H1.LT.CHOUIA.OR.H2.LT.CHOUIA.OR.H3.LT.CHOUIA ) THEN
          NF1=0.D0
          NF2=0.D0
          NF3=0.D0
          NF4=0.D0
          NF5=0.D0
          NF6=0.D0
          NG1=0.D0
          NG2=0.D0
          NG3=0.D0
          NG4=0.D0
          NG5=0.D0
          NG6=0.D0
          NH1=0.D0
          NH2=0.D0
          NH3=0.D0
        ELSE
          NF1=F(I1)/H1
          NF2=F(I2)/H2
          NF3=F(I3)/H3
          NF4=F(I4)/H1
          NF5=F(I5)/H2
          NF6=F(I6)/H3
          NG1=G(I1)/H1
          NG2=G(I2)/H2
          NG3=G(I3)/H3
          NG4=G(I4)/H1
          NG5=G(I5)/H2
          NG6=G(I6)/H3
!         DEPENDING ON THE CASE (II4=I1 OR I4, ETC.)
!         ALTERNATIVE WITH VERTICAL LINEAR VISCOSITY (II4=I4)
!         ALTERNATIVE WITH P0 VERTICAL VISCOSITY ON THE VERTICAL (II4=I1)
          NH1=(H(I1)+H(II4))/H1
          NH2=(H(I2)+H(II5))/H2
          NH3=(H(I3)+H(II6))/H3
        ENDIF
!
        SNI=NF1+NF2+NF3
        SNS=NF4+NF5+NF6
        SNHI=NF1*H1+NF2*H2+NF3*H3
        SNHS=NF4*H1+NF5*H2+NF6*H3
        SNHHI=(SNI*SH1+SNHI+SNHI)*SH1+SNI*SHH
     &       +2*(NF1*H1*H1+NF2*H2*H2+NF3*H3*H3)
        SNHHS=(SNS*SH1+SNHS+SNHS)*SH1+SNS*SHH
     &       +2*(NF4*H1*H1+NF5*H2*H2+NF6*H3*H3)
        SNHH=SNHHI+SNHHS
        SNHHI=SNHH+SNHHI+SNHHI
        SNHHS=SNHH+SNHHS+SNHHS
!
        HHI12=SNHHI*D12
        HHI13=SNHHI*D13
        HHI23=SNHHI*D23
        HHS12=SNHHS*D12
        HHS13=SNHHS*D13
        HHS23=SNHHS*D23
        HH12=SNHH*D12
        HH13=SNHH*D13
        HH23=SNHH*D23
!
        SNHI1=(SNI+NF1)*(SH1+H1)+SNHI+NF1*H1
        SNHS1=(SNS+NF4)*(SH1+H1)+SNHS+NF4*H1
        SNHI2=(SNI+NF2)*(SH1+H2)+SNHI+NF2*H2
        SNHS2=(SNS+NF5)*(SH1+H2)+SNHS+NF5*H2
        SNHI3=(SNI+NF3)*(SH1+H3)+SNHI+NF3*H3
        SNHS3=(SNS+NF6)*(SH1+H3)+SNHS+NF6*H3
!
        HRI1=RRI*SNHI1+R*SNHS1
        HRS1=RRS*SNHS1+R*SNHI1
        HRI2=RRI*SNHI2+R*SNHS2
        HRS2=RRS*SNHS2+R*SNHI2
        HRI3=RRI*SNHI3+R*SNHS3
        HRS3=RRS*SNHS3+R*SNHI3
!
        RR11=2*(RRRI*(SNI+NF1+NF1)+RRRS*(SNS+NF4+NF4))
        RR22=2*(RRRI*(SNI+NF2+NF2)+RRRS*(SNS+NF5+NF5))
        RR33=2*(RRRI*(SNI+NF3+NF3)+RRRS*(SNS+NF6+NF6))
        RR12=   RRRI*(SNI+NF1+NF2)+RRRS*(SNS+NF4+NF5)
        RR13=   RRRI*(SNI+NF1+NF3)+RRRS*(SNS+NF4+NF6)
        RR23=   RRRI*(SNI+NF2+NF3)+RRRS*(SNS+NF5+NF6)
!
!       EXTRA-DIAGONAL TERMS
!
        XM(IELEM, 1)=D*( HHI12  -D1*HRI2-D2*HRI1 +RR12)
        XM(IELEM, 2)=D*( HHI13  -D1*HRI3-D3*HRI1 +RR13)
        XM(IELEM, 3)=D*(-HH12-HH13+D1*(HRI1-HRS1)-RR11)
        XM(IELEM, 4)=D*( HH12   +D1*HRI2-D2*HRS1 -RR12)
        XM(IELEM, 5)=D*( HH13   +D1*HRI3-D3*HRS1 -RR13)
        XM(IELEM, 6)=D*( HHI23  -D2*HRI3-D3*HRI2 +RR23)
        XM(IELEM, 7)=D*( HH12   +D2*HRI1-D1*HRS2 -RR12)
        XM(IELEM, 8)=D*(-HH12-HH23+D2*(HRI2-HRS2)-RR22)
        XM(IELEM, 9)=D*( HH23   +D2*HRI3-D3*HRS2 -RR23)
        XM(IELEM,10)=D*( HH13   +D3*HRI1-D1*HRS3 -RR13)
        XM(IELEM,11)=D*( HH23   +D3*HRI2-D2*HRS3 -RR23)
        XM(IELEM,12)=D*(-HH13-HH23+D3*(HRI3-HRS3)-RR33)
        XM(IELEM,13)=D*( HHS12  +D1*HRS2+D2*HRS1 +RR12)
        XM(IELEM,14)=D*( HHS13  +D1*HRS3+D3*HRS1 +RR13)
        XM(IELEM,15)=D*( HHS23  +D2*HRS3+D3*HRS2 +RR23)
!
!-----------------------------------------------------------------------
!
!        DIFFUSION ALONG Y
!
!-----------------------------------------------------------------------
!
        D1=X(IELEM,3)-X(IELEM,2)
!       D2=X(IELEM,1)-X(IELEM,3)
        D2=          -X(IELEM,3)
!       D3=X(IELEM,2)-X(IELEM,1)
        D3=X(IELEM,2)
!
        RI=-(Z(I1)*D1+Z(I2)*D2+Z(I3)*D3)
        RS=-(Z(I4)*D1+Z(I5)*D2+Z(I6)*D3)
        R=RI+RS
        RRI=R+RI+RI
        RRS=R+RS+RS
        RRRI=R*R+2*RI*RI
        RRRS=R*R+2*RS*RS
!
        D12=D1*D2
        D13=D1*D3
        D23=D2*D3
!
        SNI=NG1+NG2+NG3
        SNS=NG4+NG5+NG6
        SNHI=NG1*H1+NG2*H2+NG3*H3
        SNHS=NG4*H1+NG5*H2+NG6*H3
        SNHHI=(SNI*SH1+SNHI+SNHI)*SH1+SNI*SHH
     &       +2*(NG1*H1*H1+NG2*H2*H2+NG3*H3*H3)
        SNHHS=(SNS*SH1+SNHS+SNHS)*SH1+SNS*SHH
     &       +2*(NG4*H1*H1+NG5*H2*H2+NG6*H3*H3)
        SNHH=SNHHI+SNHHS
        SNHHI=SNHH+SNHHI+SNHHI
        SNHHS=SNHH+SNHHS+SNHHS
!
        HHI12=SNHHI*D12
        HHI13=SNHHI*D13
        HHI23=SNHHI*D23
        HHS12=SNHHS*D12
        HHS13=SNHHS*D13
        HHS23=SNHHS*D23
        HH12=SNHH*D12
        HH13=SNHH*D13
        HH23=SNHH*D23
!
        SNHI1=(SNI+NG1)*(SH1+H1)+SNHI+NG1*H1
        SNHS1=(SNS+NG4)*(SH1+H1)+SNHS+NG4*H1
        SNHI2=(SNI+NG2)*(SH1+H2)+SNHI+NG2*H2
        SNHS2=(SNS+NG5)*(SH1+H2)+SNHS+NG5*H2
        SNHI3=(SNI+NG3)*(SH1+H3)+SNHI+NG3*H3
        SNHS3=(SNS+NG6)*(SH1+H3)+SNHS+NG6*H3
!
        HRI1=RRI*SNHI1+R*SNHS1
        HRS1=RRS*SNHS1+R*SNHI1
        HRI2=RRI*SNHI2+R*SNHS2
        HRS2=RRS*SNHS2+R*SNHI2
        HRI3=RRI*SNHI3+R*SNHS3
        HRS3=RRS*SNHS3+R*SNHI3
!
        RR11=2*(RRRI*(SNI+NG1+NG1)+RRRS*(SNS+NG4+NG4))
        RR22=2*(RRRI*(SNI+NG2+NG2)+RRRS*(SNS+NG5+NG5))
        RR33=2*(RRRI*(SNI+NG3+NG3)+RRRS*(SNS+NG6+NG6))
        RR12=   RRRI*(SNI+NG1+NG2)+RRRS*(SNS+NG4+NG5)
        RR13=   RRRI*(SNI+NG1+NG3)+RRRS*(SNS+NG4+NG6)
        RR23=   RRRI*(SNI+NG2+NG3)+RRRS*(SNS+NG5+NG6)
!
!       EXTRA-DIAGONAL TERMS
!
        XM(IELEM, 1)=XM(IELEM, 1)+D*( HHI12  -D1*HRI2-D2*HRI1 +RR12)
        XM(IELEM, 2)=XM(IELEM, 2)+D*( HHI13  -D1*HRI3-D3*HRI1 +RR13)
        XM(IELEM, 3)=XM(IELEM, 3)+D*(-HH12-HH13+D1*(HRI1-HRS1)-RR11)
        XM(IELEM, 4)=XM(IELEM, 4)+D*( HH12   +D1*HRI2-D2*HRS1 -RR12)
        XM(IELEM, 5)=XM(IELEM, 5)+D*( HH13   +D1*HRI3-D3*HRS1 -RR13)
        XM(IELEM, 6)=XM(IELEM, 6)+D*( HHI23  -D2*HRI3-D3*HRI2 +RR23)
        XM(IELEM, 7)=XM(IELEM, 7)+D*( HH12   +D2*HRI1-D1*HRS2 -RR12)
        XM(IELEM, 8)=XM(IELEM, 8)+D*(-HH12-HH23+D2*(HRI2-HRS2)-RR22)
        XM(IELEM, 9)=XM(IELEM, 9)+D*( HH23   +D2*HRI3-D3*HRS2 -RR23)
        XM(IELEM,10)=XM(IELEM,10)+D*( HH13   +D3*HRI1-D1*HRS3 -RR13)
        XM(IELEM,11)=XM(IELEM,11)+D*( HH23   +D3*HRI2-D2*HRS3 -RR23)
        XM(IELEM,12)=XM(IELEM,12)+D*(-HH13-HH23+D3*(HRI3-HRS3)-RR33)
        XM(IELEM,13)=XM(IELEM,13)+D*( HHS12  +D1*HRS2+D2*HRS1 +RR12)
        XM(IELEM,14)=XM(IELEM,14)+D*( HHS13  +D1*HRS3+D3*HRS1 +RR13)
        XM(IELEM,15)=XM(IELEM,15)+D*( HHS23  +D2*HRS3+D3*HRS2 +RR23)
!
!-----------------------------------------------------------------------
!
!        DIFFUSION ALONG Z
!
!-----------------------------------------------------------------------
!
!       VERSION WITH SIMPLIFICATIONS TO ACHIEVE MONOTONY OF THE MATRIX
!
        D=SURFAC(IELEM)*XS06
!
!       EXTRA-DIAGONAL TERMS
!
        XM(IELEM, 3)=XM(IELEM, 3)-D*NH1
        XM(IELEM, 8)=XM(IELEM, 8)-D*NH2
        XM(IELEM,12)=XM(IELEM,12)-D*NH3
!
!-----------------------------------------------------------------------
!
!       OLD VERSION OF DIFFUSION ALONG Z
!
!       R=NH1+NH2+NH3+NH4+NH5+NH6
!       D=((X(I2)-X(I1))*(Y(I3)-Y(I1))-(X(I3)-X(I1))*(Y(I2)-Y(I1)))
!    *   *XMUL/240.D0
!
!       RR11=(R+NH1+NH1+NH4+NH4)*(D+D)
!       RR22=(R+NH2+NH2+NH5+NH5)*(D+D)
!       RR33=(R+NH3+NH3+NH6+NH6)*(D+D)
!       RR12=(R+NH1+NH2+NH4+NH5)*D
!       RR13=(R+NH1+NH3+NH4+NH6)*D
!       RR23=(R+NH2+NH3+NH5+NH6)*D
!
!       EXTRA-DIAGONAL TERMS
!
!       XM(IELEM, 1)=XM(IELEM, 1)+RR12
!       XM(IELEM, 2)=XM(IELEM, 2)+RR13
!       XM(IELEM, 3)=XM(IELEM, 3)-RR11
!       XM(IELEM, 4)=XM(IELEM, 4)-RR12
!       XM(IELEM, 5)=XM(IELEM, 5)-RR13
!       XM(IELEM, 6)=XM(IELEM, 6)+RR23
!       XM(IELEM, 7)=XM(IELEM, 7)-RR12
!       XM(IELEM, 8)=XM(IELEM, 8)-RR22
!       XM(IELEM, 9)=XM(IELEM, 9)-RR23
!       XM(IELEM,10)=XM(IELEM,10)-RR13
!       XM(IELEM,11)=XM(IELEM,11)-RR23
!       XM(IELEM,12)=XM(IELEM,12)-RR33
!       XM(IELEM,13)=XM(IELEM,13)+RR12
!       XM(IELEM,14)=XM(IELEM,14)+RR13
!       XM(IELEM,15)=XM(IELEM,15)+RR23
!
!
!-----------------------------------------------------------------------
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
!     VERSION WITHOUT TREATMENT OF HYDROSTATIC INCONSISTENCIES
!
!-----------------------------------------------------------------------
!
!     DIFFUSION ALONG X
!
!-----------------------------------------------------------------------
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM=1,NELEM
!
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
        I4=IKLE(IELEM,4)
        I5=IKLE(IELEM,5)
        I6=IKLE(IELEM,6)
!       DEPENDING ON NPOU0, II4 WILL BE I4 OR I1, ETC
        II4=I1+NPOU0
        II5=I2+NPOU0
        II6=I3+NPOU0
!
        H1=Z(I4)-Z(I1)
        H2=Z(I5)-Z(I2)
        H3=Z(I6)-Z(I3)
!
        SH1=H1+H2+H3
        SHH=H1*H1+H2*H2+H3*H3
!
        D1=Y(IELEM,2)-Y(IELEM,3)
!       D2=Y(IELEM,3)-Y(IELEM,1)
        D2=Y(IELEM,3)
!       D3=Y(IELEM,1)-Y(IELEM,2)
        D3=          -Y(IELEM,2)
!
        D=XS2880/SURFAC(IELEM)
!
        RI=-(Z(I1)*D1+Z(I2)*D2+Z(I3)*D3)
        RS=-(Z(I4)*D1+Z(I5)*D2+Z(I6)*D3)
        R=RI+RS
        RRI=R+RI+RI
        RRS=R+RS+RS
        RRRI=R*R+2*RI*RI
        RRRS=R*R+2*RS*RS
!
        D12=D1*D2
        D13=D1*D3
        D23=D2*D3
!
        IF(H1.LT.CHOUIA.OR.H2.LT.CHOUIA.OR.H3.LT.CHOUIA) THEN
          NF1=0.D0
          NF2=0.D0
          NF3=0.D0
          NF4=0.D0
          NF5=0.D0
          NF6=0.D0
          NG1=0.D0
          NG2=0.D0
          NG3=0.D0
          NG4=0.D0
          NG5=0.D0
          NG6=0.D0
          NH1=0.D0
          NH2=0.D0
          NH3=0.D0
        ELSE
          NF1=F(I1)/H1
          NF2=F(I2)/H2
          NF3=F(I3)/H3
          NF4=F(I4)/H1
          NF5=F(I5)/H2
          NF6=F(I6)/H3
          NG1=G(I1)/H1
          NG2=G(I2)/H2
          NG3=G(I3)/H3
          NG4=G(I4)/H1
          NG5=G(I5)/H2
          NG6=G(I6)/H3
!         DEPENDING ON THE CASE (II4=I1 OR I4, ETC.)
!         ALTERNATIVE WITH VERTICAL LINEAR VISCOSITY (II4=I4)
!         ALTERNATIVE WITH P0 VERTICAL VISCOSITY ON THE VERTICAL (II4=I1)
          NH1=(H(I1)+H(II4))/H1
          NH2=(H(I2)+H(II5))/H2
          NH3=(H(I3)+H(II6))/H3
        ENDIF
!
        SNI=NF1+NF2+NF3
        SNS=NF4+NF5+NF6
        SNHI=NF1*H1+NF2*H2+NF3*H3
        SNHS=NF4*H1+NF5*H2+NF6*H3
        SNHHI=(SNI*SH1+SNHI+SNHI)*SH1+SNI*SHH
     &       +2*(NF1*H1*H1+NF2*H2*H2+NF3*H3*H3)
        SNHHS=(SNS*SH1+SNHS+SNHS)*SH1+SNS*SHH
     &       +2*(NF4*H1*H1+NF5*H2*H2+NF6*H3*H3)
        SNHH=SNHHI+SNHHS
        SNHHI=SNHH+SNHHI+SNHHI
        SNHHS=SNHH+SNHHS+SNHHS
!
        HHI12=SNHHI*D12
        HHI13=SNHHI*D13
        HHI23=SNHHI*D23
        HHS12=SNHHS*D12
        HHS13=SNHHS*D13
        HHS23=SNHHS*D23
        HH12=SNHH*D12
        HH13=SNHH*D13
        HH23=SNHH*D23
!
        SNHI1=(SNI+NF1)*(SH1+H1)+SNHI+NF1*H1
        SNHS1=(SNS+NF4)*(SH1+H1)+SNHS+NF4*H1
        SNHI2=(SNI+NF2)*(SH1+H2)+SNHI+NF2*H2
        SNHS2=(SNS+NF5)*(SH1+H2)+SNHS+NF5*H2
        SNHI3=(SNI+NF3)*(SH1+H3)+SNHI+NF3*H3
        SNHS3=(SNS+NF6)*(SH1+H3)+SNHS+NF6*H3
!
        HRI1=RRI*SNHI1+R*SNHS1
        HRS1=RRS*SNHS1+R*SNHI1
        HRI2=RRI*SNHI2+R*SNHS2
        HRS2=RRS*SNHS2+R*SNHI2
        HRI3=RRI*SNHI3+R*SNHS3
        HRS3=RRS*SNHS3+R*SNHI3
!
        RR11=2*(RRRI*(SNI+NF1+NF1)+RRRS*(SNS+NF4+NF4))
        RR22=2*(RRRI*(SNI+NF2+NF2)+RRRS*(SNS+NF5+NF5))
        RR33=2*(RRRI*(SNI+NF3+NF3)+RRRS*(SNS+NF6+NF6))
        RR12=   RRRI*(SNI+NF1+NF2)+RRRS*(SNS+NF4+NF5)
        RR13=   RRRI*(SNI+NF1+NF3)+RRRS*(SNS+NF4+NF6)
        RR23=   RRRI*(SNI+NF2+NF3)+RRRS*(SNS+NF5+NF6)
!
!       EXTRA-DIAGONAL TERMS
!
        XM(IELEM, 1)=D*( HHI12  -D1*HRI2-D2*HRI1 +RR12)
        XM(IELEM, 2)=D*( HHI13  -D1*HRI3-D3*HRI1 +RR13)
        XM(IELEM, 3)=D*(-HH12-HH13+D1*(HRI1-HRS1)-RR11)
        XM(IELEM, 4)=D*( HH12   +D1*HRI2-D2*HRS1 -RR12)
        XM(IELEM, 5)=D*( HH13   +D1*HRI3-D3*HRS1 -RR13)
        XM(IELEM, 6)=D*( HHI23  -D2*HRI3-D3*HRI2 +RR23)
        XM(IELEM, 7)=D*( HH12   +D2*HRI1-D1*HRS2 -RR12)
        XM(IELEM, 8)=D*(-HH12-HH23+D2*(HRI2-HRS2)-RR22)
        XM(IELEM, 9)=D*( HH23   +D2*HRI3-D3*HRS2 -RR23)
        XM(IELEM,10)=D*( HH13   +D3*HRI1-D1*HRS3 -RR13)
        XM(IELEM,11)=D*( HH23   +D3*HRI2-D2*HRS3 -RR23)
        XM(IELEM,12)=D*(-HH13-HH23+D3*(HRI3-HRS3)-RR33)
        XM(IELEM,13)=D*( HHS12  +D1*HRS2+D2*HRS1 +RR12)
        XM(IELEM,14)=D*( HHS13  +D1*HRS3+D3*HRS1 +RR13)
        XM(IELEM,15)=D*( HHS23  +D2*HRS3+D3*HRS2 +RR23)
!
!-----------------------------------------------------------------------
!
!        DIFFUSION ALONG Y
!
!-----------------------------------------------------------------------
!
        D1=X(IELEM,3)-X(IELEM,2)
!       D2=X(IELEM,1)-X(IELEM,3)
        D2=          -X(IELEM,3)
!       D3=X(IELEM,2)-X(IELEM,1)
        D3=X(IELEM,2)
!
        RI=-(Z(I1)*D1+Z(I2)*D2+Z(I3)*D3)
        RS=-(Z(I4)*D1+Z(I5)*D2+Z(I6)*D3)
        R=RI+RS
        RRI=R+RI+RI
        RRS=R+RS+RS
        RRRI=R*R+2*RI*RI
        RRRS=R*R+2*RS*RS
!
        D12=D1*D2
        D13=D1*D3
        D23=D2*D3
!
        SNI=NG1+NG2+NG3
        SNS=NG4+NG5+NG6
        SNHI=NG1*H1+NG2*H2+NG3*H3
        SNHS=NG4*H1+NG5*H2+NG6*H3
        SNHHI=(SNI*SH1+SNHI+SNHI)*SH1+SNI*SHH
     &       +2*(NG1*H1*H1+NG2*H2*H2+NG3*H3*H3)
        SNHHS=(SNS*SH1+SNHS+SNHS)*SH1+SNS*SHH
     &       +2*(NG4*H1*H1+NG5*H2*H2+NG6*H3*H3)
        SNHH=SNHHI+SNHHS
        SNHHI=SNHH+SNHHI+SNHHI
        SNHHS=SNHH+SNHHS+SNHHS
!
        HHI12=SNHHI*D12
        HHI13=SNHHI*D13
        HHI23=SNHHI*D23
        HHS12=SNHHS*D12
        HHS13=SNHHS*D13
        HHS23=SNHHS*D23
        HH12=SNHH*D12
        HH13=SNHH*D13
        HH23=SNHH*D23
!
        SNHI1=(SNI+NG1)*(SH1+H1)+SNHI+NG1*H1
        SNHS1=(SNS+NG4)*(SH1+H1)+SNHS+NG4*H1
        SNHI2=(SNI+NG2)*(SH1+H2)+SNHI+NG2*H2
        SNHS2=(SNS+NG5)*(SH1+H2)+SNHS+NG5*H2
        SNHI3=(SNI+NG3)*(SH1+H3)+SNHI+NG3*H3
        SNHS3=(SNS+NG6)*(SH1+H3)+SNHS+NG6*H3
!
        HRI1=RRI*SNHI1+R*SNHS1
        HRS1=RRS*SNHS1+R*SNHI1
        HRI2=RRI*SNHI2+R*SNHS2
        HRS2=RRS*SNHS2+R*SNHI2
        HRI3=RRI*SNHI3+R*SNHS3
        HRS3=RRS*SNHS3+R*SNHI3
!
        RR11=2*(RRRI*(SNI+NG1+NG1)+RRRS*(SNS+NG4+NG4))
        RR22=2*(RRRI*(SNI+NG2+NG2)+RRRS*(SNS+NG5+NG5))
        RR33=2*(RRRI*(SNI+NG3+NG3)+RRRS*(SNS+NG6+NG6))
        RR12=   RRRI*(SNI+NG1+NG2)+RRRS*(SNS+NG4+NG5)
        RR13=   RRRI*(SNI+NG1+NG3)+RRRS*(SNS+NG4+NG6)
        RR23=   RRRI*(SNI+NG2+NG3)+RRRS*(SNS+NG5+NG6)
!
!       EXTRA-DIAGONAL TERMS
!
        XM(IELEM, 1)=XM(IELEM, 1)+D*( HHI12  -D1*HRI2-D2*HRI1 +RR12)
        XM(IELEM, 2)=XM(IELEM, 2)+D*( HHI13  -D1*HRI3-D3*HRI1 +RR13)
        XM(IELEM, 3)=XM(IELEM, 3)+D*(-HH12-HH13+D1*(HRI1-HRS1)-RR11)
        XM(IELEM, 4)=XM(IELEM, 4)+D*( HH12   +D1*HRI2-D2*HRS1 -RR12)
        XM(IELEM, 5)=XM(IELEM, 5)+D*( HH13   +D1*HRI3-D3*HRS1 -RR13)
        XM(IELEM, 6)=XM(IELEM, 6)+D*( HHI23  -D2*HRI3-D3*HRI2 +RR23)
        XM(IELEM, 7)=XM(IELEM, 7)+D*( HH12   +D2*HRI1-D1*HRS2 -RR12)
        XM(IELEM, 8)=XM(IELEM, 8)+D*(-HH12-HH23+D2*(HRI2-HRS2)-RR22)
        XM(IELEM, 9)=XM(IELEM, 9)+D*( HH23   +D2*HRI3-D3*HRS2 -RR23)
        XM(IELEM,10)=XM(IELEM,10)+D*( HH13   +D3*HRI1-D1*HRS3 -RR13)
        XM(IELEM,11)=XM(IELEM,11)+D*( HH23   +D3*HRI2-D2*HRS3 -RR23)
        XM(IELEM,12)=XM(IELEM,12)+D*(-HH13-HH23+D3*(HRI3-HRS3)-RR33)
        XM(IELEM,13)=XM(IELEM,13)+D*( HHS12  +D1*HRS2+D2*HRS1 +RR12)
        XM(IELEM,14)=XM(IELEM,14)+D*( HHS13  +D1*HRS3+D3*HRS1 +RR13)
        XM(IELEM,15)=XM(IELEM,15)+D*( HHS23  +D2*HRS3+D3*HRS2 +RR23)
!
!-----------------------------------------------------------------------
!
!       DIFFUSION ALONG Z
!
!-----------------------------------------------------------------------
!
!       VERSION WITH SIMPLIFICATIONS TO ACHIEVE MONOTONY OF THE MATRIX
!
        D=SURFAC(IELEM)*XS06
!
!       EXTRA-DIAGONAL TERMS
!
        XM(IELEM, 3)=XM(IELEM, 3)-D*NH1
        XM(IELEM, 8)=XM(IELEM, 8)-D*NH2
        XM(IELEM,12)=XM(IELEM,12)-D*NH3
!
!-----------------------------------------------------------------------
!
      ENDDO
!
!     IF(INCHYD) THEN
      ENDIF
!
!-----------------------------------------------------------------------
!
!     POSITIVE EXTRA-DIAGONAL TERMS REMOVED
!     TO ENSURE MONOTONY
!
      IF(FORMUL(14:16).EQ.'MON') THEN
!
        IF(XMUL.GT.0.D0) THEN
          DO IELEM=1,NELEM
            XM(IELEM, 1)=MIN(XM(IELEM, 1),0.D0)
            XM(IELEM, 2)=MIN(XM(IELEM, 2),0.D0)
            XM(IELEM, 3)=MIN(XM(IELEM, 3),0.D0)
            XM(IELEM, 4)=MIN(XM(IELEM, 4),0.D0)
            XM(IELEM, 5)=MIN(XM(IELEM, 5),0.D0)
            XM(IELEM, 6)=MIN(XM(IELEM, 6),0.D0)
            XM(IELEM, 7)=MIN(XM(IELEM, 7),0.D0)
            XM(IELEM, 8)=MIN(XM(IELEM, 8),0.D0)
            XM(IELEM, 9)=MIN(XM(IELEM, 9),0.D0)
            XM(IELEM,10)=MIN(XM(IELEM,10),0.D0)
            XM(IELEM,11)=MIN(XM(IELEM,11),0.D0)
            XM(IELEM,12)=MIN(XM(IELEM,12),0.D0)
            XM(IELEM,13)=MIN(XM(IELEM,13),0.D0)
            XM(IELEM,14)=MIN(XM(IELEM,14),0.D0)
            XM(IELEM,15)=MIN(XM(IELEM,15),0.D0)
          ENDDO
        ELSE
          DO IELEM=1,NELEM
            XM(IELEM, 1)=MAX(XM(IELEM, 1),0.D0)
            XM(IELEM, 2)=MAX(XM(IELEM, 2),0.D0)
            XM(IELEM, 3)=MAX(XM(IELEM, 3),0.D0)
            XM(IELEM, 4)=MAX(XM(IELEM, 4),0.D0)
            XM(IELEM, 5)=MAX(XM(IELEM, 5),0.D0)
            XM(IELEM, 6)=MAX(XM(IELEM, 6),0.D0)
            XM(IELEM, 7)=MAX(XM(IELEM, 7),0.D0)
            XM(IELEM, 8)=MAX(XM(IELEM, 8),0.D0)
            XM(IELEM, 9)=MAX(XM(IELEM, 9),0.D0)
            XM(IELEM,10)=MAX(XM(IELEM,10),0.D0)
            XM(IELEM,11)=MAX(XM(IELEM,11),0.D0)
            XM(IELEM,12)=MAX(XM(IELEM,12),0.D0)
            XM(IELEM,13)=MAX(XM(IELEM,13),0.D0)
            XM(IELEM,14)=MAX(XM(IELEM,14),0.D0)
            XM(IELEM,15)=MAX(XM(IELEM,15),0.D0)
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     DIAGONAL TERMS OBTAINED GIVEN THAT :
!     SUM OF THE TERMS IN A ROW =0
!
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
      RETURN
      END
