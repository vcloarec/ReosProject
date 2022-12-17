!                   ********************
                    SUBROUTINE CHAR_WEAK
!                   ********************
!
     &(FTILD,FTILD_WEAK,SURFAC,IKLE,NPOIN,NELEM,NELMAX,NG,NGAUSS,
     & MESH,T2,TB,AGGLO,IELM,NPLAN,Z,CV1,AM1,SLV,UNSV,LISTIN,SOLV)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    Completing the weak form of the method of characteristics
!+        after advection of the Gauss points.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        23/04/2013
!+        V6P3
!+     First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AM1            |-->| BIEF_OBJ WORK MATRIX
!| CV1            |-->| BIEF_OBJ WORK ARRAY
!| FTILD          |-->| BLOCK OF RESULTS
!| FTILD_WEAK     |-->| BLOCK OF RESULTS FOR ADVECTED GAUSS POINTS
!| IELM           |-->| TYPE OF ELEMENT
!| IKLE           |-->| CONNECTIVITY TABLE FOR ALL POINTS
!| NELEM          |-->| NOMBRE D'ELEMENTS
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
!| NG             |-->| TOTAL NUMBER OF GAUSS POINTS IN THE MESH
!| NGAUSS         |-->| NUMBER OF GAUSS POINTS PER ELEMENT
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| SOLV           |-->| IF YES, SOLVE THE LINEAR SYSTEM
!| SURFAC         |-->| AREA OF ELEMENTS
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM,NELMAX,NPOIN,NG,IELM
      INTEGER, INTENT(IN)             :: NPLAN,NGAUSS
!                                        HERE IKLE2 AND NELMAX2
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3)
      LOGICAL, INTENT(IN)             :: LISTIN,SOLV
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX),AGGLO,Z(*)
      TYPE(BIEF_OBJ), INTENT(IN)      :: FTILD_WEAK,UNSV
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FTILD,T2,TB,CV1,AM1
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(SLVCFG), INTENT(INOUT)     :: SLV
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,I4,I5,I6,I,IG,IPLAN
      DOUBLE PRECISION TIERS,A,B,C,D,H1,H2,H3,A1,A2,A3
      DOUBLE PRECISION WEIGH1,WEIGH2,WEIGH3
!
      TIERS=1.D0/3.D0
!
!-----------------------------------------------------------------------
!
!     INITIALISATION
!
      CALL CPSTVC(FTILD,CV1)
!
      DO I=1,NPOIN*NPLAN
        CV1%R(I)=0.D0
      ENDDO
!
      IF(NG.NE.NELEM*NGAUSS.AND.IELM.EQ.11) THEN
        WRITE(LU,*) 'CHAR_WEAK: BAD NUMBER OF POINTS'
        WRITE(LU,*) 'NG=',NG,' NELEM=',NELEM,' NGAUSS=',NGAUSS
        CALL PLANTE(1)
        STOP
      ELSEIF(NG.NE.NELEM*(NPLAN-1)*NGAUSS.AND.IELM.EQ.41) THEN
        WRITE(LU,*) 'CHAR_WEAK: BAD NUMBER OF POINTS'
        WRITE(LU,*) 'NG=',NG,' NELEM=',NELEM,' NGAUSS=',NGAUSS
        WRITE(LU,*) 'NPLAN=',NPLAN
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(NGAUSS.EQ.1.AND.IELM.EQ.11) THEN
!
!       ASSEMBLING (3 BASES PER ELEMENT)
!       HERE THE VALUE OF THE BASIS IS 1/3 FOR ALL 3 OF THEM
!       AND THE WEIGHTS ARE ALL SURFAC
!
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          CV1%R(I1)=CV1%R(I1)+SURFAC(IELEM)*TIERS*FTILD_WEAK%R(IELEM)
          CV1%R(I2)=CV1%R(I2)+SURFAC(IELEM)*TIERS*FTILD_WEAK%R(IELEM)
          CV1%R(I3)=CV1%R(I3)+SURFAC(IELEM)*TIERS*FTILD_WEAK%R(IELEM)
        ENDDO
!
      ELSEIF(NGAUSS.EQ.3.AND.IELM.EQ.11) THEN
!
!       ASSEMBLING (3 BASES PER ELEMENT)
!       HERE THE WEIGHTS ARE ALL SURFAC/3
!       THE VALUES IF THE TEST FUNCTIONS AT GAUSS POINTS ARE ON THE RIGHT
!
        IG=0
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
!         POINT DE GAUSS 1
          IG=IG+1
          CV1%R(I1)=
     &    CV1%R(I1)+SURFAC(IELEM)*TIERS*FTILD_WEAK%R(IG)*2.D0/3.D0
          CV1%R(I2)=
     &    CV1%R(I2)+SURFAC(IELEM)*TIERS*FTILD_WEAK%R(IG)*1.D0/6.D0
          CV1%R(I3)=
     &    CV1%R(I3)+SURFAC(IELEM)*TIERS*FTILD_WEAK%R(IG)*1.D0/6.D0
!         POINT DE GAUSS 2
          IG=IG+1
          CV1%R(I1)=
     &    CV1%R(I1)+SURFAC(IELEM)*TIERS*FTILD_WEAK%R(IG)*1.D0/6.D0
          CV1%R(I2)=
     &    CV1%R(I2)+SURFAC(IELEM)*TIERS*FTILD_WEAK%R(IG)*2.D0/3.D0
          CV1%R(I3)=
     &    CV1%R(I3)+SURFAC(IELEM)*TIERS*FTILD_WEAK%R(IG)*1.D0/6.D0
!         POINT DE GAUSS 3
          IG=IG+1
          CV1%R(I1)=
     &    CV1%R(I1)+SURFAC(IELEM)*TIERS*FTILD_WEAK%R(IG)*1.D0/6.D0
          CV1%R(I2)=
     &    CV1%R(I2)+SURFAC(IELEM)*TIERS*FTILD_WEAK%R(IG)*1.D0/6.D0
          CV1%R(I3)=
     &    CV1%R(I3)+SURFAC(IELEM)*TIERS*FTILD_WEAK%R(IG)*2.D0/3.D0
        ENDDO
!
      ELSEIF(NGAUSS.EQ.4.AND.IELM.EQ.11) THEN
!
!       ASSEMBLING (3 BASES PER ELEMENT)
!       THE WEIGHS ARE:
!       -27*SURFAC/48 FOR POINT 1
!        25*SURFAC/48 FOR POINT 2,3 AND 4
!
        IG=0
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
!         MONOTONICITY IF FORMULA APPROXIMATE ?
          WEIGH1=-27.D0*SURFAC(IELEM)/48.D0
          WEIGH2= 25.D0*SURFAC(IELEM)/48.D0
!         POINT DE GAUSS 1
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH1*FTILD_WEAK%R(IG)*1.D0/3.D0
          CV1%R(I2)=CV1%R(I2)+WEIGH1*FTILD_WEAK%R(IG)*1.D0/3.D0
          CV1%R(I3)=CV1%R(I3)+WEIGH1*FTILD_WEAK%R(IG)*1.D0/3.D0
!         POINT DE GAUSS 2
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH2*FTILD_WEAK%R(IG)*3.D0/5.D0
          CV1%R(I2)=CV1%R(I2)+WEIGH2*FTILD_WEAK%R(IG)*1.D0/5.D0
          CV1%R(I3)=CV1%R(I3)+WEIGH2*FTILD_WEAK%R(IG)*1.D0/5.D0
!         POINT DE GAUSS 3
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH2*FTILD_WEAK%R(IG)*1.D0/5.D0
          CV1%R(I2)=CV1%R(I2)+WEIGH2*FTILD_WEAK%R(IG)*3.D0/5.D0
          CV1%R(I3)=CV1%R(I3)+WEIGH2*FTILD_WEAK%R(IG)*1.D0/5.D0
!         POINT DE GAUSS 4
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH2*FTILD_WEAK%R(IG)*1.D0/5.D0
          CV1%R(I2)=CV1%R(I2)+WEIGH2*FTILD_WEAK%R(IG)*1.D0/5.D0
          CV1%R(I3)=CV1%R(I3)+WEIGH2*FTILD_WEAK%R(IG)*3.D0/5.D0
        ENDDO
!
      ELSEIF(NGAUSS.EQ.6.AND.IELM.EQ.11) THEN
!
!       ASSEMBLING (3 BASES PER ELEMENT)
!       THE WEIGHS ARE:
!       WEIGH1 FOR POINT 1,2,3
!       WEIGH2 FOR POINT 4,5,6
!
        IG=0
        A=0.445948490915965D0
        B=0.091576213509771D0
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          WEIGH1=SURFAC(IELEM)*2.D0*0.111690794839005D0
          WEIGH2=SURFAC(IELEM)*2.D0*0.054975871827661D0
!         POINT DE GAUSS 1
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH1*FTILD_WEAK%R(IG)*(1.D0-A-A)
          CV1%R(I2)=CV1%R(I2)+WEIGH1*FTILD_WEAK%R(IG)*A
          CV1%R(I3)=CV1%R(I3)+WEIGH1*FTILD_WEAK%R(IG)*A
!         POINT DE GAUSS 2
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH1*FTILD_WEAK%R(IG)*A
          CV1%R(I2)=CV1%R(I2)+WEIGH1*FTILD_WEAK%R(IG)*(1.D0-A-A)
          CV1%R(I3)=CV1%R(I3)+WEIGH1*FTILD_WEAK%R(IG)*A
!         POINT DE GAUSS 3
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH1*FTILD_WEAK%R(IG)*A
          CV1%R(I2)=CV1%R(I2)+WEIGH1*FTILD_WEAK%R(IG)*A
          CV1%R(I3)=CV1%R(I3)+WEIGH1*FTILD_WEAK%R(IG)*(1.D0-A-A)
!         POINT DE GAUSS 4
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH2*FTILD_WEAK%R(IG)*(1.D0-B-B)
          CV1%R(I2)=CV1%R(I2)+WEIGH2*FTILD_WEAK%R(IG)*B
          CV1%R(I3)=CV1%R(I3)+WEIGH2*FTILD_WEAK%R(IG)*B
!         POINT DE GAUSS 5
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH2*FTILD_WEAK%R(IG)*B
          CV1%R(I2)=CV1%R(I2)+WEIGH2*FTILD_WEAK%R(IG)*(1.D0-B-B)
          CV1%R(I3)=CV1%R(I3)+WEIGH2*FTILD_WEAK%R(IG)*B
!         POINT DE GAUSS 6
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH2*FTILD_WEAK%R(IG)*B
          CV1%R(I2)=CV1%R(I2)+WEIGH2*FTILD_WEAK%R(IG)*B
          CV1%R(I3)=CV1%R(I3)+WEIGH2*FTILD_WEAK%R(IG)*(1.D0-B-B)
        ENDDO
!
      ELSEIF(NGAUSS.EQ.7.AND.IELM.EQ.11) THEN
!
!       ASSEMBLING (3 BASES PER ELEMENT)
!       THE WEIGHS ARE:
!       WEIGH1 FOR POINT 1,2,3
!       WEIGH2 FOR POINT 4,5,6
!
        IG=0
        A=(6.D0+SQRT(15.D0))/21.D0
        B=4.D0/7.D0-A
        A1=9.D0/80.D0
        A2=(155.D0+SQRT(15.D0))/2400.D0
        A3=31.D0/240.D0-A2
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          WEIGH1=SURFAC(IELEM)*2.D0*A1
          WEIGH2=SURFAC(IELEM)*2.D0*A2
          WEIGH3=SURFAC(IELEM)*2.D0*A3
!         POINT DE GAUSS 1
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH1*FTILD_WEAK%R(IG)*TIERS
          CV1%R(I2)=CV1%R(I2)+WEIGH1*FTILD_WEAK%R(IG)*TIERS
          CV1%R(I3)=CV1%R(I3)+WEIGH1*FTILD_WEAK%R(IG)*TIERS
!         POINT DE GAUSS 2
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH2*FTILD_WEAK%R(IG)*(1.D0-A-A)
          CV1%R(I2)=CV1%R(I2)+WEIGH2*FTILD_WEAK%R(IG)*A
          CV1%R(I3)=CV1%R(I3)+WEIGH2*FTILD_WEAK%R(IG)*A
!         POINT DE GAUSS 3
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH2*FTILD_WEAK%R(IG)*A
          CV1%R(I2)=CV1%R(I2)+WEIGH2*FTILD_WEAK%R(IG)*(1.D0-A-A)
          CV1%R(I3)=CV1%R(I3)+WEIGH2*FTILD_WEAK%R(IG)*A
!         POINT DE GAUSS 4
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH2*FTILD_WEAK%R(IG)*A
          CV1%R(I2)=CV1%R(I2)+WEIGH2*FTILD_WEAK%R(IG)*A
          CV1%R(I3)=CV1%R(I3)+WEIGH2*FTILD_WEAK%R(IG)*(1.D0-A-A)
!         POINT DE GAUSS 5
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH3*FTILD_WEAK%R(IG)*(1.D0-B-B)
          CV1%R(I2)=CV1%R(I2)+WEIGH3*FTILD_WEAK%R(IG)*B
          CV1%R(I3)=CV1%R(I3)+WEIGH3*FTILD_WEAK%R(IG)*B
!         POINT DE GAUSS 6
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH3*FTILD_WEAK%R(IG)*B
          CV1%R(I2)=CV1%R(I2)+WEIGH3*FTILD_WEAK%R(IG)*(1.D0-B-B)
          CV1%R(I3)=CV1%R(I3)+WEIGH3*FTILD_WEAK%R(IG)*B
!         POINT DE GAUSS 7
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH3*FTILD_WEAK%R(IG)*B
          CV1%R(I2)=CV1%R(I2)+WEIGH3*FTILD_WEAK%R(IG)*B
          CV1%R(I3)=CV1%R(I3)+WEIGH3*FTILD_WEAK%R(IG)*(1.D0-B-B)
        ENDDO
!
      ELSEIF(NGAUSS.EQ.12.AND.IELM.EQ.11) THEN
!
        IG=0
        A=0.063089014491502D0
        B=0.249286745170910D0
        C=0.310352451033785D0
        D=0.053145049844816D0
        A1=0.025422453185103D0
        A2=0.058393137863189D0
        A3=0.041425537809187D0
        A3=(1.D0-6.D0*A1-6.D0*A2)/12.D0
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          WEIGH1=SURFAC(IELEM)*2.D0*A1
          WEIGH2=SURFAC(IELEM)*2.D0*A2
          WEIGH3=SURFAC(IELEM)*2.D0*A3
!         POINT DE GAUSS 1
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH1*FTILD_WEAK%R(IG)*(1.D0-A-A)
          CV1%R(I2)=CV1%R(I2)+WEIGH1*FTILD_WEAK%R(IG)*A
          CV1%R(I3)=CV1%R(I3)+WEIGH1*FTILD_WEAK%R(IG)*A
!         POINT DE GAUSS 2
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH1*FTILD_WEAK%R(IG)*A
          CV1%R(I2)=CV1%R(I2)+WEIGH1*FTILD_WEAK%R(IG)*(1.D0-A-A)
          CV1%R(I3)=CV1%R(I3)+WEIGH1*FTILD_WEAK%R(IG)*A
!         POINT DE GAUSS 3
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH1*FTILD_WEAK%R(IG)*A
          CV1%R(I2)=CV1%R(I2)+WEIGH1*FTILD_WEAK%R(IG)*A
          CV1%R(I3)=CV1%R(I3)+WEIGH1*FTILD_WEAK%R(IG)*(1.D0-A-A)
!         POINT DE GAUSS 4
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH2*FTILD_WEAK%R(IG)*(1.D0-B-B)
          CV1%R(I2)=CV1%R(I2)+WEIGH2*FTILD_WEAK%R(IG)*B
          CV1%R(I3)=CV1%R(I3)+WEIGH2*FTILD_WEAK%R(IG)*B
!         POINT DE GAUSS 5
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH2*FTILD_WEAK%R(IG)*B
          CV1%R(I2)=CV1%R(I2)+WEIGH2*FTILD_WEAK%R(IG)*(1.D0-B-B)
          CV1%R(I3)=CV1%R(I3)+WEIGH2*FTILD_WEAK%R(IG)*B
!         POINT DE GAUSS 6
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH2*FTILD_WEAK%R(IG)*B
          CV1%R(I2)=CV1%R(I2)+WEIGH2*FTILD_WEAK%R(IG)*B
          CV1%R(I3)=CV1%R(I3)+WEIGH2*FTILD_WEAK%R(IG)*(1.D0-B-B)
!         POINT DE GAUSS 7
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH3*FTILD_WEAK%R(IG)*(1.D0-C-D)
          CV1%R(I2)=CV1%R(I2)+WEIGH3*FTILD_WEAK%R(IG)*C
          CV1%R(I3)=CV1%R(I3)+WEIGH3*FTILD_WEAK%R(IG)*D
!         POINT DE GAUSS 8
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH3*FTILD_WEAK%R(IG)*(1.D0-C-D)
          CV1%R(I2)=CV1%R(I2)+WEIGH3*FTILD_WEAK%R(IG)*D
          CV1%R(I3)=CV1%R(I3)+WEIGH3*FTILD_WEAK%R(IG)*C
!         POINT DE GAUSS 9
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH3*FTILD_WEAK%R(IG)*D
          CV1%R(I2)=CV1%R(I2)+WEIGH3*FTILD_WEAK%R(IG)*(1.D0-C-D)
          CV1%R(I3)=CV1%R(I3)+WEIGH3*FTILD_WEAK%R(IG)*C
!         POINT DE GAUSS 10
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH3*FTILD_WEAK%R(IG)*C
          CV1%R(I2)=CV1%R(I2)+WEIGH3*FTILD_WEAK%R(IG)*(1.D0-C-D)
          CV1%R(I3)=CV1%R(I3)+WEIGH3*FTILD_WEAK%R(IG)*D
!         POINT DE GAUSS 11
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH3*FTILD_WEAK%R(IG)*D
          CV1%R(I2)=CV1%R(I2)+WEIGH3*FTILD_WEAK%R(IG)*C
          CV1%R(I3)=CV1%R(I3)+WEIGH3*FTILD_WEAK%R(IG)*(1.D0-C-D)
!         POINT DE GAUSS 12
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)+WEIGH3*FTILD_WEAK%R(IG)*C
          CV1%R(I2)=CV1%R(I2)+WEIGH3*FTILD_WEAK%R(IG)*D
          CV1%R(I3)=CV1%R(I3)+WEIGH3*FTILD_WEAK%R(IG)*(1.D0-C-D)
        ENDDO
!
      ELSEIF(NGAUSS.EQ.6.AND.IELM.EQ.41) THEN
!
!       ASSEMBLING (3 BASES PER ELEMENT)
!       THE WEIGHS ARE:
!       WEIGH1 FOR POINT 1,2,3
!       WEIGH2 FOR POINT 4,5,6
!
        A=1.D0/6.D0
        B=2.D0/3.D0
        C=0.5D0*(1.D0-1.D0/SQRT(3.D0))
        D=0.5D0*(1.D0+1.D0/SQRT(3.D0))
!
        IG=0
        DO IPLAN=1,NPLAN-1
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)+(IPLAN-1)*NPOIN
          I2=IKLE(IELEM,2)+(IPLAN-1)*NPOIN
          I3=IKLE(IELEM,3)+(IPLAN-1)*NPOIN
          I4=I1+NPOIN
          I5=I2+NPOIN
          I6=I3+NPOIN
          H1=Z(I4)-Z(I1)
          H2=Z(I5)-Z(I2)
          H3=Z(I6)-Z(I3)
          WEIGH1=SURFAC(IELEM)*(H1*(1.D0-A-A)+H2*A+H3*A)/6.D0
          WEIGH2=SURFAC(IELEM)*(H1*(1.D0-B-A)+H2*B+H3*A)/6.D0
          WEIGH3=SURFAC(IELEM)*(H1*(1.D0-A-B)+H2*A+H3*B)/6.D0
!         POINT DE GAUSS 1
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)
     &             +WEIGH1*FTILD_WEAK%R(IG)*(1.D0-C)*(1.D0-A-A)
          CV1%R(I2)=CV1%R(I2)
     &             +WEIGH1*FTILD_WEAK%R(IG)*(1.D0-C)* A
          CV1%R(I3)=CV1%R(I3)
     &             +WEIGH1*FTILD_WEAK%R(IG)*(1.D0-C)* A
          CV1%R(I4)=CV1%R(I4)
     &             +WEIGH1*FTILD_WEAK%R(IG)*      C *(1.D0-A-A)
          CV1%R(I5)=CV1%R(I5)
     &             +WEIGH1*FTILD_WEAK%R(IG)*      C * A
          CV1%R(I6)=CV1%R(I6)
     &             +WEIGH1*FTILD_WEAK%R(IG)*      C * A
!         POINT DE GAUSS 2
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)
     &             +WEIGH2*FTILD_WEAK%R(IG)*(1.D0-C)*(1.D0-B-A)
          CV1%R(I2)=CV1%R(I2)
     &             +WEIGH2*FTILD_WEAK%R(IG)*(1.D0-C)* B
          CV1%R(I3)=CV1%R(I3)
     &             +WEIGH2*FTILD_WEAK%R(IG)*(1.D0-C)* A
          CV1%R(I4)=CV1%R(I4)
     &             +WEIGH2*FTILD_WEAK%R(IG)*      C *(1.D0-B-A)
          CV1%R(I5)=CV1%R(I5)
     &             +WEIGH2*FTILD_WEAK%R(IG)*      C * B
          CV1%R(I6)=CV1%R(I6)
     &             +WEIGH2*FTILD_WEAK%R(IG)*      C * A
!         POINT DE GAUSS 3
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)
     &             +WEIGH3*FTILD_WEAK%R(IG)*(1.D0-C)*(1.D0-A-B)
          CV1%R(I2)=CV1%R(I2)
     &             +WEIGH3*FTILD_WEAK%R(IG)*(1.D0-C)* A
          CV1%R(I3)=CV1%R(I3)
     &             +WEIGH3*FTILD_WEAK%R(IG)*(1.D0-C)* B
          CV1%R(I4)=CV1%R(I4)
     &             +WEIGH3*FTILD_WEAK%R(IG)*      C *(1.D0-A-B)
          CV1%R(I5)=CV1%R(I5)
     &             +WEIGH3*FTILD_WEAK%R(IG)*      C * A
          CV1%R(I6)=CV1%R(I6)
     &             +WEIGH3*FTILD_WEAK%R(IG)*      C * B
!         POINT DE GAUSS 4
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)
     &             +WEIGH1*FTILD_WEAK%R(IG)*(1.D0-D)*(1.D0-A-A)
          CV1%R(I2)=CV1%R(I2)
     &             +WEIGH1*FTILD_WEAK%R(IG)*(1.D0-D)* A
          CV1%R(I3)=CV1%R(I3)
     &             +WEIGH1*FTILD_WEAK%R(IG)*(1.D0-D)* A
          CV1%R(I4)=CV1%R(I4)
     &             +WEIGH1*FTILD_WEAK%R(IG)*      D *(1.D0-A-A)
          CV1%R(I5)=CV1%R(I5)
     &             +WEIGH1*FTILD_WEAK%R(IG)*      D * A
          CV1%R(I6)=CV1%R(I6)
     &             +WEIGH1*FTILD_WEAK%R(IG)*      D * A
!         POINT DE GAUSS 5
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)
     &             +WEIGH2*FTILD_WEAK%R(IG)*(1.D0-D)*(1.D0-B-A)
          CV1%R(I2)=CV1%R(I2)
     &             +WEIGH2*FTILD_WEAK%R(IG)*(1.D0-D)* B
          CV1%R(I3)=CV1%R(I3)
     &             +WEIGH2*FTILD_WEAK%R(IG)*(1.D0-D)* A
          CV1%R(I4)=CV1%R(I4)
     &             +WEIGH2*FTILD_WEAK%R(IG)*      D *(1.D0-B-A)
          CV1%R(I5)=CV1%R(I5)
     &             +WEIGH2*FTILD_WEAK%R(IG)*      D * B
          CV1%R(I6)=CV1%R(I6)
     &             +WEIGH2*FTILD_WEAK%R(IG)*      D * A
!         POINT DE GAUSS 6
          IG=IG+1
          CV1%R(I1)=CV1%R(I1)
     &             +WEIGH3*FTILD_WEAK%R(IG)*(1.D0-D)*(1.D0-A-B)
          CV1%R(I2)=CV1%R(I2)
     &             +WEIGH3*FTILD_WEAK%R(IG)*(1.D0-D)* A
          CV1%R(I3)=CV1%R(I3)
     &             +WEIGH3*FTILD_WEAK%R(IG)*(1.D0-D)* B
          CV1%R(I4)=CV1%R(I4)
     &             +WEIGH3*FTILD_WEAK%R(IG)*      D *(1.D0-A-B)
          CV1%R(I5)=CV1%R(I5)
     &             +WEIGH3*FTILD_WEAK%R(IG)*      D * A
          CV1%R(I6)=CV1%R(I6)
     &             +WEIGH3*FTILD_WEAK%R(IG)*      D * B
        ENDDO
        ENDDO
!
      ELSE
!
        WRITE(LU,11) NGAUSS
11      FORMAT(1X,'CHAR_WEAK: OPTION NOT IMPLEMENTED:',I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(SOLV) THEN
!
!       MASS-MATRIX
!
        CALL MATRIX(AM1,'M=N     ','MATMAS          ',IELM,IELM,
     &              1.D0,FTILD,FTILD,FTILD,FTILD,FTILD,FTILD,
     &              MESH,.FALSE.,FTILD)
!
!       PARTIALLY LUMPING THE MASS-MATRIX
!
        CALL LUMP(T2,AM1,MESH,AGGLO)
        CALL OM('M=CM    ', M=AM1, C=1.D0-AGGLO, MESH=MESH)
        CALL OM('M=M+D   ', M=AM1, D=T2, MESH=MESH)
!
!       INITIAL GUESS (AS IF MATRIX LUMPED...)
!
        CALL OS('X=Y     ',X=FTILD,Y=CV1)
        IF(NCSIZE.GT.1) CALL PARCOM(FTILD,2,MESH)
        CALL OS('X=XY    ',X=FTILD,Y=UNSV)
!
!       SOLUTION OF THE SYSTEM (WITHOUT BOUNDARY CONDITIONS...)
!
#if defined COMPAD
        CALL AD_SOLVE(FTILD,AM1,CV1,TB,SLV,LISTIN,MESH,MESH%M)
#else
        CALL SOLVE(FTILD,AM1,CV1,TB,SLV,LISTIN,MESH,MESH%M)
#endif
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
