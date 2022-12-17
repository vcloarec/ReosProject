!                   *********************
                    SUBROUTINE CHAR_GAUSS
!                   *********************
!
     &(XCONV,YCONV,ZCONV,SHP,SHZ,ELT,ETA,X,Y,IKLE,NPOIN,NELEM,NELMAX,
     & NG,NGAUSS,IELM,NPLAN,ZSTAR)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    Prepares the data on advected Gauss points for the weak form
!+        of the method of characteristics.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        18/06/2013
!+        V6P3
!+    First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELT            |<--| STARTING ELEMENT OF GAUSS POINTS
!| IELM           |-->| TYPE OF ELEMENT
!| IKLE           |-->| CONNECTIVITY TABLE FOR ALL POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN THE MESH
!| NG             |-->| TOTAL NUMBER OF GAUSS POINTS IN THE MESH
!| NGAUSS         |-->| NUMBER OF GAUSS POINTS PER ELEMENT
!| NLAN           |-->| NUMBER OF PLANES IN THE MESH IN 3D
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| SHP            |<--| BARYCENTRIC COORDINATES OF GAUSS POINTS
!| XCONV          |<--| ABSCISSAE OF GAUSS POINTS
!| YCONV          |<--| ORDINATES OF GAUSS POINTS
!| ZCONV          |<--| TRANSFORMED ELEVATIONS OF GAUSS POINTS
!| X              |-->| ABSCISSAE OF MESH POINTS
!| Y              |-->| ORDINATES OF MESH POINTS
!| ZSTAR          |-->| TRANSFORMED ELEVATIONS OF MESH POINTS
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
!                                        HERE IKLE2
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3)
      INTEGER, INTENT(INOUT)          :: ELT(NG),ETA(NG)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN*NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: Y(NPOIN*NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: ZSTAR(NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: XCONV(NG),YCONV(NG),ZCONV(*)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NG)
      DOUBLE PRECISION, INTENT(INOUT) :: SHZ(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,IG,IPLAN
      DOUBLE PRECISION TIERS,A,B,C,D
      TIERS=1.D0/3.D0
!
!-----------------------------------------------------------------------
!
      IF(NG.NE.NELEM*NGAUSS.AND.IELM.EQ.11) THEN
        WRITE(LU,*) 'CHAR_GAUSS: BAD NUMBER OF POINTS'
        WRITE(LU,*) 'NG=',NG,' NELEM=',NELEM,' NGAUSS=',NGAUSS
        CALL PLANTE(1)
        STOP
      ELSEIF(NG.NE.NELEM*(NPLAN-1)*NGAUSS.AND.IELM.EQ.41) THEN
        WRITE(LU,*) 'CHAR_GAUSS: BAD NUMBER OF POINTS'
        WRITE(LU,*) 'NG=',NG,' NELEM=',NELEM,' NGAUSS=',NGAUSS
        WRITE(LU,*) 'NPLAN=',NPLAN
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(NGAUSS.EQ.1.AND.IELM.EQ.11) THEN
!
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          XCONV(IELEM)=(X(I1)+X(I2)+X(I3))*TIERS
          YCONV(IELEM)=(Y(I1)+Y(I2)+Y(I3))*TIERS
          SHP(1,IELEM)=TIERS
          SHP(2,IELEM)=TIERS
          SHP(3,IELEM)=TIERS
          ELT(IELEM)=IELEM
        ENDDO
!
      ELSEIF(NGAUSS.EQ.3.AND.IELM.EQ.11) THEN
!
        IG=0
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          IG=IG+1
          SHP(1,IG)=1.D0-1.D0/6.D0-1.D0/6.D0
          SHP(2,IG)=1.D0/6.D0
          SHP(3,IG)=1.D0/6.D0
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=1.D0-2.D0/3.D0-1.D0/6.D0
          SHP(2,IG)=2.D0/3.D0
          SHP(3,IG)=1.D0/6.D0
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=1.D0-1.D0/6.D0-2.D0/3.D0
          SHP(2,IG)=1.D0/6.D0
          SHP(3,IG)=2.D0/3.D0
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
        ENDDO
!
      ELSEIF(NGAUSS.EQ.4.AND.IELM.EQ.11) THEN
!
        IG=0
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          IG=IG+1
          SHP(1,IG)=1.D0-1.D0/3.D0-1.D0/3.D0
          SHP(2,IG)=1.D0/3.D0
          SHP(3,IG)=1.D0/3.D0
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=1.D0-1.D0/5.D0-1.D0/5.D0
          SHP(2,IG)=1.D0/5.D0
          SHP(3,IG)=1.D0/5.D0
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=1.D0-3.D0/5.D0-1.D0/5.D0
          SHP(2,IG)=3.D0/5.D0
          SHP(3,IG)=1.D0/5.D0
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=1.D0-1.D0/5.D0-3.D0/5.D0
          SHP(2,IG)=1.D0/5.D0
          SHP(3,IG)=3.D0/5.D0
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
        ENDDO
!
      ELSEIF(NGAUSS.EQ.6.AND.IELM.EQ.11) THEN
!
        A=0.445948490915965D0
        B=0.091576213509771D0
!
        IG=0
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          IG=IG+1
          SHP(1,IG)=1.D0-A-A
          SHP(2,IG)=A
          SHP(3,IG)=A
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=A
          SHP(2,IG)=1.D0-A-A
          SHP(3,IG)=A
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=A
          SHP(2,IG)=A
          SHP(3,IG)=1.D0-A-A
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=1.D0-B-B
          SHP(2,IG)=B
          SHP(3,IG)=B
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=B
          SHP(2,IG)=1.D0-B-B
          SHP(3,IG)=B
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=B
          SHP(2,IG)=B
          SHP(3,IG)=1.D0-B-B
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
        ENDDO
!
      ELSEIF(NGAUSS.EQ.7.AND.IELM.EQ.11) THEN
!
        A=(6.D0+SQRT(15.D0))/21.D0
        B=4.D0/7.D0-A
!
        IG=0
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          IG=IG+1
          SHP(1,IG)=TIERS
          SHP(2,IG)=TIERS
          SHP(3,IG)=TIERS
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=1.D0-A-A
          SHP(2,IG)=A
          SHP(3,IG)=A
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=A
          SHP(2,IG)=1.D0-A-A
          SHP(3,IG)=A
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=A
          SHP(2,IG)=A
          SHP(3,IG)=1.D0-A-A
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=1.D0-B-B
          SHP(2,IG)=B
          SHP(3,IG)=B
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=B
          SHP(2,IG)=1.D0-B-B
          SHP(3,IG)=B
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=B
          SHP(2,IG)=B
          SHP(3,IG)=1.D0-B-B
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
        ENDDO
!
      ELSEIF(NGAUSS.EQ.12.AND.IELM.EQ.11) THEN
!
        A=0.063089014491502D0
        B=0.249286745170910D0
        C=0.310352451033785D0
        D=0.053145049844816D0
!
        IG=0
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          IG=IG+1
          SHP(1,IG)=1.D0-A-A
          SHP(2,IG)=A
          SHP(3,IG)=A
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=A
          SHP(2,IG)=1.D0-A-A
          SHP(3,IG)=A
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=A
          SHP(2,IG)=A
          SHP(3,IG)=1.D0-A-A
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=1.D0-B-B
          SHP(2,IG)=B
          SHP(3,IG)=B
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=B
          SHP(2,IG)=1.D0-B-B
          SHP(3,IG)=B
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=B
          SHP(2,IG)=B
          SHP(3,IG)=1.D0-B-B
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=1.D0-C-D
          SHP(2,IG)=C
          SHP(3,IG)=D
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=1.D0-C-D
          SHP(2,IG)=D
          SHP(3,IG)=C
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=D
          SHP(2,IG)=1.D0-C-D
          SHP(3,IG)=C
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=C
          SHP(2,IG)=1.D0-C-D
          SHP(3,IG)=D
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=D
          SHP(2,IG)=C
          SHP(3,IG)=1.D0-C-D
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
          IG=IG+1
          SHP(1,IG)=C
          SHP(2,IG)=D
          SHP(3,IG)=1.D0-C-D
          XCONV(IG)=SHP(1,IG)*X(I1)+SHP(2,IG)*X(I2)+SHP(3,IG)*X(I3)
          YCONV(IG)=SHP(1,IG)*Y(I1)+SHP(2,IG)*Y(I2)+SHP(3,IG)*Y(I3)
          ELT(IG)=IELEM
        ENDDO
!
      ELSEIF(NGAUSS.EQ.6.AND.IELM.EQ.41) THEN
!
        IF(NG.NE.6*NELEM*(NPLAN-1)) THEN
          WRITE(LU,*) 'CHAR_GAUSS: BAD NUMBER OF POINTS'
          WRITE(LU,*) 'NG=',NG
          WRITE(LU,*) '6*NELEM*(NPLAN-1)=',6*NELEM*NPLAN
          CALL PLANTE(1)
          STOP
        ENDIF
!
        IG=0
!
        A=1.D0/6.D0
        B=2.D0/3.D0
        C=0.5D0*(1.D0-1.D0/SQRT(3.D0))
        D=0.5D0*(1.D0+1.D0/SQRT(3.D0))
!
        DO IPLAN=1,NPLAN-1
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
!
          IG=IG+1
          SHP(1,IG)=(1.D0-A-A)
          SHP(2,IG)=A
          SHP(3,IG)=A
          SHZ(IG)=C
          XCONV(IG)=(1.D0-SHZ(IG))*( SHP(1,IG)*X(I1)
     &                              +SHP(2,IG)*X(I2)
     &                              +SHP(3,IG)*X(I3) )
     &                   +SHZ(IG) *( SHP(1,IG)*X(I1)
     &                              +SHP(2,IG)*X(I2)
     &                              +SHP(3,IG)*X(I3) )
          YCONV(IG)=(1.D0-SHZ(IG))*( SHP(1,IG)*Y(I1)
     &                              +SHP(2,IG)*Y(I2)
     &                              +SHP(3,IG)*Y(I3) )
     &                   +SHZ(IG) *( SHP(1,IG)*Y(I1)
     &                              +SHP(2,IG)*Y(I2)
     &                              +SHP(3,IG)*Y(I3) )
          ZCONV(IG)=(1.D0-SHZ(IG))*ZSTAR(IPLAN)
     &                   +SHZ(IG) *ZSTAR(IPLAN+1)
          ELT(IG)=IELEM
          ETA(IG)=IPLAN
!
          IG=IG+1
          SHP(1,IG)=(1.D0-B-A)
          SHP(2,IG)=B
          SHP(3,IG)=A
          SHZ(IG)=C
          XCONV(IG)=(1.D0-SHZ(IG))*( SHP(1,IG)*X(I1)
     &                              +SHP(2,IG)*X(I2)
     &                              +SHP(3,IG)*X(I3) )
     &                   +SHZ(IG) *( SHP(1,IG)*X(I1)
     &                              +SHP(2,IG)*X(I2)
     &                              +SHP(3,IG)*X(I3) )
          YCONV(IG)=(1.D0-SHZ(IG))*( SHP(1,IG)*Y(I1)
     &                              +SHP(2,IG)*Y(I2)
     &                              +SHP(3,IG)*Y(I3) )
     &                   +SHZ(IG) *( SHP(1,IG)*Y(I1)
     &                              +SHP(2,IG)*Y(I2)
     &                              +SHP(3,IG)*Y(I3) )
          ZCONV(IG)=(1.D0-SHZ(IG))*ZSTAR(IPLAN)
     &                   +SHZ(IG) *ZSTAR(IPLAN+1)
          ELT(IG)=IELEM
          ETA(IG)=IPLAN
!
          IG=IG+1
          SHP(1,IG)=(1.D0-A-B)
          SHP(2,IG)=A
          SHP(3,IG)=B
          SHZ(IG)=C
          XCONV(IG)=(1.D0-SHZ(IG))*( SHP(1,IG)*X(I1)
     &                              +SHP(2,IG)*X(I2)
     &                              +SHP(3,IG)*X(I3) )
     &                   +SHZ(IG) *( SHP(1,IG)*X(I1)
     &                              +SHP(2,IG)*X(I2)
     &                              +SHP(3,IG)*X(I3) )
          YCONV(IG)=(1.D0-SHZ(IG))*( SHP(1,IG)*Y(I1)
     &                              +SHP(2,IG)*Y(I2)
     &                              +SHP(3,IG)*Y(I3) )
     &                   +SHZ(IG) *( SHP(1,IG)*Y(I1)
     &                              +SHP(2,IG)*Y(I2)
     &                              +SHP(3,IG)*Y(I3) )
          ZCONV(IG)=(1.D0-SHZ(IG))*ZSTAR(IPLAN)
     &                   +SHZ(IG) *ZSTAR(IPLAN+1)
          ELT(IG)=IELEM
          ETA(IG)=IPLAN
!
          IG=IG+1
          SHP(1,IG)=(1.D0-A-A)
          SHP(2,IG)=A
          SHP(3,IG)=A
          SHZ(IG)=D
          XCONV(IG)=(1.D0-SHZ(IG))*( SHP(1,IG)*X(I1)
     &                              +SHP(2,IG)*X(I2)
     &                              +SHP(3,IG)*X(I3) )
     &                   +SHZ(IG) *( SHP(1,IG)*X(I1)
     &                              +SHP(2,IG)*X(I2)
     &                              +SHP(3,IG)*X(I3) )
          YCONV(IG)=(1.D0-SHZ(IG))*( SHP(1,IG)*Y(I1)
     &                              +SHP(2,IG)*Y(I2)
     &                              +SHP(3,IG)*Y(I3) )
     &                   +SHZ(IG) *( SHP(1,IG)*Y(I1)
     &                              +SHP(2,IG)*Y(I2)
     &                              +SHP(3,IG)*Y(I3) )
          ZCONV(IG)=(1.D0-SHZ(IG))*ZSTAR(IPLAN)
     &                   +SHZ(IG) *ZSTAR(IPLAN+1)
          ELT(IG)=IELEM
          ETA(IG)=IPLAN
!
          IG=IG+1
          SHP(1,IG)=(1.D0-B-A)
          SHP(2,IG)=B
          SHP(3,IG)=A
          SHZ(IG)=D
          XCONV(IG)=(1.D0-SHZ(IG))*( SHP(1,IG)*X(I1)
     &                              +SHP(2,IG)*X(I2)
     &                              +SHP(3,IG)*X(I3) )
     &                   +SHZ(IG) *( SHP(1,IG)*X(I1)
     &                              +SHP(2,IG)*X(I2)
     &                              +SHP(3,IG)*X(I3) )
          YCONV(IG)=(1.D0-SHZ(IG))*( SHP(1,IG)*Y(I1)
     &                              +SHP(2,IG)*Y(I2)
     &                              +SHP(3,IG)*Y(I3) )
     &                   +SHZ(IG) *( SHP(1,IG)*Y(I1)
     &                              +SHP(2,IG)*Y(I2)
     &                              +SHP(3,IG)*Y(I3) )
          ZCONV(IG)=(1.D0-SHZ(IG))*ZSTAR(IPLAN)
     &                   +SHZ(IG) *ZSTAR(IPLAN+1)
          ELT(IG)=IELEM
          ETA(IG)=IPLAN
!
          IG=IG+1
          SHP(1,IG)=(1.D0-A-B)
          SHP(2,IG)=A
          SHP(3,IG)=B
          SHZ(IG)=D
          XCONV(IG)=(1.D0-SHZ(IG))*( SHP(1,IG)*X(I1)
     &                              +SHP(2,IG)*X(I2)
     &                              +SHP(3,IG)*X(I3) )
     &                   +SHZ(IG) *( SHP(1,IG)*X(I1)
     &                              +SHP(2,IG)*X(I2)
     &                              +SHP(3,IG)*X(I3) )
          YCONV(IG)=(1.D0-SHZ(IG))*( SHP(1,IG)*Y(I1)
     &                              +SHP(2,IG)*Y(I2)
     &                              +SHP(3,IG)*Y(I3) )
     &                   +SHZ(IG) *( SHP(1,IG)*Y(I1)
     &                              +SHP(2,IG)*Y(I2)
     &                              +SHP(3,IG)*Y(I3) )
          ZCONV(IG)=(1.D0-SHZ(IG))*ZSTAR(IPLAN)
     &                   +SHZ(IG) *ZSTAR(IPLAN+1)
          ELT(IG)=IELEM
          ETA(IG)=IPLAN
!
        ENDDO
        ENDDO
!
      ELSE
!
        WRITE(LU,11) NGAUSS,IELM
11      FORMAT(1X,'CHAR_GAUSS: OPTION NOT IMPLEMENTED:',I6,
     &            ' WITH IELM=',I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
