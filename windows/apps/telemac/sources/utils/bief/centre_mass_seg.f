!                       **************************
                        SUBROUTINE CENTRE_MASS_SEG
!                       **************************
!
     &(X,Y,COORD_G,IKLE,NPOIN,ELTSEG,ORISEG,NELEM,NSEG,JMI,CMI,GLOSEG,
     & IFABOR,COORD_S,MESH)
!
!***********************************************************************
! BIEF   V6P3                                                18/12/2012
!***********************************************************************
!
!brief    GIVES COORDINATES OF CENTRE OF GRAVITY OF TRIANGLES RIGHT AND
!           LEFT OF SEGMENTS I.E. FOR A SEGMENT ISEG:
!            - COORD_G(1,ISEG) AND COORD_G(2,ISEG) ARE THE COORDINATES OF
!              THE CENTRE OF GRAVITY OF THE FIRST NEIGHBORING TRIANGLE
!            - COORD_G(3,ISEG) AND COORD_G(4,ISEG) ARE THE COORDINATES OF
!              THE CENTRE OF GRAVITY OF THE SECOND NEIGHBORING TRIANGLE
!         GIVES COORDINATES OF THE MIDDLE POINT OF SEGMENT G1G2 (STOCKED
!             AT CMI(1,ISEG) AND CMI(2,ISEG)) AND THE ELEMENT NUMBER TO
!             TO WHICH BELONGS CMI
!
!history  R.ATA
!+        18/12/12
!+        V6P3
!+
!history  R. ATA & J-M HERVOUET
!+        19/11/2013
!+        V6P3
!+    Optimisation and simplification.
!
!history  R. ATA & J-M HERVOUET
!+        01/12/2013
!+        V6P3
!+    add verification tests
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CMI            |<--| COORDINATES OF MID-INTERFACE POINTS
!| COORD_G        |<--| CENTER OF MASS OF ELEMENTS NEIGHBORS OF AN EDGE
!|                |   |  COORD_G(1,ISEG) AND COORD_G(2,ISEG) ARE X1 AND Y1
!|                |   |  COORD_G(3,ISEG) AND COORD_G(4,ISEG) ARE X2 AND Y2
!| COORD_S        |<--| OPPOSITE NODE OF ELEMENTS NEIGHBORS OF AN EDGE
!|                |   |  COORD_S(1,ISEG) AND COORD_S(2,ISEG) ARE X1 AND Y1
!|                |   |  COORD_S(3,ISEG) AND COORD_S(4,ISEG) ARE X2 AND Y2
!| ELTSEG         |-->| SEGMENTS FORMING AN ELEMENT
!| GLOSEG         |-->|  GLOBAL NUMBERS OF VERTICES OF SEGMENTS
!| IKLE           |-->| CONNECTIVITY TABLE.
!| IFABOR         |-->| IFABOR(IEL,I) IS THE ELEMENT BEHIND THE EDGE I OF
!|                |---|  ELEMENT IEL, OTHERWISE
!|                |---|     IFABOR(IEL,I) = -2 : THIS IS INTERFACE EDGE
!|                |---|     IFABOR(IEL,I) = 0  : THIS IS BOUNDARY EDGE
!|                |---|     IFABOR(IEL,I) = -1 : THIS IS LIQUID BOUNDARY EDGE
!| JMI            |<--| NUMBER OF TRIANGLE TO WHICH BELONGS THE
!|                |   |       MID-INTERFACE POINT.
!| MESH           |-->| MESH STRUCTURE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| NUBO           |-->| NUMBER OF NODE COMPOSING A SEGMENT
!|                |---|
!| ORISEG         |-->| ORIENTATION OF SEGMENTS FORMING AN
!|                |   |        ELEMENT 1:ANTI 2:CLOCKWISE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      IMPLICIT  NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG,NELEM,NPOIN
      INTEGER, INTENT(IN)             :: IKLE(NELEM,3)
      INTEGER, INTENT(IN)             :: ELTSEG(NELEM,3)
      INTEGER, INTENT(IN)             :: ORISEG(NELEM,3)
      INTEGER, INTENT(INOUT)          :: JMI(NSEG)
      INTEGER, INTENT(IN)             :: GLOSEG(NSEG,2)
      DOUBLE PRECISION, INTENT(INOUT) :: CMI(2,NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: COORD_G(NSEG,4)
      DOUBLE PRECISION, INTENT(INOUT) :: COORD_S(NSEG,4)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      INTEGER, INTENT(IN)             :: IFABOR(NELEM,3)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISEG,NB1,NB2,IELEM,I,I1,I2,I3,J
      DOUBLE PRECISION XEL,YEL,XG1,XG2,YG1,YG2,XSOM(3),YSOM(3)
      DOUBLE PRECISION X_MIDPOINT,Y_MIDPOINT,DET
      DOUBLE PRECISION, ALLOCATABLE :: TMP_CMI1(:), TMP_CMI2(:)
!
!-----------------------------------------------------------------------
!   INITIALIZATION OF VARIABLES
!
      DO I=1,NSEG
        COORD_G(I,1) = 0.D0
        COORD_G(I,2) = 0.D0
        COORD_G(I,3) = 0.D0
        COORD_G(I,4) = 0.D0
        COORD_S(I,1) = 0.D0
        COORD_S(I,2) = 0.D0
        COORD_S(I,3) = 0.D0
        COORD_S(I,4) = 0.D0
        CMI(1,I)     = 0.D0
        CMI(2,I)     = 0.D0
        JMI(I)       = 0
      ENDDO
      IF(NCSIZE.GT.1)THEN
        DO I=1,NSEG
          MESH%MSEG%X%R(I)=0.D0
        ENDDO
      ENDIF
!    RECUPERATE THE COORDINATES OF CENTER OF GRAVITY OF THE TRIANGLES
!    AT THE RIGHT AND AT THE LEFT OF THE SEGMENT BETWEEN TWO NODES
!
      DO IELEM=1, NELEM
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        XEL = (X(I1)+X(I2)+X(I3))/3.0D0
        YEL = (Y(I1)+Y(I2)+Y(I3))/3.0D0
        DO I = 1,3
          ISEG = ELTSEG(IELEM,I)
          NB1=GLOSEG(ISEG,1)
          NB2=GLOSEG(ISEG,2)
          IF(I1.NE.NB1.AND.I1.NE.NB2) J=I1
          IF(I2.NE.NB1.AND.I2.NE.NB2) J=I2
          IF(I3.NE.NB1.AND.I3.NE.NB2) J=I3
          IF(ORISEG(IELEM,I).EQ.1)THEN ! G IS LEFT OF THE EDGE
            COORD_G(ISEG,1) =XEL
            COORD_G(ISEG,2) =YEL
            COORD_S(ISEG,1) =X(J)
            COORD_S(ISEG,2) =Y(J)
          ELSEIF(ORISEG(IELEM,I).EQ.2)THEN ! G IS RIGHT OF THE EDGE
            COORD_G(ISEG,3) =XEL
            COORD_G(ISEG,4) =YEL
            COORD_S(ISEG,3) =X(J)
            COORD_S(ISEG,4) =Y(J)
          ENDIF
        ENDDO
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        ALLOCATE(TMP_CMI1(NSEG))
        ALLOCATE(TMP_CMI2(NSEG))
        TMP_CMI1 = COORD_G(1:NSEG,1)
        TMP_CMI2 = COORD_G(1:NSEG,2)
        CALL PARCOM2_SEG(TMP_CMI1,
     &                   TMP_CMI2,
     &                   CMI,
     &                   NSEG,1,1,2,MESH,1,11)
        COORD_G(1:NSEG,1) = TMP_CMI1
        COORD_G(1:NSEG,2) = TMP_CMI2
!
        TMP_CMI1 = COORD_G(1:NSEG,3)
        TMP_CMI2 = COORD_G(1:NSEG,4)
        CALL PARCOM2_SEG(TMP_CMI1,
     &                   TMP_CMI2,
     &                   CMI,
     &                   NSEG,1,1,2,MESH,1,11)

        COORD_G(1:NSEG,3) = TMP_CMI1
        COORD_G(1:NSEG,4) = TMP_CMI2
!
        TMP_CMI1 = COORD_S(1:NSEG,1)
        TMP_CMI2 = COORD_S(1:NSEG,2)
        CALL PARCOM2_SEG(TMP_CMI1,
     &                   TMP_CMI2,
     &                   CMI,
     &                   NSEG,1,1,2,MESH,1,11)
        COORD_S(1:NSEG,1) = TMP_CMI1
        COORD_S(1:NSEG,2) = TMP_CMI2
!
        TMP_CMI1 = COORD_S(1:NSEG,3)
        TMP_CMI2 = COORD_S(1:NSEG,4)
        CALL PARCOM2_SEG(TMP_CMI1,
     &                   TMP_CMI2,
     &                   CMI,
     &                   NSEG,1,1,2,MESH,1,11)
        COORD_S(1:NSEG,3) = TMP_CMI1
        COORD_S(1:NSEG,4) = TMP_CMI2
        DEALLOCATE(TMP_CMI1)
        DEALLOCATE(TMP_CMI2)

      ENDIF
!
!     SECOND PART:
!     RETRIEVE CMI: CMI(1,ISEG) AND CMI(2,ISEG) ARE X AND Y OF THE
!                   MID-POINT G1G2
!     RETRIEVE JMI: JMI(ISEG) IS THE NUMBER IF TRIANGLE TO WHICH
!                   BELONGS CMI
!
      DO IELEM=1,NELEM
        DO I = 1,3
          ISEG = ELTSEG(IELEM,I)
          NB1 = GLOSEG(ISEG,1)
          NB2 = GLOSEG(ISEG,2)
          XG1 = COORD_G(ISEG,1)
          YG1 = COORD_G(ISEG,2)
          XG2 = COORD_G(ISEG,3)
          YG2 = COORD_G(ISEG,4)
          ! CENTER OF SEGMENT G1G2
          X_MIDPOINT = 0.5D0*(XG1+XG2)
          Y_MIDPOINT = 0.5D0*(YG1+YG2)
          CMI(1,ISEG) = X_MIDPOINT
          CMI(2,ISEG) = Y_MIDPOINT
          ! JMI: IN WHICH ELEMENT IS CMI
          ! WE USE CROSS PRODUCT
          XSOM(1)=X(NB1)
          XSOM(2)=X(NB2)
          YSOM(1)=Y(NB1)
          YSOM(2)=Y(NB2)

          DET = (XSOM(2)-XSOM(1))*(Y_MIDPOINT-YSOM(1))
     &         -(YSOM(2)-YSOM(1))*(X_MIDPOINT-XSOM(1))

          IF(ORISEG(IELEM,I).EQ.2) DET = -DET
!         BE CARREFUL FIXME:
!           - IN CASE OF ISEG IS AT THE INTERFACE OF THE TRIANGLE
!             EACH SUBDOMAIN WILL SEE IT
          IF(DET.GE.0.D0) THEN
            JMI(ISEG) = IELEM
          ENDIF
        ENDDO
      ENDDO
!
! BOUNDARY SEGMENTS
!
      DO IELEM=1,NELEM
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        DO I = 1,3
          ISEG = ELTSEG(IELEM,I)
          IF(IFABOR(IELEM,I).EQ.-1.OR. IFABOR(IELEM,I).EQ. 0) THEN
!           BOUNDARY SEGMENT
            NB1=GLOSEG(ISEG,1)
            NB2=GLOSEG(ISEG,2)
!           CMI
            CMI(1,ISEG)=0.5D0*(X(NB1)+X(NB2))
            CMI(2,ISEG)=0.5D0*(Y(NB1)+Y(NB2))
!           JMI
            JMI(ISEG)=IELEM
!           ISEG HAS BEEN TREATED
          ENDIF
        ENDDO
      ENDDO
!
! TO VERIFY THAT IT IS WELL DONE
!
      IF(NCSIZE.LE.1)THEN
        DO ISEG=1,NSEG
          IF(JMI(ISEG).EQ.0)THEN
            WRITE(LU,*)'CENTRE_MASS_SEG: PROBLEM JMI NOT GOOD'
            WRITE(LU,*)'FOR SEGMENT :',ISEG
            WRITE(LU,*)'PTS SEG',GLOSEG(ISEG,1),GLOSEG(ISEG,2)
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
      ENDIF
!
!---------------------------------------------------------------------
!
      RETURN
      END

