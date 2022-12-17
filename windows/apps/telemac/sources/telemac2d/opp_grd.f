!                   ******************
                    SUBROUTINE OPP_GRD
!                   ******************
!
     &(VAR,DJX1,DJY1,DJX2,DJY2,IKLE,ELTSEG,ORISEG,NUBO,DPX,DPY)
!
!***********************************************************************
! TELEMAC2D   V8P3
!***********************************************************************
!
!brief    COMPUTE GRADIENT VAR ON TRIANGLES ON EACH SIDE OF 
!         A SEGMENT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      VAR        VARIABLE TO FIND
!>@param  [in,out]  DJX1       GRADIENT X ON THE LEFT TRIANGLE
!>@param  [in,out]  DJY1       GRADIENT Y ON THE LEFT TRIANGLE
!>@param  [in,out]  DJX2       GRADIENT X ON THE RIGHT TRIANGLE
!>@param  [in,out]  DJY2       GRADIENT Y ON THE RIGHT TRIANGLE
!>@param  [in]      IKLE       NUMBERING OF NODES IN THE TRIANGLE
!>@param  [in]      ELTSEG     SEGMENTS COMPOSING AN ELEMENT
!>@param  [in]      ORISEG     ORIENTATION OF SEGMENTS FORMING AN
!                              ELEMENT 1:ANTI 2:CLOCKWISE
!>@param  [in]      NUBO       GLOBAL INDICES OF EDGE EXTREMITIES
!>@param  [in]      DPX        GRADIENTS AT NODES
!>@param  [in]      DPY        GRADIENTS AT NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY:NPOIN,NELEM,MESH,NSEG
      USE INTERFACE_TELEMAC2D, EX_OPP_GRD => OPP_GRD
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: IKLE(NELEM,3)
      INTEGER, INTENT(IN)             :: ELTSEG(NELEM,3)
      INTEGER, INTENT(IN)             :: ORISEG(NELEM,3)
      INTEGER, INTENT(IN)             :: NUBO(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: VAR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: DPX(3,NELEM),DPY(3,NELEM)
      DOUBLE PRECISION, INTENT(INOUT) :: DJX1(NSEG),DJX2(NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: DJY1(NSEG),DJY2(NSEG)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ISEG,IELEM,I1,I2,I3,J,NB1,NB2
      DOUBLE PRECISION DJX, DJY
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DO I=1,NSEG
        DJX1(I) = 0.D0
        DJY1(I) = 0.D0
        DJX2(I) = 0.D0
        DJY2(I) = 0.D0
      ENDDO
!
      DO IELEM=1, NELEM
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
!
!       COMPUTE GRADIENTS
        DJX = VAR(I1)*DPX(1,IELEM)+VAR(I2)*DPX(2,IELEM)+
     &        VAR(I3)*DPX(3,IELEM)
        DJY = VAR(I1)*DPY(1,IELEM)+VAR(I2)*DPY(2,IELEM)+
     &        VAR(I3)*DPY(3,IELEM)
!
        DO I = 1,3
          ISEG = ELTSEG(IELEM,I)
          NB1=NUBO(1,ISEG)
          NB2=NUBO(2,ISEG)
          IF(I1.NE.NB1.AND.I1.NE.NB2) J=I1
          IF(I2.NE.NB1.AND.I2.NE.NB2) J=I2
          IF(I3.NE.NB1.AND.I3.NE.NB2) J=I3
!
!         SEGMENT LEFT/RIGHT GRADIENTS AFFECTATION
          IF(ORISEG(IELEM,I).EQ.1)THEN
            DJX1(ISEG) = DJX
            DJY1(ISEG) = DJY
          ELSEIF(ORISEG(IELEM,I).EQ.2)THEN
            DJX2(ISEG) = DJX
            DJY2(ISEG) = DJY
          ENDIF
        ENDDO
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM2_SEG(DJX1,DJX2,DJX2,NSEG,1,1,2,MESH,1,11)
        CALL PARCOM2_SEG(DJY1,DJY2,DJY2,NSEG,1,1,2,MESH,1,11)
      ENDIF
!
!---------------------------------------------------------------------
!
      RETURN
      END
