!                   ******************
                    SUBROUTINE OPP_VAR
!                   ******************
!
     &(VAR,VAR1,VAR2,IKLE,ELTSEG,ORISEG,NUBO)
!
!***********************************************************************
! TELEMAC2D   V8P3
!***********************************************************************
!
!brief    FINDS VALUES OF THE OPPOSITE NODE OF A SEGMENT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>@param  [in]      VAR        VARIABLE TO FIND
!>@param  [in,out]  VAR1       VARIABLE AT THE LEFT POINT OF THE SEGMENT
!>@param  [in,out]  VAR2       VARIABLE AT THE RIGHT POINT OF THE SEGMENT
!>@param  [in]      IKLE       NUMBERING OF NODES IN THE TRIANGLE
!>@param  [in]      ELTSEG     SEGMENTS COMPOSING AN ELEMENT
!>@param  [in]      ORISEG     ORIENTATION OF SEGMENTS FORMING AN
!                              ELEMENT 1:ANTI 2:CLOCKWISE
!>@param  [in]      NUBO       GLOBAL INDICES OF EDGE EXTREMITIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY:NPOIN,NELEM,MESH,NSEG
      USE INTERFACE_TELEMAC2D, EX_OPP_VAR => OPP_VAR
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: IKLE(NELEM,3)
      INTEGER, INTENT(IN)             :: ELTSEG(NELEM,3)
      INTEGER, INTENT(IN)             :: ORISEG(NELEM,3)
      INTEGER, INTENT(IN)             :: NUBO(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: VAR(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: VAR1(NSEG),VAR2(NSEG)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ISEG,IELEM,I1,I2,I3,J,NB1,NB2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DO I=1,NSEG
        VAR1(I) = 0.D0
        VAR2(I) = 0.D0
      ENDDO
!
      DO IELEM=1, NELEM
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        DO I = 1,3
          ISEG = ELTSEG(IELEM,I)
          NB1=NUBO(1,ISEG)
          NB2=NUBO(2,ISEG)
          IF(I1.NE.NB1.AND.I1.NE.NB2) J=I1
          IF(I2.NE.NB1.AND.I2.NE.NB2) J=I2
          IF(I3.NE.NB1.AND.I3.NE.NB2) J=I3
          IF(ORISEG(IELEM,I).EQ.1)THEN
            VAR1(ISEG) =VAR(J)
          ELSEIF(ORISEG(IELEM,I).EQ.2)THEN
            VAR2(ISEG) =VAR(J)
          ENDIF
        ENDDO
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM2_SEG(VAR1,VAR2,VAR2,NSEG,1,1,2,MESH,1,11)
      ENDIF
!
!---------------------------------------------------------------------
!
      RETURN
      END
