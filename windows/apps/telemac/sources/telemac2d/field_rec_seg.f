!                   ************************
                    SUBROUTINE FIELD_REC_SEG
!                   ************************
!
     &(X,Y,COORD_G,NPOIN,ELTSEG,NELEM,NSEG,NUBO,
     & IFABOR,COORD_S,COORD_IJP,ALPHA_IJP)
!
!***********************************************************************
! TELEMAC2D   V8P3                                            16/06/2021
!***********************************************************************
!
!brief    COMPUTE COORDINATES OF POINTS USED IN FIELD RECONSTRUCTIONS 
!         IN THE CASE OF FINITE VOLUME DIFFUSION SOLVERS
!
!           COORD_IJP(NSG,1) => XIP
!           COORD_IJP(NSG,2) => YIP
!           COORD_IJP(NSG,3) => XJP
!           COORD_IJP(NSG,4) => YJP
!           ALPHA_IJP(NSG,1) => ALPHAI
!           ALPHA_IJP(NSG,2) => SIDE_ALPHAI
!           ALPHA_IJP(NSG,3) => ALPHAJ
!           ALPHA_IJP(NSG,4) => SIDE_ALPHAJ
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      X          ABSCISSAE OF POINTS IN THE MESH
!>@param  [in]      Y          ORDINATES OF POINTS IN THE MESH
!>@param  [in]      NPOIN      NUMBER OF POINTS
!>@param  [in]      ELTSEG     SEGMENTS FORMING AN ELEMENT
!>@param  [in]      NELEM      NUMBER OF ELEMENTS
!>@param  [in]      NSEG       NUMBER OF SEGMENTS
!>@param  [in]      NUBO       GLOBAL INDICES OF EDGE EXTREMITIES
!>@param  [in]      IFABOR     ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!>@param  [in]      COORD_G    CENTER OF MASS OF ELEMENTS 
!+                             NEIGHBORS OF AN EDGE
!>@param  [in]      COORD_S    OPPOSITE NODE OF ELEMENTS
!+                             NEIGHBORS OF AN EDGE
!>@param  [in,out]  COORD_IJP  COORDINATES OF FIELD RECONSTRUCTION
!>@param  [in,out]  ALPHA_IJP  LINEAR RECONSTRUCTION WEIGHTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE DECLARATIONS_TELEMAC2D, ONLY: MESH,MVISUV,MVIST,NTRAC
      IMPLICIT  NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG,NELEM,NPOIN
      INTEGER, INTENT(IN)             :: ELTSEG(NELEM,3)
      INTEGER, INTENT(IN)             :: NUBO(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: COORD_G(NSEG,4)
      DOUBLE PRECISION, INTENT(IN)    :: COORD_S(NSEG,4)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      INTEGER, INTENT(IN)             :: IFABOR(NELEM,3)
      DOUBLE PRECISION, INTENT(INOUT) :: COORD_IJP(NSEG,4)
      DOUBLE PRECISION, INTENT(INOUT) :: ALPHA_IJP(NSEG,4)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL :: YESNO(NSEG)
      INTEGER :: I,IELEM,NUBO1,NUBO2,NSG,ISC,MVISTRAC,ITRAC
      DOUBLE PRECISION :: XA,YA,XB,YB,XM,YM,XE,YE,XF,YF,AD,BD,CD,XM0,YM0
      DOUBLE PRECISION :: XTEMP1,YTEMP1,XTEMP2,YTEMP2,DIST1,DIST2
      DOUBLE PRECISION, ALLOCATABLE :: TMP_COM1(:), TMP_COM2(:)
      DOUBLE PRECISION, PARAMETER :: DM = 0.1D0
!
!-----------------------------------------------------------------------
!
!     INITIALIZATION OF VARIABLES
      DO NSG=1,NSEG
        ALPHA_IJP(NSG,1) = 0.D0
        ALPHA_IJP(NSG,2) = 0.D0
        ALPHA_IJP(NSG,3) = 0.D0
        ALPHA_IJP(NSG,4) = 0.D0
        YESNO(NSG)=.FALSE.
      ENDDO
!
!     ONLY COMPUTE COORD IN CASE OF RTPF SCHEMES
      MVISTRAC = 1
      DO ITRAC=1,NTRAC
        IF (MVIST(ITRAC).EQ.3) THEN
          MVISTRAC = 3
        ENDIF
      ENDDO
!
      IF (MVISUV.EQ.3.OR.MVISTRAC.EQ.3) THEN
!
!       COMPUTE COORDINATES COORD_IJP
        DO IELEM=1,NELEM
          DO I = 1,3
            IF(.NOT.YESNO(ELTSEG(IELEM,I)))THEN
                NSG = ELTSEG(IELEM,I)
                NUBO1 = NUBO(1,NSG)
                NUBO2 = NUBO(2,NSG)
!
              IF(IFABOR(IELEM,I).EQ.-1.OR.IFABOR(IELEM,I).EQ. 0) THEN
                COORD_IJP(NSG,1) = X(NUBO1)
                COORD_IJP(NSG,2) = Y(NUBO1)
                COORD_IJP(NSG,3) = X(NUBO2)
                COORD_IJP(NSG,4) = Y(NUBO2)
!
              ELSE
!               COORDINATES OF FV CELL INTERFACE POINTS (A,B)
                XA = COORD_G(NSG,1)
                YA = COORD_G(NSG,2)
                XB = COORD_G(NSG,3)
                YB = COORD_G(NSG,4)
!
!               COORDINATES OF FLUX IMPOSITION M (BETWEEN A AND B)
!               INTERSECTION OF IJ AND AB
                CALL INTERSECT_SEG(X(NUBO1),Y(NUBO1),
     &                             X(NUBO2),Y(NUBO2),
     &                             XA,YA,XB,YB,XM,YM,ISC)
!
!               COORDINATES OF TRIANGLES OPPOSITE POINTS
!               (POINTS E AND F OF IJE AND IJF TRIANGLES)
                XF = COORD_S(NSG,1)
                YF = COORD_S(NSG,2)
                XE = COORD_S(NSG,3)
                YE = COORD_S(NSG,4)
!
!               COMPUTE COEF OF LINE (D)
                AD = XB-XA
                BD = YB-YA
                CD = -AD*XM-BD*YM
                IF(BD.EQ.0.D0) THEN
                  XM0 = XM
                  YM0 = YM + DM
                ELSE IF(AD.EQ.0.D0) THEN
                  XM0 = XM + DM
                  YM0 = YM
                ELSE
                  XM0 = XM + DM
                  YM0 = (-AD*XM0-CD)/BD
                ENDIF
!
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!               COMPUTE INTERSECTION OF LINE (DM)
!               WITH JE OR JF -> JP
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                CALL INTERSECT_SEG(X(NUBO2),Y(NUBO2),XE,YE,XM,YM,
     &                             XM0,YM0,XTEMP1,YTEMP1,ISC)
                CALL INTERSECT_SEG(X(NUBO2),Y(NUBO2),XF,YF,XM,YM,
     &                             XM0,YM0,XTEMP2,YTEMP2,ISC)
!
                DIST1 = SQRT((XTEMP1-XM)**2 + (YTEMP1-YM)**2)
                DIST2 = SQRT((XTEMP2-XM)**2 + (YTEMP2-YM)**2)
!
                IF (DIST1.LE.DIST2) THEN
                  COORD_IJP(NSG,3) = XTEMP1
                  COORD_IJP(NSG,4) = YTEMP1
!
                  ALPHA_IJP(NSG,4) = 1.D0
                  ALPHA_IJP(NSG,3) = SQRT((XTEMP1-X(NUBO2))**2
     &                                  + (YTEMP1-Y(NUBO2))**2)
     &                             / SQRT((XE-X(NUBO2))**2
     &                                  + (YE-Y(NUBO2))**2)
                ELSE
                  COORD_IJP(NSG,3) = XTEMP2
                  COORD_IJP(NSG,4) = YTEMP2
!
                  ALPHA_IJP(NSG,4) = 0.D0
                  ALPHA_IJP(NSG,3) = SQRT((XTEMP2-X(NUBO2))**2 
     &                                  + (YTEMP2-Y(NUBO2))**2)
     &                             / SQRT((XF-X(NUBO2))**2
     &                                  + (YF-Y(NUBO2))**2)
                ENDIF
!
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!               COMPUTE INTERSECTION OF LINE (DM)
!               WITH IE OR IF -> IP
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                CALL INTERSECT_SEG(X(NUBO1),Y(NUBO1),XE,YE,XM,YM,
     &                             XM0,YM0,XTEMP1,YTEMP1,ISC)
                CALL INTERSECT_SEG(X(NUBO1),Y(NUBO1),XF,YF,XM,YM,
     &                             XM0,YM0,XTEMP2,YTEMP2,ISC)
!
                DIST1 = SQRT((XTEMP1-XM)**2 + (YTEMP1-YM)**2)
                DIST2 = SQRT((XTEMP2-XM)**2 + (YTEMP2-YM)**2)
!
                IF (DIST1.LE.DIST2) THEN
                  COORD_IJP(NSG,1) = XTEMP1
                  COORD_IJP(NSG,2) = YTEMP1
!
                  ALPHA_IJP(NSG,2) = 1.D0
                  ALPHA_IJP(NSG,1) = SQRT((XTEMP1-X(NUBO1))**2 
     &                                  + (YTEMP1-Y(NUBO1))**2)
     &                             / SQRT((XE-X(NUBO1))**2
     &                                  + (YE-Y(NUBO1))**2)
                ELSE
                  COORD_IJP(NSG,1) = XTEMP2
                  COORD_IJP(NSG,2) = YTEMP2
!
                  ALPHA_IJP(NSG,2) = 0.D0
                  ALPHA_IJP(NSG,1) = SQRT((XTEMP2-X(NUBO1))**2
     &                                  + (YTEMP2-Y(NUBO1))**2)
     &                             / SQRT((XF-X(NUBO1))**2
     &                                  + (YF-Y(NUBO1))**2)
                ENDIF
              ENDIF
            ENDIF
!
          ENDDO
        ENDDO
!
!       FOR PARALLELISM
        IF(NCSIZE.GT.1) THEN
          ALLOCATE(TMP_COM1(NSEG))
          ALLOCATE(TMP_COM2(NSEG))
!
          TMP_COM1 = COORD_IJP(1:NSEG,1)
          TMP_COM2 = COORD_IJP(1:NSEG,2)
          CALL PARCOM2_SEG(TMP_COM1,
     &                     TMP_COM2,
     &                     TMP_COM2,
     &                     NSEG,1,1,2,MESH,1,11)
          COORD_IJP(1:NSEG,1) = TMP_COM1
          COORD_IJP(1:NSEG,2) = TMP_COM2
!
          TMP_COM1 = COORD_IJP(1:NSEG,3)
          TMP_COM2 = COORD_IJP(1:NSEG,4)
          CALL PARCOM2_SEG(TMP_COM1,
     &                     TMP_COM2,
     &                     TMP_COM2,
     &                     NSEG,1,1,2,MESH,1,11)
          COORD_IJP(1:NSEG,3) = TMP_COM1
          COORD_IJP(1:NSEG,4) = TMP_COM2
!
          TMP_COM1 = ALPHA_IJP(1:NSEG,1)
          TMP_COM2 = ALPHA_IJP(1:NSEG,2)
          CALL PARCOM2_SEG(TMP_COM1,
     &                     TMP_COM2,
     &                     TMP_COM2,
     &                     NSEG,1,1,2,MESH,1,11)
          ALPHA_IJP(1:NSEG,1) = TMP_COM1
          ALPHA_IJP(1:NSEG,2) = TMP_COM2
!
          TMP_COM1 = ALPHA_IJP(1:NSEG,3)
          TMP_COM2 = ALPHA_IJP(1:NSEG,4)
          CALL PARCOM2_SEG(TMP_COM1,
     &                     TMP_COM2,
     &                     TMP_COM2,
     &                     NSEG,1,1,2,MESH,1,11)
          ALPHA_IJP(1:NSEG,3) = TMP_COM1
          ALPHA_IJP(1:NSEG,4) = TMP_COM2
!
          DEALLOCATE(TMP_COM1)
          DEALLOCATE(TMP_COM2)
        ENDIF
!
      ENDIF ! RTPF SCHEME CHECK
!
!---------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE FIELD_REC_SEG
