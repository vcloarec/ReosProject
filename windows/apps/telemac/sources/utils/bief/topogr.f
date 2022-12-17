!                   *****************
                    SUBROUTINE TOPOGR
!                   *****************
!
     &(ZF,ZREF,ZFE,IKLE,IFABOR,NBOR,NELBOR,NULONE,
     & ITRA05,ITRA02,ITRA03,NELEM,NPTFR,NPOIN,MXPTVS)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FINELY ANALYSES THE TOPOGRAPHY AND BUILDS ZFE.
!+
!+            THE ARRAY OF BOTTOM ELEVATIONS BY ELEMENTS: ZFE
!+                WILL ENSURE IN THE FUTURE THAT THERE WILL NOT BE
!+                LIQUID DOMAINS CONNECTED BY A SINGLE NODE.
!
!history  J-M JANIN (LNH)
!+        17/08/94
!+        V5P1
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
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID
!|                |   | BOUNDARY
!| IKLE           |-->| CONNECTIVITY TABLE.
!| ITRA02         |<--| INTEGER WORK ARRAY
!| ITRA03         |<--| INTEGER WORK ARRAY
!| ITRA05         |<--| INTEGER WORK ARRAY
!| MXPTVS         |-->| MAXIMUM NUMBER OF NEIGHBOURS OF A POINT
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELBOR         |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NULONE         |-->| GOES WITH ARRAY NELBOR. NELBOR GIVES THE
!|                |   | ADJACENT ELEMENT, NULONE GIVES THE LOCAL
!|                |   | NUMBER OF THE FIRST NODE OF THE BOUNDARY EDGE
!|                |   | I.E. 1, 2 OR 3 FOR TRIANGLES.
!| ZF             |-->| ELEVATION OF BOTTOM, PER POINT
!| ZFE            |<--| ELEVATION OF BOTTOM, PER ELEMENT
!| ZREF           |<--| CORRECTED BOTTOM ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELEM,NPTFR,NPOIN,MXPTVS
      INTEGER, INTENT(IN)    :: IKLE(NELEM,3),IFABOR(NELEM,3)
      INTEGER, INTENT(IN)    :: NBOR(NPTFR),NELBOR(NPTFR),NULONE(NPTFR)
      INTEGER, INTENT(INOUT) :: ITRA05(NPOIN),ITRA02(NPOIN)
      INTEGER, INTENT(INOUT) :: ITRA03(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: ZFE(NELEM),ZREF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IPTFR,IPOIN,I,I1,I2,I3,N1,N2,ERR,IMAX
      LOGICAL FLAG
!
!     DYNAMICALLY ALLOCATES INTEGERS
!
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRA01,ITRA04,IFAN
!
      INTEGER :: IPREV(3)
      PARAMETER ( IPREV = (/ 3 , 1 , 2 /) )
      DOUBLE PRECISION, PARAMETER :: EPSILO = 1.D-6
!
!-----------------------------------------------------------------------
!
      ALLOCATE(IFAN(NELEM,3)          ,STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IFAN')
      ALLOCATE(ITRA01(NPOIN,MXPTVS+1) ,STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'ITRA01')
      ALLOCATE(ITRA04(NPOIN,6)        ,STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'ITRA04')
!
!-----------------------------------------------------------------------
!
!    FILLS IN IFAN
!
!   IFAN(IELEM,IFACE) GIVES THE LOCAL NUMBER IN THE ELEMENT DEFINED
!   BY IFABOR(IELEM,IFACE) OF THE NODE WITH LOCAL NUMBER IFACE IN
!   ELEMENT IELEM.
!
!    FIRST GO AT FILLING ZFE
!
!    STARTS TO FILL IN ITRA01 AND ITRA02
!
!   ITRA01 AND ITRA02 PERFORM THE REVERSE OPERATION FROM IKLE.
!   IN THIS LOOP ITRA01(IPOIN,1) IS FILLED IN. IT GIVES THE BIGGEST
!   ELEMENT NUMBER CONTAINING IPOIN. ITRA02(IPOIN) GIVES THE LOCAL
!   NUMBER OF IPOIN IN THIS ELEMENT.
!
!-----------------------------------------------------------------------
!
!  INITIALISES ITRA01 TO DETECT HOLES IN THE MESH
!  (POINTS NOT LINKED TO AN ELEMENT, CASE OF CURVILINEAR MESH)
!
      DO I1    = 1 , MXPTVS+1
        DO IPOIN = 1 , NPOIN
          ITRA01(IPOIN,I1) = 0
        ENDDO ! IPOIN
      ENDDO ! I1
!
!
      DO IELEM = 1,NELEM
!
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
!
        IFAN(IELEM,1) = 0
        N1 = IFABOR(IELEM,1)
        IF (N1.GT.0) THEN
          IF(IKLE(N1,1).EQ.I1) IFAN(IELEM,1) = 3
          IF(IKLE(N1,2).EQ.I1) IFAN(IELEM,1) = 1
          IF(IKLE(N1,3).EQ.I1) IFAN(IELEM,1) = 2
        ENDIF
!
        IFAN(IELEM,2) = 0
        N1 = IFABOR(IELEM,2)
        IF (N1.GT.0) THEN
          IF(IKLE(N1,1).EQ.I2) IFAN(IELEM,2) = 3
          IF(IKLE(N1,2).EQ.I2) IFAN(IELEM,2) = 1
          IF(IKLE(N1,3).EQ.I2) IFAN(IELEM,2) = 2
        ENDIF
!
        IFAN(IELEM,3) = 0
        N1 = IFABOR(IELEM,3)
        IF (N1.GT.0) THEN
          IF(IKLE(N1,1).EQ.I3) IFAN(IELEM,3) = 3
          IF(IKLE(N1,2).EQ.I3) IFAN(IELEM,3) = 1
          IF(IKLE(N1,3).EQ.I3) IFAN(IELEM,3) = 2
        ENDIF
!
        ZFE(IELEM) = MAX(ZF(I1),ZF(I2),ZF(I3))
        ITRA01(I1,1) = IELEM
        ITRA02(I1)   = 1
        ITRA01(I2,1) = IELEM
        ITRA02(I2)   = 2
        ITRA01(I3,1) = IELEM
        ITRA02(I3)   = 3
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!    STARTS TO FILL IN ITRA01 AND ITRA02 FOR BOUNDARY NODES
!
!   FOR THESE POINTS ITRA01(IPOIN,1) IS NOT ANY ELEMENT: IT'S THE
!   ELEMENT CONTAINING A BOUNDARY SIDE BETWEEN NODE IPOIN AND THE
!   FOLLOWING NODE IN THE LOCAL NUMBERING OF THE ELEMENT.
!
!-----------------------------------------------------------------------
!
      DO IPTFR = 1,NPTFR
        ITRA01(NBOR(IPTFR),1) = NELBOR(IPTFR)
        ITRA02(NBOR(IPTFR))   = NULONE(IPTFR)
      ENDDO ! IPTFR
!
!-----------------------------------------------------------------------
!
!    RESUMES TO FILL IN ITRA01
!
!   ITRA01(IPOIN,I+1) IS THE NUMBER OF THE ELEMENT ADJACENT TO ELEMENT
!   ITRA01(IPOIN,1) WHEN TURNING ANTI-CLOCKWISE AROUND POINT IPOIN.
!   ITRA02 IS A UNIDIMENSIONAL ARRAY BECAUSE THE LOCAL NUMBER OF IPOIN
!   IN ANY ELEMENT ITRA01(IPOIN,*) WILL NOT NEEDED IN THE FUTURE.
!
!
!    FILLS IN ITRA03
!
!   ITRA03(IPOIN) CORRESPONDS TO THE NUMBER OF ELEMENTS CONTAINING IPOIN
!   (WITH A NEGATIVE SIGN IF IPOIN IS A BOUNDARY NODE).
!
!    BEWARE |||
!
!   ONE POINT CAN ONLY BE PART OF A MAXIMUM OF 10 ELEMENTS
!
!-----------------------------------------------------------------------
!
      DO IPOIN = 1,NPOIN
        ITRA03(IPOIN) = 0
      ENDDO ! IPOIN
!
      IMAX = 0
!
40    CONTINUE
      FLAG = .FALSE.
      IMAX = IMAX + 1
      IF (IMAX.GT.MXPTVS+1) THEN
        WRITE(LU,24) MXPTVS
24      FORMAT(1X,'TOPOGR : THE MAXIMUM NUMBER OF NEIGHBOURS TO'/,1X,
     &            '         A POINT IS GREATER THAN THE VALUE  ',/,1X,
     &            '         GIVEN BY MXPTVS :',1I6)
        CALL PLANTE(0)
        STOP
      ENDIF
!
      DO IPOIN = 1,NPOIN
!
        IF (ITRA03(IPOIN).EQ.0) THEN
          N1 = ITRA01(IPOIN,IMAX)
          IF(N1.NE.0) THEN
            FLAG = .TRUE.
            N2 = IFABOR(N1,IPREV(ITRA02(IPOIN)))
!                        HERE IMAX IS NEVER AT ITS MAXIMUM
            ITRA01(IPOIN,IMAX+1) = N2
            ITRA02(IPOIN) = IFAN(N1,IPREV(ITRA02(IPOIN)))
            IF (N2.LE.0)               ITRA03(IPOIN) = -IMAX
            IF (N2.EQ.ITRA01(IPOIN,1)) ITRA03(IPOIN) =  IMAX
          ENDIF
        ENDIF
!
      ENDDO ! IPOIN
!
      IF (FLAG) GOTO 40
!
60    CONTINUE
!
!-----------------------------------------------------------------------
!
!    DETERMINES LOCAL EXTREMA FOR ZFE BY TURNING AROUND A NODE
!
!   ITRA04(IPOIN,I) CORRESPONDS TO THE IEME EXTREMUM FOUND, WITH THE
!   ASSOCIATED ELEMENT GIVEN BY ITRA01(IPOIN,ITRA04(IPOIN,I)).
!   ITRA02(IPOIN) GIVES THE TOTAL NUMBER OF INCREASE AND DECREASE STAGES
!   (WITH A NEGATIVE SIGN IF THE LAST STAGE FOUND IS A DECREASE).
!
!
!-----------------------------------------------------------------------
!
      DO IPOIN = 1,NPOIN
        ITRA02(IPOIN) = 0
        ITRA05(IPOIN) = 0
      ENDDO ! IPOIN
!
      DO I = 1,IMAX-1
!
        DO IPOIN = 1,NPOIN
!
          IF (ITRA03(IPOIN).GE.I.OR.ITRA03(IPOIN).LT.-I) THEN
!
            N1 = ITRA01(IPOIN,I)
            N2 = ITRA01(IPOIN,I+1)
!
            IF (ZFE(N2).GT.ZFE(N1)+EPSILO) THEN
              IF (ITRA02(IPOIN).LT.0) ITRA04(IPOIN,-ITRA02(IPOIN))=I
              IF (ITRA02(IPOIN).LE.0) ITRA02(IPOIN)=-ITRA02(IPOIN)+1
            ELSEIF (ZFE(N2).LT.ZFE(N1)-EPSILO) THEN
              IF (ITRA02(IPOIN).GT.0) ITRA04(IPOIN, ITRA02(IPOIN))=I
              IF (ITRA02(IPOIN).GE.0) ITRA02(IPOIN)=-ITRA02(IPOIN)-1
            ENDIF
!
          ENDIF
!
        ENDDO ! IPOIN
!
      ENDDO ! I
!
      DO IPOIN = 1,NPOIN
        IF((ITRA03(IPOIN).LT.0.AND.(ITRA02(IPOIN).LE.-4.OR.
     &      ITRA02(IPOIN).GE.5)).OR.ABS(ITRA02(IPOIN)).GE.6) THEN
          WRITE(LU,*) 'THE MESH AROUND THE NODE ',IPOIN,' HAS TO'
          WRITE(LU,*) 'BE REFINED BECAUSE OF THE BATHYMETRY'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO ! IPOIN
!
!-----------------------------------------------------------------------
!
!    CORRECTS ZFE DEPENDING ON ITRA02
!
!-----------------------------------------------------------------------
!
      FLAG = .FALSE.
!
      DO IPOIN = 1,NPOIN
!
        I1 = ITRA03(IPOIN)
!
        IF (I1.LT.0) THEN
!
!-----------------------------------------------------------------------
!
!    CORRECTS ZFE FOR BOUNDARY NODES
!
!   IF ITRA02(IPOIN) EQUALS 1, -1, 2 : NO CORRECTION
!   IF ITRA02(IPOIN) EQUALS -2, 3, -3, 4 : CORRECTION
!
!-----------------------------------------------------------------------
!
          IF (ITRA02(IPOIN).EQ.-2) THEN
!
            FLAG = .TRUE.
            IF (ZFE(ITRA01(IPOIN,-I1)).GT.ZFE(ITRA01(IPOIN,1))) THEN
              ITRA02(IPOIN) = ITRA04(IPOIN,1) + 1
              ITRA05(IPOIN) = -I1
            ELSE
              ITRA02(IPOIN) = 1
              ITRA05(IPOIN) = ITRA04(IPOIN,1) - 1
            ENDIF
            ZREF(IPOIN) = ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1)))
!
          ELSEIF (ITRA02(IPOIN).EQ.3) THEN
!
            FLAG = .TRUE.
            IF (ZFE(ITRA01(IPOIN,ITRA04(IPOIN,2))).GT.
     &          ZFE(ITRA01(IPOIN,1))) THEN
              ITRA02(IPOIN) = ITRA04(IPOIN,1) + 1
              ITRA05(IPOIN) = -I1
            ELSE
              ITRA02(IPOIN) = 1
              ITRA05(IPOIN) = ITRA04(IPOIN,1) - 1
            ENDIF
            ZREF(IPOIN) = ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1)))
!
          ELSEIF (ITRA02(IPOIN).EQ.-3) THEN
!
            FLAG = .TRUE.
            IF (ZFE(ITRA01(IPOIN,-I1)).GT.
     &          ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1)))) THEN
              ITRA02(IPOIN) = ITRA04(IPOIN,2) + 1
              ITRA05(IPOIN) = -I1
            ELSE
              ITRA02(IPOIN) = 1
              ITRA05(IPOIN) = ITRA04(IPOIN,2) - 1
            ENDIF
            ZREF(IPOIN) = ZFE(ITRA01(IPOIN,ITRA04(IPOIN,2)))
!
          ELSEIF (ITRA02(IPOIN).EQ.4) THEN
!
            FLAG = .TRUE.
            IF (ZFE(ITRA01(IPOIN,ITRA04(IPOIN,3))).GT.
     &          ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1)))) THEN
              ITRA02(IPOIN) = ITRA04(IPOIN,2) + 1
              ITRA05(IPOIN) = -I1
            ELSE
              ITRA02(IPOIN) = 1
              ITRA05(IPOIN) = ITRA04(IPOIN,2) - 1
            ENDIF
            ZREF(IPOIN) = ZFE(ITRA01(IPOIN,ITRA04(IPOIN,2)))
!
          ENDIF
!
        ELSE
!
!-----------------------------------------------------------------------
!
!    CORRECTS ZFE FOR INTERIOR NODES
!
!   IF ITRA02(IPOIN) EQUALS 1, -1, 2, -2, 3, -3 : NO CORRECTION
!   IF ITRA02(IPOIN) EQUALS 4, -4, 5, -5 : CORRECTION
!
!-----------------------------------------------------------------------
!
          IF (ITRA02(IPOIN).EQ.4) THEN
!
            FLAG = .TRUE.
            IF (ZFE(ITRA01(IPOIN,ITRA04(IPOIN,3))).GT.
     &          ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1)))) THEN
              ITRA02(IPOIN) = ITRA04(IPOIN,2) + 1
              ITRA05(IPOIN) = I1
            ELSE
              ITRA02(IPOIN) = 2
              ITRA05(IPOIN) = ITRA04(IPOIN,2) - 1
            ENDIF
            ZREF(IPOIN) = MIN(ZFE(ITRA01(IPOIN,ITRA04(IPOIN,2))),
     &                        ZFE(ITRA01(IPOIN,1)))
!
          ELSEIF (ITRA02(IPOIN).EQ.-4) THEN
!
            FLAG = .TRUE.
            IF (ZFE(ITRA01(IPOIN,ITRA04(IPOIN,2))).GT.
     &          ZFE(ITRA01(IPOIN,1))) THEN
              ITRA02(IPOIN) = ITRA04(IPOIN,1) + 1
              ITRA05(IPOIN) = ITRA04(IPOIN,3) - 1
            ELSE
              ITRA02(IPOIN) = MOD(ITRA04(IPOIN,3),I1) + 1
              ITRA05(IPOIN) = ITRA04(IPOIN,1) - 1
            ENDIF
            ZREF(IPOIN) = MIN(ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1))),
     &                        ZFE(ITRA01(IPOIN,ITRA04(IPOIN,3))))
!
          ELSEIF (ITRA02(IPOIN).EQ.5) THEN
!
            FLAG = .TRUE.
            IF (ZFE(ITRA01(IPOIN,ITRA04(IPOIN,4))).GT.
     &          ZFE(ITRA01(IPOIN,ITRA04(IPOIN,2)))) THEN
              ITRA02(IPOIN) = ITRA04(IPOIN,3) + 1
              ITRA05(IPOIN) = ITRA04(IPOIN,1) - 1
            ELSE
              ITRA02(IPOIN) = ITRA04(IPOIN,1) + 1
              ITRA05(IPOIN) = ITRA04(IPOIN,3) - 1
            ENDIF
            ZREF(IPOIN) = MIN(ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1))),
     &                        ZFE(ITRA01(IPOIN,ITRA04(IPOIN,3))))
!
          ELSEIF (ITRA02(IPOIN).EQ.-5) THEN
!
            FLAG = .TRUE.
            IF (ZFE(ITRA01(IPOIN,ITRA04(IPOIN,3))).GT.
     &          ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1)))) THEN
              ITRA02(IPOIN) = ITRA04(IPOIN,2) + 1
              ITRA05(IPOIN) = ITRA04(IPOIN,4) - 1
            ELSE
              ITRA02(IPOIN) = MOD(ITRA04(IPOIN,4),I1) + 1
              ITRA05(IPOIN) = ITRA04(IPOIN,2) - 1
            ENDIF
            ZREF(IPOIN) = MIN(ZFE(ITRA01(IPOIN,ITRA04(IPOIN,2))),
     &                        ZFE(ITRA01(IPOIN,ITRA04(IPOIN,4))))
!
          ENDIF
!
        ENDIF
!
      ENDDO ! IPOIN
!
      IF (FLAG) THEN
!
        DO IPOIN = 1,NPOIN
!
          IF (ITRA05(IPOIN).NE.0) THEN
!
            IF (ITRA05(IPOIN).LT.ITRA02(IPOIN)) THEN
              DO I = ITRA02(IPOIN),ITRA03(IPOIN)
                ZFE(ITRA01(IPOIN,I)) = MAX(ZFE(ITRA01(IPOIN,I)),
     &                                     ZREF(IPOIN))
              ENDDO ! I
              ITRA02(IPOIN) = 1
            ENDIF
!
            DO I = ITRA02(IPOIN),ITRA05(IPOIN)
              ZFE(ITRA01(IPOIN,I)) = MAX(ZFE(ITRA01(IPOIN,I)),
     &                                   ZREF(IPOIN))
            ENDDO ! I
!
          ENDIF
!
        ENDDO ! IPOIN
!
        GOTO 60
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(IFAN)
      DEALLOCATE(ITRA01)
      DEALLOCATE(ITRA04)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
