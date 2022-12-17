!                   *****************
                    SUBROUTINE MASKBD
!                   *****************
!
     &(MASKEL,ZFE,ZF,HN,HMIN,IKLE,IFABOR,ITRA01,NELEM,NPOIN)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MASKS DRY OR PARTIALLY DRY ELEMENTS.
!code
!+     ALGORITHM:
!+
!+    - AN ELEMENT IS MASKED IF THE BOTTOM ELEVATION ZFE IS HIGHER THAN
!+      THE FREE SURFACE ELEVATION AT ITS BARYCENTRE
!+
!+    - ANY ELEMENT WHICH BOTTOM ELEVATION ZFE IS HIGHER THAN THE
!+      ELEVATION ZFE OF A MASKED NEIGHBOUR IS IN TURN MASKED
!+
!+      WHEN TURNING AROUND A NODE, FUNCTION ZFE ONLY HAS A MIN AND
!+      A MAX (SEE TOPOGR). THE SECOND PHASE OF THE ALGORITHM THUS
!+      ENSURES THAT NO 2 PARTS OF THE DOMAIN ARE ONLY CONNECTED BY
!+      1 VERTEX. THIS TREATMENT ALSO PREVENTS INOPPORTUNE MASKING-
!+      DEMASKING, IN PARTICULAR IN TIDAL FLAT AREAS.
!+
!+
!+      DISADVANTAGES:
!+
!+      THIS ALGORITHM ASSUMES THAT THE FREE SURFACE IS QUASI HORIZONTAL.
!+      IT IS WELL SUITED TO STUDY EVOLUTIONS DUE TO TIDAL EFFECTS, BUT
!+      NOT TO STUDY DAM BREAKS.
!
!history  J-M JANIN (LNH)
!+        11/08/94
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
!| HMIN           |-->| MINIMUM VALUE OF DEPTH
!| HN             |-->| WATER DEPTH AT TIME N
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID
!|                |   | BOUNDARY
!| IKLE           |-->| CONNECTIVITY TABLE.
!| ITRA01         |-->| WORK ARRAY OF INTEGERS
!| MASKEL         |<--| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZFE            |-->| ELEVATION OF BOTTOM, PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM,NPOIN
      INTEGER, INTENT(IN)             :: IKLE(NELEM,3),IFABOR(NELEM,3)
      INTEGER, INTENT(INOUT)          :: ITRA01(NELEM)
      DOUBLE PRECISION, INTENT(IN)    :: ZFE(NELEM),ZF(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HMIN
      DOUBLE PRECISION, INTENT(INOUT) :: MASKEL(NELEM)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,N
!
      DOUBLE PRECISION ZSE
!
      LOGICAL FLAG
!
      DOUBLE PRECISION, PARAMETER :: EPSILO = 1.D-6
!
!-----------------------------------------------------------------------
!
      FLAG = .FALSE.
!
      DO IELEM = 1,NELEM
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        ZSE = (ZF(I1)+HN(I1)+ZF(I2)+HN(I2)+ZF(I3)+HN(I3))/3.D0
        IF (ZFE(IELEM)+HMIN+EPSILO.GT.ZSE) THEN
          FLAG = .TRUE.
          MASKEL(IELEM) = 0.D0
        ENDIF
      ENDDO ! IELEM
!
20    CONTINUE
!
      IF (FLAG) THEN
!
        FLAG = .FALSE.
        DO IELEM = 1,NELEM
!
          ITRA01(IELEM) = 0
          IF (MASKEL(IELEM).GT.0.5D0) THEN
!
            N=IFABOR(IELEM,1)
            IF (N.GT.0) THEN
              IF (MASKEL(N).LT.0.5D0.AND.ZFE(IELEM).GT.
     &            ZFE(N)-EPSILO) ITRA01(IELEM) = 1
            ENDIF
            N=IFABOR(IELEM,2)
            IF (N.GT.0) THEN
              IF (MASKEL(N).LT.0.5D0.AND.ZFE(IELEM).GT.
     &            ZFE(N)-EPSILO) ITRA01(IELEM) = 1
            ENDIF
            N=IFABOR(IELEM,3)
            IF (N.GT.0) THEN
              IF (MASKEL(N).LT.0.5D0.AND.ZFE(IELEM).GT.
     &            ZFE(N)-EPSILO) ITRA01(IELEM) = 1
            ENDIF
!
          ENDIF
!
        ENDDO ! IELEM
!
        DO IELEM = 1,NELEM
          IF (ITRA01(IELEM).EQ.1) THEN
            FLAG = .TRUE.
            MASKEL(IELEM) = 0.D0
          ENDIF
        ENDDO ! IELEM
!
        GOTO 20
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
