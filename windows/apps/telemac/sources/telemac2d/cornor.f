!                   *****************
                    SUBROUTINE CORNOR
!                   *****************
!
     &(XNEBOR,YNEBOR,XSGBOR,YSGBOR,NPTFR,KLOG,
     & LIHBOR,T1,T2,MESH,IKLBOR,NELEB,NELEBX,VNX1,VNY1,YESNOFR)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    CORRECTS THE NORMALS TO THE NODES IN ACCORDANCE WITH
!+                THE BOUNDARY CONDITIONS TO HAVE NORMALS TO ADJACENT
!+                LIQUID SEGMENTS IN THE CASE OF A TRANSITION BETWEEN
!+                LIQUID AND SOLID.
!
!history  J-M HERVOUET (LNHE)
!+        19/09/2008
!+        V5P9
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/03/2014
!+        V7P0
!+   Now written to enable different numbering of boundary points and
!+   boundary segments.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLBOR         |-->| CONNECTIVITY TABLE OF BOUNDARY ELEMENTS
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| MESH           |-->| MESH STRUCTURE
!| NELEB          |-->| NUMBER OF BOUNDARY ELEMENTS
!| NELEBX         |-->| MAXIMUM NUMBER OF BOUNDARY ELEMENTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| XNEBOR         |<--| X-COMPONENT OF NORMAL AT NODES
!| XSGBOR         |-->| X-COMPONENT OF NORMAL TO SEGMENTS
!| YNEBOR         |<--| Y-COMPONENT OF NORMAL AT NODES
!| YSGBOR         |-->| Y-COMPONENT OF NORMAL TO SEGMENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPTFR,KLOG,NELEB,NELEBX
      INTEGER, INTENT(IN)             :: LIHBOR(NPTFR)
      INTEGER, INTENT(IN)             :: IKLBOR(NELEBX,2)
      DOUBLE PRECISION, INTENT(IN)    :: XSGBOR(NELEBX,4)
      DOUBLE PRECISION, INTENT(IN)    :: YSGBOR(NELEBX,4)
      DOUBLE PRECISION, INTENT(INOUT) :: XNEBOR(NPTFR,2),YNEBOR(NPTFR,2)
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
      DOUBLE PRECISION, INTENT(INOUT), OPTIONAL :: VNX1(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT), OPTIONAL :: VNY1(NPTFR)
      LOGICAL, INTENT(INOUT), OPTIONAL :: YESNOFR(NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,KP1,IELEB
      DOUBLE PRECISION XNORM
      DOUBLE PRECISION XNN_TMP(NELEB),YNN_TMP(NELEB)
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
!     CONSIDERS ONLY THE NORMALS TO LIQUID BOUNDARIES HERE
!     THE ONLY ONES USED
!
!     COPIES THEN CANCELS T1 AND T2 FOR SOLID BOUNDARIES
!
      IF(NELEB.GT.0) THEN
        DO IELEB=1,NELEB
          K  =IKLBOR(IELEB,1)
          KP1=IKLBOR(IELEB,2)
!         NON NORMALISED VERSION OF XSGBOR AND YSGBOR
          T1%R(IELEB)=XSGBOR(IELEB,3)
          T2%R(IELEB)=YSGBOR(IELEB,3)
          XNN_TMP(IELEB)=XSGBOR(IELEB,3)
          YNN_TMP(IELEB)=YSGBOR(IELEB,3)
          IF(LIHBOR(K).EQ.KLOG.OR.LIHBOR(KP1).EQ.KLOG) THEN
            T1%R(IELEB)=0.D0
            T2%R(IELEB)=0.D0
          ENDIF
        ENDDO
      ENDIF
!
!     START OF COMPUTATION OF XNEBOR AND YNEBOR FOR THE LIQUID BOUNDARIES
!
      IF(NPTFR.GT.0) THEN
        DO K=1,NPTFR
          XNEBOR(K,1)=0.D0
          YNEBOR(K,1)=0.D0
          IF(PRESENT(VNX1).AND.PRESENT(VNY1).AND.PRESENT(YESNOFR)) THEN
            VNX1(K) = 0.D0
            VNY1(K) = 0.D0
            YESNOFR(K) = .FALSE.
          ENDIF
        ENDDO
        IF(NELEB.GT.0) THEN
          DO IELEB=1,NELEB
            K  =IKLBOR(IELEB,1)
            KP1=IKLBOR(IELEB,2)
            XNEBOR(K  ,1)=XNEBOR(K  ,1)+T1%R(IELEB)
            YNEBOR(K  ,1)=YNEBOR(K  ,1)+T2%R(IELEB)
            XNEBOR(KP1,1)=XNEBOR(KP1,1)+T1%R(IELEB)
            YNEBOR(KP1,1)=YNEBOR(KP1,1)+T2%R(IELEB)
            IF(PRESENT(VNX1).AND.PRESENT(VNY1)) THEN
              VNX1(K) = VNX1(K) + XNN_TMP(IELEB) - T1%R(IELEB)
              VNY1(K) = VNY1(K) + YNN_TMP(IELEB) - T2%R(IELEB)
              VNX1(KP1) = VNX1(KP1) + XNN_TMP(IELEB) - T1%R(IELEB)
              VNY1(KP1) = VNY1(KP1) + YNN_TMP(IELEB) - T2%R(IELEB)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     ASSEMBLY IN PARALLEL (EVEN IF NPTFR=0)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM_BORD(XNEBOR,2,MESH)
        CALL PARCOM_BORD(YNEBOR,2,MESH)
        IF(PRESENT(VNX1).AND.PRESENT(VNY1)) THEN
          CALL PARCOM_BORD(VNX1,2,MESH)
          CALL PARCOM_BORD(VNY1,2,MESH)
        ENDIF
      ENDIF
!
!     RENORMALISATION
!
      IF(NPTFR.GT.0) THEN
        DO K=1,NPTFR
          XNORM=SQRT(XNEBOR(K,1)**2+YNEBOR(K,1)**2)
          IF(XNORM.GT.1.D-10) THEN
!           SAVING THE NON-NORMED VERSION
            XNEBOR(K,2)=0.5D0*XNEBOR(K,1)
            YNEBOR(K,2)=0.5D0*YNEBOR(K,1)
            IF(PRESENT(VNX1).AND.PRESENT(VNY1)
     &         .AND.PRESENT(YESNOFR)) THEN
              VNX1(K) = 0.5D0* VNX1(K)
              VNY1(K) = 0.5D0* VNY1(K)
              IF(VNX1(K).NE.0.D0.OR.VNY1(K).NE.0.D0) THEN
                YESNOFR(K) = .TRUE.
              ENDIF
            ENDIF
!           NOW NORMALISE
            XNEBOR(K,1)=XNEBOR(K,1)/XNORM
            YNEBOR(K,1)=YNEBOR(K,1)/XNORM
          ELSE
!           POINT BETWEEN TWO SOLID SEGMENTS
!           TAKES THE ORIGINAL COMPUTATION DONE IN NORMAB
            XNORM=SQRT(XNEBOR(K,2)**2+YNEBOR(K,2)**2)
            XNEBOR(K,1)=XNEBOR(K,2)/XNORM
            YNEBOR(K,1)=YNEBOR(K,2)/XNORM
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
