!                   *******************
                    SUBROUTINE RECEDING
!                   *******************
!
     &(H,ZF,HREC,V2DPAR,IKLE,NPOIN,NELEM,NELMAX,DELTAH,HITS,
     & MESH,W1,YAFLODEL,FLODEL,DT)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    On tidal flats, forces small films of water to recede back
!+        in deeper waters. The call to RECEDING is triggered by the
!+        keyword THRESHOLD DEPTH FOR RECEDING PROCEDURE.
!
!history  J-M HERVOUET (LNHE)
!+        17/07/2012
!+        V6P2
!+  First version, adapted to tracer advection and parallelism.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP
!| H              |<--| WATER DEPTH
!| HREC           |-->| THRESHOLD VALUE OF DEPTH
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS OF DEPTH
!| V2DPAR         |-->| INTEGRAL OF BASES, ASSEMBLED IN PARALLEL
!| W1             |<->| WORK ARRAY FOR ELEMENTS IN A BIEF_OBJ STRUCTURE
!| ZF             |-->| BOTTOM ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: NPOIN,NELEM,NELMAX
      INTEGER,          INTENT(IN)    :: IKLE(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN)    :: HREC,DT
      DOUBLE PRECISION, INTENT(IN)    :: V2DPAR(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN),W1(NELEM,3)
      LOGICAL,          INTENT(IN)    :: YAFLODEL
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: DELTAH,HITS,FLODEL
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,I
      DOUBLE PRECISION SL1,SL2,SL3
!
      INTRINSIC MAX,MIN
!
!-----------------------------------------------------------------------
!
!     LOOP 1: COUNTING POTENTIAL TRANSFERS FOR EVERY POINT
!
      CALL OS('X=0     ',X=HITS)
!
      DO IELEM=1,NELEM
!
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
        SL1=H(I1)+ZF(I1)
        SL2=H(I2)+ZF(I2)
        SL3=H(I3)+ZF(I3)
!
!       ONLY TIDAL FLATS CONSIDERED, SAME CRITERION AS IN DECV11 (BIEF)
!
        IF(MAX(ZF(I1),ZF(I2),ZF(I3)).GT.MIN(SL1,SL2,SL3)) THEN
!
!         SMALL FILM ON POINT 1
!
          IF(H(I1).GT.0.D0.AND.H(I1).LE.HREC) THEN
            IF(ZF(I1).GT.SL2+HREC.AND.H(I2).GT.HREC) THEN
              HITS%R(I1)=HITS%R(I1)+1.D0
            ENDIF
            IF(ZF(I1).GT.SL3+HREC.AND.H(I3).GT.HREC) THEN
              HITS%R(I1)=HITS%R(I1)+1.D0
            ENDIF
          ENDIF
!
!         SMALL FILM ON POINT 2
!
          IF(H(I2).GT.0.D0.AND.H(I2).LE.HREC) THEN
            IF(ZF(I2).GT.SL1+HREC.AND.H(I1).GT.HREC) THEN
              HITS%R(I2)=HITS%R(I2)+1.D0
            ENDIF
            IF(ZF(I2).GT.SL3+HREC.AND.H(I3).GT.HREC) THEN
              HITS%R(I2)=HITS%R(I2)+1.D0
            ENDIF
          ENDIF
!
!         SMALL FILM ON POINT 3
!
          IF(H(I3).GT.0.D0.AND.H(I3).LE.HREC) THEN
            IF(ZF(I3).GT.SL1+HREC.AND.H(I1).GT.HREC) THEN
              HITS%R(I3)=HITS%R(I3)+1.D0
            ENDIF
            IF(ZF(I3).GT.SL2+HREC.AND.H(I2).GT.HREC) THEN
              HITS%R(I3)=HITS%R(I3)+1.D0
            ENDIF
          ENDIF
        ENDIF
!
      ENDDO
!
!     LOOP 2: TRANSFERS
!
!     IF A POINT IS TREATED SEVERAL TIMES (HITS), ONLY A PERCENTAGE
!     OF TRANSFER 1/HITS WILL BE DONE EACH TIME
!
      IF(NCSIZE.GT.1) CALL PARCOM(HITS,2,MESH)
!
      DO I=1,NPOIN
        IF(HITS%R(I).GT.1.5D0) HITS%R(I)=1.D0/HITS%R(I)
      ENDDO
!
!     INITIALING W1 THAT WILL GIVE THE INTERNAL FLUXES
!     INDUCED BY THE TRANSFERS
!
      DO IELEM=1,NELEM
        W1(IELEM,1)=0.D0
        W1(IELEM,2)=0.D0
        W1(IELEM,3)=0.D0
      ENDDO
!
      CALL OS('X=0     ',X=DELTAH)
!
      DO IELEM=1,NELEM
!
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
        SL1=H(I1)+ZF(I1)
        SL2=H(I2)+ZF(I2)
        SL3=H(I3)+ZF(I3)
!
!       ONLY TIDAL FLATS CONSIDERED, SAME CRITERION AS IN DECV11 (BIEF)
!
        IF(MAX(ZF(I1),ZF(I2),ZF(I3)).GT.MIN(SL1,SL2,SL3)) THEN
!
!         SMALL FILM ON POINT 1
!
          IF(H(I1).GT.0.D0.AND.H(I1).LE.HREC) THEN
            IF(ZF(I1).GT.SL2+HREC.AND.H(I2).GT.HREC) THEN
!             TRANSFER FROM I1 TO I2
              DELTAH%R(I2)=DELTAH%R(I2)
     &                    +H(I1)*HITS%R(I1)*V2DPAR(I1)/V2DPAR(I2)
              DELTAH%R(I1)=DELTAH%R(I1)-HITS%R(I1)*H(I1)
              W1(IELEM,1)=W1(IELEM,1)
     &                +HITS%R(I1)*H(I1)*V2DPAR(I1)
              W1(IELEM,2)=W1(IELEM,2)
     &                -HITS%R(I1)*H(I1)*V2DPAR(I1)
            ENDIF
            IF(ZF(I1).GT.SL3+HREC.AND.H(I3).GT.HREC) THEN
!             TRANSFER FROM I1 TO I3
              DELTAH%R(I3)=DELTAH%R(I3)
     &                    +H(I1)*HITS%R(I1)*V2DPAR(I1)/V2DPAR(I3)
              DELTAH%R(I1)=DELTAH%R(I1)-HITS%R(I1)*H(I1)
              W1(IELEM,1)=W1(IELEM,1)
     &                +HITS%R(I1)*H(I1)*V2DPAR(I1)
              W1(IELEM,3)=W1(IELEM,3)
     &                -HITS%R(I1)*H(I1)*V2DPAR(I1)
            ENDIF
          ENDIF
!
!         SMALL FILM ON POINT 2
!
          IF(H(I2).GT.0.D0.AND.H(I2).LE.HREC) THEN
            IF(ZF(I2).GT.SL1+HREC.AND.H(I1).GT.HREC) THEN
!             TRANSFER FROM I2 TO I1
              DELTAH%R(I1)=DELTAH%R(I1)
     &                    +H(I2)*HITS%R(I2)*V2DPAR(I2)/V2DPAR(I1)
              DELTAH%R(I2)=DELTAH%R(I2)-HITS%R(I2)*H(I2)
              W1(IELEM,2)=W1(IELEM,2)
     &                +HITS%R(I2)*H(I2)*V2DPAR(I2)
              W1(IELEM,1)=W1(IELEM,1)
     &                -HITS%R(I2)*H(I2)*V2DPAR(I2)
            ENDIF
            IF(ZF(I2).GT.SL3+HREC.AND.H(I3).GT.HREC) THEN
!             TRANSFER FROM I2 TO I3
              DELTAH%R(I3)=DELTAH%R(I3)
     &                    +H(I2)*HITS%R(I2)*V2DPAR(I2)/V2DPAR(I3)
              DELTAH%R(I2)=DELTAH%R(I2)-HITS%R(I2)*H(I2)
              W1(IELEM,2)=W1(IELEM,2)
     &                +HITS%R(I2)*H(I2)*V2DPAR(I2)
              W1(IELEM,3)=W1(IELEM,3)
     &                -HITS%R(I2)*H(I2)*V2DPAR(I2)
            ENDIF
          ENDIF
!
!         SMALL FILM ON POINT 3
!
          IF(H(I3).GT.0.D0.AND.H(I3).LE.HREC) THEN
            IF(ZF(I3).GT.SL1+HREC.AND.H(I1).GT.HREC) THEN
!             TRANSFER FROM I3 TO I1
              DELTAH%R(I1)=DELTAH%R(I1)
     &                    +H(I3)*HITS%R(I3)*V2DPAR(I3)/V2DPAR(I1)
              DELTAH%R(I3)=DELTAH%R(I3)-HITS%R(I3)*H(I3)
              W1(IELEM,3)=W1(IELEM,3)
     &                +HITS%R(I3)*H(I3)*V2DPAR(I3)
              W1(IELEM,1)=W1(IELEM,1)
     &                -HITS%R(I3)*H(I3)*V2DPAR(I3)
            ENDIF
            IF(ZF(I3).GT.SL2+HREC.AND.H(I2).GT.HREC) THEN
!             TRANSFER FROM I3 TO I2
              DELTAH%R(I2)=DELTAH%R(I2)
     &                    +H(I3)*HITS%R(I3)*V2DPAR(I3)/V2DPAR(I2)
              DELTAH%R(I3)=DELTAH%R(I3)-HITS%R(I3)*H(I3)
              W1(IELEM,3)=W1(IELEM,3)
     &                +HITS%R(I3)*H(I3)*V2DPAR(I3)
              W1(IELEM,2)=W1(IELEM,2)
     &                -HITS%R(I3)*H(I3)*V2DPAR(I3)
            ENDIF
          ENDIF
        ENDIF
!
      ENDDO
!
!     LOOP 3: ADDING DELTAH TO H
!
      IF(NCSIZE.GT.1) CALL PARCOM(DELTAH,2,MESH)
!
!     FINAL DEPTH, THE MAX IS JUST TO AVOID NEGATIVE VALUES DUE
!     TO TRUNCATION ERRORS
!
      DO I=1,NPOIN
        H(I)=MAX(H(I)+DELTAH%R(I),0.D0)
      ENDDO
!
!     CORRECTION OF FLUXES BETWEEN POINTS
!
      IF(YAFLODEL) THEN
!
!       TRANSFORMING W1 INTO FLUXES
!
        DO IELEM=1,NELEM
          W1(IELEM,1)=W1(IELEM,1)/DT
          W1(IELEM,2)=W1(IELEM,2)/DT
          W1(IELEM,3)=W1(IELEM,3)/DT
        ENDDO
!
!       TAKES THESE FLUXES TO CORRECT FLODEL
!
        CALL FLUX_EF_VF(FLODEL%R,W1,MESH%NSEG,MESH%NELEM,MESH%NELMAX,
     &                  MESH%ELTSEG%I,MESH%ORISEG%I,
     &                  MESH%IKLE%I,.FALSE.,2)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
