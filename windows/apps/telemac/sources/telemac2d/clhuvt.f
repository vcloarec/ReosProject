!                   *****************
                    SUBROUTINE CLHUVT
!                   *****************
!
     &(NWEIRS,NPSING,NDGA1,NDGB1,ZDIG,X,Y,ZF,IOPTAN,UNORM,CHESTR,
     & NKFROT,KARMAN,T,NTRAC,H,UBOR,VBOR,TBOR,NBOR,
     & LIHBOR,LIUBOR,LIVBOR,LITBOR)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    DEFINES THE DEPTHS, VELOCITIES, ... TO BE IMPOSED
!+                AT THE NODES, FROM THE DEPTHS AND AVERAGE FLOWS
!+                ON THE SEGMENTS CONSTITUTING THE SINGULARITY.
!
!history  V. GUINOT (LHF)
!+        19/04/1996
!+
!+
!
!history  J.-M. HERVOUET (LNH)
!+        23/11/2005
!+        V5P6
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
!history  J.-M. HERVOUET (LNHE)
!+        09/08/2011
!+        V6P2
!+   Adaptation to parallelism
!+
!
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        12/06/2013
!+        V6P3
!+   Adaptation to the dynamic allocation of weirs
!
!history  J.-M. HERVOUET (EDF LAB, LNHE)
!+        11/02/2014
!+        V7P0
!+   All formulas involving P_MAX and P_MIN simplified.
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHESTR         |-->| FRICTION COEFFICIENT.
!| H              |-->| WATER DEPTH.
!| IOPTAN         |-->| OPTION FOR TANGENTIAL VELOCITIES.
!| KARMAN         |-->| VON KARMAN CONSTANT.
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| LITBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON TRACERS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NKFROT         |-->| FRICTION LAW, PER POINT
!| NPSING         |-->| NUMBER OF POINTS FOR EVERY SINGULARITY.
!| NTRAC          |-->| NUMBER OF TRACERS
!| NDGA1          |-->| NDGA1%ADR(I)%I(NP) : BOUNDARY NUMBER OF POINT NP
!|                |   | OF WEIR I (side1)
!| NDGB1          |-->| NDGB1%ADR(I)%I(NP) : BOUNDARY NUMBER OF POINT NP
!|                |   | OF WEIR I (side2)
!| NWEIRS         |-->| NUMBER OF SINGULARITIES
!| T              |-->| BLOCK OF TRACERS.
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| UNORM          |-->| NORM OF VELOCITY
!| X              |-->| ABSCISSAE OF NODES
!| Y              |-->| ORDINATES OF NODES
!| ZDIG           |-->| ELEVATIONS OF POINTS OF WEIRS
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_MIN,
     &                               P_DMAX_ARRAY,P_DMIN_ARRAY
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NWEIRS,IOPTAN
      INTEGER, INTENT(IN)             :: NBOR(*),NKFROT(*)
      INTEGER, INTENT(INOUT)          :: LIUBOR(*),LIHBOR(*),LIVBOR(*)
      INTEGER, INTENT(IN)             :: NTRAC
      DOUBLE PRECISION, INTENT(IN)    :: X(*),Y(*),ZF(*),CHESTR(*),H(*)
      DOUBLE PRECISION, INTENT(IN)    :: UNORM(*)
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(*),VBOR(*)
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: NPSING,NDGA1,NDGB1
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: ZDIG
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TBOR,LITBOR
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,I0,I1,I2,K,N,N0,N1,N2,ITRAC
      DOUBLE PRECISION :: VALUES(8), MAXVALUES(8), MINVALUES(8)
!
      DOUBLE PRECISION DL,NX,NY,PENTE,CZ,HH,TX,TY,UTAN,XX
      DOUBLE PRECISION H0,H2,ZF0,ZF2,X0,X2,Y0,Y2,UNORM1,T1,T2
!
      INTRINSIC ABS,SQRT,SIGN
!
!-----------------------------------------------------------------------
!
!     LOOPS ON THE HYDRAULIC STRUCTURES
!
      DO N=1,NWEIRS
!
!       LOOPS ON THE CRESTS OF THE HYDRAULIC STRUCTURES
!
        DO K=1,2
!
!         LOOPS ON THE NODES OF EACH CREST
!
          DO I=1,NPSING%I(N)
!
          IF(K.EQ.1) THEN
            I1=NDGA1%ADR(N)%P%I(I)
          ELSE
            I1=NDGB1%ADR(N)%P%I(I)
          ENDIF
!
          IF(I.EQ.1) THEN
            I0=I1
            IF(K.EQ.1) THEN
              I2=NDGA1%ADR(N)%P%I(I+1)
            ELSE
              I2=NDGB1%ADR(N)%P%I(I+1)
            ENDIF
            XX=0.D0
          ELSEIF(I.LT.NPSING%I(N)) THEN
            IF(K.EQ.1) THEN
              I0=NDGA1%ADR(N)%P%I(I-1)
              I2=NDGA1%ADR(N)%P%I(I+1)
            ELSE
              I0=NDGB1%ADR(N)%P%I(I-1)
              I2=NDGB1%ADR(N)%P%I(I+1)
            ENDIF
            XX=1.D0
          ELSE
            IF(K.EQ.1) THEN
              I0=NDGA1%ADR(N)%P%I(I-1)
            ELSE
              I0=NDGB1%ADR(N)%P%I(I-1)
            ENDIF
            I2=I1
            XX=0.D0
          ENDIF
!
          IF(I0.GT.0) THEN
            N0=NBOR(I0)
            X0=X(N0)
            Y0=Y(N0)
            H0=H(N0)
            ZF0=ZF(N0)
          ELSE
            X0=0.D0
            Y0=0.D0
            H0=0.D0
            ZF0=0.D0
          ENDIF
!
          IF(I2.GT.0) THEN
            N2=NBOR(I2)
            X2=X(N2)
            Y2=Y(N2)
            H2=H(N2)
            ZF2=ZF(N2)
          ELSE
            X2=0.D0
            Y2=0.D0
            H2=0.D0
            ZF2=0.D0
          ENDIF
!
          IF(NCSIZE.GT.1) THEN
            VALUES(1) = H0
            VALUES(2) = H2
            VALUES(3) = X0
            VALUES(4) = X2
            VALUES(5) = Y0
            VALUES(6) = Y2
            VALUES(7) = ZF0
            VALUES(8) = ZF2

            CALL P_DMAX_ARRAY(VALUES, 8, MAXVALUES)
            CALL P_DMIN_ARRAY(VALUES, 8, MINVALUES)

            H0 =MAXVALUES(1)+MINVALUES(1)
            H2 =MAXVALUES(2)+MINVALUES(2)
            X0 =MAXVALUES(3)+MINVALUES(3)
            X2 =MAXVALUES(4)+MINVALUES(4)
            Y0 =MAXVALUES(5)+MINVALUES(5)
            Y2 =MAXVALUES(6)+MINVALUES(6)
            ZF0=MAXVALUES(7)+MINVALUES(7)
            ZF2=MAXVALUES(8)+MINVALUES(8)
          ENDIF
!
!         CALCULATES THE NORMAL VECTOR, OUTGOING CREST 1, ENTERING CREST 2
!
          TX=X2-X0
          TY=Y2-Y0
          DL=SQRT(TX**2+TY**2)
          TX=TX/DL
          TY=TY/DL
          NX=-TY
          NY=TX
!
!         CALCULATES THE TANGENTIAL VELOCITY
!
          IF(I1.GT.0) THEN
!
          N1=NBOR(I1)
!
          IF(IOPTAN.EQ.0) THEN
!
            UTAN=0.D0
!
          ELSEIF(IOPTAN.EQ.1) THEN
!
!           ONE TAKES THE HEIGHT ON THE CREST (TO BE DISCUSSED)
!           HH = H(N1)
            HH = H(N1)+ZF(N1)-ZDIG%ADR(N)%P%R(I)
!           HH MAY BE NEGATIVE
            HH=MAX(HH,0.D0)
            PENTE=(H0-H2+ZF0-ZF2)/DL
!
            IF (NKFROT(N1).EQ.2) THEN
              UTAN = CHESTR(N1)*SQRT(HH*ABS(PENTE))*SIGN(1.D0,PENTE)
            ELSEIF (NKFROT(N1).EQ.3) THEN
              UTAN = CHESTR(N1)*HH**(2.D0/3.D0)*SQRT(ABS(PENTE))
     &                                         *SIGN(1.D0,PENTE)
            ELSEIF (NKFROT(N1).EQ.4) THEN
              UTAN = HH**(2.D0/3.D0)*SQRT(ABS(PENTE))
     &                              *SIGN(1.D0,PENTE)/CHESTR(N1)
            ELSEIF (NKFROT(N1).EQ.5) THEN
              HH   = MAX(HH,1.D-9)
              CZ = MAX(1.D-9,LOG(11.D0*HH/MAX(CHESTR(N1),1.D-9))/KARMAN)
              UTAN = CZ*SQRT(HH*ABS(PENTE))*SIGN(1.D0,PENTE)
            ELSE
              WRITE(LU,*)'CLHUVT : UNKNOWN OPTION:',NKFROT(N1)
              WRITE(LU,*)'         FOR THE FRICTION LAW'
              CALL PLANTE(1)
              STOP
            ENDIF
!           TO GET ZERO TANGENTIAL VELOCITIES IN THE CORNERS
            UTAN = XX*UTAN
          ELSE
            WRITE(LU,*)'CLHUVT : UNKNOWN OPTION:',IOPTAN
            WRITE(LU,*)'         FOR THE TANGENTIAL VELOCITY'
            CALL PLANTE(1)
            STOP
          ENDIF
!
!         ONE CALCULATES VELOCITY COMPONENTS U AND V
!         IN THE ORDINARY COORDINATE SYSTEM (X,Y).
!
          UBOR(I1)=UTAN*TX+UNORM(I1)*NX
          VBOR(I1)=UTAN*TY+UNORM(I1)*NY
!
          ENDIF
!
        ENDDO ! I
      ENDDO ! K
!
!
!-----------------------------------------------------------------------
!
!  TYPES OF CONDITIONS FOR THE DEPTH AND THE VELOCITY:
!
      DO I=1,NPSING%I(N)
!
        I1=NDGA1%ADR(N)%P%I(I)
        IF(I1.GT.0) THEN
          LIHBOR(I1)=4
          LIUBOR(I1)=6
          LIVBOR(I1)=6
          UNORM1=UNORM(I1)
        ELSE
          UNORM1=0.D0
        ENDIF
!
        IF(NCSIZE.GT.1) THEN
          UNORM1 =P_MAX(UNORM1)+P_MIN(UNORM1)
        ENDIF
!
        I2=NDGB1%ADR(N)%P%I(I)
        IF(I2.GT.0) THEN
          LIHBOR(I2)=4
          LIUBOR(I2)=6
          LIVBOR(I2)=6
        ENDIF
!
!       CORRECTION: SOLID WALL TYPE IF NORMAL VELOCITY IS ZERO
!
        IF(ABS(UNORM1).LT.1.D-10) THEN
          IF(I1.GT.0) THEN
            LIHBOR(I1)=2
            LIUBOR(I1)=2
            LIVBOR(I1)=2
            IF(NTRAC.GT.0) THEN
              DO ITRAC=1,NTRAC
                LITBOR%ADR(ITRAC)%P%I(I1)=2
              ENDDO
            ENDIF
          ENDIF
          IF(I2.GT.0) THEN
            LIHBOR(I2)=2
            LIUBOR(I2)=2
            LIVBOR(I2)=2
            IF(NTRAC.GT.0) THEN
              DO ITRAC=1,NTRAC
                LITBOR%ADR(ITRAC)%P%I(I2)=2
              ENDDO
            ENDIF
          ENDIF
        ENDIF
!
        IF(NTRAC.GT.0) THEN
          IF(UNORM1.LT.-1.D-8) THEN
!           OUTGOING SPEED IN 1, ENTERING IN 2
            DO ITRAC=1,NTRAC
              IF(I1.GT.0) THEN
                LITBOR%ADR(ITRAC)%P%I(I1)=4
                T1=T%ADR(ITRAC)%P%R(NBOR(I1))
              ELSE
                T1=0.D0
              ENDIF
              IF(NCSIZE.GT.1) THEN
                T1=P_MAX(T1)+P_MIN(T1)
              ENDIF
              IF(I2.GT.0) THEN
                LITBOR%ADR(ITRAC)%P%I(I2)=5
                TBOR%ADR(ITRAC)%P%R(I2)=T1
              ENDIF
            ENDDO
          ELSEIF(UNORM1.GT.1.D-8) THEN
!           OUTGOING SPEED IN 2, ENTERING IN 1
            DO ITRAC=1,NTRAC
              IF(I2.GT.0) THEN
                LITBOR%ADR(ITRAC)%P%I(I2)=4
                T2=T%ADR(ITRAC)%P%R(NBOR(I2))
              ELSE
                T2=0.D0
              ENDIF
              IF(NCSIZE.GT.1) THEN
                T2=P_MAX(T2)+P_MIN(T2)
              ENDIF
              IF(I1.GT.0) THEN
                LITBOR%ADR(ITRAC)%P%I(I1)=5
                TBOR%ADR(ITRAC)%P%R(I1)=T2
              ENDIF
            ENDDO
          ELSE
!           ZERO VELOCITY
            DO ITRAC=1,NTRAC
              IF(I1.GT.0) LITBOR%ADR(ITRAC)%P%I(I1)=2
              IF(I2.GT.0) LITBOR%ADR(ITRAC)%P%I(I2)=2
            ENDDO
          ENDIF
        ENDIF
!
      ENDDO ! I
!
!-----------------------------------------------------------------------
!
! END OF THE LOOP ON THE CREST
!
      ENDDO ! N
!
!-----------------------------------------------------------------------
!
      RETURN
      END
