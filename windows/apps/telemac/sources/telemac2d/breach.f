!                       ******************
                        SUBROUTINE BREACH
!                       ******************
!
!
!***********************************************************************
! TELEMAC2D   V8P3
!***********************************************************************
!
!brief    MODIFICATION OF THE BOTTOM TOPOGRAPHY FOR BREACHES
!
!
!history  P. CHASSE (CETMEF) / C. COULET (ARTELIA)
!+        03/08/2012
!+        V6P2
!+        Creation
!
!history  Y.B. TADESSE (TUHH, INSTITUTE OF RIVER AND COASTAL ENGINEERING)
!+        14/02/2014
!+        V6P3R2
!+   Addition of later breach growth option
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM,P_MAX,P_MIN,
     &                               P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER I, J, K, N, M,END1, END2,CURNBR
      INTEGER ISTAT,VECZ,B
      INTEGER TEMPND(NPOIN)
      DOUBLE PRECISION ZC, ZW, ZB, CS2
      DOUBLE PRECISION AT1, AT2, AT3,DG,UCC,DHH,MAXDH
!
      DOUBLE PRECISION X1, X2, Y1, Y2, DX, DY,DS1,DS2,END1X,END1Y
      DOUBLE PRECISION U1, U2, V1, V2, DELS,CURDIS,DIS,END2X,END2Y
      DOUBLE PRECISION DZ, DPP,PI,TBR,BITA, BITA1
      DOUBLE PRECISION, DIMENSION (:), ALLOCATABLE :: XL, YL, XP, YP
      DOUBLE PRECISION , DIMENSION (:), ALLOCATABLE :: HAA,HVV,WDA,WDV
      DOUBLE PRECISION DEUXG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      PI = 4.D0*ATAN(1.D0)
      DEUXG = 2.D0*GRAV
!
      IF(.NOT.DEJALU_BREACH) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING LECBREACH'
        CALL LECBREACH(T2D_FILES(T2DBRC)%LU)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM LECBREACH'
        DEJALU_BREACH=.TRUE.
        WRITE (LU,*) 'READING BREACH DATA = OK'
!
        DO I = 1, NBRECH
          ZCRBR%R(I) = -HUGE(100.D0)
          DO J = 1, NBNDBR%I(I)
            K = INDBR%ADR(I)%P%I(J)
            IF(ZF%R(K).GT.ZCRBR%R(I)) THEN
              ZCRBR%R(I) = ZF%R(K)
            ENDIF
          ENDDO
          IF(NCSIZE.GT.1) THEN
            ZCRBR%R(I) = P_MAX(ZCRBR%R(I))
          ENDIF
        ENDDO
      ENDIF
!
      DO I = 1, NBRECH
        IF(OPTERO%I(I).EQ.1) THEN
          IF((OPTNBR%I(I).EQ.2).AND.(TDECBR%R(I).LT.0.D0)) THEN
            ZC = 0.D0
            N = 0
            DO J = 1, NBNDBR%I(I)
              K = INDBR%ADR(I)%P%I(J)
              IF(H%R(K).GT.0.D0) THEN
                ZW = ZF%R(K)+H%R(K)
                ZC = ZC + ZW
                N = N + 1
              ENDIF
            ENDDO
            IF(NCSIZE.GT.1) THEN
              N = P_SUM(N)
              ZC = P_SUM(ZC)
            ENDIF
            IF(N.GT.1) ZC = ZC/N
            IF(ZC.GT.ZDECBR%R(I)) THEN
              WRITE(LU,20) I, AT
              TDECBR%R(I) = AT
            ENDIF
          ELSEIF((OPTNBR%I(I).EQ.3).AND.(TDECBR%R(I).LT.0.D0)) THEN
            IF(NUMPSD%I(I).GT.0) THEN
              ZW = ZF%R(NUMPSD%I(I)) + H%R(NUMPSD%I(I))
            ELSE
              ZW = 0.D0
            ENDIF
!           CASE WHERE ONE OF THE ENDS IS NOT IN THE SUB-DOMAIN
            IF(NCSIZE.GT.1) THEN
              ZW = P_MAX(ZW)+P_MIN(ZW)
            ENDIF
            IF(ZW.GT.ZDECBR%R(I)) THEN
              WRITE(LU,20) I, AT
              TDECBR%R(I) = AT
            ENDIF
          ENDIF
          AT1 = TDECBR%R(I)
          AT2 = AT1 + DURBR%R(I)
          IF(AT1.GE.0.D0) THEN
            IF(AT.GT.AT1) THEN
              IF(AT.GT.AT2) THEN
                ZB = ZFINBR%R(I)
              ELSE
                ZB = ZCRBR%R(I)+(ZFINBR%R(I)-ZCRBR%R(I))/(AT2-AT1)
     &             *(AT-AT1)
              ENDIF
              DO J = 1, NBNDBR%I(I)
                K = INDBR%ADR(I)%P%I(J)
                ZF%R(K)=MIN(ZF%R(K), ZB)
              ENDDO
            ENDIF
          ENDIF
!       CASE 2 FOR LATERAL EROSION
        ELSEIF(OPTERO%I(I).GE.2) THEN
!       FIND NUMBER POINTS IN THE CURRENT BREACH REGION
          CURDIS=0.D0
          END1X=0.D0
          END1Y=0.D0
          END2X=0.D0
          END2Y=0.D0
          END1=0
          END2=0
!
!         NPONBR: NUMBER OF POINTS IN THE TXT FILE
          FIRSTEND: DO M=1,NPONBR%I(I)-1
            CURDIS=CURDIS+PONDSB%ADR(I)%P%R(M)
            IF(CURDIS .GT. ((FINBRW%R(I)-CURBRW%R(I))*0.5D0)) THEN
              END1=M+1
              DS1= CURDIS - (FINBRW%R(I)-CURBRW%R(I))*0.5D0
              DX = DKAXCR%ADR(I)%P%R(M+1) - DKAXCR%ADR(I)%P%R(M)
              DY = DKAYCR%ADR(I)%P%R(M+1) - DKAYCR%ADR(I)%P%R(M)
              U1 = DX/PONDSB%ADR(I)%P%R(M)
              U2 = DY/PONDSB%ADR(I)%P%R(M)
              IF(CURBRW%R(I).LT.FINBRW%R(I)) THEN
                END1X = DKAXCR%ADR(I)%P%R(M+1) - U1*DS1
                END1Y = DKAYCR%ADR(I)%P%R(M+1) - U2*DS1
              ELSEIF(CURBRW%R(I).GE.FINBRW%R(I)) THEN
                CURBRW%R(I) = FINBRW%R(I)
                END1X = DKAXCR%ADR(I)%P%R(1)- U1*
     &          (PONDSB%ADR(I)%P%R(1)*0.1D0)
                END1Y = DKAYCR%ADR(I)%P%R(1)- U2*
     &          (PONDSB%ADR(I)%P%R(1)*0.1D0)
              ELSE
                WRITE(LU,400)
                CALL PLANTE(1)
                STOP
              ENDIF
              DIS=DS1
              SECONDEND: DO J=M+1,NPONBR%I(I)-1
                DIS=DIS+PONDSB%ADR(I)%P%R(J)
                IF(DIS .GE. CURBRW%R(I)) THEN
                  END2=J
                  DS2=DIS - CURBRW%R(I)
                  DX = DKAXCR%ADR(I)%P%R(J+1) - DKAXCR%ADR(I)%P%R(J)
                  DY = DKAYCR%ADR(I)%P%R(J+1) - DKAYCR%ADR(I)%P%R(J)
                  U1 = DX/PONDSB%ADR(I)%P%R(J)
                  U2 = DY/PONDSB%ADR(I)%P%R(J)
                  IF(CURBRW%R(I).LT.FINBRW%R(I)) THEN
                    END2X = DKAXCR%ADR(I)%P%R(J+1) - U1*DS2
                    END2Y = DKAYCR%ADR(I)%P%R(J+1) - U2*DS2
                  ELSEIF(CURBRW%R(I).GE.FINBRW%R(I)) THEN
                    CURBRW%R(I) = FINBRW%R(I)
                    END2X = DKAXCR%ADR(I)%P%R(NPONBR%I(I))+ U1*
     &              (PONDSB%ADR(I)%P%R(NPONBR%I(I)-1)*0.1D0)
                    END2Y = DKAYCR%ADR(I)%P%R(NPONBR%I(I))+ U2*
     &              (PONDSB%ADR(I)%P%R(NPONBR%I(I)-1)*0.1D0)
                  ELSE
                    WRITE(LU,400) I
                    CALL PLANTE(1)
                    STOP
                  ENDIF
                  EXIT SECONDEND
                ENDIF
              ENDDO SECONDEND
!
              EXIT FIRSTEND
            ENDIF
          ENDDO FIRSTEND
400       FORMAT(1X, 'BREACH: BREACH WIDTH CANNOT BE GREATER ',
     &           'THAN FINAL WIDTH FOR BREACH: ', 1I6)
!         ALLOCATION OF LOCAL VARIABLE FOR BREACH DEFINITION
          ISTAT = 0
          VECZ=END2-END1+3
          ALLOCATE(XL(VECZ), STAT=ISTAT)
          IF(ISTAT.NE.0) THEN
            WRITE(LU,200) ISTAT
            CALL PLANTE(1)
            STOP
          ENDIF
          ALLOCATE(YL(VECZ), STAT=ISTAT)
          IF(ISTAT.NE.0) THEN
            WRITE(LU,200) ISTAT
            CALL PLANTE(1)
            STOP
          ENDIF
!
200       FORMAT(1X,'ERROR DURING ALLOCATION OF VECTOR: ',
     &           'ERROR CODE: ',1I6)
!
          XL(1)=END1X
          YL(1)=END1Y
          XL(VECZ)=END2X
          YL(VECZ)=END2Y
          DO J=2,VECZ-1
            XL(J)=DKAXCR%ADR(I)%P%R(END1+J-2)
            YL(J)=DKAYCR%ADR(I)%P%R(END1+J-2)
          ENDDO
!         SEARCH MESH POINTS INSIDE THE BREACH DOMAIN
          ISTAT = 0
          ALLOCATE(XP(2*VECZ), STAT=ISTAT)
          IF(ISTAT.NE.0) THEN
            WRITE(LU,200) ISTAT
            CALL PLANTE(1)
            STOP
          ENDIF
          ALLOCATE(YP(2*VECZ), STAT=ISTAT)
          IF(ISTAT.NE.0) THEN
            WRITE(LU,200) ISTAT
            CALL PLANTE(1)
            STOP
          ENDIF
!
          X1 = XL(1)
          Y1 = YL(1)
          X2 = XL(2)
          Y2 = YL(2)
          DX = X2 - X1
          DY = Y2 - Y1
          DELS=SQRT(DX*DX+DY*DY)
          IF(DELS.GE.0.D0) THEN
            U1 = DX/DELS
            U2 = DY/DELS
          ELSE
            WRITE(LU,*) 'PROBLEM IN DEFINITION OF BREACH :',I
            CALL PLANTE(1)
            STOP
          ENDIF
          V1 = -U2
          V2 = U1
          XP(1)      = X1 + V1*POLWDT%R(I)*0.5D0
          YP(1)      = Y1 + V2*POLWDT%R(I)*0.5D0
          XP(2*VECZ) = X1 - V1*POLWDT%R(I)*0.5D0
          YP(2*VECZ) = Y1 - V2*POLWDT%R(I)*0.5D0
!
          DO M = 2,VECZ
            X2 = XL(M)
            Y2 = YL(M)
            DX = X2 - X1
            DY = Y2 - Y1
            DELS=SQRT(DX*DX+DY*DY)
            IF(DELS.GE.0.D0) THEN
              U1 = DX/DELS
              U2 = DY/DELS
            ELSE
              WRITE(LU,*) 'PROBLEM IN DEFINITION OF BREACH :',I
              CALL PLANTE(1)
              STOP
            ENDIF
            V1 = -U2
            V2 = U1
            XP(M)         = X2 + V1*POLWDT%R(I)*0.5D0
            YP(M)         = Y2 + V2*POLWDT%R(I)*0.5D0
            XP(2*VECZ-M+1) = X2 - V1*POLWDT%R(I)*0.5D0
            YP(2*VECZ-M+1) = Y2 - V2*POLWDT%R(I)*0.5D0
            X1=X2
            Y1=Y2
          ENDDO
!         CURNBR: NUMBER OF NODES INSIDE THE BREACH DOMAIN
!         TEMPND: POINTS NUMBERS INSIDE POLYGON DEFINING THE BREACH
          CURNBR = 0
          DO M = 1, NPOIN
            IF(INPOLY(MESH%X%R(M), MESH%Y%R(M), XP, YP, 2*VECZ)) THEN
              CURNBR = CURNBR +1
              TEMPND(CURNBR) = M
            ENDIF
          ENDDO
!
          ALLOCATE(HAA(NBLS(I)))
          ALLOCATE(HVV(NBLS(I)))
          ALLOCATE(WDA(NBLS(I)))
          ALLOCATE(WDV(NBLS(I)))
!         DIKE3 CONTAINS THE SAME LIST OF POINTS OF DIKE2 BUT IN
!         OPPOSITE ORDER
          DO J = 1,NBLS(I)
            DIKE3(I,J) = DIKE2(I,NBLS(I)-J+1)
          ENDDO
          IF((OPTNBR%I(I).EQ.2).AND.(TDECBR%R(I).LT.0.D0)) THEN
            ZC = 0.D0
            N = 0
            DO J = 1, CURNBR
              K = TEMPND(J)
              IF(H%R(K).GT.0.D0) THEN
                ZW = ZF%R(K)+H%R(K)
                ZC = ZC + ZW
                N = N + 1
              ENDIF
            ENDDO
            IF(NCSIZE.GT.1) THEN
              N = P_SUM(N)
              ZC = P_SUM(ZC)
            ENDIF
            IF(N.GT.1) ZC = ZC/N
            IF(ZC.GT.ZDECBR%R(I)) THEN
              WRITE(LU,20) I, AT
              TDECBR%R(I) = AT
            ENDIF
          ENDIF
          IF((OPTNBR%I(I).EQ.3).AND.(TDECBR%R(I).LT.0.D0)) THEN
            IF(NUMPSD%I(I).GT.0) THEN
              ZW = ZF%R(NUMPSD%I(I)) + H%R(NUMPSD%I(I))
            ELSE
              ZW = 0.D0
            ENDIF
!           CASE WHERE ONE OF THE ENDS IS NOT IN THE SUB-DOMAIN
            IF(NCSIZE.GT.1) THEN
              ZW = P_MAX(ZW)+P_MIN(ZW)
            ENDIF
            IF(ZW.GT.ZDECBR%R(I)) THEN
              WRITE(LU,20) I, AT
              TDECBR%R(I) = AT
            ENDIF
          ENDIF
!
!         ASSUMPTION: DURATION FOR THE VERTICAL EROSION IS TAKEN AS A TENTH
!         OF THE TOTAL BREACH DURATION
          AT1 = TDECBR%R(I)
          AT2 = AT1 + DURBR%R(I)
          AT3 = AT1 + DURBR%R(I)*0.1D0
!
          DPP = 0.D0
          DZ = 0.D0
!
!         VELOCITY EXPANSION
!
          IF(AT1.GE.0.D0) THEN
            IF(AT.GT.AT1) THEN
!
!             VERTICAL EXPANSION (ZB)
!
              IF(AT.GT.AT3) THEN
                ZB = ZFINBR%R(I)
              ELSE
                ZB = ZCRBR%R(I)+(ZFINBR%R(I)-ZCRBR%R(I))/(AT3-AT1)
     &               *(AT-AT1)
              ENDIF
!
!             DIFFERENT FORMULA FOR LATERAL EXPANSION (CURBRW)
!             USER'S KINETICS: ONE STAGE
!
              IF(OPTERO%I(I).EQ.2) THEN
                IF(AT.GT.AT2) THEN
                  CURBRW%R(I)=FINBRW%R(I)
                ELSE
                  CURBRW%R(I) = INIBRW%R(I)+(FINBRW%R(I)-INIBRW%R(I))
     &            /(AT2-AT1)*(AT-AT1)
                ENDIF
!
!             USER'S KINETICS: TWO STAGES
!
              ELSEIF(OPTERO%I(I).EQ.3) THEN
                IF((AT-AT1).LE.FT1(I)) THEN
                  CURBRW%R(I) = CURBRW%R(I)+((DT/3600.D0)*VELS1(I))
                ELSEIF((AT-AT1).GT.FT1(I).AND.AT.LE.AT2) THEN
                  CURBRW%R(I) = CURBRW%R(I)+((DT/3600.D0)*VELS2(I))
                ENDIF
!
!             USBR FORMULA
!
              ELSEIF(OPTERO%I(I).EQ.4) THEN
                CURBRW%R(I) = 91.D0*((AT-AT1)/3600.D0) + INIBRW%R(I)
!
!             VON THUN & GILLETTE 1990 FORMULA
!
              ELSEIF(OPTERO%I(I).EQ.5) THEN
!               NON COHESIVE DIKE
                CURBRW%R(I) = (4.D0*(ZCRBR%R(I)-ZFINBR%R(I))+61.D0)
     &            *((AT-AT1)/3600.D0) + INIBRW%R(I)
!
              ELSEIF(OPTERO%I(I).EQ.6) THEN
!               COHESIVE DIKE
                CURBRW%R(I) = 4.D0*(ZCRBR%R(I)-ZFINBR%R(I))
     &            *((AT-AT1)/3600.D0) + INIBRW%R(I)

!             VERHEIJ 2002 FORMULA
!
              ELSEIF(OPTERO%I(I).EQ.7) THEN
!               NON COHESIVE DIKE
                CURBRW%R(I) = 37.2D0*(((AT-AT1)/3600.D0)**0.51D0)
     &            + INIBRW%R(I)
!
              ELSEIF(OPTERO%I(I).EQ.8) THEN
!               COHESIVE DIKE
                CURBRW%R(I) = 13.4D0*SQRT((AT-AT1)/3600.D0)
     &            + INIBRW%R(I)
!
!             FORMULE VERHEIJ 2003
              ELSEIF(OPTERO%I(I).EQ.9) THEN
!                TO DECIDE: USING DHH OR MAXDH?
!                DHH = 0.D0
                MAXDH = 0.D0
!
                DO B = 1, NBLS(I)
                  HAA(B) = 0.D0
                  HVV(B) = 0.D0
                  WDA(B) = 0.D0
                  WDV(B) = 0.D0
                ENDDO
!
                IF(NBLS(I).EQ.0) THEN
                  WRITE(LU,*) 'NUMBER OF BREACH POINTS NOT CORRECT'
                  CALL PLANTE(1)
                  STOP
                ENDIF
!
                DO B = 1, NBLS(I)
                  J = DIKE1(I,B)
                  ! To handle points at interface IFAC = 1 on only one
                  ! subdomain
                  IF(J.NE.0.AND.NCSIZE.GT.1) J = J*MESH%IFAC%I(J)
                  IF(J.NE.0) THEN
!                   HYDRAULIC HEAD
                    HAA(B) = ZF%R(J)+H%R(J)+
     &                     ((U%R(J)**2 + V%R(J)**2)/DEUXG)
                    WDA(B) = H%R(J)
                  ENDIF
                  J = DIKE3(I,B)
                  ! To handle points at interface IFAC = 1 on only one
                  ! subdomain
                  IF(J.NE.0.AND.NCSIZE.GT.1) J = J*MESH%IFAC%I(J)
                  IF(J.NE.0) THEN
!                   HYDRAULIC HEAD
                    HVV(B) = ZF%R(J)+H%R(J)+
     &                      ((U%R(J)**2 + V%R(J)**2)/DEUXG)
                    WDV(B) = H%R(J)
                  ENDIF
                  IF(NCSIZE.GT.1) THEN
                    HAA(B) = P_SUM(HAA(B))
                    HVV(B) = P_SUM(HVV(B))
                    WDA(B) = P_SUM(WDA(B))
                    WDV(B) = P_SUM(WDV(B))
                  ENDIF
!
!                 TO COMPUTE THE MAX OR AVERAGE OF HYDRAULIC HEAD,
!                 WE NEED TO KNOW ON WHICH SIDE OF THE BREACH THE
!                 WATER DEPTH IS LARGER (ON THE FLOODPLAIN SIDE OR
!                 ON THE MAIN STREAM SIDE)
                  IF(WDA(B).GE.WDV(B)) THEN
!                   MAXIMUM VALUE OF HYDRAULIC HEAD
                    IF((HAA(B)-HVV(B)).GT.MAXDH) THEN
                      MAXDH = HAA(B)-HVV(B)
                    ENDIF
                  ELSE
                    IF((HVV(B)-HAA(B)).GT.MAXDH) THEN
                      MAXDH = HVV(B)-HAA(B)
                    ENDIF
                  ENDIF
                ENDDO
!               TO DO: DELETE THIS PART IF MAXDH IS USED OR KEEP THIS
!               PART IF DHH IS USED
!               AVERAGE VALUE OF HYDRAULIC HEAD
!                IF(NBLS(I).GT.0) THEN
!                  IF(SUM(WDA).GE.SUM(WDV)) THEN
!                    DHH = (SUM(HAA)-SUM(HVV))/NBLS(I)
!                  ELSE
!                    DHH = (SUM(HVV)-SUM(HAA))/NBLS(I)
!                  ENDIF
!                ENDIF
!
!                UCC = 2.3026D0*(UC(I)**2) !ln(10)*uc**2
                UCC = LOG(10.D0)*UC(I)**2 !ln(10)*uc**2
                DG = (DT/3600.D0)*SQRT((GRAV*MAXDH)**3)
                CS2 = (F2(I)*GRAV*(AT-AT1))/(3600.D0*UC(I))
!
!               THIS IS NOT THE INTEGRAL FORM BUT THE DIFFERENTIAL FORM
                CURBRW%R(I) = CURBRW%R(I) + (F1(I)*F2(I)/UCC)*DG/
     &                       (1.D0+CS2)
!
!             FROEHLICH FORMULA 2008 (ADAPTATION 2D)
!
              ELSEIF(OPTERO%I(I).EQ.10) THEN
!
!               FINAL BREACH DEPTH

                TBR = (AT-AT1)/(AT2-AT1)
!
!               DEEPENING EXPANSION
!
                BITA1 = 0.5D0*(1.D0+SIN(PI*((AT-AT1)/(AT3-AT1)
     &                 -0.5D0)))
!
!               LATERAL EXPANSION
!
                IF(AT.GE.AT2) THEN
                  CURBRW%R(I) = FINBRW%R(I)
                  BITA = 1.D0
                ELSE
                  BITA = 0.5D0*(1.D0+SIN(PI*(TBR-0.5D0)))
                  CURBRW%R(I) = BITA*(FINBRW%R(I)-INIBRW%R(I))
     &                        +INIBRW%R(I)
                ENDIF
                DPP = MIN(BITA1*DF(I),DF(I))
!               DZ: DEEPENING CUMULATED OVER TIME
                DZ = DPP-DEPTHN%R(I)
                DEPTHN%R(I) = DPP
!
              ENDIF
!
              IF(CURBRW%R(I).GE.FINBRW%R(I)) THEN
                CURBRW%R(I) = FINBRW%R(I)
              ENDIF
              DO J = 1, CURNBR
                K = TEMPND(J)
                IF(OPTERO%I(I).LT.10) THEN
                  ZF%R(K) = MIN(ZF%R(K), ZB)
                ELSEIF(OPTERO%I(I).EQ.10) THEN
                  ZF%R(K) = ZF%R(K)-ABS(DZ)
                  IF(ZF%R(K).LE.ZFINBR%R(I)) THEN
                    ZF%R(K) = ZFINBR%R(I)
                  ENDIF
                ENDIF
              ENDDO
            ENDIF !ENDIF AT.GT.AT1
          ENDIF !ENDIF AT1.GT.0.D0
          DEALLOCATE(XL)
          DEALLOCATE(YL)
          DEALLOCATE(XP)
          DEALLOCATE(YP)
          DEALLOCATE(HVV)
          DEALLOCATE(HAA)
          DEALLOCATE(WDA)
          DEALLOCATE(WDV)
        ENDIF !ENDIF OPTERO.GE.2
      ENDDO
!
!-----------------------------------------------------------------------
!     MESSAGES
20    FORMAT(1X,'CREATION OF BREACH : ',I4,/,1X,
     &          'AT TIME : ',G16.7)
!-----------------------------------------------------------------------
!
      RETURN
      END
