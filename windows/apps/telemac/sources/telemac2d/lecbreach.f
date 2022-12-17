!                       ********************
                        SUBROUTINE LECBREACH
!                       ********************
!
     &(IFIC)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    READ THE BREACHES DATA FILE, ALLOCATE THE DEDICATED ARRAY
!+        AND IDENTIFY THE NODES
!
!
!history  P. CHASSE (CETMEF) / C.COULET (ARTELIA)
!+        03/08/2012
!+        V6P2
!+        Creation
!
!history  Y.B. TADESSE (TUHH, INSTITUTE OF RIVER AND COASTAL ENGINEERING)
!+        14/02/2014
!+        V6P3R2
!+   Addition of later breach growth option
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        01/07/2016
!+        V7P2
!+   ITMP now an allocatable array, not automatic array ITMP(NPOIN).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IFIC           |-->| LOGICAL UNIT OF BREACHES DATA FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      USE BIEF, EX_PROXIM => PROXIM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)    :: IFIC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N, M, NUM, ISTAT
      DOUBLE PRECISION X1, X2, Y1, Y2, DX, DY
      DOUBLE PRECISION U1, U2, V1, V2, DELS, MIDDIS
      DOUBLE PRECISION, DIMENSION (:), ALLOCATABLE :: XL, YL, XP, YP
      DOUBLE PRECISION, DIMENSION (:), ALLOCATABLE :: DS, XA, YA, XV, YV
!
      CHARACTER(LEN=6) :: NOM, NOMX, NOMY, NOMDS
      CHARACTER(LEN=1), PARAMETER :: CHIFFRE(0:9) =
     &           (/'0','1','2','3','4','5','6','7','8','9'/)
!
      INTEGER, ALLOCATABLE :: ITMP(:)
      CHARACTER(LEN=1) :: COMMENT = "#"
      INTEGER :: IERR
      INTEGER :: NBL
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ITMP: CONTAINS THE GLOBAL MESH POINT NUMBER OF NODES INSIDE
!     POLYGON DEFINING THE BREACH
      ALLOCATE(ITMP(NPOIN))
      ! First we read the breach file to identify the number of points
      ! per breach and fill NBLS
      CALL PRE_LECBREACH(IFIC)
      REWIND IFIC
!
      CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
      IF(IERR.NE.0) GOTO 900
      READ(IFIC,*,ERR=999) NBRECH
!
!     ALLOCATION OF SPECIFIC ARRAYS
      ALLOCATE(DIKE1(NBRECH, MAXVAL(NBLS)))
      ALLOCATE(DIKE2(NBRECH, MAXVAL(NBLS)))
      ALLOCATE(DIKE3(NBRECH, MAXVAL(NBLS)))
!
      ALLOCATE(FT1(NBRECH))
      ALLOCATE(VELS1(NBRECH))
      ALLOCATE(VELS2(NBRECH))
      ALLOCATE(UC(NBRECH))
      ALLOCATE(F1(NBRECH))
      ALLOCATE(F2(NBRECH))
      ALLOCATE(DF(NBRECH))
!
      IF(NBRECH.GT.0) THEN
        CALL BIEF_ALLVEC(2,OPTNBR,'OPTNBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(2,OPTERO,'OPTERO',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,TDECBR,'TDECBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,DURBR ,'DURBR ',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZFINBR,'ZFINBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZDECBR,'ZDECBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZCRBR ,'ZCRBR ',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,POLWDT,'POLWDT',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(2,NUMPSD,'NUMPSD',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(2,NBNDBR,'NBNDBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(2,NPONBR,'NPONBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,CURBRW,'CURBRW',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,FINBRW,'FINBRW',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,INIBRW,'INIBRW',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,DEPTHN,'DEPTHN',NBRECH,1,0,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,OPTNBR,'OPTNBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,OPTERO,'OPTERO',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,TDECBR,'TDECBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,DURBR ,'DURBR ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZFINBR,'ZFINBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZDECBR,'ZDECBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZCRBR ,'ZCRBR ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,POLWDT,'POLWDT',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,NUMPSD,'NUMPSD',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,NBNDBR,'NBNDBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,NPONBR,'NPONBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,CURBRW,'CURBRW',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,FINBRW,'FINBRW',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,INIBRW,'INIBRW',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,DEPTHN,'DEPTHN',0,1,0,MESH)
      ENDIF
      CALL ALLBLO(INDBR ,'INDBR ')
      CALL ALLBLO(DKAXCR,'DKAXCR')
      CALL ALLBLO(DKAYCR,'DKAYCR')
      CALL ALLBLO(PONDSB,'PONDSB')
!
!
      DO N = 1, NBRECH
        CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
        IF(IERR.NE.0) GOTO 900
        READ(IFIC,*,ERR=998) POLWDT%R(N)
        CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
        IF(IERR.NE.0) GOTO 900
        READ(IFIC,*,ERR=997) OPTNBR%I(N)
        CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
        IF(IERR.NE.0) GOTO 900
        IF(OPTNBR%I(N).EQ.1) THEN
          READ(IFIC,*,ERR=996) TDECBR%R(N)
          CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
          IF(IERR.NE.0) GOTO 900
        ELSE
          TDECBR%R(N) = -9999.D0
        ENDIF
        READ(IFIC,*,ERR=995) DURBR%R(N)
        CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
        IF(IERR.NE.0) GOTO 900
        READ(IFIC,*,ERR=810) OPTERO%I(N)
        CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
        IF(IERR.NE.0) GOTO 900
        READ(IFIC,*,ERR=994) ZFINBR%R(N)
        CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
        IF(IERR.NE.0) GOTO 900
        IF(OPTNBR%I(N).EQ.3) THEN
          READ(IFIC,*,ERR=993) NUMPSD%I(N)
          CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
          IF(IERR.NE.0) GOTO 900
          IF(NCSIZE.GT.1) THEN
            NUM = NUMPSD%I(N)
            NUMPSD%I(N) = 0
            DO M=1,MESH%NPOIN
              IF(NUM.EQ.MESH%KNOLG%I(M)) THEN
                NUMPSD%I(N) = M
              ENDIF
            ENDDO
          ENDIF
        ENDIF
        IF(OPTNBR%I(N).NE.1) THEN
          READ(IFIC,*,ERR=992) ZDECBR%R(N)
          CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
          IF(IERR.NE.0) GOTO 900
        ENDIF
        IF(OPTERO%I(N).EQ.3) THEN
          READ(IFIC,*,ERR=801) FT1(N),VELS1(N),VELS2(N)
          CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
          IF(IERR.NE.0) GOTO 900
        ENDIF
!
        IF(OPTERO%I(N).EQ.9) THEN
          READ(IFIC,*,ERR=802) UC(N),F1(N),F2(N)
          CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
          IF(IERR.NE.0) GOTO 900
        ENDIF
!
        IF(OPTERO%I(N).EQ.10) THEN
          READ(IFIC,*,ERR=803) DF(N)
          CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
          IF(IERR.NE.0) GOTO 900
        ENDIF
!
!       INITIAL LENGTH OF BREACH
        IF(LOGINB.EQV..TRUE.) THEN
          READ(IFIC,*,ERR=804) INIBRW%R(N)
          CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
          IF(IERR.NE.0) GOTO 900
        ENDIF
!
!       NBL: NUMBER OF POINTS OF THE POLYLINE
        READ(IFIC,*,ERR=991) NBL
        CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
        IF(IERR.NE.0) GOTO 900
!
!       ALLOCATION OF LOCAL VARIABLE TO READ BREACH DEFINITION
        ISTAT = 0
        ALLOCATE(XL(NBL), STAT=ISTAT)
        CALL CHECK_ALLOCATE(ISTAT, "XL")
        ALLOCATE(YL(NBL), STAT=ISTAT)
        CALL CHECK_ALLOCATE(ISTAT, "YL")
        ALLOCATE(DS(NBL-1), STAT=ISTAT)
        CALL CHECK_ALLOCATE(ISTAT, "DS")
!
!
!       INITIALIZE DIKE1 AND DIKE2
        DO M = 1, NBL
          DIKE1(N,M)=0
          DIKE2(N,M)=0
          DIKE3(N,M)=0
        ENDDO
!
        DO M = 1, NBL
          READ(IFIC,*,ERR=990) XL(M), YL(M)
        ENDDO
!
!       SEARCH MESH POINTS INSIDE THE BREACH DOMAIN
!
        ISTAT = 0
!       XP AND YP: COORDINATES OF POINTS LOCATED ALONG THE POLYGON
!       DEFINING THE BREACH.
!       FOR EACH (XL,YL) WE FIND (XP,YP) ON THE NORMAL OF THE AXE
!       OF THE DIKE, LOCATED TO +POLWDT/2 AND - POLWDT/2
        ALLOCATE(XP(2*NBL), STAT=ISTAT)
        CALL CHECK_ALLOCATE(ISTAT, "XP")
        ALLOCATE(YP(2*NBL), STAT=ISTAT)
        CALL CHECK_ALLOCATE(ISTAT, "YP")
!
        X1 = XL(1)
        Y1 = YL(1)
        X2 = XL(2)
        Y2 = YL(2)
        DX = X2 - X1
        DY = Y2 - Y1
        DELS=SQRT(DX*DX+DY*DY)
        IF(DELS.GT.0.D0) THEN
          U1 = DX/DELS
          U2 = DY/DELS
        ELSE
          WRITE(LU,*) 'PROBLEM IN DEFINITION OF BREACH :',N
          CALL PLANTE(1)
        ENDIF
        V1 = -U2
        V2 = U1
        XP(1)     = X1 + V1*POLWDT%R(N)/2.D0
        YP(1)     = Y1 + V2*POLWDT%R(N)/2.D0
        XP(2*NBL) = X1 - V1*POLWDT%R(N)/2.D0
        YP(2*NBL) = Y1 - V2*POLWDT%R(N)/2.D0
!
        DO M = 2,NBL
          X2 = XL(M)
          Y2 = YL(M)
          DX = X2 - X1
          DY = Y2 - Y1
          DS(M-1)=SQRT(DX*DX+DY*DY)
          IF(DS(M-1).GT.0.D0) THEN
            U1 = DX/DS(M-1)
            U2 = DY/DS(M-1)
          ELSE
            WRITE(LU,*) 'PROBLEM IN DEFINITION OF BREACH :',N
            CALL PLANTE(1)
          ENDIF
          V1 = -U2
          V2 = U1
          XP(M)         = X2 + V1*POLWDT%R(N)/2.D0
          YP(M)         = Y2 + V2*POLWDT%R(N)/2.D0
          XP(2*NBL-M+1) = X2 - V1*POLWDT%R(N)/2.D0
          YP(2*NBL-M+1) = Y2 - V2*POLWDT%R(N)/2.D0
          X1=X2
          Y1=Y2
        ENDDO
!       XA,YA: COORDINATES OF POINTS ON ONE SIDE OF BREACH
!       XV,YV: COORDINATES OF POINTS ON OTHER SIDE OF BREACH
!       NOTE THAT THEY ARE NOT STORED WITH THE SAME ORDER
        XA = XP(NBL+1:2*NBL)
        YA = YP(NBL+1:2*NBL)
        XV = XP(1:NBL)
        YV = YP(1:NBL)
!
!       MESH POINTS IN THE BREACH DOMAIN
        NBNDBR%I(N) = 0
        DO M = 1, NPOIN
          IF(INPOLY(MESH%X%R(M), MESH%Y%R(M), XP, YP, 2*NBL)) THEN
            NBNDBR%I(N) = NBNDBR%I(N)+1
            ITMP(NBNDBR%I(N)) = M
          ENDIF
        ENDDO
!
!       LOOKING FOR THE MESH POINTS (DIKE1) CLOSEST TO SET OF POINTS XA,YA
!
        CALL PROXIM(DIKE1(N,1:NBL), XA, YA, MESH%X%R, MESH%Y%R, NBL,
     &         NPOIN,MESH%IKLE%I,NELEM,NELMAX)
!
!       LOOKING FOR THE MESH POINTS (DIKE2) CLOSEST TO SET OF POINTS XV,YV
!
        CALL PROXIM(DIKE2(N,1:NBL), XV, YV, MESH%X%R, MESH%Y%R, NBL,
     &         NPOIN,MESH%IKLE%I,NELEM,NELMAX)
!
        IF(N.LE.INDBR%MAXBLOCK) THEN
          NOM='NBR   '
          IF(N.LT.10) THEN
            NOM(4:4) = CHIFFRE(N)
          ELSEIF(N.LT.100) THEN
            NOM(4:4) = CHIFFRE(N/10)
            NOM(5:5) = CHIFFRE(N-10*(N/10))
          ELSEIF(N.LT.1000) THEN
            NOM(4:4) = CHIFFRE(N/100)
            NOM(5:5) = CHIFFRE((N-100*(N/100))/10)
            NOM(6:6) = CHIFFRE((N-100*(N/100))-10*((N-100*(N/100))/10))
          ELSE
            WRITE(LU,*) 'MORE THAN 999 BREACHS ASKED IN LECBREACH'
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL FIRST_ALL_BIEFOBJ(INDBR%ADR(N)%P)
          CALL BIEF_ALLVEC(2,INDBR%ADR(N)%P,NOM,NBNDBR%I(N),1,0,MESH)
          INDBR%ADR(N)%P%FATHER = INDBR%NAME
        ELSE
          WRITE(LU,*) 'LECBREACH:'
          WRITE(LU,*) 'MORE THAN ',INDBR%MAXBLOCK,'(',N,')'
          WRITE(LU,*) 'VECTORS TO BE ALLOCATED'
          WRITE(LU,*) 'CHANGE MAXBLOCK IN ALLBLO.'
          CALL PLANTE(1)
          STOP
        ENDIF
!
        DO M=1, NBNDBR%I(N)
          INDBR%ADR(N)%P%I(M) = ITMP(M)
        ENDDO
!
!       OPTION FOR LATERAL EROSION
!
        IF (OPTERO%I(N).GE.2) THEN
          IF(N.LE.DKAXCR%MAXBLOCK) THEN
            NOMX='XCB   '
            NOMY='YCB   '
            NOMDS='PDS   '
            IF(N.LT.10) THEN
              NOMX(4:4) = CHIFFRE(N)
              NOMY(4:4) = CHIFFRE(N)
              NOMDS(4:4) = CHIFFRE(N)
            ELSEIF(N.LT.100) THEN
              NOMX(4:4) = CHIFFRE(N/10)
              NOMX(5:5) = CHIFFRE(N-10*(N/10))
              NOMY(4:4) = CHIFFRE(N/10)
              NOMY(5:5) = CHIFFRE(N-10*(N/10))
              NOMDS(4:4) = CHIFFRE(N/10)
              NOMDS(5:5) = CHIFFRE(N-10*(N/10))
            ELSEIF(N.LT.1000) THEN
              NOMX(4:4) = CHIFFRE(N/100)
              NOMX(5:5) = CHIFFRE((N-100*(N/100))/10)
              NOMX(6:6) =
     &          CHIFFRE((N-100*(N/100))-10*((N-100*(N/100))/10))
              NOMY(4:4) = CHIFFRE(N/100)
              NOMY(5:5) = CHIFFRE((N-100*(N/100))/10)
              NOMY(6:6) =
     &          CHIFFRE((N-100*(N/100))-10*((N-100*(N/100))/10))
              NOMDS(4:4) = CHIFFRE(N/100)
              NOMDS(5:5) = CHIFFRE((N-100*(N/100))/10)
              NOMDS(6:6) =
     &          CHIFFRE((N-100*(N/100))-10*((N-100*(N/100))/10))
            ELSE
                WRITE(LU,*) 'MORE THAN 999 BREACHS ASKED IN LECBREACH'
              CALL PLANTE(1)
              STOP
            ENDIF
            CALL FIRST_ALL_BIEFOBJ(DKAXCR%ADR(N)%P)
            CALL FIRST_ALL_BIEFOBJ(DKAYCR%ADR(N)%P)
            CALL FIRST_ALL_BIEFOBJ(PONDSB%ADR(N)%P)
            CALL BIEF_ALLVEC(1,DKAXCR%ADR(N)%P,NOMX,NBL,1,0,MESH)
            CALL BIEF_ALLVEC(1,DKAYCR%ADR(N)%P,NOMY,NBL,1,0,MESH)
            CALL BIEF_ALLVEC(1,PONDSB%ADR(N)%P,NOMDS,NBL-1,1,0,MESH)
            DKAXCR%ADR(N)%P%FATHER = DKAXCR%NAME
            DKAYCR%ADR(N)%P%FATHER = DKAYCR%NAME
            PONDSB%ADR(N)%P%FATHER = PONDSB%NAME
          ELSE
            WRITE(LU,*) 'LECBREACH:'
            WRITE(LU,*) 'MORE THAN ',DKAXCR%MAXBLOCK,'(',N,')'
            WRITE(LU,*) 'VECTORS TO BE ALLOCATED'
            WRITE(LU,*) 'CHANGE MAXBLOCK IN ALLBLO.'
            CALL PLANTE(1)
            STOP
          ENDIF
!         COMPUTATION OF FINAL BREACH WIDTH: EQUALS TO THE LENGTH OF THE
!         BREACH POLYLINE (SUM OF THE DISTANCE BETWEEN POINTS)
          FINBRW%R(N)=0.D0
          DO M=1,NBL
            DKAXCR%ADR(N)%P%R(M)=XL(M)
            DKAYCR%ADR(N)%P%R(M)=YL(M)
            IF (M .GT.1) THEN
              PONDSB%ADR(N)%P%R(M-1)=DS(M-1)
              FINBRW%R(N)=FINBRW%R(N)+DS(M-1)
            ENDIF
          ENDDO
          NPONBR%I(N)=NBL
          MIDDIS=0.D0
          CURBRW%R(N)=0.D0
          IF(.NOT.LOGINB) THEN
            INIBRW%R(N)=FINBRW%R(N)/10.D0
          ENDIF
          DEPTHN%R(N)=0.D0
          FINDMIDNODE: DO M=2,NBL
            MIDDIS=MIDDIS+DS(M-1)
            IF (MIDDIS .GE. (FINBRW%R(N)/2.D0)) THEN
              IF (M .GT. 2) THEN
                IF(.NOT.LOGINB) THEN
                  INIBRW%R(N)=MAX(DS(M-1)+DS(M),INIBRW%R(N))
                ENDIF
                DEPTHN%R(N)=0.D0
                CURBRW%R(N)=INIBRW%R(N)
              ELSE
                WRITE(LU,*) 'INCREASE NUMBER OF POINTS OF BREACH :',N
                GO TO 2000
              ENDIF
              EXIT FINDMIDNODE
            ENDIF
          ENDDO FINDMIDNODE
        END IF
        IF ((OPTERO%I(N).GT.10)) THEN
          GO TO 800
        END IF
        DEALLOCATE(XL)
        DEALLOCATE(YL)
        DEALLOCATE(XP)
        DEALLOCATE(YP)
        DEALLOCATE(DS)
!
      ENDDO
!
      INDBR%N = NBRECH
      DKAXCR%N = NBRECH
      DKAYCR%N = NBRECH
      PONDSB%N = NBRECH
      GOTO 1000
!
!-----------------------------------------------------------------------
!     ERROR MESSAGES
!-----------------------------------------------------------------------
!
804   CONTINUE
      WRITE(LU,*) 'LECBREACH : READ ERROR ON THE'
      WRITE(LU,*) '            BREACHES DATA FILE'
      WRITE(LU,*) '            FOR THE BREACH ',N
      WRITE(LU,*) '            INIBRW CANNOT BE READ'
      GO TO 2000
803   CONTINUE
      WRITE(LU,*) 'LECBREACH : READ ERROR ON THE'
      WRITE(LU,*) '            BREACHES DATA FILE'
      WRITE(LU,*) '            FOR THE BREACH ',N
      WRITE(LU,*) '            DF CANNOT BE READ'
      GO TO 2000
!
802   CONTINUE
      WRITE(LU,*) 'LECBREACH : READ ERROR ON THE'
      WRITE(LU,*) '            BREACHES DATA FILE'
      WRITE(LU,*) '            FOR THE BREACH ',N
      WRITE(LU,*) '            CRITICAL FLOW VELOCITY OR'
      WRITE(LU,*) '            EMPIRICAL FACTOR 1 OR'
      WRITE(LU,*) '            EMPIRICAL FACTOR 2 CANNOT BE READ'
      GO TO 2000
!
801   CONTINUE
      WRITE(LU,*) 'LECBREACH : READ ERROR ON THE'
      WRITE(LU,*) '            BREACHES DATA FILE'
      WRITE(LU,*) '            FOR THE BREACH ',N
      WRITE(LU,*) '            FINAL TIME STAGE 1 OR'
      WRITE(LU,*) '            VELOCITY STAGE 1 OR'
      WRITE(LU,*) '            VELOCITY STAGE 2 CANNOT BE READ'
      GO TO 2000
!
810   CONTINUE
      WRITE(LU,*) 'LECBREACH : READ ERROR ON THE'
      WRITE(LU,*) '            BREACHES DATA FILE'
      WRITE(LU,*) '            FOR THE BREACH ',N
      WRITE(LU,*) '            EROSION OPTION CANNOT BE READ'
      GO TO 2000
!
800   CONTINUE
      WRITE(LU,*) 'LECBREACH: UNAVAILABLE EROSION OPTION'
      GO TO 2000
!
999   CONTINUE
      WRITE(LU,*) 'LECBREACH: READ ERROR ON THE BREACHES DATA FILE'
      WRITE(LU,*) '           AT LINE 2'
      GO TO 2000
!
998   CONTINUE
      WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
      WRITE(LU,*) '         BREACHES DATA FILE'
      WRITE(LU,*) '         FOR THE BREACH ',N
      WRITE(LU,*) '         BREACH POLYGON WIDTH CANNOT BE READ'
      GO TO 2000
!
997   CONTINUE
      WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
      WRITE(LU,*) '         BREACHES DATA FILE'
      WRITE(LU,*) '         FOR THE BREACH ',N
      WRITE(LU,*) '         OPTION CANNOT BE READ'
      GO TO 2000
!
996   CONTINUE
      WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
      WRITE(LU,*) '         BREACHES DATA FILE'
      WRITE(LU,*) '         FOR THE BREACH ',N
      WRITE(LU,*) '         THE STARTING TIME CANNOT BE READ'
      GO TO 2000
!
995   CONTINUE
      WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
      WRITE(LU,*) '         BREACHES DATA FILE'
      WRITE(LU,*) '         FOR THE BREACH ',N
      WRITE(LU,*) '         THE OPENNING DURATION CANNOT BE READ'
      GO TO 2000
!
994   CONTINUE
      WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
      WRITE(LU,*) '         BREACHES DATA FILE'
      WRITE(LU,*) '         FOR THE BREACH ',N
      WRITE(LU,*) '         THE FINAL LEVEL CANNOT BE READ'
      GO TO 2000
!
993   CONTINUE
      WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
      WRITE(LU,*) '         BREACHES DATA FILE'
      WRITE(LU,*) '         FOR THE BREACH ',N
      WRITE(LU,*) '         THE NUMBER OF TEST POINT CANNOT BE READ'
      GO TO 2000
!
992   CONTINUE
      WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
      WRITE(LU,*) '         BREACHES DATA FILE'
      WRITE(LU,*) '         FOR THE BREACH ',N
      WRITE(LU,*) '         THE STARTING LEVEL CANNOT BE READ'
      GO TO 2000
!
991   CONTINUE
      WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
      WRITE(LU,*) '         BREACHES DATA FILE'
      WRITE(LU,*) '         FOR THE BREACH ',N
      WRITE(LU,*) '         THE POINT NUMBER OF LINE CANNOT BE READ'
      GO TO 2000
!
990   CONTINUE
      WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
      WRITE(LU,*) '         BREACHES DATA FILE'
      WRITE(LU,*) '         FOR THE BREACH ',N
      WRITE(LU,*) '         THE COORDINATE OF POINT ',M
      WRITE(LU,*) '         CANNOT BE READ'
      GO TO 2000
!
900   CONTINUE
      WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
      WRITE(LU,*) '         BREACHES DATA FILE'
      WRITE(LU,*) '         UNEXPECTED END OF FILE'
!
2000  CONTINUE
!
      CALL PLANTE(1)
      STOP
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(ITMP)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
