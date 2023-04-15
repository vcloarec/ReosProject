!                   ***************************
                    SUBROUTINE PRERES_TELEMAC2D
!                   ***************************
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
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
!history  C. GOEURY (EDF R&D LNHE)
!+        25/07/2013
!+        V6P3
!+   Sum of HAP in oilspills has been added.
!
!history  J-M HERVOUET EDF R&D, LNHE)
!+        02/01/2014
!+        V7P0
!+   Securing bound checking in parallelism.
!
!history  J-M HERVOUET EDF R&D, LNHE)
!+        28/10/2014
!+        V7P0
!+   Initialising Lagrangian drifts for iteration 0 in case they are
!+   in outputs.
!
!history  R. ATA & J-M HERVOUET (EDF LAB, LNHE)
!+        10/06/2015
!+        V7P1
!+   Now all the variables asked for graphic printouts are written for
!+   remarkable points.
!
!history  R. ATA (EDF LAB, LNHE)
!+        11/01/2016
!+        V7P2
!+   Now preres gives instruction to bief_desimp to write graphical
!+   results (through leo and imp)
!
!history J-M HERVOUET (EDF LAB, LNHE)
!+        20/07/2016
!+        V7P2
!+   When Elder model of turbulence is asked, the longitudinal
!+   dispersion is retrieved from NUXX and NUYY, and KL+PROPNU put in
!+   T10.
!
!history S.E.BOURBAN (HRW)
!+        11/11/2016
!+        V7P2
!+   Adding the DIFFERENTIATORS to the list of updated variables for
!+   for printing purposes.
!+   Updating function calls within WRITE statements for NAG compliancy.
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
      USE INTERFACE_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_MIN
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER LTT,N,IMAX,I,J
!
      DOUBLE PRECISION HHH,HIH,HAH,XMAX
      DOUBLE PRECISION, PARAMETER :: EPSS=1.E-10
      DOUBLE PRECISION LPRDTIME,RESTE,SEUIL
!
      INTRINSIC MAX,SQRT,CEILING
!
!-----------------------------------------------------------------------
!
!     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
!     FOLLOWING TESTS, WHICH MUST BE THE SAME AS IN BIEF_DESIMP (BIEF LIBRARY)
!
      IMP=.FALSE.
!      LEO=.FALSE.
!     THIS WILL TRIGGER THE OUTPUT OF LAST TIMESTEP
!     BUT NOT WITH PARAMETER ESTIMATION (LISPRD WOULD STAY AT 1
!     FOR FURTHER COMPUTATIONS)
!      IF(LT.EQ.NIT.AND.ESTIME(1:1).EQ.' ') THEN
!        IMP=.FALSE.
!        LEO=.FALSE.
!      ENDIF
!     Always write the initial conditions
      IF(LT.EQ.0) THEN
        IMP=.TRUE.
        LEO=.TRUE.
        COMPLEO=0
        COMPLIS=0
      ELSE
        IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!         FEM
          LTT=(LT/LISPRD)*LISPRD
          IF(LT.EQ.LTT.AND.LT.GE.PTINIL) IMP=.TRUE.
          LTT=(LT/LEOPRD)*LEOPRD
          IF(LT.EQ.LTT.AND.LT.GE.PTINIG) LEO=.TRUE.
          IF(ABS(AT-(DUREE+AT0)).LT.1.E-14)LEO=.TRUE.
!         FOR GRAPHICAL OUTPUTS
          IF(LEO)COMPLEO=COMPLEO+1
        ELSE
!         FVM
          IF(LT.GT.PTINIL)THEN
!           LISTING OUTPUT
            LPRDTIME=LISPRD*DTINI
            LTT=CEILING(AT/LPRDTIME)
            RESTE=(LTT*LPRDTIME-AT)/LPRDTIME
            IF(RESTE.LT.EPSS.OR.ABS(RESTE-1.D0).LT.EPSS.OR.
!                                   CASE WHERE RESTE=1
     &        LT.EQ.NIT)THEN
              IMP=.TRUE.
              COMPLIS=COMPLIS+1
            ENDIF
          ENDIF
!         FOR GRAPHICAL OUTPUTS
          IF(LEO)COMPLEO=COMPLEO+1
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
! 1)  PART WHICH MUST BE DONE EVEN IF THERE IS NO OUTPUT FOR THIS TIMESTEP
!     BUT ONLY AFTER FIRST TIMESTEP FOR GRAPHIC PRINTOUTS
!
!-----------------------------------------------------------------------
!
      IF(LT.GE.PTINIG) THEN
!
!=======================================================================
! COMPUTES THE MAXIMUM ELEVATION AND ASSOCIATED TIME
!=======================================================================
!
      IF(SORLEO(27).OR.SORIMP(27)) THEN
        IF(.NOT.DEJA1_PRERES) THEN
          IF(DEBU.OR..NOT.MAX_PREV.OR.TROUVE(27).NE.1) THEN
            CALL OS('X=Y     ',X=MAXZ ,Y=ZF)
          ENDIF
          IF(DEBU.OR..NOT.MAX_PREV.OR.TROUVE(28).NE.1) THEN
            CALL OS('X=C     ',X=TMAXZ,C=AT)
          ENDIF
          DEJA1_PRERES=.TRUE.
        ELSE
          DO N=1,NPOIN
            XMAX=H%R(N)+ZF%R(N)
!           DRY LAND EXCLUDED (TO AVOID RANDOM TIMES)
            IF(XMAX.GT.MAXZ%R(N).AND.H%R(N).GT.0.01D0) THEN
              IF(SORLEO(28).OR.SORIMP(28))THEN
                SEUIL=ABS(MAXZ%R(N)-XMAX)/(ABS(MAXZ%R(N))+ABS(XMAX))
                IF(SEUIL.GT.EPSS) TMAXZ%R(N)=AT
              ENDIF
              MAXZ%R(N)=XMAX
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
! COMPUTES THE MAXIMUM SPEED AND ASSOCIATED TIME
!=======================================================================
!
      IF(SORLEO(29).OR.SORIMP(29)) THEN
        IF(.NOT.DEJA2_PRERES) THEN
          IF(DEBU.OR..NOT.MAX_PREV.OR.TROUVE(29).NE.1) THEN
            CALL OS('X=C     ',X=MAXV ,C=0.D0)
          ENDIF
          IF(DEBU.OR..NOT.MAX_PREV.OR.TROUVE(30).NE.1) THEN
            CALL OS('X=C     ',X=TMAXV,C=AT)
          ENDIF
          DEJA2_PRERES=.TRUE.
        ELSE
          DO N=1,NPOIN
            XMAX=SQRT(U%R(N)**2+V%R(N)**2)
!           DRY LAND EXCLUDED (TO AVOID RANDOM TIMES)
            IF(XMAX.GT.MAXV%R(N).AND.H%R(N).GT.0.01D0) THEN
              MAXV%R(N)=XMAX
              IF(SORLEO(30).OR.SORIMP(30)) TMAXV%R(N)=AT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
!     CASE WHERE OUTINI=.TRUE. : PRIORITY ON PTINIG, VALUES FOR LT=0
!     OTHERWISE THEY WOULD NOT BE INITIALISED
        IF(DEBU.OR..NOT.MAX_PREV.OR.TROUVE(27).NE.1) THEN
          IF(SORLEO(27).OR.SORIMP(27)) CALL OS('X=Y     ',X=MAXZ, Y=ZF)
        ENDIF
        IF(DEBU.OR..NOT.MAX_PREV.OR.TROUVE(28).NE.1) THEN
          IF(SORLEO(28).OR.SORIMP(28)) CALL OS('X=C     ',X=TMAXZ,C=AT)
        ENDIF
        IF(DEBU.OR..NOT.MAX_PREV.OR.TROUVE(29).NE.1) THEN
          IF(SORLEO(29).OR.SORIMP(29)) CALL OS('X=C     ',X=MAXV,C=0.D0)
        ENDIF
        IF(DEBU.OR..NOT.MAX_PREV.OR.TROUVE(30).NE.1) THEN
          IF(SORLEO(30).OR.SORIMP(30)) CALL OS('X=C     ',X=TMAXV,C=AT)
        ENDIF
!
!     ENDIF FOR : IF(LT.GE.PTINIG) THEN
      ENDIF
!
!-----------------------------------------------------------------------
!
! 2)  PART WHICH MUST BE DONE ONLY IF THERE IS AN OUTPUT FOR THIS TIMESTEP
!
!-----------------------------------------------------------------------
!
!     NO PRINTOUT REQUIRED (LISTING OR RESULT FILE): EXITS
      IF(.NOT.(LEO.OR.IMP)) GO TO 1000
!
!
!=======================================================================
! COMPUTES CELERITY (IN FU, SEE BLOCK: VARSOR)
!=======================================================================
!
      IF((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
        CALL CPSTVC(ZF,FU)
        DO N=1,NPOIN
          FU%R(N) = SQRT ( GRAV * MAX(H%R(N),0.D0) )
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FREE SURFACE ELEVATION (= H + ZF, IN FV)
!=======================================================================
!
      IF((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
        CALL CPSTVC(ZF,FV)
        DO N=1,NPOIN
          FV%R(N) = H%R(N)+ZF%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FROUDE NUMBER
!=======================================================================
!
      IF((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        CALL CPSTVC(ZF,T2)
        DO N=1,NPOIN
          HHH = MAX( H%R(N) , 1.D-8 )
          T2%R(N) = SQRT (( U%R(N)**2 + V%R(N)**2 ) / ( HHH*GRAV ))
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FLOWRATE
!=======================================================================
!
      IF((LEO.AND.SORLEO(8)).OR.(IMP.AND.SORIMP(8))) THEN
        CALL CPSTVC(ZF,T3)
        DO N=1,NPOIN
          T3%R(N) = SQRT (U%R(N)**2 + V%R(N)**2) * H%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! RETRIEVING LONGITUDINAL DISPERSION
!=======================================================================
!
      IF(((LEO.AND.SORLEO(12)).OR.(IMP.AND.SORIMP(12)))
     &   .AND.ITURB.EQ.2) THEN
        DO N=1,NPOIN
!         RETRIEVING KL (SEE SUBROUTINE DISPER) AND ADDING PROPNU.
          T10%R(N) = (VISC%R(N)+VISC%R(N+NPOIN)-2*PROPNU)*ELDER(1)/
     &              (ELDER(1)+ELDER(2)) + PROPNU
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FLOWRATE ALONG X
!=======================================================================
!
      IF((LEO.AND.SORLEO(13)).OR.(IMP.AND.SORIMP(13))) THEN
        CALL CPSTVC(ZF,T4)
        DO N=1,NPOIN
          T4%R(N)=H%R(N)*U%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FLOWRATE ALONG Y
!=======================================================================
!
      IF((LEO.AND.SORLEO(14)).OR.(IMP.AND.SORIMP(14))) THEN
        CALL CPSTVC(ZF,T5)
        DO N=1,NPOIN
          T5%R(N)=H%R(N)*V%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES SPEED
!=======================================================================
!
      IF((LEO.AND.SORLEO(15)).OR.(IMP.AND.SORIMP(15))) THEN
        CALL OS( 'X=N(Y,Z)' , X=T6 , Y=U , Z=V )
      ENDIF
!
!=======================================================================
! LAGRANGIAN DRIFTS
!=======================================================================
!
      IF((LEO.AND.SORLEO(20)).OR.(IMP.AND.SORIMP(20))) THEN
        IF(LT.EQ.0) CALL OS('X=0     ',X=T7)
      ENDIF
      IF((LEO.AND.SORLEO(21)).OR.(IMP.AND.SORIMP(21))) THEN
        IF(LT.EQ.0) CALL OS('X=0     ',X=T8)
      ENDIF
!
!=======================================================================
! COMPUTES COURANT NUMBER
!=======================================================================
!
      IF((LEO.AND.SORLEO(22)).OR.(IMP.AND.SORIMP(22))) THEN
!                             IELM
        CALL CFLPSI(T9,U,V,DT,11,MESH,MSK,MASKEL)
        CALL MAXI(XMAX,IMAX,T9%R,NPOIN)
        IF(NCSIZE.GT.1) XMAX = P_MAX(XMAX)
        WRITE(LU,79) XMAX
79      FORMAT(1X,'PRERES: MAXIMUM COURANT NUMBER: ',G16.7)
      ENDIF
!
!=======================================================================
! COMPUTES FRICTION SPEED
!=======================================================================
!
      IF((LEO.AND.SORLEO(31)).OR.(IMP.AND.SORIMP(31))) THEN
        CALL CPSTVC(CF,T7)
        DO N=1,NPOIN
          T7%R(N) = SQRT(0.5D0*CF%R(N)*(U%R(N)**2+V%R(N)**2))
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES THE SUM OF SOLUBLE COMPONENT DURING THE OIL SPILL
!=======================================================================
!
      IF((SORLEO(23).OR.SORIMP(23)).AND.SPILL_MODEL.AND.NTRAC.GT.0) THEN
        DO N=1,NPOIN
          PRIVE1(N) = T%ADR(1)%P%R(N)
        ENDDO
        IF(NTRAC.GT.1) THEN
          DO I=2,NTRAC
            DO N=1,NPOIN
              PRIVE1(N) = PRIVE1(N) + T%ADR(I)%P%R(N)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
! UPDATE THE POINTERS TO THE DIFFERENTIATED VARIABLES
!=======================================================================
!
      J = ADR_TRAC+NTRAC+2*NPERIAF+VARCL%N
      DO I = 1,NADVAR
        IF((LEO.AND.SORLEO(J)).OR.(IMP.AND.SORIMP(J))) THEN
          CALL AD_GET_TELEMAC2D(I,ADVAR%ADR(I)%P)
          J = J + 1
        ENDIF
      ENDDO
!
!=======================================================================
!
1000  CONTINUE
!
!=======================================================================
! HARMONIC ANALYSIS USING LEAST MEAN ERROR SQUARE METHOD
!=======================================================================
!
      IF(NPERIAF.GT.0) CALL SPECTRE
!
!=======================================================================
! PRINTOUTS FOR THE REMARKABLE POINTS
!=======================================================================
!
      IF(LT.EQ.NIT.AND.NPTS.GT.0) THEN
        DO I=1,MAXVAR
!         BEWARE : HERE SORLEO IS USED INSTEAD OF SORIMP
          IF(SORLEO(I)) THEN
            WRITE(LU,*) ' '
            WRITE(LU,*) ' '
            WRITE(LU,*) ' '
            WRITE(LU,*) TEXTE(I)(1:16)
            WRITE(LU,*) ' '
            DO N=1,NPTS
!             IN PARALLEL POINT DOES NOT ALWAYS EXIST, MAYBE ELSEWHERE
              IF(NCSIZE.GT.1) THEN
                HHH = 0.D0
                IF(LIST_PTS(N).GT.0) HHH=VARSOR%ADR(I)%P%R(LIST_PTS(N))
                HIH = HHH
                HIH = P_MIN(HIH)
                HAH = HHH
                HAH = P_MAX(HAH)
                WRITE(LU,*) NAME_PTS(N),' : ',HIH+HAH
              ELSE
                WRITE(LU,*) NAME_PTS(N),' : ',
     &                                    VARSOR%ADR(I)%P%R(LIST_PTS(N))
              ENDIF
            ENDDO
            IF(T2D_FILES(T2DRFO)%NAME(1:1).NE.' ') THEN
              IF(IPID.EQ.0) THEN
                WRITE(T2D_FILES(T2DRFO)%LU,*) ' '
                WRITE(T2D_FILES(T2DRFO)%LU,*) ' '
                WRITE(T2D_FILES(T2DRFO)%LU,*) ' '
                WRITE(T2D_FILES(T2DRFO)%LU,*) TEXTE(I)(1:16)
                WRITE(T2D_FILES(T2DRFO)%LU,*) ' '
              ENDIF
              DO N=1,NPTS
!             IN PARALLEL POINT DOES NOT ALWAYS EXIST, MAYBE ELSEWHERE
                IF(NCSIZE.GT.1) THEN
                  HHH = 0.D0
                  IF(LIST_PTS(N).GT.0)THEN
                    HHH=VARSOR%ADR(I)%P%R(LIST_PTS(N))
                  ENDIF
                  HIH = HHH
                  HIH = P_MIN(HIH)
                  HAH = HHH
                  HAH = P_MAX(HAH)
                  IF(IPID.EQ.0) THEN
                    WRITE(T2D_FILES(T2DRFO)%LU,*) NAME_PTS(N),' : '
     &                      ,HIH+HAH
                  ENDIF
                ELSE
                  WRITE(T2D_FILES(T2DRFO)%LU,*) NAME_PTS(N),' : ',
     &                    VARSOR%ADR(I)%P%R(LIST_PTS(N))
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!=======================================================================
!
      CALL USER_PRERES_TELEMAC2D
!
      RETURN
      END
