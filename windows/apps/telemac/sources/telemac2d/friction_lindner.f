!                   ***************************
                    SUBROUTINE FRICTION_LINDNER
!                   ***************************
!
     &(VA,HA,CF,VK,G,DP,SP,CP)
!
!***********************************************************************
! TELEMAC2D   V8P4
!***********************************************************************
!
!brief    COMPUTES FRICTION COEFFICIENT FOR NON-SUBMERGED
!+                VEGETATION FROM PARAMETERS.
!
!history  MICHAEL SCHROEDER, BAW
!+        **/11/1992
!+
!+   THE ALGORITHM WAS DEVELOPED BY LINDNER (1982) AND PASCHE
!
!history  F. HUVELIN
!+        20/04/2004
!+
!+   WRITTEN FROM THE C++ PROGRAM: RISMO2D OF THE BAW
!
!history  J-M HERVOUET (LNHE)
!+
!+        V5P5
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
!history  FREDERIK FOLKE (BAW)
!+        07/11/2019
!+        V8P1
!+   Simplification of the Code 
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| FRICTION COEFFICIENT FOR BOTTOM ROUGHNESS
!| CP             |<--| FRICTION COEFFICIENT FOR NON-SUBMERGED VEGETATION
!| DP             |-->| DIAMETER OF ROUGHNESS ELEMENT
!| G              |-->| GRAVITY ACCELERATION
!| HA             |-->| FLOW DEPTH
!| SP             |-->| SPACING OF ROUGHNESS ELEMENT
!| VA             |-->| VELOCITY
!| VK             |-->| KINEMATIC VISCOSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_FRICTION_LINDNER => FRICTION_LINDNER
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: VA,HA,CF,VK,G,DP,SP
      DOUBLE PRECISION, INTENT(OUT) :: CP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          PARAMETER :: KMAXITER = 10
      DOUBLE PRECISION, PARAMETER :: KPRECISION = 1.D-3
      INTEGER CDRAV,CDRMAX,CDRCOUNT,ANLAV,ANLMAX,ANLCOUNT,ITERR
!
!     ICDR,IANL: ITERATION COUNTERS FOR CDR AND ANL
      INTEGER I,J,ICDR,IANL,REALROOTS
      LOGICAL LCDR
!
      DOUBLE PRECISION CD, CDR, RCDR, DCDR, ANL, ANB, RANL
      DOUBLE PRECISION CDR1, CDR2, DCDR1, DCDR2
      DOUBLE PRECISION LAMBDA, FR
      DOUBLE PRECISION X(3), VRATIO, HRATIO
      DOUBLE PRECISION ALFA, ACOF, BCOF, CCOF, DCOF
      DOUBLE PRECISION TMP1, TMP2
!
!-----------------------------------------------------------------------
!
      LCDR     = .TRUE.
      CDRAV    = 0
      CDRMAX   = 0
      CDRCOUNT = 0
      ANLAV    = 0
      ANLMAX   = 0
      ANLCOUNT = 0
      ITERR    = 0
      IF ((DP .LT. 1.D-3)     .OR.(SP .LT. 1.D-2).OR.
     &    (ABS(VA) .LT. 1.D-3).OR.(HA .LT. 1.D-3)     ) THEN
        CP = 0.D0
      ELSE
        ! INITIALIZATION
        ! --------------
        CDR   = 1.D0      ! DRAG COEFFICIENT
        ANL   = SP*0.5D0  ! WAKE LENGTH OF A CYLINDER
        CDR1  = 1.D0
        CDR2  = 1.D0
        DCDR1 = 0.D0
        DCDR2 = 0.D0
        ! START OF ITERATION FOR CDR
        ! --------------------------
        DO ICDR = 1,KMAXITER
          ! SUPERPOSED FRICTION COEFFICIENT
          ! -------------------------------
          LAMBDA = 4.D0*CDR*HA*DP/SP/SP + 8.D0*CF
          ! DRAG COEFFICIENT CD FOR ONE CYLINDER
          ! ------------------------------------
          CALL DRAGCOEFF(VA, DP, VK, CD)
          ! WAKE LENGTH OF A CYLINDER (ITERATIVE COMPUTATION)
          ! -------------------------------------------------
          DO J=1, KMAXITER
            TMP1 = 1.D0  +  ANL*LAMBDA*0.25D0/HA
            TMP2 = 30.D0/ABS(TMP1)**(1.5D0)
            RANL = CD*DP*ABS(TMP2)**(1.429D0)
            ! TEST FOR CONVERGENCE
            ! --------------------
            IF (ABS((RANL-ANL)/RANL) .LT. KPRECISION) THEN
              ANL  = RANL
              IANL = -J
              EXIT
            ENDIF
            ANL = 0.5D0 * (RANL + ANL)
          ENDDO
          ! STATISTICS OF CDR ITERATION
          ! ---------------------------
          IF ( IANL .GT. 0 ) THEN
            ANL = SP*0.5D0
          ELSE
            IANL = ABS(IANL)
            ANLCOUNT = ANLCOUNT + 1
            ANLAV = IANL + ANLAV
            IF (IANL .GT. ANLMAX) ANLMAX = IANL
          ENDIF
          ! WAKE WIDTH
          ! ----------
          ANB = 0.24D0 * ABS(ANL)**(0.59D0) * ABS(CD*DP)**(0.41D0)
          ! RATIO OF VELOCITY IN FRONT OF AND BEHIND CYLINDER
          ! -------------------------------------------------
          VRATIO = 1.151D0 * ABS(ANL/SP)**(-0.483D0)
     &           +   0.5D0 * ABS(ANB/SP)**(1.1D0)
          ! RATIO OF FLOW DEPTH
          ! -------------------
          FR = VA / SQRT( G * HA ) ! FROUDE NUMBER
          ALFA = DP / SP
          ACOF =  FR**2 * (1.D0 - ALFA * CDR * 0.5D0)
          BCOF = -FR**2 - (1.D0 - ALFA) * 0.5D0
          CCOF =  0.D0
          DCOF = (1.D0 - ALFA) * 0.5D0
          HRATIO = 1.D0
          IF (ABS(ACOF) .LT. 1.D-10) THEN
            HRATIO = SQRT( -DCOF / BCOF)
          ELSE
            CALL CUBEEQUATION(ACOF, BCOF, CCOF, DCOF, REALROOTS, X)
            DO I = 1, REALROOTS
              IF (X(I) .GT. 0.D0 .AND. X(I) .LT. 1.D0)  THEN
                HRATIO = X(I)
                EXIT
              ENDIF
            ENDDO
          ENDIF
          ! REVISE DRAG COEFFICIENT CDR
          ! ---------------------------
          RCDR = 1.3124D0*CD*VRATIO + 2.D0*(1.D0-HRATIO)/FR**2
          ! TEST FOR CONVERGENCE
          ! --------------------
          IF ( ABS((RCDR-CDR)/RCDR) .LT. KPRECISION ) THEN
            LCDR = .FALSE.
!           ICDR = -1/ICDR
            EXIT
          ENDIF
          ! USE PEGASUS ALGORITHM FOR CDR ITERATION
          ! ---------------------------------------
          DCDR = RCDR - CDR
          IF ((ICDR .GE. 3) .AND. (DCDR1*DCDR2 .LT. 0.D0)) THEN
            IF (DCDR2*DCDR .LT. 0.D0) THEN
              DCDR1 = DCDR2/(DCDR2+DCDR)*DCDR1
            ELSE
              CDR1  = CDR2
              DCDR1 = DCDR2
            ENDIF
            CDR2  = CDR
            DCDR2 = DCDR
            CDR   = CDR2 - DCDR2*(CDR2-CDR1)/(DCDR2-DCDR1)
          ELSE
            CDR1 = CDR2
            DCDR1 = DCDR2
            CDR2 = CDR
            DCDR2 = DCDR
            IF ((ICDR .GE. 2) .AND. (DCDR1*DCDR2 .LT. 0.D0 )) THEN
              CDR = CDR2 - DCDR2*(CDR2-CDR1)/(DCDR2-DCDR1)
            ELSE
              CDR = RCDR
            ENDIF
          ENDIF
        ENDDO !ICDR = 1, KMAXITER
!
        IF (LCDR) THEN
          ITERR = ITERR + 1
          CP = -1.D0
        ELSE
          ! STATISTICS OF CDR ITERATION
          ! ---------------------------
          ICDR = -1/ICDR ! AS THE PROGRAM RISMO2D FROM THE BAW
          ICDR = -ICDR
          CDRCOUNT = CDRCOUNT + 1
          CDRAV = ICDR + CDRAV
          IF (ICDR .GT. CDRMAX) CDRMAX = ICDR
          CP = LAMBDA*0.25D0 - 2.D0*CF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
