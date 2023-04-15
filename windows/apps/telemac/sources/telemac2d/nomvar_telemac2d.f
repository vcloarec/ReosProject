!                   ***************************
                    SUBROUTINE NOMVAR_TELEMAC2D
!                   ***************************
!
     &(TEXTE,TEXTPR,MNEMO,NPERIAF,NTRAC,NAMETRAC,N_NAMES_PRIV,
     & NAMES_PRIVE,SECCURRENTS,NADVAR,NAMES_ADVAR)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    GIVES THE VARIABLE NAMES FOR THE RESULTS AND GEOMETRY
!+                FILES (IN TEXTE) AND FOR THE PREVIOUS COMPUTATION
!+                RESULTS FILE (IN TEXTPR).
!+
!+                TEXTE AND TEXTPR ARE GENERALLY EQUAL EXCEPT IF THE
!+                PREVIOUS COMPUTATION COMES FROM ANOTHER SOFTWARE.
!
!history  J-M HERVOUET (LNHE)
!+        31/08/2007
!+        V5P8
!+   First version.
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
!history  D WANG & P TASSI (LNHE)
!+        10/07/2014
!+        V7P0
!+   Secondary flow correction: add variables
!+   tau_s, Omega/h and r^{-1} for visualization
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        27/07/2015
!+        V7P1
!+   Now taking into account names of private arrays given by user.
!
!history  S.E. BOURBAN (HRW)
!+        20/06/2016
!+        V7P2
!+   Now taking into account names of differentiators given by user.
!
!history J-M HERVOUET (EDF LAB, LNHE)
!+        20/07/2016
!+        V7P2
!+   Elder model of turbulence is asked, the name of viscosity is
!+   replaced by longitudinal dispersion.
!
!history  B.GLANDER (BAW)
!+        06/12/2017
!+        V7P2
!+   add variable 35 with Referenz Level (ZRL) for Nestor
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MNEMO          |<--| MNEMONIC FOR 'VARIABLES FOR GRAPHIC OUTPUTS'
!| N_NAMES_PRIV   |-->| NUMBER OF NAMES OF PRIVATE VARIABLES GIVEN
!| NAMES_PRIVE    |-->| NAME OF PRIVATE VARIABLES GIVEN BY USER
!| NAMETRAC       |-->| NAME OF TRACERS (GIVEN BY KEYWORDS)
!| NPERIAF        |-->| NUMBER OF PERIODS FOR FOURRIER ANALYSIS
!| NTRAC          |-->| NUMBER OF TRACERS
!| TEXTE          |<--| SEE ABOVE
!| TEXTPR         |<--| SEE ABOVE
!| SECCURRENTS    |-->| IF YES SECONDARY CURRENTS COMPUTED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY : IND_SEC,ITURB,ADR_TRAC,NVAR_T2D
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)              :: NPERIAF,NTRAC,N_NAMES_PRIV
      INTEGER, INTENT(IN)              :: NADVAR
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(*),TEXTPR(*)
      CHARACTER(LEN=8),  INTENT(INOUT) :: MNEMO(*)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMETRAC(*),NAMES_PRIVE(4)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMES_ADVAR(*)
      LOGICAL, INTENT(IN)              :: SECCURRENTS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=2) CHAR2
      INTEGER I,ILAST,INEXT
!
!-----------------------------------------------------------------------
!
!  ENGLISH
!
      IF(LNG.EQ.LNG_EN) THEN
!
      TEXTE (1 ) = 'VELOCITY U      M/S             '
      TEXTE (2 ) = 'VELOCITY V      M/S             '
      TEXTE (3 ) = 'CELERITY        M/S             '
      TEXTE (4 ) = 'WATER DEPTH     M               '
      TEXTE (5 ) = 'FREE SURFACE    M               '
      TEXTE (6 ) = 'BOTTOM          M               '
      TEXTE (7 ) = 'FROUDE NUMBER                   '
      TEXTE (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTE (9 ) = 'EX TRACER                       '
      TEXTE (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      IF(ITURB.EQ.2) THEN
        TEXTE (12) = 'LONG. DISPERSIONM2/S            '
      ELSE
        TEXTE (12) = 'VISCOSITY       M2/S            '
      ENDIF
      TEXTE (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTE (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTE (15) = 'SCALAR VELOCITY M/S             '
      TEXTE (16) = 'WIND ALONG X    M/S             '
      TEXTE (17) = 'WIND ALONG Y    M/S             '
      TEXTE (18) = 'AIR PRESSURE    PASCAL          '
      TEXTE (19) = 'BOTTOM FRICTION                 '
      TEXTE (20) = 'DRIFT ALONG X   M               '
      TEXTE (21) = 'DRIFT ALONG Y   M               '
      TEXTE (22) = 'COURANT NUMBER                  '
      TEXTE (23) = 'VARIABLE 23     UNIT   ??       '
      TEXTE (24) = 'VARIABLE 24     UNIT   ??       '
      TEXTE (25) = 'VARIABLE 25     UNIT   ??       '
      TEXTE (26) = 'VARIABLE 26     UNIT   ??       '
      TEXTE (27) = 'HIGH WATER MARK M               '
      TEXTE (28) = 'HIGH WATER TIME S               '
      TEXTE (29) = 'HIGHEST VELOCITYM/S             '
      TEXTE (30) = 'TIME OF HIGH VELS               '
      TEXTE (31) = 'FRICTION VEL.   M/S             '
      TEXTE (32) = 'TAU_S           NA              '
      TEXTE (33) = '1/R             1/M             '
      TEXTE (34) = 'WALLDIST        M               '
      TEXTE (35) = 'REFERENCE LEVEL M               '
      TEXTE (36) = 'INCREMENT OF H  M               '
      TEXTE (37) = 'INCREMENT OF U  M/S             '
      TEXTE (38) = 'INCREMENT OF V  M/S             '
      TEXTE (39) = 'INCREMENT OF HN M               '
      TEXTE (40) = 'SA VISCOSITY    M2/S            '
!
! TEXTPR IS USED TO READ PREVIOUS COMPUTATION FILES.
! IN GENERAL TEXTPR=TEXTE BUT YOU CAN FOLLOW UP A COMPUTATION
! FROM ANOTHER CODE WITH DIFFERENT VARIABLE NAMES, WHICH MUST
! BE GIVEN HERE:
!
      TEXTPR (1 ) = 'VELOCITY U      M/S             '
      TEXTPR (2 ) = 'VELOCITY V      M/S             '
      TEXTPR (3 ) = 'CELERITY        M/S             '
      TEXTPR (4 ) = 'WATER DEPTH     M               '
      TEXTPR (5 ) = 'FREE SURFACE    M               '
      TEXTPR (6 ) = 'BOTTOM          M               '
      TEXTPR (7 ) = 'FROUDE NUMBER                   '
      TEXTPR (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTPR (9 ) = 'EX TRACER                       '
      TEXTPR (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      IF(ITURB.EQ.2) THEN
        TEXTPR(12) = 'LONG. DISPERSIONM2/S            '
      ELSE
        TEXTPR(12) = 'VISCOSITY       M2/S            '
      ENDIF
      TEXTPR (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTPR (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTPR (15) = 'SCALAR VELOCITY M/S             '
      TEXTPR (16) = 'WIND ALONG X    M/S             '
      TEXTPR (17) = 'WIND ALONG Y    M/S             '
      TEXTPR (18) = 'AIR PRESSURE    PASCAL          '
      TEXTPR (19) = 'BOTTOM FRICTION                 '
      TEXTPR (20) = 'DRIFT ALONG X   M               '
      TEXTPR (21) = 'DRIFT ALONG Y   M               '
      TEXTPR (22) = 'COURANT NUMBER                  '
      TEXTPR (23) = 'VARIABLE 23     UNIT   ??       '
      TEXTPR (24) = 'VARIABLE 24     UNIT   ??       '
      TEXTPR (25) = 'VARIABLE 25     UNIT   ??       '
      TEXTPR (26) = 'VARIABLE 26     UNIT   ??       '
      TEXTPR (27) = 'HIGH WATER MARK M               '
      TEXTPR (28) = 'HIGH WATER TIME S               '
      TEXTPR (29) = 'HIGHEST VELOCITYM/S             '
      TEXTPR (30) = 'TIME OF HIGH VELS               '
      TEXTPR (31) = 'FRICTION VEL.   M/S             '
      TEXTPR (32) = 'TAU_S           NA              '
      TEXTPR (33) = '1/R             1/M             '
      TEXTPR (34) = 'WALLDIST        M               '
      TEXTPR (35) = 'REFERENCE LEVEL M               '
      TEXTPR (36) = 'INCREMENT OF H  M               '
      TEXTPR (37) = 'INCREMENT OF U  M/S             '
      TEXTPR (38) = 'INCREMENT OF V  M/S             '
      TEXTPR (39) = 'INCREMENT OF HN M               '
      TEXTPR (40) = 'SA VISCOSITY    M2/S            '
!
!-----------------------------------------------------------------------
!
!  FRANCAIS OU AUTRE
!
      ELSE
!
      TEXTE (1 ) = 'VITESSE U       M/S             '
      TEXTE (2 ) = 'VITESSE V       M/S             '
      TEXTE (3 ) = 'CELERITE        M/S             '
      TEXTE (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTE (5 ) = 'SURFACE LIBRE   M               '
      TEXTE (6 ) = 'FOND            M               '
      TEXTE (7 ) = 'FROUDE                          '
      TEXTE (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTE (9 ) = 'EX TRACEUR                      '
      TEXTE (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      IF(ITURB.EQ.2) THEN
        TEXTE (12) = 'DISPERSION LONG.M2/S            '
      ELSE
        TEXTE (12) = 'VISCOSITE TURB. M2/S            '
      ENDIF
      TEXTE (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTE (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTE (15) = 'VITESSE SCALAIREM/S             '
      TEXTE (16) = 'VENT X          M/S             '
      TEXTE (17) = 'VENT Y          M/S             '
      TEXTE (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTE (19) = 'FROTTEMENT                      '
      TEXTE (20) = 'DERIVE EN X     M               '
      TEXTE (21) = 'DERIVE EN Y     M               '
      TEXTE (22) = 'NBRE DE COURANT                 '
      TEXTE (23) = 'VARIABLE 23     UNITES ??       '
      TEXTE (24) = 'VARIABLE 24     UNITES ??       '
      TEXTE (25) = 'VARIABLE 25     UNITES ??       '
      TEXTE (26) = 'VARIABLE 26     UNITES ??       '
      TEXTE (27) = 'COTE MAXIMUM    M               '
      TEXTE (28) = 'TEMPS COTE MAXI S               '
      TEXTE (29) = 'VITESSE MAXIMUM M/S             '
      TEXTE (30) = 'T VITESSE MAXI  S               '
      TEXTE (31) = 'VITESSE DE FROT.M/S             '
      TEXTE (32) = 'TAU_S           NA              '
      TEXTE (33) = '1/R             1/M             '
      TEXTE (34) = 'DIST PAROI      M               '
      TEXTE (35) = 'REFERENCE LEVEL M               '
      TEXTE (36) = 'INCREMENT DE H  M               '
      TEXTE (37) = 'INCREMENT DE U  M/S             '
      TEXTE (38) = 'INCREMENT DE V  M/S             '
      TEXTE (39) = 'INCREMENT DE HN M               '
      TEXTE (40) = 'VISCOSITE SA    M2/S            '
!
! TEXTPR SERT A LA LECTURE DES FICHIERS DE CALCULS PRECEDENTS
! A PRIORI TEXTPR=TEXTE MAIS ON PEUT ESSAYER DE FAIRE UNE SUITE
! DE CALCUL A PARTIR D'UN AUTRE CODE.
!
      TEXTPR (1 ) = 'VITESSE U       M/S             '
      TEXTPR (2 ) = 'VITESSE V       M/S             '
      TEXTPR (3 ) = 'CELERITE        M/S             '
      TEXTPR (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTPR (5 ) = 'SURFACE LIBRE   M               '
      TEXTPR (6 ) = 'FOND            M               '
      TEXTPR (7 ) = 'FROUDE                          '
      TEXTPR (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTPR (9 ) = 'EX TRACEUR                      '
      TEXTPR (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      IF(ITURB.EQ.2) THEN
        TEXTPR(12) = 'DISPERSION LONG.M2/S            '
      ELSE
        TEXTPR(12) = 'VISCOSITE TURB. M2/S            '
      ENDIF
      TEXTPR (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTPR (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTPR (15) = 'VITESSE SCALAIREM/S             '
      TEXTPR (16) = 'VENT X          M/S             '
      TEXTPR (17) = 'VENT Y          M/S             '
      TEXTPR (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTPR (19) = 'FROTTEMENT                      '
      TEXTPR (20) = 'DERIVE EN X     M               '
      TEXTPR (21) = 'DERIVE EN Y     M               '
      TEXTPR (22) = 'NBRE DE COURANT                 '
      TEXTPR (23) = 'VARIABLE 23     UNITES ??       '
      TEXTPR (24) = 'VARIABLE 24     UNITES ??       '
      TEXTPR (25) = 'VARIABLE 25     UNITES ??       '
      TEXTPR (26) = 'VARIABLE 26     UNITES ??       '
      TEXTPR (27) = 'COTE MAXIMUM    M               '
      TEXTPR (28) = 'TEMPS COTE MAXI S               '
      TEXTPR (29) = 'VITESSE MAXIMUM M/S             '
      TEXTPR (30) = 'T VITESSE MAXI  S               '
      TEXTPR (31) = 'VITESSE DE FROT.M/S             '
      TEXTPR (32) = 'TAU_S           NA              '
      TEXTPR (33) = '1/R             1/M             '
      TEXTPR (34) = 'DIST PAROI      M               '
      TEXTPR (35) = 'REFERENCE LEVEL M               '
      TEXTPR (36) = 'INCREMENT DE H  M               '
      TEXTPR (37) = 'INCREMENT DE U  M/S             '
      TEXTPR (38) = 'INCREMENT DE V  M/S             '
      TEXTPR (39) = 'INCREMENT DE HN M               '
      TEXTPR (40) = 'VISCOSITE SA    M2/S            '
      ENDIF
!
!-----------------------------------------------------------------------
!
!   ALIASES FOR THE VARIABLES IN THE STEERING FILE
!
!     UVCHSBFQTKEDIJMXYPWAGLNORZ
!     VELOCITY COMPONENT U
      MNEMO(1)   = 'U       '
!     VELOCITY COMPONENT V
      MNEMO(2)   = 'V       '
!     CELERITY
      MNEMO(3)   = 'C       '
!     WATER DEPTH
      MNEMO(4)   = 'H       '
!     FREE SURFACE ELEVATION
      MNEMO(5)   = 'S       '
!     BOTTOM ELEVATION
      MNEMO(6)   = 'B       '
!     FROUDE
      MNEMO(7)   = 'F       '
!     FLOW RATE
      MNEMO(8)   = 'Q       '
!     EX TRACER
      MNEMO(9)   = '?       '
!     TURBULENT ENERGY
      MNEMO(10)   = 'K       '
!     DISSIPATION
      MNEMO(11)   = 'E       '
!     TURBULENT VISCOSITY
      MNEMO(12)   = 'D       '
!     FLOWRATE ALONG X
      MNEMO(13)   = 'I       '
!     FLOWRATE ALONG Y
      MNEMO(14)   = 'J       '
!     SPEED
      MNEMO(15)   = 'M       '
!     WIND COMPONENT X
      MNEMO(16)   = 'X       '
!     WIND COMPONENT Y
      MNEMO(17)   = 'Y       '
!     ATMOSPHERIC PRESSURE
      MNEMO(18)   = 'P       '
!     FRICTION
      MNEMO(19)   = 'W       '
!     DRIFT IN X
      MNEMO(20)   = 'A       '
!     DRIFT IN Y
      MNEMO(21)   = 'G       '
!     COURANT NUMBER
      MNEMO(22)   = 'L       '
!     VARIABLE 23
      MNEMO(23)   = 'N       '
!     VARIABLE 24
      MNEMO(24)   = 'O       '
!     VARIABLE 25
      MNEMO(25)   = 'R       '
!     VARIABLE 26
      MNEMO(26)   = 'Z       '
!     VARIABLE 27
      MNEMO(27)   = 'MAXZ    '
!     VARIABLE 28
      MNEMO(28)   = 'TMXZ    '
!     VARIABLE 29
      MNEMO(29)   = 'MAXV    '
!     VARIABLE 30
      MNEMO(30)   = 'TMXV    '
!     VARIABLE 31
      MNEMO(31)   = 'US      '
!
      MNEMO(32)   = 'TAU_S   '
!
      MNEMO(33)   = '1/R     '
!     WALL DISTANCE
      MNEMO(34)   = 'WDIST   '
!     REFERENCE LEVEL FOR NESTOR
      MNEMO(35)   = 'ZRL     '
!     INCREMENT OF H
      MNEMO(36)   = 'DH      '
!     INCREMENT OF U
      MNEMO(37)   = 'DU      '
!     INCREMENT OF V
      MNEMO(38)   = 'DV      '
!     INCREMENT OF HN
      MNEMO(39)   = 'DHN     '
!     SPALART ALLMARAS VISCOSITY
      MNEMO(40)   = 'VISCSA  '
!
      NVAR_T2D = 40
!     CAUTION IN 2D!!! ADR_TRAC = NVAR_T2D
!     CONTRARY TO 3D WITH ADR_TRAC = NVAR_T3D + 1!

      ADR_TRAC = NVAR_T2D
      ILAST = NVAR_T2D
      INEXT = ILAST+1
!
!-----------------------------------------------------------------------
!
!     FOURIER ANALYSIS
!
      IF(NPERIAF.GT.0) THEN
        DO I=1,NPERIAF
          WRITE(CHAR2,'(I2)') I
          IF(LNG.EQ.LNG_FR) THEN
            TEXTE(INEXT+NTRAC+2*(I-1))    =  'AMPLI PERIODE '
     &                         //ADJUSTL(CHAR2)
     &                         //'M               '
            TEXTE(INEXT+1+NTRAC+2*(I-1))  =  'PHASE PERIODE '
     &                         //ADJUSTL(CHAR2)
     &                         //'DEGRES          '
            TEXTPR(INEXT+NTRAC+2*(I-1))   =  'AMPLI PERIODE '
     &                         //ADJUSTL(CHAR2)
     &                         //'M               '
            TEXTPR(INEXT+1+NTRAC+2*(I-1)) =  'PHASE PERIODE '
     &                         //ADJUSTL(CHAR2)
     &                         //'DEGRES          '
          ELSE
            TEXTE(INEXT+NTRAC+2*(I-1))    =  'AMPLI PERIOD  '
     &                         //ADJUSTL(CHAR2)
     &                         //'M               '
            TEXTE(INEXT+1+NTRAC+2*(I-1))  =  'PHASE PERIOD  '
     &                         //ADJUSTL(CHAR2)
     &                         //'DEGRES          '
            TEXTPR(INEXT+NTRAC+2*(I-1))   =  'AMPLI PERIOD  '
     &                         //ADJUSTL(CHAR2)
     &                         //'M               '
            TEXTPR(INEXT+1+NTRAC+2*(I-1)) =  'PHASE PERIOD  '
     &                         //ADJUSTL(CHAR2)
     &                         //'DEGRES          '
          ENDIF
          MNEMO(INEXT+NTRAC+2*(I-1))   = 'AMPL'//ADJUSTL(CHAR2)//'  '
          MNEMO(INEXT+1+NTRAC+2*(I-1)) = 'PHAS'//ADJUSTL(CHAR2)//'  '
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          TEXTE(ILAST+I)  = NAMETRAC(I)
          TEXTPR(ILAST+I) = NAMETRAC(I)
          WRITE(CHAR2,'(I2)') I
          MNEMO(ILAST+I)  = 'T'//ADJUSTL(CHAR2)//'   '
        ENDDO
!       OMEGA FOR SECONDARY CURRENTS
        IF(SECCURRENTS) THEN
          TEXTE(ILAST+IND_SEC) = NAMETRAC(IND_SEC)
          TEXTPR(ILAST+IND_SEC)= NAMETRAC(IND_SEC)
          MNEMO(ILAST+IND_SEC) = 'OMEGA   '
        ENDIF
      ENDIF
      IF(N_NAMES_PRIV.GT.0) THEN
        DO I=1,N_NAMES_PRIV
          TEXTE(22+I)  = NAMES_PRIVE(I)
          TEXTPR(22+I) = NAMES_PRIVE(I)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     DIFFERENTIATORS
!
      IF(NADVAR.GT.0) THEN
        DO I=1,NADVAR
          TEXTE(ILAST+NTRAC+2*NPERIAF+I)  = NAMES_ADVAR(I)
          TEXTPR(ILAST+NTRAC+2*NPERIAF+I) = NAMES_ADVAR(I)
          WRITE(CHAR2,'(I2)') I
          MNEMO(ILAST+NTRAC+2*NPERIAF+I) = 'AD'//ADJUSTL(CHAR2)//'    '
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      CALL USER_NOMVAR_TELEMAC2D
     &(TEXTE,TEXTPR,MNEMO,NPERIAF,NTRAC,NAMETRAC,N_NAMES_PRIV,
     & NAMES_PRIVE,SECCURRENTS,NADVAR,NAMES_ADVAR)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
