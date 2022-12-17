!                   ****************
                    SUBROUTINE ASTRO
!                   ****************
!
     &(YEAR,MONTH,DAY,HOUR,MINU,SEC,AT,ARL,ARS,DL,DS,AL,AS)
!
!***********************************************************************
! TELEMAC2D   V7P0
!***********************************************************************
!
!brief    COMPUTES THE ASTRONOMICAL TERMS NECESSARY FOR THE
!+                COMPUTATION OF THE TIDAL FORCING TERMS.
!code
!+ DESCRIPTION OF THE ASTRONOMICAL PARAMETERS:
!+
!+  NOTE: "PERIODE" IS THE PERIOD OF THE PARAMETER EXPRESSED IN
!+          DAY/YEAR/ JULIAN CENTURIES
!+
!+
!+  THE FOLLOWING TABLE GIVES THE CORRESPONDANCE WITH NOTATIONS IN THE BOOK
!+  "HYDRODYNAMICS OF FREE SURFACE FLOWS"
!+
!+ .____.__________________________________________.________.___________.
!+ |CODE|                   ROLE                   |NOTATION| PERIOD    |
!+ |    |                                          |IN BOOK |           |
!+ |____|__________________________________________|________|___________|
!+ |    |                                          |        |           |
!+ |    |        WITH VARIATIONS IN TIME           |        |           |
!+ |    |                                          |        |           |
!+ | H  |   MEAN LONGITUDE OF THE MOON             |    H   | 365,25  J |
!+ | S  |   MEAN LONGITUDE OF THE SUN              |    S   | 27,32   J |
!+ | P  |   MEAN LONGITUDE OF MOON PERIGEE         |    P   | 8,85    A |
!+ | O  |   LONGITUDE OF THE MOON ASCENDING NODE   | N,OMEGA| 18,61   A |
!+ | OM |   INCLINAION OF THE EQUATOR ON THE       |  OMEGA | 27665,7 S |
!+ |    |   ECLIPTIC                               |        |           |
!+ | L  |   REAL LONGITUDE OF MOON                 |    L   |           |
!+ | CR |   REAL PARALLAX OF MOON                  |   C/R  |           |
!+ | DL |   DECLINATION OF MOON                    |  DELTA |           |
!+ | AL |   RIGHT ASCENSION OF MOON                |  ALPHA |           |
!+ | NU |   RIGHT ASCENSION OF G (EQUATOR-ORBIT)   |    NU  |           |
!+ | ET |   ECCENTRIC ANOMALY OF EARTH             |    e   |           |
!+ | MA |   MEAN ANOMALY OF SUN                    |    M   | 365,26  J |
!+ | EA |   ECCENTRIC ANOMALY OF SUN               |    E   |           |
!+ | TS |   DISTANCE EARTH-SUN (UA)                |    R   |           |
!+ | TL |   DISTANCE EARTH-MOON (KM)               |    R   |           |
!+ | ARL|   RATIO   RT / TL                        |   A/R  |           |
!+ | ARS|   RATIO   RT / TS                        |   A/R  |           |
!+ | VS |   REAL ANOMALY OF THE SUN                |    V   |           |
!+ | LS |   REAL LONGITUDE OF THE SUN              |  TETA  |           |
!+ | AS |   RIGHT ASCENSION OF SUN                 |  ALPHA |           |
!+ | DS |   SUN DECLINATION                        |  DELTA |           |
!+ |    |                                          |        |           |
!+ |    |               CONSTANTS                  |        |           |
!+ |    |                                          |        |           |
!+ | I0 |   INCLINATION OF MOON ORBIT ON THE       |   I    |           |
!+ |    |   ECLIPTIC                               |        |           |
!+ | E  |   ECCENTRICITY OF MOON                   |   E    |           |
!+ | M  |   RATIO OF THE AVERAGE MOVEMENT OF SUN   |   M    |           |
!+ |    |   ON AVERAGE MOVEMENT OF MOON            |        |           |
!+ | UA |   ASTRONOMICAL UNIT IN  KM               |   UA   |           |
!+ | RT |   MEAN RADIUS OF EARTH                   |   A    |           |
!+ | C  |   SEMI-MAJOR AXIS OF MOON (KM)           |   C    |           |
!+ | AC |   RATIO   RT / C                         |  A/C   |           |
!+ |____|__________________________________________|________|___________|
!+
!+ OTHER CONSTANTS:
!+
!+   PI:   VALUE OF PI
!+   API:  TRANSFORMS DEGREES INTO RADIANS
!+
!+ INTERMEDIATE VARIABLES
!+
!+   KSI,I,X,EA1:  INTERMEDIATE VARIABLES USED FOR THE CALCULATION OF
!+                 THE RISE.
!+                 RIGHT OF THE MOON (A) AND THE LUNAR DECLINATION (D)
!
!history  E. DAVID (LHF)    ;
!+        01/03/1994
!+        V5P2
!+   F LEPEINTRE (LNH) 30 87 78 54;
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
!!
!history  U.H.Merkel
!+        20/07/2012
!+        V6P2
!+    min to MINU
!
!history  User Thijslan on Telemac forum & J-M HERVOUET (EDF LAB, LNHE)
!+        24/10/2014
!+        V7P0
!+    Use of ATANC replaced by function ATAN2.
!+    ATAN2 used instead of ATAN for AS.
!+    Function ATANC can be removed from BIEF.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AL             |<--| MOON RIGHT ASCENSION
!| ARL            |<--| RATIO RT/TL
!| ARS            |<--| RATIO RT/TS
!| AS             |<--| SUN RIGHT ASCENSION
!| AT             |-->| TIME IN SECONDS
!| DAY            |-->| DAY
!| DL             |<--| MOON DECLINATION
!| DS             |<--| SUN DECLINATION
!| HOUR           |-->| HOUR
!| MINU           |-->| MINUTE
!| MONTH          |-->| DATE DU CALCUL DES TERMES ASTROS
!| SEC            |-->| SECOND
!| YEAR           |-->| DATE DU CALCUL DES TERMES ASTROS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: YEAR,MONTH,DAY,HOUR,MINU,SEC
      DOUBLE PRECISION, INTENT(IN)    :: AT
      DOUBLE PRECISION, INTENT(INOUT) :: ARL,ARS,DL,DS,AL,AS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION T,H,S,P,O,OM,L,CR,ET,MA,EA,TS,VS,LS
      DOUBLE PRECISION I0,E,M,UA,RT,C,AC,PI,API,EA1,KSI,I,X,NU
!
      INTRINSIC ACOS,ASIN,ATAN,COS,SIN,SQRT,TAN,ABS,MOD,ATAN2
!
      DOUBLE PRECISION DMO,JULTIM
      EXTERNAL         DMO,JULTIM
!
!-----------------------------------------------------------------------
!
      PI = 4.D0 * ATAN( 1.D0 )
      API = PI / 180.D0
      I0  = 5.145576994D0 * API
      E   = 0.05490D0
      M   = 0.074804D0
      UA  = 149503899.D0
      RT  = 6378.D0
      C   = 384403.D0
      AC  = RT / C
!
      T   = JULTIM(YEAR,MONTH,DAY,HOUR,MINU,SEC,AT)
!
      H   = DMO ( 279.69668D0 + 36000.76892D0       * T
     &                        + 0.0003025D0         * T**2 )
      S   = DMO ( 270.434164D0 + 481267.8831D0      * T
     &                         - 0.001133D0         * T**2
     &                         + 0.0000019D0        * T**3 )
      P   = DMO ( 334.328019444D0 + 4069.03220556D0 * T
     &                            - 0.01034D0       * T**2
     &                            + 0.0000125D0     * T**3 )
      O   = DMO ( 259.183275D0 - 1934.1420D0        * T
     &                         + 0.002078D0         * T**2
     &                         + 0.0000022D0        * T**3 )
      OM  = DMO ( 23.452294D0 - 0.0130125D0         * T
     &                        - 0.00000164D0        * T**2
     &                        + 0.000000503D0       * T**3 )
      L   = S + 2*E*SIN(S-P) +  5.D0/4.D0 *E*E*SIN(2*(S-P)) +
     &                         15.D0/4.D0 *M*E*SIN(S-2*H+P) +
     &                         11.D0/8.D0 *M*M*SIN(2*(S-H))
      CR  = 1.D0 + E*COS(S-P) +            E*E*COS(2*(S-P)) +
     &                         15.D0/8.D0 *M*E*COS(S-2*H+P) +
     &                                     M*M*COS(2*(S-H))
      KSI=MOD(O-ATAN(SIN(O)/(SIN(I0)/TAN(OM)+COS(I0)*COS(O))),PI)
!
! KSI VARIES FROM -12 TO +12 DEGREES IN 18.7 YEARS
!
      IF (KSI.GT.+API*13.D0) KSI=KSI-PI
      IF (KSI.LT.-API*13.D0) KSI=KSI+PI
!
! CALCULATES I
!
      I   = ACOS( COS(OM)*COS(I0) - SIN(OM)*SIN(I0)*COS(O) )
!
      ET  = 0.01675104D0 - 0.0000418D0 * T - 0.000000126D0 * T**2
      MA  = DMO ( 358.47583D0 + 35999.04975D0 * T
     &                        - 0.00015D0     * T**2
     &                        + 0.0000033D0   * T**3 )
      EA1 = MA
10    CONTINUE
      EA  = MA + ET * SIN (EA1)
      IF(ABS(EA-EA1).GT.1.D-12) THEN
        EA1=EA
        GOTO 10
      ENDIF
      TS  = 1.0000002D0 * ( 1.D0-ET*COS(EA) ) * UA
      VS  = 2.D0 * ATAN( SQRT((1.D0+ET)/(1.D0-ET)) * TAN(EA/2.D0) )
      LS  = H + VS - MA
!
! OUTPUT PARAMETERS
!
      ARL = AC*CR
      ARS = RT/TS
      DL  = ASIN(SIN(L-KSI)*SIN(I))
      DS  = ASIN(SIN(OM)*SIN(LS))
!     CALCULATES X SO THAT TAN(X) = TAN(L-KSI) * COS(I)
!     WITH X BETWEEN 0 AND 2 PI
      X  = ATAN2(SIN(L-KSI)*COS(I),COS(L-KSI))+PI
      NU = ATAN2(SIN(O),SIN(OM)/TAN(I0)+COS(OM)*COS(O))+PI
      AL = MOD(X+NU,2.D0*PI)
      AS = ATAN2(COS(OM)*SIN(LS),COS(LS))
!
!-----------------------------------------------------------------------
!
      RETURN
      END

