!                     ***********
                      MODULE TPXO
!                     ***********
!
!***********************************************************************
! TELEMAC2D   V8P4
!***********************************************************************
!
!brief    Module containing TPXO variables and subroutines
!+        TPXO is a tidal harmonic model based on the topex-poseidon
!+        satellite mission of sea surface observations.
!+        This module might also host variables from the other
!+        tidal model methodologies.
!
!history  G.Egbert of the Oregon State University (OSU)
!+        13/02/1996
!+        OTPS v 1.4
!+        Developments of the Tidal Prediction Software:
!+        OTPS (Fortran90 code) outside the TELEMAC system.
!+        ARGUMENTS and ASTROL subroutines supplied by
!+        R.Ray, March 1999, attached to OTIS
!
!history  Lana Erofeeva of the Oregon State University (OSU)
!+        23/02/1998
!+        OTPS v 1.5
!+        Gary's approach to minor constituents interpolation
!+        new FORTRAN version
!
!history  Lana Erofeeva of the Oregon State University (OSU)
!+        01/06/2004
!+        OTPS v 1.6
!+        OTPS based on R.Ray code perth2
!
!history  M.S.TURNBULL (HRW), N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+        Integration of OTPS in the TELEMAC system (for both 2D and 3D)
!
!history  S.E.BOURBAN (HRW)
!+        26/01/2016
!+        V7P2
!+        Replacement of COMPLEX numbers by their imaginary and real parts
!+        (to be compatible with algorithmic differentiation tools)
!
!history  J.RIEHME (ADJOINTWARE)
!+        26/12/2016
!+        V7P3
!+        Replacement of DATA statements equivalent PARAMETER statements
!+        (as no DATA statement allowed for allocatable user types)
!
!history  J.RIEHME (ADJOINTWARE)
!+        26/01/2017
!+        V7P3
!+        Removal of EQUIVALENCE statements
!+        (not allowed for algorithmic differentiation)
!
!reference RODNEY'S CONSTITUENT.H, 2/23/96
!
!reference RICHARD RAY'S
!+  "ARGUMENTS" AND "ASTROL", FOR JAN 1, 1992, 00:00 GREENWICH TIME
!+  CORRECTED JULY 12, 2000
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!history  C.-T. PHAM (LNHE)
!+        28/07/2019
!+        V8P1
!+        S1 is now taken into account (for TPXO9)
!
!history  C.-T. PHAM (LNHE)
!+        27/01/2022
!+        V8P4
!+        2Q1 and M3 are now taken into account (for TPXO9v5)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!note     THIS FILE IS ORGANISED IN SEVERAL PARTS:
!+      1ST: PARAMETERS AND VARIABLES OF THE TPXO MODEL
!+      ...
!+      2ND: CONTAINS SUBROUTINE AND FUNCTIONS OF THE TPXO MODEL
!+      ...
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      PRIVATE
      PUBLIC :: CONDI_TPXO,BORD_TIDE_TPXO,CD2MSL_TPXO,DEJA,DEALLOC_TPXO
!
!-----------------------------------------------------------------------
!
!       1a) TPXO -- CONSTIT
!
!-----------------------------------------------------------------------
!
!brief  THIS PART CONTAINS THE STANDARD PARAMETERS WHICH DEFINE THE
!+  AMPLITUDES, FREQUENCIES, ETC. FOR THE PRIMARY TIDAL CONSTITUENTS
!+  (CURRENTLY 29)
!
      INTEGER, PARAMETER :: TPXO_NCMX = 32
      CHARACTER(LEN=4) :: TPXO_CONSTID(TPXO_NCMX)
      PARAMETER ( TPXO_CONSTID = (
     &            /'m2  ','s2  ','k1  ','o1  ',
     &             'n2  ','p1  ','k2  ','q1  ',
     &             '2n2 ','mu2 ','nu2 ','l2  ',
     &             't2  ','j1  ','m1  ','oo1 ',
     &             'rho1','mf  ','mm  ','ssa ',
     &             'm4  ','ms4 ','mn4 ','m6  ',
     &             'm8  ','mk3 ','s6  ','2sm2',
     &             '2mk3','s1  ','2q1 ','m3  '/) )
!
!brief  FOR EACH CONSTITUENT, THE FOLLOWING PARAMETERS ARE GIVEN:
!+  - ALPHA = CORRECTION FACTOR FOR FIRST ORDER LOAD TIDES
!+  - AMP = AMPLITUDE OF EQUILIBRIUM TIDE, IN M
!+  - PH = CURRENTLY SET TO ZERO ...
!+            PHASES FOR EACH CONSTITUENT ARE REFERRED TO THE TIME
!+            WHEN THE PHASE OF THE FORCING FOR THAT CONSTITUENT
!+            IS ZERO ON THE GREENWICH MERIDIAN
!+  - OMEGA = ANGULAR FREQUENCY OF CONSTITUENT, IN RADIANS/S
!+  TIDAL PARAMETERS TAKEN FROM RODNEY'S CONSTITUENT.H, 2/23/96:
!+     (EXCEPT FOR ISPEC).
!
      DOUBLE PRECISION :: TPXO_OMEGA_D(TPXO_NCMX)
      DOUBLE PRECISION :: TPXO_PHASE_MKB(TPXO_NCMX)
      DOUBLE PRECISION :: TPXO_BETA_SE(TPXO_NCMX)
!
!     DOUBLE PRECISION TPXO_ALPHA_D(TPXO_NCMX)
!     DATA TPXO_ALPHA_D/
!    &    0.693,       0.693,       0.736,       0.695,
!    &    0.693,       0.706,       0.693,       0.695,
!    &    0.693,       0.693,       0.693,       0.693,
!    &    0.693,       0.695,       0.695,       0.695,
!    &    0.695,       0.693,       0.693,       0.693,
!    &    0.693,       0.693,       0.693,       0.693,
!    &    0.693,       0.693,       0.693,       0.693,
!    &    0.693/
!
!     DOUBLE PRECISION TPXO_AMP_D(TPXO_NCMX)
!     DATA TPXO_AMP_D/
!    &    0.242334,    0.112743,    0.141565,    0.100661,
!    &    0.046397,    0.046848,    0.030684,    0.019273,
!    &    0.006141,    0.007408,    0.008811,    0.006931,
!    &    0.006608,    0.007915,    0.007915,    0.004338,
!    &    0.003661,    0.042041,    0.022191,    0.019567,
!         AMPLITUDE FOR M4 ETC. IS ZERO
!    &    0.,          0.,          0.,          0.,
!    &    0.,          0.,          0.,          0.,
!    &    0./
!
!     ORIGINAL VALUES WITH 7 DIGITS. ACCURACY HAS BEEN INCREASED AFTER
!
!$$$      DATA TPXO_OMEGA_D/
!$$$     &    1.405189D-04,1.454441D-04,7.292117D-05,6.759774D-05,
!$$$     &    1.378797D-04,7.252295D-05,1.458423D-04,6.495854D-05,
!$$$     &    1.352405D-04,1.355937D-04,1.382329D-04,1.431581D-04,
!$$$     &    1.452450D-04,7.556036D-05,7.028195D-05,7.824458D-05,
!$$$     &    6.531174D-05,0.053234D-04,0.026392D-04,0.003982D-04,
!$$$     &    2.810377D-04,2.859630D-04,2.783984D-04,4.215566D-04,
!$$$     &    5.620755D-04,2.134402D-04,4.363323D-04,1.503693D-04,
!$$$     &    2.081166D-04/
!
!     MORE ACCURATE VALUES
!
!     NO DATA STATEMENT FOR ALLOCATABLE USER TYPES ALLOWED
      PARAMETER ( TPXO_OMEGA_D = (/
     &    1.405189025757300D-4,1.454441043328608D-4,
     &    7.292115854682400D-5,6.759774402890597D-5,
     &    1.378796995659399D-4,7.252294578603679D-5,
     &    1.458423170936480D-4,6.495854101911592D-5,
     &    1.352404965561499D-4,1.355937008185992D-4,
     &    1.382329038283892D-4,1.431581055855200D-4,
     &    1.452450074605617D-4,7.556036155661406D-5,
     &    7.028195553703394D-5,7.824457306474201D-5,
     &    6.531174528156523D-5,5.323414517918016D-6,
     &    2.639203009790057D-6,3.982127607872015D-7,
     &    2.810378051514599D-4,2.859630069085908D-4,
     &    2.783986021416699D-4,4.215567077271900D-4,
     &    5.620756103029198D-4,2.134400611225540D-4,
     &    4.363323129985824D-4,1.503693060899916D-4,
     &    2.081166445429299D-4,7.272205216643039D-5,
     &    6.231933800932588D-5,2.107783538635950D-4/
     & ) )
!
!brief   ASTRONOMICAL ARGUMENTS, OBTAINED WITH RICHARD RAY'S
!+  "ARGUMENTS" AND "ASTROL", FOR JAN 1, 1992, 00:00 GREENWICH TIME
!+  CORRECTED JULY 12, 2000
!
!
!     ORIGINAL VALUES WITH 7 DIGITS. ACCURACY HAS BEEN INCREASED AFTER
!
!$$$      DATA TPXO_PHASE_MKB/
!$$$     &    1.731557546D0, 0.000000000D0, 0.173003674D0, 1.558553872D0,
!$$$     &    6.050721243D0, 6.110181633D0, 3.487600001D0, 5.877717569D0,
!$$$     &    4.086699633D0, 3.463115091D0, 5.427136701D0, 0.553986502D0,
!$$$     &    0.052841931D0, 2.137025284D0, 2.436575100D0, 1.929046130D0,
!$$$     &    5.254133027D0, 1.756042456D0, 1.964021610D0, 3.487600001D0,
!$$$     &    3.463115091D0, 1.731557546D0, 1.499093481D0, 5.194672637D0,
!$$$     &    6.926230184D0, 1.904561220D0, 0.000000000D0, 4.551627762D0,
!$$$     &    3.809122439D0/
!
!     MORE ACCURATE VALUES
!
!     2MK3 MAY BE WRONG, IT IS DIFFERENT FROM THE ORIGINAL VALUE
!     M8 ARE THE SAME, MODULO TWO*PI
!
!     NO DATA STATEMENT FOR ALLOCATABLE USER TYPES ALLOWED
      PARAMETER ( TPXO_PHASE_MKB = (/
     &    1.73155754567656D0,  0.00000000000000D0,
     &    0.173003673872453D0, 1.55855387180411D0,
     &    6.05072124295143D0,  6.11018163330713D0,
     &    3.48760000133470D0,  5.87771756907898D0,
     &    4.08669963304672D0,  3.46311509135312D0,
     &    5.42713670125784D0,  0.553986501991483D0,
     &    5.284193133561309D-2,2.13702528377717D0,
     &    2.43657509963318D0,  1.92904612953059D0,
     &    5.25413302738539D0,  1.75604245565814D0,
     &    1.96402160990471D0,  3.48760000133470D0,
     &    3.46311509135312D0,  1.73155754567656D0,
     &    1.49909348144841D0,  5.19467263702969D0,
     &    0.643044875526663D0, 1.90456121954902D0,
     &    0.00000000000000D0,  4.55162776150302D0,
     &    3.29011141748067D0,  0.00000000000000D0,
     &    3.91369595917427D0,  2.59733631851484D0/
     & ) )
!
!note I AM PUTTING 0 FOR MS2, MN4 ETC. FOR NOW: CORRECT LATER
!+ NOW THIS CORRECTION IS DONE USING THE SAL FILE (H_TPXO3_90-90.LOAD)
!+ I REPLACE BETA_SE WITH UNITS FOR NOW (IN CASE WE DECIDE TO SWITCH
!+ BACK TO OLD VERSION) AND COMMENT THE OLD NUMBERS - THIS WAY I DO
!+ NOT CHANGE ANYTHING IN SUBROUTINES
!+ THIS WAS IN WEIGHTS.f BEFORE - PLACED HERE NOT TO MIX WITH W!
!+ TO REMOVE SOLID EARTH TIDE MULTIPLY BY BETA:
!
!     NO DATA STATEMENT FOR ALLOCATABLE USER TYPES ALLOWED
      PARAMETER ( TPXO_BETA_SE = (/
     &    0.9540D0,      0.9540D0,      0.9400D0,      0.9400D0,
     &    0.9540D0,      0.9400D0,      0.9540D0,      0.9400D0,
     &    0.9540D0,      0.9540D0,      0.9540D0,      0.9540D0,
     &    0.9540D0,      0.9400D0,      0.9400D0,      0.9400D0,
     &    0.9400D0,      0.9400D0,      0.9400D0,      0.9400D0,
!         FOR M4 JUST USING VALUE FOR SEMI-DIURNALS (NO GOOD REASON!)
     &    0.9540D0,      0.9540D0,      0.9540D0,      0.954D0,
     &    0.9540D0,      0.9540D0,      0.9540D0,      0.954D0,
!         FOR 2Q1 USING VALUE FROM CONSTIT.H
!         NOT A PROBLEM AS WHEN CALLING MAKE_A, 1ST ARGUMENT IS
!         FALSE AND LAST IS TRUE => TPXO_BETA_SE NOT USED
     &    0.9540D0,      0.9400D0,      0.9540D0,      0.9540D0/
     & ) )
!      DATA BETA_SE/29*1./
!
!     INTEGER TPXO_ISPEC_D(TPXO_NCMX)
!     DATA TPXO_ISPEC_D/
!    &    2,2,1,1,
!    &    2,1,2,1,
!    &    2,2,2,2,
!    &    2,1,1,1,
!    &    1,0,0,0,
!         NOTE: FOR NOW ISPEC FOR M4 SET TO 0 (ISPEC IS ONLY USED
!         TO DEFINE FORCING IN ATGF, AND THIS IS ALWAYS 0 FOR M4)
!    &    0,0,0,0,
!    &    0,0,0,0,
!    &    0/
!
!-----------------------------------------------------------------------
!
!       1b) TPXO -- WEIGHTS FOR M2, S2, K1, O1, N2, P1, K2, Q1
!
!-----------------------------------------------------------------------
!
!warning  This is good only for 8 constituents in order:
!+        M2, S2, K1, O1, N2, P1, K2, Q1
!
!brief   THE SAME ORDER IS SUPPORTED IN THE PREVIOUS SECTION (CONSTIT)
!+  I.E. NO NEED TO CARE ABOUT THE CORRESPONDING INDICES
!
!        FOR CASE OF USING MODULE WEIGHTS
      INTEGER, PARAMETER :: TPXO_NCON = 17
!
      DOUBLE PRECISION :: TPXO_W(TPXO_NCON,8)
!
!     NO DATA STATEMENT FOR ALLOCATABLE USER TYPES ALLOWED
!      DATA TPXO_W(1,:) /1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0/
!      DATA TPXO_W(2,:) /0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0/
!      DATA TPXO_W(3,:) /0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0/
!      DATA TPXO_W(4,:) /0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 0.D0/
!      DATA TPXO_W(5,:) /0.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0/
!      DATA TPXO_W(6,:) /0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0/
!      DATA TPXO_W(7,:) /0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0/
!      DATA TPXO_W(8,:) /0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D0/
!      DATA TPXO_W(9,:) /-0.0379D0, 0.D0,0.D0,0.D0,
!     &                  0.30859D0,0.D0, 0.03289D0,0.D0/
!      DATA TPXO_W(10,:)/-0.03961D0,0.D0,0.D0,0.D0,
!     &                  0.34380D0,0.D0, 0.03436D0,0.D0/
!      DATA TPXO_W(11,:)/ 0.00696D0,0.D0,0.D0,0.D0,
!     &                   0.15719D0,0.D0,-0.00547D0,0.D0/
!      DATA TPXO_W(12,:)/ 0.02884D0,0.D0,0.D0,0.D0,
!     &                  -0.05036D0,0.D0, 0.07424D0,0.D0/
!      DATA TPXO_W(13,:)/ 0.00854D0,0.D0,0.D0,0.D0,-0.01913D0,0.D0,
!     &                   0.17685D0,0.D0/
!      DATA TPXO_W(14,:)/0.D0,0.D0,-0.00571D0,0.11234D0,0.D0,
!     &                  0.05285D0,0.D0,-0.26257D0/
!      DATA TPXO_W(15,:)/0.D0,0.D0, 0.00749D0,0.07474D0,0.D0,
!     &                  0.03904D0,0.D0,-0.12959D0/
!      DATA TPXO_W(16,:)/0.D0,0.D0,-0.03748D0,0.12419D0,0.D0,
!     &                  0.05843D0,0.D0,-0.29027D0/
!      DATA TPXO_W(17,:)/0.D0,0.D0,0.00842D0,0.01002D0,0.D0,
!     &                 -0.03064D0,0.D0,0.15028D0/
      PARAMETER  ( TPXO_W = RESHAPE( (/
     &   1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0,
     &   0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0,
     &   0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0,
     &   0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 0.D0,
     &   0.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0,
     &   0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0,
     &   0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0,
     &   0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D0,
     &   -0.0379D0, 0.D0,0.D0,0.D0,0.30859D0,0.D0, 0.03289D0,0.D0,
     &   -0.03961D0,0.D0,0.D0,0.D0,0.34380D0,0.D0, 0.03436D0,0.D0,
     &    0.00696D0,0.D0,0.D0,0.D0, 0.15719D0,0.D0,-0.00547D0,0.D0,
     &    0.02884D0,0.D0,0.D0,0.D0,-0.05036D0,0.D0, 0.07424D0,0.D0,
     &    0.00854D0,0.D0,0.D0,0.D0,-0.01913D0,0.D0, 0.17685D0,0.D0,
     &   0.D0,0.D0,-0.00571D0,0.11234D0,0.D0,0.05285D0,0.D0,-0.26257D0,
     &   0.D0,0.D0, 0.00749D0,0.07474D0,0.D0,0.03904D0,0.D0,-0.12959D0,
     &   0.D0,0.D0,-0.03748D0,0.12419D0,0.D0,0.05843D0,0.D0,-0.29027D0,
     &   0.D0,0.D0,0.00842D0,0.01002D0,0.D0,-0.03064D0,0.D0,0.15028D0
     &  /), SHAPE=(/ 17,8 /) ) )
!
!-----------------------------------------------------------------------
!
!
!     NUMBER OF CONSTITUENTS TURNED ON
!
      INTEGER NCON
!
!     NUMBER OF CONSTITUENTS TURNED ON FOR SCHEMATIC TIDES
!
      INTEGER NCON2
!
!     INDICES OF THE CONSTITUENTS TURNED ON FOR SCHEMATIC TIDES
!
      INTEGER, ALLOCATABLE :: INDW(:)
!
!     INDICES OF AVAILABLE CONSTITUENTS AMONGST THE ALL POSSIBLE
!
      CHARACTER(LEN=4) C_ID(TPXO_NCMX)
!     SAME AS C_ID FOR SCHEMATIC TIDES
!     BUT ONLY FOR DIURNAL, SEMIDIUARNAL AND FOURTH-DIURNAL WAVES
      CHARACTER(LEN=4), ALLOCATABLE :: C_ID2(:)
!
      INTEGER, ALLOCATABLE :: CCIND(:)
!     SAME AS CCIND FOR SCHEMATIC TIDES
!     BUT ONLY FOR DIURNAL, SEMIDIUARNAL AND FOURTH-DIURNAL WAVES
      INTEGER, ALLOCATABLE :: CCIND2(:)
!
!     INTERPOLATED CONSTITUENTS FOR LIQUID BOUNDARY NODES
!
      INTEGER, ALLOCATABLE :: TPXO_NFR(:)
!      COMPLEX(KIND(1.D0)), ALLOCATABLE :: TPXO_BOR(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: TPXO_BOR_R(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: TPXO_BOR_I(:,:,:)
!     SAME AS TPXO_BOR FOR SCHEMATIC TIDES
!     BUT ONLY FOR DIURNAL, SEMIDIUARNAL AND FOURTH-DIURNAL WAVES
!      COMPLEX(KIND(1.D0)), ALLOCATABLE :: TPXO_BOR2(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: TPXO_BOR2_R(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: TPXO_BOR2_I(:,:,:)
!     TO CALIBRATE HIGH WATER FOR SCHEMATIC TIDES
!      COMPLEX(KIND(1.D0)), ALLOCATABLE :: BOR2HW(:)
      DOUBLE PRECISION, ALLOCATABLE :: BOR2HW_R(:), BOR2HW_I(:)
      DOUBLE PRECISION, ALLOCATABLE :: ARGHW(:)
!
!     WHETHER DATA ARE ALREADY READ OR NOT
!
      LOGICAL :: DEJA = .FALSE.
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!                   ******************
                    SUBROUTINE DEF_CID
!                   ******************
!
     &( NC0, CID, IND )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  C_ID          |-->| NAME OF AVAILABLE CONTITUENTS
!|                |   | AMONGST THE ALL POSSIBLE
!|  IND           |<--| INDICES OF AVAILABLE CONTITUENTS
!|                |   | AMONGST THE ALL POSSIBLE
!|  NC0           |-->| NUMBER OF CONSTITUENTS AVAILABLE IN THE FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NC0
      INTEGER, INTENT(OUT)         :: IND(NC0)
      CHARACTER(LEN=4), INTENT(IN) :: CID(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IC,JC,K
!
!-----------------------------------------------------------------------
!
      K = 1
      DO IC = 1,NC0
        IND(K) = 0
        DO JC = 1,TPXO_NCMX
          IF( CID(IC).EQ.TPXO_CONSTID(JC) ) THEN
            IND(K) = JC
            EXIT
          ENDIF
        ENDDO
        IF( IND(K).EQ.0 ) THEN
          WRITE(LU,*) 'TPXO : WARNING:' //
     &       'CONSTITUENT ID ',CID(IC),' IS NOT ALLOWED'
        ENDIF
        K = K + 1
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE DEF_CID


!             ************************
              INTEGER FUNCTION IPSHFT
!             ************************
!
     &( I,ISH,N )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    PERIODIC SHIFT MAPS I TO I+ISH , MOD N;
!+        (ALWAYS BETWEEN 1 AND N;  NEVER 0 )
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  I             |-->| INDEX TO BE SHIFTED
!|  ISH           |-->| SHIFT
!|  N             |-->| CLASS OF MODULO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: I,N,ISH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IPSHFT = MOD( I+ISH+N-1,N ) + 1
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION IPSHFT

!               ********************************
                DOUBLE PRECISION FUNCTION HEIGHT
!               ********************************
!
     &( A_R,A_I,P_R,P_I,NC )
!
!***********************************************************************
! TELEMAC2D   V7P2                                   26/01/2016
!***********************************************************************
!
!brief    RETURNS HEIGHT FROM MODEL ARRAY OF COMPLEX CONSTITUENTS
!
!history  S.E.BOURBAN (HRW)
!+        26/01/2016
!+        V7P2
!+        Replacement of COMPLEX numbers by their imaginary and real parts
!+        (to be compatible with algorithmic differentiation tools)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  A_I           |-->| ARRAY OF IMAGINARY PARTS COMPLEX CONSTITUENTS
!|  A_R           |-->| ARRAY OF REAL PARTS OF COMPLEX CONSTITUENTS
!|  NC            |-->| SIZE OF THE ARRAYS
!|  P_I           |-->| ARRAY OF IMAGINARY PARTS COMPLEX CONSTITUENTS
!|  P_R           |-->| ARRAY OF REAL PARTS OF COMPLEX CONSTITUENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NC
!      COMPLEX(KIND(1.D0)), INTENT(IN) :: A(NC),P(NC)
      DOUBLE PRECISION, INTENT(IN) :: A_R(NC),A_I(NC),P_R(NC),P_I(NC)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      HEIGHT = 0.D0
!
      IF( NC.NE.0 ) THEN
!     HEIGHT(I)=SUM_OF_REAL(A(I)*P(I))
        DO I = 1,NC
          HEIGHT = HEIGHT + P_R(I)*A_R(I) - P_I(I)*A_I(I)
!     &           + REAL(P(I))*REAL(A(I))-AIMAG(P(I))*AIMAG(A(I))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION HEIGHT

!                   ******************
                    SUBROUTINE INTERPT
!                   ******************
!
     &( UV_R,UV_I,NT,N,M,MZ,TH_LIM,PH_LIM,XLAT,XLON,
     &  UV1_R,UV1_I,IERR,ZUV )
!
!***********************************************************************
! TELEMAC2D   V7P2                                   26/01/2016
!***********************************************************************
!
!brief    INTERPOLATES OLD COMPLEX ARRAY UV(NT,N,M) AT POINT XLAT,XLON
!+        TH_LIM AND PH_LIM GIVE LATITUDE AND LONGITUDE LIMITS OF GRID
!+        OLD COMPLEX ARRAY HAS BECOME 2 REAL ARRAYS SINCE V7P2
!
!history  S.E.BOURBAN (HRW)
!+        26/01/2016
!+        V7P2
!+        Replacement of COMPLEX numbers by their imaginary and real parts
!+        (to be compatible with algorithmic differentiation tools)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  IERR          |<--| ERROR INDEX
!|  M             |-->| ONE SIZE OF ARRAY UV
!|  MZ            |-->| MASK (=1 FOR OCEAN NODE)
!|  N             |-->| ONE SIZE OF ARRAY UV
!|  NT            |-->| NUMBER OF CONSTITUENTS TURNED ON (NCON)
!|  PH_LIM        |-->| LONGITUDE LIMITS OF GRID
!|  TH_LIM        |-->| LATITUDE LIMITS OF GRID
!|  UV_I          |-->| ARRAY OF IMAGINARY PART OF COMPLEX ARRAY
!|                |   | TO BE INTERPOLATED AT POINT XLAT,XLON
!|  UV_R          |-->| ARRAY OF REAL PART OF COMPLEX ARRAY
!|                |   | TO BE INTERPOLATED AT POINT XLAT,XLON
!|  UV1_I         |<--| INTERPOLATION OF UV_I AT POINT XLAT,XLON
!|  UV1_R         |<--| INTERPOLATION OF UV_R AT POINT XLAT,XLON
!|  XLAT          |-->| LATITUDE OF THE POINT WHERE INTERPOLATED
!|  XLON          |-->| LONGITUDE OF THE POINT WHERE INTERPOLATED
!|  ZUV           |-->| C-GRID ZUV: 'u','v','z' ('U','V','Z')
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)  :: N,M
      INTEGER, INTENT(OUT) :: IERR
      INTEGER, INTENT(IN)  :: MZ(N,M),NT
      CHARACTER(LEN=1), INTENT(IN) :: ZUV
      DOUBLE PRECISION, INTENT(IN) :: TH_LIM(2),PH_LIM(2)
      DOUBLE PRECISION, INTENT(IN) :: XLON,XLAT
!      COMPLEX, INTENT(IN)  :: UV(NT,N,M)
      REAL, INTENT(IN) :: UV_R(NT,N,M),UV_I(NT,N,M)
!      COMPLEX(KIND(1.D0)), INTENT(OUT) :: UV1(NT)
      DOUBLE PRECISION, INTENT(OUT) :: UV1_R(NT),UV1_I(NT)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K
      INTEGER IW(0:1),JW(0:1)
!
      DOUBLE PRECISION WW(0:1,0:1),DLAT,DLON
      DOUBLE PRECISION DTHETA,DPHI,XLONC
!
!-----------------------------------------------------------------------
!
      IERR = 0
      DTHETA = ( TH_LIM(2)-TH_LIM(1) )/( REAL(M) )
      DPHI   = ( PH_LIM(2)-PH_LIM(1) )/( REAL(N) )
!
!     CHECKS XLON LONGITUDE CONVENTION
!
      XLONC = XLON
      IF( XLONC.LT.PH_LIM(1) ) XLONC = XLONC + 360.D0
      IF( XLONC.GT.PH_LIM(2) ) XLONC = XLONC - 360.D0
      IF( XLONC.LT.PH_LIM(1).OR.XLONC.GT.PH_LIM(2) ) THEN
        IERR = -1
        RETURN
      END IF
      IF( XLAT.LT.TH_LIM(1).OR.XLAT.GT.TH_LIM(2) ) THEN
        IERR = -1
        RETURN
      END IF
      DLAT = XLAT
      DLON = XLONC
!
      CALL BSI_WEIGHTS( ZUV,DLAT,DLON,TH_LIM,PH_LIM,
     &                 DPHI,DTHETA,MZ,N,M,WW,IW,JW )
!
      IF( (WW(0,0)+WW(1,0)+WW(0,1)+WW(1,1)).LT.0.01D0 ) THEN
        IERR = -2
        DO K = 1,NT
!          UV1(K) = CMPLX(0.D0,0.D0,KIND(1.D0))
          UV1_R(K) = 0.D0
          UV1_I(K) = 0.D0
        ENDDO
      ELSE
        IERR = 0
        DO K = 1,NT
!          UV1(K) = UV(K,IW(0),JW(0))*WW(0,0) +
!     &             UV(K,IW(1),JW(0))*WW(1,0) +
!     &             UV(K,IW(0),JW(1))*WW(0,1) +
!     &             UV(K,IW(1),JW(1))*WW(1,1)
          UV1_R(K) = UV_R(K,IW(0),JW(0))*WW(0,0)
     &             + UV_R(K,IW(1),JW(0))*WW(1,0)
     &             + UV_R(K,IW(0),JW(1))*WW(0,1)
     &             + UV_R(K,IW(1),JW(1))*WW(1,1)
          UV1_I(K) = UV_I(K,IW(0),JW(0))*WW(0,0)
     &             + UV_I(K,IW(1),JW(0))*WW(1,0)
     &             + UV_I(K,IW(0),JW(1))*WW(0,1)
     &             + UV_I(K,IW(1),JW(1))*WW(1,1)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE INTERPT

!                   ************************
                    SUBROUTINE INTERPT_SCHEM
!                   ************************
!
     &( UV_R,UV_I,NT,NT2,INDW,N,M,MZ,TH_LIM,PH_LIM,XLAT,XLON,
     &  UV1_R,UV1_I,IERR,ZUV )
!
!***********************************************************************
! TELEMAC2D   V7P2                                   26/01/2016
!***********************************************************************
!
!brief    INTERPOLATES OLD COMPLEX ARRAY UV(NT,N,M) AT POINT XLAT,XLON
!+        TH_LIM AND PH_LIM GIVE LATITUDE AND LONGITUDE LIMITS OF GRID
!+        FOR SCHEMATIC TIDES
!+        INDW + NT2: ONLY A PART OF THE AVAILABLE CONSTITUENTS IS USED
!+        OLD COMPLEX ARRAY HAS BECOME 2 REAL ARRAYS SINCE V7P2
!
!history  S.E.BOURBAN (HRW)
!+        26/01/2016
!+        V7P2
!+        Replacement of COMPLEX numbers by their imaginary and real parts
!+        (to be compatible with algorithmic differentiation tools)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  IERR          |<--| ERROR INDEX
!|  INDW          |-->| INDICES FOR SCHEMATIC TIDES
!|  M             |-->| ONE SIZE OF ARRAY UV
!|  MZ            |-->| MASK (=1 FOR OCEAN NODE)
!|  N             |-->| ONE SIZE OF ARRAY UV
!|  NT            |-->| NUMBER OF CONSTITUENTS TURNED ON (NCON)
!|  NT2           |-->| NUMBER OF CONSTITUENTS TURNED ON (NCON2)
!|                |   | FOR SCHEMATIC TIDES
!|  PH_LIM        |-->| LONGITUDE LIMITS OF GRID
!|  TH_LIM        |-->| LATITUDE LIMITS OF GRID
!|  UV_I          |-->| ARRAY OF IMAGINARY PART OF COMPLEX ARRAY
!|                |   | TO BE INTERPOLATED AT POINT XLAT,XLON
!|  UV_R          |-->| ARRAY OF REAL PART OF COMPLEX ARRAY
!|                |   | TO BE INTERPOLATED AT POINT XLAT,XLON
!|  UV1_I         |<--| INTERPOLATION OF UV_I AT POINT XLAT,XLON
!|  UV1_R         |<--| INTERPOLATION OF UV_R AT POINT XLAT,XLON
!|  XLAT          |-->| LATITUDE OF THE POINT WHERE INTERPOLATED
!|  XLON          |-->| LONGITUDE OF THE POINT WHERE INTERPOLATED
!|  ZUV           |-->| C-GRID ZUV: 'u','v','z' ('U','V','Z')
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)  :: N,M
      INTEGER, INTENT(OUT) :: IERR
      INTEGER, INTENT(IN)  :: MZ(N,M),NT
      INTEGER, INTENT(IN)  :: NT2,INDW(NT2)
      CHARACTER(LEN=1), INTENT(IN) :: ZUV
      DOUBLE PRECISION, INTENT(IN) :: TH_LIM(2),PH_LIM(2)
      DOUBLE PRECISION, INTENT(IN) :: XLON,XLAT
!      COMPLEX, INTENT(IN)  :: UV(NT,N,M)
      REAL, INTENT(IN) :: UV_R(NT,N,M),UV_I(NT,N,M)
!      COMPLEX(KIND(1.D0)), INTENT(OUT) :: UV1(NT2)
      DOUBLE PRECISION, INTENT(OUT) :: UV1_R(NT2),UV1_I(NT2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K
      INTEGER IW(0:1),JW(0:1)
!
      DOUBLE PRECISION WW(0:1,0:1),DLAT,DLON
      DOUBLE PRECISION DTHETA,DPHI,XLONC
!
!-----------------------------------------------------------------------
!
      IERR = 0
      DTHETA = ( TH_LIM(2)-TH_LIM(1) )/( REAL(M) )
      DPHI   = ( PH_LIM(2)-PH_LIM(1) )/( REAL(N) )
!
!     CHECKS XLON LONGITUDE CONVENTION
!
      XLONC = XLON
      IF( XLONC.LT.PH_LIM(1) ) XLONC = XLONC + 360.D0
      IF( XLONC.GT.PH_LIM(2) ) XLONC = XLONC - 360.D0
      IF( XLONC.LT.PH_LIM(1).OR.XLONC.GT.PH_LIM(2) ) THEN
        IERR = -1
        RETURN
      END IF
      IF( XLAT.LT.TH_LIM(1).OR.XLAT.GT.TH_LIM(2) ) THEN
        IERR = -1
        RETURN
      END IF
      DLAT = XLAT
      DLON = XLONC
!
      CALL BSI_WEIGHTS( ZUV,DLAT,DLON,TH_LIM,PH_LIM,
     &                 DPHI,DTHETA,MZ,N,M,WW,IW,JW )
!
      IF( (WW(0,0)+WW(1,0)+WW(0,1)+WW(1,1)).LT.0.01D0 ) THEN
        IERR = -2
        DO K = 1,NT2
!          UV1(K) = CMPLX(0.D0,0.D0,KIND(1.D0))
          UV1_R(K) = 0.D0
          UV1_I(K) = 0.D0
        ENDDO
      ELSE
        IERR = 0
        DO K = 1,NT2
!          UV1(K) = UV(INDW(K),IW(0),JW(0))*WW(0,0) +
!     &             UV(INDW(K),IW(1),JW(0))*WW(1,0) +
!     &             UV(INDW(K),IW(0),JW(1))*WW(0,1) +
!     &             UV(INDW(K),IW(1),JW(1))*WW(1,1)
          UV1_R(K) = UV_R(INDW(K),IW(0),JW(0))*WW(0,0)
     &             + UV_R(INDW(K),IW(1),JW(0))*WW(1,0)
     &             + UV_R(INDW(K),IW(0),JW(1))*WW(0,1)
     &             + UV_R(INDW(K),IW(1),JW(1))*WW(1,1)
          UV1_I(K) = UV_I(INDW(K),IW(0),JW(0))*WW(0,0)
     &             + UV_I(INDW(K),IW(1),JW(0))*WW(1,0)
     &             + UV_I(INDW(K),IW(0),JW(1))*WW(0,1)
     &             + UV_I(INDW(K),IW(1),JW(1))*WW(1,1)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE INTERPT_SCHEM

!                   **********************
                    SUBROUTINE BSI_WEIGHTS
!                   **********************
!
     &(ZUVM,THETA,PHI,THETA_LIM,PHI_LIM,DX,DY,MASK,N,M,WW,IW,JW)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    BILINEAR SPLINE INTERPOLATION WEIGHTS FOR DELTA-FORCING
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  DX            |-->| STEP X
!|  DY            |-->| STEP Y
!|  IW            |<--| INDEX FOR INTERPOLATION
!|  JW            |<--| INDEX FOR INTERPOLATION
!|  M             |-->| GRID DIMENSION
!|  MASK          |-->| MASK
!|  N             |-->| GRID DIMENSION
!|  PHI           |-->| LONGITUDE
!|  PHI_LIM       |-->| GRID LIMITS
!|  THETA         |-->| LATITUDE
!|  THETA_LIM     |-->| GRID LIMITS
!|  WW            |<--| INTERPOLATION
!|  ZUVM          |-->| C-GRID ZUV: 'u','v','z' ('U','V','Z')
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)  :: N,M
      INTEGER, INTENT(IN)  :: MASK(N,M)
      INTEGER, INTENT(OUT) :: IW(0:1),JW(0:1)
      CHARACTER(LEN=1), INTENT(IN) :: ZUVM
      DOUBLE PRECISION, INTENT(IN)  :: THETA,PHI
      DOUBLE PRECISION, INTENT(IN)  :: THETA_LIM(2),PHI_LIM(2)
      DOUBLE PRECISION, INTENT(IN)  :: DX,DY
      DOUBLE PRECISION, INTENT(OUT) :: WW(0:1,0:1)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I0,J0,I1,J1
      INTEGER SM !,IPSHFT
      CHARACTER(LEN=1) ZUV
      DOUBLE PRECISION XI,XJ,X,Y
      DOUBLE PRECISION W00,W01,W10,W11,WTOT
!
!-----------------------------------------------------------------------
!
!     COPY OF ZUVM (SMALL OR CAPITAL), BECAUSE ZUVM MAY NOT BE CHANGED
!
      ZUV = ZUVM
!
      IF( ZUV.EQ.'U' ) ZUV = 'u'
      IF( ZUV.EQ.'V' ) ZUV = 'v'
      IF( ZUV.EQ.'Z' ) ZUV = 'z'
!
      IF( ZUV.NE.'u'.AND.ZUV.NE.'v'.AND.ZUV.NE.'z' ) THEN
        WRITE(LU,*) 'TPXO : NO ',ZUV,' NODES'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF( ZUV.EQ.'z' ) THEN
!
!     FINDS GRID COORDINATES FOR LOWER LEFT HAND Z NODE
!     AMONG THE SQUARE (OF Z NODES WHICH SURROUND THE OBS POINT)
!
!                 |
!                \|/
!     -> -z---u---z---u---z--
!         |       |       |     x IS THE LOCATION GIVEN BY THETA,PHI
!         v       v       v     THE FOUR SURROUNDING ELEVATION
!         | x     |       |     NODES ARE USED FOR THE BILINEAR
!     -> -z---u---z---u---z-    SPLINE INTERPOLATION
!         |      /|\      |
!                 |
!
        XI = (PHI  -PHI_LIM(1)  )/DX + 0.5D0
        XJ = (THETA-THETA_LIM(1))/DY + 0.5D0
!
      ELSEIF( ZUV.EQ.'u' ) THEN
!
!     INTERIOR POINT: MEASUREMENT OF CURRENT VECTOR
!                     DIRECTION IS GIVEN AS UNIT VECTOR (TH,PH)
!                     (TH,PH)=(1.,0.)->U
!                     (TH,PH)=(0.,1.)->V
!
!     FINDS GRID COORDINATES FOR LOWER LEFT HAND Z NODE
!     AMONG THE SQUARE (OF Z NODES WHICH SURROUND THE OBS POINT)
!
!             u---z---u--
!             |       |         x IS THE LOCATION GIVEN BY THETA,PHI
!             |   v---|---v     THE 8 SURROUNDING U,V NODES
!             |   | x |   |     ARE USED FOR THE BILINEAR SPLINE
!             u---z---u---z-    INTERPOLATION
!                 |       |     CURRENT VECTOR DIRECTION IS GIVEN
!                 v-------v     WITH A UNIT VECTOR (TH,PH):
!                               (1.,0)->EW, (0.,1.)->NS
!
        XI = (PHI  -PHI_LIM(1)  )/DX + 1.D0
        XJ = (THETA-THETA_LIM(1))/DY + 0.5D0
!
      ELSEIF( ZUV.EQ.'v' ) THEN
!
        XI = (PHI  -PHI_LIM(1)  )/DX + 0.5D0
        XJ = (THETA-THETA_LIM(1))/DY + 1.D0
!
      ENDIF
!
      IF( XI.LT.1.D0 ) XI = REAL(N) + XI
      I0 = INT(XI)
      X = XI - REAL(I0)
      J0 = INT(XJ)
      Y = XJ - REAL(J0)
!
      WW(0,0) = 0.D0
      WW(0,1) = 0.D0
      WW(1,0) = 0.D0
      WW(1,1) = 0.D0
!
!     CHECK TO SEE IF CALCULATED INDICES ARE IN RANGE
      IF( (I0.GT.N).OR.(I0.LT.1).OR.(J0.GT.M).OR.(J0.LT.1) ) THEN
        RETURN
      ENDIF
!
!     COMPUTES WEIGHTS FOR BILINEAR SPLINE INTERPOLATION; ONLY
!     USE OCEAN NODES (FOR WHICH MASK IS = 1)
!
      J1 = IPSHFT(J0,1,M)
      I1 = IPSHFT(I0,1,N)
      SM = MASK(I0,J0) + MASK(I0,J1) + MASK(I1,J0) + MASK(I1,J1)
      IF( SM.GT.0 ) THEN
        W00 = (1.D0-X)*(1.D0-Y)*REAL(MASK(I0,J0))
        W01 = (1.D0-X)*Y*REAL(MASK(I0,J1))
        W10 = X*(1.D0-Y)*REAL(MASK(I1,J0))
        W11 = X*Y*REAL(MASK(I1,J1))
        WTOT = W00+W01+W10+W11
        IF( WTOT.EQ.0.D0 ) THEN
          RETURN
        ENDIF
        WW(0,0) = W00/WTOT
        WW(0,1) = W01/WTOT
        WW(1,0) = W10/WTOT
        WW(1,1) = W11/WTOT
      ENDIF
!
      IW(0) = I0
      IW(1) = I1
      JW(0) = J0
      JW(1) = J1
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE BSI_WEIGHTS


!             *******************************
              DOUBLE PRECISION FUNCTION PTIDE
!             *******************************
!
     &( Z1_R,Z1_I,CID,NCON,IND,TIME_MJD,INTERP ) !,NTIME=1
!
!***********************************************************************
! TELEMAC2D   V7P2                                   26/01/2016
!***********************************************************************
!
!brief
!
!history  S.E.BOURBAN (HRW)
!+        26/01/2016
!+        V7P2
!+        Replacement of COMPLEX numbers by their imaginary and real parts
!+        (to be compatible with algorithmic differentiation tools)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  CID           |-->| GIVEN CONSTITUENTS
!|  IND           |-->| INDICES OF GIVEN CONTITUENTS
!|  INTERP        |<->| LOGICAL (INFERENCE OF MINOR CONSTITUENT OR NOT)
!|  LAT           |-->| LATITUDE (DUMMY ARGUMENT)
!|  NCON          |-->| NUMBER OF CONSTITUENTS TURNED ON
!|  TIME_MJD      |-->| MODIFIED JULIAN DAY
!|  Z1_I          |-->| FIELD TO INTERPOLATE
!|  Z1_R          |-->| FIELD TO INTERPOLATE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NCON !,NTIME
      INTEGER, INTENT(IN) :: IND(NCON)
      LOGICAL, INTENT(INOUT) :: INTERP
      CHARACTER(LEN=4), INTENT(IN) :: CID(NCON)
      DOUBLE PRECISION, INTENT(IN) :: TIME_MJD !(NTIME)
!      COMPLEX(KIND(1.D0)), INTENT(IN) :: Z1(NCON)
      DOUBLE PRECISION, INTENT(IN) :: Z1_R(NCON),Z1_I(NCON)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IERR
      DOUBLE PRECISION, PARAMETER :: SECONDSPERDAY = 86400.D0
      DOUBLE PRECISION TIME,DH
      DOUBLE PRECISION WW(TPXO_NCON,8)
      DOUBLE PRECISION PU(TPXO_NCMX),PF(TPXO_NCMX)
! NODAL ARGUMENTS SHOUD BE REAL*8
!
!      COMPLEX(KIND(1.D0)), ALLOCATABLE :: A(:)
      DOUBLE PRECISION, ALLOCATABLE :: A_R(:),A_I(:)
!
!-----------------------------------------------------------------------
!
      IF(INTERP) CALL MKW( INTERP,IND,NCON,WW )
!
      ALLOCATE( A_R(NCON), A_I(NCON) )
      IERR = 0
      DH = 0.D0
!
      CALL NODAL( TIME_MJD,PU,PF )
!     TO USE PHASE SHIFTS FROM CONSTIT, TIME SHOULD BE
!     IN SECONDS, RELATIVE TO JAN 1 1992 (48622MJD)
      TIME = (TIME_MJD-48622.D0)*SECONDSPERDAY
!     .TRUE. MEANS NO SOLID EARTH CORRECTION APPLIED IN MAKE_A
      CALL MAKE_A(.FALSE.,IND,NCON,TIME,PU,PF,WW,A_R,A_I,.TRUE.)
      PTIDE = HEIGHT(A_R,A_I,Z1_R,Z1_I,NCON)
      IF( INTERP )
     &  CALL INFER_MINOR( Z1_R,Z1_I,CID,NCON,TIME_MJD,DH,IERR )
      IF( IERR.EQ.-1 ) THEN
        WRITE(LU,*) 'NOT ENOUGH CONSTITUENTS FOR' //
     &    ' INFERENCE OF MINOR CONSTITUENTS: IGNORED'
        INTERP=.FALSE.
      ENDIF
!     ADDS MINOR CONSTITUENTS
      PTIDE = PTIDE + DH
!
!      DEALLOCATE(A)
      DEALLOCATE(A_R,A_I)
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION PTIDE

!             *************************************
              DOUBLE PRECISION FUNCTION PTIDE_SCHEM
!             *************************************
!
     &( Z1_R,Z1_I,CID,NCON,IND,TIME )
!
!***********************************************************************
! TELEMAC2D   V7P2                                   26/01/2016
!***********************************************************************
!
!brief
!
!history  S.E.BOURBAN (HRW)
!+        26/01/2016
!+        V7P2
!+        Replacement of COMPLEX numbers by their imaginary and real parts
!+        (to be compatible with algorithmic differentiation tools)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  CID           |-->| GIVEN CONSTITUENTS
!|  IND           |-->| INDICES OF GIVEN CONTITUENTS
!|  NCON          |-->| NUMBER OF CONSTITUENTS TURNED ON
!|  TIME          |-->| TIME IN SECONDS!
!|                |   | WARNING DIFFERENT FROM REAL TIDES
!|  Z1_I          |-->| FIELD TO INTERPOLATE
!|  Z1_R          |-->| FIELD TO INTERPOLATE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NCON
      INTEGER, INTENT(IN) :: IND(NCON)
      CHARACTER(LEN=4), INTENT(IN) :: CID(NCON)
      DOUBLE PRECISION, INTENT(IN) :: TIME
!      COMPLEX(KIND(1.D0)), INTENT(IN) :: Z1(NCON)
      DOUBLE PRECISION, INTENT(IN) :: Z1_R(NCON),Z1_I(NCON)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!      COMPLEX(KIND(1.D0)), ALLOCATABLE :: A(:)
      DOUBLE PRECISION, ALLOCATABLE :: A_R(:),A_I(:)
!
!-----------------------------------------------------------------------
!
!      ALLOCATE( A(NCON) )
      ALLOCATE( A_R(NCON), A_I(NCON) )
!
      CALL MAKE_A_SCHEM(IND,NCON,CID,TIME,A_R,A_I)
      PTIDE_SCHEM = HEIGHT(A_R,A_I,Z1_R,Z1_I,NCON)
!
!      DEALLOCATE(A)
      DEALLOCATE(A_R,A_I)
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION PTIDE_SCHEM

!             ************************************
              DOUBLE PRECISION FUNCTION TPXO_PTIDE
!             ************************************
!
     &( IV,KFR,TPXO_NFR,TPXO_BOR_R,TPXO_BOR_I,C_ID,NCON,
     &  CCIND,TIME_MJD,INTERP )
!
!***********************************************************************
! TELEMAC2D   V7P2                                   26/01/2016
!***********************************************************************
!
!brief    Prescribes the free surface elevation, u or v-component
!+        of the velocity based on the TPXO tidal model,
!+        for level or velocity imposed liquid boundary
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW), C.-T. PHAM (LNHE)
!+        06/12/2011
!+        V6P2
!+        Implementation for interfacing with TELEMAC-2D AND 3D
!+        from old HRW functions SL_TPXO, VITU_TPXO, VITV_TPXO
!
!history  S.E.BOURBAN (HRW)
!+        26/01/2016
!+        V7P2
!+        Replacement of COMPLEX numbers by their imaginary and real parts
!+        (to be compatible with algorithmic differentiation tools)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  C_ID          |-->| NAME OF GIVEN CONSTITUENTS
!|  CCIND         |-->| INDICES OF GIVEN CONTITUENTS
!|  INTERP        |<->| LOGICAL (INFERENCE OF MINOR CONSTITUENT OR NOT)
!|  IV            |-->| INDEX OF COMPONENT. 1: WATER LEVEL,
!|                |   | 2: U-VELOCITY COMPONENT, 3: V-VELOCITY COMPONENT
!|  KFR           |-->| BOUNDARY GLOBAL NODE NUMBER
!|  NCON          |-->| NUMBER OF CONSTITUENTS TURNED ON
!|  TIME_MJD      |-->| MODIFIED JULIAN DAY
!|  TPXO_BOR_I    |-->| ARRAY OF HARMONIC CONSTITUENTS (IMAGINARY PART)
!|  TPXO_BOR_R    |-->| ARRAY OF HARMONIC CONSTITUENTS (REAL PART)
!|  TPXO_NFR      |-->| ARRAY OF MARITIME BOUNDARY NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IV,KFR,NCON
      INTEGER, INTENT(IN) :: CCIND(NCON)
      INTEGER, INTENT(IN) :: TPXO_NFR(:)
      LOGICAL, INTENT(INOUT) :: INTERP
      CHARACTER(LEN=4), INTENT(IN) :: C_ID(NCON)
      DOUBLE PRECISION, INTENT(IN) :: TIME_MJD
!      COMPLEX(KIND(1.D0)), INTENT(IN) :: TPXO_BOR(:,:,:)
      DOUBLE PRECISION, INTENT(IN) :: TPXO_BOR_R(:,:,:)
      DOUBLE PRECISION, INTENT(IN) :: TPXO_BOR_I(:,:,:)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TPXO_PTIDE = 0.D0
      IF( TPXO_NFR(KFR).NE.0 ) THEN
        TPXO_PTIDE = PTIDE( TPXO_BOR_R(IV,TPXO_NFR(KFR),:),
     &                      TPXO_BOR_I(IV,TPXO_NFR(KFR),:),
     &                      C_ID,NCON,CCIND,TIME_MJD,INTERP )
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION TPXO_PTIDE

!             ******************************************
              DOUBLE PRECISION FUNCTION TPXO_PTIDE_SCHEM
!             ******************************************
!
     &( IV,KFR,TPXO_NFR,TPXO_BOR_R,TPXO_BOR_I,C_ID,NCON,CCIND,TIME )
!
!***********************************************************************
! TELEMAC2D   V7P2                                   26/01/2016
!***********************************************************************
!
!brief    Prescribes the free surface elevation, u or v-component
!+        of the velocity based on the TPXO tidal model,
!+        for level or velocity imposed liquid boundary
!+        for schematic tides
!
!history  C.-T. PHAM (LNHE)
!+        19/03/2015
!+        V7P1
!+        For schematic tides
!
!history  S.E.BOURBAN (HRW)
!+        26/01/2016
!+        V7P2
!+        Replacement of COMPLEX numbers by their imaginary and real parts
!+        (to be compatible with algorithmic differentiation tools)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  C_ID          |-->| NAME OF GIVEN CONSTITUENTS
!|  CCIND         |-->| INDICES OF GIVEN CONTITUENTS
!|  IV            |-->| INDEX OF COMPONENT. 1: WATER LEVEL,
!|                |   | 2: U-VELOCITY COMPONENT, 3: V-VELOCITY COMPONENT
!|  KFR           |-->| BOUNDARY GLOBAL NODE NUMBER
!|  NCON          |-->| NUMBER OF CONSTITUENTS TURNED ON
!|  TIME          |-->| TIME IN SECONDS!
!|                |   | WARNING DIFFERENT FROM REAL TIDES
!|  TPXO_BOR_I    |-->| ARRAY OF HARMONIC CONSTITUENTS (IMAGINARY PART)
!|  TPXO_BOR_R    |-->| ARRAY OF HARMONIC CONSTITUENTS (REAL PART)
!|  TPXO_NFR      |-->| ARRAY OF MARITIME BOUNDARY NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IV,KFR,NCON
      INTEGER, INTENT(IN) :: CCIND(NCON)
      INTEGER, INTENT(IN) :: TPXO_NFR(:)
      CHARACTER(LEN=4), INTENT(IN) :: C_ID(NCON)
      DOUBLE PRECISION, INTENT(IN) :: TIME
!      COMPLEX(KIND(1.D0)), INTENT(IN) :: TPXO_BOR(:,:,:)
      DOUBLE PRECISION, INTENT(IN) :: TPXO_BOR_R(:,:,:)
      DOUBLE PRECISION, INTENT(IN) :: TPXO_BOR_I(:,:,:)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TPXO_PTIDE_SCHEM = 0.D0
      IF( TPXO_NFR(KFR).NE.0 ) THEN
        TPXO_PTIDE_SCHEM = PTIDE_SCHEM( TPXO_BOR_R(IV,TPXO_NFR(KFR),:),
     &                                  TPXO_BOR_I(IV,TPXO_NFR(KFR),:),
     &                                  C_ID,NCON,CCIND,TIME )
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION TPXO_PTIDE_SCHEM

!                   **************
                    SUBROUTINE MKW
!                   **************
!
     &( INTERP,IND,NC,WR )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  IND           |-->| INDICES OF GIVEN CONTITUENTS
!|  INTERP        |-->| LOGICAL (INFERENCE OF MINOR CONSTITUENT OR NOT)
!|  NC            |-->| NUMBER OF CONSTITUENTS TURNED ON
!|  WR            |<--| FIELD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NC
      INTEGER, INTENT(IN) :: IND(NC)
      DOUBLE PRECISION, INTENT(OUT) :: WR(TPXO_NCON,8)
      LOGICAL, INTENT(IN) :: INTERP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J
!
!-----------------------------------------------------------------------
!
      DO J=1,TPXO_NCON
        DO I=1,8
          WR(J,I) = TPXO_W(J,I)
        ENDDO
      ENDDO
!
      IF( .NOT.INTERP ) THEN
        DO J=1,NC
          IF( IND(J).NE.0 ) THEN
            DO I=1,8
              WR(IND(J),I) = 0.D0
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE MKW
!                   ****************
                    SUBROUTINE NODAL
!                   ****************
!
     &( DTIME,PU,PF )
!
!***********************************************************************
! TELEMAC2D   V8P1
!***********************************************************************
!
!brief
!
!note     "NO1" IN CONSTIT CORRESPONDS TO "M1" IN ARGUMENTS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  DTIME         |-->| MODIFIED JULIAN DAY
!|  LATITUDE      |-->| LATITUDE (DUMMY ARGUMENT)
!|  PF            |<--| NODAL FACTOR F
!|  PU            |<--| NODAL FACTOR U (IN RADIANS)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: DTIME
!     NO DEFAULT INITIALISATION FOR USER TYPE COMPONENTS ALLOWED
      DOUBLE PRECISION, INTENT(INOUT) :: PU(*),PF(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: INDEX(TPXO_NCMX),I
!     53 CHANGED INTO 55 TO ADD M8 AND 2MK3
      DOUBLE PRECISION ARG(55),F(55),U(55)
      DOUBLE PRECISION DTR
!
!     INDEX GIVES CORRESPONDENCE BETWEEN CONSTIT AND RICHARD'S SUBROUTINES
!     IN CONSTIT   M2,S2,K1,O1,N2,P1,K2,q1,2N2,mu2,nu2,L2,t2,
!                  J1,M1(no1),OO1,rho1,Mf,Mm,SSA,M4,
!                  MS4,MN4,M6,M8,MK3,S6,2SM2,2MK3,S1,2Q1,M3
      PARAMETER ( INDEX = (/ 30,35,19,12,27,17,37,10,25,26,28,33,34,
     &                       23,14,24,11,5,3,2,45,46,44,50,54,42,51,40,
     &                       55,18, 8,41 /) )
!
      INTRINSIC ATAN
!
!------------------------------------------------------------------------
!
      DTR=ATAN(1.D0)/45.D0
!
!------------------------------------------------------------------------
!
!     F, U - SAME AS PF, PU IN OLD NODAL.F; ARG IS NOT NEEDED;
!     DTIME - MJD
!     ARGUMENTS AND ASTROL SUBROUTINES SUPPLIED BY
!     R. RAY, MARCH 1999, ATTACHED TO OTIS
      CALL ARGUMENTS( DTIME,ARG,F,U )
      DO I=1,TPXO_NCMX
        PU(I) = 0.D0
        PF(I) = 1.D0
      ENDDO
      DO I = 1,TPXO_NCMX
!       INDEX(I).EQ.0 FOR M8 AND 2MK3
        IF( INDEX(I).GT.0 ) THEN
!         U IS RETURNED BY "ARGUMENTS" IN DEGREES
          PU(I) = U(INDEX(I))*DTR
          PF(I) = F(INDEX(I))
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE NODAL
!                   ********************
                    SUBROUTINE ARGUMENTS
!                   ********************
!
     &(TIME1,ARG,F,U)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    CALCULATES TIDAL ARGUMENTS
!+        KERNEL ROUTINE FOR SUBROUTINE HAT53
!+
!+        ARGUMENTS AND ASTROL SUBROUTINES SUPPLIED BY
!+        R. RAY, MARCH 1999, ATTACHED TO OTIS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  ARG           |<--| EQUILIBRIUM ARGUMENTS
!|  F             |<--| NODAL FACTOR F
!|  TIME1         |-->| MODIFIED JULIAN DAY
!|  U             |<--| NODAL FACTOR U (IN DEGREES)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: TIME1
!     NO DEFAULT INITIALISATION FOR USER TYPE COMPONENTS ALLOWED
      DOUBLE PRECISION, INTENT(INOUT) :: ARG(*), F(*), U(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION :: SHPN(4),S,H,P,OMEGA,HOUR,T1,T2
      DOUBLE PRECISION :: TMP1,TMP2,TEMP1,TEMP2
      DOUBLE PRECISION :: COSN,COS2N,SINN,SIN2N,SIN3N
      DOUBLE PRECISION :: DTR,RTD
!
      DOUBLE PRECISION, PARAMETER :: ZERO=0.D0, ONE=1.D0, TWO=2.D0
      DOUBLE PRECISION, PARAMETER :: THREE=3.D0, FOUR=4.D0, FIVE=5.D0
      DOUBLE PRECISION, PARAMETER :: NINETY=90.D0
!     PP VALUE SLIGHTLY DIFFERENT FROM IN INFER_MINOR (=282.8.D0)
      DOUBLE PRECISION, PARAMETER :: PP=282.94D0
      ! SOLAR PERIGEE AT EPOCH 2000.
!
      INTRINSIC COS,SIN,ATAN
!
!-----------------------------------------------------------------------
!
      DTR=ATAN(1.D0)/45.D0
      RTD=45.D0/ATAN(1.D0)
!
!-----------------------------------------------------------------------
!
!     DETERMINES EQUILIBRIUM ARGUMENTS
!     --------------------------------
      CALL ASTROL( TIME1, SHPN )
!     NO EQUIVALENCE STATEMENT FOR ALLOCATABLE USER TYPES
      S = SHPN(1);  H = SHPN(2); P = SHPN(3);  OMEGA = SHPN(4)

      HOUR = (TIME1 - INT(TIME1))*24.D0
      T1 = 15.D0*HOUR
      T2 = 30.D0*HOUR
      ARG( 1) = H - PP                                  ! Sa
      ARG( 2) = TWO*H                                   ! Ssa
      ARG( 3) = S - P                                   ! Mm
      ARG( 4) = TWO*S - TWO*H                           ! MSf
      ARG( 5) = TWO*S                                   ! Mf
      ARG( 6) = THREE*S - P                             ! Mt
      ARG( 7) = T1 - FIVE*S + THREE*H + P - NINETY      ! alpha1
      ARG( 8) = T1 - FOUR*S + H + TWO*P - NINETY        ! 2Q1
      ARG( 9) = T1 - FOUR*S + THREE*H - NINETY          ! sigma1
      ARG(10) = T1 - THREE*S + H + P - NINETY           ! q1
      ARG(11) = T1 - THREE*S + THREE*H - P - NINETY     ! rho1
      ARG(12) = T1 - TWO*S + H - NINETY                 ! o1
      ARG(13) = T1 - TWO*S + THREE*H + NINETY           ! tau1
      ARG(14) = T1 - S + H + NINETY                     ! M1
      ARG(15) = T1 - S + THREE*H - P + NINETY           ! chi1
      ARG(16) = T1 - TWO*H + PP - NINETY                ! pi1
      ARG(17) = T1 - H - NINETY                         ! p1
      ARG(18) = T1                                      ! s1
!      ARG(18) = T1 + NINETY                             ! s1
      ARG(19) = T1 + H + NINETY                         ! k1
      ARG(20) = T1 + TWO*H - PP + NINETY                ! psi1
      ARG(21) = T1 + THREE*H + NINETY                   ! phi1
      ARG(22) = T1 + S - H + P + NINETY                 ! theta1
      ARG(23) = T1 + S + H - P + NINETY                 ! J1
      ARG(24) = T1 + TWO*S + H + NINETY                 ! OO1
      ARG(25) = T2 - FOUR*S + TWO*H + TWO*P             ! 2N2
      ARG(26) = T2 - FOUR*S + FOUR*H                    ! mu2
      ARG(27) = T2 - THREE*S + TWO*H + P                ! n2
      ARG(28) = T2 - THREE*S + FOUR*H - P               ! nu2
      ARG(29) = T2 - TWO*S + H + PP                     ! M2a
      ARG(30) = T2 - TWO*S + TWO*H                      ! M2
      ARG(31) = T2 - TWO*S + THREE*H - PP               ! M2b
      ARG(32) = T2 - S + P + 180.D0                     ! lambda2
      ARG(33) = T2 - S + TWO*H - P + 180.D0             ! L2
      ARG(34) = T2 - H + PP                             ! t2
      ARG(35) = T2                                      ! S2
      ARG(36) = T2 + H - PP + 180.D0                    ! R2
      ARG(37) = T2 + TWO*H                              ! K2
      ARG(38) = T2 + S + TWO*H - PP                     ! eta2
      ARG(39) = T2 - FIVE*S + FOUR*H + P                ! MNS2
      ARG(40) = T2 + TWO*S - TWO*H                      ! 2SM2
      ARG(41) = 1.5D0*ARG(30)                           ! M3
      ARG(42) = ARG(19) + ARG(30)                       ! MK3
      ARG(43) = THREE*T1                                ! S3
      ARG(44) = ARG(27) + ARG(30)                       ! MN4
      ARG(45) = TWO*ARG(30)                             ! M4
      ARG(46) = ARG(30) + ARG(35)                       ! MS4
      ARG(47) = ARG(30) + ARG(37)                       ! MK4
      ARG(48) = FOUR*T1                                 ! S4
      ARG(49) = FIVE*T1                                 ! S5
      ARG(50) = THREE*ARG(30)                           ! M6
      ARG(51) = THREE*T2                                ! S6
      ARG(52) = 7.D0*T1                                 ! S7
      ARG(53) = FOUR*T2                                 ! S8
      ARG(54) = FOUR*ARG(30)                            ! M8
      ARG(55) = THREE*T1-FOUR*S+THREE*H-NINETY          ! 2MK3
!
!-----------------------------------------------------------------------
!
!     DETERMINES NODAL CORRECTIONS F AND U
!     ------------------------------------
      SINN = SIN(OMEGA*DTR)
      COSN = COS(OMEGA*DTR)
      SIN2N = SIN(TWO*OMEGA*DTR)
      COS2N = COS(TWO*OMEGA*DTR)
      SIN3N = SIN(THREE*OMEGA*DTR)
      F( 1) = ONE                                         ! Sa
      F( 2) = ONE                                         ! Ssa
      F( 3) = ONE - 0.130D0*COSN                          ! Mm
      F( 4) = ONE                                         ! MSf
      F( 5) = 1.043D0 + 0.414D0*COSN                      ! Mf
      F( 6) = SQRT((ONE+.203D0*COSN+.040D0*COS2N)**2 +
     &              (.203D0*SINN+.040D0*SIN2N)**2)        ! Mt

      F( 7) = ONE                                         ! alpha1
      F( 8) = SQRT((ONE+.188D0*COSN)**2+(.188D0*SINN)**2) ! 2Q1
      F( 9) = F(8)                                        ! sigma1
      F(10) = F(8)                                        ! q1
      F(11) = F(8)                                        ! rho1
      F(12) = SQRT((ONE+0.189D0*COSN-0.0058D0*COS2N)**2 +
     &             (0.189D0*SINN-0.0058D0*SIN2N)**2)      ! O1
      F(13) = ONE                                         ! tau1
!     TMP1  = 2.D0*COS(P*DTR)+.4D0*COS((P-OMEGA)*DTR)     ! Doodson's
!     TMP2  = SIN(P*DTR)+.2D0*SIN((P-OMEGA)*DTR)
      TMP1  = 1.36D0*COS(P*DTR)+.267D0*COS((P-OMEGA)*DTR) ! Ray's
      TMP2  = 0.64D0*SIN(P*DTR)+.135D0*SIN((P-OMEGA)*DTR)
      F(14) = SQRT(TMP1**2 + TMP2**2)                     ! M1
      F(15) = SQRT((ONE+.221D0*COSN)**2+(.221D0*SINN)**2) ! chi1
      F(16) = ONE                                         ! pi1
      F(17) = ONE                                         ! P1
      F(18) = ONE                                         ! S1
      F(19) = SQRT((ONE+.1158D0*COSN-.0029D0*COS2N)**2 +
     &             (.1554D0*SINN-.0029D0*SIN2N)**2)       ! K1
      F(20) = ONE                                         ! psi1
      F(21) = ONE                                         ! phi1
!     BUG IN TPXO F(22) SHOULD BE EQUAL TO F(23) OR F(15) RATHER THAN ONE
!     THETA1 IS NOT A SOLAR COMPONENT
!     IN SCHUREMAN, THETA1 HAS THE SAME NODAL FACTORS AS
!     CHI1 AND J1
!     HERE, GEOMETRIC AVERAGE OF CHI1 AND J1
!$$$      F(22) = ONE                                         ! theta1
      F(22) = SQRT( SQRT((ONE+.221D0*COSN)**2+(.221D0*SINN)**2)
     &             *SQRT((ONE+.169D0*COSN)**2+(.227D0*SINN)**2)) ! theta1
      F(23) = SQRT((ONE+.169D0*COSN)**2+(.227D0*SINN)**2) ! J1
      F(24) = SQRT((ONE+0.640D0*COSN+0.134D0*COS2N)**2 +
     &             (0.640D0*SINN+0.134D0*SIN2N)**2 )      ! OO1
      F(25) = SQRT((ONE-.03731D0*COSN+.00052D0*COS2N)**2 +
     &             (.03731D0*SINN-.00052D0*SIN2N)**2)     ! 2N2
      F(26) = F(25)                                       ! mu2
      F(27) = F(25)                                       ! N2
      F(28) = F(25)                                       ! nu2
      F(29) = ONE                                         ! M2a
      F(30) = F(25)                                       ! M2
      F(31) = ONE                                         ! M2b
!     BUG IN TPXO F(32) SHOULD BE EQUAL TO F(25) RATHER THAN ONE
!     LIKE M2,N2,2N2,MU2,NU2
!     LAMBDA2 IS NOT A SOLAR COMPONENT
      F(32) = F(25)                                       ! lambda2
      TEMP1 = ONE-0.25D0*COS(TWO*P*DTR)
     &        -0.11D0*COS((TWO*P-OMEGA)*DTR)-0.04D0*COSN
      TEMP2 = 0.25D0*SIN(TWO*P)+0.11D0*SIN((TWO*P-OMEGA)*DTR)
     &        + 0.04D0*SINN
      F(33) = SQRT(TEMP1**2 + TEMP2**2)                   ! L2
      F(34) = ONE                                         ! t2
      F(35) = ONE                                         ! S2
      F(36) = ONE                                         ! R2
      F(37) = SQRT((ONE+.2852D0*COSN+.0324D0*COS2N)**2 +
     &             (.3108D0*SINN+.0324D0*SIN2N)**2)       ! K2
      F(38) = SQRT((ONE+.436D0*COSN)**2+(.436D0*SINN)**2) ! eta2
      F(39) = F(30)**2                                    ! MNS2
      F(40) = F(30)                                       ! 2SM2
!     BUG IN TPXO F(41) SHOULD BE EQUAL TO F(25)**1.5D0 RATHER THAN ONE
!     M3 IS NOT A SOLAR COMPONENT
      F(41) = SQRT(F(30)**3)                              ! M3
      F(42) = F(19)*F(30)                                 ! MK3
      F(43) = ONE                                         ! S3
      F(44) = F(30)**2                                    ! MN4
      F(45) = F(44)                                       ! M4
!     BUG IN TPXO F(46) SHOULD BE EQUAL TO F(30) RATHER THAN F(44)
      F(46) = F(30)                                       ! MS4
      F(47) = F(30)*F(37)                                 ! MK4
      F(48) = ONE                                         ! S4
      F(49) = ONE                                         ! S5
      F(50) = F(30)**3                                    ! M6
      F(51) = ONE                                         ! S6
      F(52) = ONE                                         ! S7
      F(53) = ONE                                         ! S8
      F(54) = F(30)**4                                    ! M8
      F(55) = F(30)**2*F(19)                              ! 2MK3
!
!-----------------------------------------------------------------------
!
      U( 1) = ZERO                                         ! Sa
      U( 2) = ZERO                                         ! Ssa
      U( 3) = ZERO                                         ! Mm
      U( 4) = ZERO                                         ! MSf
      U( 5) = -23.7D0*SINN + 2.7D0*SIN2N - 0.4D0*SIN3N     ! Mf
      U( 6) = ATAN(-(.203D0*SINN+.040D0*SIN2N)/
     &              (ONE+.203D0*COSN+.040D0*COS2N))*RTD    ! Mt
      U( 7) = ZERO                                         ! alpha1
      U( 8) = ATAN(.189D0*SINN/(ONE+.189D0*COSN))*RTD      ! 2Q1
      U( 9) = U(8)                                         ! sigma1
      U(10) = U(8)                                         ! q1
      U(11) = U(8)                                         ! rho1
      U(12) = 10.8D0*SINN - 1.3D0*SIN2N + 0.2D0*SIN3N      ! O1
      U(13) = ZERO                                         ! tau1
      U(14) = ATAN2(TMP2,TMP1)*RTD                         ! M1
      U(15) = ATAN(-.221D0*SINN/(ONE+.221D0*COSN))*RTD     ! chi1
      U(16) = ZERO                                         ! pi1
      U(17) = ZERO                                         ! P1
      U(18) = ZERO                                         ! S1
      U(19) = ATAN((-.1554D0*SINN+.0029D0*SIN2N)/
     &             (ONE+.1158D0*COSN-.0029D0*COS2N))*RTD   ! K1
      U(20) = ZERO                                         ! psi1
      U(21) = ZERO                                         ! phi1
!     BUG IN TPXO U(22) SHOULD BE EQUAL TO U(23) OR U(15) RATHER THAN ZERO
!     THETA1 IS NOT A SOLAR COMPONENT
!     IN SCHUREMAN, THETA1 HAS THE SAME NODAL FACTORS AS
!     CHI1 AND J1
!     HERE, ARITHMETIC AVERAGE OF CHI1 AND J1
!$$$      U(22) = ZERO                                         ! theta1
      U(22) = (  ATAN(-.221D0*SINN/(ONE+.221D0*COSN))
     &         + ATAN(-.227D0*SINN/(ONE+.169D0*COSN)))     ! theta1
     &        *RTD*0.5D0
      U(23) = ATAN(-.227D0*SINN/(ONE+.169D0*COSN))*RTD     ! J1
      U(24) = ATAN(-(.640D0*SINN+.134D0*SIN2N)/
     &             (ONE+.640D0*COSN+.134D0*COS2N))*RTD     ! OO1
      U(25) = ATAN((-.03731D0*SINN+.00052D0*SIN2N)/
     &             (ONE-.03731D0*COSN+.00052D0*COS2N))*RTD ! 2N2
      U(26) = U(25)                                        ! mu2
      U(27) = U(25)                                        ! N2
      U(28) = U(25)                                        ! nu2
      U(29) = ZERO                                         ! M2a
      U(30) = U(25)                                        ! M2
      U(31) = ZERO                                         ! M2b
!     BUG IN TPXO U(32) SHOULD BE EQUAL TO U(25) RATHER THAN ZERO
!     LIKE M2,N2,2N2,MU2,NU2
!     LAMBDA2 IS NOT A SOLAR COMPONENT
      U(32) = U(25)                                        ! lambda2
      U(33) = ATAN(-TEMP2/TEMP1)*RTD                       ! L2
      U(34) = ZERO                                         ! t2
      U(35) = ZERO                                         ! S2
      U(36) = ZERO                                         ! R2
      U(37) = ATAN(-(.3108D0*SINN+.0324D0*SIN2N)/
     &             (ONE+.2852D0*COSN+.0324D0*COS2N))*RTD   ! K2
      U(38) = ATAN(-.436D0*SINN/(ONE+.436D0*COSN))*RTD     ! eta2
      U(39) = U(30)*TWO                                    ! MNS2
!     BUG IN TPXO U(40) SHOULD BE EQUAL TO -U(30) RATHER THAN U(30)
!     2SM2 = 2S2-M2
      U(40) = -U(30)                                       ! 2SM2
      U(41) = 1.5D0*U(30)                                  ! M3
      U(42) = U(30) + U(19)                                ! MK3
      U(43) = ZERO                                         ! S3
      U(44) = U(30)*TWO                                    ! MN4
      U(45) = U(44)                                        ! M4
      U(46) = U(30)                                        ! MS4
      U(47) = U(30)+U(37)                                  ! MK4
      U(48) = ZERO                                         ! S4
      U(49) = ZERO                                         ! S5
      U(50) = U(30)*THREE                                  ! M6
      U(51) = ZERO                                         ! S6
      U(52) = ZERO                                         ! S7
      U(53) = ZERO                                         ! S8
      U(54) = U(30)*FOUR                                   ! M8
      U(55) = U(30)*TWO-U(19)                              ! 2MK3
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE ARGUMENTS

!                   *****************
                    SUBROUTINE MAKE_A
!                   *****************
!
     &( INTERP,IND,NC,TIME,PU,PF,W,A_R,A_I,L_SAL )
!
!***********************************************************************
! TELEMAC2D   V7P2                                   26/01/2016
!***********************************************************************
!
!brief    COMPUTES A MATRIX ELEMENTS FOR ONE DATA POINT
!
!history  S.E.BOURBAN (HRW)
!+        26/01/2016
!+        V7P2
!+        Replacement of COMPLEX numbers by their imaginary and real parts
!+        (to be compatible with algorithmic differentiation tools)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  A_I           |<--| MATRIX
!|  A_R           |<--| MATRIX
!|  IND           |-->| INDICES OF GIVEN CONTITUENTS
!|  INTERP        |-->| LOGICAL (FORCED TO .FALSE.)
!|  L_SAL         |-->| LOGICAL (.TRUE. = NO SOLID EARTH CORRECTION)
!|  NC            |-->| NUMBER OF CONSTITUENTS TURNED ON
!|  PF            |-->| NODAL FACTOR F
!|  PU            |-->| NODAL FACTOR U (IN RADIANS)
!|  TIME          |-->| TIME IN SECONDS
!|                |   | RELATIVE TO JAN 1 1992 (48622MJD)
!|  W             |-->| FIELD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)  :: NC
      INTEGER, INTENT(IN)  :: IND(NC)
      LOGICAL, INTENT(IN)  :: INTERP, L_SAL
      DOUBLE PRECISION, INTENT(IN) :: W(TPXO_NCON,8)
      DOUBLE PRECISION, INTENT(IN) :: PU(*),PF(*)
      DOUBLE PRECISION, INTENT(IN) :: TIME
!      COMPLEX(KIND(1.D0)), INTENT(OUT) :: A(NC)
      DOUBLE PRECISION, INTENT(OUT) :: A_R(NC),A_I(NC)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J
      DOUBLE PRECISION OMEGA(TPXO_NCMX),PHASE(TPXO_NCMX)
!      COMPLEX(KIND(1.D0)) C(TPXO_NCMX)
      DOUBLE PRECISION C_R(TPXO_NCMX),C_I(TPXO_NCMX)
!
!-----------------------------------------------------------------------
!
!     IF L_SAL=.TRUE. - NO SOLID EARTH CORRECTION IS APPLIED
!     USING BETA_SE COEFFICIENTS
!
!-----------------------------------------------------------------------
!
      IF( .NOT.INTERP ) THEN
        DO J = 1,TPXO_NCMX
          OMEGA(J) = TPXO_OMEGA_D(J)
          PHASE(J) = TPXO_PHASE_MKB(J)
        ENDDO
        DO J = 1,NC
          I = IND(J)
          IF( I.NE.0 ) THEN
!            C(J) = CMPLX( PF(I)*COS(OMEGA(I)*TIME+PHASE(I)+PU(I)),
!     &                    PF(I)*SIN(OMEGA(I)*TIME+PHASE(I)+PU(I)),
!     &                    KIND(1.D0))
            C_R(J) = PF(I)*COS(OMEGA(I)*TIME+PHASE(I)+PU(I))
            C_I(J) = PF(I)*SIN(OMEGA(I)*TIME+PHASE(I)+PU(I))
          ENDIF
        ENDDO
!       REMOVE SOLID EARTH TIDE
        IF( .NOT.L_SAL ) THEN
          DO J = 1,NC
!            A(J) = CMPLX(0.D0,0.D0,KIND(1.D0))
            A_R(J) = 0.D0
            A_I(J) = 0.D0
            IF( IND(J).NE.0 ) THEN
              A_R(J) = C_R(J)*TPXO_BETA_SE(IND(J))
              A_I(J) = C_I(J)*TPXO_BETA_SE(IND(J))
            ENDIF
          ENDDO
        ELSE
          DO J = 1,NC
!            A(J) = C(J)
            A_R(J) = C_R(J)
            A_I(J) = C_I(J)
          ENDDO
        ENDIF
!
      ELSE
!     THIS IS THE CASE WHEN W FROM MODULE WEIGHTS IS USED
!-----------------------------------------------------------------------
        DO I=1,TPXO_NCON
          OMEGA(I) = TPXO_OMEGA_D(I)
          PHASE(I) = TPXO_PHASE_MKB(I)
        ENDDO
!
        DO I=1,TPXO_NCON
!          C(I) = CMPLX( PF(I)*COS(OMEGA(I)*TIME+PHASE(I)+PU(I)),
!     &                  PF(I)*SIN(OMEGA(I)*TIME+PHASE(I)+PU(I)),
!     &                  KIND(1.D0))
          C_R(I) = PF(I)*COS(OMEGA(I)*TIME+PHASE(I)+PU(I))
          C_I(I) = PF(I)*SIN(OMEGA(I)*TIME+PHASE(I)+PU(I))
        ENDDO
!
        DO J = 1,NC
!          A(J) = CMPLX(0.D0,0.D0,KIND(1.D0))
          A_R(J) = 0.D0
          A_I(J) = 0.D0
        ENDDO
!
!       IND(J)=0 MEANS THE CONSTITUENT IS EXCLUDED
        DO I = 1,TPXO_NCON
          DO J = 1,NC
            IF( IND(J).NE.0 ) THEN
!              A(J) = A(J)+C(I)*TPXO_BETA_SE(I)*W(I,IND(J))
              A_R(J) = A_R(J)+C_R(I)*TPXO_BETA_SE(I)*W(I,IND(J))
              A_I(J) = A_I(J)+C_I(I)*TPXO_BETA_SE(I)*W(I,IND(J))
            ENDIF
          ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE MAKE_A
!
!                   ***********************
                    SUBROUTINE MAKE_A_SCHEM
!                   ***********************
!
     &( IND,NC,CID,TIME,A_R,A_I )
!
!***********************************************************************
! TELEMAC2D   V7P2                                    26/01/2016
!***********************************************************************
!
!brief    COMPUTES A MATRIX ELEMENTS FOR ONE DATA POINT
!+        FOR SCHEMATIC TIDES
!
!history  S.E.BOURBAN (HRW)
!+        26/01/2016
!+        V7P2
!+        Replacement of COMPLEX numbers by their imaginary and real parts
!+        (to be compatible with algorithmic differentiation tools)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  A_I           |<--| MATRIX
!|  A_R           |<--| MATRIX
!|  CID           |-->| GIVEN CONSTITUENTS
!|  IND           |-->| INDICES OF GIVEN CONTITUENTS
!|  NC            |-->| NUMBER OF CONSTITUENTS TURNED ON
!|  TIME          |-->| TIME IN SECONDS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)  :: NC
      INTEGER, INTENT(IN)  :: IND(NC)
      CHARACTER(LEN=4), INTENT(IN) :: CID(NC)
      DOUBLE PRECISION, INTENT(IN) :: TIME
!      COMPLEX(KIND(1.D0)), INTENT(OUT) :: A(NC)
      DOUBLE PRECISION, INTENT(OUT) :: A_R(NC),A_I(NC)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J
!
!-----------------------------------------------------------------------
!
!
      DO J = 1,NC
        I = IND(J)
        IF( I.NE.0 .AND. CID(J)(2:2).EQ.'2' ) THEN
!         I =  1 => M2
!          A(J) = CMPLX( COS(TPXO_OMEGA_D(1)*TIME),
!     &                  SIN(TPXO_OMEGA_D(1)*TIME),
!     &                  KIND(1.D0))
          A_R(J) = COS(TPXO_OMEGA_D(1)*TIME)
          A_I(J) = SIN(TPXO_OMEGA_D(1)*TIME)
        ELSEIF( I.NE.0 .AND.
     &          ( CID(J)(2:2).EQ.'4' .OR. CID(J)(3:3).EQ.'4' ) ) THEN
!         I = 21 => M4
!          A(J) = CMPLX( COS(TPXO_OMEGA_D(21)*TIME),
!     &                  SIN(TPXO_OMEGA_D(21)*TIME),
!     &                  KIND(1.D0))
          A_R(J) = COS(TPXO_OMEGA_D(21)*TIME)
          A_I(J) = SIN(TPXO_OMEGA_D(21)*TIME)
        ELSEIF( I.NE.0 .AND. CID(J)(2:2).EQ.'1' ) THEN
!         I = 1 => M2 OVER 2 FOR DIURNAL WAVES (TIMES 2 FOR THE PERIODS)
!          A(J) = CMPLX( COS(TPXO_OMEGA_D(1)*0.5D0*TIME),
!     &                  SIN(TPXO_OMEGA_D(1)*0.5D0*TIME),
!     &                  KIND(1.D0))
          A_R(J) = COS(TPXO_OMEGA_D(1)*0.5D0*TIME)
          A_I(J) = SIN(TPXO_OMEGA_D(1)*0.5D0*TIME)
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE MAKE_A_SCHEM

!                   **********************
                    SUBROUTINE INFER_MINOR
!                   **********************
!
     &( ZMAJ_R,ZMAJ_I,CID,NCON,TIME,DH,IERR )
!
!***********************************************************************
! TELEMAC2D   V7P2                                   26/01/2016
!***********************************************************************
!
!brief    RETURNS CORRECTION FOR THE 16 MINOR CONSTITUENTS
!+        LISTED IN SUBROUTINE
!+        BASED ON PERTH2 (RICHARD RAY'S PROGRAM)
!
!history  S.E.BOURBAN (HRW)
!+        26/01/2016
!+        V7P2
!+        Replacement of COMPLEX numbers by their imaginary and real parts
!+        (to be compatible with algorithmic differentiation tools)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  CID           |-->| GIVEN CONSTITUENTS
!|  DH            |<->| CORRECTION AT GIVEN TIME FOR 16 MINOR CONSTITUENTS
!|  IERR          |<--| =-1 IF NOT ENOUGH CONSTITUENTS FOR INFERENCE
!|  NCON          |-->| NUMBER OF CONSTITUENTS TURNED ON
!|  TIME          |-->| TIME, MODIFIED JULIAN DAY
!|  ZMAJ_I        |-->| HC FOR GIVEN CONSTITUENTS
!|  ZMAJ_R        |-->| HC FOR GIVEN CONSTITUENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)  :: NCON
      INTEGER, INTENT(OUT) :: IERR
      CHARACTER(LEN=4), INTENT(IN)  :: CID(NCON)
      DOUBLE PRECISION, INTENT(IN)  :: TIME
      DOUBLE PRECISION, INTENT(INOUT) :: DH
!      COMPLEX(KIND(1.D0)), INTENT(IN)  :: ZMAJ(NCON)
      DOUBLE PRECISION, INTENT(IN)  :: ZMAJ_R(NCON),ZMAJ_I(NCON)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,NI
      DOUBLE PRECISION HOUR,T1,T2,SHPN(4),S,H,P,OMEGA
      DOUBLE PRECISION SINN,COSN,SIN2N,COS2N
      DOUBLE PRECISION U(18),F(18),ARG(18)
      DOUBLE PRECISION DTR,RTD
!     PP VALUE SLIGHTLY DIFFERENT FROM IN ARGUMENTS (=282.94.D0)
      DOUBLE PRECISION, PARAMETER :: PP = 282.8D0
!      COMPLEX(KIND(1.D0)) ZMIN(18)
      DOUBLE PRECISION ZMIN_R(18),ZMIN_I(18)
!      COMPLEX(KIND(1.D0)) Z8(8)
      DOUBLE PRECISION Z8_R(8),Z8_I(8)
!
!     AD: ALGORITHMIC DIFERENTIATION
!     NO EQUIVALENCE STATEMENT FOR ALLOCATABLE USER TYPES
!      EQUIVALENCE (SHPN(1),S),(SHPN(2),H),(SHPN(3),P),(SHPN(4),OMEGA)
!
      CHARACTER(LEN=4) CID8(8)      ! IN ORDER TO CORRESPOND RR COEFFICIENTS
      PARAMETER ( CID8 = (/'q1  ','o1  ','p1  ','k1  ',
     &          'n2  ','m2  ','s2  ','k2  ' /) )
!
      INTRINSIC COS,SIN,SQRT,ATAN2,ATAN
!
!-----------------------------------------------------------------------
!
      DTR=ATAN(1.D0)/45.D0
      RTD=45.D0/ATAN(1.D0)
!
!-----------------------------------------------------------------------
!
!     RE-ORDER TO CORRESPOND TO CID8
      IERR = 0
!      Z8 = CMPLX(0.D0,0.D0,KIND(1.D0))
      NI = 0
      DO I = 1,8
        Z8_R(I) = 0.D0
        Z8_I(I) = 0.D0
        DO J = 1,NCON
          IF( CID(J).EQ.CID8(I) ) THEN
            Z8_R(I)=ZMAJ_R(J)
            Z8_I(I)=ZMAJ_I(J)
            IF( I.NE.3.AND.I.NE.8 ) NI = NI+1
          ENDIF
        ENDDO
      ENDDO
!
      IF( NI.LT.6 ) THEN
        IERR=-1 ! NOT ENOUGH CONSTITUENTS FOR INFERENCE
        RETURN
      ENDIF
!
      ZMIN_R(1)  = 0.263D0 *Z8_R(1) - 0.0252D0*Z8_R(2)  ! 2Q1
      ZMIN_R(2)  = 0.297D0 *Z8_R(1) - 0.0264D0*Z8_R(2)  ! sigma1
      ZMIN_R(3)  = 0.164D0 *Z8_R(1) + 0.0048D0*Z8_R(2)  ! rho1 +
      ZMIN_R(4)  = 0.0140D0*Z8_R(2) + 0.0101D0*Z8_R(4)  ! M1
      ZMIN_R(5)  = 0.0389D0*Z8_R(2) + 0.0282D0*Z8_R(4)  ! M1
      ZMIN_R(6)  = 0.0064D0*Z8_R(2) + 0.0060D0*Z8_R(4)  ! chi1
      ZMIN_R(7)  = 0.0030D0*Z8_R(2) + 0.0171D0*Z8_R(4)  ! pi1
      ZMIN_R(8)  =-0.0015D0*Z8_R(2) + 0.0152D0*Z8_R(4)  ! phi1
      ZMIN_R(9)  =-0.0065D0*Z8_R(2) + 0.0155D0*Z8_R(4)  ! theta1
      ZMIN_R(10) =-0.0389D0*Z8_R(2) + 0.0836D0*Z8_R(4)  ! J1 +
      ZMIN_R(11) =-0.0431D0*Z8_R(2) + 0.0613D0*Z8_R(4)  ! OO1 +
      ZMIN_R(12) = 0.264D0 *Z8_R(5) - 0.0253D0*Z8_R(6)  ! 2N2 +
      ZMIN_R(13) = 0.298D0 *Z8_R(5) - 0.0264D0*Z8_R(6)  ! mu2 +
      ZMIN_R(14) = 0.165D0 *Z8_R(5) + 0.00487D0*Z8_R(6) ! nu2 +
      ZMIN_R(15) = 0.0040D0*Z8_R(6) + 0.0074D0*Z8_R(7)  ! lambda2
      ZMIN_R(16) = 0.0131D0*Z8_R(6) + 0.0326D0*Z8_R(7)  ! L2 +
      ZMIN_R(17) = 0.0033D0*Z8_R(6) + 0.0082D0*Z8_R(7)  ! L2 +
      ZMIN_R(18) = 0.0585D0*Z8_R(7)                     ! t2 +
!
      ZMIN_I(1)  = 0.263D0 *Z8_I(1) - 0.0252D0*Z8_I(2)  ! 2Q1
      ZMIN_I(2)  = 0.297D0 *Z8_I(1) - 0.0264D0*Z8_I(2)  ! sigma1
      ZMIN_I(3)  = 0.164D0 *Z8_I(1) + 0.0048D0*Z8_I(2)  ! rho1 +
      ZMIN_I(4)  = 0.0140D0*Z8_I(2) + 0.0101D0*Z8_I(4)  ! M1
      ZMIN_I(5)  = 0.0389D0*Z8_I(2) + 0.0282D0*Z8_I(4)  ! M1
      ZMIN_I(6)  = 0.0064D0*Z8_I(2) + 0.0060D0*Z8_I(4)  ! chi1
      ZMIN_I(7)  = 0.0030D0*Z8_I(2) + 0.0171D0*Z8_I(4)  ! pi1
      ZMIN_I(8)  =-0.0015D0*Z8_I(2) + 0.0152D0*Z8_I(4)  ! phi1
      ZMIN_I(9)  =-0.0065D0*Z8_I(2) + 0.0155D0*Z8_I(4)  ! theta1
      ZMIN_I(10) =-0.0389D0*Z8_I(2) + 0.0836D0*Z8_I(4)  ! J1 +
      ZMIN_I(11) =-0.0431D0*Z8_I(2) + 0.0613D0*Z8_I(4)  ! OO1 +
      ZMIN_I(12) = 0.264D0 *Z8_I(5) - 0.0253D0*Z8_I(6)  ! 2N2 +
      ZMIN_I(13) = 0.298D0 *Z8_I(5) - 0.0264D0*Z8_I(6)  ! mu2 +
      ZMIN_I(14) = 0.165D0 *Z8_I(5) + 0.00487D0*Z8_I(6) ! nu2 +
      ZMIN_I(15) = 0.0040D0*Z8_I(6) + 0.0074D0*Z8_I(7)  ! lambda2
      ZMIN_I(16) = 0.0131D0*Z8_I(6) + 0.0326D0*Z8_I(7)  ! L2 +
      ZMIN_I(17) = 0.0033D0*Z8_I(6) + 0.0082D0*Z8_I(7)  ! L2 +
      ZMIN_I(18) = 0.0585D0*Z8_I(7)                     ! t2 +
!
      HOUR = (TIME - INT(TIME))*24.D0
      T1 = 15.D0*HOUR
      T2 = 30.D0*HOUR
      CALL ASTROL( TIME, SHPN )
!     AD: ALGORITHMIC DIFERENTIATION
!     NO EQUIVALENCE STATEMENT FOR ALLOCATABLE USER TYPES
      S = SHPN(1);  H = SHPN(2); P = SHPN(3);  OMEGA = SHPN(4)
!
      ARG(1)  = T1 - 4.D0*S + H + 2.D0*P - 90.D0 ! 2Q1
      ARG(2)  = T1 - 4.D0*S + 3.D0*H - 90.D0     ! sigma1
      ARG(3)  = T1 - 3.D0*S + 3.D0*H - P - 90.D0 ! rho1
      ARG(4)  = T1 - S + H - P + 90.D0           ! M1
      ARG(5)  = T1 - S + H + P + 90.D0           ! M1
      ARG(6)  = T1 - S + 3.D0*H - P + 90.D0      ! chi1
      ARG(7)  = T1 - 2.D0*H + PP - 90.D0         ! pi1
      ARG(8)  = T1 + 3.D0*H + 90.D0              ! phi1
      ARG(9)  = T1 + S - H + P + 90.D0           ! theta1
      ARG(10) = T1 + S + H - P + 90.D0           ! J1
      ARG(11) = T1 + 2.D0*S + H + 90.D0          ! OO1
      ARG(12) = T2 - 4.D0*S + 2.D0*H + 2.D0*P    ! 2N2
      ARG(13) = T2 - 4.D0*S + 4.D0*H             ! mu2
      ARG(14) = T2 - 3.D0*S + 4.D0*H - P         ! nu2
      ARG(15) = T2 - S + P + 180.D0              ! lambda2
      ARG(16) = T2 - S + 2.D0*H - P + 180.D0     ! L2
      ARG(17) = T2 - S + 2.D0*H + P              ! L2
      ARG(18) = T2 - H + PP                      ! t2
!
!     DETERMINES NODAL CORRECTIONS F AND U
      SINN = SIN(OMEGA*DTR)
      COSN = COS(OMEGA*DTR)
      SIN2N = SIN(2.D0*OMEGA*DTR)
      COS2N = COS(2.D0*OMEGA*DTR)
!
      F(1)  = SQRT((1.D0 + 0.189D0*COSN - 0.0058D0*COS2N)**2 +
     &             (0.189D0*SINN - 0.0058D0*SIN2N)**2)             ! 2Q1
      F(2)  = F(1)                                                 ! sigma1
      F(3)  = F(1)                                                 ! rho1
      F(4)  = SQRT((1.D0 + 0.185D0*COSN)**2 + (0.185D0*SINN)**2)   ! M1
      F(5)  = SQRT((1.D0 + 0.201D0*COSN)**2 + (0.201D0*SINN)**2)   ! M1
      F(6)  = SQRT((1.D0 + 0.221D0*COSN)**2 + (0.221D0*SINN)**2)   ! chi1
      F(7)  = 1.D0                                                 ! pi1
      F(8)  = 1.D0                                                 ! phi1
!     BUG IN TPXO F(9) SHOULD BE EQUAL TO F(10) OR F(6) RATHER THAN ONE
!     THETA1 IS NOT A SOLAR COMPONENT
!     IN SCHUREMAN, THETA1 HAS THE SAME NODAL FACTORS AS
!     CHI1 AND J1
!     HERE, GEOMETRIC AVERAGE OF CHI1 AND J1
      ! theta1
      F(9)  = SQRT( SQRT((1.D0 + 0.221D0*COSN)**2 + (0.221D0*SINN)**2)
     &             *SQRT((1.D0 + 0.198D0*COSN)**2 + (0.198D0*SINN)**2))
      F(10) = SQRT((1.D0 + 0.198D0*COSN)**2 + (0.198D0*SINN)**2)   ! J1
      F(11) = SQRT((1.D0 + 0.640D0*COSN + 0.134D0*COS2N)**2 +
     &             (0.640D0*SINN + 0.134D0*SIN2N)**2 )             ! OO1
      F(12) = SQRT((1.D0 - 0.0373D0*COSN)**2 + (0.0373D0*SINN)**2) ! 2N2
      F(13) = F(12)                                                ! mu2
      F(14) = F(12)                                                ! nu2
!     BUG IN TPXO F(15) SHOULD BE EQUAL TO F(12) RATHER THAN ONE
!     LIKE M2,N2,2N2,MU2,NU2
!     LAMBDA2 IS NOT A SOLAR COMPONENT
      F(15) = F(12)                                                ! lambda2
      F(16) = F(12)                                                ! L2
      F(17) = SQRT((1.D0 + 0.441D0*COSN)**2 + (0.441D0*SINN)**2)   ! L2
      F(18) = 1.D0                                                 ! t2
!
      U(1)  = ATAN2(0.189D0*SINN - 0.0058D0*SIN2N,
     &              1.D0 + 0.189D0*COSN - 0.0058D0*SIN2N)*RTD ! 2Q1
      U(2)  = U(1)                                            ! sigma1
      U(3)  = U(1)                                            ! rho1
      U(4)  = ATAN2( 0.185D0*SINN, 1.D0 + 0.185D0*COSN)*RTD   ! M1
      U(5)  = ATAN2(-0.201D0*SINN, 1.D0 + 0.201D0*COSN)*RTD   ! M1
      U(6)  = ATAN2(-0.221D0*SINN, 1.D0 + 0.221D0*COSN)*RTD   ! chi1
      U(7)  = 0.D0                                            ! pi1
      U(8)  = 0.D0                                            ! phi1
!     BUG IN TPXO U(9) SHOULD BE EQUAL TO U(10) OR U(6) RATHER THAN ZERO
!     THETA1 IS NOT A SOLAR COMPONENT
!     IN SCHUREMAN, THETA1 HAS THE SAME NODAL FACTORS AS
!     CHI1 AND J1
!     HERE, ARITHMETIC AVERAGE OF CHI1 AND J1
      U(9)  = ( ATAN2(-0.221D0*SINN, 1.D0 + 0.221D0*COSN)
     &        + ATAN2(-0.198D0*SINN, 1.D0 + 0.198D0*COSN))*RTD*0.5D0 ! theta1
      U(10) = ATAN2(-0.198D0*SINN, 1.D0 + 0.198D0*COSN)*RTD   ! J1
      U(11) = ATAN2(-0.640D0*SINN - 0.134D0*SIN2N,
     &              1.D0 + 0.640D0*COSN + 0.134D0*COS2N)*RTD  ! OO1
      U(12) = ATAN2(-0.0373D0*SINN, 1.D0 - 0.0373D0*COSN)*RTD ! 2N2
      U(13) = U(12)                                           ! mu2
      U(14) = U(12)                                           ! nu2
!     BUG IN TPXO U(15) SHOULD BE EQUAL TO U(12) RATHER THAN ZERO
!     LIKE M2,N2,2N2,MU2,NU2
!     LAMBDA2 IS NOT A SOLAR COMPONENT
      U(15) = U(12)                                           ! lambda2
      U(16) = U(12)                                           ! L2
      U(17) = ATAN2(-0.441D0*SINN, 1.D0 + 0.441D0*COSN)*RTD   ! L2
      U(18) = 0.D0                                            ! t2
!
!     SUM OVER ALL TIDES
!     ------------------
      DH = 0.D0
      DO I = 1,18
        DH = DH + ZMIN_R(I)*F(I)*COS((ARG(I)+U(I))*DTR)
     &          - ZMIN_I(I)*F(I)*SIN((ARG(I)+U(I))*DTR)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE INFER_MINOR


!                   *****************
                    SUBROUTINE ASTROL
!                   *****************
!
     &( TIME,SHPN )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    COMPUTES THE BASIC ASTRONOMICAL MEAN LONGITUDES  S, H, P, N
!+        NOTE N IS NOT N', I.E. N IS DECREASING WITH TIME.
!+        THESE FORMULAE ARE FOR THE PERIOD 1990 - 2010, AND WERE
!+        DERIVED BY DAVID CARTWRIGHT (PERSONAL COMM., NOV. 1990)
!+        TIME IS UTC IN DECIMAL MJD.
!+        ALL LONGITUDES RETURNED IN DEGREES.
!+
!+        ARGUMENTS AND ASTROL SUBROUTINES SUPPLIED BY
!+        R. RAY, MARCH 1999, ATTACHED TO OTIS
!+
!+        OBTAINED PRECISION IS 0.01D0 COMPARED TO SCHUREMAN FORMULAE
!+        (SECOND OR THIRD ORDER) WHEN USED WITH DATE_MJD
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  SHPN          |<--| ARRAY OF FOUR BASIC ASTRONOMICAL MEAN LONGITUDES
!|                |   | S, H, P, N
!|  TIME          |-->| MODIFIED JULIAN DAY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: TIME
      DOUBLE PRECISION, INTENT(OUT) :: SHPN(4)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION T
!
!-----------------------------------------------------------------------
!
!     NON-VECTORIZED VERSION
!
!     51544.D0 CORRESPONDS TO 01/01/2000
      T = TIME - 51544.4993D0
!
!     MEAN LONGITUDE OF MOON (S)
!     --------------------------
      SHPN(1) = 218.3164D0 + 13.17639648D0 * T
!
!     MEAN LONGITUDE OF SUN (H)
!     -------------------------
      SHPN(2) = 280.4661D0 +  0.98564736D0 * T
!
!     MEAN LONGITUDE OF LUNAR PERIGEE (P)
!     -----------------------------------
      SHPN(3) =  83.3535D0 +  0.11140353D0 * T
!
!     MEAN LONGITUDE OF ASCENDING LUNAR NODE (N)
!     ------------------------------------------
      SHPN(4) = 125.0445D0 -  0.05295377D0 * T

      SHPN(1) = MOD(SHPN(1),360.D0)
      SHPN(2) = MOD(SHPN(2),360.D0)
      SHPN(3) = MOD(SHPN(3),360.D0)
      SHPN(4) = MOD(SHPN(4),360.D0)

      IF( SHPN(1).LT.0.D0 ) SHPN(1) = SHPN(1) + 360.D0
      IF( SHPN(2).LT.0.D0 ) SHPN(2) = SHPN(2) + 360.D0
      IF( SHPN(3).LT.0.D0 ) SHPN(3) = SHPN(3) + 360.D0
      IF( SHPN(4).LT.0.D0 ) SHPN(4) = SHPN(4) + 360.D0
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE ASTROL
!               *************************
                INTEGER FUNCTION DATE_MJD
!               *************************
!
     &( MM,ID,IYYY )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    CONVERTS DATE TO MJD (MODIFIED JULIAN DAYS)
!+        INPUT:  ID - DAY, MM - MONTH, IYYY - YEAR
!+        OUTPUT: MJD > 0 - MODIFIED JULIAN DAYS
!+        DATE >= 11.17.1858 CORRESPONDS TO MJD = 0
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  ID            |<->| DAY
!|  IYYY          |<->| YEAR
!|  MM            |<->| MONTH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: MM,ID,IYYY
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER DPM(12),DAYS,I,NLEAP,K
      PARAMETER ( DPM = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /) )
!
!-----------------------------------------------------------------------
!
      DATE_MJD = 0
!     NO EARLIER DATES THAN NOVEMBER 17TH 1858
      IF(     IYYY.LT.1858.OR.(IYYY.EQ.1858.AND.MM.LT.11)
     &   .OR.(IYYY.EQ.1858.AND.MM.EQ.11.AND.ID.LT.17) ) THEN
        WRITE(LU,*) 'NO EARLIER DATES ' //
     &               'THAN NOVEMBER 17TH 1858 ARE ALLOWED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DAYS = 0
      DO I = 1,MM-1
        DAYS = DAYS+DPM(I)
        IF( I.EQ.2.AND.INT(IYYY/4)*4.EQ.IYYY ) DAYS = DAYS+1
      ENDDO
!     321TH DAY CORRESPONDS TO NOVEMBER 17TH FOR A NON LEAP YEAR
      DAYS = DAYS+ID-321

!     LEAP DAY CORRECTION
      DO K = 1900,IYYY,100
        IF( K.EQ.IYYY.AND.MM.GT.2 ) DAYS = DAYS-1
      ENDDO
      DO K = 2000,IYYY,400
        IF( K.EQ.IYYY.AND.MM.GT.2 ) DAYS = DAYS+1
      ENDDO
!     EACH 4TH YEAR IS LEAP YEAR
      NLEAP = INT(REAL(IYYY-1-1860)*0.25)
      IF( IYYY.GT.1860 ) NLEAP = NLEAP+1
!     EXCEPT
      DO K = 1900,IYYY-1,100
        IF( K.LT.IYYY ) NLEAP = NLEAP-1
!     THE FOLLOWING LINE IS USELESS AS K.GE.IYYY-1
!       IF( K.EQ.IYYY.AND.MM.GT.2 ) DAYS = DAYS-1
      ENDDO
!     BUT EACH IN THE ROW 2000:400:... IS LEAP YEAR AGAIN
      DO K = 2000,IYYY-1,400
        IF( K.LT.IYYY ) NLEAP = NLEAP+1
!     THE FOLLOWING LINE IS USELESS AS K.GE.IYYY-1
!       IF( K.EQ.IYYY.AND.MM.GT.2 ) DAYS = DAYS+1
      ENDDO
      DATE_MJD = 365*(IYYY-1858)+NLEAP+DAYS
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION DATE_MJD

!                   *********************
                    SUBROUTINE CONDI_TPXO
!                   *********************
!
     &(NPOIN,NPTFR,NBOR,X,Y,H,U,V,LIHBOR,LIUBOR,KENT,KENTU,
     & GEOSYST,NUMZONE,T2DL93,LAMBD0,PHI0,T2D_FILES,T2DBB1,T2DBB2,
     & MARDAT,MARTIM,INTMICON,MSL,TIDALTYPE,BOUNDARY_COLOUR,ICALHW,
     & I_ORIG,J_ORIG,HMIN_VIT_IC,VITINI_TPXO)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    Prepare a level boundary filter to store the TPXO constituents
!+        at the boundary. In particular,
!+        count NPTNFR and ALLOCATE and set the filter TPXO_NFR
!
!note     Passing NPOIN, LIHBOR, LIUBOR, X, Y and H as arguments allows
!+        this SUBROUTINE to be called from TELEMAC-2D or TELEMAC-3D
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+        Implementation and generalised for interfacing with
!+        TELEMAC-2D AND 3D
!
!history  C.-T. PHAM (LNHE)
!+        05/11/2012
!+        V6P3
!+        Correction of a bug for TELEMAC-3D. Initialisation of the
!+        velocity arrays at 0
!+        Correction of a bug when sea levels are referenced
!+        with respect to Chart Datum (CD)
!
!history  C.-T. PHAM (LNHE)
!+        22/07/2015
!+        V7P1
!+        Schematic tides, High Water at the beginning
!
!history  C. COULET (ARTELIA), C.-T. PHAM (LNHE)
!+        13/10/2015
!+        V7P1
!+        Correction of a bug when MSL .NE. 0.D0
!
!history  S.E.BOURBAN (HRW)
!+        26/01/2016
!+        V7P2
!+        Replacement of COMPLEX numbers by their imaginary and real parts
!+        (to be compatible with algorithmic differentiation tools)
!
!warning  (-ZF) should be stored in H as you enter this subroutine
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BOUNDARY_COLOUR|-->| AN INTEGER LINKED TO BOUNDARY POINTS
!|                |   | BY DEFAULT THE LAST LINE OF BOUNDARY CONDITIONS
!|                |   | FILE, HENCE THE GLOBAL BOUNDARY NUMBER, BUT CAN
!|                |   | BE CHANGED BY USER.
!|  GEOSYST       |-->| TYPE OF GEOGRAPHIC SYSTEM (WGS84 LONG/LAT, UTM OR LAMBERT)
!|  H             |<->| COMES IN AS -ZF, TO WHICH THE TPXO FREE SURFACE
!|                |   | WILL BE ADDED TO PRODUCE WATER DEPTH
!|  HMIN_VIT_IC   |-->| MINIMUM DEPTH TO COMPUTE TIDAL VELOCITIES
!|                |   | INITIAL CONDITIONS
!|  ICALHW        |<->| NUMBER THAT MAY BE CHOSEN BY THE USER
!|                |   | TO CALIBRATE HIGH WATER OR AUTOMATICALLY CHOSEN
!|                |   | IN CASE OF THE MODELLING OF A SCHEMATIC TIDE
!|  INTMICON      |-->| IF YES, INFERENCE OF MINOR CONSTITUENTS
!|  I_ORIG        |-->| OFFSET ON X COORDINATES
!|  J_ORIG        |-->| OFFSET ON Y COORDINATES
!|  KENT          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!|  KENTU         |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!|  LAMBD0        |-->| LATITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!|  LIHBOR        |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!|                |-->| (KENT IS HERE OF INTEREST)
!|  LIUBOR        |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!|  MARDAT        |<->| DATE (YEAR, MONTH,DAY)
!|  MARTIM        |-->| TIME (HOUR, MINUTE,SECOND)
!|  MSL           |-->| COEFFICIENT TO CALIBRATE SEA LEVEL KEYWORD
!|  NBOR          |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!|  NPOIN         |-->| NUMBER OF 2D NODES IN THE MESH
!|  NPTFR         |-->| NUMBER OF BOUNDARY POINTS
!|  NUMZONE       |-->| NUMBER OF ZONE WHEN PLANE PROJECTION (UTM OR LAMBERT)
!|  PHI0          |-->| LONGITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!|  T2DBB1        |-->| ADDRESS OF DATA BASE 1 IN T2D_FILES
!|  T2DBB2        |-->| ADDRESS OF DATA BASE 2 IN T2D_FILES
!|  T2DL93        |-->| ADDRESS OF GRID CONVERSION FOR LAMBERT 93
!|                |   | IN T2D_FILES
!|  T2D_FILES     |-->| ARRAY OF FILES
!|  TIDALTYPE     |-->| TYPE OF TIDE TO MODEL
!|  U             |<--| 2D DEPTH-AVERAGED VELOCITY COMPONENT U
!|  V             |<--| 2D DEPTH-AVERAGED VELOCITY COMPONENT V
!|  VITINI_TPXO   |-->| IF YES, INITIAL VELOCITIES ARE COMPUTED FROM OSU
!|                |   | TIDAL SOLUTIONS, E.G. TPXO. OTHERWISE = 0.
!|  X             |-->| COORDINATES X OF THE NODES OF THE MESH
!|  Y             |-->| COORDINATES Y OF THE NODES OF THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_PARALLEL, ONLY : P_MIN,P_MAX
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,T2DBB1,T2DBB2
      INTEGER, INTENT(IN)             :: KENT,KENTU
      INTEGER, INTENT(IN)             :: LIHBOR(*),LIUBOR(*)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: GEOSYST,NUMZONE,T2DL93
      INTEGER, INTENT(IN)             :: TIDALTYPE,MARTIM(3)
      INTEGER, INTENT(IN)             :: I_ORIG,J_ORIG
      INTEGER, INTENT(INOUT)          :: ICALHW,MARDAT(3)
      DOUBLE PRECISION, INTENT(IN)    :: LAMBD0,PHI0,MSL,HMIN_VIT_IC
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN),V(NPOIN)
      TYPE(BIEF_FILE), INTENT(IN)     :: T2D_FILES(*)
      TYPE(BIEF_OBJ), INTENT(IN)      :: BOUNDARY_COLOUR
      LOGICAL, INTENT(INOUT)          :: INTMICON,VITINI_TPXO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IC,I,J,K,IPOIN,IERR,NC,N,M,NPTFRL
      INTEGER IPTFRL
      INTEGER, ALLOCATABLE :: MASKT(:,:),MASKU(:,:),MASKV(:,:)
      DOUBLE PRECISION PI,DTR,RTD
      DOUBLE PRECISION STIME_MJD
      DOUBLE PRECISION XM,XL,YL,XO,YO,ALPHA,RADIUS,X_ORIG,Y_ORIG
      DOUBLE PRECISION, ALLOCATABLE :: LAT(:),LON(:)
      DOUBLE PRECISION, ALLOCATABLE :: XT(:),YT(:)
      DOUBLE PRECISION PH_LIM(2),TH_LIM(2)
      REAL PH_LIM_R(2),TH_LIM_R(2)
!      COMPLEX, ALLOCATABLE :: ZT(:,:,:)
      REAL, ALLOCATABLE :: ZT_R(:,:,:),ZT_I(:,:,:)
!      COMPLEX, ALLOCATABLE :: UT(:,:,:), VT(:,:,:)
      REAL, ALLOCATABLE :: UT_R(:,:,:),UT_I(:,:,:)
      REAL, ALLOCATABLE :: VT_R(:,:,:),VT_I(:,:,:)
!      COMPLEX, ALLOCATABLE :: UV(:,:,:)
      REAL, ALLOCATABLE :: UV_R(:,:,:),UV_I(:,:,:)
!      COMPLEX(KIND(1.D0)), ALLOCATABLE :: ZCON(:),ZCON2(:),ZCON3(:)
      DOUBLE PRECISION, ALLOCATABLE :: ZCON_R(:),ZCON_I(:)
      DOUBLE PRECISION, ALLOCATABLE :: ZCON2_R(:),ZCON2_I(:)
      DOUBLE PRECISION, ALLOCATABLE :: ZCON3_R(:),ZCON3_I(:)
      CHARACTER(LEN=4) C_ID_MOD(TPXO_NCMX)
      DOUBLE PRECISION SPEED,MAXSP
      DOUBLE PRECISION TMP_R,TMP_I
#if !defined NO_CONVERT_ENDIAN
      CHARACTER(LEN=PATH_LEN) :: FILE_NAME
#endif
!
!     N,M: SIZES OF THE GRID SUPPORTING THE TPXO MODEL
!     NC: NUMBER OF CONSTITUENTS AVAILABLE IN THE FILE
!     MASKT,MASKU,MASKV MASKS TO FILTER VALID AND INVALID (U,V,H) VALUES
!     STIME_MJD: DAYS IN MODIFIED JULIAN DAYS
!     PH_LIM,TH_LIM: MIN AND MAX RANGES FOR PHASES AND PERIODES
!     ZT,UT,VT,UV,ZCON: PHASES AND PERIODS FOR U, V AND H
!     HERE DEFINED AS COMPLEX
!     C_ID_MOD INDICES OF AVAILABLE CONTITUENTS AMONGST THE ALL POSSIBLE
!
      INTRINSIC ATAN
!
!-----------------------------------------------------------------------
!
      PI = 4.D0*ATAN(1.D0)
      DTR = PI/180.D0
      RTD = 180.D0/PI
!
!-----------------------------------------------------------------------
!
!  SPECIFIC VALUES FOR THE EXAMPLE OF A GEOGRAPHIC SYSTEM DEFINED BY
!  THE USER
!
      XO = 0.D0
      YO = 51.5D0
!  OR E.G.
!     XO = PHI0       ! LONGITUDE
!     YO = LAMBD0     ! LATITUDE
!
!  ANGLE BETWEEN EAST AXIS ---> X AXIS (TRIGONOMETRIC DEGREES)
      ALPHA = 40.D0
      ALPHA = ALPHA*DTR ! IN RADIANS
!  RADIUS: RADIUS OF THE EARTH
      RADIUS = 6371000.D0
!
!-----------------------------------------------------------------------
!
!  TEST TO CHECK CORRECT VALUES FOR TIDALTYPE
!
      IF(.NOT.DEJA) THEN
        IF(TIDALTYPE.LT.1.OR.TIDALTYPE.GT.6) THEN
          WRITE(LU,*) 'UNEXPECTED VALUE FOR TIDALTYPE=',TIDALTYPE
          WRITE(LU,*) 'IT MUST BE CHOSEN BETWEEN 1 AND 6'
          WRITE(LU,*) 'WITH TPXO TIDAL DATA BASE'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      IF(.NOT.DEJA) THEN
!     PREPARE STORAGE ON LEVEL BOUNDARIES
!     NPTFRL: NUMBERS OF BOUNDARY POINTS WHERE TIDE IS PRESCRIBED
!
      ALLOCATE( TPXO_NFR(NPOIN) )
      DO K=1,NPOIN
        TPXO_NFR(K) = 0
      ENDDO
      NPTFRL = 0
      DO K = 1,NPTFR
        IF( LIHBOR(K).EQ.KENT.OR.LIUBOR(K).EQ.KENTU ) THEN
          NPTFRL = NPTFRL + 1
          TPXO_NFR(NBOR(K)) = NPTFRL
        ENDIF
      ENDDO
!
      IF(T2D_FILES(T2DBB1)%NAME(1:1).EQ.' ' .OR.
     &   T2D_FILES(T2DBB2)%NAME(1:1).EQ.' ') THEN
        WRITE(LU,*) 'TPXO FILES NOT AVAILABLE'
        CALL PLANTE(1)
        STOP
      ENDIF
      WRITE(LU,*) 'INITIALISATION BASED ON TPXO:'
!
!-----------------------------------------------------------------------
!
!     READ AVAILABLE DIMENSIONS
!
#if !defined NO_CONVERT_ENDIAN
      INQUIRE(UNIT=T2D_FILES(T2DBB1)%LU,NAME=FILE_NAME)
      CLOSE(T2D_FILES(T2DBB1)%LU)
      OPEN(T2D_FILES(T2DBB1)%LU, FILE=FILE_NAME,
     &     FORM='UNFORMATTED',ACTION=T2D_FILES(T2DBB1)%ACTION,
     &     CONVERT='BIG_ENDIAN')
#endif
      REWIND(T2D_FILES(T2DBB1)%LU)
!     READING AND WRITING ASSUMED ARRAYS
      READ(T2D_FILES(T2DBB1)%LU) N,M,NC,
     &     ( TH_LIM_R(I),I=1,2 ),( PH_LIM_R(I),I=1,2 ),C_ID_MOD(1:NC)
!
!-----------------------------------------------------------------------
!
!     COPY OF TH_LIM AND PH_LIM IN DOUBLE PRECISION
!
      TH_LIM(1) = TH_LIM_R(1)
      TH_LIM(2) = TH_LIM_R(2)
      PH_LIM(1) = PH_LIM_R(1)
      PH_LIM(2) = PH_LIM_R(2)
!
!-----------------------------------------------------------------------
!
!     GET ALL AVAILABLE CONSTITUENTS AND SET THEIR INDICES
!
      NCON = NC
      DO IC = 1,NC
        C_ID(IC) = C_ID_MOD(IC)
      ENDDO
      ALLOCATE( CCIND(NCON) )
      CALL DEF_CID( NCON,C_ID,CCIND )
!
!-----------------------------------------------------------------------
!
!     PREPARE STORAGE ON LEVEL BOUNDARIES
!
!      ALLOCATE( TPXO_BOR(3,NPTFRL,NCON) )
      ALLOCATE( TPXO_BOR_R(3,NPTFRL,NCON), TPXO_BOR_I(3,NPTFRL,NCON) )
      DO K=1,NCON
        DO J=1,NPTFRL
          DO I=1,3
!            TPXO_BOR(I,J,K) = CMPLX(0.D0,0.D0,KIND(1.D0))
            TPXO_BOR_R(I,J,K) = 0.D0
            TPXO_BOR_I(I,J,K) = 0.D0
          ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      STIME_MJD = DATE_MJD( MARDAT(2),MARDAT(3),MARDAT(1) ) +
     &            MARTIM(1)/24.D0+MARTIM(2)/1440.D0+MARTIM(3)/86400.D0
!
!-----------------------------------------------------------------------
!
!     TREATMENTS FOR SCHEMATIC TIDES
!
      IF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
        I = 0
        WRITE(LU,*) 'AVAILABLE CONSTITUENTS FOR SCHEMATIC TIDES:'
      ENDIF
!
      IF(TIDALTYPE.EQ.2.OR.TIDALTYPE.EQ.6) THEN
        DO IC = 1,NCON
          IF(C_ID(IC)(2:2).EQ.'1'.OR.C_ID(IC)(2:2).EQ.'2'.OR.
     &       C_ID(IC)(2:2).EQ.'4'.OR.C_ID(IC)(3:3).EQ.'4') THEN
            I = I+1
            WRITE(LU,*) 'C_ID(',IC,') = ',C_ID(IC)
          ENDIF
        ENDDO
      ELSEIF(TIDALTYPE.EQ.3.OR.TIDALTYPE.EQ.5) THEN
        DO IC = 1,NCON
          IF(C_ID(IC).EQ.'m2  '.OR.C_ID(IC).EQ.'s2  '.OR.
     &       C_ID(IC).EQ.'m4  ') THEN
            I = I+1
            WRITE(LU,*) 'C_ID(',IC,') = ',C_ID(IC)
          ENDIF
        ENDDO
      ELSEIF(TIDALTYPE.EQ.4) THEN
        DO IC = 1,NCON
          IF(C_ID(IC).EQ.'m2  '.OR.C_ID(IC).EQ.'m4  ') THEN
            I = I+1
            WRITE(LU,*) 'C_ID(',IC,') = ',C_ID(IC)
          ENDIF
        ENDDO
      ELSE
!     REAL TIDES, TIDALTYPE = 1
        DO IC = 1,NCON
          WRITE(LU,*) 'C_ID(',IC,') = ',C_ID(IC)
        ENDDO
        I = NCON
      ENDIF
!
      IF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
        NCON2 = I
        ALLOCATE(CCIND2(NCON2))
        ALLOCATE(INDW(NCON2),STAT=IERR)
        ALLOCATE(C_ID2(NCON2))
        ALLOCATE(ARGHW(NCON2))
        ALLOCATE(BOR2HW_R(NCON2), BOR2HW_I(NCON2))
      ENDIF
!
      I = 1
!
      IF(TIDALTYPE.EQ.2.OR.TIDALTYPE.EQ.6) THEN
        DO IC = 1,NCON
          IF(C_ID(IC)(2:2).EQ.'1'.OR.C_ID(IC)(2:2).EQ.'2'.OR.
     &       C_ID(IC)(2:2).EQ.'4'.OR.C_ID(IC)(3:3).EQ.'4') THEN
            CCIND2(I) = CCIND(IC)
            INDW(I)   = IC
            C_ID2(I)  = C_ID(IC)
            I = I+1
          ENDIF
        ENDDO
      ELSEIF(TIDALTYPE.EQ.3.OR.TIDALTYPE.EQ.5) THEN
        DO IC = 1,NCON
          IF(C_ID(IC).EQ.'m2  '.OR.C_ID(IC).EQ.'s2  '.OR.
     &       C_ID(IC).EQ.'m4  ') THEN
            CCIND2(I) = CCIND(IC)
            INDW(I)   = IC
            C_ID2(I)  = C_ID(IC)
            I = I+1
          ENDIF
        ENDDO
      ELSEIF(TIDALTYPE.EQ.4) THEN
        DO IC = 1,NCON
          IF(C_ID(IC).EQ.'m2  '.OR.C_ID(IC).EQ.'m4  ') THEN
            CCIND2(I) = CCIND(IC)
            INDW(I)   = IC
            C_ID2(I)  = C_ID(IC)
            I = I+1
          ENDIF
        ENDDO
      ENDIF
!
      IF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
        ALLOCATE( TPXO_BOR2_R(3,NPTFRL,NCON2) )
        ALLOCATE( TPXO_BOR2_I(3,NPTFRL,NCON2) )
        DO K=1,NCON2
          DO J=1,NPTFRL
            DO I=1,3
!              TPXO_BOR2(I,J,K) = CMPLX(0.D0,0.D0,KIND(1.D0))
              TPXO_BOR2_R(I,J,K) = 0.D0
              TPXO_BOR2_I(I,J,K) = 0.D0
            ENDDO
          ENDDO
        ENDDO
!
        IF(ICALHW.EQ.0) THEN
          WRITE(LU,*) 'GIVE A NUMBER OF BOUNDARY ' //
     &       'NODE FOR A LIQUID BOUNDARY ' //
     &       'TO CALIBRATE HIGH WATER FOR SCHEMATIC TIDES'
          CALL PLANTE(1)
          STOP
        ENDIF
        WRITE(LU,*) 'ICALHW =',ICALHW
      ENDIF
!
!-----------------------------------------------------------------------
!
      ALLOCATE( LON(NPOIN) )
      ALLOCATE( LAT(NPOIN) )
!
!  WGS84 NORTHERN OR SOUTHERN UTM, OR MERCATOR FOR TELEMAC
!  WARNING!!! IN TELEMAC DICO, LAMBD0 IS LATITUDE AND PHI0 IS LONGITUDE
!  LAMBD0 AND PHI0 ARE NOT USED FOR GEOSYST = 2 OR 3
      IF(GEOSYST.EQ.2.OR.GEOSYST.EQ.3.OR.GEOSYST.EQ.5) THEN
!       POSSIBLE OFFSET STORED IN THE GEOMETRY FILE
        IF(I_ORIG.NE.0.OR.J_ORIG.NE.0) THEN
          X_ORIG = DBLE(I_ORIG)
          Y_ORIG = DBLE(J_ORIG)
!         X AND Y ARE READ ONLY => OTHER ARRAYS XT AND YT
          ALLOCATE( XT(NPOIN) )
          ALLOCATE( YT(NPOIN) )
          DO K=1,NPOIN
            XT(K) = X(K) + X_ORIG
            YT(K) = Y(K) + Y_ORIG
          ENDDO
          CALL CONV_MERCATOR_TO_DEGDEC(NPOIN,XT(1:NPOIN),YT(1:NPOIN),
     &                                 LON(1:NPOIN),LAT(1:NPOIN),
     &                                 GEOSYST,NUMZONE,PHI0,LAMBD0)
          DEALLOCATE( XT,YT )
        ELSE
          CALL CONV_MERCATOR_TO_DEGDEC(NPOIN,X(1:NPOIN),Y(1:NPOIN),
     &                                 LON(1:NPOIN),LAT(1:NPOIN),
     &                                 GEOSYST,NUMZONE,PHI0,LAMBD0)
        ENDIF
!  NTF LAMBERT
      ELSEIF(GEOSYST.EQ.4) THEN
!       POSSIBLE OFFSET STORED IN THE GEOMETRY FILE
        IF(I_ORIG.NE.0.OR.J_ORIG.NE.0) THEN
          X_ORIG = DBLE(I_ORIG)
          Y_ORIG = DBLE(J_ORIG)
!         X AND Y ARE READ ONLY => OTHER ARRAYS XT AND YT
          ALLOCATE( XT(NPOIN) )
          ALLOCATE( YT(NPOIN) )
          DO K=1,NPOIN
            XT(K) = X(K) + X_ORIG
            YT(K) = Y(K) + Y_ORIG
          ENDDO
          CALL CONV_LAMBERT_TO_DEGDEC(NPOIN,XT(1:NPOIN),YT(1:NPOIN),
     &                                LON(1:NPOIN),LAT(1:NPOIN),
     &                                NUMZONE,T2D_FILES(T2DL93)%LU)
          DEALLOCATE( XT,YT )
        ELSE
          CALL CONV_LAMBERT_TO_DEGDEC(NPOIN,X(1:NPOIN),Y(1:NPOIN),
     &                                LON(1:NPOIN),LAT(1:NPOIN),
     &                                NUMZONE,T2D_FILES(T2DL93)%LU)
        ENDIF
!  WGS84 LONGITUDE/LATITUDE
      ELSEIF(GEOSYST.EQ.1) THEN
        DO K=1,NPOIN
          LON(K) = X(K)
          LAT(K) = Y(K)
        ENDDO
      ELSEIF(GEOSYST.EQ.0) THEN
!  DEFINED BY THE USER
!  THIS IS AN EXAMPLE
        DO K=1,NPOIN
          XL = X(K)
          YL = Y(K)
!  ROTATION WITH ALPHA ANGLE HERE
          XM=XL*COS(ALPHA)-YL*SIN(ALPHA)
          YL=XL*SIN(ALPHA)+YL*COS(ALPHA)
          XL=XM
!  TRANSLATION AND CONVERSION INTO REAL DEGREES
          LON(K) = XO+XL/RADIUS/COS(YO*DTR)*RTD
          LAT(K) = YO+YL/RADIUS            *RTD
        ENDDO
      ELSEIF(GEOSYST.EQ.-1) THEN
!  DEFAULT VALUE
        WRITE(LU,*) 'INCORRECT DEFAULT VALUE FOR THE GEOGRAPHIC'
        WRITE(LU,*) 'SYSTEM. TO BE CHOSEN AMONG THE POSSIBLE CHOICES'
        WRITE(LU,*) 'OR IMPLEMENT THE CONVERSION'
        WRITE(LU,*) 'BY YOURSELF WITH CHOICE 0 IN BORD_TIDAL_BC.F:'
        WRITE(LU,*) '0: DEFINED BY USER,'
        WRITE(LU,*) '1: WGS84 LONGITUDE/LATITUDE IN REAL DEGREES,'
        WRITE(LU,*) '2: WGS84 NORTHERN UTM,'
        WRITE(LU,*) '3: WGS84 SOUTHERN UTM,'
        WRITE(LU,*) '4: LAMBERT,'
        WRITE(LU,*) '5: MERCATOR FOR TELEMAC.'
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) 'GEOGRAPHIC SYSTEM FOR COORDINATES'
        WRITE(LU,*) 'NOT TAKEN INTO ACCOUNT.'
        WRITE(LU,*) 'CHANGE THE SYSTEM OR IMPLEMENT THE CONVERSION'
        WRITE(LU,*) 'BY YOURSELF WITH CHOICE 0 IN BORD_TIDAL_BC.F:'
        WRITE(LU,*) '0: DEFINED BY USER,'
        WRITE(LU,*) '1: WGS84 LONGITUDE/LATITUDE IN REAL DEGREES,'
        WRITE(LU,*) '2: WGS84 NORTHERN UTM,'
        WRITE(LU,*) '3: WGS84 SOUTHERN UTM,'
        WRITE(LU,*) '4: LAMBERT,'
        WRITE(LU,*) '5: MERCATOR FOR TELEMAC.'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO I = 1,NPOIN
        IF( LON(I).GT.PH_LIM(2) ) LON(I) = LON(I) - 360.D0
        IF( LON(I).LT.PH_LIM(1) ) LON(I) = LON(I) + 360.D0
      ENDDO
!
      DO I = 1,NPOIN
        IF(LON(I).GT.PH_LIM(2).OR.LON(I).LT.PH_LIM(1)) THEN
          WRITE(LU,*) 'WARNING!!! THE POINT WITH X/Y IN THE MESH =',
     &                X(I),Y(I)
          WRITE(LU,35) 'CONVERTED TO LONGITUDE/LATITUDE =  ',
     &                 LON(I),LAT(I)
          WRITE(LU,*) 'SEEMS TO BE OUTSIDE OF THE EXTENT OF THE TIDAL '
     &      //        'SOLUTION'
          WRITE(LU,35) 'WHOSE MIN/MAX FOR LONGITUDE ARE:   ',
     &                  PH_LIM(1),PH_LIM(2)
          WRITE(LU,73) 'CHECK THE KEYWORDS GEOGRAPHIC SYSTEM +' //
     &                 ' ZONE NUMBER IN GEOGRAPHIC SYSTEM =',
     &                 GEOSYST,NUMZONE
          WRITE(LU,*)
        ENDIF
        IF(LAT(I).GT.TH_LIM(2).OR.LAT(I).LT.TH_LIM(1)) THEN
          WRITE(LU,*) 'WARNING!!! THE POINT WITH X/Y IN THE MESH =',
     &                X(I),Y(I)
          WRITE(LU,35) 'CONVERTED TO LONGITUDE/LATITUDE =  ',
     &                 LON(I),LAT(I)
          WRITE(LU,*) 'SEEMS TO BE OUTSIDE OF THE EXTENT OF THE TIDAL '
     &      //        'SOLUTION'
          WRITE(LU,35) 'WHOSE MIN/MAX FOR LATITUDE ARE:    ',
     &                  TH_LIM(1),TH_LIM(2)
          WRITE(LU,73) 'CHECK THE KEYWORDS GEOGRAPHIC SYSTEM +' //
     &                 ' ZONE NUMBER IN GEOGRAPHIC SYSTEM =',
     &                 GEOSYST,NUMZONE
          WRITE(LU,*)
        ENDIF
      ENDDO
!
 35   FORMAT(1X,A35,1X,F8.2,F8.2)
 73   FORMAT(1X,A73,I3,I3)
!
!-----------------------------------------------------------------------
!
!     GET DATA FROM THE H-FILE
!
      WRITE(LU,*) ' - ACQUIRING LEVELS'
!
      ALLOCATE( ZT_R(NCON,N,M), ZT_I(NCON,N,M), MASKT(N,M) )
      DO J=1,M
        DO I=1,N
          MASKT(I,J) = 0
        ENDDO
      ENDDO
!
      DO IC = 1,NCON
        REWIND(T2D_FILES(T2DBB1)%LU)
        READ(T2D_FILES(T2DBB1)%LU)  ! HEADER LINE
        DO K = 1,IC-1
          READ(T2D_FILES(T2DBB1)%LU)
        ENDDO
!       /!\ reading complex numbers as pairs of double /!\
!        READ(T2D_FILES(T2DBB1)%LU) ( ( ZT(IC,I,J), I=1,N ), J=1,M )
        READ(T2D_FILES(T2DBB1)%LU)
     &    ( ( ZT_R(IC,I,J), ZT_I(IC,I,J), I=1,N ), J=1,M )
!        WHERE( ZT(IC,:,:).NE.CMPLX(0.D0,0.D0) ) MASKT = 1
        DO J=1,M
          DO I=1,N
            IF( ( ZT_R(IC,I,J).NE.(0.0) ).OR.
     &          ( ZT_I(IC,I,J).NE.(0.0) ) ) MASKT(I,J) = 1
          ENDDO
        ENDDO
      ENDDO
!
!     INTERPOLATE TPXO IN SPACE
!
      WRITE(LU,*) ' - INTERPOLATING LEVELS'
!
      IF(TIDALTYPE.EQ.1) THEN
        ALLOCATE( ZCON_R(NCON), ZCON_I(NCON) )
        DO IPOIN = 1,NPOIN
!
          CALL INTERPT( ZT_R,ZT_I,NCON,N,M,MASKT,TH_LIM,PH_LIM,
     &                  LAT(IPOIN),LON(IPOIN),ZCON_R,ZCON_I,IERR,'z' )
          IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON
              TPXO_BOR_R(1,TPXO_NFR(IPOIN),K) = ZCON_R(K)
              TPXO_BOR_I(1,TPXO_NFR(IPOIN),K) = ZCON_I(K)
            ENDDO
          ENDIF
          IF( IERR.EQ.0 ) H(IPOIN) = H(IPOIN) +
     &        PTIDE(ZCON_R,ZCON_I,C_ID,NCON,CCIND,
     &              STIME_MJD,INTMICON)
!         CHECKING DRY LANDS
!         IF(H(IPOIN).LT.0.D0) H(IPOIN) = 0.D0
!         H(IPOIN) = MAX(H(IPOIN),0.D0)
!         CASE WHEN BOTTOM IS REFERENCED WITH RESPECT TO CD
!               ALSO TAKEN INTO ACCOUNT
          IF(H(IPOIN).LE.-MSL) THEN
            H(IPOIN) = 0.D0
          ELSE
            H(IPOIN) = H(IPOIN) + MSL
          ENDIF
!
        ENDDO
      ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
        ALLOCATE( ZCON_R(NCON2), ZCON_I(NCON2) )
        DO IPOIN = 1,NPOIN
!
          CALL INTERPT_SCHEM( ZT_R,ZT_I,NCON,NCON2,INDW,N,M,MASKT,
     &                        TH_LIM,PH_LIM,LAT(IPOIN),LON(IPOIN),
     &                        ZCON_R,ZCON_I,
     &                        IERR,'z' )
          IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON2
              TPXO_BOR2_R(1,TPXO_NFR(IPOIN),K) = ZCON_R(K)
              TPXO_BOR2_I(1,TPXO_NFR(IPOIN),K) = ZCON_I(K)
            ENDDO
          ENDIF
!
        ENDDO
!
        DO K=1,NCON2
          ARGHW(K)  = 0.D0
!
          DO J=1,NPTFR
            IF(BOUNDARY_COLOUR%I(J).EQ.ICALHW) THEN
              ARGHW(K)  = ATAN2(TPXO_BOR2_I(1,TPXO_NFR(NBOR(J)),K),
     &                          TPXO_BOR2_R(1,TPXO_NFR(NBOR(J)),K))
            ENDIF
          ENDDO
!
          IF(NCSIZE.GT.1) THEN
            ARGHW(K)  = P_MAX(ARGHW(K))  + P_MIN(ARGHW(K))
          ENDIF
!
!          BOR2HW(K) = CMPLX( COS(ARGHW(K)), -SIN(ARGHW(K)), KIND(1.D0))
          BOR2HW_R(K) =  COS(ARGHW(K))
          BOR2HW_I(K) = -SIN(ARGHW(K))
        ENDDO
!
        DO IPTFRL = 1,NPTFRL
          DO K=1,NCON2
!            /!\ complex multiplications /!\
!            TPXO_BOR2(1,IPTFRL,K) = TPXO_BOR2(1,IPTFRL,K)*BOR2HW(K)
            TMP_R = TPXO_BOR2_R(1,IPTFRL,K)*BOR2HW_R(K)
     &            - TPXO_BOR2_I(1,IPTFRL,K)*BOR2HW_I(K)
            TMP_I = TPXO_BOR2_R(1,IPTFRL,K)*BOR2HW_I(K)
     &            + TPXO_BOR2_I(1,IPTFRL,K)*BOR2HW_R(K)
            TPXO_BOR2_R(1,IPTFRL,K) = TMP_R
            TPXO_BOR2_I(1,IPTFRL,K) = TMP_I
          ENDDO
        ENDDO
!
!     SPECIFIC TREATMENTS FOR NEAP TIDES, IN PARTICULAR FOR S2 WAVE
!
          IF(TIDALTYPE.EQ.5) THEN
            DO IPTFRL = 1,NPTFRL
              DO K=1,NCON2
                IF(C_ID2(K).EQ.'s2  ') THEN
                  TPXO_BOR2_R(1,IPTFRL,K) = -TPXO_BOR2_R(1,IPTFRL,K)
                  TPXO_BOR2_I(1,IPTFRL,K) = -TPXO_BOR2_I(1,IPTFRL,K)
                ENDIF
              ENDDO
            ENDDO
          ELSEIF(TIDALTYPE.EQ.6) THEN
            DO IPTFRL = 1,NPTFRL
              DO K=1,NCON2
                IF(C_ID2(K).EQ.'s2  '.OR.C_ID2(K).EQ.'n2  '
     &             .OR.C_ID2(K).EQ.'k2  '.OR.C_ID2(K).EQ.'k1  '
     &             .OR.C_ID2(K).EQ.'p1  '.OR.C_ID2(K).EQ.'q1  '
     &             .OR.C_ID2(K).EQ.'o1  '
     &             .OR.C_ID2(K).EQ.'ms4 '.OR.C_ID2(K).EQ.'mn4 ') THEN
                  TPXO_BOR2_R(1,IPTFRL,K) = -TPXO_BOR2_R(1,IPTFRL,K)
                  TPXO_BOR2_I(1,IPTFRL,K) = -TPXO_BOR2_I(1,IPTFRL,K)
                ENDIF
              ENDDO
            ENDDO
          ENDIF
!
        DO IPOIN = 1,NPOIN
!
          CALL INTERPT_SCHEM( ZT_R,ZT_I,NCON,NCON2,INDW,N,M,MASKT,
     &                        TH_LIM,PH_LIM,LAT(IPOIN),LON(IPOIN),
     &                        ZCON_R,ZCON_I,
     &                        IERR,'z' )
          DO K=1,NCON2
!         /!\ complex multiplications /!\
!            ZCON(K) = ZCON(K)*BOR2HW(K)
            TMP_R = ZCON_R(K)*BOR2HW_R(K) - ZCON_I(K)*BOR2HW_I(K)
            TMP_I = ZCON_R(K)*BOR2HW_I(K) + ZCON_I(K)*BOR2HW_R(K)
            ZCON_R(K) = TMP_R
            ZCON_I(K) = TMP_I
          ENDDO
!
!     SPECIFIC TREATMENTS FOR NEAP TIDES, IN PARTICULAR FOR S2 WAVE
!
          IF(TIDALTYPE.EQ.5) THEN
            DO K=1,NCON2
              IF(C_ID2(K).EQ.'s2  ') THEN
                ZCON_R(K) = -ZCON_R(K)
                ZCON_I(K) = -ZCON_I(K)
              ENDIF
            ENDDO
          ELSEIF(TIDALTYPE.EQ.6) THEN
            DO K=1,NCON2
              IF(C_ID2(K).EQ.'s2  '.OR.C_ID2(K).EQ.'n2  '
     &           .OR.C_ID2(K).EQ.'k2  '.OR.C_ID2(K).EQ.'k1  '
     &           .OR.C_ID2(K).EQ.'p1  '.OR.C_ID2(K).EQ.'q1  '
     &           .OR.C_ID2(K).EQ.'o1  '
     &           .OR.C_ID2(K).EQ.'ms4 '.OR.C_ID2(K).EQ.'mn4 ') THEN
                ZCON_R(K) = -ZCON_R(K)
                ZCON_I(K) = -ZCON_I(K)
              ENDIF
            ENDDO
          ENDIF
!
          IF( IERR.EQ.0 ) H(IPOIN) = H(IPOIN) +
     &        PTIDE_SCHEM(ZCON_R,ZCON_I,C_ID2,NCON2,CCIND2,0.D0)
!         CHECKING DRY LANDS
!         CASE WHEN BOTTOM IS REFERENCED WITH RESPECT TO CD
!         ALSO TAKEN INTO ACCOUNT
          IF(H(IPOIN).LE.-MSL) THEN
            H(IPOIN) = 0.D0
          ELSE
            H(IPOIN) = H(IPOIN) + MSL
          ENDIF
!
        ENDDO
!
      ENDIF
      DEALLOCATE( ZCON_R,ZCON_I,ZT_R,ZT_I,MASKT )
!
!-----------------------------------------------------------------------
!
!     GET DATA FROM THE U-FILE
!
      WRITE(LU,*) ' - ACQUIRING VELOCITIES'
!
      ALLOCATE(UT_R(NCON,N,M),UT_I(NCON,N,M))
      ALLOCATE(VT_R(NCON,N,M),VT_I(NCON,N,M))
      ALLOCATE(MASKU(N,M),MASKV(N,M))
      DO J=1,M
        DO I=1,N
          MASKU(I,J) = 0
          MASKV(I,J) = 0
        ENDDO
      ENDDO
!
      ALLOCATE( UV_R(2,N,M),UV_I(2,N,M) )
#if !defined NO_CONVERT_ENDIAN
      INQUIRE(UNIT=T2D_FILES(T2DBB2)%LU,NAME=FILE_NAME)
      CLOSE(T2D_FILES(T2DBB2)%LU)
      OPEN(T2D_FILES(T2DBB2)%LU, FILE=FILE_NAME,
     &     FORM='UNFORMATTED',ACTION=T2D_FILES(T2DBB2)%ACTION,
     &     CONVERT='BIG_ENDIAN')
#endif
      DO IC = 1,NCON
        REWIND(T2D_FILES(T2DBB2)%LU)
        READ(T2D_FILES(T2DBB2)%LU)  ! HEADER LINE
        DO K = 1,IC-1
          READ(T2D_FILES(T2DBB2)%LU)
        ENDDO
!       reading complex numbers as pairs of double
        READ(T2D_FILES(T2DBB2)%LU) ( (
     &                                 UV_R(1,I,J),UV_I(1,I,J),
     &                                 UV_R(2,I,J),UV_I(2,I,J),
     &                             I=1,N ), J=1,M )
        DO J=1,M
          DO I=1,N
            UT_R(IC,I,J) = UV_R(1,I,J)
            UT_I(IC,I,J) = UV_I(1,I,J)
            VT_R(IC,I,J) = UV_R(2,I,J)
            VT_I(IC,I,J) = UV_I(2,I,J)
          ENDDO
        ENDDO
!        /!\ handle with care /!\
!        WHERE( UT(IC,:,:).NE.CMPLX(0.D0,0.D0) ) MASKU = 1
        DO J=1,M
          DO I=1,N
            IF( ( UT_R(IC,I,J).NE.(0.0) ).OR.
     &          ( UT_I(IC,I,J).NE.(0.0) ) ) MASKU(I,J) = 1
          ENDDO
        ENDDO
!        /!\ handle with care /!\
!        WHERE( VT(IC,:,:).NE.CMPLX(0.D0,0.D0) ) MASKV = 1
        DO J=1,M
          DO I=1,N
            IF( ( VT_R(IC,I,J).NE.(0.0) ).OR.
     &          ( VT_I(IC,I,J).NE.(0.0) ) ) MASKV(I,J) = 1
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE( UV_R, UV_I )
!
!     INTERPOLATE TPXO IN SPACE
!
      WRITE(LU,*) ' - INTERPOLATING VELOCITIES'
!
      IF(TIDALTYPE.EQ.1) THEN
        ALLOCATE( ZCON_R(NCON), ZCON_I(NCON) )
        DO IPOIN = 1,NPOIN
!
!     INITIALISATION AT 0.D0
!
          U(IPOIN) = 0.D0
          V(IPOIN) = 0.D0
!
          CALL INTERPT(UT_R,UT_I,NCON,N,M,MASKU,TH_LIM,PH_LIM,
     &                 LAT(IPOIN),LON(IPOIN),ZCON_R,ZCON_I,IERR,'u')
          IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON
!     VELOCITY READ IN M2/S
              TPXO_BOR_R(2,TPXO_NFR(IPOIN),K) = ZCON_R(K)
              TPXO_BOR_I(2,TPXO_NFR(IPOIN),K) = ZCON_I(K)
            ENDDO
          ENDIF
          IF( IERR.EQ.0 ) U(IPOIN) =
     &        PTIDE(ZCON_R,ZCON_I,C_ID,NCON,CCIND,
     &              STIME_MJD,INTMICON)
!
          CALL INTERPT(VT_R,VT_I,NCON,N,M,MASKV,TH_LIM,PH_LIM,
     &                 LAT(IPOIN),LON(IPOIN),ZCON_R,ZCON_I,IERR,'v')
          IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON
!     VELOCITY READ IN M2/S
              TPXO_BOR_R(3,TPXO_NFR(IPOIN),K) = ZCON_R(K)
              TPXO_BOR_I(3,TPXO_NFR(IPOIN),K) = ZCON_I(K)
            ENDDO
          ENDIF
          IF( IERR.EQ.0 ) V(IPOIN) =
     &        PTIDE(ZCON_R,ZCON_I,C_ID,NCON,CCIND,
     &              STIME_MJD,INTMICON)
!
!     VELOCITY READ IN M/S
          IF( H(IPOIN).GT.HMIN_VIT_IC .AND. VITINI_TPXO ) THEN
            U(IPOIN) = U(IPOIN) / H(IPOIN)
            V(IPOIN) = V(IPOIN) / H(IPOIN)
            SPEED = SQRT( U(IPOIN)**2+V(IPOIN)**2 )
            MAXSP = 2.D0
            IF( SPEED.GT.2.D0 ) THEN
              U(IPOIN) = MAXSP*U(IPOIN)/SPEED
              V(IPOIN) = MAXSP*V(IPOIN)/SPEED
            ENDIF
          ELSE
            U(IPOIN) = 0.D0
            V(IPOIN) = 0.D0
          ENDIF
!
        ENDDO
      ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
        ALLOCATE( ZCON_R(NCON2),  ZCON_I(NCON2) )
        ALLOCATE( ZCON2_R(NCON2), ZCON2_I(NCON2) )
        ALLOCATE( ZCON3_R(NCON2), ZCON3_I(NCON2) )
        DO IPOIN = 1,NPOIN
!
!     INITIALISATION AT 0.D0
!
          U(IPOIN) = 0.D0
          V(IPOIN) = 0.D0
!
          CALL INTERPT_SCHEM( UT_R,UT_I,NCON,NCON2,INDW,N,M,MASKU,
     &                        TH_LIM,PH_LIM,LAT(IPOIN),LON(IPOIN),
     &                        ZCON2_R,ZCON2_I,IERR,'u' )
          CALL INTERPT_SCHEM( VT_R,VT_I,NCON,NCON2,INDW,N,M,MASKV,
     &                        TH_LIM,PH_LIM,LAT(IPOIN),LON(IPOIN),
     &                        ZCON3_R,ZCON3_I,IERR,'v' )
          DO K=1,NCON2
!           /!\ complex multiplications /!\
!            ZCON2(K) = ZCON2(K)*BOR2HW(K)
            TMP_R = ZCON2_R(K)*BOR2HW_R(K) - ZCON2_I(K)*BOR2HW_I(K)
            TMP_I = ZCON2_R(K)*BOR2HW_I(K) + ZCON2_I(K)*BOR2HW_R(K)
            ZCON2_R(K) = TMP_R
            ZCON2_I(K) = TMP_I
!           /!\ complex multiplications /!\
!            ZCON3(K) = ZCON3(K)*BOR2HW(K)
            TMP_R = ZCON3_R(K)*BOR2HW_R(K) - ZCON3_I(K)*BOR2HW_I(K)
            TMP_I = ZCON3_R(K)*BOR2HW_I(K) + ZCON3_I(K)*BOR2HW_R(K)
            ZCON3_R(K) = TMP_R
            ZCON3_I(K) = TMP_I
          ENDDO
!
!     SPECIFIC TREATMENTS FOR NEAP TIDES, IN PARTICULAR FOR S2 WAVE
!
          IF(TIDALTYPE.EQ.5) THEN
            DO K=1,NCON2
              IF(C_ID2(K).EQ.'s2  ') THEN
                ZCON2_R(K) = -ZCON2_R(K)
                ZCON2_I(K) = -ZCON2_I(K)
                ZCON3_R(K) = -ZCON3_R(K)
                ZCON3_I(K) = -ZCON3_I(K)
              ENDIF
            ENDDO
          ELSEIF(TIDALTYPE.EQ.6) THEN
            DO K=1,NCON2
              IF(C_ID2(K).EQ.'s2  '.OR.C_ID2(K).EQ.'n2  '
     &           .OR.C_ID2(K).EQ.'k2  '.OR.C_ID2(K).EQ.'k1  '
     &           .OR.C_ID2(K).EQ.'p1  '.OR.C_ID2(K).EQ.'q1  '
     &           .OR.C_ID2(K).EQ.'o1  '
     &           .OR.C_ID2(K).EQ.'ms4 '.OR.C_ID2(K).EQ.'mn4 ') THEN
                ZCON2_R(K) = -ZCON2_R(K)
                ZCON2_I(K) = -ZCON2_I(K)
                ZCON3_R(K) = -ZCON3_R(K)
                ZCON3_I(K) = -ZCON3_I(K)
              ENDIF
            ENDDO
          ENDIF
!
          IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON2
!     VELOCITY READ IN M2/S
              TPXO_BOR2_R(2,TPXO_NFR(IPOIN),K) = ZCON2_R(K)
              TPXO_BOR2_I(2,TPXO_NFR(IPOIN),K) = ZCON2_I(K)
              TPXO_BOR2_R(3,TPXO_NFR(IPOIN),K) = ZCON3_R(K)
              TPXO_BOR2_I(3,TPXO_NFR(IPOIN),K) = ZCON3_I(K)
            ENDDO
          ENDIF
!
          IF( IERR.EQ.0 ) U(IPOIN) =
     &        PTIDE_SCHEM(ZCON2_R,ZCON2_I,C_ID2,NCON2,CCIND2,0.D0)
!
          IF( IERR.EQ.0 ) V(IPOIN) =
     &        PTIDE_SCHEM(ZCON3_R,ZCON3_I,C_ID2,NCON2,CCIND2,0.D0)
!
!     VELOCITY READ IN M/S
          IF( H(IPOIN).GT.HMIN_VIT_IC  .AND. VITINI_TPXO ) THEN
            U(IPOIN) = U(IPOIN) / H(IPOIN)
            V(IPOIN) = V(IPOIN) / H(IPOIN)
            SPEED = SQRT( U(IPOIN)**2+V(IPOIN)**2 )
            MAXSP = 2.D0
            IF( SPEED.GT.2.D0 ) THEN
              U(IPOIN) = MAXSP*U(IPOIN)/SPEED
              V(IPOIN) = MAXSP*V(IPOIN)/SPEED
            ENDIF
          ELSE
            U(IPOIN) = 0.D0
            V(IPOIN) = 0.D0
          ENDIF
!
        ENDDO
!
        DEALLOCATE( ZCON2_R,ZCON2_I,ZCON3_R,ZCON3_I )
!
      ENDIF
      DEALLOCATE( UT_R,UT_I,VT_R,VT_I,ZCON_R,ZCON_I,MASKU,MASKV )
      DEALLOCATE( LON,LAT )
!
      WRITE(LU,*) 'END OF TPXO INITIALISATION'
!
      DEJA = .TRUE.
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE CONDI_TPXO

!                   *************************
                    SUBROUTINE BORD_TIDE_TPXO
!                   *************************
!
     &(ZF,NBOR,LIHBOR,LIUBOR,NPOIN,NPTFR,TEMPS,
     & NUMLIQ,KENT,KENTU,TIDALTYPE,CTIDE,MSL,CTIDEV,
     & BOUNDARY_COLOUR,HBTIDE,UBTIDE,VBTIDE,
     & ICALHW,MARDAT,MARTIM,
     & T2D_FILES,T2DBB1,T2DBB2,X,Y,GEOSYST,NUMZONE,T2DL93,LAMBD0,PHI0,
     & I_ORIG,J_ORIG,INTMICON,HMIN_VIT_BC)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    MODIFIES THE BOUNDARY CONDITIONS ARRAYS FOR TIDES
!+                WHEN THEY VARY IN TIME.
!+        BASED ON TPXO (FROM HRW)
!+
!
!history  M.S.TURNBULL (HRW), N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+        Addition of the TPXO tidal model by calling CONDI_TPXO
!+        (the TPXO model being coded in DECLARATIONS_TPXO)
!
!history  C-T PHAM (LNHE)
!+        07/05/2012
!+        V6P2
!+
!
!history  C.-T. PHAM (LNHE)
!+        19/03/2015
!+        V7P1
!+        Schematic tides
!
!history  S.E.BOURBAN (HRW)
!+        26/01/2016
!+        V7P2
!+        Replacement of COMPLEX numbers by their imaginary and real parts
!+        (to be compatible with algorithmic differentiation tools)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BOUNDARY_COLOUR|-->| AN INTEGER LINKED TO BOUNDARY POINTS
!|                |   | BY DEFAULT THE LAST LINE OF BOUNDARY CONDITIONS
!|                |   | FILE, HENCE THE GLOBAL BOUNDARY NUMBER, BUT CAN
!|                |   | BE CHANGED BY USER.
!| CTIDE          |-->| COEFFICIENT TO CALIBRATE THE TIDAL RANGE
!| CTIDEV         |-->| COEFFICIENT TO CALIBRATE THE VELOCITIES
!| GEOSYST        |-->| TYPE OF GEOGRAPHIC SYSTEM (WGS84 LONG/LAT, UTM OR LAMBERT)
!| HBTIDE         |<->| WATER DEPTH ON TIDAL BOUNDARY CONDITIONS
!| HMIN_VIT_BC    |-->| MINIMUM DEPTH TO COMPUTE TIDAL VELOCITIES
!|                |   | BOUNDARY CONDITIONS
!| ICALHW         |<->| NUMBER THAT MAY BE CHOSEN BY THE USER
!|                |   | TO CALIBRATE HIGH WATER OR AUTOMATICALLY CHOSEN
!|                |   | IN CASE OF THE MODELLING OF A SCHEMATIC TIDE
!| INTMICON       |-->| IF YES, INFERENCE OF MINOR CONSTITUENTS
!| I_ORIG         |-->| OFFSET ON X COORDINATES
!| J_ORIG         |-->| OFFSET ON Y COORDINATES
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| LAMBD0         |-->| LATITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| MARDAT         |-->| DATE (YEAR,MONTH,DAY)
!| MARTIM         |-->| TIME (HOUR,MINUTE,SECOND)
!| MSL            |---| COEFFICIENT TO CALIBRATE THE SEA LEVEL
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| NUMZONE        |-->| NUMBER OF ZONE WHEN PLANE PROJECTION (UTM OR LAMBERT)
!| PHI0           |-->| LONGITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!| T2DBB1         |-->| ADDRESS OF DATA BASE 1 IN T2D_FILES
!| T2DBB2         |-->| ADDRESS OF DATA BASE 2 IN T2D_FILES
!| T2DL93         |-->| ADDRESS OF GRID CONVERSION FOR LAMBERT 93
!|                |   | IN T2D_FILES
!| T2D_FILES      |-->| ARRAY OF FILES
!| TEMPS          |-->| TIME IN SECONDS
!| TIDALTYPE      |-->| TYPE OF TIDE TO MODEL
!| UBTIDE         |<->| VELOCITY ON TIDAL BOUNDARY CONDITIONS
!| VBTIDE         |<->| VELOCITY ON TIDAL BOUNDARY CONDITIONS
!| X              |-->| COORDINATES X OF THE NODES OF THE MESH
!| Y              |-->| COORDINATES Y OF THE NODES OF THE MESH
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_MIN
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR
      INTEGER, INTENT(IN)             :: T2DBB1,T2DBB2,GEOSYST,NUMZONE
      INTEGER, INTENT(IN)             :: KENT,KENTU,T2DL93
      INTEGER, INTENT(IN)             :: LIHBOR(NPTFR),LIUBOR(NPTFR)
      INTEGER, INTENT(IN)             :: NUMLIQ(NPTFR),NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: TIDALTYPE,MARDAT(3),MARTIM(3)
      INTEGER, INTENT(IN)             :: I_ORIG,J_ORIG
      INTEGER, INTENT(INOUT)          :: ICALHW
      DOUBLE PRECISION, INTENT(IN)    :: TEMPS,CTIDE,MSL,CTIDEV
      DOUBLE PRECISION, INTENT(IN)    :: LAMBD0,PHI0,HMIN_VIT_BC
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      TYPE(BIEF_FILE), INTENT(IN)     :: T2D_FILES(*)
      TYPE(BIEF_OBJ), INTENT(IN)      :: BOUNDARY_COLOUR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: UBTIDE,VBTIDE,HBTIDE
      LOGICAL, INTENT(INOUT)          :: INTMICON
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IC,I,J,K,IPOIN,IERR,NC,N,M
      INTEGER NPTFRL,IPTFRL
      INTEGER, ALLOCATABLE :: MASKT(:,:),MASKU(:,:),MASKV(:,:)
!
      DOUBLE PRECISION PI,DTR,RTD
      DOUBLE PRECISION Z
      DOUBLE PRECISION STIME_MJD
      DOUBLE PRECISION XM,XL,YL,XO,YO,ALPHA,RADIUS,X_ORIG,Y_ORIG
      DOUBLE PRECISION, ALLOCATABLE :: LAT(:),LON(:)
      DOUBLE PRECISION, ALLOCATABLE :: XT(:),YT(:)
      DOUBLE PRECISION PH_LIM(2),TH_LIM(2)
      REAL PH_LIM_R(2),TH_LIM_R(2)
!      COMPLEX, ALLOCATABLE :: ZT(:,:,:)
      REAL, ALLOCATABLE :: ZT_R(:,:,:), ZT_I(:,:,:)
!      COMPLEX, ALLOCATABLE :: UT(:,:,:), VT(:,:,:)
      REAL, ALLOCATABLE :: UT_R(:,:,:), UT_I(:,:,:)
      REAL, ALLOCATABLE :: VT_R(:,:,:), VT_I(:,:,:)
!      COMPLEX, ALLOCATABLE :: UV(:,:,:)
      REAL, ALLOCATABLE :: UV_R(:,:,:), UV_I(:,:,:)
!      COMPLEX(KIND(1.D0)), ALLOCATABLE :: ZCON(:)
      DOUBLE PRECISION, ALLOCATABLE :: ZCON_R(:), ZCON_I(:)
      CHARACTER(LEN=4) C_ID_MOD(TPXO_NCMX)
      DOUBLE PRECISION TMP_R,TMP_I
!
!     N,M: SIZES OF THE GRID SUPPORTING THE TPXO MODEL
!     NC: NUMBER OF CONSTITUENTS AVAILABLE IN THE FILE
!     MASKT,MASKU,MASKV MASKS TO FILTER VALID AND INVALID (U,V,H) VALUES
!     STIME_MJD: DAYS IN MODIFIED JULIAN DAYS
!     PH_LIM,TH_LIM: MIN AND MAX RANGES FOR PHASES AND PERIODES
!     ZT,UT,VT,UV,ZCON: PHASES AND PERIODS FOR U, V AND H
!     HERE DEFINED AS COMPLEX
!     C_ID_MOD INDICES OF AVAILABLE CONTITUENTS AMONGST THE ALL POSSIBLE
!
      INTRINSIC ATAN
!
!-----------------------------------------------------------------------
!
      PI = 4.D0*ATAN(1.D0)
      DTR = PI/180.D0
      RTD = 180.D0/PI
!
!-----------------------------------------------------------------------
!
!  SPECIFIC VALUES FOR THE EXAMPLE OF A GEOGRAPHIC SYSTEM DEFINED BY
!  THE USER
!
      XO = 0.D0
      YO = 51.5D0
!  OR E.G.
!     XO = PHI0       ! LONGITUDE
!     YO = LAMBD0     ! LATITUDE
!
!  ANGLE BETWEEN EAST AXIS ---> X AXIS (TRIGONOMETRIC DEGREES)
      ALPHA = 40.D0
      ALPHA = ALPHA*DTR ! IN RADIANS
!  RADIUS: RADIUS OF THE EARTH
      RADIUS = 6371000.D0
!
!-----------------------------------------------------------------------
!
      STIME_MJD = DATE_MJD( MARDAT(2),MARDAT(3),MARDAT(1) ) +
     &            MARTIM(1)/24.D0+MARTIM(2)/1440.D0+MARTIM(3)/86400.D0
!
!-----------------------------------------------------------------------
!
!  TEST TO CHECK CORRECT VALUES FOR TIDALTYPE
!
      IF(.NOT.DEJA) THEN
        IF(TIDALTYPE.LT.1.OR.TIDALTYPE.GT.6) THEN
          WRITE(LU,*) 'UNEXPECTED VALUE FOR TIDALTYPE=',TIDALTYPE
          WRITE(LU,*) 'IT MUST BE CHOSEN BETWEEN 1 AND 6'
          WRITE(LU,*) 'WITH TPXO TIDAL DATA BASE'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      IF(.NOT.DEJA) THEN
!
!-----------------------------------------------------------------------
!
!     ALLBORD_TPXO REPLACED HERE
!
!brief    Prepare a level boundary filter to store the TPXO constituents
!+        at the boundary. In particular,
!+        count NPTFRL and ALLOCATE and set the filter TPXO_NFR
!
!note     Passing MESH, LIHBOR and LIUBOR as arguments allows
!+        this SUBROUTINE to be called from TELEMAC-2D or TELEMAC-3D
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+        Implementation and generalised for interfacing with
!+        TELEMAC-2D AND 3D
!
!     PREPARE STORAGE ON LEVEL BOUNDARIES
!     NPTFRL: NUMBERS OF BOUNDARY POINTS WHERE TIDE IS PRESCRIBED
!
      ALLOCATE( TPXO_NFR(NPOIN) )
      DO K=1,NPOIN
        TPXO_NFR(K) = 0
      ENDDO
      NPTFRL = 0
      DO K = 1,NPTFR
        IF( LIHBOR(K).EQ.KENT.OR.LIUBOR(K).EQ.KENTU ) THEN
          NPTFRL = NPTFRL + 1
          TPXO_NFR(NBOR(K)) = NPTFRL
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(T2D_FILES(T2DBB1)%NAME(1:1).EQ.' ' .OR.
     &   T2D_FILES(T2DBB2)%NAME(1:1).EQ.' ') THEN
        WRITE(LU,*) 'TPXO FILES NOT AVAILABLE'
        CALL PLANTE(1)
        STOP
      ENDIF
      WRITE(LU,*) 'INITIALISATION BASED ON TPXO:'
!
!-----------------------------------------------------------------------
!
!     READ AVAILABLE DIMENSIONS
!
      REWIND(T2D_FILES(T2DBB1)%LU)
      READ(T2D_FILES(T2DBB1)%LU) N,M,NC,
     &     ( TH_LIM_R(I), I=1, SIZE(TH_LIM_R,1) ),
     &     ( PH_LIM_R(I), I=1, SIZE(PH_LIM_R,1) ),
     &     C_ID_MOD(1:NC)
!
!-----------------------------------------------------------------------
!
!     COPY OF TH_LIM AND PH_LIM IN DOUBLE PRECISION
!
      TH_LIM(1) = TH_LIM_R(1)
      TH_LIM(2) = TH_LIM_R(2)
      PH_LIM(1) = PH_LIM_R(1)
      PH_LIM(2) = PH_LIM_R(2)
!
!-----------------------------------------------------------------------
!
!     GET ALL AVAILABLE CONSTITUENTS AND SET THEIR INDICES
!
      NCON = NC
      DO IC = 1,NC
        C_ID(IC) = C_ID_MOD(IC)
      ENDDO
      ALLOCATE( CCIND(NCON) )
      CALL DEF_CID( NCON,C_ID,CCIND )
!
!-----------------------------------------------------------------------
!
!     PREPARE STORAGE ON LEVEL BOUNDARIES
!
      ALLOCATE( TPXO_BOR_R(3,NPTFRL,NCON), TPXO_BOR_I(3,NPTFRL,NCON) )
      DO K=1,NCON
        DO J=1,NPTFRL
          DO I=1,3
            TPXO_BOR_R(I,J,K) = 0.D0
            TPXO_BOR_I(I,J,K) = 0.D0
          ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!     TREATMENTS FOR SCHEMATIC TIDES
!
      IF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
        I = 0
        WRITE(LU,*) 'AVAILABLE CONSTITUENTS FOR SCHEMATIC TIDES:'
      ENDIF
!
      IF(TIDALTYPE.EQ.2.OR.TIDALTYPE.EQ.6) THEN
        DO IC = 1,NCON
          IF(C_ID(IC)(2:2).EQ.'1'.OR.C_ID(IC)(2:2).EQ.'2'.OR.
     &       C_ID(IC)(2:2).EQ.'4'.OR.C_ID(IC)(3:3).EQ.'4') THEN
            I = I+1
            WRITE(LU,*) 'C_ID(',IC,') = ',C_ID(IC)
          ENDIF
        ENDDO
      ELSEIF(TIDALTYPE.EQ.3.OR.TIDALTYPE.EQ.5) THEN
        DO IC = 1,NCON
          IF(C_ID(IC).EQ.'m2  '.OR.C_ID(IC).EQ.'s2  '.OR.
     &       C_ID(IC).EQ.'m4  ') THEN
            I = I+1
            WRITE(LU,*) 'C_ID(',IC,') = ',C_ID(IC)
          ENDIF
        ENDDO
      ELSEIF(TIDALTYPE.EQ.4) THEN
        DO IC = 1,NCON
          IF(C_ID(IC).EQ.'m2  '.OR.C_ID(IC).EQ.'m4  ') THEN
            I = I+1
            WRITE(LU,*) 'C_ID(',IC,') = ',C_ID(IC)
          ENDIF
        ENDDO
      ELSE
!     REAL TIDES, TIDALTYPE = 1
        DO IC = 1,NCON
          WRITE(LU,*) 'C_ID(',IC,') = ',C_ID(IC)
        ENDDO
        I = NCON
      ENDIF
!
      IF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
        NCON2 = I
        ALLOCATE(CCIND2(NCON2))
        ALLOCATE(INDW(NCON2),STAT=IERR)
        ALLOCATE(C_ID2(NCON2))
        ALLOCATE(ARGHW(NCON2))
        ALLOCATE(BOR2HW_R(NCON2), BOR2HW_I(NCON2))
      ENDIF
!
      I = 1
!
      IF(TIDALTYPE.EQ.2.OR.TIDALTYPE.EQ.6) THEN
        DO IC = 1,NCON
          IF(C_ID(IC)(2:2).EQ.'1'.OR.C_ID(IC)(2:2).EQ.'2'.OR.
     &       C_ID(IC)(2:2).EQ.'4'.OR.C_ID(IC)(3:3).EQ.'4') THEN
            CCIND2(I) = CCIND(IC)
            INDW(I)   = IC
            C_ID2(I)  = C_ID(IC)
            I = I+1
          ENDIF
        ENDDO
      ELSEIF(TIDALTYPE.EQ.3.OR.TIDALTYPE.EQ.5) THEN
        DO IC = 1,NCON
          IF(C_ID(IC).EQ.'m2  '.OR.C_ID(IC).EQ.'s2  '.OR.
     &       C_ID(IC).EQ.'m4  ') THEN
            CCIND2(I) = CCIND(IC)
            INDW(I)   = IC
            C_ID2(I)  = C_ID(IC)
            I = I+1
          ENDIF
        ENDDO
      ELSEIF(TIDALTYPE.EQ.4) THEN
        DO IC = 1,NCON
          IF(C_ID(IC).EQ.'m2  '.OR.C_ID(IC).EQ.'m4  ') THEN
            CCIND2(I) = CCIND(IC)
            INDW(I)   = IC
            C_ID2(I)  = C_ID(IC)
            I = I+1
          ENDIF
        ENDDO
      ENDIF
!
      IF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
        ALLOCATE( TPXO_BOR2_R(3,NPTFRL,NCON2) )
        ALLOCATE( TPXO_BOR2_I(3,NPTFRL,NCON2) )
        DO K=1,NCON2
          DO J=1,NPTFRL
            DO I=1,3
!              TPXO_BOR2(I,J,K) = CMPLX(0.D0,0.D0,KIND(1.D0))
              TPXO_BOR2_R(I,J,K) = 0.D0
              TPXO_BOR2_I(I,J,K) = 0.D0
            ENDDO
          ENDDO
        ENDDO
!
        IF(ICALHW.EQ.0) THEN
          WRITE(LU,*) 'GIVE A NUMBER OF BOUNDARY ' //
     &       'NODE FOR A LIQUID BOUNDARY ' //
     &       'TO CALIBRATE HIGH WATER FOR SCHEMATIC TIDES'
          CALL PLANTE(1)
          STOP
        ENDIF
        WRITE(LU,*) 'ICALHW =',ICALHW
      ENDIF
!
!-----------------------------------------------------------------------
!
      ALLOCATE( LON(NPOIN) )
      ALLOCATE( LAT(NPOIN) )
!
!  WGS84 NORTHERN OR SOUTHERN UTM, OR MERCATOR FOR TELEMAC
!  WARNING!!! IN TELEMAC DICO, LAMBD0 IS LATITUDE AND PHI0 IS LONGITUDE
!  LAMBD0 AND PHI0 ARE NOT USED FOR GEOSYST = 2 OR 3
      IF(GEOSYST.EQ.2.OR.GEOSYST.EQ.3.OR.GEOSYST.EQ.5) THEN
!       POSSIBLE OFFSET STORED IN THE GEOMETRY FILE
        IF(I_ORIG.NE.0.OR.J_ORIG.NE.0) THEN
          X_ORIG = DBLE(I_ORIG)
          Y_ORIG = DBLE(J_ORIG)
!         X AND Y ARE READ ONLY => OTHER ARRAYS XT AND YT
          ALLOCATE( XT(NPOIN) )
          ALLOCATE( YT(NPOIN) )
          DO K=1,NPOIN
            XT(K) = X(K) + X_ORIG
            YT(K) = Y(K) + Y_ORIG
          ENDDO
          CALL CONV_MERCATOR_TO_DEGDEC(NPOIN,XT(1:NPOIN),YT(1:NPOIN),
     &                                 LON(1:NPOIN),LAT(1:NPOIN),
     &                                 GEOSYST,NUMZONE,PHI0,LAMBD0)
          DEALLOCATE( XT,YT )
        ELSE
          CALL CONV_MERCATOR_TO_DEGDEC(NPOIN,X(1:NPOIN),Y(1:NPOIN),
     &                                 LON(1:NPOIN),LAT(1:NPOIN),
     &                                 GEOSYST,NUMZONE,PHI0,LAMBD0)
        ENDIF
!
!  NTF LAMBERT
      ELSEIF(GEOSYST.EQ.4) THEN
!       POSSIBLE OFFSET STORED IN THE GEOMETRY FILE
        IF(I_ORIG.NE.0.OR.J_ORIG.NE.0) THEN
          X_ORIG = DBLE(I_ORIG)
          Y_ORIG = DBLE(J_ORIG)
!         X AND Y ARE READ ONLY => OTHER ARRAYS XT AND YT
          ALLOCATE( XT(NPOIN) )
          ALLOCATE( YT(NPOIN) )
          DO K=1,NPOIN
            XT(K) = X(K) + X_ORIG
            YT(K) = Y(K) + Y_ORIG
          ENDDO
          CALL CONV_LAMBERT_TO_DEGDEC(NPOIN,XT(1:NPOIN),YT(1:NPOIN),
     &                                LON(1:NPOIN),LAT(1:NPOIN),
     &                                NUMZONE,T2D_FILES(T2DL93)%LU)
          DEALLOCATE( XT,YT )
        ELSE
          CALL CONV_LAMBERT_TO_DEGDEC(NPOIN,X(1:NPOIN),Y(1:NPOIN),
     &                                LON(1:NPOIN),LAT(1:NPOIN),
     &                                NUMZONE,T2D_FILES(T2DL93)%LU)
        ENDIF
!  WGS84 LONGITUDE/LATITUDE
      ELSEIF(GEOSYST.EQ.1) THEN
        DO K=1,NPOIN
          LON(K) = X(K)
          LAT(K) = Y(K)
        ENDDO
      ELSEIF(GEOSYST.EQ.0) THEN
!  DEFINED BY THE USER
!  THIS IS AN EXAMPLE
        DO K=1,NPOIN
          XL = X(K)
          YL = Y(K)
!  ROTATION WITH ALPHA ANGLE HERE
          XM=XL*COS(ALPHA)-YL*SIN(ALPHA)
          YL=XL*SIN(ALPHA)+YL*COS(ALPHA)
          XL=XM
!  TRANSLATION AND CONVERSION INTO REAL DEGREES
          LON(K) = XO+XL/RADIUS/COS(YO*DTR)*RTD
          LAT(K) = YO+YL/RADIUS            *RTD
        ENDDO
      ELSEIF(GEOSYST.EQ.-1) THEN
!  DEFAULT VALUE
        WRITE(LU,*) 'INCORRECT DEFAULT VALUE FOR THE GEOGRAPHIC'
        WRITE(LU,*) 'SYSTEM. TO BE CHOSEN AMONG THE POSSIBLE CHOICES'
        WRITE(LU,*) 'OR IMPLEMENT THE CONVERSION'
        WRITE(LU,*) 'BY YOURSELF WITH CHOICE 0 IN BORD_TIDAL_BC.F:'
        WRITE(LU,*) '0: DEFINED BY USER,'
        WRITE(LU,*) '1: WGS84 LONGITUDE/LATITUDE IN REAL DEGREES,'
        WRITE(LU,*) '2: WGS84 NORTHERN UTM,'
        WRITE(LU,*) '3: WGS84 SOUTHERN UTM,'
        WRITE(LU,*) '4: LAMBERT,'
        WRITE(LU,*) '5: MERCATOR FOR TELEMAC.'
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) 'GEOGRAPHIC SYSTEM FOR COORDINATES'
        WRITE(LU,*) 'NOT TAKEN INTO ACCOUNT.'
        WRITE(LU,*) 'CHANGE THE SYSTEM OR IMPLEMENT THE CONVERSION'
        WRITE(LU,*) 'BY YOURSELF WITH CHOICE 0 IN BORD_TIDAL_BC.F:'
        WRITE(LU,*) '0: DEFINED BY USER,'
        WRITE(LU,*) '1: WGS84 LONGITUDE/LATITUDE IN REAL DEGREES,'
        WRITE(LU,*) '2: WGS84 NORTHERN UTM,'
        WRITE(LU,*) '3: WGS84 SOUTHERN UTM,'
        WRITE(LU,*) '4: LAMBERT,'
        WRITE(LU,*) '5: MERCATOR FOR TELEMAC.'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO I = 1,NPOIN
        IF( LON(I).GT.PH_LIM(2) ) LON(I) = LON(I) - 360.D0
        IF( LON(I).LT.PH_LIM(1) ) LON(I) = LON(I) + 360.D0
      ENDDO
!
      DO I = 1,NPOIN
        IF(     (LON(I).GT.PH_LIM(2).OR.LON(I).LT.PH_LIM(1))
     &     .AND.TPXO_NFR(I).NE.0) THEN
          WRITE(LU,*) 'WARNING!!! THE TIDAL BOUNDARY POINT WITH X/Y ',
     &                'IN THE MESH =',X(I),Y(I)
          WRITE(LU,35) 'CONVERTED TO LONGITUDE/LATITUDE =  ',
     &                 LON(I),LAT(I)
          WRITE(LU,*) 'SEEMS TO BE OUTSIDE OF THE EXTENT OF THE TIDAL '
     &      //        'SOLUTION'
          WRITE(LU,35) 'WHOSE MIN/MAX FOR LONGITUDE ARE:   ',
     &                  PH_LIM(1),PH_LIM(2)
          WRITE(LU,73) 'CHECK THE KEYWORDS GEOGRAPHIC SYSTEM +' //
     &                 ' ZONE NUMBER IN GEOGRAPHIC SYSTEM =',
     &                 GEOSYST,NUMZONE
          WRITE(LU,*)
        ENDIF
        IF(     (LAT(I).GT.TH_LIM(2).OR.LAT(I).LT.TH_LIM(1))
     &     .AND.TPXO_NFR(I).NE.0) THEN
          WRITE(LU,*) 'WARNING!!! THE TIDAL BOUNDARY POINT WITH X/Y ',
     &                'IN THE MESH =',X(I),Y(I)
          WRITE(LU,35) 'CONVERTED TO LONGITUDE/LATITUDE =  ',
     &                 LON(I),LAT(I)
          WRITE(LU,*) 'SEEMS TO BE OUTSIDE OF THE EXTENT OF THE TIDAL '
     &      //        'SOLUTION'
          WRITE(LU,35) 'WHOSE MIN/MAX FOR LATITUDE ARE:    ',
     &                  TH_LIM(1),TH_LIM(2)
          WRITE(LU,73) 'CHECK THE KEYWORDS GEOGRAPHIC SYSTEM +' //
     &                 ' ZONE NUMBER IN GEOGRAPHIC SYSTEM =',
     &                 GEOSYST,NUMZONE
          WRITE(LU,*)
        ENDIF
      ENDDO
!
 35   FORMAT(1X,A35,1X,F8.2,F8.2)
 73   FORMAT(1X,A73,I3,I3)
!
!-----------------------------------------------------------------------
!
!     GET DATA FROM THE H-FILE
!
      WRITE(LU,*) ' - ACQUIRING LEVELS'
!
      ALLOCATE( ZT_R(NCON,N,M), ZT_I(NCON,N,M), MASKT(N,M) )
      DO J=1,M
        DO I=1,N
          MASKT(I,J) = 0
        ENDDO
      ENDDO
!
      DO IC = 1,NCON
        REWIND(T2D_FILES(T2DBB1)%LU)
        READ(T2D_FILES(T2DBB1)%LU)  ! HEADER LINE
        DO K = 1,IC-1
          READ(T2D_FILES(T2DBB1)%LU)
        ENDDO
!       /!\ reading complex numbers as pairs of double /!\
!        READ(T2D_FILES(T2DBB1)%LU) ( ( ZT(IC,I,J), I=1,N ), J=1,M )
        READ(T2D_FILES(T2DBB1)%LU)
     &    ( ( ZT_R(IC,I,J), ZT_I(IC,I,J), I=1,N ), J=1,M )
!        /!\ handle with care /!\
!        WHERE( ZT(IC,:,:).NE.CMPLX(0.D0,0.D0) ) MASKT = 1
        DO J=1,M
          DO I=1,N
            IF( ( ZT_R(IC,I,J).NE.(0.0) ).OR.
     &          ( ZT_I(IC,I,J).NE.(0.0) ) ) MASKT(I,J) = 1
          ENDDO
        ENDDO
      ENDDO
!
!     INTERPOLATE TPXO IN SPACE
!
      WRITE(LU,*) ' - INTERPOLATING LEVELS'
!
      IF(TIDALTYPE.EQ.1) THEN
        ALLOCATE( ZCON_R(NCON), ZCON_I(NCON) )
        DO IPOIN = 1,NPOIN
!
          CALL INTERPT( ZT_R,ZT_I,NCON,N,M,MASKT,TH_LIM,PH_LIM,
     &                  LAT(IPOIN),LON(IPOIN),ZCON_R,ZCON_I,IERR,'z' )
          IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON
              TPXO_BOR_R(1,TPXO_NFR(IPOIN),K) = ZCON_R(K)
              TPXO_BOR_I(1,TPXO_NFR(IPOIN),K) = ZCON_I(K)
            ENDDO
          ENDIF
!
        ENDDO
      ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
        ALLOCATE( ZCON_R(NCON2), ZCON_I(NCON2) )
        DO IPOIN = 1,NPOIN
!
          CALL INTERPT_SCHEM( ZT_R,ZT_I,NCON,NCON2,INDW,N,M,MASKT,
     &                        TH_LIM,PH_LIM,LAT(IPOIN),LON(IPOIN),
     &                        ZCON_R,ZCON_I,IERR,'z' )
          IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON2
              TPXO_BOR2_R(1,TPXO_NFR(IPOIN),K) = ZCON_R(K)
              TPXO_BOR2_I(1,TPXO_NFR(IPOIN),K) = ZCON_I(K)
            ENDDO
          ENDIF
!
        ENDDO
!
        DO K=1,NCON2
          ARGHW(K)  = 0.D0
!
          DO J=1,NPTFR
            IF(BOUNDARY_COLOUR%I(J).EQ.ICALHW) THEN
!              ARGHW(K)  = ATAN2(AIMAG(TPXO_BOR2(1,TPXO_NFR(NBOR(J)),K)),
!     &                           REAL(TPXO_BOR2(1,TPXO_NFR(NBOR(J)),K)))
              ARGHW(K)  = ATAN2(TPXO_BOR2_I(1,TPXO_NFR(NBOR(J)),K),
     &                          TPXO_BOR2_R(1,TPXO_NFR(NBOR(J)),K))
            ENDIF
          ENDDO
!
          IF(NCSIZE.GT.1) THEN
            ARGHW(K)  = P_MAX(ARGHW(K))  + P_MIN(ARGHW(K))
          ENDIF
!
!          BOR2HW(K) = CMPLX( COS(ARGHW(K)), -SIN(ARGHW(K)), KIND(1.D0))
          BOR2HW_R(K) = COS(ARGHW(K))
          BOR2HW_I(K) = -SIN(ARGHW(K))
        ENDDO
!
        DO IPTFRL = 1,NPTFRL
          DO K=1,NCON2
!         /!\ complex multiplications /!\
!            TPXO_BOR2(1,IPTFRL,K) = TPXO_BOR2(1,IPTFRL,K)*BOR2HW(K)
            TMP_R = TPXO_BOR2_R(1,IPTFRL,K)*BOR2HW_R(K)
     &            - TPXO_BOR2_I(1,IPTFRL,K)*BOR2HW_I(K)
            TMP_I = TPXO_BOR2_R(1,IPTFRL,K)*BOR2HW_I(K)
     &            + TPXO_BOR2_I(1,IPTFRL,K)*BOR2HW_R(K)
            TPXO_BOR2_R(1,IPTFRL,K) = TMP_R
            TPXO_BOR2_I(1,IPTFRL,K) = TMP_I
          ENDDO
        ENDDO
      ENDIF
      DEALLOCATE( ZCON_R,ZCON_I,ZT_R,ZT_I,MASKT )
!
!-----------------------------------------------------------------------
!
!     GET DATA FROM THE U-FILE
!
      WRITE(LU,*) ' - ACQUIRING VELOCITIES'
!
      ALLOCATE( UT_R(NCON,N,M),UT_I(NCON,N,M) )
      ALLOCATE( VT_R(NCON,N,M),VT_I(NCON,N,M) )
      ALLOCATE( MASKU(N,M),MASKV(N,M) )
      DO J=1,M
        DO I=1,N
          MASKU(I,J) = 0
          MASKV(I,J) = 0
        ENDDO
      ENDDO
!
      ALLOCATE( UV_R(2,N,M),UV_I(2,N,M) )
      DO IC = 1,NCON
        REWIND(T2D_FILES(T2DBB2)%LU)
        READ(T2D_FILES(T2DBB2)%LU)  ! HEADER LINE
        DO K = 1,IC-1
          READ(T2D_FILES(T2DBB2)%LU)
        ENDDO
!       /!\ reading complex numbers as pairs of double /!\
!        READ(T2D_FILES(T2DBB2)%LU) UV
        READ(T2D_FILES(T2DBB2)%LU) ( (
     &                                 UV_R(1,I,J),UV_I(1,I,J),
     &                                 UV_R(2,I,J),UV_I(2,I,J),
     &                             I=1,N ), J=1,M )
        DO J=1,M
          DO I=1,N
            UT_R(IC,I,J) = UV_R(1,I,J)
            UT_I(IC,I,J) = UV_I(1,I,J)
            VT_R(IC,I,J) = UV_R(2,I,J)
            VT_I(IC,I,J) = UV_I(2,I,J)
          ENDDO
        ENDDO
!        /!\ handle with care /!\
!        WHERE( UT(IC,:,:).NE.CMPLX(0.D0,0.D0) ) MASKU = 1
        DO J=1,M
          DO I=1,N
            IF( ( UT_R(IC,I,J).NE.(0.0) ).OR.
     &          ( UT_I(IC,I,J).NE.(0.0) ) ) MASKU(I,J) = 1
          ENDDO
        ENDDO
!        /!\ handle with care /!\
!        WHERE( VT(IC,:,:).NE.CMPLX(0.D0,0.D0) ) MASKV = 1
        DO J=1,M
          DO I=1,N
            IF( ( VT_R(IC,I,J).NE.(0.0) ).OR.
     &          ( VT_I(IC,I,J).NE.(0.0) ) ) MASKV(I,J) = 1
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE( UV_R, UV_I )
!
!     INTERPOLATE TPXO IN SPACE
!
      WRITE(LU,*) ' - INTERPOLATING VELOCITIES'
!
      IF(TIDALTYPE.EQ.1) THEN
        ALLOCATE( ZCON_R(NCON), ZCON_I(NCON) )
        DO IPOIN = 1,NPOIN
!
          CALL INTERPT(UT_R,UT_I,NCON,N,M,MASKU,TH_LIM,PH_LIM,
     &                 LAT(IPOIN),LON(IPOIN),ZCON_R,ZCON_I,IERR,'u')
          IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON
!     VELOCITY READ IN M2/S
              TPXO_BOR_R(2,TPXO_NFR(IPOIN),K) = ZCON_R(K)
              TPXO_BOR_I(2,TPXO_NFR(IPOIN),K) = ZCON_I(K)
            ENDDO
          ENDIF
!
          CALL INTERPT(VT_R,VT_I,NCON,N,M,MASKV,TH_LIM,PH_LIM,
     &                 LAT(IPOIN),LON(IPOIN),ZCON_R,ZCON_I,IERR,'v')
          IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON
!     VELOCITY READ IN M2/S
              TPXO_BOR_R(3,TPXO_NFR(IPOIN),K) = ZCON_R(K)
              TPXO_BOR_I(3,TPXO_NFR(IPOIN),K) = ZCON_I(K)
            ENDDO
          ENDIF
!
        ENDDO
      ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
        ALLOCATE( ZCON_R(NCON2), ZCON_I(NCON2) )
        DO IPOIN = 1,NPOIN
!
          CALL INTERPT_SCHEM( UT_R,UT_I,NCON,NCON2,INDW,N,M,MASKU,
     &                        TH_LIM,PH_LIM,LAT(IPOIN),LON(IPOIN),
     &                        ZCON_R,ZCON_I,IERR,'u' )
          IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON2
!     VELOCITY READ IN M2/S
!         /!\ complex multiplications /!\
!              TPXO_BOR2(2,TPXO_NFR(IPOIN),K) = ZCON(K)*BOR2HW(K)
              TPXO_BOR2_R(2,TPXO_NFR(IPOIN),K) = ZCON_R(K)*BOR2HW_R(K)
     &                                         - ZCON_I(K)*BOR2HW_I(K)
              TPXO_BOR2_I(2,TPXO_NFR(IPOIN),K) = ZCON_R(K)*BOR2HW_I(K)
     &                                         + ZCON_I(K)*BOR2HW_R(K)
            ENDDO
          ENDIF
!
          CALL INTERPT_SCHEM( VT_R,VT_I,NCON,NCON2,INDW,N,M,MASKV,
     &                        TH_LIM,PH_LIM,LAT(IPOIN),LON(IPOIN),
     &                        ZCON_R,ZCON_I,IERR,'v' )
          IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON2
!     VELOCITY READ IN M2/S
!         /!\ complex multiplications /!\
!              TPXO_BOR2(3,TPXO_NFR(IPOIN),K) = ZCON(K)*BOR2HW(K)
              TPXO_BOR2_R(3,TPXO_NFR(IPOIN),K) = ZCON_R(K)*BOR2HW_R(K)
     &                                         - ZCON_I(K)*BOR2HW_I(K)
              TPXO_BOR2_I(3,TPXO_NFR(IPOIN),K) = ZCON_R(K)*BOR2HW_I(K)
     &                                         + ZCON_I(K)*BOR2HW_R(K)
            ENDDO
          ENDIF
!
        ENDDO
      ENDIF
!
!     SPECIFIC TREATMENTS FOR NEAP TIDES, IN PARTICULAR FOR S2 WAVE
!
      IF(TIDALTYPE.EQ.5) THEN
        DO IPOIN = 1,NPOIN
          J = TPXO_NFR(IPOIN)
          IF( J.NE.0 ) THEN
            DO K=1,NCON2
              IF(C_ID2(K).EQ.'s2  ') THEN
                TPXO_BOR2_R(1,J,K) = -TPXO_BOR2_R(1,J,K)
                TPXO_BOR2_I(1,J,K) = -TPXO_BOR2_I(1,J,K)
                TPXO_BOR2_R(2,J,K) = -TPXO_BOR2_R(2,J,K)
                TPXO_BOR2_I(2,J,K) = -TPXO_BOR2_I(2,J,K)
                TPXO_BOR2_R(3,J,K) = -TPXO_BOR2_R(3,J,K)
                TPXO_BOR2_I(3,J,K) = -TPXO_BOR2_I(3,J,K)
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ELSEIF(TIDALTYPE.EQ.6) THEN
        DO IPOIN = 1,NPOIN
          J = TPXO_NFR(IPOIN)
          IF( J.NE.0 ) THEN
            DO K=1,NCON2
              IF(C_ID2(K).EQ.'s2  '.OR.C_ID2(K).EQ.'n2  '
     &           .OR.C_ID2(K).EQ.'k2  '.OR.C_ID2(K).EQ.'k1  '
     &           .OR.C_ID2(K).EQ.'p1  '.OR.C_ID2(K).EQ.'q1  '
     &           .OR.C_ID2(K).EQ.'o1  '
     &           .OR.C_ID2(K).EQ.'ms4 '.OR.C_ID2(K).EQ.'mn4 ') THEN
                TPXO_BOR2_R(1,J,K) = -TPXO_BOR2_R(1,J,K)
                TPXO_BOR2_I(1,J,K) = -TPXO_BOR2_I(1,J,K)
                TPXO_BOR2_R(2,J,K) = -TPXO_BOR2_R(2,J,K)
                TPXO_BOR2_I(2,J,K) = -TPXO_BOR2_I(2,J,K)
                TPXO_BOR2_R(3,J,K) = -TPXO_BOR2_R(3,J,K)
                TPXO_BOR2_I(3,J,K) = -TPXO_BOR2_I(3,J,K)
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
      DEALLOCATE( UT_R,UT_I,VT_R,VT_I,ZCON_R,ZCON_I,MASKU,MASKV )
      DEALLOCATE( LON,LAT )
!
      WRITE(LU,*) 'END OF TPXO INITIALISATION'
!
      DEJA = .TRUE.
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  LOOP ON ALL BOUNDARY POINTS
!
      DO K=1,NPTFR
!
!     LEVEL IMPOSED WITH VALUE GIVEN IN THE CAS FILE (NCOTE0)
!
        IF(LIHBOR(K).EQ.KENT) THEN
!         BEGINNING OF PRESCRIBED DEPTHS
!
!  TYPE OF TIDE TO MODEL
!  1: REAL TIDE
!  2: ASTRONOMICAL TIDE      (COEF. NEARLY 120)
!  3: MEAN SPRING TIDE       (COEF. NEARLY 95)
!  4: MEAN TIDE              (COEF. NEARLY 70)
!  5: MEAN NEAP TIDE         (COEF. NEARLY 45)
!  6: ASTRONOMICAL NEAP TIDE (COEF. NEARLY 20)
!
          IF(TIDALTYPE.EQ.1) THEN
            Z = CTIDE*TPXO_PTIDE(1,NBOR(K),TPXO_NFR,
     &                           TPXO_BOR_R,TPXO_BOR_I,C_ID,
     &                           NCON,CCIND,
     &                           STIME_MJD+TEMPS/86400.D0,INTMICON)
     &        + MSL
            HBTIDE%R(K) = MAX( 0.D0 , Z-ZF(NBOR(K)) )
          ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
            Z = CTIDE*TPXO_PTIDE_SCHEM(1,NBOR(K),TPXO_NFR,
     &                                 TPXO_BOR2_R,TPXO_BOR2_I,
     &                                 C_ID2,NCON2,CCIND2,TEMPS)
     &        + MSL
            HBTIDE%R(K) = MAX( 0.D0 , Z-ZF(NBOR(K)) )
          ENDIF
!         ELSE HBOR TAKEN IN BOUNDARY CONDITIONS FILE
!$$$          ENDIF
        ENDIF
!
!  VELOCITY IMPOSED: ONE USES THE OUTGOING DIRECTION
!                    PROVIDED BY THE USER.
!
      IF(LIUBOR(K).EQ.KENTU) THEN
!$$$      IF(LIUBOR(K).EQ.KENTU.AND.
!$$$     &  (NVITES.NE.0.OR.NOMIMP(1:1).NE.' ')) THEN
!
!       POINTS ON WEIRS HAVE NUMLIQ(K)=0
        IF(NUMLIQ(K).GT.0) THEN
!
!         BEGINNING OF PRESCRIBED VELOCITIES
!
!  TYPE OF TIDE TO MODEL
!  1: REAL TIDE
!  2: ASTRONOMICAL TIDE      (COEF. NEARLY 120)
!  3: MEAN SPRING TIDE       (COEF. NEARLY 95)
!  4: MEAN TIDE              (COEF. NEARLY 70)
!  5: MEAN NEAP TIDE         (COEF. NEARLY 45)
!  6: ASTRONOMICAL NEAP TIDE (COEF. NEARLY 20)
!
          IF(TIDALTYPE.EQ.1) THEN
!  IF LIHBOR(K).EQ.4, Z IS NOT CALCULATED BEFORE
            Z = CTIDE*TPXO_PTIDE(1,NBOR(K),TPXO_NFR,
     &                           TPXO_BOR_R,TPXO_BOR_I,C_ID,
     &                           NCON,CCIND,
     &                           STIME_MJD+TEMPS/86400.D0,INTMICON)
     &        + MSL
!$$$           IF(PROVEL(NUMLIQ(K)).EQ.1) THEN
            UBTIDE%R(K) =
     &              CTIDEV*TPXO_PTIDE(2,NBOR(K),TPXO_NFR,
     &                                TPXO_BOR_R,TPXO_BOR_I,C_ID,
     &                                NCON,CCIND,
     &                                STIME_MJD+TEMPS/86400.D0,INTMICON)
     &              / MAX( HMIN_VIT_BC , Z-ZF(NBOR(K)) )
            VBTIDE%R(K) =
     &              CTIDEV*TPXO_PTIDE(3,NBOR(K),TPXO_NFR,
     &                                TPXO_BOR_R,TPXO_BOR_I,C_ID,
     &                                NCON,CCIND,
     &                                STIME_MJD+TEMPS/86400.D0,INTMICON)
     &              / MAX( HMIN_VIT_BC , Z-ZF(NBOR(K)) )
!$$$           ENDIF
          ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
!  IF LIHBOR(K).EQ.4, Z IS NOT CALCULATED BEFORE
            Z = CTIDE*TPXO_PTIDE_SCHEM(1,NBOR(K),TPXO_NFR,
     &                                 TPXO_BOR2_R,TPXO_BOR2_I,
     &                                 C_ID2,NCON2,CCIND2,TEMPS)
     &        + MSL
!$$$           IF(PROVEL(NUMLIQ(K)).EQ.1) THEN
            UBTIDE%R(K) =
     &         CTIDEV*TPXO_PTIDE_SCHEM(2,NBOR(K),TPXO_NFR,
     &                                 TPXO_BOR2_R,TPXO_BOR2_I,
     &                                 C_ID2,NCON2,CCIND2,TEMPS)
     &               / MAX( HMIN_VIT_BC , Z-ZF(NBOR(K)) )
            VBTIDE%R(K) =
     &         CTIDEV*TPXO_PTIDE_SCHEM(3,NBOR(K),TPXO_NFR,
     &                                 TPXO_BOR2_R,TPXO_BOR2_I,
     &                                 C_ID2,NCON2,CCIND2,TEMPS)
     &               / MAX( HMIN_VIT_BC , Z-ZF(NBOR(K)) )
!$$$           ENDIF
          ENDIF
!
        ENDIF
      ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE BORD_TIDE_TPXO

!                   **********************
                    SUBROUTINE CD2MSL_TPXO
!                   **********************
!
     &(NPOIN,X,Y,H,GEOSYST,NUMZONE,T2DL93,LAMBD0,PHI0,T2D_FILES,T2DBB1,
     & CAMPLIF,MSL,I_ORIG,J_ORIG)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    Prepare a level boundary filter to store the TPXO constituents
!+        at the boundary.
!
!note     Passing NPOIN, X, Y and H as arguments allows
!+        this SUBROUTINE to be called from TELEMAC-2D or TELEMAC-3D
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+        Implementation and generalised for interfacing with
!+        TELEMAC-2D AND 3D
!
!history  S.E.BOURBAN (HRW)
!+        26/01/2016
!+        V7P2
!+        Replacement of COMPLEX numbers by their imaginary and real parts
!+        (to be compatible with algorithmic differentiation tools)
!
!warning  (-ZF) should be stored in H as you enter this subroutine
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  CAMPLIF       |-->| COEFFICIENT TO CALIBRATE THE AMPLITUDE OF
!|                |   | HALF TIDAL RANGE
!|  GEOSYST       |-->| TYPE OF GEOGRAPHIC SYSTEM (WGS84 LONG/LAT, UTM OR LAMBERT)
!|  H             |<->| COMES IN AS -ZF, TO WHICH THE TPXO FREE SURFACE
!|                |   | WILL BE ADDED TO PRODUCE WATER DEPTH
!|  I_ORIG        |-->| OFFSET ON X COORDINATES
!|  J_ORIG        |-->| OFFSET ON Y COORDINATES
!|  LAMBD0        |-->| LATITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!|  MSL           |-->| COEFFICIENT TO CALIBRATE SEA LEVEL KEYWORD
!|  NPOIN         |-->| NUMBER OF 2D NODES IN THE MESH
!|  NUMZONE       |-->| NUMBER OF ZONE WHEN PLANE PROJECTION (UTM OR LAMBERT)
!|  PHI0          |-->| LONGITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!|  T2DBB1        |-->| ADDRESS OF DATA BASE 1 IN T2D_FILES
!|  T2DL93        |-->| ADDRESS OF GRID CONVERSION FOR LAMBERT 93
!|                |   | IN T2D_FILES
!|  T2D_FILES     |-->| ARRAY OF FILES
!|  U             |<--| 2D DEPTH-AVERAGED VELOCITY COMPONENT U
!|  X             |-->| COORDINATES X OF THE NODES OF THE MESH
!|  Y             |-->| COORDINATES Y OF THE NODES OF THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,T2DBB1
      INTEGER, INTENT(IN)             :: GEOSYST,NUMZONE,T2DL93
      INTEGER, INTENT(IN)             :: I_ORIG,J_ORIG
      DOUBLE PRECISION, INTENT(IN)    :: LAMBD0,PHI0,CAMPLIF,MSL
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN)
      TYPE(BIEF_FILE), INTENT(IN)     :: T2D_FILES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IC,I,J,K,IPOIN,IERR,NC,N,M
      INTEGER, ALLOCATABLE :: MASKT(:,:)
      DOUBLE PRECISION PI,DTR,RTD
      DOUBLE PRECISION XM,XL,YL,XO,YO,ALPHA,RADIUS,X_ORIG,Y_ORIG
      DOUBLE PRECISION, ALLOCATABLE :: LAT(:),LON(:)
      DOUBLE PRECISION, ALLOCATABLE :: XT(:),YT(:)
      DOUBLE PRECISION PH_LIM(2),TH_LIM(2)
      REAL PH_LIM_R(2),TH_LIM_R(2)
!      COMPLEX, ALLOCATABLE :: ZT(:,:,:)
      REAL, ALLOCATABLE :: ZT_R(:,:,:),ZT_I(:,:,:)
!      COMPLEX(KIND(1.D0)), ALLOCATABLE :: ZCON(:)
      DOUBLE PRECISION, ALLOCATABLE :: ZCON_R(:),ZCON_I(:)
      CHARACTER(LEN=4) C_ID_MOD(TPXO_NCMX)
!
!     N,M: SIZES OF THE GRID SUPPORTING THE TPXO MODEL
!     NC: NUMBER OF CONSTITUENTS AVAILABLE IN THE FILE
!     MASKT MASK TO FILTER VALID AND INVALID (U,V,H) VALUES
!     PH_LIM,TH_LIM: MIN AND MAX RANGES FOR PHASES AND PERIODES
!     ZT,ZCON: PHASES AND PERIODS FOR U, V AND H
!     HERE DEFINED AS COMPLEX
!     C_ID_MOD INDICES OF AVAILABLE CONTITUENTS AMONGST THE ALL POSSIBLE
!
      INTRINSIC ATAN
!
!-----------------------------------------------------------------------
!
      PI = 4.D0*ATAN(1.D0)
      DTR = PI/180.D0
      RTD = 180.D0/PI
!
!-----------------------------------------------------------------------
!
!  SPECIFIC VALUES FOR THE EXAMPLE OF A GEOGRAPHIC SYSTEM DEFINED BY
!  THE USER
!
      XO = 0.D0
      YO = 51.5D0
!  OR E.G.
!     XO = PHI0       ! LONGITUDE
!     YO = LAMBD0     ! LATITUDE
!
!  ANGLE BETWEEN EAST AXIS ---> X AXIS (TRIGONOMETRIC DEGREES)
      ALPHA = 40.D0
      ALPHA = ALPHA*DTR ! IN RADIANS
!  RADIUS: RADIUS OF THE EARTH
      RADIUS = 6371000.D0
!
!-----------------------------------------------------------------------
!
!     PREPARE STORAGE ON LEVEL BOUNDARIES
!
      IF(T2D_FILES(T2DBB1)%NAME(1:1).EQ.' ') THEN
        WRITE(LU,*) 'TPXO FILE FOR H NOT AVAILABLE'
        CALL PLANTE(1)
        STOP
      ENDIF
      WRITE(LU,*) 'MEAN SEA LEVEL BASED ON TPXO:'
!
!-----------------------------------------------------------------------
!
!     READ AVAILABLE DIMENSIONS
!
      REWIND(T2D_FILES(T2DBB1)%LU)
      READ(T2D_FILES(T2DBB1)%LU) N,M,NC,
     &     ( TH_LIM_R(I), I=1, SIZE(TH_LIM_R,1) ),
     &     ( PH_LIM_R(I), I=1, SIZE(PH_LIM_R,1) ),
     &     C_ID_MOD(1:NC)
!
!-----------------------------------------------------------------------
!
!     COPY OF TH_LIM AND PH_LIM IN DOUBLE PRECISION
!
      TH_LIM(1) = TH_LIM_R(1)
      TH_LIM(2) = TH_LIM_R(2)
      PH_LIM(1) = PH_LIM_R(1)
      PH_LIM(2) = PH_LIM_R(2)
!
!-----------------------------------------------------------------------
!
!     GET ALL AVAILABLE CONSTITUENTS AND SET THEIR INDICES
!
      NCON = NC
      DO IC = 1,NC
        C_ID(IC) = C_ID_MOD(IC)
      ENDDO
      ALLOCATE( CCIND(NCON) )
      CALL DEF_CID( NCON,C_ID,CCIND )
!
!-----------------------------------------------------------------------
!
      ALLOCATE( LON(NPOIN) )
      ALLOCATE( LAT(NPOIN) )
!
!  WGS84 NORTHERN OR SOUTHERN UTM, OR MERCATOR FOR TELEMAC
!  WARNING!!! IN TELEMAC DICO, LAMBD0 IS LATITUDE AND PHI0 IS LONGITUDE
!  LAMBD0 AND PHI0 ARE NOT USED FOR GEOSYST = 2 OR 3
      IF(GEOSYST.EQ.2.OR.GEOSYST.EQ.3.OR.GEOSYST.EQ.5) THEN
!       POSSIBLE OFFSET STORED IN THE GEOMETRY FILE
        IF(I_ORIG.NE.0.OR.J_ORIG.NE.0) THEN
          X_ORIG = DBLE(I_ORIG)
          Y_ORIG = DBLE(J_ORIG)
!         X AND Y ARE READ ONLY => OTHER ARRAYS XT AND YT
          ALLOCATE( XT(NPOIN) )
          ALLOCATE( YT(NPOIN) )
          DO K=1,NPOIN
            XT(K) = X(K) + X_ORIG
            YT(K) = Y(K) + Y_ORIG
          ENDDO
          CALL CONV_MERCATOR_TO_DEGDEC(NPOIN,XT(1:NPOIN),YT(1:NPOIN),
     &                                 LON(1:NPOIN),LAT(1:NPOIN),
     &                                 GEOSYST,NUMZONE,PHI0,LAMBD0)
          DEALLOCATE( XT,YT )
        ELSE
          CALL CONV_MERCATOR_TO_DEGDEC(NPOIN,X(1:NPOIN),Y(1:NPOIN),
     &                                 LON(1:NPOIN),LAT(1:NPOIN),
     &                                 GEOSYST,NUMZONE,PHI0,LAMBD0)
        ENDIF
!  NTF LAMBERT
      ELSEIF(GEOSYST.EQ.4) THEN
!       POSSIBLE OFFSET STORED IN THE GEOMETRY FILE
        IF(I_ORIG.NE.0.OR.J_ORIG.NE.0) THEN
          X_ORIG = DBLE(I_ORIG)
          Y_ORIG = DBLE(J_ORIG)
!         X AND Y ARE READ ONLY => OTHER ARRAYS XT AND YT
          ALLOCATE( XT(NPOIN) )
          ALLOCATE( YT(NPOIN) )
          DO K=1,NPOIN
            XT(K) = X(K) + X_ORIG
            YT(K) = Y(K) + Y_ORIG
          ENDDO
          CALL CONV_LAMBERT_TO_DEGDEC(NPOIN,XT(1:NPOIN),YT(1:NPOIN),
     &                                LON(1:NPOIN),LAT(1:NPOIN),
     &                                NUMZONE,T2D_FILES(T2DL93)%LU)
          DEALLOCATE( XT,YT )
        ELSE
          CALL CONV_LAMBERT_TO_DEGDEC(NPOIN,X(1:NPOIN),Y(1:NPOIN),
     &                                LON(1:NPOIN),LAT(1:NPOIN),
     &                                NUMZONE,T2D_FILES(T2DL93)%LU)
        ENDIF
!  WGS84 LONGITUDE/LATITUDE
      ELSEIF(GEOSYST.EQ.1) THEN
        DO K=1,NPOIN
          LON(K) = X(K)
          LAT(K) = Y(K)
        ENDDO
      ELSEIF(GEOSYST.EQ.0) THEN
!  DEFINED BY THE USER
!  THIS IS AN EXAMPLE
        DO K=1,NPOIN
          XL = X(K)
          YL = Y(K)
!  ROTATION WITH ALPHA ANGLE HERE
          XM=XL*COS(ALPHA)-YL*SIN(ALPHA)
          YL=XL*SIN(ALPHA)+YL*COS(ALPHA)
          XL=XM
!  TRANSLATION AND CONVERSION INTO REAL DEGREES
          LON(K) = XO+XL/RADIUS/COS(YO*DTR)*RTD
          LAT(K) = YO+YL/RADIUS            *RTD
        ENDDO
      ELSEIF(GEOSYST.EQ.-1) THEN
!  DEFAULT VALUE
        WRITE(LU,*) 'INCORRECT DEFAULT VALUE FOR THE GEOGRAPHIC'
        WRITE(LU,*) 'SYSTEM. TO BE CHOSEN AMONG THE POSSIBLE CHOICES'
        WRITE(LU,*) 'OR IMPLEMENT THE CONVERSION'
        WRITE(LU,*) 'BY YOURSELF WITH CHOICE 0 IN BORD_TIDAL_BC.F:'
        WRITE(LU,*) '0: DEFINED BY USER,'
        WRITE(LU,*) '1: WGS84 LONGITUDE/LATITUDE IN REAL DEGREES,'
        WRITE(LU,*) '2: WGS84 NORTHERN UTM,'
        WRITE(LU,*) '3: WGS84 SOUTHERN UTM,'
        WRITE(LU,*) '4: LAMBERT,'
        WRITE(LU,*) '5: MERCATOR FOR TELEMAC.'
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) 'GEOGRAPHIC SYSTEM FOR COORDINATES'
        WRITE(LU,*) 'NOT TAKEN INTO ACCOUNT.'
        WRITE(LU,*) 'CHANGE THE SYSTEM OR IMPLEMENT THE CONVERSION'
        WRITE(LU,*) 'BY YOURSELF WITH CHOICE 0 IN BORD_TIDAL_BC.F:'
        WRITE(LU,*) '0: DEFINED BY USER,'
        WRITE(LU,*) '1: WGS84 LONGITUDE/LATITUDE IN REAL DEGREES,'
        WRITE(LU,*) '2: WGS84 NORTHERN UTM,'
        WRITE(LU,*) '3: WGS84 SOUTHERN UTM,'
        WRITE(LU,*) '4: LAMBERT,'
        WRITE(LU,*) '5: MERCATOR FOR TELEMAC.'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO I = 1,NPOIN
        IF( LON(I).GT.PH_LIM(2) ) LON(I) = LON(I) - 360.D0
        IF( LON(I).LT.PH_LIM(1) ) LON(I) = LON(I) + 360.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
!     GET DATA FROM THE H-FILE
!
      WRITE(LU,*) ' - ACQUIRING LEVELS'
!
      ALLOCATE( ZT_R(NCON,N,M), ZT_I(NCON,N,M), MASKT(N,M) )
      DO J=1,M
        DO I=1,N
          MASKT(I,J) = 0
        ENDDO
      ENDDO
!
      DO IC = 1,NCON
        REWIND(T2D_FILES(T2DBB1)%LU)
        READ(T2D_FILES(T2DBB1)%LU)  ! HEADER LINE
        DO K = 1,IC-1
          READ(T2D_FILES(T2DBB1)%LU)
        ENDDO
!       /!\ reading complex numbers as pairs of double /!\
!        READ(T2D_FILES(T2DBB1)%LU) ( ( ZT(IC,I,J), I=1,N ), J=1,M )
        READ(T2D_FILES(T2DBB1)%LU)
     &    ( ( ZT_R(IC,I,J), ZT_I(IC,I,J), I=1,N ), J=1,M )
!        /!\ handle with care /!\
!        WHERE( ZT(IC,:,:).NE.CMPLX(0.D0,0.D0) ) MASKT = 1
        DO J=1,M
          DO I=1,N
            IF( ( ZT_R(IC,I,J).NE.(0.0) ).OR.
     &          ( ZT_I(IC,I,J).NE.(0.0) ) ) MASKT(I,J) = 1
          ENDDO
        ENDDO
      ENDDO
!
!     INTERPOLATE TPXO IN SPACE
!
      WRITE(LU,*) ' - INTERPOLATING LEVELS'
!
      ALLOCATE( ZCON_R(NCON), ZCON_I(NCON) )
      DO IPOIN = 1,NPOIN
        CALL INTERPT( ZT_R,ZT_I,NCON,N,M,MASKT,TH_LIM,PH_LIM,
     &                LAT(IPOIN),LON(IPOIN),ZCON_R,ZCON_I,IERR,'z' )
        IF( IERR.EQ.0 ) THEN
          H(IPOIN) = 0.D0
          DO I=1,NCON
!            H(IPOIN) = H(IPOIN) + ABS(ZCON(I))
            H(IPOIN) = H(IPOIN) + SQRT(ZCON_R(I)**2+ZCON_I(I)**2)
          ENDDO
          H(IPOIN) = CAMPLIF*H(IPOIN)
        ELSE
          H(IPOIN) = MSL
        ENDIF
      ENDDO
      DEALLOCATE( ZCON_R,ZCON_I,ZT_R,ZT_I,MASKT )
      DEALLOCATE( LON,LAT )
!
      WRITE(LU,*) 'END OF BUILDING OF MEAN SEA LEVEL'
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE CD2MSL_TPXO
!                   ***********************
                    SUBROUTINE DEALLOC_TPXO
!                   ***********************
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief DEALLOCATE TPXO ARRAYS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      IF(ALLOCATED(INDW)) DEALLOCATE(INDW)
      IF(ALLOCATED(C_ID2)) DEALLOCATE(C_ID2)
      IF(ALLOCATED(CCIND)) DEALLOCATE(CCIND)
      IF(ALLOCATED(CCIND2)) DEALLOCATE(CCIND2)
      IF(ALLOCATED(TPXO_NFR)) DEALLOCATE(TPXO_NFR)
      IF(ALLOCATED(TPXO_BOR_R)) DEALLOCATE(TPXO_BOR_R)
      IF(ALLOCATED(TPXO_BOR_I)) DEALLOCATE(TPXO_BOR_I)
      IF(ALLOCATED(TPXO_BOR2_R)) DEALLOCATE(TPXO_BOR2_R)
      IF(ALLOCATED(TPXO_BOR2_I)) DEALLOCATE(TPXO_BOR2_I)
      IF(ALLOCATED(BOR2HW_R)) DEALLOCATE(BOR2HW_R)
      IF(ALLOCATED(BOR2HW_I)) DEALLOCATE(BOR2HW_I)
      IF(ALLOCATED(ARGHW)) DEALLOCATE(ARGHW)

      DEJA = .FALSE.
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE DEALLOC_TPXO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      END MODULE TPXO
