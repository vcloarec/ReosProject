!                     ************
                      MODULE OKADA
!                     ************
!
!***********************************************************************
! TELEMAC2D   V6P2                                   06/12/2011
!***********************************************************************
!
!brief    Module containing subroutines used for the OKADA model
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      PUBLIC  :: CONDI_OKADA
      PRIVATE :: OKADA_DIP_SLIP,OKADA_STRIKE_SLIP
!
      CONTAINS
!
!-----------------------------------------------------------------------
!
!                   **********************
                    SUBROUTINE CONDI_OKADA
!                   **********************
!
     &(NPOIN,X,Y,H,COEFS,LAMBD0,PHI0)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   06/12/2011
!***********************************************************************
!
!brief    Computes the free surface displacement according to Okada,
!+        assuming it is similar to that of the seabed.
!+        The user inputs are the prinicpal cheracteristics of the
!+        tsunami and a polygon defining the area where the free surface
!+        will be modified.
!+        The prinicpal cheracteristics of the tsunami are:
!+        - the focal depth (HH),
!+        - the fault lenght (L),
!+        - the fault width (W)
!+        - the dislocation (D),
!+        - the strike direction (TH),
!+        - the dip angle (DL),
!+        - the slip (RD),
!+        - the epicentre latitude (Y0) and
!+        - the epicentre longitude (X0)
!
!note     Passing NPOIN, X,Y and H as arguments allows this SUBROUTINE
!+        to be called from TELEMAC-2D or TELEMAC-3D
!
!history  M.S.TURNBULL (HRW), S.E.BOURBAN (HRW)
!+        21/10/2011
!+        V6P2
!+        Implementation to study Tsunami development and propagation
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+        Generalised for interfacing with TELEMAC-2D AND 3D
!
!reference Okada Y., "Internal deformation due to shear and tensile
!+        faults in a half-space",
!+        Bulletin of the Seismological Society of America,
!+        Vol. 82, No. 2, pp. 1018-1040, April 1992
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  NPOIN         |-->| NUMBER OF 2D NODES IN THE MESH
!|  X,Y           |-->| COORDINATES X AND Y OF THE NODES OF THE MESH
!|  H             |<->| WATER DEPTH TO WHICH THE TSUNAMI DEFORMATION
!|                |   | WILL BE ADDED
!|  COEFS         |   | TSUNAMI DEFINITION, IN ORDER:
!|                |   | - HH FOCAL DEPTH
!|                |   | - L  FAULT LENGTH
!|                |   | - W  FAULT WIDTH
!|                |   | - D  DISLOCATION
!|                |   | - TH STRIKE DIRECTION
!|                |   | - DL DIP ANGLE
!|                |   | - RD SLIP ANGLE
!|                |   | - Y0 EPICENTRE LATITUDE
!|                |   | - X0 EPICENTRE LONGITUDE
!|                |   | - C0 SIZE OF THE ELLIPSE OF INFLUENCE (LxW),
!|                |   |   WITHIN WHICH THE POINTS WILL BE SET TO THE
!|                |   |   MODEL DISPLACEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: COEFS(10),LAMBD0,PHI0
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      DOUBLE PRECISION HH, L, W, D, TH, DL, RD, Y0, X0, C0
!
      DOUBLE PRECISION ZERO,DEG2RAD, RR
      DOUBLE PRECISION HALFL, MINOR,MAJOR, XSPH0,YSPH0
      DOUBLE PRECISION CS2,SN2,DIST, XSPH,YSPH
      DOUBLE PRECISION DEL_X, DEL_Y, DS, DD, SN, CS
      DOUBLE PRECISION XI, YJ, YY, XX, X1, X2, P
      DOUBLE PRECISION F1, F2, F3, F4, G1, G2, G3, G4
      DOUBLE PRECISION US, UD
      DOUBLE PRECISION PI, LAT0, LONG0, CONST,CONST0
!
      INTRINSIC ATAN, COS, SIN, TAN, EXP, LOG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     USER AND CONVERTION CONSTANTS
!
      ZERO = 0.D0
      RR = 6.371D6
!
      PI = 4.D0*ATAN(1.D0)
      DEG2RAD = PI/180.D0
!
!     FOLLOWING PARAMETERS SHOULD BE ADDED TO DICO
      HH = COEFS(1) ! FOCAL DEPTH
      L  = COEFS(2) ! FAULT LENGTH
      W  = COEFS(3) ! FAULT WIDTH
      D  = COEFS(4) ! DISLOCATION
      TH = COEFS(5) * DEG2RAD ! STRIKE DIRECTION
      DL = COEFS(6) * DEG2RAD ! DIP ANGLE
      RD = COEFS(7) * DEG2RAD ! SLIP ANGLE
      Y0 = COEFS(8) * DEG2RAD ! EPICENTRE LATITUDE
      X0 = COEFS(9) * DEG2RAD ! EPICENTRE LONGITUDE
      C0 = COEFS(10) ! SIZE OF THE ELLIPSE
!
      LAT0 = LAMBD0 * DEG2RAD
      LONG0 = PHI0 * DEG2RAD
!
      CONST = TAN( 0.5D0*LAT0 + 0.25D0*PI )
      CONST0 = TAN( 0.5D0*Y0 + 0.25D0*PI )
      HALFL = 0.5D0*L
!
!     PARAMETERS FOR THE ELLIPSE OF INFLUENCE
      MAJOR = C0 * 2*L
      MINOR = C0 * 2*W
      CS2 = COS( 0.5D0*PI-TH )
      SN2 = SIN( 0.5D0*PI-TH )
!
      XSPH0 = RR*( X0-LONG0 )
      YSPH0 = RR*( LOG( CONST0 ) - LOG( CONST ) )
!
!-----------------------------------------------------------------------
!
! FOCAL DEPTH USED FOR OKADA'S MODEL
!
      HH = HH+0.5D0*W*SIN(DL)
!
! DISPLACEMENT DUE TO DIFFERENT EPICENTER DEFINITION
!
      DEL_X = W*COS(DL)*COS(TH)
      DEL_Y = W*COS(DL)*SIN(TH)
!
      DS = D*COS(RD)
      DD = D*SIN(RD)
      SN = SIN(DL)
      CS = COS(DL)
!
      DO I = 1,NPOIN
! IF POINT INSIDE ELLIPSE C0 x ( L x W )
        XSPH =  CS2*( X(I)-XSPH0 ) + SN2*( Y(I)-YSPH0 )
        YSPH = -SN2*( X(I)-XSPH0 ) + CS2*( Y(I)-YSPH0 )
        DIST = (XSPH/MAJOR)**2 + (YSPH/MINOR)**2
        IF( DIST.LT.1.D0 ) THEN
!
          XI = X(I)/RR + LONG0
          YJ = 2.D0*ATAN(CONST*EXP(Y(I)/RR)) - 0.5D0*PI
          YY = RR*(YJ-Y0)
          XX = RR*COS(YJ)*(XI-X0)
          X1 = (XX-DEL_X)*SIN(TH)+(YY+DEL_Y)*COS(TH)
          X2 = (XX-DEL_X)*COS(TH)-(YY+DEL_Y)*SIN(TH)
          X2 = -X2
          P = X2*CS+HH*SN
!
          F1 = OKADA_STRIKE_SLIP(X2,X1+HALFL,P,DL,HH )
          F2 = OKADA_STRIKE_SLIP(X2,X1+HALFL,P-W,DL,HH )
          F3 = OKADA_STRIKE_SLIP(X2,X1-HALFL,P,DL,HH )
          F4 = OKADA_STRIKE_SLIP(X2,X1-HALFL,P-W,DL,HH )
          G1 = OKADA_DIP_SLIP(X2,X1+HALFL,P,DL,HH )
          G2 = OKADA_DIP_SLIP(X2,X1+HALFL,P-W,DL,HH )
          G3 = OKADA_DIP_SLIP(X2,X1-HALFL,P,DL,HH )
          G4 = OKADA_DIP_SLIP(X2,X1-HALFL,P-W,DL,HH )
!
          US = (F1-F2-F3+F4)*DS
          UD = (G1-G2-G3+G4)*DD
          H(I) = H(I) + (US+UD)
!
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE CONDI_OKADA
!             *******************************************
              DOUBLE PRECISION FUNCTION OKADA_STRIKE_SLIP
!             *******************************************
!
     &(X2,Y1,Y2,DP,DD)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   06/12/2011
!***********************************************************************
!
!brief    Part of the Okada model. Calculates the strike slip.
!
!history  M.S.TURNBULL (HRW), S.E.BOURBAN (HRW)
!+        21/10/2011
!+        V6P2
!+        Implementation to study Tsunami development and propagation
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+        Generalised for interfacing with TELEMAC-2D AND 3D
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| Y1,Y2          |-->|
!| DP             |-->|
!| DD             |-->|
!| F              |<--|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: X2,Y1,Y2,DP,DD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION SN,CS,Q,D_BAR,R,A4,PI
!
      INTRINSIC ATAN,COS,SIN,LOG,SQRT
!
!-----------------------------------------------------------------------
!
      PI = 4.D0*ATAN(1.D0)
!
      SN = SIN(DP)
      CS = COS(DP)
      Q = X2*SN - DD*CS
      D_BAR = Y2*SN - Q*CS
      R  = SQRT(Y1**2 + Y2**2 + Q**2)
      A4 = 0.5D0/CS*(LOG(R+D_BAR) - SN*LOG(R+Y2))
      OKADA_STRIKE_SLIP=-(D_BAR*Q/R/(R+Y2)+Q*SN/(R+Y2)+A4*SN)/(2.D0*PI)
!
!-----------------------------------------------------------------------
!
      END FUNCTION OKADA_STRIKE_SLIP
!             ****************************************
              DOUBLE PRECISION FUNCTION OKADA_DIP_SLIP
!             ****************************************
!
     &(X2,Y1,Y2,DP,DD)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   06/12/2011
!***********************************************************************
!
!brief    Part of the Okada model. Calculates the strike slip.
!
!history  M.S.TURNBULL (HRW), S.E.BOURBAN (HRW)
!+        21/10/2011
!+        V6P2
!+        Implementation to study Tsunami development and propagation
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+        Generalised for interfacing with TELEMAC-2D AND 3D
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| X2             |-->|
!| Y1,Y2          |-->|
!| DP             |-->|
!| DD             |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: X2,Y1,Y2,DP,DD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION SN,CS,Q,D_BAR,R,XX,A5,PI
!
      INTRINSIC ATAN,COS,SIN,SQRT
!
!-----------------------------------------------------------------------
!
      PI = 4.D0*ATAN(1.D0)
!
      SN = SIN(DP)
      CS = COS(DP)
      Q = X2*SN - DD*CS
      D_BAR = Y2*SN - Q*CS
      R = SQRT(Y1**2 + Y2**2 + Q**2)
      XX = SQRT(Y1**2 + Q**2)
      A5 = 1.D0/CS*ATAN((Y2*(XX+Q*CS)+XX*(R+XX)*SN)/Y1/(R+XX)/CS)
      OKADA_DIP_SLIP = -(D_BAR*Q/R/(R+Y1) +
     &   SN*ATAN(Y1*Y2/Q/R) - A5*SN*CS)/(2.D0*PI)
!
!-----------------------------------------------------------------------
!
      END FUNCTION OKADA_DIP_SLIP
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      END MODULE OKADA
