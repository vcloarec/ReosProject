!                   *****************
                    SUBROUTINE FLUSRC
!                   *****************
!
     &(IEL1,IEL2,ISEGIN,VNOIN,W,HDZ1,HDZ2,HDXZ1,HDYZ1,HDXZ2,HDYZ2,EPS)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief    COMPUTES FLUXES DUE TO NON CENTERED SOURCES TERMS.
!
!>@history  N.GOUTAL
!!        19/08/1994
!!        V5P2
!!
!
!>@history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!!        13/07/2010
!!        V6P0
!!   Translation of French comments within the FORTRAN sources into
!!   English comments
!
!>@history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!!        21/08/2010
!!        V6P0
!!   Creation of DOXYGEN tags for automated documentation and
!!   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      EPS      TOLERANCE
!>@param  [in,out]  HDZ1     SOURCE FLUXES
!>@param  [in,out]  HDZ2     SOURCE FLUXES
!>@param  [in,out]  HDXZ1    SOURCE FLUXES
!>@param  [in,out]  HDYZ1    SOURCE FLUXES
!>@param  [in,out]  HDXZ2    SOURCE FLUXES
!>@param  [in,out]  HDYZ2    SOURCE FLUXES
!>@param  [in]      IEL1     FIRST ELEMENT NUMBER
!>@param  [in]      IEL2     SECOND ELEMENT NUMBER
!>@param  [in]      ISEGIN   SEGMENT NUMBER
!>@param  [in]      VNOIN    NORMAL VECTOR TO THE INTERFACE
!!                           (2 FIRST COMPONENTS) AND
!!                           LENGTH OF THE SEGMENT (3RD COMPONENT)
!>@param  [in]      W        CONSERVATIVE VARIABLE OF THE PROBLEM AT TIME TN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY: NPOIN,NSEG,GRAV,ZF,X,Y
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: ISEGIN,IEL1,IEL2
      DOUBLE PRECISION, INTENT(IN)    :: EPS,VNOIN(3,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: HDZ1,HDZ2,HDXZ1,HDYZ1,HDXZ2
      DOUBLE PRECISION, INTENT(INOUT) :: HDYZ2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER INDIC(2)
!
      DOUBLE PRECISION XGI, YGI, XGJ, YGJ, DIJ, A1, A2
      DOUBLE PRECISION HI,UI,VI,HJ,VJ,UJ,XN,YN
      DOUBLE PRECISION CT2,CT,RLAMB0,RLAMBM,ALPHA,CI2
      DOUBLE PRECISION CJ,CJ2,RLAMBJ,RLAMBI,RLAMBP
      DOUBLE PRECISION RI,RJ,UT,VT,CI,UN
      DOUBLE PRECISION T11(3),T21(3),TS11(3),TS21(3)
      DOUBLE PRECISION T12(3),T22(3),TS12(3),TS22(3)
      DOUBLE PRECISION GE(3),FE(3),ZF1,ZF2,PSA1,PSA2,PSA
!
      INTRINSIC MIN,MAX
!
!-----------------------------------------------------------------------
!
!------
! 1. COMPUTES SOURCES TERMS AT THE INTERFACE IEL1 , IEL2
!------
!
!
!
!   --->    SOME INTERMEDIATE CALCULATIONS
!           ------------------------------
!
      HI = W(1,IEL1)
      IF (HI.GT.EPS) THEN
        UI = W(2,IEL1) / HI
        VI = W(3,IEL1) / HI
        INDIC(1) = 0
      ELSE
        UI = 0.D0
        VI = 0.D0
        INDIC(1) = 1
      ENDIF
!
      HJ = W(1,IEL2)
      IF ( HJ.GT.EPS) THEN
        UJ = W(2,IEL2) / HJ
        VJ = W(3,IEL2) / HJ
        INDIC(2) = 0
      ELSE
        UJ = 0.D0
        VJ = 0.D0
        INDIC(2) = 1
      ENDIF
!
      XN = VNOIN (1,ISEGIN)
      YN = VNOIN (2,ISEGIN)
!
!    BOTTOM MODIFICATION: AT REST WITH A DRY ELEMENT
!
      UN = UJ*XN+VJ*YN
      ZF1 = ZF%R(IEL1)
      IF(INDIC(1).EQ.1) THEN
        IF((ZF1+EPS.GT.ZF%R(IEL2)+HJ).AND.(UN.GE.-EPS)) THEN
          ZF1 = ZF%R(IEL2)+HJ-EPS
        ENDIF
      ENDIF
!
      UN = UI*XN+VI*YN
      ZF2 = ZF%R(IEL2)
      IF(INDIC(2).EQ.1) THEN
        IF((ZF2+EPS.GT.ZF%R(IEL1)+HI).AND.(UN.LE.EPS)) THEN
          ZF2 = ZF%R(IEL1)+HI-EPS
        ENDIF
      ENDIF
!
!   --->    COMPUTES THE AVERAGES OF ROE OF U,V,H,C**2 AND C
!           ---------------------------------------------
!
      IF(HI.LE.0.D0) THEN
        HI = 0.D0
        RI = 0.D0
      ELSE
        RI = SQRT ( HI )
      ENDIF
      IF(HJ.LE.0.D0) THEN
        HJ = 0.D0
        RJ = 0.D0
      ELSE
        RJ = SQRT ( HJ )
      ENDIF
!     MAX OF THE TWO FOLLOWING LINES
      UT = ( RI * UI + RJ * UJ ) / MAX(RI+RJ,1.D-8)
      VT = ( RI * VI + RJ * VJ ) / MAX(RI+RJ,1.D-8)
      CT2 = GRAV*(HI+HJ)/2.D0
      CT = SQRT ( CT2 )
!
!   --->  TEST ON THE SIGN OF THE EIGENVALUE LAMB0 =
!           ----------------------------------------------------------
!
      RLAMB0 = UT * XN + VT * YN
!
!     COMPUTES EIGENVALUES MATRICES
!--------------------------------------------
!
      T11(1) = 1.D0
      T11(2) = UT - CT * XN
      T11(3) = VT - CT * YN
      T21(1) = 0.D0
      T21(2) = CT * YN
      T21(3) = -CT * XN
!
      T12(1) = 1.D0
      T12(2) = UT + CT * XN
      T12(3) = VT + CT * YN
      T22(1) = 0.D0
      T22(2) = -CT * YN
      T22(3) = +CT * XN
!
      TS11(1) = (UT * XN + VT * YN) * CT + CT2
      TS21(1) = (2.D0 * VT * XN - 2.D0 * UT * YN) * CT
      TS11(2) = -XN * CT
      TS21(2) = 2.D0 * YN * CT
      TS11(3) = -YN * CT
      TS21(3) = -2.D0 * XN * CT
!
      TS12(1) = -(UT * XN + VT * YN) * CT + CT2
      TS22(1) = -(2.D0 * VT * XN - 2.D0 * UT * YN) * CT
      TS12(2) = +XN * CT
      TS22(2) = -2.D0 * YN * CT
      TS12(3) = +YN * CT
      TS22(3) = +2.D0* XN * CT
!
!----------CALCULS POUR LES TERMES SOURCES--------------------
!
!
      XGI = X(IEL1)
      YGI = Y(IEL1)
      XGJ = X(IEL2)
      YGJ = Y(IEL2)
!
      DIJ = SQRT ((XGJ -XGI)**2 + (YGJ - YGI)**2)
      A1  = VNOIN(3,ISEGIN)*DIJ/2.D0
      A2  = VNOIN(3,ISEGIN)*DIJ/2.D0
!
!  BOTTOM GRADIENTS
!
      GE(1)=0.D0
      GE(2)=GRAV*((HI+HJ)/2.D0)*(ZF2-ZF1)*XN/DIJ
      GE(3)=GRAV*((HI+HJ)/2.D0)*(ZF2-ZF1)*YN/DIJ
!
!  FRICTION TERMS
!
!     CH = 900.D0
!     H = (CT2/GRAV)**(1./3.)
      FE(1)= 0.D0
!     FE(2)= GRAV*UT*SQRT(UT**2 + VT**2)/((CH**2)*H)
!     FE(3)= GRAV*VT*SQRT(UT**2 + VT**2)/((CH**2)*H)
      FE(2) = 0.D0
      FE(3) = 0.D0
!
      GE(1)=GE(1)+FE(1)
      GE(2)=GE(2)+FE(2)
      GE(3)=GE(3)+FE(3)
!
!---------------------------------------------------------------------
      IF(RLAMB0.GE.-.000001D0) THEN
!     ---- EXIT SEGMENT ---------
!
!   --->   SMALL CALCULATIONS
!
        RLAMBM = RLAMB0 - CT
!
!
        ALPHA = UI * XN + VI * YN
!
!TBTB BEGINNING: MODIFICATION OF RLAMBM IF RLAMBM
!
        CI2 = GRAV*HI
        CI = SQRT (CI2)
        CJ2 =  GRAV*HJ
        CJ = SQRT (CJ2)
        RLAMBI = ALPHA - CI
        RLAMBJ = UJ * XN + VJ * YN - CJ
!
        IF ( RLAMBI .LT. 0.D0 .AND. RLAMBJ .GT. 0.D0
     &                                                ) THEN
          RLAMBM = MIN(0.D0,RLAMBM) - ABS(RLAMBI - RLAMBJ) / 4.D0
        ENDIF
!
!------------CALCUL DES TERMES SOURCES ------------------------
!
        HDZ1 = 0.D0
        HDXZ1= 0.D0
        HDYZ1= 0.D0
!
        HDZ2 = 0.D0
        HDXZ2= 0.D0
        HDYZ2= 0.D0
!
!
!
!   --->TEST ON THE SIGN OF LAMBDAM
!       ----------------------------
!
        IF ( RLAMBM . LT . 0.D0 ) THEN
!       - - - - - - - - - - - - - -
!
!----------CALCUL DES TERMES SOURCES --------------------------
!
          PSA = TS11(1)*GE(1)+TS11(2)*GE(2)+TS11(3)*GE(3)
!
          HDZ1 = PSA*T11(1)
          HDXZ1 = PSA*T11(2)
          HDYZ1 = PSA*T11(3)
!
!
          PSA1= TS12(1)*GE(1)+TS12(2)*GE(2)+TS12(3)*GE(3)
          PSA2= TS22(1)*GE(1)+TS22(2)*GE(2)+TS22(3)*GE(3)
!
!
          HDZ2 = (PSA1*T12(1)+PSA2*T22(1))
          HDXZ2 = (PSA1*T12(2)+PSA2*T22(2))
          HDYZ2 = (PSA1*T12(3)+PSA2*T22(3))
!
        ELSE
!           -----
!
!
          HDZ1 = 0.D0
          HDXZ1 = 0.D0
          HDYZ1 = 0.D0
!
          HDZ2 = GE(1)*CT2*2.D0
          HDXZ2 = GE(2)*CT2*2.D0
          HDYZ2 = GE(3)*CT2*2.D0
!
        ENDIF
!          -----
!      TESTEST
      ELSE
!      TESTEST
!
!   --->   SMALL CALCULATIONS
!          --------------
!
        RLAMBP = RLAMB0 + CT
        ALPHA = UI * XN + VI * YN
!
        CI2 = GRAV*HI
        CI = SQRT (CI2)
        CJ2 =  GRAV*HJ
        CJ = SQRT (CJ2)
        RLAMBI = ALPHA - CI
        RLAMBJ = UJ * XN + VJ * YN - CJ
!
        IF(RLAMBI .LT. 0.D0 .AND. RLAMBJ .GT. 0.D0) THEN
          RLAMBP = MAX(0.D0,RLAMBP) + ABS(RLAMBI - RLAMBJ)/4.D0
        ENDIF
!
!-----------COMPUTATION OF SOURCE TERMS --------------------------
!
        HDZ1 = GE(1)*CT2*2.D0
        HDXZ1= GE(2)*CT2*2.D0
        HDYZ1= GE(3)*CT2*2.D0
!
        HDZ2 = 0.D0
        HDXZ2= 0.D0
        HDYZ2= 0.D0
!
!
!   --->    TEST ON THE SIGN OF LAMBDAP
!           ----------------------------
!
        IF ( RLAMBP . GT . 0.D0 ) THEN
!       - - - - - - - - - - - - - -
!
!-----------COMPUTATION OF SOURCE TERMS------------------
!
!
          HDZ1 = 0.D0
          HDXZ1= 0.D0
          HDYZ1= 0.D0
!
          HDZ2 = 0.D0
          HDXZ2= 0.D0
          HDYZ2= 0.D0
          PSA1= TS11(1)*GE(1)+TS11(2)*GE(2)+TS11(3)*GE(3)
          PSA2= TS21(1)*GE(1)+TS21(2)*GE(2)+TS21(3)*GE(3)
!
!
          HDZ1 = (PSA1*T11(1)+PSA2*T21(1))
          HDXZ1= (PSA1*T11(2)+PSA2*T21(2))
          HDYZ1= (PSA1*T11(3)+PSA2*T21(3))
!
          PSA = TS12(1)*GE(1)+TS12(2)*GE(2)+TS12(3)*GE(3)
!
          HDZ2 = PSA*T12(1)
          HDXZ2= PSA*T12(2)
          HDYZ2= PSA*T12(3)
!
        ENDIF
!       -----
!
!       TESTEST
      ENDIF
!       TESTEST
      HDZ1 =HDZ1 *A1/CT2
      HDXZ1=HDXZ1*A1/CT2
      HDYZ1=HDYZ1*A1/CT2
      HDZ2 =HDZ2 *A2/CT2
      HDXZ2=HDXZ2*A2/CT2
      HDYZ2=HDYZ2*A2/CT2
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!
      RETURN
      END
