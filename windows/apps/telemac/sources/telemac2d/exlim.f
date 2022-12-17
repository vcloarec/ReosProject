!                   *******************************
                    DOUBLE PRECISION FUNCTION EXLIM
!                   *******************************
!
     &(ILIM,BETA,GRI,GRIJ)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    EXTRAPOLATES THE GRADIENT AND USES OF A SLOPE LIMITER.
!
!history  INRIA
!+
!+        V5P4
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETA           |-->| EXTRAPOLATION COEFFICIENT FOR ORDRE 2
!| GRI,GRIJ       |-->| GRADIENTS
!| ILIM           |-->| OPTIONS FOR THE LIMITER
!|                |   | 1 : MINMOD
!|                |   | 2 : VAN ALBADA
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: ILIM
      DOUBLE PRECISION, INTENT(IN) :: GRI,GRIJ,BETA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION GRI1,GRI2,GRIJ2,AUX1,AUX2,THETA
      DOUBLE PRECISION, PARAMETER :: E2 = 1.D-12
!
!-----------------------------------------------------------------------
!
!     EXTRAPOLATES THE GRADIENT AND USES SLOPE LIMITER
      GRI1 = (1.D0+BETA)*GRI - BETA*GRIJ
!
!     =======================
!     MINMOD
!     =======================
      IF(ILIM.EQ.1) THEN
        EXLIM=0.5D0*(SIGN(1.D0,GRI1)+SIGN(1.D0,GRIJ))
     &   *MIN(ABS(GRI1),ABS(GRIJ))
!
!     =======================
!     VAN ALBADA
!     =======================
      ELSEIF (ILIM.EQ.2) THEN
        AUX1 = 0.5D0*(1.D0+SIGN(1.D0,GRI1*GRIJ))
        GRI2  = GRI1*GRI1  + E2
        GRIJ2 = GRIJ*GRIJ  + E2
        EXLIM  = AUX1*(GRI2*GRIJ+GRIJ2*GRI)/(GRI2+GRIJ2)
!
!     =======================
!     MONOTONIZED CENTRAL
!     =======================
      ELSEIF (ILIM.EQ.3) THEN
        AUX1 = SIGN(1.D0, GRIJ)
        AUX2 = DMIN1(2.D0*GRI1*AUX1,
     &         DMIN1(0.5D0*(ABS(GRIJ)+GRI1*AUX1), 2.D0*ABS(GRIJ)))
        EXLIM = AUX1*DMAX1(0.D0, AUX2)
!
!     =======================
!     GENERALIZED MINMOD
!     =======================
      ELSEIF (ILIM.EQ.4) THEN
        THETA = 2.D0 ! BETWEEN 1 AND 2
        AUX1 = SIGN(1.D0, GRIJ)
        AUX2 = DMIN1(THETA*GRI1*AUX1,
     &         DMIN1(0.5D0*(ABS(GRIJ)+GRI1*AUX1), THETA*ABS(GRIJ)))
        EXLIM = AUX1*DMAX1(0.D0, AUX2)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
