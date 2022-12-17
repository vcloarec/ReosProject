!                   ********************
                    SUBROUTINE MIXLENGTH
!                   ********************
!
     &(VISC,MESH,T1,T2,T3,T4)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    COMPUTES THE EDDY VISCOSITY USING THE MIXING LENGTH
!         FOR THE HORIZONTAL + PARABOLIC MODEL FOR THE VERTICAL
!
!history  C. DORFMANN (TU GRAZ)
!+        15/03/2016
!+        V7P2
!+   First version, with negative depths secured and some optimisation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MESH           |-->| MESH STRUCTURE
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| VISC           |-->| TURBULENT DIFFUSION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY : CF,U,V,H,MSK,MASKEL,PROPNU,
     &                      UNSV2D,IELMU,NPTFR,KARMAN,NPOIN,CALMIXLENGTH
      USE INTERFACE_TELEMAC2D, EX_MIXLENGTH => MIXLENGTH
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: VISC,T1,T2,T3,T4
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,N
      DOUBLE PRECISION LM,LMB,USTAR,VISCVERT,VISCHOR2,HC
!
!-----------------------------------------------------------------------
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
!     COEFFICIENTS:
!
!     Horizontal mixing length model:
!     CL: theoretically from integration of lm along depth
!     can be used as calibration coefficient
!     Old value, correponding to CALMIXLENGTH(1)/KARMAN now
!
!     CALMIXLENGTH(1) = CL*KARMAN = 0.2666667D0*KARMAN
!
!     Vertical parabolic model:
!     ALPHA: theoretically from integration of parabolic model along
!     depth can be used as calibration coefficient
!
!     CALMIXLENGTH(2) = ALPHA = KARMAN/6.D0
!
!-----------------------------------------------------------------------
!
      CALL VECTOR(T1,'=','GRADF          X',IELMU,
     &            1.D0,U,U,U,U,U,U,MESH,MSK,MASKEL)
      CALL VECTOR(T2,'=','GRADF          Y',IELMU,
     &            1.D0,U,U,U,U,U,U,MESH,MSK,MASKEL)
      CALL VECTOR(T3,'=','GRADF          X',IELMU,
     &            1.D0,V,V,V,V,V,V,MESH,MSK,MASKEL)
      CALL VECTOR(T4,'=','GRADF          Y',IELMU,
     &            1.D0,V,V,V,V,V,V,MESH,MSK,MASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM (T1, 2, MESH)
        CALL PARCOM (T2, 2, MESH)
        CALL PARCOM (T3, 2, MESH)
        CALL PARCOM (T4, 2, MESH)
      ENDIF
!
      DO I=1,NPOIN
        USTAR = SQRT(0.5D0*CF%R(I)*(U%R(I)**2+V%R(I)**2))
        HC = MAX(0.D0,H%R(I))
        VISCVERT = CALMIXLENGTH(2)*HC*USTAR
        LM = CALMIXLENGTH(1)*HC
        VISCHOR2 = LM**4 * (2.D0*T1%R(I)**2+2.D0*T4%R(I)**2
     &            +(T2%R(I)+T3%R(I))**2) * UNSV2D%R(I)**2
        VISC%R(I) = PROPNU + SQRT(VISCVERT**2+VISCHOR2)
      ENDDO
!
!     LOOP ON THE BOUNDARY NODES
!     reducing eventually the mixing length LMB at the nodes near the wall
!
      DO K=1,NPTFR
        N = MESH%NBOR%I(K)
        USTAR = SQRT(0.5D0*CF%R(N)*(U%R(N)**2+V%R(N)**2))
        HC = MAX(0.D0,H%R(N))
        VISCVERT = CALMIXLENGTH(2)*HC*USTAR
        LMB = MIN(CALMIXLENGTH(1)*HC,KARMAN*MESH%DISBOR%R(K))
        VISCHOR2 = LMB**4 * (2.D0*T1%R(N)**2+2.D0*T4%R(N)**2
     &            +(T2%R(N)+T3%R(N))**2) * UNSV2D%R(N)**2
        VISC%R(N) = PROPNU + SQRT(VISCVERT**2+VISCHOR2)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
