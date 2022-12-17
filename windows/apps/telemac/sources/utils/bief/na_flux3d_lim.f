!                   ************************
                    SUBROUTINE NA_FLUX3D_LIM
!                   ************************
!
     &(W,FLULIM,NSEG2D,NELEM,NELMAX,NELEM2,NELMAX2,ELTSEG)
!
!***********************************************************************
! BIEF   V6P2                                   24/08/2012
!***********************************************************************
!
!brief    LIMITS 3D HORIZONTAL FLUXES IN A NON ASSEMBLED ELEMENT BY
!         ELEMENT ARRAY. FOR PRISMS. FIRST THE FLUXES ARE COMPUTED
!         WITH THE FINITE ELEMENT FLUXES. THEN LIMITATION IS DONE,
!         THEN FINITE ELEMENT FLUXES ARE REASSEMBLED.
!
!history  J-M HERVOUET (LNHE)
!+        23/08/2012
!+        V6P2
!+  First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTSEG         |-->| ELEMENTS THAT FORM A TRIANGLE
!| FLULIM         |-->| LIMITING FACTOR OF 2D SEGMENTS
!| NELEM          |-->| NUMBER OF ELEMENTS IN 3D
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NSEG2D         |-->| NUMBER OF SEGMENTS IN 2D
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 3D
!| NELMAX2        |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| W              |-->| FINITE ELEMENT FLUXES (I.E. LEAVING POINTS)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER, INTENT(IN)             :: NSEG2D,NELMAX
      INTEGER, INTENT(IN)             :: NELEM,NELEM2,NELMAX2
      INTEGER, INTENT(IN)             :: ELTSEG(NELMAX2,3)
      DOUBLE PRECISION, INTENT(INOUT) :: W(NELMAX,6)
      DOUBLE PRECISION, INTENT(IN)    :: FLULIM(NSEG2D)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER IELEM,IELEM2
      DOUBLE PRECISION F12,F23,F31,A1,A2,A3,F1,F2,F3
!
!-----------------------------------------------------------------------
!
!     LIMITS 3D HORIZONTAL FLUXES BY COEFFICIENT OF 2D FLUXES
!
!     POINT TO POINT FLUXES ARE COMPUTED
!     SEE FLUX_EF_VF IN BIEF, OPTION 2
!     THEN THEY ARE REGROUPED ON POINTS
!
!-----------------------------------------------------------------------
!
!     HORIZONTAL SEGMENTS
!
      DO IELEM=1,NELEM
!
        IELEM2=MOD(IELEM-1,NELEM2)+1
!
!       LOWER LEVEL (SUM OF 3 W MUST BE 0, IT IS WITH VC04PP)
!
        F1 = W(IELEM,1)
        F2 = W(IELEM,2)
        F3 = W(IELEM,3)
        A1 = ABS(F1)
        A2 = ABS(F2)
        A3 = ABS(F3)
        F12=0.D0
        F23=0.D0
        F31=0.D0
        IF(A1.GE.A2.AND.A1.GE.A3) THEN
!         ALL FLOW TO AND FROM NODE 1
          F12 = - F2
          F31 = + F3
        ELSEIF(A2.GE.A1.AND.A2.GE.A3) THEN
!         ALL FLOW TO AND FROM NODE 2
          F12 = + F1
          F23 = - F3
        ELSE
!         ALL FLOW TO AND FROM NODE 3
          F23 = + F2
          F31 = - F1
        ENDIF
!       LIMITATION
        F12=F12*FLULIM(ELTSEG(IELEM2,1))
        F23=F23*FLULIM(ELTSEG(IELEM2,2))
        F31=F31*FLULIM(ELTSEG(IELEM2,3))
!       REGROUPING
        W(IELEM,1)=F12-F31
        W(IELEM,2)=F23-F12
        W(IELEM,3)=F31-F23
!
!       UPPER LEVEL (SUM OF 3 W MUST BE 0, IT IS WITH VC04PP)
!
        F1 = W(IELEM,4)
        F2 = W(IELEM,5)
        F3 = W(IELEM,6)
        A1 = ABS(F1)
        A2 = ABS(F2)
        A3 = ABS(F3)
        F12=0.D0
        F23=0.D0
        F31=0.D0
        IF(A1.GE.A2.AND.A1.GE.A3) THEN
!         ALL FLOW TO AND FROM NODE 1
          F12 = - F2
          F31 = + F3
        ELSEIF(A2.GE.A1.AND.A2.GE.A3) THEN
!         ALL FLOW TO AND FROM NODE 2
          F12 = + F1
          F23 = - F3
        ELSE
!         ALL FLOW TO AND FROM NODE 3
          F23 = + F2
          F31 = - F1
        ENDIF
!       LIMITATION
        F12=F12*FLULIM(ELTSEG(IELEM2,1))
        F23=F23*FLULIM(ELTSEG(IELEM2,2))
        F31=F31*FLULIM(ELTSEG(IELEM2,3))
!       REGROUPING
        W(IELEM,4)=F12-F31
        W(IELEM,5)=F23-F12
        W(IELEM,6)=F31-F23
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
