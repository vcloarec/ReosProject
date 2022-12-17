!                   *********************
                    SUBROUTINE FLUX_EF_VF
!                   *********************
!
     &(FLOW,PHIEL,NSEG,NELEM,NELMAX,ELTSEG,ORISEG,IKLE,INIFLO,IOPT,FN,
     & YAFLULIM,FLULIM,YAFLULIMEBE,FLULIMEBE)
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    Computing fluxes between points within a triangle, with
!+        options.
!
!warning  If YAFLULIM and YAFLULIMEBE are both true, only FLULIMEBE
!+        is taken into account !!!!!!!!!!!
!
!
!history  JMH and Leo Postma
!+        06/05/2009
!+        V5P9
!+   First version
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
!history  J-M HERVOUET & SARA PAVAN (EDF R&D, LNHE)
!+        22/10/2013
!+        V7P0
!+   Correction of PSI scheme. Reduction of fluxes based on same
!+   contribution than classical distributive schemes (i.e. without
!+   integration by part).
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        05/11/2016
!+        V7P3
!+   Adding flux limitation with two options, EBE or per segment
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        09/09/2017
!+        V7P3
!+   Argument NELMAX added for dimensioning arrays, instead of NELEM.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTSEG         |-->| SEGMENTS OF EVERY TRIANGLE
!| FLOW           |<--| FLUXES PER SEGMENTS
!| FN             |-->| OPTIONAL ARGUMENT FOR PSI SCHEME
!| IKLE           |-->| CONNECTIVITY TABLE
!| INIFLO         |-->| IF(YES) FLOW WILL BE INITIALISED AT 0
!| IOPT           |-->| OPTION FOR THE CONSTANT PER ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| ORISEG         |-->| ORIENTATION OF SEGMENTS OF EVERY TRIANGLE
!| PHIEL          |-->| PER ELEMENT, FLUXES LEAVING POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FLUX_EF_VF => FLUX_EF_VF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)                    :: NSEG,IOPT,NELEM,NELMAX
      INTEGER, INTENT(IN)                    :: ELTSEG(NELMAX,3)
      INTEGER, INTENT(IN)                    :: ORISEG(NELMAX,3)
      INTEGER, INTENT(IN)                    :: IKLE(NELMAX,3)
      DOUBLE PRECISION, INTENT(INOUT)        :: FLOW(NSEG)
      DOUBLE PRECISION, INTENT(IN)           :: PHIEL(NELMAX,3)
      LOGICAL, INTENT(IN)                    :: INIFLO
      TYPE(BIEF_OBJ), INTENT(IN), OPTIONAL   :: FN
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: FLULIMEBE(*)
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: FLULIM(*)
      LOGICAL, INTENT(IN), OPTIONAL          :: YAFLULIM,YAFLULIMEBE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,ISEG,I1,I2,I3
      DOUBLE PRECISION A1,A2,A3,K1,K2,K3,THIRD,F12,F23,F31,F1,F2,F3
      DOUBLE PRECISION FN1,FN2,FN3,F21,F32,F13
      DOUBLE PRECISION BETA1FI,BETA2FI,BETA3FI,FI
      LOGICAL TFLULIM,TFLULIMEBE
!
      INTRINSIC ABS,MIN,MAX
!
!-----------------------------------------------------------------------
!
!     CHECKING OPTIONAL PARAMETERS
!
      IF(PRESENT(YAFLULIM)) THEN
        TFLULIM=YAFLULIM
        IF(TFLULIM.AND..NOT.PRESENT(FLULIM)) THEN
          WRITE(LU,*) 'FLUX_EF_VF:'
          WRITE(LU,*) 'VARIABLE FLULIM MISSING'
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        TFLULIM=.FALSE.
      ENDIF
      IF(PRESENT(YAFLULIMEBE)) THEN
        TFLULIMEBE=YAFLULIMEBE
        IF(TFLULIMEBE.AND..NOT.PRESENT(FLULIMEBE)) THEN
          WRITE(LU,*) 'FLUX_EF_VF:'
          WRITE(LU,*) 'VARIABLE FLULIMEBE MISSING'
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        TFLULIMEBE=.FALSE.
      ENDIF
!
      THIRD=1.D0/3.D0
!
!-----------------------------------------------------------------------
!
!     INITIALISES FLOW TO 0.D0
!
      IF(INIFLO) THEN
        DO ISEG = 1,NSEG
          FLOW(ISEG) = 0.D0
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(IOPT.EQ.-1) THEN
!
!-----------------------------------------------------------------------
!
!     FLUXES ALREADY COMPUTED BEFORE CALLING THIS SUBROUTINE
!     THEY ARE JUST ASSEMBLED HERE
!
      IF(TFLULIMEBE) THEN
!       WITH EBE FLUX LIMITATION
        DO IELEM = 1,NELEM
!         SEGMENT 1
          ISEG  = ELTSEG(IELEM,1)
          IF(ORISEG(IELEM,1).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) +
     &            PHIEL(IELEM,1)*FLULIMEBE((1-1)*NELMAX+IELEM)
          ELSE
            FLOW(ISEG) = FLOW(ISEG) -
     &            PHIEL(IELEM,1)*FLULIMEBE((1-1)*NELMAX+IELEM)
          ENDIF
!         SEGMENT 2
          ISEG  = ELTSEG(IELEM,2)
          IF(ORISEG(IELEM,2).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) +
     &            PHIEL(IELEM,2)*FLULIMEBE((2-1)*NELMAX+IELEM)
          ELSE
            FLOW(ISEG) = FLOW(ISEG) -
     &            PHIEL(IELEM,2)*FLULIMEBE((2-1)*NELMAX+IELEM)
          ENDIF
!         SEGMENT 3
          ISEG  = ELTSEG(IELEM,3)
          IF(ORISEG(IELEM,3).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) +
     &            PHIEL(IELEM,3)*FLULIMEBE((3-1)*NELMAX+IELEM)
          ELSE
            FLOW(ISEG) = FLOW(ISEG) -
     &            PHIEL(IELEM,3)*FLULIMEBE((3-1)*NELMAX+IELEM)
          ENDIF
        ENDDO
      ELSE
!       WITHOUT FLUX LIMITATION (OR LATER)
        DO IELEM = 1,NELEM
!         SEGMENT 1
          ISEG  = ELTSEG(IELEM,1)
          IF(ORISEG(IELEM,1).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) + PHIEL(IELEM,1)
          ELSE
            FLOW(ISEG) = FLOW(ISEG) - PHIEL(IELEM,1)
          ENDIF
!         SEGMENT 2
          ISEG  = ELTSEG(IELEM,2)
          IF(ORISEG(IELEM,2).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) + PHIEL(IELEM,2)
          ELSE
            FLOW(ISEG) = FLOW(ISEG) - PHIEL(IELEM,2)
          ENDIF
!         SEGMENT 3
          ISEG  = ELTSEG(IELEM,3)
          IF(ORISEG(IELEM,3).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) + PHIEL(IELEM,3)
          ELSE
            FLOW(ISEG) = FLOW(ISEG) - PHIEL(IELEM,3)
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(IOPT.EQ.0) THEN
!
!-----------------------------------------------------------------------
!
!     WITH NO CONSTANT
!
      IF(TFLULIMEBE) THEN
!       WITH EBE FLUX LIMITATION
        DO IELEM = 1,NELEM
          F1 = PHIEL(IELEM,1)
          F2 = PHIEL(IELEM,2)
          F3 = PHIEL(IELEM,3)
!         SEGMENT 1
          ISEG  = ELTSEG(IELEM,1)
          IF(ORISEG(IELEM,1).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) +
     &           THIRD*(F1-F2)*FLULIMEBE((1-1)*NELMAX + IELEM)
          ELSE
            FLOW(ISEG) = FLOW(ISEG) -
     &           THIRD*(F1-F2)*FLULIMEBE((1-1)*NELMAX + IELEM)
          ENDIF
!         SEGMENT 2
          ISEG  = ELTSEG(IELEM,2)
          IF(ORISEG(IELEM,2).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) +
     &           THIRD*(F2-F3)*FLULIMEBE((2-1)*NELMAX + IELEM)
          ELSE
            FLOW(ISEG) = FLOW(ISEG) -
     &           THIRD*(F2-F3)*FLULIMEBE((2-1)*NELMAX + IELEM)
          ENDIF
!         SEGMENT 3
          ISEG  = ELTSEG(IELEM,3)
          IF(ORISEG(IELEM,3).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) +
     &           THIRD*(F3-F1)*FLULIMEBE((3-1)*NELMAX + IELEM)
          ELSE
            FLOW(ISEG) = FLOW(ISEG) -
     &           THIRD*(F3-F1)*FLULIMEBE((3-1)*NELMAX + IELEM)
          ENDIF
        ENDDO
      ELSE
!       WITHOUT FLUX LIMITATION
        DO IELEM = 1,NELEM
          F1 = PHIEL(IELEM,1)
          F2 = PHIEL(IELEM,2)
          F3 = PHIEL(IELEM,3)
!         SEGMENT 1
          ISEG  = ELTSEG(IELEM,1)
          IF(ORISEG(IELEM,1).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) + THIRD*(F1-F2)
          ELSE
            FLOW(ISEG) = FLOW(ISEG) - THIRD*(F1-F2)
          ENDIF
!         SEGMENT 2
          ISEG  = ELTSEG(IELEM,2)
          IF(ORISEG(IELEM,2).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) + THIRD*(F2-F3)
          ELSE
            FLOW(ISEG) = FLOW(ISEG) - THIRD*(F2-F3)
          ENDIF
!         SEGMENT 3
          ISEG  = ELTSEG(IELEM,3)
          IF(ORISEG(IELEM,3).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) + THIRD*(F3-F1)
          ELSE
            FLOW(ISEG) = FLOW(ISEG) - THIRD*(F3-F1)
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(IOPT.EQ.2) THEN
!
!-----------------------------------------------------------------------
!
!     LEO POSTMA'S METHOD (EQUIVALENT TO FLUXES GIVEN BY N-SCHEME)
!
      IF(TFLULIMEBE) THEN
        DO IELEM = 1,NELEM
          F1 = PHIEL(IELEM,1)
          F2 = PHIEL(IELEM,2)
          F3 = PHIEL(IELEM,3)
          A1 = ABS(F1)
          A2 = ABS(F2)
          A3 = ABS(F3)
          IF(A1.GE.A2.AND.A1.GE.A3) THEN
!           ALL FLOW TO AND FROM NODE 1
            ISEG  = ELTSEG(IELEM,1)
            IF(ORISEG(IELEM,1).EQ.1) THEN
              FLOW(ISEG) = FLOW(ISEG) -
     &             F2*FLULIMEBE((1-1)*NELMAX + IELEM)
            ELSE
              FLOW(ISEG) = FLOW(ISEG) +
     &             F2*FLULIMEBE((1-1)*NELMAX + IELEM)
            ENDIF
            ISEG = ELTSEG(IELEM,3)
            IF(ORISEG(IELEM,3).EQ.2) THEN
              FLOW(ISEG) = FLOW(ISEG) -
     &             F3*FLULIMEBE((3-1)*NELMAX + IELEM)
            ELSE
              FLOW(ISEG) = FLOW(ISEG) +
     &             F3*FLULIMEBE((3-1)*NELMAX + IELEM)
            ENDIF
          ELSEIF(A2.GE.A1.AND.A2.GE.A3) THEN
!           ALL FLOW TO AND FROM NODE 2
            ISEG = ELTSEG(IELEM,1)
            IF(ORISEG(IELEM,1).EQ.2) THEN
              FLOW(ISEG) = FLOW(ISEG) -
     &             F1*FLULIMEBE((1-1)*NELMAX + IELEM)
            ELSE
              FLOW(ISEG) = FLOW(ISEG) +
     &             F1*FLULIMEBE((1-1)*NELMAX + IELEM)
            ENDIF
            ISEG = ELTSEG(IELEM,2)
            IF(ORISEG(IELEM,2).EQ.1) THEN
              FLOW(ISEG) = FLOW(ISEG) -
     &             F3*FLULIMEBE((2-1)*NELMAX + IELEM)
            ELSE
              FLOW(ISEG) = FLOW(ISEG) +
     &             F3*FLULIMEBE((2-1)*NELMAX + IELEM)
            ENDIF
          ELSE
!           ALL FLOW TO AND FROM NODE 3
            ISEG = ELTSEG(IELEM,2)
            IF(ORISEG(IELEM,2).EQ.2) THEN
              FLOW(ISEG) = FLOW(ISEG) -
     &             F2*FLULIMEBE((2-1)*NELMAX + IELEM)
            ELSE
              FLOW(ISEG) = FLOW(ISEG) +
     &             F2*FLULIMEBE((2-1)*NELMAX + IELEM)
            ENDIF
            ISEG = ELTSEG(IELEM,3)
            IF(ORISEG(IELEM,3).EQ.1) THEN
              FLOW(ISEG) = FLOW(ISEG) -
     &             F1*FLULIMEBE((3-1)*NELMAX + IELEM)
            ELSE
              FLOW(ISEG) = FLOW(ISEG) +
     &             F1*FLULIMEBE((3-1)*NELMAX + IELEM)
            ENDIF
          ENDIF
        ENDDO
      ELSE
        DO IELEM = 1,NELEM
          F1 = PHIEL(IELEM,1)
          F2 = PHIEL(IELEM,2)
          F3 = PHIEL(IELEM,3)
          A1 = ABS(F1)
          A2 = ABS(F2)
          A3 = ABS(F3)
          IF(A1.GE.A2.AND.A1.GE.A3) THEN
!           ALL FLOW TO AND FROM NODE 1
            ISEG  = ELTSEG(IELEM,1)
            IF(ORISEG(IELEM,1).EQ.1) THEN
              FLOW(ISEG) = FLOW(ISEG) - F2
            ELSE
              FLOW(ISEG) = FLOW(ISEG) + F2
            ENDIF
            ISEG = ELTSEG(IELEM,3)
            IF(ORISEG(IELEM,3).EQ.2) THEN
              FLOW(ISEG) = FLOW(ISEG) - F3
            ELSE
              FLOW(ISEG) = FLOW(ISEG) + F3
            ENDIF
          ELSEIF(A2.GE.A1.AND.A2.GE.A3) THEN
!           ALL FLOW TO AND FROM NODE 2
            ISEG = ELTSEG(IELEM,1)
            IF(ORISEG(IELEM,1).EQ.2) THEN
              FLOW(ISEG) = FLOW(ISEG) - F1
            ELSE
              FLOW(ISEG) = FLOW(ISEG) + F1
            ENDIF
            ISEG = ELTSEG(IELEM,2)
            IF(ORISEG(IELEM,2).EQ.1) THEN
              FLOW(ISEG) = FLOW(ISEG) - F3
            ELSE
              FLOW(ISEG) = FLOW(ISEG) + F3
            ENDIF
          ELSE
!           ALL FLOW TO AND FROM NODE 3
            ISEG = ELTSEG(IELEM,2)
            IF(ORISEG(IELEM,2).EQ.2) THEN
              FLOW(ISEG) = FLOW(ISEG) - F2
            ELSE
              FLOW(ISEG) = FLOW(ISEG) + F2
            ENDIF
            ISEG = ELTSEG(IELEM,3)
            IF(ORISEG(IELEM,3).EQ.1) THEN
              FLOW(ISEG) = FLOW(ISEG) - F1
            ELSE
              FLOW(ISEG) = FLOW(ISEG) + F1
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(IOPT.EQ.3.AND.PRESENT(FN)) THEN
!
!-----------------------------------------------------------------------
!
!     PSI-SCHEME
!
      IF(TFLULIMEBE) THEN
        DO IELEM = 1,NELEM
!
          K1 = -PHIEL(IELEM,1)
          K2 = -PHIEL(IELEM,2)
          K3 = -PHIEL(IELEM,3)
!
!         STARTS WITH N-SCHEME (EQUIVALENT TO LEO POSTMA'S IMPLEMENTATION)
!         FIJ HERE LIKE LAMBDA(I,J) IN BOOK, SO FLUXES FROM J TO I.
!
          F12=MAX(MIN(K1,-K2),0.D0)
          F23=MAX(MIN(K2,-K3),0.D0)
          F31=MAX(MIN(K3,-K1),0.D0)
          F21=MAX(MIN(K2,-K1),0.D0)
          F32=MAX(MIN(K3,-K2),0.D0)
          F13=MAX(MIN(K1,-K3),0.D0)
!
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          FN1=FN%R(I1)
          FN2=FN%R(I2)
          FN3=FN%R(I3)
!
          BETA1FI=F12*(FN1-FN2)+F13*(FN1-FN3)
          BETA2FI=F21*(FN2-FN1)+F23*(FN2-FN3)
          BETA3FI=F31*(FN3-FN1)+F32*(FN3-FN2)
!
          FI=BETA1FI+BETA2FI+BETA3FI
!
!         NOW PSI-SCHEME
!
!         WHAT FOLLOWS IS INSPIRED FROM SUBROUTINE VC08AA
!         WHERE FIJ IS LIJ
!
          IF(FI.GT.0.D0) THEN
            IF(BETA1FI.GT.FI) THEN
              F12=F12*FI/BETA1FI
              F13=F13*FI/BETA1FI
            ELSEIF(BETA1FI.LT.0.D0) THEN
              F12=0.D0
              F13=0.D0
            ENDIF
            IF(BETA2FI.GT.FI) THEN
              F21=F21*FI/BETA2FI
              F23=F23*FI/BETA2FI
            ELSEIF(BETA2FI.LT.0.D0) THEN
              F21=0.D0
              F23=0.D0
            ENDIF
            IF(BETA3FI.GT.FI) THEN
              F31=F31*FI/BETA3FI
              F32=F32*FI/BETA3FI
            ELSEIF(BETA3FI.LT.0.D0) THEN
              F31=0.D0
              F32=0.D0
            ENDIF
          ELSEIF(FI.LT.0.D0) THEN
            IF(BETA1FI.LT.FI) THEN
              F12=F12*FI/BETA1FI
              F13=F13*FI/BETA1FI
            ELSEIF(BETA1FI.GT.0.D0) THEN
              F12=0.D0
              F13=0.D0
            ENDIF
            IF(BETA2FI.LT.FI) THEN
              F21=F21*FI/BETA2FI
              F23=F23*FI/BETA2FI
            ELSEIF(BETA2FI.GT.0.D0) THEN
              F21=0.D0
              F23=0.D0
            ENDIF
            IF(BETA3FI.LT.FI) THEN
              F31=F31*FI/BETA3FI
              F32=F32*FI/BETA3FI
            ELSEIF(BETA3FI.GT.0.D0) THEN
              F31=0.D0
              F32=0.D0
            ENDIF
          ELSE
            F12=0.D0
            F23=0.D0
            F31=0.D0
            F21=0.D0
            F32=0.D0
            F13=0.D0
          ENDIF
!
!         ASSEMBLES FLUXES
!         A DIFFERENCE WITH STANDARD DISTRIBUTIVE SCHEMES
!
!         HERE BEWARE CONVENTION ON FLOW
!
!         SEGMENT 1
          ISEG  = ELTSEG(IELEM,1)
          IF(ORISEG(IELEM,1).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) +
     &           (+ F21 - F12)*FLULIMEBE((1-1)*NELMAX + IELEM)
          ELSE
            FLOW(ISEG) = FLOW(ISEG) +
     &           (- F21 + F12)*FLULIMEBE((1-1)*NELMAX + IELEM)
          ENDIF
!         SEGMENT 2
          ISEG  = ELTSEG(IELEM,2)
          IF(ORISEG(IELEM,2).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) +
     &           (+ F32 - F23)*FLULIMEBE((2-1)*NELMAX + IELEM)
          ELSE
            FLOW(ISEG) = FLOW(ISEG) +
     &           (- F32 + F23)*FLULIMEBE((2-1)*NELMAX + IELEM)
          ENDIF
!         SEGMENT 3
          ISEG  = ELTSEG(IELEM,3)
          IF(ORISEG(IELEM,3).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) +
     &           (+ F13 - F31)*FLULIMEBE((3-1)*NELMAX + IELEM)
          ELSE
            FLOW(ISEG) = FLOW(ISEG) +
     &           (- F13 + F31)*FLULIMEBE((3-1)*NELMAX + IELEM)
          ENDIF
        ENDDO
      ELSE
        DO IELEM = 1,NELEM
!
          K1 = -PHIEL(IELEM,1)
          K2 = -PHIEL(IELEM,2)
          K3 = -PHIEL(IELEM,3)
!
!         STARTS WITH N-SCHEME (EQUIVALENT TO LEO POSTMA'S IMPLEMENTATION)
!         FIJ HERE LIKE LAMBDA(I,J) IN BOOK, SO FLUXES FROM J TO I.
!
          F12=MAX(MIN(K1,-K2),0.D0)
          F23=MAX(MIN(K2,-K3),0.D0)
          F31=MAX(MIN(K3,-K1),0.D0)
          F21=MAX(MIN(K2,-K1),0.D0)
          F32=MAX(MIN(K3,-K2),0.D0)
          F13=MAX(MIN(K1,-K3),0.D0)
!
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          FN1=FN%R(I1)
          FN2=FN%R(I2)
          FN3=FN%R(I3)
!
          BETA1FI=F12*(FN1-FN2)+F13*(FN1-FN3)
          BETA2FI=F21*(FN2-FN1)+F23*(FN2-FN3)
          BETA3FI=F31*(FN3-FN1)+F32*(FN3-FN2)
!
          FI=BETA1FI+BETA2FI+BETA3FI
!
!         NOW PSI-SCHEME
!
!         WHAT FOLLOWS IS INSPIRED FROM SUBROUTINE VC08AA
!         WHERE FIJ IS LIJ
!
          IF(FI.GT.0.D0) THEN
            IF(BETA1FI.GT.FI) THEN
              F12=F12*FI/BETA1FI
              F13=F13*FI/BETA1FI
            ELSEIF(BETA1FI.LT.0.D0) THEN
              F12=0.D0
              F13=0.D0
            ENDIF
            IF(BETA2FI.GT.FI) THEN
              F21=F21*FI/BETA2FI
              F23=F23*FI/BETA2FI
            ELSEIF(BETA2FI.LT.0.D0) THEN
              F21=0.D0
              F23=0.D0
            ENDIF
            IF(BETA3FI.GT.FI) THEN
              F31=F31*FI/BETA3FI
              F32=F32*FI/BETA3FI
            ELSEIF(BETA3FI.LT.0.D0) THEN
              F31=0.D0
              F32=0.D0
            ENDIF
          ELSEIF(FI.LT.0.D0) THEN
            IF(BETA1FI.LT.FI) THEN
              F12=F12*FI/BETA1FI
              F13=F13*FI/BETA1FI
            ELSEIF(BETA1FI.GT.0.D0) THEN
              F12=0.D0
              F13=0.D0
            ENDIF
            IF(BETA2FI.LT.FI) THEN
              F21=F21*FI/BETA2FI
              F23=F23*FI/BETA2FI
            ELSEIF(BETA2FI.GT.0.D0) THEN
              F21=0.D0
              F23=0.D0
            ENDIF
            IF(BETA3FI.LT.FI) THEN
              F31=F31*FI/BETA3FI
              F32=F32*FI/BETA3FI
            ELSEIF(BETA3FI.GT.0.D0) THEN
              F31=0.D0
              F32=0.D0
            ENDIF
          ELSE
            F12=0.D0
            F23=0.D0
            F31=0.D0
            F21=0.D0
            F32=0.D0
            F13=0.D0
          ENDIF
!
!         ASSEMBLES FLUXES
!         A DIFFERENCE WITH STANDARD DISTRIBUTIVE SCHEMES
!
!         HERE BEWARE CONVENTION ON FLOW
!
!         SEGMENT 1
          ISEG  = ELTSEG(IELEM,1)
          IF(ORISEG(IELEM,1).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) + F21 - F12
          ELSE
            FLOW(ISEG) = FLOW(ISEG) - F21 + F12
          ENDIF
!         SEGMENT 2
          ISEG  = ELTSEG(IELEM,2)
          IF(ORISEG(IELEM,2).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) + F32 - F23
          ELSE
            FLOW(ISEG) = FLOW(ISEG) - F32 + F23
          ENDIF
!         SEGMENT 3
          ISEG  = ELTSEG(IELEM,3)
          IF(ORISEG(IELEM,3).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) + F13 - F31
          ELSE
            FLOW(ISEG) = FLOW(ISEG) - F13 + F31
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,*) 'FLUX_EF_VF:'
        WRITE(LU,*) 'UNKNOWN OPTION: ',IOPT
!       IF(IOPT.EQ.3.AND..NOT.PRESENT(FN)) THEN
        IF(IOPT.EQ.3) THEN
          WRITE(LU,*) 'OPTION 3: ADVECTED FUNCTION REQUIRED'
        ENDIF
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     OPTIONAL FLUX LIMITATION BY SEGMENT
!
      IF(TFLULIM.AND..NOT.TFLULIMEBE) THEN
        DO ISEG=1,NSEG
          FLOW(ISEG) = FLOW(ISEG)*FLULIM(ISEG)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

