!                   ********************
                    SUBROUTINE SPALALLIN
!                   ********************
     &(LIMSA, LIUBOR, NPTFR, KENT, KENTU, KSORT,
     & KADH, KLOG, KINC, KNEU, KDIR)
!
!***********************************************************************
! TELEMAC2D   V7P0                                  31/08/2015
!***********************************************************************
!
!brief    INITIALISES THE BOUNDARY CONDITIONS FOR THE DIFFUSION STEP
!+                - SOURCE TERMS OF THE SPALART-ALLAMRAS MODEL.
!
!history  A BOURGOIN (LNHE)
!+        31/08/2015
!+        V7p0
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| KADH           |-->| CONVENTION FOR NO SLIP BOUNDARY CONDITION
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| KINC           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KNEU           |-->| CONVENTION FOR NEUMANN CONDITION
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| LIMKSA         |-->| BOUNDARY CONDITIONS FOR VISCSA
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTSAKSAIN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TELEMAC2D, EX_SPALALLIN => SPALALLIN
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR, KENT, KSORT, KADH, KLOG
      INTEGER, INTENT(IN) :: KINC, KNEU, KDIR, KENTU
      INTEGER, INTENT(INOUT) :: LIMSA(NPTFR)
      INTEGER, INTENT(IN) :: LIUBOR(NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: K
!
!----------------------------------------------------------------------
!
!  BUILDS THE ARRAY LIMSA
!

      DO K=1, NPTFR

        IF(LIUBOR(K).EQ.KENT) THEN
          LIMSA(K)=KDIR
        ELSEIF(LIUBOR(K).EQ.KENTU) THEN
          LIMSA(K)=KDIR
        ELSEIF(LIUBOR(K).EQ.KADH) THEN
          LIMSA(K)=KDIR
        ELSEIF(LIUBOR(K).EQ.KSORT) THEN
          LIMSA(K)=KNEU
        ELSEIF(LIUBOR(K).EQ.KINC) THEN
          LIMSA(K)=KNEU
        ELSEIF(LIUBOR(K).EQ.KLOG) THEN
          LIMSA(K)=KDIR
        ELSE
          WRITE(LU, 101) K, LIUBOR(K)
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
101   FORMAT(1X, 'SPALALLIN: K=', 1I6, 'LIUBOR=', 1I6, 'UNKNOWN CASE')
!
      RETURN
      END SUBROUTINE
