!                   *****************
                    SUBROUTINE KEPSIN
!                   *****************
!
     &(LIMKEP,LIUBOR,NPTFR,KENT,KENTU,KSORT,KADH,KLOG,KINC,KNEU,KDIR)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE BOUNDARY CONDITIONS FOR THE DIFFUSION STEP
!+                - SOURCE TERMS OF THE K-EPSILON MODEL.
!
!warning  LIMKEP IS BUILT FROM LIUBOR (LIKBOR AND LIEBOR DO NOT EXIST)
!
!history  J-M HERVOUET (LNHE)
!+        17/08/1994
!+        V5P2
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
!| KADH           |-->| CONVENTION FOR NO SLIP BOUNDARY CONDITION
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| KINC           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KNEU           |-->| CONVENTION FOR NEUMANN CONDITION
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| LIMKEP         |-->| BOUNDARY CONDITIONS FOR K AND EPSILON
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NPTFR,KENT,KSORT,KADH,KLOG
      INTEGER, INTENT(IN)    :: KINC,KNEU,KDIR,KENTU
      INTEGER, INTENT(INOUT) :: LIMKEP(NPTFR,2)
      INTEGER, INTENT(IN)    :: LIUBOR(NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K
!
!-----------------------------------------------------------------------
!
!  BUILDS THE ARRAY LIMKEP
!
      DO K=1,NPTFR
!
        IF(LIUBOR(K).EQ.KENT) THEN
!
          LIMKEP(K,1) = KDIR
          LIMKEP(K,2) = KDIR
!
        ELSEIF(LIUBOR(K).EQ.KENTU) THEN
!
          LIMKEP(K,1) = KDIR
          LIMKEP(K,2) = KDIR
!
        ELSEIF(LIUBOR(K).EQ.KADH) THEN
!
          LIMKEP(K,1) = KDIR
          LIMKEP(K,2) = KDIR
!
        ELSEIF(LIUBOR(K).EQ.KSORT) THEN
!
          LIMKEP(K,1) = KNEU
          LIMKEP(K,2) = KNEU
!
        ELSEIF(LIUBOR(K).EQ.KINC) THEN
!
          LIMKEP(K,1) = KNEU
          LIMKEP(K,2) = KNEU
!
        ELSEIF(LIUBOR(K).EQ.KLOG ) THEN
!
          LIMKEP(K,1) = KDIR
          LIMKEP(K,2) = KDIR
!
        ELSE
!
          WRITE(LU,101) K,LIUBOR(K)
101       FORMAT(1X,'KEPSIN: K=',1I6,' LIUBOR=',1I6,' UNKNOWN CASE')
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
      ENDDO ! K
!
!-----------------------------------------------------------------------
!
      RETURN
      END
