!                   ***********************
                    INTEGER FUNCTION IELBOR
!                   ***********************
!
     &( IELM , I )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GIVES THE BOUNDARY ELEMENT TYPE CORRESPONDING TO
!+                A GIVEN ELEMENT TYPE IN THE DOMAIN.
!+
!+            WHEN THERE ARE SEVERAL TYPES (AS IS THE CASE FOR THE
!+                PRISMS FOR EXAMPLE) USES INDEX I TO DISTINGUISH THEM.
!
!history  J-M HERVOUET
!+        06/02/08
!+        V5P9
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
!| I              |-->| CASE OF SEVERAL BOUNDARY ELEMENTS
!| IELM           |-->| TYPE OF ELEMENT IN THE DOMAIN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER IELM,I
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.10.OR.IELM.EQ.20) THEN
        IELBOR = 0
      ELSEIF(IELM.EQ.11.OR.IELM.EQ.12.OR.IELM.EQ.21) THEN
        IELBOR = 1
      ELSEIF(IELM.EQ.13) THEN
        IELBOR = 2
      ELSEIF(IELM.EQ.30) THEN
        IELBOR = 80
      ELSEIF(IELM.EQ.31) THEN
        IELBOR = 81
      ELSEIF(IELM.EQ.51.AND.I.EQ.1) THEN
        IELBOR = 11
      ELSEIF(IELM.EQ.51.AND.I.EQ.2) THEN
        IELBOR = 61
      ELSEIF(IELM.EQ.50.AND.I.EQ.1) THEN
        IELBOR = 10
      ELSEIF(IELM.EQ.50.AND.I.EQ.2) THEN
        IELBOR = 60
      ELSEIF(IELM.EQ.40.AND.I.EQ.1) THEN
        IELBOR = 10
      ELSEIF(IELM.EQ.40.AND.I.EQ.2) THEN
        IELBOR = 70
      ELSEIF(IELM.EQ.41.AND.I.EQ.1) THEN
        IELBOR = 11
      ELSEIF(IELM.EQ.41.AND.I.EQ.2) THEN
        IELBOR = 71
      ELSE
        WRITE(LU,101) IELM
101     FORMAT(1X,'IELBOR (BIEF) : ',1I6,' ELEMENT NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
