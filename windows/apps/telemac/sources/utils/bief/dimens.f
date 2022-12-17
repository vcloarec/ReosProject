!                   ***********************
                    INTEGER FUNCTION DIMENS
!                   ***********************
!
     &(IELM)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GIVES THE DIMENSION OF AN ELEMENT.
!
!note     COULD ALSO CREATE A DATA ARRAY TO SPEED UP.
!
!history  J-M HERVOUET (LNHE)
!+        13/02/2008
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
!| IELM           |-->| TYPE OF ELEMENT.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IELM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(IELM.EQ.0 .OR.
     &   IELM.EQ.1 .OR.
     &   IELM.EQ.2) THEN
!
        DIMENS = 1
!
      ELSEIF(IELM.EQ.10.OR.
     &       IELM.EQ.11.OR.
     &       IELM.EQ.12.OR.
     &       IELM.EQ.13.OR.
     &       IELM.EQ.14.OR.
     &       IELM.EQ.70.OR.
     &       IELM.EQ.71.OR.
     &       IELM.EQ.80.OR.
     &       IELM.EQ.81.OR.
     &       IELM.EQ.61.OR.
     &       IELM.EQ.60.OR.
     &       IELM.EQ.20.OR.
     &       IELM.EQ.21) THEN
!
        DIMENS = 2
!
      ELSEIF(IELM.EQ.30.OR.
     &       IELM.EQ.31.OR.
     &       IELM.EQ.40.OR.
     &       IELM.EQ.41.OR.
     &       IELM.EQ.50.OR.
     &       IELM.EQ.51    ) THEN
!
        DIMENS = 3
!
      ELSE
        WRITE(LU,101) IELM
101     FORMAT(1X,'DIMENS (BIEF) : ',1I6,' ELEMENT NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
