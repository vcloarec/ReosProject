!                   *************************
                    SUBROUTINE SPECIAL_PLANTE
!                   *************************
!
     &(IVAL,NCSIZE,LNG,LU)
!
!***********************************************************************
! SPECIAL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    CALLED BY PLANTE FROM BIEF
!+             FOR POSSIBLE SYSTEM DEPENDENT EXIT PROCEDURE
!
!warning  ALSO EXISTS IN THE BIEF LIBRARY
!warning  CALL TO PLANTE MUST BE FOLLOWED BY A "STOP" SO THAT
!+            THE COMPILER UNDERSTANDS THAT'S THE END
!
!bug      IN THE EVENT OF A COMPILATION ERROR WITH THIS SUBROUTINE
!+            ERASE THE TWO LINES MARKED CJAJ
!
!history  J-M HERVOUET (LNH) ; F  LEPEINTRE (LNH)
!+        17/08/1994
!+        V5P5
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
!| IVAL           |-->| VALEUR ENTIERE
!| LNG            |-->| 1: FRENCH 2: ENGLISH
!| LU             |-->| NUMBER OF PROCESSORS
!| NCSIZE         |-->| NUMBER OF PROCESSORS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IVAL,NCSIZE,LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!-----------------------------------------------------------------------
!
!     THIS IS NOT STANDARD FORTRAN
!     CALL EXIT(ICODE)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
