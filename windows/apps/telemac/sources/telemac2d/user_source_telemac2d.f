!                   ********************************
                    SUBROUTINE USER_SOURCE_TELEMAC2D
!                   ********************************
!
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    REDEFINES THE CHARACTERISTICS OF THE SOURCES
!+                WITHOUT USING THE STEERING FILE.
!
!code
!+     EXAMPLE
!+
!+     NREJET = ?  (UNTIL MAXSCE)
!+     NREJEU = NREJET (IF VELOCITIES GIVEN)
!+
!+     DO I=1,NREJET
!+
!+       XSCE(I) = ???
!+       YSCE(I) = ???
!+       DSCE(I) = ???
!+       TSCE(I) = ???
!+       USCE(I) = ???
!+       VSCE(I) = ???
!+
!+     ENDDO
!
!history  J-M HERVOUET LNH
!+        26/10/1994
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
!
      INTEGER I,J
!
!-----------------------------------------------------------------------
!
      IF (.FALSE.) THEN
!     EXAMPLE
      NREJET = 1 ! (UNTIL MAXSCE)
      NREJEU = NREJET ! (IF VELOCITIES GIVEN)

      DO I=1,NREJET

        XSCE(I) = 0.D0
        YSCE(I) = 0.D0
        DSCE(I) = 0.D0
        USCE(I) = 0.D0
        VSCE(I) = 0.D0
        DO J=1,NTRAC
          TSCE(I,J) = 0.D0
        ENDDO

      ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
