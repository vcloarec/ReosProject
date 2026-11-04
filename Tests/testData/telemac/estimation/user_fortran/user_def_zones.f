!                   *************************
                    SUBROUTINE USER_DEF_ZONES
!                   *************************
!
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    DEFINES ZONES IN THE MESH. THE RESULT MUST BE :
!+
!+            NZONE : THE NUMBER OF ZONES,
!+
!+            ZONE : STRUCTURE OF SIZE NPOIN STATING THE ZONE NUMBER
!+                       OF EVERY POINT.
!
!warning  USER SUBROUTINE; DOES NOTHING BY DEFAULT
!
!history  J-M HERVOUET
!+        17/08/2001
!+        V5P3
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      NZONE = 1
!
      DO I=1,NPOIN
        ZONE%I(I) = 1
      ENDDO
!-----------------------------------------------------------------------
!
      RETURN
      END

