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
!
!-----------------------------------------------------------------------
!
!     TO BE REMOVED IF IMPLEMENTED FOR A SPECIFIC CASE
!     THIS JUST TO WARN IN CASE A USER FORGETS TO DO IT
!
      WRITE(LU,*) 'SUBROUTINE DEF_ZONES MUST BE INCLUDED'
      WRITE(LU,*) 'IN THE FORTRAN FILE TO DEFINE ZONES'
      CALL PLANTE(1)
      STOP
!
!     NZONE = ???
!     ZONE%I(I) = ???
!
!-----------------------------------------------------------------------
!
      RETURN
      END

