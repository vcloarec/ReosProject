!                   ********************
                    SUBROUTINE ASSIGNSTR
!                   ********************
!
     &(CHESTR,SETSTR,PZONE,NZONE,NPOIN)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ASSIGNS STRICKLER VALUES.
!
!history  A. LEOPARDI (UNINA)
!+        08/11/2000
!+
!+
!
!history  J-M HERVOUET
!+        22/10/2001
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
!| CHESTR         |<--| FRICTION COEFFICIENT
!| NPOIN          |-->| NUMBER OF POINTS
!| NZONE          |-->| NUMBER OF ZONES
!| PZONE          |-->| TABLE OF ZONES
!| SETSTR         |-->| SET OF STRICKLERS' (ZONES)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: CHESTR
      TYPE(BIEF_OBJ), INTENT(IN)      :: SETSTR
      INTEGER, INTENT(IN)             :: PZONE(*)
      INTEGER, INTENT(IN)             :: NZONE
      INTEGER, INTENT(IN)             :: NPOIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J
!
!---------------------------------------------------------------------
!
      IF(NZONE.GT.0) THEN
        DO J=1,NZONE
          DO I=1,NPOIN
            IF (PZONE(I).EQ.J) CHESTR%R(I)=SETSTR%R(J)
          ENDDO
        ENDDO
      ELSE
        CALL OS('X=Y     ',X=CHESTR,Y=SETSTR)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
