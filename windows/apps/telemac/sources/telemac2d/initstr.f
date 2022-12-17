!                   ******************
                    SUBROUTINE INITSTR
!                   ******************
!
     &(CHESTR,SETSTR,PZONE,NZONE,NPOIN,T1)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ASSIGNS INITIAL VALUES OF STRICKLERS PER ZONE.
!
!history  J-M HERVOUET (LNHE)
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
!| CHESTR         |-->| FRICTION COEFFICIENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NZONE          |-->| NUMBER OF ZONES
!| PZONE          |-->| TABLE OF ZONES
!| SETSTR         |-->| SET OF STRICKLERS (ZONES)
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN)      :: CHESTR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SETSTR,T1
      INTEGER, INTENT(IN)             :: PZONE(*)
      INTEGER, INTENT(IN)             :: NZONE
      INTEGER, INTENT(IN)             :: NPOIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J
!
!----------------------------------------------------------------------
!
      IF(NZONE.GT.0) THEN
!
!       ZONATION : SETSTR=AVERAGE PER ZONE OF CHESTR
!
        CALL OS('X=0     ',X=SETSTR)
        CALL OS('X=Y     ',X=T1    ,Y=SETSTR)
        DO J=1,NZONE
          DO I=1,NPOIN
            IF(PZONE(I).EQ.J) THEN
              SETSTR%R(J)=SETSTR%R(J)+CHESTR%R(I)
              T1%R(J)=T1%R(J)+1.D0
            ENDIF
          ENDDO
          SETSTR%R(J)=SETSTR%R(J)/T1%R(J)
        ENDDO
!
      ELSE
!
!       NO ZONATION : SETSTR=CHESTR
!
        CALL OS('X=Y     ',X=SETSTR,Y=CHESTR)
!
      ENDIF
!
!----------------------------------------------------------------------
!
      RETURN
      END
