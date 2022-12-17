!                   *****************
                    SUBROUTINE SURVOL
!                   *****************
!
     &(SURFAC, XEL,YEL,NELEM,NELMAX,IELM)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE AREA (VOLUME) OF THE ELEMENTS OF A MESH.
!code
!+  MEANING OF IELM:
!+
!+  TYPE OF ELEMENT      NUMBER OF POINTS      CODED IN THIS SUBROUTINE
!+
!+  11 : TRIANGLE P1            3                       YES
!+  21 : QUADRILATERAL Q1       4                       YES
!+  41 : TELEMAC-3D PRISMS      6                       NO
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        05/02/91
!+        V5P1
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
!| IELM           |-->| TYPE OF ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SURFAC         |<--| AREA OR VOLUME OF ELEMENTS.
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SURVOL => SURVOL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IELM,NELEM,NELMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: SURFAC(*)
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(IN) :: YEL(NELMAX,*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(IELM.EQ.11) THEN
!
        CALL SURV11(SURFAC, XEL,YEL,NELEM,NELMAX)
!
!     ELSEIF(IELM.EQ.21) THEN
!
!       CALL SURV21(SURFAC, XEL,YEL,NELEM,NELMAX)
!
!     ELSEIF(IELM.EQ.41) THEN
!
!       CALL SURV41(SURFAC, XEL,YEL,ZEL,NELEM,NELMAX)
!
!  VALUE FOR IELM NOT PERMITTED : ERROR
!
      ELSE
        WRITE(LU,101) IELM
101     FORMAT(1X,
     &  'SURVOL (BIEF) : IELM = ',1I6,' ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
