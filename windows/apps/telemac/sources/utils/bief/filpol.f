!                   *****************
                    SUBROUTINE FILPOL
!                   *****************
!
     &( F , C , XSOM , YSOM , NSOM , MESH )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    THE POINTS OF VECTOR F, WHICH ARE INSIDE OF THE
!+                POLYGON DEFINED BY VERTICES XSOM AND YSOM, ARE
!+                INITIALISED TO CONSTANT C.
!
!note     THE POLYGON IS READ ANTI-CLOCKWISE.
!
!history  J-M HERVOUET (LNH)
!+        08/12/94
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
!| C              |-->| CONSTANT TO BE PRESCRIBED INSIDE THE POLYGON.
!| F              |<->| THE RESULT (VECTOR)
!| MESH           |-->| MESH STRUCTURE
!| NSOM           |-->| NUMBER OF VERTICES IN THE POLYGON
!| XSOM           |-->| ABSCISSAE OF VERTICES IN THE POLYGON
!| YSOM           |-->| ORDINATES OF VERTICES IN THE POLYGON
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FILPOL => FILPOL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NSOM
      DOUBLE PRECISION, INTENT(IN) :: C,XSOM(*),YSOM(*)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: F
      TYPE(BIEF_MESH), INTENT(IN) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NPOIN,IELM
!
!-----------------------------------------------------------------------
!
!  TYPE OF ELEMENT FOR VELOCITY
!
      IELM = F%ELM
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.10) THEN
!
        CALL FILP10( F%R , C , XSOM , YSOM , NSOM ,
     &       MESH%X%R,MESH%Y%R,MESH%NELEM,MESH%NELMAX,MESH%IKLE%I)
!
      ELSEIF(IELM.EQ.11) THEN
!
        NPOIN = F%DIM1
        CALL FILP11(F%R,C,XSOM,YSOM,NSOM,MESH%X%R,MESH%Y%R,NPOIN)
!
      ELSEIF(IELM.EQ.12) THEN
!
        NPOIN = F%DIM1 - MESH%NELEM
        CALL FILP12( F%R , C , XSOM , YSOM , NSOM , MESH%X%R , MESH%Y%R,
     &               NPOIN , MESH%NELEM , MESH%NELMAX , MESH%IKLE%I)
!
      ELSE
!
!       ELM NOT IMPLEMENTED: ERROR
!
        WRITE(LU,101) IELM
101     FORMAT(1X,'FILPOL (BIEF): IELM = ',1I6,' ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
