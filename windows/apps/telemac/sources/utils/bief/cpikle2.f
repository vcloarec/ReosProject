!                   ******************
                    SUBROUTINE CPIKLE2
!                   ******************
!
     &(IKLE3,KLEI3,IKLES,NELEM2,NELMAX2,NPOIN2,NPLAN)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    EXTENDS THE CONNECTIVITY TABLE.
!+                CASE OF A MESH OF PRISMS, IKLE BUILT FROM THE
!+                CONNECTIVITY OF THE TRIANGLE MESH.
!
!history  J-M HERVOUET (LNH)
!+        23/08/99
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
!| IKLE3          |<->| 3D CONNECTIVITY TABLE
!| IKLES          |-->| 2D CONNECTIVITY TABLE WITH DIMENSION (3,NELEM2)
!| KLEI3          |<--| LIKE IKLE3 BUT WITH SWAPPED DIMENSIONS
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NELMAX2        |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELEM2,NELMAX2,NPOIN2,NPLAN
      INTEGER, INTENT(IN)    :: IKLES(3,NELEM2)
      INTEGER, INTENT(INOUT) :: IKLE3(NELMAX2,NPLAN-1,6)
      INTEGER, INTENT(INOUT) :: KLEI3(6,NELMAX2,NPLAN-1)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I
!
!-----------------------------------------------------------------------
!
!     BOTTOM AND TOP OF ALL LAYERS
!
      IF(NPLAN.GE.2) THEN
        DO I = 1,NPLAN-1
          DO IELEM = 1,NELEM2
            IKLE3(IELEM,I,1) = IKLES(1,IELEM) + (I-1)*NPOIN2
            IKLE3(IELEM,I,2) = IKLES(2,IELEM) + (I-1)*NPOIN2
            IKLE3(IELEM,I,3) = IKLES(3,IELEM) + (I-1)*NPOIN2
            IKLE3(IELEM,I,4) = IKLES(1,IELEM) +  I   *NPOIN2
            IKLE3(IELEM,I,5) = IKLES(2,IELEM) +  I   *NPOIN2
            IKLE3(IELEM,I,6) = IKLES(3,IELEM) +  I   *NPOIN2
            KLEI3(1,IELEM,I) = IKLES(1,IELEM) + (I-1)*NPOIN2
            KLEI3(2,IELEM,I) = IKLES(2,IELEM) + (I-1)*NPOIN2
            KLEI3(3,IELEM,I) = IKLES(3,IELEM) + (I-1)*NPOIN2
            KLEI3(4,IELEM,I) = IKLES(1,IELEM) +  I   *NPOIN2
            KLEI3(5,IELEM,I) = IKLES(2,IELEM) +  I   *NPOIN2
            KLEI3(6,IELEM,I) = IKLES(3,IELEM) +  I   *NPOIN2
          ENDDO
        ENDDO
      ELSE
        WRITE(LU,*) 'CPIKLE2 : MINIMUM OF 2 PLANES NEEDED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
