!                   *****************
                    SUBROUTINE GETTRI
!                   *****************
!
     &(M,MDIFF,TETA,MESH3D,NPLAN,NPOIN2,NSEG2D,IELM3,NELEM2)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    GETS THE TRIDIAGONAL PART OF A DIFFUSION MATRIX ON
!+                PRISMS AND REMOVES IT FROM THE INITIAL MATRIX.
!code
!+            IF MTRI IS THIS TRIDIAGONAL PART, M THE RESULT AND MDIF
!+            THE DIFFUSION MATRIX, THIS SUBROUTINE DOES:
!+
!+            M = TETA * MTRI
!+            MDIF CHANGED INTO (1-TETA) * MDIF
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)
!+        16/06/05
!+        V5P6
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
!| IELM3          |<--| TYPE OF ELEMENT
!| M              |<--| TRIDIAGONAL MATRIX
!| MDIFF          |-->| ORIGINAL DIFFUSION MATRIX
!| MESH3D         |-->| 3D MESH STRUCTURE
!| NELEM2         |-->| NUMBER OF TRIANGLES OF ORIGINAL 2D MESH
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN2         |-->| NUMBER OF POINTS OF 2D MESH
!| NSEG2D         |-->| NUMBER OF SEGMENTS IN 2D
!| TETA           |-->| COEFFICIENT USED IN THE RESULT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_GETTRI => GETTRI
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: NPLAN,NPOIN2,NSEG2D,IELM3,NELEM2
!
      DOUBLE PRECISION, INTENT(IN)    :: TETA
      DOUBLE PRECISION, INTENT(INOUT) :: M(NPOIN2*NPLAN,*)
!
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: MDIFF
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3D
!
!-----------------------------------------------------------------------
!
      IF(MDIFF%STO.EQ.1) THEN
!
        CALL GETTRIEBE(M,MDIFF%D%R,MDIFF%X%R,TETA,
     &                 MESH3D%IKLE%I,MESH3D%NPOIN,MESH3D%NELEM,
     &                 MESH3D%NELMAX,MESH3D,IELM3,NELEM2,NPLAN,
     &                 MESH3D%KNOLG%I)
!
      ELSEIF(MDIFF%STO.EQ.3) THEN
!
        CALL GETTRISEG(M,MDIFF%D%R,MDIFF%X%R,TETA,
     &                 MESH3D%NPOIN,MESH3D,
     &                 MESH3D%NSEG,NSEG2D,NPLAN,NPOIN2,IELM3)
!
      ELSE
!
        WRITE(LU,*) 'UNKNOWN STORAGE FOR MDIFF IN GETTRI'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
