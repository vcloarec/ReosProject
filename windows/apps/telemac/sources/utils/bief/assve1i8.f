!                   *******************
                    SUBROUTINE ASSVE1I8
!                   *******************
!
     &(IX, IKLE,IW,NELEM)
!
!***********************************************************************
! BIEF   V7P0                                   13/01/2014
!***********************************************************************
!
!brief    Assembly loop for a vector of I8 integers.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        13/01/2014
!+        V7P0
!+   First version, inspired of assve1 (but masking not treated here)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| IW             |-->| WORK ARRAY WITH A NON ASSEMBLED FORM OF THE
!|                |   | RESULT
!| IX             |<->| ASSEMBLED VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER(KIND=K8), INTENT(INOUT) :: IX(*)
      INTEGER         , INTENT(IN)    :: NELEM
      INTEGER         , INTENT(IN)    :: IKLE(NELEM)
      INTEGER(KIND=K8), INTENT(IN)    :: IW(NELEM)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
!
!-----------------------------------------------------------------------
!
      DO IELEM = 1 , NELEM
        IX(IKLE(IELEM)) = IX(IKLE(IELEM)) + IW(IELEM)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
