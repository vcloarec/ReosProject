!                   ****************
                    SUBROUTINE OVD_2
!                   ****************
!
     & ( OP , X , DIMX , Y , DIMY , Z , DIMZ , C , DIM1 , NPOIN ,
     &   IOPT , INFINI, ZERO )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BETWEEN OS AND OVD WHEN 2-DIMENSION VECTORS ARE INVOLVED.
!
!history  J-M HERVOUET (LNH)
!+        29/11/94
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
!| C              |-->| A GIVEN CONSTANT
!| DIM1           |-->| FIRST DIMENSION OF X, Y AND Z
!| DIMX           |-->| SECOND DIMENSION OF X
!| DIMY           |-->| SECOND DIMENSION OF Y
!| DIMZ           |-->| SECOND DIMENSION OF Z
!| INFINI         |-->| VALUE TO PUT INSTEAD OF 1/0
!| IOPT           |-->| OPTION FOR DIVISIONS BY ZERO
!|                |   | 1: NO TEST DONE (WILL CRASH IF DIVISION BY 0.).
!|                |   | 2: INFINITE TERMS REPLACED BY CONSTANT INFINI.
!|                |   | 3: STOP IF DIVISION BY ZERO.
!|                |   | 4: DIVISIONS BY 0. REPLACED BY DIVISIONS/ZERO
!|                |   |    ZERO BEING AN OPTIONAL ARGUMENT
!| NPOIN          |-->| SIZE OF VECTORS
!| OP             |-->| STRING INDICATING THE OPERATION TO BE DONE
!| X              |<--| RESULTING VECTOR
!| Y              |-->| TO BE USED IN THE OPERATION
!| Z              |-->| TO BE USED IN THE OPERATION
!| ZERO           |-->| CRITERION FOR DIVISION BY ZERO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: DIMX,DIMY,DIMZ,DIM1,NPOIN,IOPT
      DOUBLE PRECISION, INTENT(IN)    :: C,INFINI,ZERO
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      DOUBLE PRECISION, INTENT(INOUT) :: X(DIM1,*)
      DOUBLE PRECISION, INTENT(IN)    :: Y(DIM1,*),Z(DIM1,*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL OVD( OP , X(1,DIMX) , Y(1,DIMY) , Z(1,DIMZ) , C , NPOIN ,
     &          IOPT , INFINI , ZERO )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
