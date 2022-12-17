!                   ***********************
                    SUBROUTINE USER_MESURES
!                   ***********************
!
     &(ITER,TT)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    READS MEASURED H, U AND V AT TIME AT.
!+        GIVES THE CORRESPONDING WEIGHTS ALPHA1, ALPHA2 AND ALPHA3.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ITER           |-->| ITERATION WHERE TO LOOK FOR THE MEASUREMENTS
!| TT             |-->| CORRESPONDING TIME (TO CHECK)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: ITER
      DOUBLE PRECISION, INTENT(IN) :: TT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      WRITE(LU,*) 'MEASUREMENTS TO IMPLEMENT IN USER_MESURES'
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
