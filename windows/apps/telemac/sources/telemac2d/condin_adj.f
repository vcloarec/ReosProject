!                   *********************
                    SUBROUTINE CONDIN_ADJ
!                   *********************
!
     &(NRES,RESFORMAT)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE PHYSICAL PARAMETERS TO START
!+                AN ADJOINT COMPUTATION.
!
!history  J-M HERVOUET (LNHE)
!+        24/04/2009
!+        V6P0
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
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NRES           |-->| LOGICAL UNIT OF RESULTS FILE
!| RESFORMAT      |-->| FORMAT OF RESULTS FILE
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
      INTEGER, INTENT(IN)    :: NRES
      CHARACTER(LEN=*), INTENT(IN) :: RESFORMAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITER, IERR, ITER1
      DOUBLE PRECISION AT1
!
!-----------------------------------------------------------------------
!
!     CONDIN FOR ADJOINT PROBLEM: T=T(N+1) : P=0,Q=0,R=0
!
      CALL OS('X=C     ',X=PP,C=0.D0)
      CALL OS('X=C     ',X=QQ,C=0.D0)
      CALL OS('X=C     ',X=RR,C=0.D0)
!
!     JUST IN CASE CV1,.. IS WRITTEN IN THE RESULT FILE
!
      CALL OS('X=C     ',X=CV1,C=0.D0)
      CALL OS('X=C     ',X=CV2,C=0.D0)
      CALL OS('X=C     ',X=CV3,C=0.D0)
!
!     READS THE LAST TIME IN THE TELEMAC RESULT FILE (NRES)
!     INITIALISES U,V AND H
!
      CALL GET_DATA_NTIMESTEP(RESFORMAT,NRES,ITER,IERR)
      CALL CHECK_CALL(IERR,'CONDIN_ADJ:GET_DATA_NTIMESTEP')
      ! Iteration are from 0 to ntimestep - 1
      ITER = ITER - 1
      CALL READ_DATASET(RESFORMAT,NRES,VARSOR,NPOIN,ITER,AT,TEXTE,
     &                  TROUVE,ALIRE,LISTIN,.TRUE.,MAXVAR)
!
!     GIVES MEASUREMENTS HD,UD AND VD AT THE LAST TIME STEP
!     (ITER AND AT GIVEN BY THE PREVIOUS CALL TO SUITE)
!
      CALL MESURES(ITER,AT)
!     INITIALISES HH, UU, VV
!
      CALL OS('X=Y     ', X=HH   , Y=H)
      CALL OS('X=Y     ', X=UU   , Y=U)
      CALL OS('X=Y     ', X=VV   , Y=V)
      CALL OS('X=C     ', X=HIT1 , C=0.D0 )
      CALL OS('X=C     ', X=UIT1 , C=0.D0 )
      CALL OS('X=C     ', X=VIT1 , C=0.D0 )
!
!     READS TELEMAC2D RESULTS (RESULT FILE - UNIT NRES)
!     THIS IS TO HAVE UN AT THE LAST TIME STEP INTO U.
!
!     BEWARE : ASSUMES THAT NVARRES HAS ALREADY BEEN COMPUTED
!
      ITER1 = ITER - 1
      CALL READ_DATASET(RESFORMAT,NRES,VARSOR,NPOIN,ITER1,AT1,TEXTE,
     &                  TROUVE,ALIRE,LISTIN,.FALSE.,MAXVAR)
!
!-----------------------------------------------------------------------
!
      AT = AT + DT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
