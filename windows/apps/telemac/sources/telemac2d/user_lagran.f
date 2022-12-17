!                   **********************
                    SUBROUTINE USER_LAGRAN
!                   **********************
!
     &(NLAG,DEBLAG,FINLAG)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES FIRST AND FINAL TIMESTEPS FOR THE LAGRANGIAN DRIFTS.
!
!warning  TWO DRIFTS CANNOT COMPLETE IN THE SAME TIMESTEP (ONLY THE 1ST WILL BE WRITTEN TO FILE)
!warning  THE RESULTS MUST BE SAVED BETWEEN TWO DRIFT COMPUTATION ENDS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEBLAG         |<--| TIME STEP AT THE BEGINNING
!| FINLAG         |<--| TIME STEP AT THE END
!| NLAG           |-->| NUMBER OF LAGRANGIAN DRIFTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NLAG
      INTEGER, INTENT(INOUT) :: DEBLAG(NLAG) , FINLAG(NLAG)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ILAG
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE FIRST AND FINAL TIMESTEPS FOR THE COMPUTATION OF
!   LAGRANGIAN DRIFT - BY DEFAULT NOTHING IS DONE
!
!-----------------------------------------------------------------------
!
!     THIS WARNING AND THE CALL TO PLANTE MUST BE REMOVED IF
!     SOMETHING IS IMPLEMENTED BY THE USER BELOW
!
      WRITE(LU,21)
21    FORMAT(1X,'WARNING, YOU CALL SUBROUTINE LAGRAN OF THE LIBRARY.',
     &     /,1X,'AS YOU COMPUTE ONE OR MORE FIELDS OF LAGRANGIAN',/,1X,
     &          'DRIFTS, YOU NEED TO COPY THIS SUBROUTINE IN YOUR',/,1X,
     &          'OWN FORTRAN FILE AND COMPLETE IT.',/////)
!
      CALL PLANTE(1)
!
!-----------------------------------------------------------------------
!
!  EXAMPLE :
!
      IF(.FALSE.) THEN
        DO ILAG=1,NLAG
          DEBLAG(ILAG) = 1
          FINLAG(ILAG) = 299
        ENDDO ! ILAG
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
