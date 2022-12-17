      FUNCTION TO_LOWER(STRIN) RESULT(STROUT)
      ! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May
      ! 2012)
      ! Original author: Clive Page

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: STRIN
      CHARACTER(LEN=LEN(STRIN)) :: STROUT
      INTEGER :: I,J

      DO I = 1, LEN(STRIN)
        J = IACHAR(STRIN(I:I))
        IF (J>= IACHAR("A") .AND. J<=IACHAR("Z") ) THEN
          STROUT(I:I) = ACHAR(IACHAR(STRIN(I:I))+32)
        ELSE
          STROUT(I:I) = STRIN(I:I)
        END IF
      END DO

      END FUNCTION TO_LOWER
