!                   *************************
                    SUBROUTINE  INTERSECT_SEG
!                   *************************
!
     &(X_A1,Y_A1,X_A2,Y_A2,X_B1,Y_B1,X_B2,Y_B2,X_S,Y_S,INFO)
!
!***********************************************************************
! TELEMAC2D   V8P3
!***********************************************************************
!
!brief    COMPUTE INTERSECTION POINT OF TWO SEGMENTS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      X_A1, Y_A1   COORDINATES OF POINT A1
!>@param  [in]      X_A2, Y_A2   COORDINATES OF POINT A2
!>@param  [in]      X_B1, Y_B1   COORDINATES OF POINT B1
!>@param  [in]      X_B2, Y_B2   COORDINATES OF POINT B2
!>@param  [out]     X_S,  Y_S    COORDINATES OF INTERSECTION POINT S
!>@param  [out]     INFO         INFO: 0 ABORT / 1 SUCCESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       \       /
!        \     * A2  WE HAVE TWO LINES EACH GIVEN BY TWO POINTS
!      B2 *   /      BUT WE NEED THE EQUATION OF A LINE IN
!          \ /       GENERAL (OR STANDARD) FORM WHICH IS   "A*X+B*Y=C"
!           S        THUS WE CALCULATE THE COEFFICIENTS    "A   B   C"
!          / \
!         /   \
!     A1 *     \
!       /       * B1
!      /         \
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION,INTENT(IN)  :: X_A1,Y_A1, X_A2,Y_A2
      DOUBLE PRECISION,INTENT(IN)  :: X_B1,Y_B1, X_B2,Y_B2
      DOUBLE PRECISION,INTENT(OUT) :: X_S,Y_S
      INTEGER,INTENT(OUT)          :: INFO
!
      DOUBLE PRECISION :: AAX, AAY, AAC ! EQUATION COEFFICIENTS A1A2
      DOUBLE PRECISION :: BBX, BBY, BBC ! EQUATION COEFFICIENTS B1B2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      AAX = Y_A1 - Y_A2           ! COEFFICIENT  "A" OF LINE A1A2
      AAY = X_A2 - X_A1           ! COEFFICIENT  "B" OF LINE A1A2
      AAC = X_A2*Y_A1 - X_A1*Y_A2 ! COEFFICIENT  "C" OF LINE A1A2
!
      BBX = Y_B1 - Y_B2           ! COEFFICIENT  "A" OF LINE B1B2
      BBY = X_B2 - X_B1           ! COEFFICIENT  "B" OF LINE B1B2
      BBC = X_B2*Y_B1 - X_B1*Y_B2 ! COEFFICIENT  "C" OF LINE B1B2
!
      CALL INTERSECT( AAX,AAY,AAC, BBX,BBY,BBC, X_S,Y_S, INFO)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE INTERSECT_SEG
