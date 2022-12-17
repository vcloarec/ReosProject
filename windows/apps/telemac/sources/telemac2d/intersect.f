!                   *********************
                    SUBROUTINE  INTERSECT
!                   *********************
!
     &(AAX,AAY,AAC,BBX,BBY,BBC,X_S,Y_S,INFO)
!
!***********************************************************************
! TELEMAC2D   V8P3
!***********************************************************************
!
!brief    COMPUTE INTERSECTION POINT OF TWO SEGMENTS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      AAX,AAY,AAC  SYSTEM COEFFICIENTS (FIRST ROW)
!>@param  [in]      BBX,BBY,BBC  SYSTEM COEFFICIENTS (SECOND ROW)
!>@param  [out]     X_S,  Y_S    COORDINATES OF INTERSECTION POINT S
!>@param  [out]     INFO         INFO: 0 ABORT / 1 SUCCESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     WE WRITE THE SYSTEM OF EQUATIONS
!     AAX + AAY = AAC   
!     BBX + BBY = BBC
!     DETERMINANT DD: |AAX AAY|  DX: |AAC AAY|  DY: |AAX AAC|
!                     |BBX BBY|      |BBC BBY|      |BBX BBC|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION,INTENT(IN)  :: AAX, AAY, AAC
      DOUBLE PRECISION,INTENT(IN)  :: BBX, BBY, BBC
      DOUBLE PRECISION,INTENT(OUT) :: X_S,Y_S  ! POINT:  S
      INTEGER,INTENT(OUT)          :: INFO
!
      DOUBLE PRECISION::  DD, DX, DY           ! DETERMINANTS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DD = AAX*BBY - AAY*BBX
      DX = AAC*BBY - AAY*BBC
      DY = AAX*BBC - AAC*BBX
!
      IF (DD .EQ. 0.D0) THEN
        WRITE(LU,*) 'WARNING! ABORT INTERSECTION: COLINEAR SEGMENTS'
        X_S = 0.D0
        Y_S = 0.D0
        INFO = 0
      ELSE
        X_S = DX / DD
        Y_S = DY / DD
        INFO = 1
      ENDIF
!
!---------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE INTERSECT
