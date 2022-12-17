!                   **************************
                    SUBROUTINE READ_FIC_POINTS
!                   **************************
!
     &( FFORMAT,FID,NPOIN,VALUE_PLY,X_PLY,Y_PLY,IERR )
!
!***********************************************************************
! TELEMAC2D   V8P0
!***********************************************************************
!
!brief    Reads data from a polygon ASCII file, allowing for multiple
!+        polygons defined within the same file.
!
!note     So far, only the I2S format (native of Blue Kenue) is
!+        supported.
!
!history  S.E. BOURBAN (HRW)
!+        21/08/2018
!+        V8P0
!+        New subroutine created to let users define initial zones for
!+        the sampling of drogues.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE LOGICAL UNIT
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!| NPOIN          |<->| NUMBER OF POINTS
!| VALUE_PLY      |<->| VALUE OF POINTS
!| X_PLY          |<->| X COORDINATES OF POINTS
!| Y_PLY          |<->| Y COORDINATES OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,           INTENT(INOUT) :: NPOIN
      DOUBLE PRECISION,  INTENT(INOUT) :: VALUE_PLY(*)
      DOUBLE PRECISION,  INTENT(INOUT) :: X_PLY(*),Y_PLY(*)
      CHARACTER(LEN=8),  INTENT(IN)    :: FFORMAT
      INTEGER,           INTENT(IN)    :: FID
      INTEGER,           INTENT(INOUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     MAXIMUM NUMBER OF CHARACTERS PER LIGN (MAY BE CHANGED)
!
      INTEGER, PARAMETER :: SIZELIGN = 3000
      CHARACTER(LEN=SIZELIGN) :: LIGNE
!
      INTEGER          ILINE
      DOUBLE PRECISION RVALUE,XVALUE,YVALUE
!
!-----------------------------------------------------------------------
!
      IERR = 0
!
!-----------------------------------------------------------------------
!
!     HEADER OF BLUE KENUE XYZ FILE IN ASCII FORM
      IF( FFORMAT .EQ.'BKASCXYZ' .AND. FID.NE.0 ) THEN
        ILINE = 0
        REWIND(FID)
!
!       SKIPS COMMENTS FOR NOW (TODO: RECORD ATTRIBUTES)
        DO WHILE( .TRUE. )
          ILINE = ILINE + 1
          READ( FID,FMT='(A)',END=1001,ERR=1901 ) LIGNE
          IF( LIGNE(1:10).EQ.':EndHeader' ) EXIT
        ENDDO
!
        NPOIN = 0
        DO WHILE( .TRUE. )
          ILINE = ILINE + 1
          READ( FID,*,END=23,ERR=23 ) XVALUE,YVALUE,RVALUE
          NPOIN = NPOIN + 1
          X_PLY(NPOIN) = XVALUE
          Y_PLY(NPOIN) = YVALUE
          VALUE_PLY(NPOIN) = RVALUE
        ENDDO
 23     CONTINUE
!
      ELSE
!       UNRECOGNISED FILE FORMAT
        IERR = 1
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
!
!-----------------------------------------------------------------------
!
 1001 CONTINUE
      WRITE(LU,*) 'REACHED THE END OF THE XYZ FILE'
      WRITE(LU,*) 'BEFORE FINDING THE END OF THE HEADER.'
      WRITE(LU,*) 'MAYBE THIS IS NOT AN XYZ FILE.'
      CALL PLANTE(1)
      STOP
 1901 CONTINUE
      WRITE(LU,*) 'READING ERROR ON THE XYZ FILE'
      WRITE(LU,*) 'AT LINE OF DATA : ',ILINE
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
