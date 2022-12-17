!                   ***************************
                    SUBROUTINE READ_FIC_POLYGON
!                   ***************************
!
     &( FFORMAT,FID,NPOLY,NPOIN_PLY,VALUE_PLY,X_PLY,Y_PLY,IERR )
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
!| NPOIN_PLY      |<->| NUMBER OF POINTS FOR EACH POLYGON
!| NPOLY          |<->| NUMBER OF POLYGONS
!| VALUE_PLY      |<->| VALUE OF POLYGONS
!| X_PLY          |<->| X COORDINATES OF POLYGONS
!| Y_PLY          |<->| Y COORDINATES OF POLYGONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,           INTENT(INOUT) :: NPOLY
      INTEGER,           INTENT(INOUT) :: NPOIN_PLY(*)
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
      INTEGER          IVALUE,I,NPOIN,ILINE
      DOUBLE PRECISION RVALUE
!
      DOUBLE PRECISION, PARAMETER :: CHOUIA = 1.D-9
!
!-----------------------------------------------------------------------
!
      IERR = 0
!
!-----------------------------------------------------------------------
!
!     HEADER OF BLUE KENUE I2S FILE IN ASCII FORM
      IF( FFORMAT .EQ.'BKASCI2S' .AND. FID.NE.0 ) THEN
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
        NPOLY = 0
        NPOIN = 0
        DO WHILE( .TRUE. )
          ILINE = ILINE + 1
          READ( FID,*,END=23,ERR=23 ) IVALUE,RVALUE
          NPOLY = NPOLY + 1
          VALUE_PLY(NPOLY) = RVALUE
          DO I = 1,IVALUE
            ILINE = ILINE + 1
            READ( FID,*,END=1002,ERR=1901 )
     &        X_PLY(NPOIN+I),Y_PLY(NPOIN+I)
          ENDDO
!         REMOVE DUPLICATE POINT OF CLOSED POLYGONS
          IF( ( (X_PLY(NPOIN+1)-X_PLY(NPOIN+IVALUE))**2 +
     &      (Y_PLY(NPOIN+1)-Y_PLY(NPOIN+IVALUE))**2 ).LT.CHOUIA ) THEN
            IVALUE = IVALUE - 1
          ENDIF
          NPOIN = NPOIN + IVALUE
          NPOIN_PLY(NPOLY) = NPOIN
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
      WRITE(LU,*) 'REACHED THE END OF THE POLYGON FILE'
      WRITE(LU,*) 'BEFORE FINDING THE END OF THE HEADER.'
      WRITE(LU,*) 'MAYBE THIS IS NOT AN I2S FILE.'
      CALL PLANTE(1)
      STOP
 1002 CONTINUE
      WRITE(LU,*) 'REACHED THE END OF THE POLYGON FILE'
      WRITE(LU,*) 'BEFORE COMPLETING POLYGON ',NPOLY
      CALL PLANTE(1)
      STOP
 1901 CONTINUE
      WRITE(LU,*) 'READING ERROR ON THE POLYGON FILE'
      WRITE(LU,*) 'AT LINE OF DATA : ',ILINE
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
