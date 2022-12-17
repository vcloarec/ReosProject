      MODULE HASH_TABLE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: MAX_FILE = 3000
!
      CONTAINS
!
      SUBROUTINE GET_OBJ
!***********************************************************************
!
     &(HASH,FILE_ID,HASHED_ID,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the index in the obj_tab for a give file id
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HASH           |<->| HASH TABLE
!| FILE_ID        |-->| ID OF THE FILE
!| HASHED_ID      |<->| ID IN THE SRF_OBJ_TAB
!| IERR           |-->| 0 IF EVERYTHING WENT FINE, ERROR INDEX OTHERWISE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(INOUT) :: HASHED_ID
        INTEGER, INTENT(INOUT) :: HASH(MAX_FILE)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: I
        !
        HASHED_ID = 0
        ! LOOK FOR THE ID LINKED TO THAT FILE_ID
        DO I=1,MAX_FILE
          IF (HASH(I).EQ.FILE_ID) THEN
            HASHED_ID = I
            EXIT
          ENDIF
        ENDDO
        ! IF HASHED_ID == 0 THEN NO ID WAS FOUND RETURN ERROR
        IF(HASHED_ID.EQ.0) THEN
          IERR = HERMES_FILE_NOT_OPENED_ERR
        ELSE
          IERR = 0
        ENDIF
        HASHED_ID = HASHED_ID
        !
        RETURN
      END SUBROUTINE

!***********************************************************************
      SUBROUTINE ADD_OBJ
!***********************************************************************
!
     &(HASH,FILE_ID,HASHED_ID,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Add a new file to the obj_tab and returns its new id
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HASH           |<->| HASH TABLE
!| FILE_ID        |-->| ID OF THE FILE
!| HASHED_ID      |<->| ID IN THE SRF_OBJ_TAB
!| IERR           |-->| 0 IF EVERYTHING WENT FINE, ERROR INDEX OTHERWISE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(INOUT) :: HASH(MAX_FILE)
        INTEGER, INTENT(IN)  :: FILE_ID
        INTEGER, INTENT(INOUT) :: HASHED_ID
        INTEGER, INTENT(OUT) :: IERR
        !
        HASHED_ID = 0
        ! CHECK IF THE FILE IS ALREADY OPEN
        CALL GET_OBJ(HASH,FILE_ID,HASHED_ID,IERR)
        IF(HASHED_ID.NE.0) THEN
          IERR = HERMES_FILE_ID_ALREADY_IN_USE_ERR
          RETURN
        ENDIF
        ! WE RESET THE IERR TO ZERO AS THE PREVIOUS CALL SHOULD
        ! HAVE CRASHED AS THE FILES IS NOT OPENED YET
        IERR = 0
        !
        ! Look far a place in the hash table
        HASHED_ID = 1
        DO
          IF(HASHED_ID.GT.MAX_FILE) EXIT
          IF(HASH(HASHED_ID).EQ.0) EXIT
          HASHED_ID = HASHED_ID + 1
        ENDDO
        ! CHECK IF WE'VE REACH THE MAXIMUM NUMBER OF FILES
        IF(HASHED_ID.GT.MAX_FILE) THEN
          IERR = HERMES_MAX_FILE_ERR
          RETURN
        ELSE
          HASH(HASHED_ID) = FILE_ID
        ENDIF
        !
        RETURN
      END SUBROUTINE

      END MODULE HASH_TABLE
