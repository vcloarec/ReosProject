!                   ******************************
                    SUBROUTINE TRANSFER_GROUP_INFO
!                   ******************************
!
     &(FFORMAT,FID,FID2,TYPE_ELT,TYPE_BND_ELT,IKLE_BND,NELEBD,NDP,
     & TRANS_ELEM,TRANS_POINT,IERR)
!
!***********************************************************************
! HERMES   V7P0 01/05/2014
!***********************************************************************
!
!brief    Transfer group information from fid into fid2
!         Writes boundary elements as well
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR OF INPUT FILE
!| FID2           |-->| FILE DESCRIPTOR OF OUTPUT FILE
!| TYPE_ELT       |-->| TYPE OF THE ELEMENTS
!| TYPE_BND_ELT   |-->| TYPE OF THE BOUNDARY ELEMENTS
!| TRANS_ELEM     |-->| IF TRUE TRANSFERING GROUP ON TYP_ELT AS WELL
!| TRANS_POINT    |-->| IF TRUE TRANSFERING GROUP ON POINTS AS WELL
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE UTILS_SERAFIN
      USE UTILS_MED
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
      INTEGER,          INTENT(IN)  :: FID, FID2
      INTEGER,          INTENT(IN)  :: NELEBD
      INTEGER,          INTENT(IN)  :: NDP
      INTEGER,          INTENT(IN)  :: IKLE_BND(*)
      INTEGER,          INTENT(IN)  :: TYPE_ELT
      INTEGER,          INTENT(IN)  :: TYPE_BND_ELT
      LOGICAL,          INTENT(IN)  :: TRANS_ELEM
      LOGICAL,          INTENT(IN)  :: TRANS_POINT
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          ! DOING nothing
          CONTINUE
        CASE ('MED    ')
          CALL TRANSFER_GROUP_INFO_MED(FID,FID2,TYPE_ELT,
     &                                 TYPE_BND_ELT,IKLE_BND,NELEBD,NDP,
     &                                 TRANS_ELEM,TRANS_POINT,IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*)
     &         'TRANSFER_GROUP_INFO: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
