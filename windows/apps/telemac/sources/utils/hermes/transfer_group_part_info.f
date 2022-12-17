!                   ***********************************
                    SUBROUTINE TRANSFER_GROUP_PART_INFO
!                   ***********************************
!
     &(FFORMAT,SOURCE_ID, DEST_ID,
     & TYP_BND_ELEM, IKLE_BND_DEST, NELEBD_DEST,
     & NDP_DEST, NELEBD_SRC, KNOLG_BND,
     & TRANS_POINT, NPOIN_SRC, NPOIN_DEST, KNOLG, IERR)
!
!***********************************************************************
! HERMES   V7P0 01/05/2014
!***********************************************************************
!
!brief    Transfer group information from fid into fid2
!+        with fid2 a partionned mesh of fid
!+        Writes boundary elements as well
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| SOURCE_ID      |-->| FILE DESCRIPTOR OF INPUT FILE
!| DEST_ID        |-->| FILE DESCRIPTOR OF OUTPUT FILE
!| TYPE_ELEM      |-->| TYPE OF THE ELEMENTS
!| TYPE_BND_ELEM  |-->| TYPE OF THE BOUNDARY ELEMENTS
!| IKLE_BND_DEST  |-->| CONNECTIVITY FOR OUTPUT FILE BOUNDARY ELEMENTS
!| NELEBD_DEST    |-->| NUMBER OF BOUNDARY ELEMENTS IN OUTPUT FILE
!| NDP_DEST       |-->| Number of node per element in output file
!| NELEBD_SRC     |-->| Number of noudary elements in the input file
!| KNOLG_BND      |-->| Local to global numbering for boundary elements
!| TRANS_POINT    |-->| IF TRUE TRANSFERING GROUP ON POINTS AS WELL
!| NPOIN_SRC      |-->| Number of points in input file
!| NPOIN_DEST     |-->| Number if points in output file
!| KNOLG          |-->| Local to global numbering
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
      INTEGER,          INTENT(IN)  :: SOURCE_ID
      INTEGER,          INTENT(IN)  :: DEST_ID
      INTEGER,          INTENT(IN)  :: NELEBD_DEST
      INTEGER,          INTENT(IN)  :: NDP_DEST
      INTEGER,       INTENT(IN)  :: IKLE_BND_DEST(NELEBD_DEST*NDP_DEST)
      INTEGER,          INTENT(IN)  :: TYP_BND_ELEM
      INTEGER,          INTENT(IN)  :: NELEBD_SRC
      INTEGER,          INTENT(IN)  :: KNOLG_BND(NELEBD_SRC)
      LOGICAL,          INTENT(IN)  :: TRANS_POINT
      INTEGER,          INTENT(IN)  :: NPOIN_SRC
      INTEGER,          INTENT(IN)  :: NPOIN_DEST
      INTEGER,          INTENT(IN)  :: KNOLG(NPOIN_DEST)
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          ! DOING nothing
          CONTINUE
        CASE ('MED    ')
          CALL TRANSFER_GROUP_PART_INFO_MED(
     &     SOURCE_ID, DEST_ID,
     &     TYP_BND_ELEM, IKLE_BND_DEST, NELEBD_DEST,
     &     NDP_DEST, NELEBD_SRC, KNOLG_BND,
     &     TRANS_POINT, NPOIN_SRC, NPOIN_DEST, KNOLG, IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*)
     &      'TRANSFER_GROUP_INFO: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
