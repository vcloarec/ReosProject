!                 ************************
                  MODULE HERMES_INDEX_FILE
!                 ************************
!
!***********************************************************************
! HERMES                                                            2018
!***********************************************************************
!
!brief    Implementation of the index files use by concatenated serafin
!         and MED
!
!history YOANN AUDOUIN
!+       10/05/2018
!+       Initial version
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTERFACE READ_INDEX
        MODULE PROCEDURE
     &    READ_INDEX32,
     &    READ_INDEX64
      END INTERFACE READ_INDEX
      INTERFACE WRITE_INDEX
        MODULE PROCEDURE
     &    WRITE_INDEX32,
     &    WRITE_INDEX64
      END INTERFACE WRITE_INDEX
!
      CONTAINS

!***********************************************************************
      SUBROUTINE OPEN_INDEX
!***********************************************************************
!
     &(FILENAME, FILE_ID)
!
!***********************************************************************
! HERMES                                                      10/05/2018
!***********************************************************************
!
!BRIEF    Open an index file, either for writing or reading and return
!         the file id.
!
!HISTORY  Judicaël Grasset (Daresbury Lab & EDF)
!+        10/05/2018
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILENAME       |<--| NAME OF THE FILE ASSOCIATED TO THE INDEX
!| FILE_ID        |<--| FILE DESCRIPTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        USE DECLARATIONS_SPECIAL
        CHARACTER(LEN=*), INTENT(IN)  :: FILENAME
        INTEGER,          INTENT(OUT) :: FILE_ID
        !
        CHARACTER(LEN=MAXLENFILE) :: INDEX_FILENAME
        INTEGER :: FILENAME_SIZE,IERR
        LOGICAL :: ISOPEN
        CALL GET_FREE_ID(FILE_ID)
        !IF THE FILE IS CONCATENATED, THEN THE INDEX FILE IS IN A
        !FILE OF THE SAME NAME WITH THE 'CONCAT' SUFFIX REPLACED
        !BY 'INDEX'
        INDEX_FILENAME=FILENAME
        FILENAME_SIZE=LEN_TRIM(INDEX_FILENAME)
        INDEX_FILENAME(FILENAME_SIZE-5:FILENAME_SIZE) = 'INDEX '

        INQUIRE(FILE=INDEX_FILENAME,OPENED=ISOPEN)
        IF(ISOPEN)THEN
          INQUIRE(FILE=INDEX_FILENAME,NUMBER=FILE_ID)
        ELSE
          OPEN(UNIT=FILE_ID,FILE=TRIM(INDEX_FILENAME),ACTION='READWRITE'
     &         ,FORM='UNFORMATTED', ACCESS='STREAM',IOSTAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(INDEX_FILENAME)//': '//
     &           'OPEN_INDEX_FILE:OPEN'
            RETURN
          ENDIF
        ENDIF
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE READ_INDEX64
!***********************************************************************
!
     &(IDX_ID,OPENMODE,MESH_NUMBER,OFFSET_BEGIN,OFFSET_END)
!
!***********************************************************************
! HERMES                                                      10/05/2018
!***********************************************************************
!
!BRIEF    Read offsets from the index file. If openmode is read then we
!         read the offset needed for reading our part. If openmode is
!         write we read the offset needed to write after our part.
!
!HISTORY  Judicaël Grasset (Daresbury Lab & EDF)
!+        10/05/2018
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IDX_ID       |-->| FILE ID OF THE INDEX FILE
!| OPENMODE     |-->| OPENMODE OF THE FILE ASSOCIATED TO THE INDEX
!| MESH_NUMBER  |-->| NUMBER OF THE MESH WE ARE WORKING WITH
!| OFFSET_BEGIN |<--| OFFSET TO THE BEGINING OF OUR PART
!| OFFSET_END   |<--| OFFSET TO THE END OF OUR PART
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        USE DECLARATIONS_SPECIAL
        INTEGER,          INTENT(IN)  :: IDX_ID
        CHARACTER(LEN=9), INTENT(IN)  :: OPENMODE
        INTEGER,          INTENT(IN)  :: MESH_NUMBER
        INTEGER(KIND=K8), INTENT(OUT) :: OFFSET_BEGIN
        INTEGER(KIND=K8), INTENT(OUT) :: OFFSET_END

        INTEGER(KIND=K8) :: POS
        !GET THE OFFSET OF OUR PART.
        !THE OFFSETS ARE PAIRS OF 64 BITS INTEGER (8 BYTES * 2)
        !IF WE ARE IN READ MODE, THEN WE CAN READ OUR BEGINING OFFSET
        !AND OUR END OFFSET.
        !IF WE ARE IN WRITE MODE, WE CAN ONLY READ OUR BEGINING OFFSET
        !SINCE WE DO NOT KNOW WHERE WE WILL STOP WRITING.
        IF(OPENMODE(1:5)=='WRITE')THEN
          IF(MESH_NUMBER==1)THEN
            OFFSET_BEGIN=1
          ELSE
            POS=(MESH_NUMBER-2)*8*2+1
            READ(IDX_ID,POS=POS)OFFSET_BEGIN,OFFSET_END
            OFFSET_BEGIN=OFFSET_END
          ENDIF
          OFFSET_END=-1
        ELSE IF(OPENMODE(1:4)=='READ')THEN
          POS=(MESH_NUMBER-1)*8*2+1
          READ(IDX_ID,POS=POS)OFFSET_BEGIN,OFFSET_END
        ENDIF
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE WRITE_INDEX64
!***********************************************************************
!
     &(IDX_ID,MESH_NUMBER,OFFSET_BEGIN,OFFSET_END)
!
!***********************************************************************
! HERMES                                                      10/05/2018
!***********************************************************************
!
!BRIEF    Write the offset of our part into the index file
!
!HISTORY  Judicaël Grasset (Daresbury Lab & EDF)
!+        10/05/2018
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IDX_ID       |-->| FILE ID OF THE INDEX FILE
!| MESH_NUMBER  |-->| NUMBER OF THE MESH WE ARE WORKING WITH
!| OFFSET_BEGIN |-->| OFFSET TO THE BEGINING OF OUR PART
!| OFFSET_END   |-->| OFFSET TO THE END OF OUR PART
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        USE DECLARATIONS_SPECIAL
        INTEGER,          INTENT(IN) :: IDX_ID
        INTEGER,          INTENT(IN) :: MESH_NUMBER
        INTEGER(KIND=K8), INTENT(IN) :: OFFSET_BEGIN
        INTEGER(KIND=K8), INTENT(IN) :: OFFSET_END

        INTEGER(KIND=K8) :: POS
        POS=(MESH_NUMBER-1)*8*2+1
        WRITE(UNIT=IDX_ID,POS=POS)OFFSET_BEGIN,OFFSET_END
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE READ_INDEX32
!***********************************************************************
!
     &(IDX_ID,OPENMODE,MESH_NUMBER,OFFSET_BEGIN,OFFSET_END)
!
!***********************************************************************
! HERMES                                                      10/05/2018
!***********************************************************************
!
!BRIEF    Read offsets from the index file. If openmode is read then we
!         read the offset needed for reading our part. If openmode is
!         write we read the offset needed to write after our part.
!
!HISTORY  Judicaël Grasset (Daresbury Lab & EDF)
!+        10/05/2018
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IDX_ID       |-->| FILE ID OF THE INDEX FILE
!| OPENMODE     |-->| OPENMODE OF THE FILE ASSOCIATED TO THE INDEX
!| MESH_NUMBER  |-->| NUMBER OF THE MESH WE ARE WORKING WITH
!| OFFSET_BEGIN |<--| OFFSET TO THE BEGINING OF OUR PART
!| OFFSET_END   |<--| OFFSET TO THE END OF OUR PART
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        USE DECLARATIONS_SPECIAL
        INTEGER,          INTENT(IN)  :: IDX_ID
        CHARACTER(LEN=9), INTENT(IN)  :: OPENMODE
        INTEGER,          INTENT(IN)  :: MESH_NUMBER
        INTEGER,          INTENT(OUT) :: OFFSET_BEGIN
        INTEGER,          INTENT(OUT) :: OFFSET_END

        INTEGER(KIND=K8) :: POS
        !GET THE OFFSET OF OUR PART.
        !THE OFFSETS ARE PAIRS OF 32 BITS INTEGER (4 BYTES * 2)
        !IF WE ARE IN READ MODE, THEN WE CAN READ OUR BEGINING OFFSET
        !AND OUR END OFFSET.
        !IF WE ARE IN WRITE MODE, WE CAN ONLY READ OUR BEGINING OFFSET
        !SINCE WE DO NOT KNOW WHERE WE WILL STOP WRITING.
        IF(OPENMODE(1:5)=='WRITE')THEN
          IF(MESH_NUMBER==1)THEN
            OFFSET_BEGIN=1
          ELSE
            POS=(MESH_NUMBER-2)*4*2+1
            READ(IDX_ID,POS=POS)OFFSET_BEGIN,OFFSET_END
            OFFSET_BEGIN=OFFSET_END
          ENDIF
          OFFSET_END=-1
        ELSE IF(OPENMODE(1:4)=='READ')THEN
          POS=(MESH_NUMBER-1)*4*2+1
          READ(IDX_ID,POS=POS)OFFSET_BEGIN,OFFSET_END
        ENDIF
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE WRITE_INDEX32
!***********************************************************************
!
     &(IDX_ID,MESH_NUMBER,OFFSET_BEGIN,OFFSET_END)
!
!***********************************************************************
! HERMES                                                      10/05/2018
!***********************************************************************
!
!BRIEF    Write the offset of our part into the index file
!
!HISTORY  Judicaël Grasset (Daresbury Lab & EDF)
!+        10/05/2018
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IDX_ID       |-->| FILE ID OF THE INDEX FILE
!| MESH_NUMBER  |-->| NUMBER OF THE MESH WE ARE WORKING WITH
!| OFFSET_BEGIN |-->| OFFSET TO THE BEGINING OF OUR PART
!| OFFSET_END   |-->| OFFSET TO THE END OF OUR PART
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        USE DECLARATIONS_SPECIAL
        INTEGER, INTENT(IN) :: IDX_ID
        INTEGER, INTENT(IN) :: MESH_NUMBER
        INTEGER, INTENT(IN) :: OFFSET_BEGIN
        INTEGER, INTENT(IN) :: OFFSET_END

        INTEGER(KIND=K8) :: POS
        POS=(MESH_NUMBER-1)*4*2+1
        WRITE(UNIT=IDX_ID,POS=POS)OFFSET_BEGIN,OFFSET_END
      END SUBROUTINE
!
      END MODULE HERMES_INDEX_FILE
