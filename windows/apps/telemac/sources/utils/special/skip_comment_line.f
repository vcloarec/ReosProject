!                   ****************************
                    SUBROUTINE SKIP_COMMENT_LINE
!                   ****************************
!
     &(IFIC, COMMENT, IERR)
!
!***********************************************************************
! SPECIAL
!***********************************************************************
!
!brief    READ an ascii file untile the line does not start with the
!         character comment given as argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IFIC    [in]  File id
!| COMMENT [in]  First character of a comment line
!| IERR    [out] Error id
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
        INTEGER, INTENT(IN) :: IFIC
        CHARACTER(LEN=1), INTENT(IN) :: COMMENT
        INTEGER, INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
        CHARACTER(LEN=200) :: LINE
!
        READ(IFIC,*,IOSTAT=IERR) LINE
        IF(IERR.NE.0) RETURN
!
        DO WHILE(LINE(1:1).EQ.COMMENT(1:1))
          READ(IFIC,*,IOSTAT=IERR) LINE
          IF(IERR.NE.0) RETURN
        ENDDO
        BACKSPACE(IFIC)
      END SUBROUTINE
