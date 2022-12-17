!                   *************************
                    SUBROUTINE WRITE_HEADER
!                   *************************
!
     &(FFORMAT,NRES,TITLE,NVAR,NOMVAR,OUTVAR)
!
!***********************************************************************
! HERMES   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    Write the header of the result file
!
!history  Y.audouin (LNHE)
!+        05/02/04
!+        V7P0
!+        Creation of the file
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FILE FORMAT
!| NRES           |-->| LOGICAL UNIT OF FILE
!| TITLE          |-->| TITLE OF FILE
!| NVAR           |-->| TOTAL NUMBER OF VARIABLES
!| NOMVAR         |-->| NAME OF VARIABLES
!| OUTVAR         |-->| VARIABLES TO BE PUT IN THE FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8)                 , INTENT(IN) :: FFORMAT
      INTEGER                          , INTENT(IN) :: NRES
      CHARACTER(LEN=72)                , INTENT(IN) :: TITLE
      INTEGER                          , INTENT(IN) :: NVAR
      CHARACTER(LEN=32),DIMENSION(NVAR), INTENT(IN) :: NOMVAR
      LOGICAL          ,DIMENSION(NVAR), INTENT(IN) :: OUTVAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=80) :: F_TITLE
      CHARACTER(LEN=32), ALLOCATABLE :: VAR_NAME(:)
      INTEGER F_NVAR, IERR, I
!
      ! COUTING THE REAL NUMBER OF VARIABLE
      F_NVAR = 0
      DO I=1,NVAR
        IF(OUTVAR(I)) F_NVAR = F_NVAR + 1
      ENDDO
      ! BUILD THE ARRAY CONTAING ONLY THE OUTPUT VARIABLE
      ALLOCATE(VAR_NAME(F_NVAR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'VAR_NAME')
      F_NVAR = 0
      DO I=1,NVAR
        IF(OUTVAR(I)) THEN
          F_NVAR = F_NVAR + 1
          VAR_NAME(F_NVAR)(1:32) = NOMVAR(I)(1:32)
        ENDIF
      ENDDO
!
      ! ADD THE FORMAT TO THE TITLE
      F_TITLE = TITLE(1:72)//FFORMAT
      CALL SET_HEADER(FFORMAT,NRES,F_TITLE,F_NVAR,VAR_NAME,IERR)
      CALL CHECK_CALL(IERR,'WRITE_HEADER:SET_HEADER')
!
      DEALLOCATE(VAR_NAME)
!
      RETURN
      END
