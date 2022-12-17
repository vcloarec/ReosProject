!                   ************************
                    SUBROUTINE FRICTION_SCAN
!                   ************************
!
     &(NCOF,NOMCOF,TYP,LINE)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS FRICTION FILE.
!
!history  F. HUVELIN
!+        20/04/2004
!+
!+
!
!history  J-M HERVOUET (LNHE)
!+
!+        V5P5
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LINE           |<--| LINE READ IN THE FRICTION FILE
!| NCOF           |-->| LOGICAL UNIT OF FRICTION FILE
!| NOMCOF         |-->| NAME OF FRICTION FILE
!| TYP            |<--| 2: STARTING LINE
!|                |   | 3: ENDING LINE
!|                |   | 1: CURRENT LINE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,       INTENT(IN)    :: NCOF
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: NOMCOF
      INTEGER,       INTENT(INOUT) :: LINE
      INTEGER,       INTENT(OUT)   :: TYP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=20)                 :: C
!
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
      DO
        READ(NCOF,*,END=989,ERR=988) C
        LINE = LINE + 1
        IF (C(1:1).NE.'*') EXIT
      ENDDO
!
      CALL MAJUS(C)
!
      IF(C(1:4).EQ.'FROM'.OR.C(1:3).EQ.'VON'.OR.C(1:2).EQ.'DE') THEN
        TYP = 2
      ELSEIF(C(1:3).EQ.'END'.OR.C(1:3).EQ.'FIN') THEN
        TYP = 3
      ELSE
        TYP = 1
      ENDIF
!
      BACKSPACE(NCOF)
!
      GOTO 987
!
! -------------------------------------------------------------- !
!                         WARNING MESSAGE                        !
! -------------------------------------------------------------- !
!
989   CONTINUE
      WRITE(LU,*) 'FRICTION DATA FILE : ',NOMCOF
      WRITE(LU,*) 'ABNORMAL END OF FILE'
      CALL PLANTE(1)
      STOP
!
988   CONTINUE
      WRITE(LU,*) 'FRICTION DATA FILE : ',NOMCOF
      WRITE(LU,*) 'READ ERROR'
      WRITE(LU,*) 'ERROR WHEN READING: ',C
      CALL PLANTE(1)
      STOP
!
! -------------------------------------------------------------- !
! -------------------------------------------------------------- !
!
987   CONTINUE
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END
