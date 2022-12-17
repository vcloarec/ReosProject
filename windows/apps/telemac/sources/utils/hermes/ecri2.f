!                   ****************
                    SUBROUTINE ECRI2
!                   ****************
!
     &(X , I , C , NVAL , TYPE , CANAL , STD , ISTAT)
!
!***********************************************************************
! HERMES   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WRITES OUT VALUES ACCORDING TO VARIOUS STANDARDS.
!
!note     FORMER SUBROUTINE ECRIT;
!+         NAME CHANGED BECAUSE THIS NAME EXISTS IN THE CALCIUM LIBRARY
!
!
!history  J-M HERVOUET (LNH)
!+        17/08/94
!+        V5P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| X              |-->| DOUBLE PRECISION ARRAY TO BE WRITTEN
!| I              |-->| INTEGER ARRAY TO BE WRITTEN
!| C              |-->| CHARACTER STRING TO BE WRITTEN
!| NVAL           |-->| NUMBER OF VALUES (INTEGER, CHARACTER, ETC.)
!|                |   | TO BE WRITTEN
!| TYPE           |-->| TYPE OF DATA : 'I' , 'CH' , 'R4' , 'R8'
!| CANAL          |-->| LOGICAL UNIT FOR WRITING
!| STD            |-->| OUTPUT STANDARD : STD , IBM OU I3E, ETC.
!| ISTAT          |<--| ERROR NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NVAL,CANAL
      DOUBLE PRECISION, INTENT(IN) :: X(*)
      INTEGER, INTENT(IN) :: I(*)
      CHARACTER(LEN=*), INTENT(IN) :: TYPE,STD,C
      INTEGER, INTENT(OUT) :: ISTAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER J
!
      INTRINSIC REAL
!
!-----------------------------------------------------------------------
!
      ISTAT = 0
!
!-----------------------------------------------------------------------
!
      IF(STD(1:3).EQ.'STD') THEN
!
        IF (TYPE(1:2).EQ.'R4') THEN
          WRITE(CANAL)(REAL(X(J)),J=1,NVAL)
        ELSEIF (TYPE(1:2).EQ.'R8') THEN
          WRITE(CANAL)(X(J),J=1,NVAL)
        ELSEIF (TYPE(1:1).EQ.'I') THEN
          WRITE(CANAL)(I(J),J=1,NVAL)
        ELSEIF (TYPE(1:2).EQ.'CH') THEN
          WRITE(CANAL) C(1:NVAL)
        ELSE
          WRITE(LU,21) TYPE
21        FORMAT(1X,'ECRI2 : UNKNOWN TYPE:',A2)
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSE
!
        WRITE(LU,11) STD
11      FORMAT(1X,'ECRI2 : UNKNOWN STANDARD:',A8)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
