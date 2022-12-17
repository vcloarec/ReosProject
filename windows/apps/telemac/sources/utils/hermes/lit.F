!                   **************
                    SUBROUTINE LIT
!                   **************
!
     &( X , W , I , C , NVAL , TYPE , CANAL , STD2 , ISTAT )
!
!***********************************************************************
! HERMES   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS VALUES ACCORDING TO VARIOUS STANDARDS.
!
!warning  IF THE CHARACTER STRING STD EQUALS IBM OR I3E, CALLS THE
!+            SUBROUTINES LECIBM OR LECI3E, WHICH DEPEND ON THE TYPE
!+            OF MACHINE USED
!
!history  J-M HERVOUET (LNHE)
!+        01/04/2009
!+        V6P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| X              |-->| DOUBLE PRECISION ARRAY TO BE READ
!| W              |<--| REAL WORK ARRAY (IN CASE OF
!|                |   | CONVERSION FROM SIMPLE TO DOUBLE PRECISION)
!| I              |-->| INTEGER ARRAY TO BE READ
!| C              |<--| CHARACTER STRING TO BE READ
!| NVAL           |-->| NUMBER OF VALUES (INTEGER, CHARACTER, ETC.)
!|                |   | TO BE READ
!| TYPE           |-->| TYPE OF DATA : 'I' , 'CH' , 'R4' , 'R8'
!| CANAL          |-->| LOGICAL UNIT FOR READING
!| STD2           |-->| INPUT STANDARD : STD , IBM OU I3E, ETC.
!| ISTAT          |<--| ERROR NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NVAL,CANAL
      INTEGER, INTENT(INOUT)          :: ISTAT
      CHARACTER(LEN=*), INTENT(IN)       :: TYPE,STD2
      INTEGER, INTENT(INOUT)          :: I(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      REAL, INTENT(INOUT)             :: W(*)
      CHARACTER(LEN=*), INTENT(INOUT)    :: C
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER J
      CHARACTER(LEN=8) STD
!
      INTRINSIC DBLE,MIN,LEN
!
!-----------------------------------------------------------------------
!
      ISTAT = 0
!
!-----------------------------------------------------------------------
!
!     STD2 MAY BE SHORTER THAN 8 CHARACTERS
      STD='        '
      STD(1:MIN(8,LEN(STD2)))=STD2(1:MIN(8,LEN(STD2)))
!
!-----------------------------------------------------------------------
!
      IF(STD(1:3).EQ.'STD'.OR.STD(1:7).EQ.'SERAFIN') THEN
!
        IF(TYPE(1:2).EQ.'R4') THEN
          READ(CANAL,END=100,ERR=101)(W(J),J=1,NVAL)
          DO J=1,NVAL
            X(J) = DBLE(W(J))
          ENDDO
        ELSEIF(TYPE(1:2).EQ.'R8') THEN
          READ(CANAL,END=100,ERR=101)(X(J),J=1,NVAL)
        ELSEIF (TYPE(1:1).EQ.'I') THEN
          READ(CANAL,END=100,ERR=101)(I(J),J=1,NVAL)
        ELSEIF(TYPE(1:2).EQ.'CH') THEN
          READ(CANAL,END=100,ERR=101) C(1:NVAL)
        ELSE
          WRITE(LU,21) TYPE
21        FORMAT(1X,'LIT : UNKNOWN TYPE :',A2)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        GO TO 102
!
100     CONTINUE
        WRITE(LU,'(1X,A)')       'LIT : ABNORMAL END OF FILE'
        WRITE(LU,'(1X,A)')       'ONE INTENDED TO READ'
        WRITE(LU,'(1X,A,1I6,A)') 'A RECORD OF ',NVAL,' VALUES'
        WRITE(LU,'(1X,A,A)')     'OF TYPE : ',TYPE
        WRITE(LU,'(1X,A,1I6)')   'ON LOGICAL UNIT : ',CANAL
!       ISTAT = -6
        CALL PLANTE(1)
        STOP
!
101     CONTINUE
        WRITE(LU,'(1X,A)')       'LIT : READ ERROR'
        WRITE(LU,'(1X,A)')       'ONE INTENDED TO READ'
        WRITE(LU,'(1X,A,1I6,A)') 'A RECORD OF ',NVAL,' VALUES'
        WRITE(LU,'(1X,A,A)')     'OF TYPE : ',TYPE
        WRITE(LU,'(1X,A,1I6)')   'ON LOGICAL UNIT : ',CANAL
!       ISTAT = -6
        CALL PLANTE(1)
        STOP
!
102     CONTINUE
!
      ELSE
!
        WRITE(LU,11) STD
11      FORMAT(1X,'LIT : UNKNOWN STANDARD:',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
