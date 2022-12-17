!                   **************************
                    SUBROUTINE ALLBLO_IN_BLOCK
!                   **************************
!
     &( BLO , N , NOMGEN )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ALLOCATES MEMORY FOR N BLOCKS, WHICH WILL BE PART
!+                OF A GIVEN BLOCK.
!
!history  J-M HERVOUET (LNH)
!+        11/07/95
!+        V5P1
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
!| BLO            |<->| BLOCK WHERE TO ALLOCATE THE BLOCK STRUCTURES
!| N              |-->| NUMBER OF BLOCKS TO BE ADDED IN BLO
!| NOMGEN         |-->| GENERIC FORTRAN NAME OF THE BLOCKS
!|                |   | IT WILL BE COMPLETED WITH THEIR RANK
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ALLBLO_IN_BLOCK => ALLBLO_IN_BLOCK
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: BLO
      INTEGER         , INTENT(IN)    :: N
      CHARACTER(LEN=6), INTENT(IN)    :: NOMGEN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IDEB,I,II
!
      CHARACTER(LEN=6) :: NOM
      CHARACTER(LEN=1), PARAMETER :: CHIFFRE(0:9) =
     &            (/'0','1','2','3','4','5','6','7','8','9'/)
!
!-----------------------------------------------------------------------
!
      IDEB = 6
      DO I=5,2,-1
        IF(NOMGEN(I:I).EQ.' ') IDEB = I
      ENDDO ! I
!
!-----------------------------------------------------------------------
!
      IF(N.LE.BLO%MAXBLOCK) THEN
!
      DO I = 1 , N
!
!       NAME OF THE BLOCK
!
        NOM=NOMGEN
        IF(I.LT.10) THEN
          IDEB = MIN(6,IDEB)
          NOM(IDEB:IDEB) = CHIFFRE(I)
        ELSEIF(I.LT.100) THEN
          IDEB = MIN(5,IDEB)
          NOM(IDEB  :IDEB  ) = CHIFFRE(I/10)
          NOM(IDEB+1:IDEB+1) = CHIFFRE(I-10*(I/10))
        ELSEIF(I.LT.1000) THEN
          IDEB = MIN(4,IDEB)
          NOM(IDEB  :IDEB  ) = CHIFFRE(I/100)
          II=I-100*(I/100)
          NOM(IDEB+1:IDEB+1) = CHIFFRE(II/10)
          NOM(IDEB+2:IDEB+2) = CHIFFRE(II-10*(II/10))
        ELSE
          WRITE(LU,*) 'TOO MANY BLOCKS IN ALLBLO_IN_BLOCK'
          CALL PLANTE(1)
          STOP
        ENDIF
!
!       ALLOCATES THE BLOCK IF NOT ALREADY DONE
!
        CALL FIRST_ALL_BIEFOBJ(BLO%ADR(I)%P)
        CALL ALLBLO(BLO%ADR(I)%P,NOM)
        BLO%N=BLO%N+1
        BLO%ADR(I)%P%FATHER = BLO%NAME
!
      ENDDO
!
      ELSE
!
        WRITE(LU,*) 'ALLBLO_IN_BLOCK : MORE THAN '
        WRITE(LU,*) '                 ',BLO%MAXBLOCK,' (',N,')'
        WRITE(LU,*) '                  BLOCKS TO BE ALLOCATED'
        WRITE(LU,*) '                  CHANGE MAXBLOCK IN ALLBLO'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
