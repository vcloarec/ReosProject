!                   *****************
                    SUBROUTINE ALLBLO
!                   *****************
!
     &( BLO , NOM )
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    ALLOCATES MEMORY FOR A BLOCK STRUCTURE.
!
!history  J-M HERVOUET (LNH)
!+        10/01/95
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
!history  J-M HERVOUET (LNHE)
!+        09/08/2011
!+        V6P25
!+   Maximum size BLO%MAXBLOCK increased from 128 to 256
!+
!history  J-M HERVOUET (jubilado)
!+        04/11/2016
!+        V7P3
!+   Allowing several successive allocations of the same BIEF_OBJ.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BLO            |-->| THE BLOCK TO BE ALLOCATED
!| NOM            |-->| FORTRAN NAME OF THIS BLOCK
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ALLBLO => ALLBLO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: BLO
      CHARACTER(LEN=6), INTENT(IN)    :: NOM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ERR
!
!-----------------------------------------------------------------------
!  COMMON PART FOR ALL OBJECTS
!-----------------------------------------------------------------------
!
!     KEY OF THE OBJECT
!
      BLO%KEY = 123456
!
!     TYPE OF THE OBJECT
!
      BLO%TYPE = 4
!
!     Defines how the object was created
!
      BLO%FATHER = 'XXXXXX'
!
!     NAME OF THE OBJECT
!
      BLO%NAME = NOM
!
!-----------------------------------------------------------------------
!  PART SPECIFIC TO BLOCKS
!-----------------------------------------------------------------------
!
!     NUMBER OF OBJECTS IN THE BLOCK
!
      BLO%N = 0
!
!     ALLOCATES THE POINTERS ARRAY ADR
!
      BLO%MAXBLOCK = 256
      ERR=0
      IF(.NOT.ASSOCIATED(BLO%ADR)) THEN
        ALLOCATE(BLO%ADR(BLO%MAXBLOCK),STAT=ERR)
        DO I=1,BLO%MAXBLOCK
          NULLIFY(BLO%ADR(I)%P)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(ERR.NE.0) THEN
        WRITE(LU,20) NOM,ERR
20      FORMAT(1X,'ERROR DURING ALLOCATION OF BLOCK: ',A6,/,1X,
     &            'ERROR CODE: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
