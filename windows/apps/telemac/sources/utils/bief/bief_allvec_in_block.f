!                   *******************************
                    SUBROUTINE BIEF_ALLVEC_IN_BLOCK
!                   *******************************
!
     &( BLO , N , NAT , NOMGEN , IELM , NDIM , STATUT , MESH )
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    ALLOCATES MEMORY FOR N VECTORS, WHICH WILL BE PART
!+                OF A GIVEN BLOCK.
!
!note     THIS MODIFICATION OF ALLVEC_IN_BLOCK ALLOWS ADDING A NUMBER
!+         OF IDENTICALLY NUMBERED VECTORS TO AN ALREADY EXISTING BLOCK
!+         WITHOUT DESTROYING THE PREVIOUS STRUCTURE.
!
!history  J-M HERVOUET (LNH)
!+        11/07/1995
!+        V5P1
!+   First version
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
!history  J-M HERVOUET (jubilado)
!+        07/09/2017
!+        V7P3
!+   Allowing several successive allocations of the same BIEF_OBJ.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BLO            |<->| BLOCK WHERE THE VECTORS WILL BE ALLOCATED
!| IELM           |-->| TYPE OF ELEMENT OF VECTORS, OR DIMENSION
!|                |   | (DEPENDING ON 'STATUT', SEE BELOW)
!| N              |-->| NUMBER OF VECTORS TO BE ALLOCATED
!| NAT            |<--| 1: REAL VECTOR   2:VECTOR OF INTEGERS
!| NDIM           |-->| SECOND DIMENSION OF VECTORS
!| NOMGEN         |-->| GENERIC NAME OF VECTORS
!|                |   | WILL BE COMPLETED WITH RANK
!| STATUT         |-->| VECTOR STATUS:
!|                |   | 0 : FREE VECTOR, IELM IS ITS DIMENSION
!|                |   | 1 : VECTOR DEFINED ON A MESH
!|                |   | IELM IS THEN THE ELEMENT TYPE
!|                |   | CHANGING DISCRETISATION FORBIDDEN
!|                |   | 2 : LIKE 1 BUT CHANGING DISCRETISATION ALLOWED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_ALLVEC_IN_BLOCK => BIEF_ALLVEC_IN_BLOCK
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: BLO
      INTEGER         , INTENT(IN)    :: IELM,NDIM,STATUT,NAT,N
      CHARACTER(LEN=6), INTENT(IN)    :: NOMGEN
      TYPE(BIEF_MESH) , INTENT(IN)    :: MESH
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
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(BLO%N+N.LE.BLO%MAXBLOCK) THEN
!
        IF(N.GT.0) THEN
!
          DO I = BLO%N+1 , BLO%N+N
!
!           NAME OF THE VECTOR
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
              WRITE(LU,*) 'MORE THAN 999 VECTORS ASKED
     &                     IN ALLVEC_IN_BLOCK'
              CALL PLANTE(1)
              STOP
            ENDIF
!
!           ALLOCATES THE VECTOR
!
            IF(.NOT.ASSOCIATED(BLO%ADR(I)%P)) THEN
              ALLOCATE(BLO%ADR(I)%P)
              NULLIFY(BLO%ADR(I)%P%R)
              NULLIFY(BLO%ADR(I)%P%I)
            ENDIF
            CALL BIEF_ALLVEC(NAT,BLO%ADR(I)%P,NOM,IELM,NDIM,STATUT,MESH)
            BLO%ADR(I)%P%FATHER = BLO%NAME
!
          ENDDO ! I
!
          BLO%N=BLO%N+N
!
        ENDIF
!
      ELSE
!
        WRITE(LU,*) 'BIEF_ALLVEC_IN_BLOCK:'
        WRITE(LU,*) 'MORE THAN ',BLO%MAXBLOCK,'(',N,')'
        WRITE(LU,*) 'VECTORS TO BE ALLOCATED'
        WRITE(LU,*) 'CHANGE MAXBLOCK IN ALLBLO.'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
