!                   *******************
                    SUBROUTINE MXPTEL31
!                   *******************
!
     & (NELEM,NPOIN,MXELVS,IKLES,MXPTVS)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FOR TETRAHEDRA, GIVES THE MAXIMUM NUMBER OF NEIGHBOURS
!+                FOR A GIVEN MESH NODE.
!
!history  LAM MINH-PHUONG; F. DECUNG
!+        31/08/2009
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
!| IKLES          |-->| LIKE CONNECTIVITY TABLE BUT IN SELAFIN FORMAT
!|                |   | IKLES(4,NELEM) INSTEAD OF IKLE(NELEM,4)
!| MXELVS         |-->| MAXIMUM NUMBER OF NEIGHBOURING ELEMENTS
!| MXPTVS         |<--| MAXIMUM NUMBER OF NEIGHBOURS OF A POINT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MXPTEL31 => MXPTEL31
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)                       :: NELEM
      INTEGER, INTENT(IN)                       :: NPOIN
      INTEGER, INTENT(IN)                       :: MXELVS
      INTEGER, INTENT(IN), DIMENSION(4,NELEM)   :: IKLES
      INTEGER, INTENT(OUT)                      :: MXPTVS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                            :: IPOIN,I,J,K,IKLJ,IKL
      INTEGER                            :: NVOIS
      INTEGER,DIMENSION(:),ALLOCATABLE   :: VOIS
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: IND_ELEM
!
      ALLOCATE(VOIS(3*MXELVS))
      ALLOCATE(IND_ELEM(NPOIN,MXELVS+1))
!
!-----------------------------------------------------------------------
!
! IND_ELEM GIVES THE NUMBER OF ELEMENTS AROUND A NODE AND THEIR NUMBERS
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      DO I = 1, NPOIN
        IND_ELEM(I,1) = 0
      ENDDO
!
      DO J=1, 4
        DO I=1,NELEM
          IKL = IKLES(J,I)
          IND_ELEM(IKL,1)=IND_ELEM(IKL,1)+1
          IND_ELEM(IKL,IND_ELEM(IKL,1)+1)=I
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      MXPTVS = 0
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
! LOOP ON ALL THE NODES OF THE MESH      !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
      DO IPOIN = 1, NPOIN
!
        NVOIS   = 1
        VOIS(1) = IPOIN
!
!       INITIALISES VOIS TO 0
!
        DO I = 1, 3*MXELVS
          VOIS(I) = 0
        ENDDO
!
!       FILLS IN VOIS, WHICH CONTAINS THE NUMBERS OF ALL THE NODES
!       NEIGHBOURING IPOIN
!
        DO J = 1,4
          DO I = 2, IND_ELEM(IPOIN,1)+1
            IKLJ = IKLES(J,IND_ELEM(IPOIN,I))
            DO K = 1, NVOIS
              IF ( VOIS(K) == IKLJ ) EXIT
            ENDDO
            IF( K > NVOIS ) THEN
              NVOIS       = NVOIS + 1
              VOIS(NVOIS) = IKLJ
            ENDIF
          ENDDO
        ENDDO
!
        NVOIS = NVOIS - 1
        IF( MXPTVS < NVOIS) MXPTVS = NVOIS
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      DEALLOCATE (VOIS)
      DEALLOCATE (IND_ELEM)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
