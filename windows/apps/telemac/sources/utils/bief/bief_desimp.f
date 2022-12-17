!                   **********************
                    SUBROUTINE BIEF_DESIMP
!                   **********************
!
     &(FORMAT_RES,VARSOR,N,NRES,AT,LT,LISPRD,LEOPRD,
     & SORLEO,SORIMP,MAXVAR,TEXTE,PTINIG,PTINIL,MESH,
     & IIMP,ILEO,COMPGRAPH)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    WRITES TO RESULT OR LISTING FILE.
!
!history  J-M HERVOUET (LNHE)
!+        01/04/2009
!+        V6P0
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
!history  V. STOBIAC
!+        10/12/2014
!+        V7P0
!+   Add update of mesh coordinates for moving mesh
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!history  R. ATA (LNHE)
!+       11/01/2015
!+       V7P2
!+       adaptation for fv (leo, imp and compgraph added as
!+        optional arguments)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FORMAT_RES     |-->| FORMAT OF RESULT FILE
!| LEOPRD         |-->| GRAPHIC PRINTOUT PERIOD
!| LISPRD         |-->| LISTING PRINTOUT PERIOD
!| MAXVAR         |-->| MAXIMUM OF VARIABLES IN THE FILE
!| N              |-->| NUMBER OF POINTS IN THE MESH
!| NRES           |-->| LOGICAL UNIT OF THE RESULTS FILE
!| PTINIG         |-->| NUMBER OF FIRST TIME STEP FOR GRAPHIC PRINTOUTS
!| PTINIL         |-->| NUMBER OF FIRST TIME STEP FOR LISTING PRINTOUTS
!| SORIMP         |-->| ARRAY OF LOGICAL SAYING IF VARIABLES MUST BE PUT
!|                |   | IN THE LISTING
!| SORLEO         |-->| ARRAY OF LOGICAL SAYING IF VARIABLES MUST BE PUT
!|                |   | IN THE RESULTS FILE
!| TEXTE          |-->| NAMES AND UNITS OF VARIABLES
!| VARSOR         |-->| BLOCK WITH VARIABLES TO BE PRINTED OR COPIED
!| MESH (OPTIONAL)|-->| MESH STRUCTURE
!| ILEO & IIMP)   |-->| LOGICAL FOR LISTING AND GRAPHICAL OUTPUTS
!| COMPGRAPH      |-->| COUNTER FOR FV GRAPHICAL OUTPUTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_DESIMP => BIEF_DESIMP
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)   , INTENT(IN) :: VARSOR
      CHARACTER(LEN=8) , INTENT(IN) :: FORMAT_RES
      INTEGER          , INTENT(IN) :: NRES,LT,LISPRD,LEOPRD
      INTEGER          , INTENT(IN) :: PTINIG,PTINIL,N
      INTEGER          , INTENT(IN) :: MAXVAR
      DOUBLE PRECISION , INTENT(IN) :: AT
      CHARACTER(LEN=32), INTENT(IN) :: TEXTE(*)
      LOGICAL          , INTENT(IN) :: SORLEO(MAXVAR),SORIMP(MAXVAR)
      TYPE(BIEF_MESH)  , INTENT(IN), OPTIONAL :: MESH
      LOGICAL          , INTENT(IN), OPTIONAL :: IIMP,ILEO
      INTEGER          , INTENT(IN), OPTIONAL :: COMPGRAPH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER LTT,K
!
      LOGICAL LEO,IMP
!
!-----------------------------------------------------------------------
!
! LOGICAL THAT DEFINE THE OUTPUTS
!
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LT/LISPRD)*LISPRD
      IF(LT.EQ.LTT.AND.LT.GE.PTINIL) IMP=.TRUE.
      LTT=(LT/LEOPRD)*LEOPRD
      IF(LT.EQ.LTT.AND.LT.GE.PTINIG) LEO=.TRUE.
      IF(LT.EQ.0) LEO=.TRUE.
      IF(PRESENT(IIMP))IMP=IIMP
      IF(PRESENT(ILEO))LEO=ILEO
!
!-----------------------------------------------------------------------
!
      IF(LEO) THEN
!
!       COUNTERS FOR TELEMAC2D
        IF(PRESENT(COMPGRAPH))THEN
          LTT=COMPGRAPH
        ELSE
!       COUNTERS FOR OTHER CODES
          LTT = (LT-PTINIG)/LEOPRD
        ! In case the starting point is not at lt.eq.0 but later
        !  we still write the timestep 0 so we need to increment LTT
          IF(PTINIG.NE.0) LTT = LTT + 1
          IF(LT.EQ.0) LTT = 0
        ENDIF
!
        IF(PRESENT(MESH)) THEN
          CALL WRITE_DATA(FORMAT_RES,NRES,MAXVAR,AT,LTT,SORLEO,
     &                    TEXTE,VARSOR,N,MESH)
        ELSE
          CALL WRITE_DATA(FORMAT_RES,NRES,MAXVAR,AT,LTT,SORLEO,
     &                    TEXTE,VARSOR,N)
        ENDIF
      ENDIF
!
! TO LISTING FILE
!
      IF(IMP) THEN
        DO K=1,MAXVAR
          IF(SORIMP(K)) THEN
            IF(ASSOCIATED(VARSOR%ADR(K)%P%R)) THEN
              CALL IMPVEC(VARSOR%ADR(K)%P%R,TEXTE(K),N)
            ELSE
              WRITE(LU,*) 'DESIMP: VARIABLE NUMBER: ',K
              WRITE(LU,*) '        NOT OR NOT WELL ALLOCATED'
              WRITE(LU,*) '        OR POINTER NOT ASSOCIATED '
!             CALL PLANTE(1)
!             STOP
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!=======================================================================
!
      RETURN
      END
