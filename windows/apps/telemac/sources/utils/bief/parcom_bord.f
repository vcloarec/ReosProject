!                   **********************
                    SUBROUTINE PARCOM_BORD
!                   **********************
!
     &( X , ICOM , MESH )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPLEMENTS A VECTOR AT THE INTERFACES BETWEEN
!+                SUB-DOMAINS. HERE BOUNDARY VECTOR OF TYPE 1.
!
!history  J-M HERVOUET (LNHE)
!+        24/10/2008
!+        V5P9
!+   First version, inspired from parcom.
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
!| ICOM           |-->| COMMUNICATION MODE
!|                |   | = 1 : VALUE WITH MAXIMUM ABSOLUTE VALUE TAKEN
!|                |   | = 2 : CONTRIBUTIONS ADDED
!|                |   | = 3 : MAXIMUM CONTRIBUTION RETAINED
!|                |   | = 4 : MINIMUM CONTRIBUTION RETAINED
!| MESH           |-->| MESH STRUCTURE
!| X              |<->| VECTOR OR BLOCK OF VECTORS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PARCOM_BORD => PARCOM_BORD
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: ICOM
!
!     STRUCTURES: VECTORS OR BLOCKS
!
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NPTFR,I,TELM,TDIM1,TDIM2,TDIMDISC
!
!***********************************************************************
!
      NPTFR=BIEF_NBPTS(1,MESH)
!
      TELM     = MESH%T%ELM
      TDIM1    = MESH%T%DIM1
      TDIM2    = MESH%T%DIM2
      TDIMDISC = MESH%T%DIMDISC
!
      MESH%T%ELM     = 11
      MESH%T%DIM1    = BIEF_NBPTS(11,MESH)
      MESH%T%DIM2    = 1
      MESH%T%DIMDISC = 0
!
      CALL OS('X=0     ',X=MESH%T)
!
      DO I=1,NPTFR
        MESH%T%R(MESH%NBOR%I(I))=X(I)
      ENDDO
!
      CALL PARCOM(MESH%T,ICOM,MESH)
!
      DO I=1,NPTFR
        X(I)=MESH%T%R(MESH%NBOR%I(I))
      ENDDO
!
      MESH%T%ELM     = TELM
      MESH%T%DIM1    = TDIM1
      MESH%T%DIM2    = TDIM2
      MESH%T%DIMDISC = TDIMDISC
!
!-----------------------------------------------------------------------
!
      RETURN
      END
