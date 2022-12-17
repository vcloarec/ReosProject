!                   *****************
                    SUBROUTINE PARCOM_COMP
!                   *****************
!
     &( X ,ERRX, ICOM , MESH)
!
!***********************************************************************
! BIEF   V6P2                                  24/02/2016
!***********************************************************************
!
!brief    COMPLEMENTS A VECTOR AND ERROR VECTOR AT THE INTERFACES BETWEEN
!+                SUB-DOMAINS.
!+
!+            X CAN BE A BLOCK OF VECTORS. IN THIS CASE, ALL THE
!+                VECTORS IN THE BLOCK ARE TREATED.
!
!note     IMPORTANT : FROM RELEASE 5.9 ON, IDENTICAL VALUES ARE
!+                     ENSURED AT INTERFACE POINTS SO THAT DIFFERENT
!+                     PROCESSORS WILL ALWAYS MAKE THE SAME DECISION
!+                     IN TESTS ON REAL NUMBERS.
!
!warning  IF THE VECTORS HAVE A SECOND DIMENSION, IT IS
!+            IGNORED FOR THE TIME BEING
!
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ICOM           |-->| COMMUNICATION MODE
!|                |   | = 1 : VALUE WITH MAXIMUM ABSOLUTE VALUE TAKEN
!|                |   | = 2 : CONTRIBUTIONS ADDED
!|                |   | = 3 : MAXIMUM CONTRIBUTION RETAINED
!|                |   | = 4 : MINIMUM CONTRIBUTION RETAINED
!| MESH           |-->| MESH STRUCTURE
!| X              |<->| VECTOR OR BLOCK OF VECTORS
!| ERRX           |<->| ERROR VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PARCOM_COMP => PARCOM_COMP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: ICOM
!
!     STRUCTURES: VECTORS OR BLOCKS
!
      TYPE(BIEF_MESH), INTENT(INOUT)   :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X
      DOUBLE PRECISION, INTENT(INOUT) :: ERRX(*)
!
!-----------------------------------------------------------------------
!
      TYPE(BIEF_OBJ), POINTER  :: X2,X3
      INTEGER NPOIN,NPLAN,IAN,NP11,NSEG
!
!***********************************************************************
!
!  OF NO USE IF A SUB-DOMAIN IS DISCONNECTED FROM THE OTHERS
!
      IF(NPTIR.EQ.0) RETURN
!
!-----------------------------------------------------------------------
!
      NPOIN = MESH%NPOIN
      NPLAN = 1
      IF(MESH%DIM1.EQ.3) THEN
        NPOIN = BIEF_NBPTS(11,MESH)
        NPLAN = MESH%NPOIN/NPOIN
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(X%TYPE.EQ.2) THEN
!
!     VECTOR STRUCTURE
!
      IAN = 1
      CALL PARCOM2_COMP(X%R,X%R,X%R,ERRX,NPOIN,NPLAN,ICOM,IAN
     & ,MESH)
!
      IF(X%ELM.EQ.13) THEN
        NP11=BIEF_NBPTS(11,MESH)
        NSEG=MESH%NSEG
        CALL PARCOM2_SEG(X%R(NP11+1:NP11+NSEG),
     &                   X%R(NP11+1:NP11+NSEG),
     &                   X%R(NP11+1:NP11+NSEG),
     &                   NSEG,1,ICOM,IAN,MESH,1,11)
      ENDIF
!
      ELSEIF(X%TYPE.EQ.4) THEN
!
!     BLOCK STRUCTURE
!
!     BEWARE: NUMBER LIMITED TO 3 |||||||||||||||||||||||||
      IAN = X%N
      IF(IAN.EQ.1) THEN
        X2 => X%ADR(1)%P
        X3 => X%ADR(1)%P
      ELSEIF(IAN.EQ.2) THEN
        X2 => X%ADR(2)%P
        X3 => X%ADR(2)%P
      ELSEIF(IAN.EQ.3) THEN
        X2 => X%ADR(2)%P
        X3 => X%ADR(3)%P
      ELSE
        WRITE(LU,*) 'PARCOM: NO MORE THAN 3 VECTORS'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      CALL PARCOM2_COMP(X%ADR(1)%P%R,X2%R,X3%R,ERRX,NPOIN
     & ,NPLAN,ICOM,IAN,MESH)
!
!     PROVISIONNALY 1 BY 1, COULD BE OPTIMISED
!
      IF(X%ADR(1)%P%ELM.EQ.13) THEN
        NP11=BIEF_NBPTS(11,MESH)
        NSEG=MESH%NSEG
        CALL PARCOM2_SEG(X%ADR(1)%P%R(NP11+1:NP11+NSEG),
     &                   X%ADR(1)%P%R(NP11+1:NP11+NSEG),
     &                   X%ADR(1)%P%R(NP11+1:NP11+NSEG),
!    *                   NSEG,1,ICOM,IAN,MESH)
     &                   NSEG,1,ICOM,1  ,MESH,1,11)
      ENDIF
      IF(IAN.GE.2.AND.X2%ELM.EQ.13) THEN
        NP11=BIEF_NBPTS(11,MESH)
        NSEG=MESH%NSEG
        CALL PARCOM2_SEG(X2%R(NP11+1:NP11+NSEG),
     &                   X2%R(NP11+1:NP11+NSEG),
     &                   X2%R(NP11+1:NP11+NSEG),
!    *                   NSEG,1,ICOM,IAN,MESH)
     &                   NSEG,1,ICOM,1  ,MESH,1,11)
      ENDIF
      IF(IAN.EQ.3.AND.X3%ELM.EQ.13) THEN
        NP11=BIEF_NBPTS(11,MESH)
        NSEG=MESH%NSEG
        CALL PARCOM2_SEG(X3%R(NP11+1:NP11+NSEG),
     &                   X3%R(NP11+1:NP11+NSEG),
     &                   X3%R(NP11+1:NP11+NSEG),
!    *                   NSEG,1,ICOM,IAN,MESH)
     &                   NSEG,1,ICOM,1  ,MESH,1,11)
      ENDIF
!
      ELSE
!
!     ERROR ON THE STRUCTURE
!
      WRITE(LU,53)
53    FORMAT(1X,'                CAS NON PREVU')
      WRITE(LU,54)
54    FORMAT(1X,'               UNEXPECTED CASE')
      CALL PLANTE(1)
      STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
