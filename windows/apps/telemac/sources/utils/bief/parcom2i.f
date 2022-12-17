!                   *******************
                    SUBROUTINE PARCOM2I
!                   *******************
!
     &( X1 , X2 , X3 , NPOIN , NPLAN , ICOM , IAN , MESH )
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPLEMENTS A VECTOR OF INTEGERS AT THE INTERFACES BETWEEN
!+                SUB-DOMAINS.
!+
!+            X CAN BE A BLOCK OF VECTORS. IN THIS CASE, ALL THE
!+                VECTORS IN THE BLOCK ARE TREATED.
!
!warning  IF THE VECTORS HAVE A SECOND DIMENSION, IT IS
!+            IGNORED FOR THE TIME BEING
!
!history  J-M HERVOUET (LNHE)
!+        19/11/2013
!+        V7P0
!+   Inspired from PARCOM2.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IAN            |-->| NUMBER OF VECTORS TO BE CONDIDERED (1, 2 OR 3)
!| ICOM           |-->| COMMUNICATION MODE
!|                |   | = 1 : VALUE WITH MAXIMUM ABSOLUTE VALUE TAKEN
!|                |   | = 2 : CONTRIBUTIONS ADDED
!|                |   | = 3 : MAXIMUM CONTRIBUTION RETAINED
!|                |   | = 4 : MINIMUM CONTRIBUTION RETAINED
!| MESH           |-->| MESH STRUCTURE
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN          |-->| NUMBER OF POINTS
!| X1             |<->| INTEGER VECTOR TO BE COMPLETED
!| X2             |<->| INTEGER VECTOR TO BE COMPLETED
!| X3             |<->| INTEGER VECTOR TO BE COMPLETED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PARCOM2I => PARCOM2I
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: ICOM,NPOIN,NPLAN,IAN
!
!     STRUCTURES: VECTORS OR BLOCKS
!
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      INTEGER, INTENT(INOUT) :: X1(NPOIN,NPLAN)
      INTEGER, INTENT(INOUT) :: X2(NPOIN,NPLAN)
      INTEGER, INTENT(INOUT) :: X3(NPOIN,NPLAN)
!
!-----------------------------------------------------------------------
!
      CALL PARACOI(X1,X2,X3,NPOIN,ICOM,IAN,NPLAN,
     &             MESH%NB_NEIGHB,MESH%NB_NEIGHB_PT%I,MESH%LIST_SEND%I,
     &             MESH%NH_COM%I,MESH%NH_COM%DIM1,MESH%BUF_SEND%I,
     &             MESH%BUF_RECV%I,MESH%BUF_SEND%DIM1)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
