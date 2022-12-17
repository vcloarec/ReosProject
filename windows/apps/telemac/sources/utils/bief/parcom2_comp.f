!                   ***********************
                    SUBROUTINE PARCOM2_COMP
!                   ***********************
!
     &( X1 , X2 , X3 ,ERRX, NPOIN , NPLAN , ICOM , IAN , MESH )
!
!***********************************************************************
! BIEF   V6P1                                   24/02/2016
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
!| X1             |<->| VECTOR TO BE COMPLETED
!| X2             |<->| VECTOR TO BE COMPLETED
!| X3             |<->| VECTOR TO BE COMPLETED
!| ERRX           |<->| ERRORS VECTOR TO BE COMPLETED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PARCOM2_COMP => PARCOM2_COMP
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
      DOUBLE PRECISION, INTENT(INOUT) :: X1(NPOIN,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: X2(NPOIN,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: X3(NPOIN,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: ERRX(*)
!
!-----------------------------------------------------------------------
!
      CALL PARACO_COMP(X1,X2,X3,ERRX,NPOIN,ICOM,IAN,NPLAN,
     &            MESH%NB_NEIGHB,MESH%NB_NEIGHB_PT%I,MESH%LIST_SEND%I,
     &            MESH%NH_COM%I,MESH%NH_COM%DIM1,MESH%BUF_SEND%R,
     &            MESH%BUF_RECV%R,MESH%BUF_SEND%DIM1,
     &            MESH%BUF_SEND_ERR%R,MESH%BUF_RECV_ERR%R)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
