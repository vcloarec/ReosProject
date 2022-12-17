!                   ***********************
                    SUBROUTINE PARCOM2I_SEG
!                   ***********************
!
     &( X1 , X2 , X3 , NSEG , NPLAN , ICOM , IAN , MESH , OPT , IELM )
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPLEMENTS A VECTOR OF SEGMENT AT THE INTERFACES
!+                BETWEEN SUB-DOMAINS.
!+
!+            X CAN BE A BLOCK OF VECTORS. IN THIS CASE, ALL THE
!+                VECTORS IN THE BLOCK ARE TREATED.
!
!note     IN 3D, THE FINITE VOLUME SEGMENTS IN PRISMS ARE
!+         CONSIDERED HERE, I.E. HORIZONTAL FIRST : NSEG*NPLAN
!+         THEN, VERTICAL : NPOIN2*(NPLAN-1).
!
!warning  IF THE VECTORS HAVE A SECOND DIMENSION, IT IS
!+            IGNORED FOR THE TIME BEING
!
!history  J-M HERVOUET (LNHE)
!+        19/11/2013
!+        V7P0
!+   Mere copy of PARCOM2_SEG for integers.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IAN            |-->| NUMBER OF VECTORS TO BE TREATED (X1, X2, X3)
!| ICOM           |-->| COMMUNICATION MODE
!|                |   | = 1 : VALUE WITH MAXIMUM ABSOLUTE VALUE TAKEN
!|                |   | = 2 : CONTRIBUTIONS ADDED
!|                |   | = 3 : MAXIMUM CONTRIBUTION RETAINED
!|                |   | = 4 : MINIMUM CONTRIBUTION RETAINED
!| IELM           |-->| TYPE OF ELEMENT (11: LINEAR TRIANGLE, ETC.)
!| MESH           |-->| MESH STRUCTURE
!| NPLAN          |-->| NUMBER OF PLANES
!| NSEG           |-->| NUMBER OF 2D SEGMENTS
!| OPT            |-->| 1 : HORIZONTAL AND VERTICAL SEGMENTS ONLY
!|                |   | 2 : ALL SEGMENTS
!| X1             |<->| INTEGER VECTOR TO BE COMPLETED, SEE IAN
!| X2             |<->| INTEGER VECTOR TO BE COMPLETED, SEE IAN
!| X3             |<->| INTEGER VECTOR TO BE COMPLETED, SEE IAN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PARCOM2I_SEG => PARCOM2I_SEG
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: ICOM,NSEG,NPLAN,IAN,OPT,IELM
!
!     STRUCTURES: VECTORS OR BLOCKS
!
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
!
!     IN 2D X1(NSEG)
!     IN 3D X1(NSEG*NPLAN+NPOIN2*(NPLAN-1))
!
      INTEGER, INTENT(INOUT) :: X1(*),X2(*),X3(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NPOIN2,IDEB,IFIN,IPLAN,NPL
!
!-----------------------------------------------------------------------
!
      IF(NPLAN.GT.1) THEN
!
!       1) HORIZONTAL FLUXES
!
        DO IPLAN=1,NPLAN
          IDEB=1+NSEG*(IPLAN-1)
          IFIN=  NSEG* IPLAN
          CALL PARACOI(X1(IDEB:IFIN),X2(IDEB:IFIN),X3(IDEB:IFIN),
     &                 NSEG,ICOM,IAN,1,
     &                 MESH%NB_NEIGHB_SEG,MESH%NB_NEIGHB_PT_SEG%I,
     &                 MESH%LIST_SEND_SEG%I,MESH%NH_COM_SEG%I,
     &                 MESH%NH_COM_SEG%DIM1,MESH%BUF_SEND%I,
     &                 MESH%BUF_RECV%I,MESH%BUF_SEND%DIM1)
        ENDDO
!
!       2) VERTICAL FLUXES
!
        NPOIN2=MESH%NPOIN
        DO IPLAN=1,NPLAN-1
          IDEB=NSEG*NPLAN + NPOIN2*(IPLAN-1) + 1
          IFIN=NSEG*NPLAN + NPOIN2* IPLAN
          CALL PARCOM2I(X1(IDEB:IFIN),X2(IDEB:IFIN),X3(IDEB:IFIN),
     &                  NPOIN2,1,ICOM,IAN,MESH)
        ENDDO
!
!       3) CROSSED FLUXES
!
        IF(OPT.EQ.2) THEN
          DO IPLAN=1,NPLAN-1
            IF(IELM.EQ.41) THEN
!             TWO CROSSED FLUXES (PER RECTANGLE OF PRISM)
              NPL=2
              IDEB=NSEG*NPLAN+NPOIN2*(NPLAN-1)+2*NSEG*(IPLAN-1) + 1
              IFIN=NSEG*NPLAN+NPOIN2*(NPLAN-1)+2*NSEG* IPLAN
            ELSEIF(IELM.EQ.51) THEN
!             ONE CROSSED FLUX (PER ORIGINAL RECTANGLE OF PRISM)
              NPL=1
              IDEB=NSEG*NPLAN+NPOIN2*(NPLAN-1)+NSEG*(IPLAN-1) + 1
              IFIN=NSEG*NPLAN+NPOIN2*(NPLAN-1)+NSEG* IPLAN
            ELSE
              WRITE(LU,*) 'UNKNOWN ELEMENT IN PARCOM2_SEG: ',IELM
              CALL PLANTE(1)
              STOP
            ENDIF
            CALL PARACOI(X1(IDEB:IFIN),X2(IDEB:IFIN),X3(IDEB:IFIN),
     &                   NSEG,ICOM,IAN,NPL,
     &                   MESH%NB_NEIGHB_SEG,MESH%NB_NEIGHB_PT_SEG%I,
     &                   MESH%LIST_SEND_SEG%I,MESH%NH_COM_SEG%I,
     &                   MESH%NH_COM_SEG%DIM1,MESH%BUF_SEND%I,
     &                   MESH%BUF_RECV%I,MESH%BUF_SEND%DIM1)
          ENDDO
        ENDIF
!
      ELSE
!
        CALL PARACOI(X1,X2,X3,
     &               NSEG,ICOM,IAN,1,
     &               MESH%NB_NEIGHB_SEG,MESH%NB_NEIGHB_PT_SEG%I,
     &               MESH%LIST_SEND_SEG%I,MESH%NH_COM_SEG%I,
     &               MESH%NH_COM_SEG%DIM1,MESH%BUF_SEND%I,
     &               MESH%BUF_RECV%I,MESH%BUF_SEND%DIM1)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
