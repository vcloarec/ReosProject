!                   **************************
                    SUBROUTINE COMP_NH_COM_SEG
!                   **************************
!
     &(ELTSEG,DIM1ELTSEG,NH_COM_SEG,DIM1NHCOM,NB_NEIGHB_SEG,
     & NB_NEIGHB_PT_SEG,GLOSEG,DIMGLO,KNOLG,NPOIN)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPLETES THE REAL ADDRESS OF SEGMENTS IN NH_COM_SEG.
!+                SEE PARINI WHERE NH_COM_SEG IS INITIALISED AT -999999
!+                AND THEN FILLED WITH 4*IELEM+IFACE TO STORE IELEM AND
!+                IFACE.
!+
!+            THEN THE ADDRESSES ARE ORDERED WITH RESPECT TO THE
!+                GLOBAL NUMBER OF THE FIRST AND SECOND POINT OF
!+                EVERY SEGMENT, SO THAT THE PROCESSORS SHARE THE
!+                INFORMATION ON THE SAME SEGMENTS.
!
!history  C. DENIS
!+        22/07/2009
!+        V6P0
!+   AVOIDS THE (TOO) LARGE INTEGERS OF 5.9 RELEASE
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
!history  J-M HERVOUET
!+        09/09/2017
!+        V7P3
!+   Local NELEM renamed DIM1ELTSEG, it is not always equal to NELEM,
!+   it is in fact NELMAX.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIM1ELTSEG     |-->| FIRST DIMENSION OF ELTSEG
!| DIM1NHCOM      |-->| FIRST DIMENSION OF NH_COM_SEG
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| ELTSEG         |-->| GIVES THE SEGMENT NUMBER OF EDGES OF ELEMENTS
!| GLOSEG         |-->| GLOBAL NUMBERS (IN SUB-DOMAIN) OF POINTS
!|                |   | OF A SEGMENT
!| KNOLG          |-->| GLOBAL NUMBERS (WHOLE MESH) FUNCTION OF
!|                |   | LOCAL NUMBERS OF POINTS
!| NB_NEIGHB_SEG  |-->| NUMBER OF NEIGHBOUR PROCESSOR (FOR SEGMENTS)
!| NH_COM_SEG     |-->| ADDRESSES OF INTERFACE SEGMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: DIM1ELTSEG,DIM1NHCOM,NB_NEIGHB_SEG
      INTEGER, INTENT(IN)    :: DIMGLO,NPOIN
      INTEGER, INTENT(INOUT) :: NH_COM_SEG(DIM1NHCOM,NB_NEIGHB_SEG)
      INTEGER, INTENT(IN)    :: ELTSEG(DIM1ELTSEG,3),GLOSEG(DIMGLO,2)
      INTEGER, INTENT(IN)    :: NB_NEIGHB_PT_SEG(NB_NEIGHB_SEG)
      INTEGER, INTENT(IN)    :: KNOLG(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IFACE,ISEG,IPROC,IKA,I,J,B,NUMSEG
      INTEGER I11,I12,I21,I22
      LOGICAL IS_LE_THAN
!
!-----------------------------------------------------------------------
!
      DO IPROC=1,NB_NEIGHB_SEG
        IKA = NB_NEIGHB_PT_SEG(IPROC)
        DO ISEG=1,IKA
          IFACE=MOD(NH_COM_SEG(ISEG,IPROC),4)
          IELEM=(NH_COM_SEG(ISEG,IPROC)-IFACE)/4
          NUMSEG=ELTSEG(IELEM,IFACE)
          NH_COM_SEG(ISEG,IPROC)=NUMSEG
        ENDDO
        IF(IKA.GT.1) THEN
          DO J=2,IKA
            B=NH_COM_SEG(J,IPROC)
            DO I=J-1,1,-1
              NUMSEG=NH_COM_SEG(I,IPROC)
              I11=KNOLG(GLOSEG(NUMSEG,1))
              I12=KNOLG(GLOSEG(NUMSEG,2))
              I21=KNOLG(GLOSEG(B     ,1))
              I22=KNOLG(GLOSEG(B     ,2))
              IF(I11.GT.I21) THEN
                IS_LE_THAN=.FALSE.
              ELSEIF(I11.LT.I21) THEN
                IS_LE_THAN=.TRUE.
              ELSEIF(I11.EQ.I21.AND.I12.GT.I22) THEN
                IS_LE_THAN=.FALSE.
              ELSEIF(I11.EQ.I21.AND.I12.LT.I22) THEN
                IS_LE_THAN=.TRUE.
              ELSEIF(I11.EQ.I21.AND.I12.EQ.I22) THEN
                IS_LE_THAN=.TRUE.
              ELSE
                WRITE(LU,*) 'UNEXPECTED CASE IN COMP_NH_COM_SEG'
                CALL PLANTE(1)
                STOP
              ENDIF
              IF(IS_LE_THAN) GO TO 10
              NH_COM_SEG(I+1,IPROC)=NH_COM_SEG(I,IPROC)
            ENDDO
 10         CONTINUE
            NH_COM_SEG(I+1,IPROC)=B
          ENDDO
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

