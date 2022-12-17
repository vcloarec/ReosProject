!                   *****************
                    SUBROUTINE PREBOR
!                   *****************
!
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,HN,T,NBOR,NPOIN,NPTFR,NTRAC,NFRLIQ,
     & FRTYPE,NUMLIQ)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PREPARES THE BOUNDARY CONDITIONS FOR TREATMENT BY THOMPSON.
!+     UBOR, VBOR, HBOR, TBOR ARE INITIALISED HERE WITH VALUES AT TIME N
!+     AFTER BORD, THESE ARRAYS THEREFORE CONTAIN EITHER THE VALUE AT
!+     TIME N OR THE IMPOSED VALUE.
!+
!+
!+            STORES H IN A TEMPORARY ARRAY TO SAVE ITS VALUE AT
!+     THE BOUNDARY AT TIME N (MODIFIED IN BORD).
!
!history  E DAVID (LHF)
!+        03/09/2007
!+        V5P8
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
!| H              |-->| WATER DEPTH AT TIME N
!| HBOR           |<--| PRESCRIBED DEPTH
!| HN             |-->| PROPAGATION DEPTH (OPTION H-U)
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| T              |-->| BLOCK OF TRACERS AT TIME N
!| TBOR           |<--| BLOCK OF IMPOSED TRACERS AT BOUNDARIES
!| U              |<->| X-COMPONENT OF VELOCITY
!| V              |<->| Y-COMPONENT OF VELOCITY
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,NFRLIQ,NTRAC
      INTEGER, INTENT(IN)             :: NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: NUMLIQ(NPTFR),FRTYPE(NFRLIQ)
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR),UBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: HN(NPOIN)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,ITRAC,IFRLIQ
!
!-----------------------------------------------------------------------
!
!  LOOP ON THE LIQUID BOUNDARIES
!
      IF(NFRLIQ.GT.0) THEN
!
      DO K= 1 , NPTFR
        IFRLIQ=NUMLIQ(K)
        IF(IFRLIQ.NE.0) THEN
          IF(FRTYPE(IFRLIQ).EQ.2) THEN
            UBOR(K)=U(NBOR(K))
            VBOR(K)=V(NBOR(K))
            HBOR(K)=H(NBOR(K))
            IF(NTRAC.GT.0) THEN
              DO ITRAC=1,NTRAC
                TBOR%ADR(ITRAC)%P%R(K)=T%ADR(ITRAC)%P%R(NBOR(K))
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDDO
!
      ENDIF
!
      CALL OV('X=Y     ', X=HN, Y=H, DIM1=NPOIN)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
