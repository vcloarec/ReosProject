!                   ******************
                    SUBROUTINE COMPLIM
!                   ******************
!
     &(LIUBOR,LIVBOR,LITBOR,UBOR,VBOR,TBOR,
     & AUBOR,ATBOR,BTBOR,NPTFR,TRAC,
     & KENT,KENTU,KSORT,KADH,KLOG,IELMU,IELMV,IELMT,
     & IKLBOR,NELEB,NELEBX)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   27/03/2014
!***********************************************************************
!
!brief    SUPPLEMENTS THE BOUNDARY CONDITION FILE
!+                FOR THE QUADRATIC ELEMENTS.
!
!history  ALGIANE FROEHLY (MATMECA PLACEMENT)
!+        23/10/2008
!+        V5P9
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        27/03/2014
!+        V7P0
!+   Adaptation to new numbering of boundary segments in parallel.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ATBOR          |<->| BOUNDARY CONDITIONS FOR TRACERS
!|                |   | NU*DT/DN=ATBOR*T+BTBOR
!| BTBOR          |<->| BOUNDARY CONDITIONS FOR TRACERS
!|                |   | NU*DT/DN=ATBOR*T+BTBOR
!| AUBOR          |<->| COEFFICIENT DE FROTTEMENT AU BORD
!| IELMT          |-->| TYPE OF ELEMENT OF TRACERS
!| IELMU          |-->| TYPE OF ELEMENT OF VELOCITY U
!| IELMV          |-->| TYPE OF ELEMENT OF VELOCITY V
!| IKLBOR         |-->| CONNECTIVITY OF BOUNDARY SEGMENTS
!| KADH           |-->| CONVENTION FOR NO SLIP BOUNDARY CONDITION
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY V
!| LITBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON TRACERS
!| NELEB          |-->| NUMBER OF BOUNDARY SEGMENTS
!| NELEBX         |-->| MAXIMUM NUMBER OF BOUNDARY SEGMENTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| TRAC           |-->| LOGICAL SAYING IF THERE ARE TRACERS
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
      INTEGER, INTENT(IN) :: NPTFR,KENT,KSORT,KADH,KLOG,KENTU
      INTEGER, INTENT(IN) :: IELMU,IELMV,IELMT,NELEB,NELEBX
      LOGICAL, INTENT(IN) :: TRAC
      INTEGER, INTENT(INOUT) :: LIUBOR(*),LIVBOR(*)
      INTEGER, INTENT(INOUT) :: LITBOR(*)
      INTEGER, INTENT(IN)    :: IKLBOR(NELEBX,2)
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(2*NPTFR,2),VBOR(2*NPTFR,2)
      DOUBLE PRECISION, INTENT(INOUT) :: AUBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: TBOR(*),ATBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: BTBOR(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,KP1,IELEB
!
!----------------------------------------------------------------------
!
!     VELOCITY WITH QUADRATIC U-COMPONENT
!
      IF(IELMU.EQ.13) THEN
!
        DO IELEB=1,NELEB
!
        K  =IKLBOR(IELEB,1)
        KP1=IKLBOR(IELEB,2)
!
        IF(KP1.NE.K) THEN
        IF(LIUBOR(K).EQ.LIUBOR(KP1)) THEN
          LIUBOR(K+NPTFR) = LIUBOR(K)
        ELSEIF( LIUBOR(K  ).EQ.KLOG .OR.
     &          LIUBOR(KP1).EQ.KLOG       ) THEN
          LIUBOR(K+NPTFR) = KLOG
        ELSEIF( LIUBOR(K  ).EQ.KADH .OR.
     &          LIUBOR(KP1).EQ.KADH       ) THEN
          LIUBOR(K+NPTFR) = KADH
        ELSEIF( LIUBOR(K  ).EQ.KENTU .OR.
     &          LIUBOR(KP1).EQ.KENTU      ) THEN
          LIUBOR(K+NPTFR) = KENTU
        ELSEIF( LIUBOR(K  ).EQ.KSORT .OR.
     &          LIUBOR(KP1).EQ.KSORT      ) THEN
          LIUBOR(K+NPTFR) = KSORT
        ELSE
          WRITE(LU,*) 'CONDITION INITIALE QUADRATIQUE DE U ','K ',K,
     &                ' NON PREVUE POUR LIUBOR = ',LIUBOR(K),
     &                ' ET LIUBOR(K+1) = ',LIUBOR(KP1)
          CALL PLANTE(1)
          STOP
        ENDIF
        UBOR(K+NPTFR,1) = (UBOR(K,1)+UBOR(KP1,1))*0.5D0
!
        ENDIF
!
        ENDDO
!
      ENDIF
!
!     VELOCITY WITH QUADRATIC V-COMPONENT
!
      IF(IELMV.EQ.13) THEN
!
        DO IELEB=1,NELEB
!
        K  =IKLBOR(IELEB,1)
        KP1=IKLBOR(IELEB,2)
!
        IF(KP1.NE.K) THEN
        IF(LIVBOR(K).EQ.LIVBOR(KP1)) THEN
          LIVBOR(K+NPTFR) = LIVBOR(K)
        ELSEIF( LIVBOR(K  ).EQ.KLOG .OR.
     &          LIVBOR(KP1).EQ.KLOG       ) THEN
          LIVBOR(K+NPTFR) = KLOG
        ELSEIF( LIVBOR(K  ).EQ.KADH .OR.
     &          LIVBOR(KP1).EQ.KADH       ) THEN
          LIVBOR(K+NPTFR) = KADH
        ELSEIF( LIVBOR(K  ).EQ.KENTU .OR.
     &          LIVBOR(KP1).EQ.KENTU      ) THEN
          LIVBOR(K+NPTFR) = KENTU
        ELSEIF( LIVBOR(K  ).EQ.KSORT .OR.
     &          LIVBOR(KP1).EQ.KSORT      ) THEN
          LIVBOR(K+NPTFR) = KSORT
        ELSE
          WRITE(LU,*) 'CONDITION INITIALE QUADRATIQUE DE U ','K ',K,
     &                ' NON PREVUE POUR LIUBOR = ',LIUBOR(K),
     &                ' ET LIUBOR(K+1) = ',LIUBOR(KP1)
          CALL PLANTE(1)
          STOP
        ENDIF
        VBOR(K+NPTFR,1) = (VBOR(K,1)+VBOR(KP1,1))*0.5D0
        ENDIF
!
        ENDDO
!
      ENDIF
!
      IF(IELMV.EQ.13.OR.IELMU.EQ.13) THEN
        DO IELEB=1,NELEB
          K  =IKLBOR(IELEB,1)
          KP1=IKLBOR(IELEB,2)
          AUBOR(K+NPTFR) = (AUBOR(K)+AUBOR(KP1))*0.5D0
        ENDDO
      ENDIF
!
!     WITH QUADRATIC TRACER T
!
      IF(TRAC.AND.IELMT.EQ.13) THEN
!
        DO IELEB=1,NELEB
!
        K  =IKLBOR(IELEB,1)
        KP1=IKLBOR(IELEB,2)
!
        IF(KP1.NE.K) THEN
        IF(LITBOR(K).EQ.LITBOR(KP1)) THEN
          LITBOR(K+NPTFR) = LITBOR(K)
        ELSEIF( LITBOR(K  ).EQ.KLOG .OR.
     &          LITBOR(KP1).EQ.KLOG       ) THEN
          LITBOR(K+NPTFR) = KLOG
        ELSEIF( LITBOR(K  ).EQ.KADH .OR.
     &          LITBOR(KP1).EQ.KADH       ) THEN
          LITBOR(K+NPTFR) = KADH
        ELSEIF( LITBOR(K  ).EQ.KENTU .OR.
     &          LITBOR(KP1).EQ.KENTU      ) THEN
          LITBOR(K+NPTFR) = KENTU
        ELSEIF( LITBOR(K  ).EQ.KSORT .OR.
     &          LITBOR(KP1).EQ.KSORT      ) THEN
          LITBOR(K+NPTFR) = KSORT
        ELSE
          WRITE(LU,*) 'CONDITION INITIALE QUADRATIQUE DE U ','K ',K,
     &                ' NON PREVUE POUR LIUBOR = ',LIUBOR(K),
     &                ' ET LIUBOR(K+1) = ',LIUBOR(KP1)
          CALL PLANTE(1)
          STOP
        ENDIF
        TBOR(K+NPTFR)  = (TBOR(K)+TBOR(KP1))  *0.5D0
        ATBOR(K+NPTFR) = (ATBOR(K)+ATBOR(KP1))*0.5D0
        BTBOR(K+NPTFR) = (BTBOR(K)+BTBOR(KP1))*0.5D0
        ENDIF
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  CHECKS, CORRECTS AND SAVES:
!
      IF(IELMU.EQ.13.OR.IELMV.EQ.13) THEN
!
      DO K=NPTFR+1,2*NPTFR
!
!     FRICTION COEFFICIENT SET TO 0 WHEN NOT NEEDED
!
      IF(LIUBOR(K).NE.KLOG.AND.LIVBOR(K).NE.KLOG) AUBOR(K) = 0.D0
!
!     WALL ADHERENCE MODIFIED FOR H
!
      IF(AUBOR(K).GT.0.D0) THEN
        WRITE(LU,49) AUBOR(K),K
49      FORMAT(1X,'COMPLIM : AUBOR MUST BE NEGATIVE',/,1X,
     &            '         IT IS ',F10.3,' AT BOUNDARY POINT ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     DIRICHLET VALUES SET TO 0 WHEN THE POINT IS NOT A DIRICHLET
!     FOR THE NODES WITH WALL ADHERENCE, UBOR OR VBOR =0 IS REQUIRED
!
      IF(LIUBOR(K).NE.KENT.AND.LIUBOR(K).NE.KENTU) UBOR(K,1)=0.D0
      IF(LIVBOR(K).NE.KENT.AND.LIVBOR(K).NE.KENTU) VBOR(K,1)=0.D0
!
!     SAVES UBOR AND VBOR ON THEIR SECOND DIMENSION
!
      UBOR(K,2) = UBOR(K,1)
      VBOR(K,2) = VBOR(K,1)
!
      ENDDO
!
      IF(TRAC) THEN
        DO K=1,NPTFR
          IF(LITBOR(K).NE.KENT) TBOR(K)=0.D0
        ENDDO
      ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

