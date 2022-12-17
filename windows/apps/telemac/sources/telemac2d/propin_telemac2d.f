!                   ***************************
                    SUBROUTINE PROPIN_TELEMAC2D
!                   ***************************
!
     &(LIMPRO,LIMDIM,MASK,LIUBOR,LIVBOR,LIHBOR,NBOR,NPTFR,
     & KENT,KENTU,KSORT,KADH,KLOG,KNEU,KDIR,KDDL,
     & CLH,CLU,CLV,IELMU,U,V,GRAV,H,NPOIN,NELBOR,MSK,MASKEL,
     & NFRLIQ,THOMFR,NUMLIQ,FRTYPE,XNEBOR,YNEBOR,IKLBOR,ENTET,
     & NELEBX,NELEB)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    1) CHECKS THE COMPATIBILITY OF BOUNDARY CONDITIONS.
!+
!+        2) FILLS ARRAYS LIMPRO AND MASK.
!
!history  J-M HERVOUET (LNHE)
!+        27/06/2008
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
!history  J-M HERVOUET (LNHE)
!+        16/05/2012
!+        V6P2
!+   Security added in LIMPRO in parallel, look for -9999.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/03/2014
!+        V7P0
!+   New implementation that enables a different numbering of boundary
!+   points and boundary segments.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        16/05/2014
!+        V7P0
!+   Velocity points KLOG now treated in LIMPRO as KNEU (they were
!+   before quoted KDDL because the treatment was the same, but some
!+   advection schemes may now then change the KDDL into KDIR if the
!+   flux corresponds to an entrance)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CLH            |<->| COPY OF LIHBOR
!| CLU            |<->| COPY OF LIUBOR
!| CLV            |<->| COPY OF LIVBOR
!| ENTET          |-->| IF YES, MESSAGES PRINTED
!| FRTYPE         |-->| TYPE OF TREATMENT FOR LIQUID BOUNDARIES
!| GRAV           |-->| GRAVITY
!| IELMU          |-->| TYPE OF ELEMENT FOR U
!| IKLBOR         |-->| CONNECTIVITY TABLE FOR BOUNDARY ELEMENTS
!| KADH           |-->| CONVENTION FOR NO SLIP BOUNDARY CONDITION
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KNEU           |-->| CONVENTION FOR NEUMANN CONDITION
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIMDIM         |-->| FIRST DIMENSION OF LIMPRO
!| LIMPRO         |<--| TYPES OF BOUNDARY CONDITIONS FOR PROPAGATION
!|                |   | PER POINT   :    .1:H  .2:U  .3:V
!|                |   | PER SEGMENT :    .4:H  .5:U  .6:V
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| MASK           |<--| BLOCK OF MASKS FOR SEGMENTS :
!|                |   | MASK(NPTFR,1) : 1. IF KDIR ON U 0. SINON
!|                |   | MASK(NPTFR,2) : 1. IF KDIR ON V 0. SINON
!|                |   | MASK(NPTFR,3) : 1. IF KDDL ON U 0. SINON
!|                |   | MASK(NPTFR,4) : 1. IF KDDL ON V 0. SINON
!|                |   | MASK(NPTFR,5) : 1. IF KNEU ON U 0. SINON
!|                |   | MASK(NPTFR,6) : 1. IF KNEU ON V 0. SINON
!|                |   | MASK(NPTFR,7) : NOT USED
!|                |   | MASK(NPTFR,8) : 1. - MASK( ,5)
!|                |   | MASK(NPTFR,9) : 1. IF H DIRICHLET
!|                |   | MASK(NPTFR,10): 1. IF H NEUMANN
!|                |   | MASK(NPTFR,11): 1. IF H DEGREE OF FREEDOM
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELBOR         |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT.
!| NELEB          |-->| NUMBER OF BOUNDARY ELEMENTS
!| NELEBX         |-->| MAXIMUM NUMBER OF BOUNDARY ELEMENTS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| THOMFR         |-->| IF YES, THOMPSON BOUNDARY CONDITIONS
!| XNEBOR         |<--| X-COMPONENT OF NORMAL AT NODES
!| YNEBOR         |<--| Y-COMPONENT OF NORMAL AT NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NPTFR,KENTU,NFRLIQ
      INTEGER, INTENT(IN) :: KENT,KSORT,KADH,KLOG,KNEU,KDIR,KDDL
      INTEGER, INTENT(IN) :: LIMDIM,IELMU,NELEBX,NELEB
      INTEGER, INTENT(IN) :: NELBOR(NELEBX),LIVBOR(NPTFR),LIHBOR(NPTFR)
      INTEGER, INTENT(IN) :: LIUBOR(NPTFR),FRTYPE(NFRLIQ)
      INTEGER, INTENT(IN) :: IKLBOR(NELEBX,*)
      INTEGER, INTENT(INOUT) :: LIMPRO(LIMDIM,6)
      INTEGER, INTENT(IN) :: NBOR(NPTFR),NUMLIQ(NPTFR)
      INTEGER, INTENT(INOUT) :: CLH(NPTFR),CLU(NPTFR),CLV(NPTFR)
      LOGICAL, INTENT(IN) :: MSK,THOMFR,ENTET
      DOUBLE PRECISION, INTENT(IN)   :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)   :: U(NPOIN),V(NPOIN),H(NPOIN)
      DOUBLE PRECISION, INTENT(IN)   :: GRAV,MASKEL(*)
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: MASK
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,N,IFRLIQ,IELEM,KP1
!
      INTEGER, PARAMETER :: UDIR    =  1
      INTEGER, PARAMETER :: VDIR    =  2
      INTEGER, PARAMETER :: UDDL    =  3
      INTEGER, PARAMETER :: VDDL    =  4
      INTEGER, PARAMETER :: UNEU    =  5
      INTEGER, PARAMETER :: VNEU    =  6
      INTEGER, PARAMETER :: UNONNEU =  8
      INTEGER, PARAMETER :: HDIR    =  9
      INTEGER, PARAMETER :: HNEU    = 10
      INTEGER, PARAMETER :: HDDL    = 11
!
      DOUBLE PRECISION YY,F2,F3
!
      LOGICAL ALERTE1,ALERTE2
!
      INTEGER IGUILT1,IGUILT2,ISEG
!
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
      DO K=1,NPTFR
        CLH(K) = LIHBOR(K)
        CLU(K) = LIUBOR(K)
        CLV(K) = LIVBOR(K)
      ENDDO
!
!     FOR THOMPSON TREATMENT
!
      IF(NFRLIQ.NE.0.AND.THOMFR) THEN
!
        DO K = 1 , NPTFR
          IFRLIQ=NUMLIQ(K)
          IF(IFRLIQ.NE.0) THEN
            IF(FRTYPE(IFRLIQ).EQ.2.AND.H(NBOR(K)).GT.1.D-3) THEN
              CLH(K) = KENT
              CLU(K) = KENTU
              CLV(K) = KENTU
            ENDIF
          ENDIF
        ENDDO
!
      ENDIF
!
!  CHECKS AND MODIFIES THE CONDITIONS (IF REQUIRED) TO AVOID NON
!  PHYSICAL CASES : COMPLETELY FREE EXIT IN RIVER FLOW
!
      ALERTE1=.FALSE.
      ALERTE2=.FALSE.
!
      DO K=1,NPTFR
!
        N = NBOR(K)
        F2 = (U(N)**2+V(N)**2) / GRAV / MAX(H(N),1.D-8)
!
!       INCOMING FREE VELOCITY
!
        IF(CLU(K).EQ.KSORT.AND.CLV(K).EQ.KSORT) THEN
          F3 = U(N)*XNEBOR(K)+V(N)*YNEBOR(K)
          IF(F3.LE.-1.D-2) THEN
            ALERTE1=.TRUE.
            IGUILT1=K
          ENDIF
        ENDIF
!
!       SUPERCRITICAL INFLOW WITH FREE ELEVATION
!
        IF(CLH(K).EQ.KSORT.AND.F2.GE.1.D0) THEN
          F3 = U(N)*XNEBOR(K)+V(N)*YNEBOR(K)
          IF(F3.LE.-1.D-2) THEN
            ALERTE2=.TRUE.
            IGUILT2=K
          ENDIF
        ENDIF
!
      ENDDO
!
      IF(ALERTE1.AND.ENTET) THEN
        WRITE(LU,*) 'ILL-POSED PROBLEM, ENTERING FREE VELOCITY'
        WRITE(LU,*) 'FOR EXAMPLE AT BOUNDARY POINT NUMBER ',IGUILT1
      ENDIF
!
      IF(ALERTE2.AND.ENTET) THEN
        WRITE(LU,*) 'ILL-POSED PROBLEM, FREE DEPTH'
        WRITE(LU,*) 'ON BOUNDARY WITH ENTERING VELOCITY'
        WRITE(LU,*) 'AND SUPERCRITICAL FLOW'
        WRITE(LU,*) 'FOR EXAMPLE AT BOUNDARY POINT NUMBER ',IGUILT2
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE BOUNDARY CONDITIONS FOR PROPAGATION:
!
!-----------------------------------------------------------------------
!
!     NODAL VALUES: LIMPRO(K,1 2 AND 3)
!
      DO K=1,NPTFR
        IF(CLH(K).EQ.KENT) THEN
          LIMPRO(K,1) = KDIR
        ELSEIF(CLH(K).EQ.KSORT) THEN
          LIMPRO(K,1) = KDDL
        ELSEIF(CLH(K).EQ.KLOG ) THEN
          LIMPRO(K,1) = KNEU
        ELSE
          WRITE(LU,11) K
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(CLU(K).EQ.KENT.OR.CLU(K).EQ.KENTU.OR.CLU(K).EQ.KADH) THEN
          LIMPRO(K,2) = KDIR
        ELSEIF(CLU(K).EQ.KSORT) THEN
          LIMPRO(K,2) = KDDL
        ELSEIF(CLU(K).EQ.KLOG) THEN
          LIMPRO(K,2) = KNEU
        ELSE
          WRITE(LU,21) K
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(CLV(K).EQ.KENT.OR.CLV(K).EQ.KENTU.OR.CLV(K).EQ.KADH) THEN
          LIMPRO(K,3) = KDIR
        ELSEIF(CLV(K).EQ.KSORT) THEN
          LIMPRO(K,3) = KDDL
        ELSEIF(CLV(K).EQ.KLOG) THEN
          LIMPRO(K,3) = KNEU
        ELSE
          WRITE(LU,31) K
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!     SEGMENT VALUES
!
!     INITIALISES ALL THE VECTORS OF THE BLOCK TO 0
!
      CALL OS('X=0     ',X=MASK)
!
      DO ISEG=1,NELEB
!
      K=  IKLBOR(ISEG,1)
      KP1=IKLBOR(ISEG,2)
!
!-----------------------------------------------------------------------
!
!     BOUNDARY CONDITIONS ON ELEVATION
!
      IF(CLH(K).EQ.KENT) THEN
        IF(CLH(KP1).EQ.KENT) THEN
          MASK%ADR(HDIR)%P%R(ISEG)=1.D0
        ELSEIF(CLH(KP1).EQ.KLOG) THEN
          MASK%ADR(HNEU)%P%R(ISEG)=1.D0
        ELSEIF(CLH(KP1).EQ.KSORT) THEN
          MASK%ADR(HDDL)%P%R(ISEG)=1.D0
        ELSE
          WRITE(LU,11) K
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSEIF(CLH(K).EQ.KSORT) THEN
        IF(CLH(KP1).EQ.KSORT) THEN
          MASK%ADR(HDDL)%P%R(ISEG)=1.D0
        ELSEIF(CLH(KP1).EQ.KLOG) THEN
          MASK%ADR(HNEU)%P%R(ISEG)=1.D0
        ELSEIF(CLH(KP1).EQ.KENT) THEN
          MASK%ADR(HDDL)%P%R(ISEG)=1.D0
        ELSE
          WRITE(LU,11) K
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSEIF(CLH(K).EQ.KLOG ) THEN
        MASK%ADR(HNEU)%P%R(ISEG)=1.D0
      ENDIF
!
!     BOUNDARY CONDITIONS ON U
!
      IF(CLU(K).EQ.KENT.OR.CLU(K).EQ.KENTU) THEN
        IF(CLU(KP1).EQ.KENT.OR.CLU(KP1).EQ.KENTU) THEN
          MASK%ADR(UDIR)%P%R(ISEG)=1.D0
        ELSEIF(CLU(KP1).EQ.KADH) THEN
          MASK%ADR(UDIR)%P%R(ISEG)=1.D0
        ELSEIF(CLU(KP1).EQ.KLOG) THEN
          MASK%ADR(UNEU)%P%R(ISEG)=1.D0
        ELSEIF(CLU(KP1).EQ.KSORT) THEN
          MASK%ADR(UDDL)%P%R(ISEG)=1.D0
        ELSE
          WRITE(LU,21) K
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSEIF(CLU(K).EQ.KADH) THEN
        MASK%ADR(UNEU)%P%R(ISEG)=1.D0
      ELSEIF(CLU(K).EQ.KSORT) THEN
        IF(CLU(KP1).EQ.KSORT) THEN
          MASK%ADR(UDDL)%P%R(ISEG)=1.D0
        ELSEIF(CLU(KP1).EQ.KLOG) THEN
          MASK%ADR(UNEU)%P%R(ISEG)=1.D0
        ELSEIF(CLU(KP1).EQ.KENT) THEN
          MASK%ADR(UDDL)%P%R(ISEG)=1.D0
        ELSEIF(CLU(KP1).EQ.KENTU) THEN
          MASK%ADR(UDDL)%P%R(ISEG)=1.D0
        ELSEIF(CLU(KP1).EQ.KADH) THEN
          MASK%ADR(UNEU)%P%R(ISEG)=1.D0
        ELSE
          WRITE(LU,21) K
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSEIF(CLU(K).EQ.KLOG) THEN
        MASK%ADR(UNEU)%P%R(ISEG)=1.D0
      ENDIF
!
!     BOUNDARY CONDITIONS ON V
!
      IF(CLV(K).EQ.KENT.OR.CLV(K).EQ.KENTU) THEN
        IF(CLV(KP1).EQ.KENT.OR.CLV(KP1).EQ.KENTU) THEN
          MASK%ADR(VDIR)%P%R(ISEG)=1.D0
        ELSEIF(CLV(KP1).EQ.KADH) THEN
          MASK%ADR(VDIR)%P%R(ISEG)=1.D0
        ELSEIF(CLV(KP1).EQ.KLOG) THEN
          MASK%ADR(VNEU)%P%R(ISEG)=1.D0
        ELSEIF(CLV(KP1).EQ.KSORT) THEN
          MASK%ADR(VDDL)%P%R(ISEG)=1.D0
        ELSE
          WRITE(LU,31) K
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSEIF(CLV(K).EQ.KADH) THEN
        MASK%ADR(VNEU)%P%R(ISEG)=1.D0
      ELSEIF(CLV(K).EQ.KSORT) THEN
        IF(CLV(KP1).EQ.KSORT) THEN
          MASK%ADR(VDDL)%P%R(ISEG)=1.D0
        ELSEIF(CLV(KP1).EQ.KLOG) THEN
          MASK%ADR(VNEU)%P%R(ISEG)=1.D0
        ELSEIF(CLV(KP1).EQ.KENT) THEN
          MASK%ADR(VDDL)%P%R(ISEG)=1.D0
        ELSEIF(CLV(KP1).EQ.KENTU) THEN
          MASK%ADR(VDDL)%P%R(ISEG)=1.D0
        ELSEIF(CLV(KP1).EQ.KADH) THEN
          MASK%ADR(VDDL)%P%R(ISEG)=1.D0
        ELSE
          WRITE(LU,31) K
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSEIF(CLV(K).EQ.KLOG ) THEN
        MASK%ADR(VNEU)%P%R(ISEG)=1.D0
      ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     LIQUID BOUNDARIES MASK
!
      DO ISEG=1,NELEB
        MASK%ADR(UNONNEU)%P%R(ISEG)=1.D0-MASK%ADR(UNEU)%P%R(ISEG)
      ENDDO
!
!     DEDUCES ARRAYS LIMPRO (K, 4 5 AND 6) FROM THE MASKS
!     SEGMENTS WHICH ARE NOT IN THE DOMAIN WILL GET 0
!
      DO K=1,NPTFR
        LIMPRO(K,4)=0
        LIMPRO(K,5)=0
        LIMPRO(K,6)=0
      ENDDO
!
      DO ISEG=1,NELEB
        K=IKLBOR(ISEG,1)
        IF(MASK%ADR(HDIR)%P%R(ISEG).GT.0.5D0) THEN
          LIMPRO(K,4)=KDIR
        ELSEIF(MASK%ADR(HDDL)%P%R(ISEG).GT.0.5D0) THEN
          LIMPRO(K,4)=KDDL
        ELSEIF(MASK%ADR(HNEU)%P%R(ISEG).GT.0.5D0) THEN
          LIMPRO(K,4)=KNEU
        ENDIF
        IF(MASK%ADR(UDIR)%P%R(ISEG).GT.0.5D0) THEN
          LIMPRO(K,5)=KDIR
        ELSEIF(MASK%ADR(UDDL)%P%R(ISEG).GT.0.5D0) THEN
          LIMPRO(K,5)=KDDL
        ELSEIF(MASK%ADR(UNEU)%P%R(ISEG).GT.0.5D0) THEN
          LIMPRO(K,5)=KNEU
        ENDIF
        IF(MASK%ADR(VDIR)%P%R(ISEG).GT.0.5D0) THEN
          LIMPRO(K,6)=KDIR
        ELSEIF(MASK%ADR(VDDL)%P%R(ISEG).GT.0.5D0) THEN
          LIMPRO(K,6)=KDDL
        ELSEIF(MASK%ADR(VNEU)%P%R(ISEG).GT.0.5D0) THEN
          LIMPRO(K,6)=KNEU
        ENDIF
      ENDDO
!
!     SUPPLEMENT FOR QUADRATIC SPEEDS
!     THE POINT IN THE MIDDLE OF A SEGMENT HAS THE SAME CONDITION AS THE SEGMENT
!
      IF(IELMU.EQ.13) THEN
!       LOOP ON BOUNDARY SEGMENTS
        DO ISEG=1,NELEB
          K=  IKLBOR(ISEG,1)
          KP1=IKLBOR(ISEG,2)
          LIMPRO(IKLBOR(ISEG,3),2)=LIMPRO(K,5)
          LIMPRO(IKLBOR(ISEG,3),3)=LIMPRO(K,6)
        ENDDO
      ENDIF
!
!     MASKS USING THE MASK OF THE ELEMENTS
!
      IF(MSK) THEN
        DO ISEG=1,NELEB
          IELEM=NELBOR(ISEG)
          YY = MASKEL(IELEM)
          MASK%ADR(UDIR   )%P%R(ISEG) = MASK%ADR(UDIR   )%P%R(ISEG) * YY
          MASK%ADR(VDIR   )%P%R(ISEG) = MASK%ADR(VDIR   )%P%R(ISEG) * YY
          MASK%ADR(UDDL   )%P%R(ISEG) = MASK%ADR(UDDL   )%P%R(ISEG) * YY
          MASK%ADR(VDDL   )%P%R(ISEG) = MASK%ADR(VDDL   )%P%R(ISEG) * YY
          MASK%ADR(UNEU   )%P%R(ISEG) = MASK%ADR(UNEU   )%P%R(ISEG) * YY
          MASK%ADR(VNEU   )%P%R(ISEG) = MASK%ADR(VNEU   )%P%R(ISEG) * YY
          MASK%ADR(HDIR   )%P%R(ISEG) = MASK%ADR(HDIR   )%P%R(ISEG) * YY
          MASK%ADR(HNEU   )%P%R(ISEG) = MASK%ADR(HNEU   )%P%R(ISEG) * YY
          MASK%ADR(HDDL   )%P%R(ISEG) = MASK%ADR(HDDL   )%P%R(ISEG) * YY
          MASK%ADR(UNONNEU)%P%R(ISEG) = MASK%ADR(UNONNEU)%P%R(ISEG) * YY
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
11    FORMAT(1X,'PROPIN: BOUNDARY POINT',1I5,' UNKNOWN PARAMETER FOR H')
21    FORMAT(1X,'PROPIN: BOUNDARY POINT',1I5,' UNKNOWN PARAMETER FOR U')
31    FORMAT(1X,'PROPIN: BOUNDARY POINT',1I5,' UNKNOWN PARAMETER FOR V')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
