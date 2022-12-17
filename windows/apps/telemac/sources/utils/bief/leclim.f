!                   *****************
                    SUBROUTINE LECLIM
!                   *****************
!
     &(LIHBOR,LIUBOR,LIVBOR,LITBOR,HBOR,UBOR,VBOR,TBOR,
     & CHBORD,ATBOR,BTBOR,NPTFR,CODE,TRAC,FFORMAT,NGEO,
     & KENT,KENTU,KSORT,KADH,KLOG,KINC,NUMLIQ,MESH,BOUNDARY_COLOUR,
     & NPTFR2)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS THE BOUNDARY CONDITIONS FILE AND
!+                STORES IN ARRAYS THE DATA READ.
!
!history  J-M HERVOUET (LNHE)
!+        09/07/2009
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
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!history  J. GRASSET (Daresbury Lab & EDF)
!+        01/05/2018
!+        Add code for managing concatenated mesh
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| CHBORD         |<--| FRICTION COEFFICIENT AT BOUNDARY
!| ATBOR,BTBOR    |<--| THERMAL EXCHANGE COEFFICIENTS.
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| CODE           |-->| 3 LETTER NAME OF THE CODE
!| TRAC           |-->| IF YES, THERE ARE TRACERS
!| FFORMAT        |-->| FILE FORMAT
!| NGEO           |-->| LOGICAL UNIT OF BOUNDARY CONDITIONS FILE
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| KADH           |-->| CONVENTION FOR NO SLIP BOUNDARY CONDITION
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KINC           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| MESH           |-->| MESH STRUCTURE
!| BOUNDARY_COLOUR|<--| COLOUR OF BOUNDARY POINT (DEFAULT: ITS RANK)
!| NPTFR2         |-->| NUMBER OF QUADRATIC BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_LECLIM => LECLIM
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NGEO,KENT,KSORT,KADH,KLOG,KINC,KENTU
      INTEGER, INTENT(IN)    :: NPTFR
      LOGICAL, INTENT(IN)    :: TRAC
      INTEGER, INTENT(INOUT) :: NUMLIQ(*)
      INTEGER, INTENT(INOUT) :: LIUBOR(NPTFR),LIVBOR(NPTFR)
      INTEGER, INTENT(INOUT) :: LIHBOR(NPTFR),LITBOR(*)
      DOUBLE PRECISION,  INTENT(INOUT) :: UBOR(*),VBOR(*)
      DOUBLE PRECISION,  INTENT(INOUT) :: HBOR(NPTFR),CHBORD(NPTFR)
      DOUBLE PRECISION,  INTENT(INOUT) :: TBOR(*),ATBOR(*)
      DOUBLE PRECISION,  INTENT(INOUT) :: BTBOR(*)
      TYPE(BIEF_MESH),   INTENT(INOUT) :: MESH
      CHARACTER(LEN=3),  INTENT(IN) :: CODE
      CHARACTER(LEN=8),  INTENT(IN) :: FFORMAT
      INTEGER, OPTIONAL, INTENT(INOUT) :: BOUNDARY_COLOUR(NPTFR)
      INTEGER, OPTIONAL, INTENT(IN)    :: NPTFR2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NPTFR_BND, I, IERR, K
      INTEGER, ALLOCATABLE :: BOUNDARY_COLOUR2(:)
      INTEGER :: DIMUBOR, TYPE_BND_ELEM, NDP_BND, NELEBD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE_BND_ELEM = MESH%TYPELMBND
      DIMUBOR = NPTFR
      IF(PRESENT(NPTFR2)) DIMUBOR = NPTFR2
      CALL GET_BND_NPOIN(FFORMAT,NGEO,TYPE_BND_ELEM,NPTFR_BND,IERR)
      CALL CHECK_CALL(IERR,'LECLIM:GET_BND_NPOIN')
      ! Test difference between nptfr and nptfr_bnd
      IF(NPTFR.NE.NPTFR_BND) THEN
        WRITE(LU,24) NPTFR_BND,NPTFR
24      FORMAT(1X,'LECLIM: ERROR IN THE BOUNDARY CONDITIONS FILE,',
     &       /,9X,1I5,' LINES INSTEAD OF ',I5,' REQUESTED')
        CALL PLANTE(1)
        STOP
      ENDIF
      !
      CALL GET_BND_NELEM(FFORMAT,NGEO,TYPE_BND_ELEM,NELEBD,IERR)
      CALL CHECK_CALL(IERR,'LECLIM:GET_BND_NPOIN')

      CALL GET_NODES_PER_ELEMENT(TYPE_BND_ELEM,NDP_BND)
      ! Get nbor
      ! Only reading nbor from mesh in serial (because of renumbering)
      ! In parallel it will be read by read_partel_info
      IF(NCSIZE.LE.1) THEN
        CALL GET_BND_NUMBERING(FFORMAT,NGEO,TYPE_BND_ELEM,NPTFR_BND,
     &                    MESH%NBOR%I,IERR)
        IF(PRESENT(BOUNDARY_COLOUR)) THEN
          CALL GET_BND_COLOR(FFORMAT,NGEO,TYPE_BND_ELEM,NPTFR_BND,
     &                       BOUNDARY_COLOUR,IERR)
          CALL CHECK_CALL(IERR,'LECLIM:GET_BND_COLOR')
        ENDIF
      ELSE
        ALLOCATE(BOUNDARY_COLOUR2(NPTFR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'LECLIM:BOUNDARY_COLOUR2')
!
        CALL READ_PARTEL_INFO(CODE,NPTFR,NUMLIQ,BOUNDARY_COLOUR2,
     &                        MESH)
!
        IF(PRESENT(BOUNDARY_COLOUR)) THEN
          DO I=1,NPTFR
            BOUNDARY_COLOUR(I) = BOUNDARY_COLOUR2(I)
          ENDDO
        ENDIF
        DEALLOCATE(BOUNDARY_COLOUR2)
      ENDIF
      ! Get value of each boundary conditions
      CALL GET_BND_VALUE(FFORMAT,NGEO,TYPE_BND_ELEM,NELEBD,
     & LIHBOR,LIUBOR,LIVBOR,HBOR,UBOR,VBOR,CHBORD,TRAC,
     & LITBOR,TBOR,ATBOR,BTBOR,NPTFR_BND,IERR)
      CALL CHECK_CALL(IERR,'LECLIM:GET_BND_VALUE')
!
      DO K=1,NPTFR
!
!       ADHERENCE FOR H CHANGED AT THE WALL
!
        IF(LIHBOR(K).EQ.KADH) THEN
          LIHBOR(K)=KLOG
          WRITE(LU,52) K
52        FORMAT(1X,'LECLIM : ADHERENCE SUR LA HAUTEUR AU POINT ',1I6,
     &              '         CHANGEE EN CONDITION DE PAROI')
        ENDIF
!
!       INCIDENT WAVE FOR H TREATED LIKE A FREE EXIT
!
        IF(LIHBOR(K).EQ.KINC) THEN
          LIHBOR(K)=KSORT
        ENDIF
!
!       CANCELS DIRICHLET VALUES WHEN THE POINT IS NOT DIRICHLET
!       FOR POINTS WITH ADHERENCE, NEEDS UBOR OR VBOR =0
!
        IF(LIUBOR(K).NE.KENT.AND.LIUBOR(K).NE.KENTU) UBOR(K)=0.D0
        IF(LIVBOR(K).NE.KENT.AND.LIVBOR(K).NE.KENTU) VBOR(K)=0.D0
!
!       BACKS UP UBOR AND VBOR ON THEIR SECOND DIMENSION
!
        UBOR(K+DIMUBOR) = UBOR(K)
        VBOR(K+DIMUBOR) = VBOR(K)
!
      ENDDO ! K
!
!-----------------------------------------------------------------------
!
      RETURN
      END
