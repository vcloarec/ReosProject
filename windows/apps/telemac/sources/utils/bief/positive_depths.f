!                   **************************
                    SUBROUTINE POSITIVE_DEPTHS
!                   **************************
!
     &(T1,T2,T3,T4,H,HN,MESH,FLODEL,COMPUTE_FLODEL,FLBOR,DT,
     & UNSV2D,NPOIN,GLOSEG1,GLOSEG2,NBOR,NPTFR,
     & SMH,YASMH,PLUIE,RAIN,OPTSOU,FLULIM,LIMPRO,HBOR,KDIR,INFO,
     & FLOPOINT,NAMECODE,OPTION,NITMAX,DOFLULIM,FLULIMEBE,DOFLULIMEBE)
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    SUPPRESSES NEGATIVE DEPTHS BY A LIMITATION OF FLUXES.
!
!history  J-M HERVOUET (LNHE)
!+        12/03/2010
!+        V6P0
!+   ADDED COMPUTE_FLODEL
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
!+        16/07/2012
!+        V6P2
!+   The value of transmitted fluxes is given back in FLODEL in all
!+   cases, YAFLODEL no longer used (but left for compatibility)
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        30/05/2013
!+        V6P2
!+   Argument NITMAX added.
!
!history  J-M HERVOUET (LNHE)
!+        30/12/2013
!+        V7P0
!+   Argument YAFLODEL removed.
!
!history  J-M HERVOUET (LNHE)
!+        18/09/2014
!+        V7P0
!+   Now positive sources are first added, then the transfers between
!+   points are done, then the negative sources are treated. In this way
!+   the transfers cannot be corrupted by an initial negative depth.
!+   + one NCSIZE.GT.0 changed into NCSISE.GT.1.
!
!history  J-M HERVOUET (LNHE)
!+        19/06/2015
!+        V7P1
!+   Adaptation to new coefficient IFAC instead of FAC.
!+   Option 1 removed. FLULIM now equal on either side of a segment
!+   in parallel. FLODEL differently shared at the exit.
!
!history  J-M HERVOUET (LNHE)
!+        25/03/2016
!+        V7P2
!+   OPTION has a new meaning (2 was the only previous possibility)
!+   Now : 1 : positive depths ensured with an EBE approach (new !)
!+             that is necessary for the ERIA advection scheme.
!+         2 : positive depths ensured with an edge-based approach (old)
!+   Option 1 will be mandatory with the new advection scheme 15.
!
!history  J-M HERVOUET (LNHE)
!+        16/07/2016
!+        V7P2
!+   In ERIA scheme, now forbidding that two fluxes on either side of
!+   a segment be of opposite sign (one is cancelled and the other is
!+   reduced). The final flux limitation is thus in the range [0,1] and
!+   OPTION=1 (i.e. TREATMENT OF NEGATIVE DEPTHS=3) is now also possible
!+   for the LIPS scheme.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        09/09/2016
!+        V7P2
!+   Swapping rain and boundary fluxes at the end of the algorithm. It
!+   caused a wrong correction of FLBOR in case of evaporation.
!+   Adding rain (it was mixed before with sources).
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        15/09/2016
!+        V7P2
!+   Limiting evaporation to avoid negative depths.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        06/11/2016
!+        V7P3
!+   This subroutine can now provide a flux limitation in the form of
!+   FLULIM(NSEG) or FLULIMEBE(NELMAX,3). This depends on OPTION
!+   However, FLULIMEBE can be produced only with OPTION 1.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        19/11/2016
!+        V7P3
!+   Subroutine split into POSITIVE_DEPTHS_ERIA and POSITIVE_DEPTHS_NERD
!+   The remaining part is just the bifurcation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COMPUTE_FLODEL |-->| IF YES, COMPUTE FLODEL HERE
!| DOFLULIM       |-->| OPTIONAL, IF YES DOES ARRAY FLULIM
!| DOFLULIMEBE    |-->| OPTIONAL, IF YES DOES ARRAY FLULIMEBE
!| DT             |-->| TIME STEP
!| FLBOR          |<->| BOUNDARY FLUXES
!| FLODEL         |<->| FLUXES GIVEN BY SEGMENT
!|                |   | MAY BE COMPUTED HERE (SEE COMPUTE-FLODEL)
!|                |   | OR SIMPLY GIVEN. AT THE EXIT, THE REAL FLUX
!|                |   | TRANSMITTED IS GIVEN BACK.
!| FLOPOINT       |-->| FLUXES GIVEN BY POINTS (ELEMENT BY ELEMENT)
!| FLULIM         |<--| PER SEGMENT: PERCENTAGE OF FLUX THAT HAS NOT
!|                |   | BEEN TRANSMITTED AT THE END OF THE ALGORITHM
!| FLULIMEBE      |<--| AS FLULIM BUT GIVEN PER ELEMENT
!|                |   | HENCE FLULIMEBE(NELMAX,3)
!| GLOSEG1        |-->| FIRST POINT OF SEGMENTS
!| GLOSEG2        |-->| SECOND POINT OF SEGMENTS
!| H              |<->| NEW DEPTH
!| HBOR           |-->| PRESCRIBED DEPTHS AT BOUNDARIES
!| HN             |-->| OLD DEPTH
!| INFO           |-->| IF YES, PRINTING INFORMATION ON LISTING
!| KDIR           |-->| CONVENTION FOR DIRICHLET BOUNDARY CONDITION
!| LIMPRO         |-->| TYPE OF BOUNDARY CONDITIONS
!|                |   | IF EQUAL TO KDIR: PRESCRIBED DEPTH.
!| MESH           |<->| MESH STRUCTURE
!| NAMECODE       |-->| NAME OF CALLING CODE (SISYPHE, TELEMEC2D, ETC.)
!| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS
!| NITMAX         |-->| MAXIMUM NUMBER OF ITERATIONS
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| OPTION         |-->| OPTION OF ALGORITHM FOR EDGE-BASED ADVECTION
!|                |   | 1: TRIANGLES, COMPATIBLE WITH ERIA
!|                |   | 2: SEGMENTS, COMPATIBLE WITH NERD
!| OPTSOU         |-->| OPTION FOR SOURCES 1: NORMAL 2: DIRAC
!| PLUIE          |-->| RAIN IN A BIEF_OBJ, IN M/S.
!| RAIN           |-->| IF YES, THERE IS RAIN OR EVAPORATION
!| SMH            |-->| SOURCE TERMS
!| T1             |-->| WORK ARRAY
!| T2             |-->| WORK ARRAY
!| T3             |-->| WORK ARRAY
!| T4             |-->| WORK ARRAY
!| UNSV2D         |-->| INVERSE OF INTEGRAL OF BASIS FUNCTIONS
!| YASMH          |-->| IF(YES) SMH MUST BE TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_POSITIVE_DEPTHS => POSITIVE_DEPTHS
      USE INTERFACE_PARALLEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,OPTSOU,KDIR,OPTION
      INTEGER, INTENT(IN)             :: NITMAX
      INTEGER, INTENT(IN)             :: GLOSEG1(*),GLOSEG2(*)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: LIMPRO(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,HBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: FLULIM(*)
      TYPE(BIEF_MESH),INTENT(INOUT)   :: MESH
      DOUBLE PRECISION, INTENT(INOUT) :: FLOPOINT(MESH%NELMAX,3)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,T3,T4,FLODEL,H,FLBOR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: PLUIE
      TYPE(BIEF_OBJ), INTENT(IN)      :: UNSV2D,HN,SMH
      LOGICAL, INTENT(IN)             :: YASMH,INFO,RAIN,COMPUTE_FLODEL
      CHARACTER(LEN=24)               :: NAMECODE
      LOGICAL, INTENT(IN), OPTIONAL   :: DOFLULIM,DOFLULIMEBE
      DOUBLE PRECISION, INTENT(INOUT),
     &                      OPTIONAL, TARGET :: FLULIMEBE(MESH%NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAKEFLULIM,MAKEFLULIMEBE
      DOUBLE PRECISION, POINTER :: PT_FLULIMEBE(:,:)
!
!-----------------------------------------------------------------------
!
      IF(PRESENT(DOFLULIM)) THEN
        MAKEFLULIM=DOFLULIM
      ELSE
!       DEFAULT VALUE (FLULIM WAS ALWAYS DONE BEFORE INTRODUCING DOFLULIM)
        MAKEFLULIM=.TRUE.
        ENDIF
      IF(PRESENT(DOFLULIMEBE)) THEN
        MAKEFLULIMEBE=DOFLULIMEBE
      ELSE
!       DEFAULT VALUE (FLULIMEBE WAS NOT DONE BEFORE INTRODUCING DOFLULIMEBE)
        MAKEFLULIMEBE=.FALSE.
      ENDIF
!
      IF(MAKEFLULIMEBE.AND..NOT.PRESENT(FLULIMEBE)) THEN
        WRITE(LU,*) 'POSITIVE_DEPTHS: '
        WRITE(LU,*) 'MISSING ARGUMENT FLULIMEBE'
        CALL PLANTE(1)
        STOP
      ELSEIF(PRESENT(FLULIMEBE)) THEN
        PT_FLULIMEBE=>FLULIMEBE
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(OPTION.EQ.1) THEN
        CALL POSITIVE_DEPTHS_ERIA(T1,T2,T3,T4,H,HN,MESH,FLODEL,
     &                            COMPUTE_FLODEL,FLBOR,DT,
     &                            UNSV2D,NPOIN,GLOSEG1,GLOSEG2,
     &                            NBOR,NPTFR,SMH,YASMH,PLUIE,RAIN,
     &                            OPTSOU,LIMPRO,HBOR,KDIR,INFO,
     &                            FLOPOINT,NAMECODE,NITMAX,
     &                            MAKEFLULIMEBE,PT_FLULIMEBE)
      ELSEIF(OPTION.EQ.2) THEN
        CALL POSITIVE_DEPTHS_NERD(T1,T2,T4,H,HN,MESH,FLODEL,
     &                            COMPUTE_FLODEL,FLBOR,DT,
     &                            UNSV2D,NPOIN,GLOSEG1,GLOSEG2,
     &                            NBOR,NPTFR,SMH,YASMH,PLUIE,RAIN,
     &                            OPTSOU,FLULIM,LIMPRO,HBOR,KDIR,INFO,
     &                            FLOPOINT,NAMECODE,NITMAX,MAKEFLULIM)
      ELSE
        WRITE(LU,*) 'UNKNOWN OPTION IN POSITIVE_DEPTHS: ',OPTION
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

