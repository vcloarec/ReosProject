!                   ********************
                    SUBROUTINE USER_BORD
!                   ********************
!
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,
     & ZF,NBOR,TRA05,TRA06,LIHBOR,LIUBOR,LITBOR,
     & XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2,TEMPS,NDEBIT,NCOTE,NVITES,
     & NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,MASK,MESH,EQUA,
     & NOMIMP)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    USER MODIFIES THE BOUNDARY CONDITIONS ARRAYS
!+                WHEN THEY VARY IN TIME.
!+
!
!history  J-M HERVOUET (LNHE)
!+        27/03/2008
!+        V5P9
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EQUA           |-->| STRING DESCRIBING THE EQUATIONS SOLVED
!| H              |-->| DEPTH AT TIME N
!| HBOR           |<->| PRESCRIBED DEPTH
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LITBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON TRACERS
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| MASK           |-->| BLOCK OF MASKS FOR DIFFERENT BOUNDARY CONDITIONS
!| MESH           |-->| MESH STRUCTURE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NCOTE          |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NDEBIT         |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NOMIMP         |-->| NAME OF LIQUID BOUNDARIES FILE
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NPTFR2         |-->| NUMBER OF QUADRATIC BOUNDARY POINTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| NTRACE         |-->| NUMBER OF BOUNDARIES WITH TRACER PRESCRIBED
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| NVITES         |-->| NUMBER OF BOUNDARIES WITH VELOCITY PRESCRIBED
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| PROVEL         |-->| OPTION FOR VELOCITY PROFILES
!| TBOR           |<--| BLOCK WITH PRESCRIBED VALUES OF TRACERS
!| TEMPS          |-->| TIME IN SECONDS
!| TRA05          |-->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| TRA06          |-->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| U              |-->| X-COMPONENT OF VELOCITY AT TIME N
!| V              |-->| Y-COMPONENT OF VELOCITY AT TIME N
!| UBOR           |<->| X-COMPONENT OF PRESCRIBED VELOCITY
!| VBOR           |<->| Y-COMPONENT OF PRESCRIBED VELOCITY
!| XNEBOR         |-->| X-COMPONENT OF NORMAL VECTOR AT BOUNDARY NODES
!| YNEBOR         |-->| Y-COMPONENT OF NORMAL VECTOR AT BOUNDARY NODES
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_USER_BORD => USER_BORD
!     USE DECLARATIONS_TELEMAC2D, ONLY: STA_DIS_CURVES,PTS_CURVES,QZ,
!    &                                  FLUX_BOUNDARIES,MAXFRO,FRTYPE,
!    &                                  TIDALTYPE,BOUNDARY_COLOUR
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_MIN
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NPTFR,NDEBIT,NCOTE,NVITES,NTRACE
      INTEGER, INTENT(IN) :: KENT,KENTU,NFRLIQ,NTRAC,NPTFR2
      INTEGER, INTENT(IN) :: PROVEL(*)
      INTEGER, INTENT(INOUT) :: LIHBOR(NPTFR),LIUBOR(NPTFR2)
      INTEGER, INTENT(IN) :: NUMLIQ(NPTFR),NBOR(NPTFR2)
      DOUBLE PRECISION, INTENT(IN) :: TEMPS
      DOUBLE PRECISION, INTENT(IN) :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      CHARACTER(LEN=20), INTENT(IN)   :: EQUA
      CHARACTER(LEN=PATH_LEN), INTENT(IN)  :: NOMIMP
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR2,2),VBOR(NPTFR2,2)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: H,U,V,TRA05,TRA06,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASK,LITBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!

!
!-----------------------------------------------------------------------
!
      RETURN
      END
