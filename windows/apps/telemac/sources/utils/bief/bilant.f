!                   *****************
                    SUBROUTINE BILANT
!                   *****************
!
     &(H,WORK2,WORK3,DT,LT,NIT,INFO,
     & T,AGGLOT,MASSOU,MASTR0,MASTR2,MASTEN,
     & MASTOU,MSK,MASKEL,MESH,
     & NUMLIQ,NFRLIQ,NPTFR,NAMETRAC,FLBORTRA,MASS_RAIN,TRAIN,
     & MASTRAIN)
!
!***********************************************************************
! BIEF   V7P0
!***********************************************************************
!
!brief    COMPUTES THE MASS BALANCE FOR THE TRACER.
!
!history  J-M HERVOUET (LNHE)     ; C MOULIN (LNH)
!+        10/06/08
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
!+        30/09/20148
!+        V7P0
!+   Some ABS put in the relative accuracy formulas because the mass of
!+   a tracer can be negative (case of vorticity).
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AGGLOT         |-->| MASS-LUMPING ON TRACER
!| DT             |-->| TIME-STEP
!| FLBORTRA       |-->| TRACER FLUXES AT BOUNDARIES
!| H              |-->| DEPTH AT TIME N+1.
!| INFO           |-->| LOGICAL, IF YES, PRINTING INFORMATION ON LISTING
!| LT,NIT         |-->| TIME STEP NUMBER, TOTAL NUMBER OF STEPS.
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASS_RAIN      |<--| MASS OF WATER ADDED BY RAIN OR EVAPORATION
!| MASSOU         |-->| MASS OF TRACER BROUGTH BY SOURCE TERM
!| MASTEN         |<--| WATER MASS ENTERED THROUGH BOUNDARIES
!| MASTRAIN       |<->| TOTAL WATER MASS ENTERED THROUGH BOUNDARIES
!| MASTOU         |<--| WATER MASS CREATED BY SOURCE TERMS
!| MASTR0         |<--| INITIAL TRACER MASS
!| MASTR2         |<--| CURRENT TRACER MASS
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NAMETRAC       |-->| NAMES OF TRACERS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUMLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| T              |-->| TRACER AT TIME T(N+1)
!| TRAIN          |-->| VALUE OF TRACER IN THE RAIN
!| WORK2          |<->| WORK ARRAY
!| WORK3          |<->| WORK ARRAY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BILANT => BILANT
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: LT,NIT,NFRLIQ,NPTFR
      INTEGER, INTENT(IN)            :: NUMLIQ(NPTFR)
      DOUBLE PRECISION, INTENT(IN)   :: DT,MASSOU,AGGLOT,MASS_RAIN,TRAIN
      DOUBLE PRECISION, INTENT(INOUT):: MASTRAIN
      LOGICAL, INTENT(IN)            :: INFO,MSK
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: WORK2,WORK3
      TYPE(BIEF_OBJ), INTENT(IN)     :: H,T,MASKEL
      TYPE(BIEF_OBJ), INTENT(IN)     :: FLBORTRA
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      DOUBLE PRECISION, INTENT(INOUT):: MASTR0,MASTR2,MASTEN,MASTOU
      CHARACTER(LEN=32), INTENT(IN)  :: NAMETRAC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IFRLIQ,IELMT
!
      DOUBLE PRECISION ERREUT,PERDUE,FLUXT,MASBOR,RELATI,DENOM,MASTR1
      DOUBLE PRECISION MASRAI
!     300 IS HERE MAXFRO, THE MAXIMUM NUMBER OF LIQUID BOUNDARIES
      DOUBLE PRECISION FLT_BOUND(300)
!
      INTRINSIC ABS,MAX
!
!-----------------------------------------------------------------------
!
      IELMT = T%ELM
!
!-----------------------------------------------------------------------
!
!  COMPATIBLE COMPUTATION OF THE TRACER QUANTITY AT TIME N+1:
!  TAKES MASS-LUMPING INTO ACCOUNT BUT REQUIRES AGGLOC=AGGLOT
!
      IF(LT.NE.0) MASTR1 = MASTR2
!
      CALL VECTOR(WORK2,'=','MASVEC          ',IELMT,
     &            1.D0-AGGLOT,T,T,T,T,T,T,MESH,MSK,MASKEL)
!     H IS GIVEN HERE FOR A DUMMY STRUCTURE
      CALL VECTOR(WORK3,'=','MASBAS          ',IELMT,
     &                 AGGLOT,H,H,H,H,H,H,MESH,MSK,MASKEL)
!
      CALL OS('X=X+YZ  ',X=WORK2,Y=WORK3,Z=T)
!
      MASTR2 = DOTS(WORK2,H)
      IF(NCSIZE.GT.1) MASTR2=P_SUM(MASTR2)
!
      IF(LT.EQ.0) THEN
        MASTR0   = MASTR2
        MASTR1   = MASTR2
        MASTEN   = 0.D0
        MASTOU   = 0.D0
        MASTRAIN = 0.D0
      ENDIF
!
!=======================================================================
!
!   COMPUTES THE FLUXES (MISSES THE DIFFUSION FLUX,... INVESTIGATE)
!
!=======================================================================
!
      FLUXT=0.D0
!
      IF(LT.GT.0.AND.NFRLIQ.GT.0) THEN
        DO IFRLIQ=1,NFRLIQ
          FLT_BOUND(IFRLIQ)=0.D0
        ENDDO
        IF(NPTFR.GT.0) THEN
          DO I=1,NPTFR
!           NOTE: COULD DEFINE FLUX_BOUNDARIES BETWEEN 0 AND NFRLIQ
            IFRLIQ=NUMLIQ(I)
            IF(IFRLIQ.GT.0) THEN
!             FLBORTRA MUST NOT BE ASSEMBLED IN PARALLEL MODE
              FLT_BOUND(IFRLIQ)=FLT_BOUND(IFRLIQ)+FLBORTRA%R(I)
            ENDIF
          ENDDO
        ENDIF
        IF(NCSIZE.GT.1) THEN
          DO IFRLIQ=1,NFRLIQ
            FLT_BOUND(IFRLIQ)=P_SUM(FLT_BOUND(IFRLIQ))
          ENDDO
        ENDIF
        DO IFRLIQ=1,NFRLIQ
          FLUXT=FLUXT+FLT_BOUND(IFRLIQ)
        ENDDO
      ENDIF
!
!=======================================================================
!
!     COMPUTES THE FLUXES AT THE LIQUID BOUNDARIES
!
      MASTEN = MASTEN - FLUXT * DT
      MASTOU = MASTOU + MASSOU
!
!=======================================================================
!
!     COMPUTES THE TRACER FLUXES THRU THE WALLS (FLUX LAW)
!
!     TEMPORARY, TO BE CODED UP
      MASBOR = 0.D0
!
!=======================================================================
!
!     COMPUTES THE TRACER MASS BROUGHT BY THE RAIN
!     MAX(...,0.D0) BECAUSE EVAPORATION IS NOT TAKEN INTO ACCOUNT
!
!     WILL WORK ONLY IF TRAIN AND RAIN CONSTANT ON ALL THE DOMAIN...
      MASRAI = MAX(MASS_RAIN,0.D0) * TRAIN
      IF(NCSIZE.GT.1) MASRAI=P_SUM(MASRAI)
      MASTRAIN = MASTRAIN + MASRAI
!
!=======================================================================
!
!     COMPUTES THE ERROR ON THE MASS FOR THIS TIMESTEP
!
      ERREUT = MASTR1 + MASSOU + MASRAI - MASTR2 - DT*FLUXT
!
!=======================================================================
!
!     PRINTOUTS :
!
      IF(INFO) THEN
!
!-----------------------------------------------------------------------
!
!     PRINTOUTS FOR THE TRACER
!
        WRITE(LU,*)
        WRITE(LU,*) '                           BALANCE OF ',
     &    TRIM(NAMETRAC(1:16)),' (UNIT: ',TRIM(NAMETRAC(17:32)),' * M3)'
!
        IF(LT.EQ.0) THEN
          WRITE(LU,2090) MASTR0
        ELSE
          WRITE(LU,2160) MASTR1,MASTR2
          IF(NFRLIQ.GT.0) THEN
            DO IFRLIQ=1,NFRLIQ
              WRITE(LU,2110) IFRLIQ,-FLT_BOUND(IFRLIQ)
            ENDDO
          ENDIF
          IF(ABS(MASSOU).GT.1.D-8) THEN
            WRITE(LU,2113) MASSOU
          ENDIF
          IF(ABS(MASRAI).GT.1.D-8) THEN
            WRITE(LU,2166) MASRAI
          ENDIF
          WRITE(LU,2165) ERREUT
!         ABS BECAUSE THE MASS OF A TRACER CAN BE NEGATIVE
!         EXAMPLE : VORTICITY
          DENOM = MAX(ABS(MASTR1),ABS(MASTR2),ABS(FLUXT*DT),
     &                ABS(MASRAI),ABS(MASSOU))
          IF(DENOM.GT.1.D-8) THEN
            ERREUT = ERREUT / DENOM
            WRITE(LU,2120) ERREUT
          ENDIF
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  FINAL MASS BALANCE
!
      IF(LT.EQ.NIT) THEN
!
        WRITE(LU,*)
        WRITE(LU,*) '                     FINAL BALANCE OF ',
     &    TRIM(NAMETRAC(1:16)),' (UNIT: ',TRIM(NAMETRAC(17:32)),' * M3)'
!
        PERDUE = MASTR0+MASTEN+MASBOR+MASTOU+MASTRAIN-MASTR2
        DENOM = MAX(ABS(MASTR0),ABS(MASTR2),ABS(MASTEN),
     &              ABS(MASTOU),ABS(MASTRAIN))
        WRITE(LU,2160) MASTR0,MASTR2
        IF(ABS(MASTEN).GT.1.D-8) WRITE(LU,2161) MASTEN
        IF(ABS(MASTOU).GT.1.D-8) WRITE(LU,2164) MASTOU
        IF(ABS(MASTRAIN).GT.1.D-8) WRITE(LU,2166) MASTRAIN
        WRITE(LU,2165) PERDUE
        IF(DENOM.GT.1.D-8) THEN
          RELATI = PERDUE / DENOM
          WRITE(LU,2120) RELATI
        ENDIF
        WRITE(LU,*)
!
      ENDIF
!
!  END OF THE PRINTOUTS :
!
!=======================================================================
!
!  FORMATS :
!
2090  FORMAT(  5X,'INITIAL QUANTITY OF TRACER   :',G16.7)
2110  FORMAT(  5X,'BOUNDARY ',1I3,' FLUX:         ',G16.7,
     &          ' ( >0 : ENTERING  <0 : EXITING )')
2113  FORMAT(  5X,'QUANTITY CREATED BY SOURCE TERM   :   ' , G16.7 )
2120  FORMAT(  5X,'RELATIVE ERROR                    : ',G16.7)
2160  FORMAT(/,5X,'INITIAL QUANTITY                  : ',G16.7,
     &       /,5X,'FINAL QUANTITY                    : ',G16.7)
2161  FORMAT(  5X,'QUANTITY ENTERED THROUGH LIQ. BND.: ',G16.7,
     &            '  ( IF <0 EXIT )')
2164  FORMAT(  5X,'QUANTITY CREATED BY SOURCE TERM   : ',G16.7)
2166  FORMAT(  5X,'MASS BROUGHT BY THE RAIN          : ',G16.7)
2165  FORMAT(  5X,'TOTAL QUANTITY LOST               : ',G16.7)
!
!=======================================================================
!
      RETURN
      END

