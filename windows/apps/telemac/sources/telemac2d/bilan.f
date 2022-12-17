!                   ****************
                    SUBROUTINE BILAN
!                   ****************
!
     &(MESH,H,WORK,AT,DT,LT,NIT,INFO,MASSES,MSK,MASKEL,EQUA,POROSS,
     & OPTBAN,NPTFR,FLBOR,FLUX_BOUNDARIES,NUMLIQ,NFRLIQ,GAMMA)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    CALCULATES THE BALANCE OF THE MASS OF WATER.
!
!history
!+        14/01/2005
!+
!+   COMPATIBLE COMPUTATION OF FLUXES AT EXITS
!
!history  J-M HERVOUET (LNHE)
!+        27/03/2008
!+        V5P9
!+   PRINTS FLUXES PER BOUNDARY INSTEAD OF FREE AND IMPOSED FLUX
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
!history R. ATA (LNHE)
!+        06/01/2012
!+        V6P0
!+   ADAPTATION FOR FV
!
!history J-M HERVOUET (EDF LAB, LNHE)
!+        06/05/2016
!+        V7P2
!+    Now assuming that FLBOR is initialised if LT=0, and fluxes at
!+    boundaries always computed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| DT             |-->| TIME STEP IN SECONDS
!| EQUA           |-->| STRING DESCRIBING THE EQUATIONS SOLVED
!| FLBOR          |-->| FLUXES AT BOUNDARY POINTS
!| FLUX_BOUNDARIES|-->| FLUXES OF LIQUID BOUNDARIES
!| H              |-->| DEPTH AT TIME N+1.
!| INFO           |-->| IF YES, PRINTING INFORMATIONS
!| LT             |-->| TIME STEP NUMBER
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASSES         |-->| MASS OF TRACER ADDED BY SOURCE TERM
!|                |   | SEE DIFSOU
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NIT            |-->| TOTAL NUMBER OF TIME STEPS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| OPTBAN         |-->| OPTION FOR THE TREATMENT OF TIDAL FLATS
!| POROSS         |-->| POROSITY, PER ELEMENT.
!| WORK           |-->| WORK ARRAY
!| GAMMA          |-->| NEWMARK COEFFICIENT FOR TIME INTEGRATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_BILAN => BILAN
      USE DECLARATIONS_TELEMAC2D, ONLY : FLUX1_OLD,MASSE0,MASSE1,
     &                                   MASSE2,MASENT,MASSET,
     &                                   FLUXN_OLD
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     SIZE OF NUMLIQ AND FLUX_BOUNDARIES IS NFRLIQ BUT NFRLIQ
!     CAN BE 0.
!
      INTEGER, INTENT(IN)            :: LT,NIT,OPTBAN,NPTFR,NFRLIQ
      INTEGER, INTENT(IN)            :: NUMLIQ(*)
      CHARACTER(LEN=20), INTENT(IN)  :: EQUA
      LOGICAL, INTENT(IN)            :: INFO,MSK
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: WORK,FLBOR
      TYPE(BIEF_OBJ), INTENT(IN)     :: H,MASKEL,POROSS
      DOUBLE PRECISION, INTENT(IN)   :: AT,DT,GAMMA
      DOUBLE PRECISION, INTENT(INOUT):: MASSES,FLUX_BOUNDARIES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      DOUBLE PRECISION ERREUR,FLUX1,PERDUE,DENOM,RELATI
!
      DOUBLE PRECISION CONTRIB
      INTRINSIC ABS
!
!-----------------------------------------------------------------------
!
!  COMPATIBLE CALCULATION OF THE MASS OF WATER
!
      IF(LT.NE.0) MASSE1 = MASSE2
!
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        CALL VECTOR(WORK,'=','MASBAS          ',H%ELM,
     &              1.D0,H,H,H,H,H,H,MESH,MSK,MASKEL)
        CALL OS( 'X=XY    ' , X=WORK , Y=H )
      ELSEIF(OPTBAN.EQ.3) THEN
        CALL VECTOR(WORK,'=','MASVEC          ',H%ELM,
     &              1.D0,H,H,H,H,H,H,MESH,.TRUE.,POROSS)
      ELSE
        CALL VECTOR(WORK,'=','MASVEC          ',H%ELM,
     &              1.D0,H,H,H,H,H,H,MESH,MSK,MASKEL)
      ENDIF
      MASSE2 = BIEF_SUM(WORK)
!
      IF(NCSIZE.GT.1) MASSE2 = P_SUM(MASSE2)
!
      IF(LT.EQ.0) THEN
!       INITIALISATION
        MASSE0 = MASSE2
        MASSE1 = MASSE2
        MASENT = 0.D0
        MASSET = 0.D0
        FLUXN_OLD = 0.D0
      ELSE
!       SOURCE TERMS ADDED TO MASS
        IF(NCSIZE.GT.1) MASSES = P_SUM(MASSES)
        MASSET = MASSET + MASSES
      ENDIF
!
!=======================================================================
!
!   CALCULATES FLUXES AT THE LIQUID BOUDNARIES
!
      IF(NFRLIQ.GT.0) THEN
        DO I=1,NFRLIQ
          FLUX_BOUNDARIES(I)=0.D0
        ENDDO
        IF(NPTFR.GT.0) THEN
          DO I=1,NPTFR
!           NOTE: ONE COULD DEFINE FLUX_BOUNDARIES BETWEEN 0 AND NFRLIQ
            IF(NUMLIQ(I).GT.0) THEN
              FLUX_BOUNDARIES(NUMLIQ(I))=
     &        FLUX_BOUNDARIES(NUMLIQ(I))+FLBOR%R(I)
            ENDIF
          ENDDO
        ENDIF
        IF(NCSIZE.GT.1) THEN
          DO I=1,NFRLIQ
            FLUX_BOUNDARIES(I)=P_SUM(FLUX_BOUNDARIES(I))
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!
!   TOTAL FLUX AT THE LIQUID BOUNDARY
!
      FLUX1=0.D0
      IF(NFRLIQ.GT.0) THEN
        DO I=1,NFRLIQ
          FLUX1=FLUX1+FLUX_BOUNDARIES(I)
        ENDDO
      ENDIF
!     FOR FINITE VOLUMES
      FLUX1_OLD=FLUX1
!
!=======================================================================
!
!     IF INITIALISATION, RETURN NOW (NO COMPUTATION YET, NO MASS-BALANCE)
!
      IF(LT.EQ.0) RETURN
!
!=======================================================================
!
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        CONTRIB = DT*( (1-GAMMA)*FLUX1_OLD + GAMMA*FLUX1)
        MASENT = MASENT - CONTRIB
      ELSE
        MASENT = MASENT - FLUX1*DT
      ENDIF
!
!=======================================================================
!
!   COMPUTES ERROR FOR THIS TIME STEP
!
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        ERREUR = MASSE1 + MASSES - MASSE2 - CONTRIB
      ELSE
        ERREUR = MASSE1 + MASSES - MASSE2 - DT*FLUX1
      ENDIF
!
!=======================================================================
!
!   PRINTS:
!
      IF(INFO) THEN
!
!-----------------------------------------------------------------------
!
!     PRINTS THE MASS OF WATER
!
        IF(LT.EQ.0) THEN
!
          CALL ENTETE(7,AT,LT)
          WRITE(LU,2000) MASSE0
!
        ELSE
!
          CALL ENTETE(7,AT,LT)
          WRITE(LU,2010) MASSE2
          IF(NFRLIQ.GT.0) THEN
            DO I=1,NFRLIQ
              WRITE(LU,4020) I,-FLUX_BOUNDARIES(I)
            ENDDO
          ENDIF
          IF(ABS(MASSES).GT.1.D-6) THEN
            WRITE(LU,2031) MASSES
          ENDIF
!         CALCULATES THE RELATIVE OR ABSOLUTE ERROR
          DENOM = MAX(MASSE2,ABS(FLUX1*DT))
          IF(DENOM.GT.1.D-8) THEN
            ERREUR = ERREUR / DENOM
            WRITE(LU,2040) AT,ERREUR
          ELSE
            WRITE(LU,2050) AT,ERREUR
          ENDIF
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  FINAL MASS BALANCE
!
      IF(LT.EQ.NIT.AND.INFO) THEN
!
        CALL ENTETE(8,AT,LT)
!       PERDUE = MASSE0+MASSET+MASENT+MASAJT-MASSE2
        PERDUE = MASSE0+MASSET+MASENT-MASSE2
        DENOM = MAX( MASSE0 , MASSE2 , ABS(MASENT) )
        IF(DENOM.GT.1.D-8) THEN
          RELATI = PERDUE / DENOM
          WRITE(LU,2060) RELATI
        ELSE
          RELATI = PERDUE
          WRITE(LU,2070) RELATI
        ENDIF
        WRITE(LU,2080) MASSE0,MASSE2
        IF(ABS(MASENT).GT.1.D-8) WRITE(LU,2081) MASENT
        IF(ABS(MASSET).GT.1.D-8) WRITE(LU,2082) MASSET
        WRITE(LU,2084) PERDUE
!
      ENDIF
!
!  END OF PRINTS
!
!=======================================================================
!
!  PRINT FORMATS:
!
2000  FORMAT(5X,'INITIAL WATER VOLUME IN THE DOMAIN: ',G16.7,' M3')
!
2010  FORMAT(5X,'VOLUME IN THE DOMAIN :',G16.7,' M3')
!
2031  FORMAT(5X,'ADDITIONAL VOLUME DUE TO SOURCE TERMS: ',G16.7,' M3')
!
2040  FORMAT(5X,'RELATIVE ERROR IN VOLUME AT T = ',G16.4,' S : ',G16.7)
!
2050  FORMAT(5X,'ABSOLUTE ERROR IN VOLUME AT T = ',G16.4,'S: ',G16.7)
!
2060  FORMAT(/,5X,'RELATIVE ERROR CUMULATED ON VOLUME: ',G16.7)
!
2070  FORMAT(/,5X,'ABSOLUTE ERROR CUMULATED ON VOLUME: ',G16.7)
!
2080  FORMAT(/,5X,'INITIAL VOLUME              : ',G16.7,' M3',
     &       /,5X,'FINAL VOLUME                : ',G16.7,' M3')
2081  FORMAT(  5X,'VOLUME THAT ENTERED THE DOMAIN: ',G16.7,' M3',
     &            '  ( IF <0 EXIT )')
2082  FORMAT(  5X,'VOLUME ADDED BY SOURCE TERM   : ',G16.7,' M3')
2084  FORMAT(  5X,'TOTAL VOLUME LOST             : ',G16.7,' M3')
4020  FORMAT(5X,'FLUX BOUNDARY ',I4,': ', G16.7 ,' M3/S',
     &          '  ( >0 : ENTERING  <0 : EXITING )')
!
!=======================================================================
!
      RETURN
      END

