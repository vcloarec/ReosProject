!                   **************
                    SUBROUTINE HVF
!                   **************
!
     &(H,HN,FXMAT,UNSV2D,DT,FXBOR,SMH,YASMH,NSEG,NPOIN,NPTFR,GLOSEG,
     & SIZGLO,NBOR,OPTSOU,T7,MESH,MSK,RAIN,PLUIE)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    FOR FINITE VOLUMES ADVECTION, COMPUTES AN INTERMEDIATE DEPTH
!+                IF THERE ARE SUB-ITERATIONS.
!
!history  CHI-TUAN PHAM (LNHE)
!+        09/02/2009
!+        V5P9
!+   JMH : SEQUENCE IF(MSK) : AVOIDS NEGATIVE DEPTHS
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
!+        24/02/2012
!+        V6P2
!+   Rain and evaporation added (after initiative by O. Boutron, from
!+   Tour du Valat, and O. Bertrand, Artelia group)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP
!| FXBOR          |-->| FLUX ON BOUNDARIES (DEFINED ON ALL DOMAIN
!|                |   | AND ASSEMBLED IN PARALLEL)
!| FXMAT          |-->| MATRIX FOR STORING FLUXES.
!| GLOSEG         |-->| GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT
!| H              |-->| WATER DEPTH AT TIME N+1
!| HN             |-->| WATER DEPTH AT TIME N
!| MESH           |-->| MESh STRUCTURE
!| MSK            |-->| MSK : IF YES, MASKING OF DRY ELEMENTS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| OPTSOU         |-->| OPTION FOR THE TREATMENT OF SOURCES
!|                |   | 1: NORMAL  2: DIRAC
!|                |   | SEE PROPAG IN TELEMAC-2D
!| PLUIE          |-->| RAIN OR EVAPORATION, IN M/S
!| RAIN           |-->| IF YES: RAIN OR EVAPORATION
!| SIZGLO         |-->| FIRST DIMENSION OF GLOSEG
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| T7             |<->| WORK BIEF_OBJ STRUCTURE
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| YASMH          |-->| IF YES, SMH MUST BE TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_HVF => HVF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG,NPOIN,NPTFR,OPTSOU,SIZGLO
      INTEGER, INTENT(IN)             :: GLOSEG(SIZGLO,2)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN),UNSV2D(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXBOR(NPOIN),SMH(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXMAT(NSEG*2),PLUIE(NPOIN)
      LOGICAL, INTENT(IN)             :: YASMH,MSK,RAIN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T7
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,N
!
!-----------------------------------------------------------------------
!
      DO I = 1,NPOIN
        H(I) = HN(I)
      ENDDO
!
!     SOURCES
!
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I = 1,NPOIN
            H(I) = H(I) + DT*SMH(I)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I = 1,NPOIN
            H(I) = H(I) + DT*UNSV2D(I)*SMH(I)
          ENDDO
        ENDIF
      ENDIF
!
!     RAIN OR EVAPORATION
!
      IF(RAIN) THEN
        DO I = 1,NPOIN
          H(I) = H(I) + DT*PLUIE(I)
        ENDDO
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
        DO I = 1,NPOIN
          T7%R(I) = 0.D0
        ENDDO
        DO I = 1,NSEG
          T7%R(GLOSEG(I,1))=T7%R(GLOSEG(I,1))
     &                     -DT*UNSV2D(GLOSEG(I,1))*FXMAT(I)
          T7%R(GLOSEG(I,2))=T7%R(GLOSEG(I,2))
     &                     +DT*UNSV2D(GLOSEG(I,2))*FXMAT(I)
        ENDDO
        CALL PARCOM(T7,2,MESH)
        DO I = 1,NPOIN
          H(I) = H(I) + T7%R(I)
        ENDDO
      ELSE
        DO I = 1,NSEG
          H(GLOSEG(I,1))=H(GLOSEG(I,1))-DT*UNSV2D(GLOSEG(I,1))*FXMAT(I)
          H(GLOSEG(I,2))=H(GLOSEG(I,2))+DT*UNSV2D(GLOSEG(I,2))*FXMAT(I)
        ENDDO
      ENDIF
!
!     ON THE BOUNDARIES : BOUNDARY FLUX TERMS
!
      DO I=1,NPTFR
        N=NBOR(I)
        H(N) = H(N) - DT*UNSV2D(N)*FXBOR(N)
      ENDDO
!
!-----------------------------------------------------------------------
!
!     WHEN NEGATIVE DEPTHS APPEAR WHILE COMPUTING H, THE PREVIOUS
!     VALUE OF H IS KEPT
!
      IF(MSK) THEN
        DO I = 1,NPOIN
          IF(H(I).LT.0.D0) H(I) = MAX(1.D-2,HN(I))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
