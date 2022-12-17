!                   ****************
                    SUBROUTINE CFLVF
!                   ****************
!
     &(DTMAX,HSTART,FXMAT,FXMATPAR,MAS,DT,FXBOR,SMH,YASMH,TAB1,NSEG,
     & NPOIN,NPTFR,GLOSEG,SIZGLO,MESH,MSK,MASKPT,RAIN,PLUIE,FC,
     & NELEM,IKLE,LIMTRA,KDIR,KDDL,FBOR,FSCEXP,TRAIN,NBOR,MINFC,MAXFC,
     & SECU,OPT,WITHABS)
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    COMPUTES THE MAXIMUM TIMESTEP THAT ENABLES
!+                MONOTONY OF THE N ADVECTION SCHEME.
!
!history  JMH
!+        11/04/2008
!+
!+   ADDED YASMH
!
!history  JMH
!+        10/06/2008
!+
!+   ADDED SIZGLO
!
!history  JMH
!+        02/10/2008
!+
!+   PARALLEL MODE (ADDED FXMATPAR, ETC.)
!
!history  C-T PHAM (LNHE)
!+        30/11/2009
!+        V6P0
!+   REFINED COMPUTATION OF DTMAX (AS IN 3D)
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
!history  S. PAVAN & J-M HERVOUET (EDF R&D, LNHE)
!+        06/06/2013
!+        V6P3
!+   New stability criterion based on local min and max of function.
!+   See hardcoded option OPT.
!
!history  S. PAVAN & J-M HERVOUET (EDF LAB, LNHE)
!+        24/10/2013
!+        V7P0
!+   Old stability criterion simplified.
!
!history  S. PAVAN & J-M HERVOUET (EDF LAB, LNHE)
!+        29/04/2014
!+        V7P0
!+   Security coefficient added (for predictor-corrector scheme).
!
!history  S. PAVAN & J-M HERVOUET (EDF LAB, LNHE)
!+        13/10/2014
!+        V7P0
!+   More general formula with parameters COEMIN and COESOU to account
!+   for predictor-corrector scheme.
!
!history  S. PAVAN & J-M HERVOUET (EDF LAB, LNHE)
!+        16/06/2015
!+        V7P1
!+   Option OPT added in argument. Implementation of OPT=2 changed.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        27/09/2016
!+        V7P2
!+   Coefficients COEMIN and COESOU non longer useful. Stability is
!+   done here only for the N scheme.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        29/09/2017
!+        V7P3
!+   Argument WITHABS added to have a new computation of the stability
!+   condition for all predictor-corrector based schemes. This corrects
!+   a possible hack in monotonicity (which never occurred in tests).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP
!| DTMAX          |<--| MAXIMUM TIME STEP FOR STABILITY
!| FXBOR          |-->| BOUNDARY FLUXES
!| FXMAT          |-->| FLUXES
!| FXMATPAR       |-->| FLUXES ASSEMBLED IN PARALLEL
!| GLOSEG         |-->| GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT
!| HSTART         |-->| H AT BEGINNING OF SUB TIME STEP
!| MAS            |-->| INTEGRAL OF TEST FUNCTIONS (=AREA AROUND POINTS)
!| MASKPT         |-->| ARRAY FOR MASKING POINTS
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| OPT            |-->| OPTION: 1=CLASSIC 2=BASED ON MIN AND MAX
!|                |   |         3=BASED ON GIVEN MIN AND MAX
!| PLUIE          |-->| RAIN OR EVAPORATION IN M/S
!| RAIN           |-->| IF YES: RAIN OR EVAPORATION
!| SIZGLO         |-->| FIRST DIMENSION OF GLOSEG
!| SMH            |-->| RIGHT HAND SIDE OF CONTINUITY EQUATION
!| TAB1           |-->| WORK ARRAY
!| WITHABS        |-->| IF YES, STABILITY COMPUTED WITH ABSOLUTE VALUE
!|                |   | OF FLUXES.
!| YASMH          |-->| IF YES, TAKE SHM INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CFLVF => CFLVF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG,NPOIN,NPTFR,SIZGLO,OPT
      INTEGER, INTENT(IN)             :: GLOSEG(SIZGLO,2),NELEM
      INTEGER, INTENT(IN)             :: IKLE(NELEM,3),NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: LIMTRA(NPTFR),KDIR,KDDL
      DOUBLE PRECISION, INTENT(INOUT) :: DTMAX
      DOUBLE PRECISION, INTENT(IN)    :: DT,HSTART(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: MAS(NPOIN),SMH(*)
      DOUBLE PRECISION, INTENT(IN)    :: PLUIE(*)
!                                              NOT NPTFR, SEE TRACVF
      DOUBLE PRECISION, INTENT(IN)    :: FXBOR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXMAT(NSEG),FXMATPAR(NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: FC(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FSCEXP(*)
      DOUBLE PRECISION, INTENT(IN)    :: FBOR(NPTFR),TRAIN,SECU
      LOGICAL, INTENT(IN)             :: YASMH,MSK,RAIN,WITHABS
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TAB1,MINFC,MAXFC
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKPT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,IELEM,I1,I2,I3,N,ISEG
      DOUBLE PRECISION DI,MINEL,MAXEL
!
!     HARDCODED OPTION (1: CLASSICAL 2: BASED ON LOCAL MIN AND MAX
!                                    3: MIN AND MAX GIVEN)
!     OPT=1
!
!-----------------------------------------------------------------------
! DETERMINES MAX AND MIN FOR THE NEW MONOTONICITY CRITERION
!-----------------------------------------------------------------------
!
      IF(OPT.EQ.2.OR.OPT.EQ.3) THEN
        CALL CPSTVC(MASKPT,MINFC)
        CALL CPSTVC(MASKPT,MAXFC)
      ENDIF
!
      IF(OPT.EQ.3) THEN
!
!       HARDCODED MIN AND MAX
!
        DO I=1,NPOIN
          MINFC%R(I)=0.D0
          MAXFC%R(I)=1000.D0
!         CLIPPING IS THERE NECESSARY
          FC(I)=MAX(FC(I),MINFC%R(I))
          FC(I)=MIN(FC(I),MAXFC%R(I))
        ENDDO
!
      ELSEIF(OPT.EQ.2) THEN
!
        DO I=1,NPOIN
          MINFC%R(I)=FC(I)
          MAXFC%R(I)=FC(I)
        ENDDO
!
        DO I=1,NPTFR
          IF(LIMTRA(I).EQ.KDIR) THEN
            N=NBOR(I)
            MINFC%R(N)=MIN(MINFC%R(N),FBOR(I))
            MAXFC%R(N)=MAX(MAXFC%R(N),FBOR(I))
          ENDIF
        ENDDO
!
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          MINEL=MIN(FC(I1),FC(I2),FC(I3))
          MAXEL=MAX(FC(I1),FC(I2),FC(I3))
          MINFC%R(I1)=MIN(MINFC%R(I1),MINEL)
          MINFC%R(I2)=MIN(MINFC%R(I2),MINEL)
          MINFC%R(I3)=MIN(MINFC%R(I3),MINEL)
          MAXFC%R(I1)=MAX(MAXFC%R(I1),MAXEL)
          MAXFC%R(I2)=MAX(MAXFC%R(I2),MAXEL)
          MAXFC%R(I3)=MAX(MAXFC%R(I3),MAXEL)
        ENDDO
!
        IF(YASMH.AND.RAIN) THEN
          DO I=1,NPOIN
            MINFC%R(I)=MIN(MINFC%R(I),FSCEXP(I),TRAIN)
            MAXFC%R(I)=MAX(MAXFC%R(I),FSCEXP(I),TRAIN)
          ENDDO
        ELSEIF(YASMH) THEN
          DO I=1,NPOIN
            MINFC%R(I)=MIN(MINFC%R(I),FSCEXP(I))
            MAXFC%R(I)=MAX(MAXFC%R(I),FSCEXP(I))
          ENDDO
        ELSEIF(RAIN) THEN
          DO I=1,NPOIN
            MINFC%R(I)=MIN(MINFC%R(I),TRAIN)
            MAXFC%R(I)=MAX(MAXFC%R(I),TRAIN)
          ENDDO
        ENDIF
!
!       IN PARALLEL MODE: GLOBAL EXTREMA
!
        IF(NCSIZE.GT.1) THEN
!         GLOBAL EXTREMA
          CALL PARCOM(MAXFC,3,MESH)
          CALL PARCOM(MINFC,4,MESH)
        ENDIF
!
      ENDIF
!
!---------------------------------------------------------------------
!
! COMPUTES THE CRITERION FOR COURANT NUMBER
!
!     USES HERE FXMAT ASSEMBLED IN PARALLEL FOR UPWINDING
!
      IF(OPT.EQ.1) THEN
!
!       CLASSICAL CRITERION, WITH ALTERNATE OPTION IF WITHABS=.TRUE.
!       WITHABS=.FALSE. : MAX(FLUX,0.D0) TAKEN, OR -MIN(FLUX,0.D0)
!       WITHABS=.TRUE. : ABS(FLUX) TAKEN (FOR PREDICTOR-CORRECTOR AND LIPS)
!
        DO I = 1,NPOIN
          TAB1%R(I) = 0.D0
        ENDDO
!
        IF(WITHABS) THEN
          DO I = 1,NSEG
!           HERE WILL BE WRONG IN // IF THE 2 FLUXES OF A SAME SEGMENT
!           ARE CROSSING. THIS IS FORBIDDEN ANYWAY.
            TAB1%R(GLOSEG(I,1)) = TAB1%R(GLOSEG(I,1)) + ABS(FXMAT(I))
            TAB1%R(GLOSEG(I,2)) = TAB1%R(GLOSEG(I,2)) + ABS(FXMAT(I))
          ENDDO
        ELSE
          DO I = 1,NSEG
            IF(FXMATPAR(I).GT.0.D0) THEN
!             FXMAT(I) POSITIVE = MAX(...,0.D0) FOR 1
              TAB1%R(GLOSEG(I,1)) = TAB1%R(GLOSEG(I,1)) + FXMAT(I)
            ELSE
!             - FXMAT(I) POSITIVE = MAX(...,0.D0) FOR 2
              TAB1%R(GLOSEG(I,2)) = TAB1%R(GLOSEG(I,2)) - FXMAT(I)
            ENDIF
          ENDDO
        ENDIF
!
        IF(NCSIZE.GT.1) CALL PARCOM(TAB1,2,MESH)
!
!       MASKS TAB1
        IF(MSK) THEN
          CALL OS('X=XY    ',X=TAB1,Y=MASKPT)
        ENDIF
!
!       STABILITY (AND MONOTONICITY) CRITERION
        DTMAX = DT
!
!       SEE RELEASE NOTES 5.7, CRITERION AT THE END OF 4.4 PAGE 33
!       BUT HERE THE FINAL H IS NOT H(N+1) BUT A FUNCTION OF DTMAX ITSELF
!       H FINAL = HSTART + DTMAX/DT *(H-HSTART)
!
        IF(YASMH.AND.RAIN) THEN
          IF(WITHABS) THEN
            DO I = 1,NPOIN
              TAB1%R(I)=TAB1%R(I)+ABS(FXBOR(I))
     &                           +ABS(SMH(I)+PLUIE(I))
            ENDDO
          ELSE
            DO I = 1,NPOIN
              TAB1%R(I)=TAB1%R(I)+MAX(FXBOR(I),0.D0)
     &                           -MIN(SMH(I)+PLUIE(I),0.D0)
            ENDDO
          ENDIF
        ELSEIF(YASMH) THEN
          IF(WITHABS) THEN
            DO I = 1,NPOIN
              TAB1%R(I)=TAB1%R(I)+ABS(FXBOR(I))+ABS(SMH(I))
            ENDDO
          ELSE
            DO I = 1,NPOIN
              TAB1%R(I)=TAB1%R(I)+MAX(FXBOR(I),0.D0)-MIN(SMH(I),0.D0)
            ENDDO
          ENDIF
        ELSEIF(RAIN) THEN
          IF(WITHABS) THEN
            DO I = 1,NPOIN
              TAB1%R(I)=TAB1%R(I)+ABS(FXBOR(I))+ABS(PLUIE(I))
            ENDDO
          ELSE
            DO I = 1,NPOIN
              TAB1%R(I)=TAB1%R(I)+MAX(FXBOR(I),0.D0)-MIN(PLUIE(I),0.D0)
            ENDDO
          ENDIF
        ELSE
          IF(WITHABS) THEN
            DO I = 1,NPOIN
              TAB1%R(I)=TAB1%R(I)+ABS(FXBOR(I))
            ENDDO
          ELSE
            DO I = 1,NPOIN
              TAB1%R(I)=TAB1%R(I)+MAX(FXBOR(I),0.D0)
            ENDDO
          ENDIF
        ENDIF
!
        DO I = 1,NPOIN
          IF(TAB1%R(I).GT.1.D-20) THEN
            MINFC%R(I)=SECU*MAS(I)*HSTART(I)/TAB1%R(I)
            DTMAX=MIN(DTMAX,MINFC%R(I))
          ELSE
            MINFC%R(I)=DT
          ENDIF
        ENDDO
!
      ELSEIF(OPT.EQ.2.OR.OPT.EQ.3) THEN
!
        DTMAX = DT
!
!       NEW CRITERION, COMPUTING BI (SEE RELEASE NOTES 7.1)
!
!       WITH MAXFC%R
!
        DO I = 1,NPOIN
          TAB1%R(I) = 0.D0
        ENDDO
!
        DO ISEG=1,NSEG
          I=GLOSEG(ISEG,1)
          J=GLOSEG(ISEG,2)
          IF(FXMATPAR(ISEG).LT.0.D0) THEN
            TAB1%R(I)=TAB1%R(I)
     &               -FXMAT(ISEG)*(FC(J)-MAXFC%R(I))
            TAB1%R(J)=TAB1%R(J)
     &               +FXMAT(ISEG)*(FC(J)-MAXFC%R(J))
          ELSE
            TAB1%R(I)=TAB1%R(I)
     &               -FXMAT(ISEG)*(FC(I)-MAXFC%R(I))
            TAB1%R(J)=TAB1%R(J)
     &               +FXMAT(ISEG)*(FC(I)-MAXFC%R(J))
          ENDIF
        ENDDO
!
        DO I=1,NPTFR
          IF(LIMTRA(I).EQ.KDIR) THEN
            N=NBOR(I)
!           FXBOR IS HERE IN GLOBAL NUMBERING
            TAB1%R(N)=TAB1%R(N)
     &               -MIN(FXBOR(N),0.D0)*(FBOR(I)-MAXFC%R(N))
          ELSEIF(LIMTRA(I).EQ.KDDL) THEN
            N=NBOR(I)
            TAB1%R(N)=TAB1%R(N)
     &               -MAX(FXBOR(N),0.D0)*(FC(N)  -MAXFC%R(N))
          ENDIF
        ENDDO
!
        IF(YASMH) THEN
          DO I = 1,NPOIN
            TAB1%R(I)=TAB1%R(I)
     &               +MAX(SMH(I),0.D0)*(FSCEXP(I)-MAXFC%R(I))
     &               +MIN(SMH(I),0.D0)*(FC(I)    -MAXFC%R(I))
          ENDDO
        ENDIF
!
        IF(RAIN) THEN
          DO I = 1,NPOIN
            TAB1%R(I)=TAB1%R(I)
     &               +MAX(PLUIE(I),0.D0)*(TRAIN-MAXFC%R(I))
     &               +MIN(PLUIE(I),0.D0)*(FC(I)-MAXFC%R(I))
          ENDDO
        ENDIF
!
        IF(NCSIZE.GT.1) CALL PARCOM(TAB1,2,MESH)
!
!       MASKS TAB1
!
        IF(MSK) CALL OS('X=XY    ',X=TAB1,Y=MASKPT)
!
        DO I= 1,NPOIN
          DI=TAB1%R(I)/MAS(I)
          IF(DI.GT.1.D-12) THEN
            DTMAX = MIN(DTMAX,HSTART(I)*(MAXFC%R(I)-FC(I))/DI)
          ENDIF
        ENDDO
!
!       WITH MINFC%R
!
        DO I = 1,NPOIN
          TAB1%R(I) = 0.D0
        ENDDO
!
        DO ISEG=1,NSEG
          I=GLOSEG(ISEG,1)
          J=GLOSEG(ISEG,2)
          IF(FXMATPAR(ISEG).LT.0.D0) THEN
            TAB1%R(I)=TAB1%R(I)
     &               -FXMAT(ISEG)*(FC(J)-MINFC%R(I))
            TAB1%R(J)=TAB1%R(J)
     &               +FXMAT(ISEG)*(FC(J)-MINFC%R(J))
          ELSE
            TAB1%R(I)=TAB1%R(I)
     &               -FXMAT(ISEG)*(FC(I)-MINFC%R(I))
            TAB1%R(J)=TAB1%R(J)
     &               +FXMAT(ISEG)*(FC(I)-MINFC%R(J))
          ENDIF
        ENDDO
!
        DO I=1,NPTFR
          IF(LIMTRA(I).EQ.KDIR) THEN
            N=NBOR(I)
!           FXBOR IS HERE IN GLOBAL NUMBERING
            TAB1%R(N)=TAB1%R(N)
     &               -MIN(FXBOR(N),0.D0)*(FBOR(I)-MINFC%R(N))
          ELSEIF(LIMTRA(I).EQ.KDDL) THEN
            N=NBOR(I)
            TAB1%R(N)=TAB1%R(N)
     &               -MAX(FXBOR(N),0.D0)*(FC(N)  -MINFC%R(N))
          ENDIF
        ENDDO
!
        IF(YASMH) THEN
          DO I = 1,NPOIN
            TAB1%R(I)=TAB1%R(I)
     &               +MAX(SMH(I),0.D0)*(FSCEXP(I)-MINFC%R(I))
     &               +MIN(SMH(I),0.D0)*(FC(I)    -MINFC%R(I))
          ENDDO
        ENDIF
!
        IF(RAIN) THEN
          DO I = 1,NPOIN
            TAB1%R(I)=TAB1%R(I)
     &               +MAX(PLUIE(I),0.D0)*(TRAIN-MINFC%R(I))
     &               +MIN(PLUIE(I),0.D0)*(FC(I)-MINFC%R(I))
          ENDDO
        ENDIF
!
        IF(NCSIZE.GT.1) CALL PARCOM(TAB1,2,MESH)
!
!       MASKS TAB1
!
        IF(MSK) CALL OS('X=XY    ',X=TAB1,Y=MASKPT)
!
        DO I= 1,NPOIN
          DI=TAB1%R(I)/MAS(I)
          IF(DI.LT.-1.D-12) THEN
            DTMAX = MIN(DTMAX,HSTART(I)*(MINFC%R(I)-FC(I))/DI)
          ENDIF
        ENDDO
!
      ELSE
!
        WRITE(LU,*) 'UNKNOWN OPTION IN CFLVF: ',OPT
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

