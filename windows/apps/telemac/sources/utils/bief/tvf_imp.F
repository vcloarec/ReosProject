!                   ******************
                    SUBROUTINE TVF_IMP
!                   ******************
!
     &(F,FC,FXMAT,FXMATPAR,
     & DT,FXBOR,HNP1MT,FBOR,SMH,YASMH,FSCEXP,
     & NSEG,NPOIN,NPTFR,GLOSEG,SIZGLO,NBOR,LIMTRA,KDIR,KDDL,OPTSOU,
     & IOPT2,FLBORTRA,SURNIT,MESH,SF,RAIN,PLUIE,TRAIN,TETAF,INFOGT,
     & VOLU2D,SM,PSIEXP,AM2,BB,SLVPSI,
     & PREDICTOR,CORRECTOR,ICOR,NCOR,MASSOU)
!
!***********************************************************************
! BIEF   V7P2
!***********************************************************************
!
!brief    Semi-implicit distributive scheme.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        26/06/2015
!+        V7P1
!+   First version.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        01/09/2015
!+        V7P1
!+   Optimisation. Matrix simplified into a diagonal except for the last
!+   correction. Does not spoil mass conservation and results virtually
!+   unchanged.
!+   Jacobi solver programmed here, when GMRES or direct solver
!+   is not asked.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        30/06/2016
!+        V7P2
!+   Computing the global extrema for clipping the results that may be
!+   out of range due to truncation errors. True errors may be hidden
!+   but will be seen with the mass balance.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        30/06/2016
!+        V7P2
!+   Relaxing the clipping for monotony in case of rain (evaporation
!+   may break the monotony).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME-STEP
!| F              |<--| VALUES OF F AT TIME N+1 OF SUB-ITERATION
!| FBOR           |-->| VALUES OF F AT THE PRESCRIBED BOUNDARIES
!| FC             |-->| VALUES OF F AT TIME N OF SUB-ITERATION
!| FLBORTRA       |<->| FLUX OF TRACER AT THE BOUNDARIES
!| FSCEXP         |-->| EXPLICIT SOURCE TERM FOR F
!| FXBOR          |-->| FLUXES ON BOUNDARIES
!| FXMAT          |-->| FLUXES (NON ASSEMBLED IN PARALLEL)
!| FXMATPAR       |-->| FLUXES (ASSEMBLED IN PARALLEL)
!| GLOSEG         |-->| GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT.
!| ICOR           |-->| CURRENT CORRECTION NUMBER
!| INFOGT         |-->| IF YES, PRINT INFORMATION ON SOLVER
!| IOPT2          |-->| 0: CONSERVATIVE ADVECTION FIELD
!|                |   | 1: NON CONSERVATIVE ADVECTION FIELD
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| LIMTRA         |-->| TECHNICAL BOUNDARY CONDITIONS FOR TRACERS
!| MASSOU         |-->| MASS ADDED BY SOURCE TERM
!| MESH           |-->| MESH STRUCTURE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NCOR           |-->| TOTAL NUMBER OF CORRECTIONS ASKED
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| OPTSOU         |-->| TYPE OF SOURCES
!|                |   | 1: NORMAL
!|                |   | 2: DIRAC
!| PLUIE          |-->| RAIN OR EVAPORATION, IN M/S
!| RAIN           |-->| IF YES: RAIN OR EVAPORATION
!| SF             |<->| BIEF_OBJ STRUCTURE OF F
!| SIZGLO         |-->| FIRST DIMENSION OF GLOSEG
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SURNIT         |-->| SURNIT=1/NIT
!| TETAF          |-->| IMPLICITATION COEFFICIENT ON F
!| HNP1MT         |-->| INTERMEDIATE DEPTH H(N+1-TETA)
!| TRAIN          |-->| VALUE OF TRACER IN RAIN
!| VOLU2D         |-->| INTEGRALS OF TEST FUNCTIONS, NOT ASSEMBLED IN //
!| YASMH          |-->| IF YES, SMH MUST BE TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_TVF_IMP => TVF_IMP
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG,NPOIN,NPTFR,KDIR,KDDL
      INTEGER, INTENT(IN)             :: SIZGLO,OPTSOU,IOPT2
      INTEGER, INTENT(IN)             :: ICOR,NCOR
      INTEGER, INTENT(IN)             :: GLOSEG(SIZGLO,2)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMTRA(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,SURNIT,TRAIN,TETAF(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLBORTRA(NPTFR),MASSOU
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: FC(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HNP1MT(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SMH(*)
      DOUBLE PRECISION, INTENT(IN)    :: VOLU2D(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: PLUIE(*),PSIEXP(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FSCEXP(*)
      DOUBLE PRECISION, INTENT(IN)    :: FBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: FXMAT(NSEG),FXMATPAR(NSEG)
      LOGICAL, INTENT(IN)             :: YASMH,RAIN,INFOGT
      LOGICAL, INTENT(IN)             :: PREDICTOR,CORRECTOR
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: SF,SM,AM2,BB
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(SLVCFG),    INTENT(INOUT)  :: SLVPSI
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,N,I1,I2
      DOUBLE PRECISION NORMR,NORMB,FMIN,FMAX
      TYPE(BIEF_OBJ), POINTER :: BB1,SURDIAG,R
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
      BB1    =>BB%ADR(1)%P
      R      =>BB%ADR(2)%P
      SURDIAG=>BB%ADR(3)%P
      CALL CPSTVC(SF,BB1)
      CALL CPSTVC(SF,R)
      CALL CPSTVC(SF,SURDIAG)
!
!-----------------------------------------------------------------------
!
!     COMPUTING GLOBAL EXTREMA, FOR CLIPPING TRUNCATION ERRORS
!
      FMIN=FC(1)
      FMAX=FC(1)
      DO I=1,NPOIN
        FMIN=MIN(FMIN,FC(I))
        FMAX=MAX(FMAX,FC(I))
      ENDDO
      DO I=1,NPTFR
        IF(LIMTRA(I).EQ.KDIR) THEN
          FMIN=MIN(FMIN,FBOR(I))
          FMAX=MAX(FMAX,FBOR(I))
        ENDIF
      ENDDO
      IF(YASMH) THEN
        DO I=1,NPOIN
          FMIN=MIN(FMIN,FSCEXP(I))
          FMAX=MAX(FMAX,FSCEXP(I))
        ENDDO
      ENDIF
      IF(RAIN) THEN
        FMIN=MIN(FMIN,TRAIN)
        FMAX=MAX(FMAX,TRAIN)
      ENDIF
      IF(NCSIZE.GT.1) THEN
        FMIN=P_MIN(FMIN)
        FMAX=P_MAX(FMAX)
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(PREDICTOR) THEN
!
        IF(IOPT2.EQ.0) THEN
!         CONSERVATIVE ADVECTION FIELD
          DO I = 1,NPOIN
            F(I) = FC(I)
          ENDDO
        ELSE
          WRITE(LU,*) 'TVF_IMP: UNKNOWN OPTION'
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!     BUILDING THE MATRIX AND THE RIGHT-HAND SIDE
!     THE MATRIX IS NOT DONE WITH A CALL MATRIX
!     SO ITS FEATURES HAVE TO BE HARDCODED HERE
!
      AM2%X%DIM1=2*NSEG
      AM2%X%DIM2=1
      AM2%TYPEXT='Q'
      AM2%TYPDIA='Q'
      AM2%ELMLIN=11
      AM2%ELMCOL=11
      CALL CPSTVC(SF,AM2%D)
!
!     DIAGONAL OF MATRIX
!
      DO I=1,NPOIN
        AM2%D%R(I)=HNP1MT(I)*VOLU2D(I)
      ENDDO
!
!     RIGHT HAND SIDE
!
!     TERM FROM THE DERIVATIVE IN TIME
      IF(PREDICTOR) THEN
        DO I=1,NPOIN
          SM%R(I)=AM2%D%R(I)*FC(I)
        ENDDO
      ELSEIF(CORRECTOR) THEN
!       THE PREDICTOR IS TAKEN, AT THIS LEVEL IT IS STILL F
        DO I=1,NPOIN
          SM%R(I)=AM2%D%R(I)*F(I)
        ENDDO
      ELSE
        WRITE(LU,*) 'TVF_IMP, CHECK ARGUMENTS PREDICTOR, CORRECTOR'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     IMPLICIT BOUNDARY TERM
!
      DO I=1,NPTFR
        N=NBOR(I)
        IF(LIMTRA(I).EQ.KDIR) THEN
          AM2%D%R(N)=AM2%D%R(N)-DT*TETAF(N)*FXBOR(I)
        ENDIF
      ENDDO
!
!     DIAGONAL AND OFF-DIAGONAL TERMS
!
      IF(ICOR.LT.NCOR) THEN
!       SYSTEM SIMPLIFIED, MASS SPOILED, BUT IT WILL BE CORRECTED AFTER
!       BY THE LAST CORRECTION
!       HERE EXTRA-DIAGONAL TERMS NOT BUILT
        DO I=1,NSEG
          I1=GLOSEG(I,1)
          I2=GLOSEG(I,2)
          IF(FXMATPAR(I).LT.0.D0) THEN
            AM2%D%R(I1) = AM2%D%R(I1) - DT*TETAF(I1)*FXMAT(I)
          ELSE
            AM2%D%R(I2) = AM2%D%R(I2) + DT*TETAF(I2)*FXMAT(I)
          ENDIF
        ENDDO
      ELSE
!       NO SIMPLIFICATION, REAL MATRIX
        DO I=1,NSEG
          I1=GLOSEG(I,1)
          I2=GLOSEG(I,2)
          IF(FXMATPAR(I).LT.0.D0) THEN
            AM2%D%R(I1) = AM2%D%R(I1) - DT*TETAF(I1)*FXMAT(I)
            AM2%X%R(I)=DT*TETAF(I2)*FXMAT(I)
            AM2%X%R(I+NSEG)=0.D0
          ELSE
            AM2%D%R(I2) = AM2%D%R(I2) + DT*TETAF(I2)*FXMAT(I)
            AM2%X%R(I)=0.D0
            AM2%X%R(I+NSEG)=-DT*TETAF(I1)*FXMAT(I)
          ENDIF
        ENDDO
!
      ENDIF
!
!     SOURCES IN CONTINUITY EQUATION (SMH)
!
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
!         SEE DIFSOU, FSCEXP CONTAINS THE VALUE OF THE TRACER AT THE SOURCE
          DO I=1,NPOIN
            AM2%D%R(I)=AM2%D%R(I)
     &                +DT*TETAF(I)*VOLU2D(I)*MAX(SMH(I),0.D0)
            SM%R(I)=SM%R(I)+(FSCEXP(I)-(1.D0-TETAF(I))*FC(I))
     &                     *VOLU2D(I)*DT*MAX(SMH(I),0.D0)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            AM2%D%R(I)=AM2%D%R(I)+DT*TETAF(I)*MAX(SMH(I),0.D0)
            SM%R(I)=SM%R(I)+(FSCEXP(I)-(1.D0-TETAF(I))*FC(I))
     &                     *DT*MAX(SMH(I),0.D0)
          ENDDO
        ENDIF
      ENDIF
!
!     TERMES BII * CIN ET BIJ * CJN
!
!     ASSEMBLED CONTRIBUTION, DONE BY FLUX_EF_VF_3, POSSIBLY
!     WITH DERIVATIVE IN TIME.
      DO I=1,NPOIN
        SM%R(I)=SM%R(I)-DT*PSIEXP(I)
      ENDDO
!
!     ADD FLUX ON BOUNDARY AND OTHER TERMS
!
      DO I=1,NPTFR
        IF(LIMTRA(I).EQ.KDIR) THEN
          N=NBOR(I)
          SM%R(N)=SM%R(N)+DT*FXBOR(I)*((1.D0-TETAF(N))*FC(N)-FBOR(I))
        ENDIF
      ENDDO
!
!     RAIN-EVAPORATION
!
      IF(RAIN) THEN
        DO I=1,NPOIN
          IF(PLUIE(I).GT.0.D0) THEN
!           REAL RAIN, VALUE IN RAIN CONSIDERED...
            SM%R(I)=SM%R(I)+DT*VOLU2D(I)*PLUIE(I)
     &          *(TRAIN-(1.D0-TETAF(I))*FC(I))
          ELSE
!           EVAPORATION, VALUE IN RAIN NOT CONSIDERED...
            SM%R(I)=SM%R(I)+DT*VOLU2D(I)*PLUIE(I)
     &          *(     -(1.D0-TETAF(I))*FC(I))
          ENDIF
          AM2%D%R(I)=AM2%D%R(I)+DT*TETAF(I)*VOLU2D(I)*PLUIE(I)
        ENDDO
      ENDIF
!
!     TIDAL FLATS
!
      CALL OS('X=Y     ',X=BB1,Y=AM2%D)
      IF(NCSIZE.GT.1) CALL PARCOM(BB1,2,MESH)
      DO I=1,NPOIN
!       SEE PRECD1 EPSILON HERE MUST BE GREATER THAN 1.D-20, OR PRECD1 WILL
!       DO THE CLIPPING ITSELF, IN A LESS CONSISTANT WAY
!       THE TEST MUST BE DONE ON THE ASSEMBLED DIAGONAL
        IF(BB1%R(I).LT.1.D-15) THEN
!         VOLU2D IS A NON ASSEMBLED COEFFICIENT, ANY SUCH COEFFICIENT WOULD WORK...
!         DRY POINT THAT RECEIVES NO WATER, F=FC IS GIVEN AS EQUATION
          AM2%D%R(I)=VOLU2D(I)
          SM%R(I)   =VOLU2D(I)*FC(I)
        ENDIF
      ENDDO
!
!     SOLVING THE FINAL LINEAR SYSTEM
!
      IF(ICOR.LT.NCOR) THEN
!       HERE ONE ITERATION OF JACOBI, OFF-DIAGONAL TERMS BUILT ON THE SPOT
        DO I=1,NSEG
          I1=GLOSEG(I,1)
          I2=GLOSEG(I,2)
          IF(FXMATPAR(I).LT.0.D0) THEN
            SM%R(I1)=SM%R(I1)-DT*TETAF(I2)*FXMAT(I)*F(I2)
          ELSE
            SM%R(I2)=SM%R(I2)+DT*TETAF(I1)*FXMAT(I)*F(I1)
          ENDIF
        ENDDO
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(AM2%D,2,MESH)
          CALL PARCOM(SM,2,MESH)
        ENDIF
        DO I=1,NPOIN
          F(I)=SM%R(I)/AM2%D%R(I)
        ENDDO
      ELSE
        IF(SLVPSI%SLV.EQ.7.OR.SLVPSI%SLV.EQ.8) THEN
#if defined COMPAD
          CALL AD_SOLVE(SF,AM2,SM,BB,SLVPSI,INFOGT,MESH,AM2)
#else
          CALL SOLVE(SF,AM2,SM,BB,SLVPSI,INFOGT,MESH,AM2)
#endif
        ELSE
!         IF NOT GMRES OR DIRECT, JACOBI SOLUTION
          N=0
          CALL OS('X=Y     ',X=BB1,Y=SM)
          CALL OS('X=Y     ',X=SURDIAG,Y=AM2%D)
          IF(NCSIZE.GT.1) THEN
            CALL PARCOM(SURDIAG,2,MESH)
            CALL PARCOM(BB1,2,MESH)
          ENDIF
          CALL OS('X=1/Y   ',X=SURDIAG,Y=SURDIAG)
          NORMB=SQRT(P_DOTS(BB1,BB1,MESH))
100       CONTINUE
          N=N+1
!         ONE ITERATION OF JACOBI
          CALL OS('X=Y     ',X=BB1,Y=SM)
          DO I=1,NSEG
            I1=GLOSEG(I,1)
            I2=GLOSEG(I,2)
            IF(FXMATPAR(I).LT.0.D0) THEN
              BB1%R(I1)=BB1%R(I1)-AM2%X%R(I     )*F(I2)
            ELSE
              BB1%R(I2)=BB1%R(I2)-AM2%X%R(I+NSEG)*F(I1)
            ENDIF
          ENDDO
          IF(NCSIZE.GT.1) CALL PARCOM(BB1,2,MESH)
!         NEW SOLUTION IN BB1
          DO I=1,NPOIN
            BB1%R(I)=BB1%R(I)*SURDIAG%R(I)
          ENDDO
!         END OF ONE ITERATION, COMPUTING THE RESIDUAL
!         AX-B = (A%D+A%X)X-B = A%X*(NEW F - OLD F)
          CALL OS('X=0     ',X=R)
          DO I=1,NSEG
            I1=GLOSEG(I,1)
            I2=GLOSEG(I,2)
            IF(FXMATPAR(I).LT.0.D0) THEN
              R%R(I1)=R%R(I1)-AM2%X%R(I     )*(F(I2)-BB1%R(I2))
            ELSE
              R%R(I2)=R%R(I2)-AM2%X%R(I+NSEG)*(F(I1)-BB1%R(I1))
            ENDIF
          ENDDO
          IF(NCSIZE.GT.1) CALL PARCOM(R,2,MESH)
          NORMR=SQRT(P_DOTS(R,R,MESH))
!         COPY OF NEW SOLUTION ON F, WITH CLIPPING WITH GLOBAL EXTREMA
!         TO COPE WITH TRUNCATION ERRORS. IF CLIPPING TRUE ERRORS IT
!         WILL DO MASS ERRORS
          IF(RAIN) THEN
!           NO CLIPPING HERE SINCE EVAPORATION MAY BREAK MONOTONY
            DO I=1,NPOIN
              F(I)=BB1%R(I)
            ENDDO
          ELSE
            DO I=1,NPOIN
              F(I)=MAX(MIN(BB1%R(I),FMAX),FMIN)
            ENDDO
          ENDIF
          IF(N.LT.SLVPSI%NITMAX.AND.NORMR.GT.SLVPSI%EPS*NORMB) THEN
            GO TO 100
          ENDIF
!         DONE !
          IF(INFOGT) THEN
            IF(NORMR.GT.SLVPSI%EPS*NORMB) THEN
              IF(NORMB.GT.1.D0) THEN
                WRITE(LU,*)
     &            'JACOBI: MAXIMUM ITERATIONS REACHED ',N,
     &            ' ITERATIONS, RELATIVE PRECISION = ',NORMR/NORMB
              ELSE
                WRITE(LU,*)
     &            'JACOBI: MAXIMUM ITERATIONS REACHED ',N,
     &            ' ITERATIONS, ABSOLUTE PRECISION = ',NORMR
              ENDIF
            ELSE
              IF(NORMB.GT.1.D0) THEN
                WRITE(LU,*) 'JACOBI:',N,
     &            ' ITERATIONS, RELATIVE PRECISION = ',NORMR/NORMB
              ELSE
                WRITE(LU,*) 'JACOBI:',N,
     &            ' ITERATIONS, ABSOLUTE PRECISION = ',NORMR
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
!
!     ON EXITS, EXITING FLUX DEPENDS ON F AND FN AT EVERY SUB-TIME
!     STEP, SO IT MUST BE COMPUTED AT THIS LEVEL.
!     THE CASE KDIR IS TREATED IN CVTRVF
!
!     FLUX AND ADDED MASS FOR MASS BALANCE
!
      IF(ICOR.EQ.NCOR) THEN
!       BOUNDARIES
        DO I=1,NPTFR
          IF(LIMTRA(I).EQ.KDIR) THEN
            FLBORTRA(I)=FLBORTRA(I)+FXBOR(I)*FBOR(I)*SURNIT
          ELSEIF(LIMTRA(I).EQ.KDDL) THEN
            N=NBOR(I)
            FLBORTRA(I)=FLBORTRA(I)
     &            +FXBOR(I)*(TETAF(N)*F(N)+(1.D0-TETAF(N))*FC(N))*SURNIT
          ENDIF
        ENDDO
!       SOURCES
        IF(YASMH) THEN
!         FOR MASS BALANCE
          IF(OPTSOU.EQ.1) THEN
            DO I=1,MESH%NPOIN
              IF(SMH(I).GE.0.D0) THEN
                MASSOU=MASSOU+VOLU2D(I)*FSCEXP(I)*DT*SMH(I)
              ELSE
                MASSOU=MASSOU+VOLU2D(I)*
     &                (TETAF(I)*F(I)+(1.D0-TETAF(I))*FC(I))*DT*SMH(I)
              ENDIF
            ENDDO
          ELSEIF(OPTSOU.EQ.2) THEN
            DO I=1,MESH%NPOIN
              IF(SMH(I).GE.0.D0) THEN
                MASSOU=MASSOU+FSCEXP(I)*DT*SMH(I)
              ELSE
                MASSOU=MASSOU+
     &                  (TETAF(I)*F(I)+(1.D0-TETAF(I))*FC(I))*DT*SMH(I)
              ENDIF
            ENDDO
          ENDIF
        ENDIF
!       RAIN
!       DONE IN BILANT FOR ALL SCHEMES
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

