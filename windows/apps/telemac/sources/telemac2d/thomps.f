!                   *****************
                    SUBROUTINE THOMPS
!                   *****************
!
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,T,ZF,X,Y,NBOR,FRTYPE,C,
     & UCONV,VCONV,XCONV,YCONV,LIHBOR,LIUBOR,LIVBOR,
     & IT1,W1R,W2R,W3R,
     & HBTIL,UBTIL,VBTIL,TBTIL,ZBTIL,SURDET,IKLE,IFABOR,NELEM,
     & MESH,XNEBOR,YNEBOR,NPOIN,NPTFR,DT,GRAV,
     & NTRAC,NFRLIQ,KENT,KENTU,MSK,MASKEL,
     & NELMAX,IELM,SHPP,NUMLIQ,SHP,
     & DX_T,DY_T,DZ_T,IT3,IT4,HFIELD,UFIELD,VFIELD,ZS,GZSX,GZSY,
     & SHPBUF)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    Treats liquid boundaries with the Thompson method based
!+        on characteristics. See XVIIIth Telemac User Club proceedings
!+        page 142.
!
!warning  This is probably not correct with spherical coordinates,
!+        because in this case with characteristics we divide the
!+        advection field by cos(latitude). This is only possible if
!+        U is different from UCONV, which is not done in the theory
!+        as non linear terms are dealt with. Here the advected
!+        velocity U and the advection field UCONV are the same.
!
!history  J-M HERVOUET (LNHE)
!+        01/09/2008
!+
!+   POINTS GROUPED REGARDLESS OF THEIR BOUNDARY NUMBER.
!
!history  E DAVID (LHF)
!+        05/09/2008
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
!history  C. DENIS (SINETICS), J.-M. HERVOUET (LNHE)
!+        02/05/2012
!+        V6P2
!+   Use of data structure MESH%ELTCAR to have the same starting element
!+   in scalar mode and in parallel, for the treatment of characteristic
!+   curves.
!
!history  J-M HERVOUET (LNHE)
!+        15/07/2014
!+        V7P0
!+   Size of IT1 changed from 2*NPTFR to NPOIN (to avoid a problem of
!+   bound checking when NPTFR=0)
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        14/08/2015
!+        V7P1
!+   Argument NRK added to SCARACT.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| WORK ARRAY: CELERITY OF WAVES
!| DT             |-->| TIME STEP
!| DX_T           |<->| WORK ARRAY
!| DY_T           |<->| WORK ARRAY
!| DZ_T           |<->| WORK ARRAY
!| ELT_T          |<->| WORK ARRAY
!| FRTYPE         |-->| TYPE OF LIQUID BOUNDARIES
!| GRAV           |-->| GRAVITY
!| GZSX           |<--| FREE SURFACE GRADIENT ALONG X
!| GZSY           |<--| FREE SURFACE GRADIENT ALONG Y
!| H              |-->| WATER DEPTH AT TIME N
!| HBOR           |<--| PRESCRIBED DEPTH AT BOUNDARIES
!| HBTIL          |<->| WORK ARRAY, DEPTH AT BOUNDARIES AFTER ADVECTION
!| HFIELD         |<->| WORK ARRAY, DEPTH WITH RELAXATION
!| IELM           |-->| TYPE OF ELEMENT
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID
!|                |   | BOUNDARY
!| IKLE           |-->| CONNECTIVITY TABLE
!| IT3            |<->| INTEGER WORK ARRAY
!| IT3            |<->| INTEGER WORK ARRAY
!| KINC           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LISPFR         |-->| LIST OF BOUNDARY POINTS TO BE DEALT WITH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| LV             |-->| VECTOR LENGTH (FOR VECTOR MACHINES)
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| SHP            |<--| BARYCENTRIC COORDINATES AT THE FOOT
!|                |   | OF CHARACTERISTICS
!| SHPP           |<--| BARYCENTRIC COORDINATES AT THE FOOT
!|                |   | OF CHARACTERISTICS, FOR BOUNDARY POINTS
!| SURDET         |-->| 1/(DETERMINANT OF ISOPARAMETRIC TRANSFORMATION)
!| T              |-->| BLOCK OF TRACERS AT TIME N
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| TBTIL          |<--| BLOCK OF WORK ARRAYS, TRACERS AFTER ADVECTION
!| U              |<->| X-COMPONENT OF VELOCITY
!| UBOR           |<--| PRESCRIBED VELOCITY U.
!| UBTIL          |<--| WORK ARRAY, U AT BOUNDARIES AFTER ADVECTION
!| UCONV          |-->| WORK ARRAY: ADVECTION FIELDS
!| UFIELD         |<->| WORK ARRAY, U WITH RELAXATION
!| V              |<->| Y-COMPONENT OF VELOCITY
!| UBOR           |<--| PRESCRIBED VELOCITY V.
!| VBTIL          |<--| WORK ARRAY, V AT BOUNDARIES AFTER ADVECTION
!| VCONV          |-->| WORK ARRAY: ADVECTION FIELDS
!| VFIELD         |<->| WORK ARRAY, V WITH RELAXATION
!| W1R            |<->| WORK ARRAY
!| W2R            |<->| WORK ARRAY
!| W3R            |<->| WORK ARRAY
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XNEBOR         |-->| X-COMPONENT OF NORMAL AT NODES
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YNEBOR         |-->| Y-COMPONENT OF NORMAL AT NODES
!| ZBTIL          |<--| WORK ARRAY, BOTTOM AT BOUNDARIES AFTER ADVECTION
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZS             |<--| FREE SURFACE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_THOMPS => THOMPS
      USE STREAMLINE, ONLY : SCARACT
      USE DECLARATIONS_TELEMAC2D, ONLY: INIT_THOMPS, FNCAR1,FTILD1
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR,NPOIN,NELEM,NELMAX,NFRLIQ
      INTEGER, INTENT(IN) :: IELM,NTRAC
      INTEGER, INTENT(IN) :: KENT,KENTU
      INTEGER, INTENT(IN) :: NBOR(NPTFR)
      INTEGER, INTENT(IN) :: IKLE(NELMAX,3),IFABOR(*)
      INTEGER, INTENT(IN) :: LIHBOR(NPTFR),LIUBOR(NPTFR),LIVBOR(NPTFR)
      INTEGER, INTENT(IN) :: FRTYPE(NFRLIQ),NUMLIQ(NPTFR)
!                                           2*NPTFR REALLY USED HERE
      INTEGER, INTENT(INOUT),TARGET  :: IT1(NPOIN)
      LOGICAL, INTENT(IN) :: MSK
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(*)
      DOUBLE PRECISION, INTENT(IN)    :: GRAV,DT
      DOUBLE PRECISION, INTENT(INOUT) :: W1R(NPTFR),W2R(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: W3R(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPP(3,*),SHP(3,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DX_T(NPTFR),DY_T(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: DZ_T(NPTFR)
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL
      TYPE(BIEF_OBJ), TARGET,INTENT(INOUT)   :: HBTIL,UBTIL,VBTIL,ZBTIL
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: XCONV,YCONV
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: UCONV,VCONV,C,U,V
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: H,T,TBOR,TBTIL,IT3,IT4
      TYPE(BIEF_OBJ), TARGET,INTENT(INOUT)   :: HFIELD,UFIELD,VFIELD,ZS
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: GZSX,GZSY,SHPBUF
      TYPE(BIEF_OBJ), TARGET, INTENT(IN)      :: ZF
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, DIMENSION(:), POINTER  :: LISPFR
      INTEGER, DIMENSION(:),POINTER :: ELT_T
      INTEGER K,NPT,J,ITRAC,N,NOMB,NDP,NPLAN,IELMU,ETA(1),I,IFR,IPT,NRK
      INTEGER FREBUF(1)
      DOUBLE PRECISION HHBOR,DETADX,DETADY,TBAR(100),TT(100)
      DOUBLE PRECISION UCSI,UCSIBAR,UETA,UETABAR,CBAR,HH,TETA
      DOUBLE PRECISION ZSTAR(1),ZCONV(1,1),SHZ(1),Z(1,1),UNORM,NORMZS
      INTEGER IELEM,SIZEBUF
!
      DOUBLE PRECISION, PARAMETER :: HMIN = 2.D-2
!
!-----------------------------------------------------------------------
!
!     SLICING IT1
!
      I=MAX(1,NPTFR)
      LISPFR=>IT1(1:I)
      ELT_T =>IT1(I+1:2*I)
!
!     SIZE OF BUFFER FOR SHP
!
      SIZEBUF=SHPBUF%DIM1/3
!
!-----------------------------------------------------------------------
!
      IF(INIT_THOMPS) THEN
        CALL ALLBLO(FNCAR1,'FNCAR1')
        CALL ALLBLO(FTILD1,'FTILD1')
        CALL ADDBLO(FNCAR1,UFIELD)
        CALL ADDBLO(FNCAR1,VFIELD)
        CALL ADDBLO(FNCAR1,HFIELD)
        CALL ADDBLO(FNCAR1,ZF)
        CALL ADDBLO(FTILD1,UBTIL)
        CALL ADDBLO(FTILD1,VBTIL)
        CALL ADDBLO(FTILD1,HBTIL)
        CALL ADDBLO(FTILD1,ZBTIL)
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            CALL ADDBLO(FNCAR1,T%ADR(ITRAC)%P)
            CALL ADDBLO(FTILD1,TBTIL%ADR(ITRAC)%P)
          ENDDO
        ENDIF
        INIT_THOMPS=.FALSE.
      ENDIF
!
      ETA(1)=1
!     NDP ALSO FIRST DIMENSION OF SHP AND SHPP
      NDP=3
      NRK=3
      NPLAN=1
      IELMU=IELM
!
!-----------------------------------------------------------------------
!
!     CREATING FIELDS OF H, U AND V, THAT CONTAIN THE PRESCRIBED DATA
!     WITH RELAXATION TETA (IF 1.D0, THE ROUGH DATA ARE TAKEN ON THE
!     THOMPSON BOUNDARIES FOR INTERPOLATION AT THE FOOT OF
!     CHARACTERISTICS)
!
      CALL OS('X=Y     ',X=HFIELD,Y=H)
      CALL OS('X=Y     ',X=UFIELD,Y=U)
      CALL OS('X=Y     ',X=VFIELD,Y=V)
!
!     FORCING DISCRETISATION TO IELM
!
      UFIELD%ELM=IELM
      VFIELD%ELM=IELM
      HFIELD%ELM=IELM
!
      UFIELD%DIM1=BIEF_NBPTS(IELM,MESH)
      VFIELD%DIM1=BIEF_NBPTS(IELM,MESH)
      HFIELD%DIM1=BIEF_NBPTS(IELM,MESH)
!
      TETA=1.D0
!     CAN BE RELAXED, TESTED UP TO 0.05 IN PALUEL BOX MODEL
!     TETA=0.05D0
!
      DO K=1,NPTFR
        IF(NUMLIQ(K).NE.0) THEN
          IF(FRTYPE(NUMLIQ(K)).EQ.2) THEN
            N=NBOR(K)
            IF(LIHBOR(K).EQ.KENT) THEN
              HFIELD%R(N)=TETA*HBOR(K)+(1.D0-TETA)*HFIELD%R(N)
            ENDIF
            IF(LIUBOR(K).EQ.KENT.OR.LIUBOR(K).EQ.KENTU) THEN
              UFIELD%R(N)=TETA*UBOR(K)+(1.D0-TETA)*UFIELD%R(N)
            ENDIF
            IF(LIVBOR(K).EQ.KENT.OR.LIVBOR(K).EQ.KENTU) THEN
              VFIELD%R(N)=TETA*VBOR(K)+(1.D0-TETA)*VFIELD%R(N)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE CELERITY
!
      CALL OS('X=CY    ',X=C,Y=H,C=GRAV)
      CALL CLIP(C,0.D0,.TRUE.,1.D6,.FALSE.,0)
      CALL OS('X=SQR(Y)',X=C,Y=C)
!
!     COMPUTES MINUS THE FREE SURFACE GRADIENT
!
      CALL OS('X=Y+Z   ',X=ZS,Y=H,Z=ZF)
      CALL VECTOR(GZSX,'=','GRADF          X',IELM,
     &            -1.D0,ZS,ZS,ZS,ZS,ZS,ZS,MESH,MSK,MASKEL)
      CALL VECTOR(GZSY,'=','GRADF          Y',IELM,
     &            -1.D0,ZS,ZS,ZS,ZS,ZS,ZS,MESH,MSK,MASKEL)
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(GZSX,2,MESH)
        CALL PARCOM(GZSY,2,MESH)
      ENDIF
!
!     REGROUPS THE POINTS WITH CHARACTERISTICS TREATMENT BY THIS
!     PROCESSOR AND BUILDS THEIR BARYCENTRIC COORDINATES
!     NPT: NUMBER OF POINTS
!     LISPFR: LIST OF THOSE POINTS (BOUNDARY NODE NUMBERS)
!
      NPT=0
      DO K=1,NPTFR
        IF(NUMLIQ(K).NE.0) THEN
          N=NBOR(K)
          IF(FRTYPE(NUMLIQ(K)).EQ.2.AND.MESH%ELTCAR%I(N).NE.0) THEN
            NPT=NPT+1
            LISPFR(NPT)=K
            IELEM= MESH%ELTCAR%I(N)
            IF(IKLE(IELEM,1).EQ.N) THEN
              SHP(1,N)=1.D0
              SHP(2,N)=0.D0
              SHP(3,N)=0.D0
            ENDIF
            IF(IKLE(IELEM,2).EQ.N) THEN
              SHP(1,N)=0.D0
              SHP(2,N)=1.D0
              SHP(3,N)=0.D0
            ENDIF
            IF(IKLE(IELEM,3).EQ.N) THEN
              SHP(1,N)=0.D0
              SHP(2,N)=0.D0
              SHP(3,N)=1.D0
            ENDIF
          ENDIF
        ENDIF
      ENDDO
!
!--------------------------------------------------------------------
!     CHARACTERISTICS WITH ADVECTION FIELD U
!     MAY BE CALLED WITH NPT=0 IF OTHER PROCESSORS STILL AT WORK
!--------------------------------------------------------------------
!
!     COMPUTES THE ADVECTION FIELD U
!
      DO N=1,NPOIN
        UCONV%R(N)=U%R(N)
        VCONV%R(N)=V%R(N)
      ENDDO
!
!     PREPARING THE CALL TO SCARACT
!
      DO IFR=1,NPT
        IPT=NBOR(LISPFR(IFR))
        XCONV%R(IFR) = X(IPT)
        YCONV%R(IFR) = Y(IPT)
        ELT_T(IFR)   = MESH%ELTCAR%I(IPT)
        SHPP(1,IFR)= SHP(1,IPT)
        SHPP(2,IFR)= SHP(2,IPT)
        SHPP(3,IFR)= SHP(3,IPT)
      ENDDO
!
      NOMB=4+NTRAC
      CALL SCARACT(FNCAR1,FTILD1,UCONV%R,VCONV%R,VCONV%R,VCONV%R,
     &             X,Y,
     &             ZSTAR,ZSTAR,XCONV%R,YCONV%R,ZCONV,ZCONV,
     &             DX_T,DY_T,DZ_T,DZ_T,Z,
     &             SHPP,SHZ,SHZ,
     &             SURDET,DT,IKLE,IFABOR,ELT_T,
     &             ETA,ETA,IT3%I,IT4%I,IELM,IELMU,NELEM,NELMAX,
     &             NOMB,NPOIN,NDP,NRK,NPLAN,1,MESH,NPT,U%DIM1,-1,
     &             SHPBUF%R,SHPBUF%R,SHPBUF%R,FREBUF,SIZEBUF)
!
!----------------------------------------------------------------------
!     UBTIL, VBTIL, HBTIL, TBTIL AT BOUNDARY NODES NUMBERING
!----------------------------------------------------------------------
!
!     BACKWARD LOOP TO AVOID ERASING DATA, K ALWAYS GREATER THAN J
      DO J=NPT,1,-1
        K=LISPFR(J)
        UBTIL%R(K)=UBTIL%R(J)
        VBTIL%R(K)=VBTIL%R(J)
        HBTIL%R(K)=HBTIL%R(J)
!       ZBTIL%R(K)=ZBTIL%R(J)
      ENDDO
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          DO J=NPT,1,-1
            K=LISPFR(J)
            TBTIL%ADR(ITRAC)%P%R(K)=TBTIL%ADR(ITRAC)%P%R(J)
          ENDDO
        ENDDO
      ENDIF
!
!     BEFORE PARCOM_BORD ,CANCELLING VALUES OF POINTS TREATED
!     IN ANOTHER SUB-DOMAIN
!
      IF(NCSIZE.GT.1) THEN
        DO K=1,NPTFR
          IF(MESH%ELTCAR%I(NBOR(K)).EQ.0) THEN
            UBTIL%R(K)=0.D0
            VBTIL%R(K)=0.D0
            HBTIL%R(K)=0.D0
!           ZBTIL%R(K)=0.D0
          ENDIF
        ENDDO
        IF(NTRAC.GT.0) THEN
          DO K=1,NPTFR
            IF(MESH%ELTCAR%I(NBOR(K)).EQ.0) THEN
              DO ITRAC=1,NTRAC
                TBTIL%ADR(ITRAC)%P%R(K)=0.D0
              ENDDO
            ENDIF
          ENDDO
        ENDIF
        CALL PARCOM_BORD(UBTIL%R,1,MESH)
        CALL PARCOM_BORD(VBTIL%R,1,MESH)
        CALL PARCOM_BORD(HBTIL%R,1,MESH)
!       CALL PARCOM_BORD(ZBTIL%R,1,MESH)
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            CALL PARCOM_BORD(TBTIL%ADR(ITRAC)%P%R,1,MESH)
          ENDDO
        ENDIF
      ENDIF
!
!     COMPUTES THE RIEMANN INVARIANTS W1 AND W4 (SECOND DIMENSION OF TBOR)
!     CARRIED BY THIS FIELD
!
!     IF W1=0 IS OK, THIS PART COULD BE DONE ONLY WHEN NTRAC.GT.0
!
      DO K=1,NPTFR
        IF(NUMLIQ(K).NE.0) THEN
        IF(FRTYPE(NUMLIQ(K)).EQ.2) THEN
          N=NBOR(K)
          UNORM=SQRT(U%R(N)**2+V%R(N)**2)
          IF(UNORM.GT.1.D-12) THEN
            DETADX=U%R(N)/UNORM
            DETADY=V%R(N)/UNORM
          ELSE
            DETADX=0.D0
            DETADY=0.D0
          ENDIF
          UCSIBAR=-U%R(N)*DETADY+V%R(N)*DETADX
          HH=HBTIL%R(K)
          UCSI=-UBTIL%R(K)*DETADY+VBTIL%R(K)*DETADX
          W1R(K)=HH*(UCSIBAR-UCSI)
          IF(NTRAC.GT.0) THEN
            DO ITRAC=1,NTRAC
              TBAR(ITRAC)=T%ADR(ITRAC)%P%R(N)
            ENDDO
            IF(UCONV%R(N)*XNEBOR(K)+VCONV%R(N)*YNEBOR(K).GT.0.D0) THEN
!             VELOCITY EXITING THE DOMAIN, THE CHARACTERISTICS ARE USED
              DO ITRAC=1,NTRAC
                TT(ITRAC)=TBTIL%ADR(ITRAC)%P%R(K)
              ENDDO
            ELSE
!             VELOCITY ENTERING THE DOMAIN, PRESCRIBED VALUES TAKEN
              DO ITRAC=1,NTRAC
                TT(ITRAC)=TBOR%ADR(ITRAC)%P%R(K)
              ENDDO
            ENDIF
            DO ITRAC=1,NTRAC
!             W4
              TBOR%ADR(ITRAC)%P%R(K+NPTFR)=HH*(TT(ITRAC)-TBAR(ITRAC))
            ENDDO
          ENDIF
        ENDIF
        ENDIF
      ENDDO
!
!----------------------------------------------------------
!     COMPUTES THE ADVECTION FIELD U + C
!----------------------------------------------------------
!
      DO N=1,NPOIN
        UNORM=SQRT(U%R(N)**2+V%R(N)**2)
        IF(UNORM.GT.1.D-12) THEN
          DETADX=U%R(N)/UNORM
          DETADY=V%R(N)/UNORM
        ELSE
!         VERY IMPORTANT
          NORMZS=SQRT(GZSX%R(N)**2+GZSY%R(N)**2)
          IF(NORMZS.GT.1.D-12) THEN
            DETADX=GZSX%R(N)/NORMZS
            DETADY=GZSY%R(N)/NORMZS
          ELSE
            DETADX=0.D0
            DETADY=0.D0
          ENDIF
        ENDIF
        UETABAR=U%R(N)*DETADX+V%R(N)*DETADY+C%R(N)
        UCONV%R(N)=UETABAR*DETADX
        VCONV%R(N)=UETABAR*DETADY
      ENDDO
!
!----------------------------------------------------------------------
!     CHARACTERISTICS FOR THE GROUP OF POINTS, FIELD U + C
!     MAY BE CALLED WITH NPT=0 IF OTHER PROCESSORS STILL AT WORK
!----------------------------------------------------------------------
!
!     PREPARING THE CALL TO SCARACT
!
      DO IFR=1,NPT
        IPT=NBOR(LISPFR(IFR))
        XCONV%R(IFR) = X(IPT)
        YCONV%R(IFR) = Y(IPT)
        ELT_T(IFR)   = MESH%ELTCAR%I(IPT)
        SHPP(1,IFR)  = SHP(1,IPT)
        SHPP(2,IFR)  = SHP(2,IPT)
        SHPP(3,IFR)  = SHP(3,IPT)
      ENDDO
!
      NOMB=4
      CALL SCARACT(FNCAR1,FTILD1,UCONV%R,VCONV%R,VCONV%R,VCONV%R,
     &             X,Y,
     &             ZSTAR,ZSTAR,XCONV%R,YCONV%R,ZCONV,ZCONV,
     &             DX_T,DY_T,DZ_T,DZ_T,Z,
     &             SHPP,SHZ,SHZ,
     &             SURDET,DT,IKLE,IFABOR,ELT_T,ETA,ETA,
     &             IT3%I,IT4%I,IELM,IELMU,NELEM,NELMAX,NOMB,
     &             NPOIN,NDP,NRK,NPLAN,1,MESH,NPT,U%DIM1,-1,
     &             SHPBUF%R,SHPBUF%R,SHPBUF%R,FREBUF,SIZEBUF)
!
!----------------------------------------------------------------------
!     UBTIL, VBTIL, HBTIL AT BOUNDARY NODES NUMBERING
!----------------------------------------------------------------------
!
!     BACKWARD LOOP TO AVOID ERASING DATA, K ALWAYS GREATER THAN J
      DO J=NPT,1,-1
        K=LISPFR(J)
        UBTIL%R(K)=UBTIL%R(J)
        VBTIL%R(K)=VBTIL%R(J)
        HBTIL%R(K)=HBTIL%R(J)
        ZBTIL%R(K)=ZBTIL%R(J)
      ENDDO
!
!     BEFORE PARCOM_BORD ,CANCELLING VALUES OF POINTS TREATED
!     IN ANOTHER SUB-DOMAIN
!
      IF(NCSIZE.GT.1) THEN
        DO K=1,NPTFR
          IF(MESH%ELTCAR%I(NBOR(K)).EQ.0) THEN
            UBTIL%R(K)=0.D0
            VBTIL%R(K)=0.D0
            HBTIL%R(K)=0.D0
            ZBTIL%R(K)=0.D0
          ENDIF
        ENDDO
        CALL PARCOM_BORD(UBTIL%R,1,MESH)
        CALL PARCOM_BORD(VBTIL%R,1,MESH)
        CALL PARCOM_BORD(HBTIL%R,1,MESH)
        CALL PARCOM_BORD(ZBTIL%R,1,MESH)
      ENDIF
!
!----------------------------------------------------------------------
!     COMPUTES THE RIEMANN INVARIANTS W2 CARRIED BY THIS ADVECTION FIELD
!----------------------------------------------------------------------
!
      DO K=1,NPTFR
        IF(NUMLIQ(K).NE.0) THEN
        IF(FRTYPE(NUMLIQ(K)).EQ.2) THEN
          N=NBOR(K)
          UNORM=SQRT(U%R(N)**2+V%R(N)**2)
          IF(UNORM.GT.1.D-12) THEN
            DETADX=U%R(N)/UNORM
            DETADY=V%R(N)/UNORM
          ELSE
            DETADX=0.D0
            DETADY=0.D0
          ENDIF
!         UETABAR=U%R(N)*DETADX+V%R(N)*DETADY
          UETABAR=UNORM
          CBAR=C%R(N)
          HH  =HBTIL%R(K)
          UETA=UBTIL%R(K)*DETADX+VBTIL%R(K)*DETADY
          W2R(K)=(HH+ZBTIL%R(K)-ZF%R(N))*CBAR+HH*(UETA-UETABAR)
        ENDIF
        ENDIF
      ENDDO
!
!     COMPUTES THE ADVECTION FIELD U-C
!
      DO N=1,NPOIN
        UNORM=SQRT(U%R(N)**2+V%R(N)**2)
        IF(UNORM.GT.1.D-12) THEN
          DETADX=U%R(N)/UNORM
          DETADY=V%R(N)/UNORM
        ELSE
          NORMZS=SQRT(GZSX%R(N)**2+GZSY%R(N)**2)
          IF(NORMZS.GT.1.D-12) THEN
            DETADX=GZSX%R(N)/NORMZS
            DETADY=GZSY%R(N)/NORMZS
          ELSE
            DETADX=0.D0
            DETADY=0.D0
          ENDIF
        ENDIF
        UETABAR=U%R(N)*DETADX+V%R(N)*DETADY-C%R(N)
        UCONV%R(N)=UETABAR*DETADX
        VCONV%R(N)=UETABAR*DETADY
      ENDDO
!
!     CHARACTERISTICS FOR THE GROUP OF POINTS, FIELD U + C
!
!     PREPARING THE CALL TO SCARACT
!
      DO IFR=1,NPT
        IPT=NBOR(LISPFR(IFR))
        XCONV%R(IFR) = X(IPT)
        YCONV%R(IFR) = Y(IPT)
        ELT_T(IFR)   = MESH%ELTCAR%I(IPT)
        SHPP(1,IFR)  = SHP(1,IPT)
        SHPP(2,IFR)  = SHP(2,IPT)
        SHPP(3,IFR)  = SHP(3,IPT)
      ENDDO
!
      NOMB=4
      CALL SCARACT(FNCAR1,FTILD1,UCONV%R,VCONV%R,VCONV%R,VCONV%R,
     &             X,Y,ZSTAR,ZSTAR,
     &             XCONV%R,YCONV%R,ZCONV,ZCONV,
     &             DX_T,DY_T,DZ_T,DZ_T,Z,SHPP,SHZ,SHZ,
     &             SURDET,DT,IKLE,IFABOR,ELT_T,ETA,ETA,
     &             IT3%I,IT4%I,IELM,
     &             IELMU,NELEM,NELMAX,NOMB,NPOIN,NDP,NRK,NPLAN,1,
     &             MESH,NPT,U%DIM1,-1,
     &             SHPBUF%R,SHPBUF%R,SHPBUF%R,FREBUF,SIZEBUF)
!
!----------------------------------------------------------------------
!     UBTIL, VBTIL, HBTIL AT BOUNDARY NODES NUMBERING
!----------------------------------------------------------------------
!
!     BACKWARD LOOP TO AVOID ERASING DATA, K ALWAYS GREATER THAN J
      DO J=NPT,1,-1
        K=LISPFR(J)
        UBTIL%R(K)=UBTIL%R(J)
        VBTIL%R(K)=VBTIL%R(J)
        HBTIL%R(K)=HBTIL%R(J)
        ZBTIL%R(K)=ZBTIL%R(J)
      ENDDO
!
!     BEFORE PARCOM_BORD ,CANCELLING VALUES OF POINTS TREATED
!     IN ANOTHER SUB-DOMAIN
!
      IF(NCSIZE.GT.1) THEN
        DO K=1,NPTFR
          IF(MESH%ELTCAR%I(NBOR(K)).EQ.0) THEN
            UBTIL%R(K)=0.D0
            VBTIL%R(K)=0.D0
            HBTIL%R(K)=0.D0
            ZBTIL%R(K)=0.D0
          ENDIF
        ENDDO
        CALL PARCOM_BORD(UBTIL%R,1,MESH)
        CALL PARCOM_BORD(VBTIL%R,1,MESH)
        CALL PARCOM_BORD(HBTIL%R,1,MESH)
        CALL PARCOM_BORD(ZBTIL%R,1,MESH)
      ENDIF
!
! COMPUTES THE RIEMANN INVARIANTS W3 CARRIED BY THIS ADVECTION FIELD
!
      DO K=1,NPTFR
        IF(NUMLIQ(K).NE.0) THEN
        IF(FRTYPE(NUMLIQ(K)).EQ.2) THEN
          N=NBOR(K)
          UNORM=SQRT(U%R(N)**2+V%R(N)**2)
          IF(UNORM.GT.1.D-12) THEN
            DETADX=U%R(N)/UNORM
            DETADY=V%R(N)/UNORM
          ELSE
            DETADX=0.D0
            DETADY=0.D0
          ENDIF
!         UETABAR=U%R(N)*DETADX+V%R(N)*DETADY
!         MARCHE AUSSI
          UETABAR=UNORM
          CBAR=C%R(N)
          HH  =HBTIL%R(K)
          UETA=UBTIL%R(K)*DETADX+VBTIL%R(K)*DETADY
          W3R(K)=(HH+ZBTIL%R(K)-ZF%R(N))*CBAR+HH*(-UETA+UETABAR)
        ENDIF
        ENDIF
      ENDDO
!
!----------------------------------------------------------------------
!       RE-BUILDS THE TELEMAC-2D VARIABLES
!----------------------------------------------------------------------
!
      DO K=1,NPTFR
        IF(NUMLIQ(K).NE.0) THEN
        IF(FRTYPE(NUMLIQ(K)).EQ.2) THEN
          N=NBOR(K)
          UNORM=SQRT(U%R(N)**2+V%R(N)**2)
          IF(UNORM.GT.1.D-12) THEN
            DETADX=U%R(N)/UNORM
            DETADY=V%R(N)/UNORM
          ELSE
            NORMZS=SQRT(GZSX%R(N)**2+GZSY%R(N)**2)
            IF(NORMZS.GT.1.D-12) THEN
              DETADX=GZSX%R(N)/NORMZS
              DETADY=GZSY%R(N)/NORMZS
            ELSE
              DETADX=0.D0
              DETADY=0.D0
            ENDIF
          ENDIF
          IF(C%R(N)**2.GT.GRAV*HMIN) THEN
            HBOR(K)=(W2R(K)+W3R(K))/(2*C%R(N))
            IF(HBOR(K).GT.HMIN) THEN
!             BEWARE TIDAL FLATS, AND HIDDEN PARAMETER 0.1
              HHBOR=MAX(0.1D0,HBOR(K))
!             formulas 37 and 38 of proceedings but coefficient 0.5
!             is missing in this document.
              UBOR(K)=(       DETADY* W1R(K)
     &                 +0.5D0*DETADX*(W2R(K)-W3R(K)))/HHBOR+U%R(N)
              VBOR(K)=(      -DETADX* W1R(K)
     &                 +0.5D0*DETADY*(W2R(K)-W3R(K)))/HHBOR+V%R(N)
              IF(NTRAC.GT.0) THEN
                DO ITRAC=1,NTRAC
!                 REMEMBER THAT W4 IS STORED INTO SECOND DIMENSION OF TBOR
                  TBOR%ADR(ITRAC)%P%R(K)=
     &            TBOR%ADR(ITRAC)%P%R(K+NPTFR)/HHBOR+T%ADR(ITRAC)%P%R(N)
                ENDDO
              ENDIF
            ELSE
!             BECOMES DRY
              HBOR(K)=MAX(0.D0,HBOR(K))
              UBOR(K)=0.D0
              VBOR(K)=0.D0
            ENDIF
          ELSE
!           WAS DRY, H IS GIVEN BY BORD
            UBOR(K)=0.D0
            VBOR(K)=0.D0
          ENDIF
        ENDIF
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
