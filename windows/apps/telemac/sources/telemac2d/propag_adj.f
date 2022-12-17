!                   *********************
                    SUBROUTINE PROPAG_ADJ
!                   *********************
!
     &(UN,VN,HN,
     & MESH,ZF,AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1,
     & CV1,CV2,CV3,
     & TE3,T1,T2,T3,T4,T5,T6,T7,T10,T11,
     & LIMPRO,MASK,GRAV,CF,LT,AT,DT,
     & TETAH,TETAU,
     & KDIR,INFOGR,KFROT,
     & MSK,MASKEL,MASKPT,
     & RHS,TB,SOLSYS,
     & SLVPRO,VERTIC,
     & U,V,H,UU,VV,HH,UIT1,VIT1,HIT1,PP,QQ,RR,
     & TAM1,TAM2,TAM3,TBM1,TBM2,TCM1,
     & TCM2,MATADJ,UNKADJ,ADJDIR,ESTIME,OPTCOST,
     & NIT,VARSOR,
     & ALIRE,TROUVE,MAXVAR,
     & TEXTE,CHESTR,KARMAN,NDEF,
     & LISRUG,CHBORD,CFBOR,HFROT,UNSV2D)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE RIGHT HAND SIDE OF THE ADJOINT SYSTEM
!+                IN MATRIX FORM.
!code
!+        T      N-1    T     N-1    T     N-1        *
!+         AM1  P     +  CM1 Q     +  CM2 R     =  CV1
!+
!+        T     N-1     T     N-1                     *
!+         BM1 P      +  AM2 Q                  =  CV2
!+
!+        T     N-1                  T     N-1        *
!+         BM2 P                   +  AM3 R     =  CV3
!
!history  J-M HERVOUET (LNHE)     ; C MOULIN (LNH)
!+        24/04/1997
!+        V5P5
!+
!
!history  A LEOPARDI (UNINA)
!+        18/09/2000
!+
!+
!
!history
!+        13/11/2000
!+
!+   COMPLETE VERSION
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ADJDIR         |<--| DIRICHLET CONDITIONS FOR ADJOINT VARIABLES
!| ALIRE          |<--| INTEGER ARRAY STATING WHICH VARIABLES MUST BE READ
!| AM1            |<->| MATRIX APPLYING TO H
!| AM2            |<->| MATRIX APPLYING TO U
!| AM3            |<->| MATRIX APPLYING TO V
!| AT             |-->| TIME IN SECONDS
!| BM2            |<->| MATRIX
!| CF             |<--| ADIMENSIONAL FRICTION COEFFICIENT
!| CFBOR          |<--| ADIMENSIONAL FRICTION COEFFICIENT ON BOUNDARY
!| CHBORD         |<--| FRICTION COEFFICIENT ON BOUNDARY
!| CHESTR         |-->| FRICTION COEFFICIENT ON BOTTOM
!| CM1            |<->| MATRIX
!| CM2            |<->| MATRIX
!| CV1            |<->| RIGHT-HAND SIDE OF LINEAR SYSTEM
!| CV2            |<->| RIGHT-HAND SIDE OF LINEAR SYSTEM
!| CV3            |<->| RIGHT-HAND SIDE OF LINEAR SYSTEM
!| DIRBOR         |<--| BLOCK WITH DIRICHLET BOUNDARY CONDITIONS
!| DT             |-->| TIME STEP
!| ESTIME         |---| ???? NOT USED ()
!| GRAV           |-->| GRAVITY
!| HFROT          |-->| KEYWORD: 'DEPTH IN FRICTION TERMS'
!| HH             |<--| ADJOINT H
!| HIT1           |-->| DIRECT H AT TIME IT+1
!| HN             |-->| DEPTH AT TIME T(N)
!| ICONVF         |-->| TYPE OF ADVECTION: 4 INTEGERS
!|                |   | ICONVF(1) : U AND V
!|                |   | ICONVF(2) : H (MANDATORY VALUE = 5)
!|                |   | ICONVF(3) : TRACERS
!|                |   | ICONVF(4) : K AND EPSILON
!| INFOGR         |-->| IF YES, INFORMATION ON GRADIENT
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KFROT          |-->| KEYWORD: 'LAW OF BOTTOM FRICTION'
!| LIMPRO         |-->| BOUNDARY CONDITIONS FOR H, U V PER POINTS
!|                |   | AND SEGMENTS
!| LISRUG         |-->| TURBULENCE REGIME (1: SMOOTH 2: ROUGH)
!| LT             |-->| ITERATION NUMBER
!| MASK           |-->| BLOCK OF MASKS FOR SEGMENTS :
!|                |   | MASK(MSK1): 1. IF KDIR ON U 0. ELSE
!|                |   | MASK(MSK2): 1. IF KDIR ON V 0. ELSE
!|                |   | MASK(MSK3): 1. IF KDDL ON U 0. ELSE
!|                |   | MASK(MSK4): 1. IF KDDL ON V 0. ELSE
!|                |   | MASK(MSK6): 1. IF KNEU ON V 0. ELSE
!|                |   | MASK(MSK7): 1. IF KOND 0. ELSE
!|                |   | MASK(MSK9): 1. IF KDIR ON H (POINT)
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MATADJ         |<--| BLOCK OF MATRICES FOR ADJOINT SYSTEM
!| MAXVAR         |-->| MAXIMUM NUMBER OF VARIABLES
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NDEF           |-->| DEFAULT MANNING
!| NIT            |-->| TOTAL NUMBER OF ITERATIONS
!| NREF           |-->| LOGICAL UNIT OF REFERENCE FILE
!| OPTCOST        |-->| OPTION FOR COMPUTING THE COST FUNCTION
!| PP             |<--| ADJOINT H
!| QQ             |<--| ADJOINT U
!| RHS            |<->| BLOCK OF PRIVATE BIEF_OBJ STRUCTURES
!| RR             |<--| ADJOINT R
!| S              |-->| VOID STRUCTURE
!| SB             |---| NOT USED !!!!!!!!!!!!!!!!!!!!!!
!| SLVPRO         |-->| SOLVER STRUCTURE FOR PROPAGATION
!| SOLSYS         |-->| KEYWORD: 'TREATMENT OF THE LINEAR SYSTEM'
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T5             |<->| WORK BIEF_OBJ STRUCTURE
!| T6             |<->| WORK BIEF_OBJ STRUCTURE
!| T7             |<->| WORK BIEF_OBJ STRUCTURE
!| T10            |<->| WORK BIEF_OBJ STRUCTURE
!| TAM1           |<->| MATRIX FOR ADJOINT SYSTEM
!| TAM2           |<->| MATRIX FOR ADJOINT SYSTEM
!| TAM3           |<->| MATRIX FOR ADJOINT SYSTEM
!| TB             |<->| BLOCK WITH T1,T2,...
!| TBM1           |<->| MATRIX FOR ADJOINT SYSTEM
!| TBM2           |<->| MATRIX FOR ADJOINT SYSTEM
!| TCM1           |<->| MATRIX FOR ADJOINT SYSTEM
!| TCM2           |<->| MATRIX FOR ADJOINT SYSTEM
!| TE3            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TETAH          |-->| IMPLICITATION OF H IN U EQUATION
!| TETAHC         |-->| IMPLICITATION OF H IN CONTINUITY
!| TETAU          |-->| IMPLICITATION OF U AND
!| TEXTE          |<--| WORK STRINGS, LIKE TEXREF
!| TM1            |<->| MATRIX
!| TROUVE         |<--| INTEGER ARRAY SAYING IF A VARIABLE HAS BEEN FOUND
!| UIT1           |<--| DIRECT U AT TIME IT+1
!| UN             |<->| X-COMPONENT OF VELOCITY AT TIME T(N)
!| VN             |<->| Y-COMPONENT OF VELOCITY AT TIME T(N)
!| UNKADJ         |<->| BLOCK OF UNKNOWNS OF ADJOINT SYSTEM
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| UU             |<--| ADJOINT U
!| VARSOR         |<->| BLOCK OF VARIABLES IN RESULT FILE
!| VERTIC         |-->| IF YES, THERE ARE VERTICAL STRUCTURES
!| VIT1           |<--| DIRECT V AT TIME IT+1
!| VV             |<->| ADJOINT V
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_PROPAG_ADJ => PROPAG_ADJ
      USE DECLARATIONS_TELEMAC2D, ONLY : KFROTL,T2D_FILES,T2DRES,LISTIN,
     &    FRICOU,ORBVEL
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: LT,KDIR,KFROT
      INTEGER, INTENT(IN) :: SOLSYS
      INTEGER, INTENT(IN) :: LISRUG,OPTCOST
      INTEGER, INTENT(IN)    :: NIT,MAXVAR,HFROT
      INTEGER, INTENT(INOUT) :: TROUVE(*),ALIRE(*)
      LOGICAL, INTENT(IN)    :: INFOGR,MSK
      LOGICAL, INTENT(IN)    :: VERTIC
      DOUBLE PRECISION, INTENT(IN)    :: TETAU,TETAH
      DOUBLE PRECISION, INTENT(IN)    :: AT,DT,GRAV
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN,NDEF
      TYPE(SLVCFG), INTENT(INOUT)     :: SLVPRO
      TYPE(BIEF_OBJ), INTENT(IN)      :: UN,VN,HN
      TYPE(BIEF_OBJ), INTENT(IN)      :: CF,UNSV2D
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: U,V,H,CV1,CV2,CV3
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,MASKPT,ZF
      TYPE(BIEF_OBJ), INTENT(IN)      :: LIMPRO
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,T3,T4,T5,T6,T7,T10
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T11
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TE3
!     STRUCTURES OF MATRICES
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TAM1,TAM2,TAM3,TBM1
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TBM2,TCM1,TCM2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1
!
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: MASK,RHS,TB
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: CHESTR
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: HH,UU,VV,UIT1,VIT1,HIT1
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: PP,QQ,RR,CHBORD,CFBOR
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: VARSOR
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: MATADJ,UNKADJ,ADJDIR
      CHARACTER(LEN=72), INTENT(IN)   :: ESTIME
      CHARACTER(LEN=32), INTENT(INOUT):: TEXTE(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITER,IELMU,IELMH,NPOIN,IERR
      INTEGER UDIR,UDDL,VDIR,VDDL
!
      DOUBLE PRECISION C,AT1
!
      CHARACTER(LEN=16) FORMULE
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION, PARAMETER :: VK = 1.D-6
!-----------------------------------------------------------------------
!
      IELMH=HH%ELM
      IELMU=UU%ELM
!
!  ADDRESSES OF THE ARRAYS IN THE MASKING BLOCK: MASK
!
      UDIR = 1
      VDIR = 2
      UDDL = 3
      VDDL = 4
!
!-----------------------------------------------------------------------
!
      IF(SOLSYS.NE.1) THEN
        WRITE(LU,*) 'TREATMENT OF THE LINEAR SYSTEM : ',SOLSYS
        WRITE(LU,*) 'UNEXPECTED CASE IN ESTIMATION MODE'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     COMPUTES MATRIX FOR ADJOINT SYSTEM
!
      CALL OM('M=TN    ', M=TAM1, N=AM1, MESH=MESH)
      CALL OM('M=TN    ', M=TAM2, N=AM2, MESH=MESH)
      CALL OM('M=TN    ', M=TAM3, N=AM3, MESH=MESH)
      CALL OM('M=TN    ', M=TBM1, N=BM1, MESH=MESH)
      CALL OM('M=TN    ', M=TBM2, N=BM2, MESH=MESH)
      CALL OM('M=TN    ', M=TCM1, N=CM1, MESH=MESH)
      CALL OM('M=TN    ', M=TCM2, N=CM2, MESH=MESH)
!
!=======================================================================
!
!     COMPUTES RIGHT HAND SIDES FOR ADJOINT SYSTEM
!
!=======================================================================
!
!     NB: HIT1, UIT1, VIT1 ARE DIRECT VARIABLES AT TIME IT+1
!         HH  , UU  , VV   ARE DIRECT VARIABLES AT TIME IT
!         HN  , UN  , VN   ARE DIRECT VARIABLES AT TIME IT-1
!
!
!           IT    IT    IT
!  TERMS 2 W   ( X   - M   ) OR EQUIVALENT DEPENDING ON THE COST FUNCTION
!           IP    IP    IP
!
!     INITIALISES CV1, CV2 AND CV3
!     IN STEADY STATE MODE, WEIGHTS ALPHA1, ALPHA2 AND ALPHA3 ARE
!     INITIALISED BY A CALL TO "MESURES" IN HOMERE_T2D_ADJ, IN THE LOOP
!     FOR THE COMPUTATION OF THE COST FUNCTION. THEN THEY ARE CANCELLED
!     AT THE END OF THIS ROUTINE
!
      CALL COST_FUNCTION(C,OPTCOST,'RHS')
!
!-----------------------------------------------------------------------
!
!  PREPARES FRICTION TERMS AND VECTOR T1 EQUAL TO 0
!
!     T10 = MASS MATRIX LUMPED / COS(SLOPE)
      CALL SLOPES(TE3,ZF,MESH)
      CALL VECTOR(T10,'=','MASBAS          ',IELMU,1.D0,T2,
     &                T2,T2,T2,T2,T2,MESH,.TRUE.,TE3)
!     FU IN T11 (AND FV=FU)
!
!     T2 WILL HOLD CF AT ITERATION IT+1
!
      CALL CPSTVC(CF,T2)
!
!FH-FRDATA
      CALL FRICTION_UNIF(MESH,HH,UU,VV,CHESTR,KFROT,KFROTL,LISRUG,
     &                   .FALSE.,NDEF,VK,KARMAN,GRAV,
     &                   T1,T3,CHBORD,T2,CFBOR,FRICOU,MESH%NPOIN,ORBVEL)
!FH-FRDATA
!
      CALL FRICTI(T11,T3,T4,T5,UU,VV,HH,T2,MESH,T6,T7,VERTIC,UNSV2D,
     &            MSK,MASKEL,HFROT)
!     CALL FRICTI(T11,T3,T4,T5,UU,VV,HH,CF,MESH,T6,VERTIC)
!
!     FINAL FU OF PHD IN T11
      CALL OS('X=XY    ', X=T11, Y=T10)
!
!     T1 : 0 VECTOR
      CALL CPSTVC(HH,T1)
      CALL OS('X=C     ', X=T1, C=0.D0)
!
!-----------------------------------------------------------------------
!
!  COMPUTES CV1 : CV1 = CV1 + T11+T12+T13+T14+T15+T16+T17
!
!-----------------------------------------------------------------------
!  TERM T11 (3 PARTS)
!-----------------------------------------------------------------------
!
!  T11_1 : M/DT  * H
!                   ADJ
!     TERM 1B OF AL
      CALL MATRIX(AM1,'M=N     ','MATMAS          ',IELMH,IELMH,
     &            1.D0/DT,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV1 , AM1 , PP , C , MESH)
!
!  T11_2 : ADVECTION
!
!  T11_3 :
!
!     8B OF AL
!     CALL MATRIX(BM1,'M=N     ','MATGRF         X',IELMH,IELMH,
!    *           (TETAU-1.D0),UU,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
!     CALL MATVEC('X=X+AY  ', CV1 , BM1 , PN , C , MESH)
!     9B OF AL
!     CALL MATRIX(BM2,'M=N     ','MATGRF         Y',IELMH,IELMH,
!    *           (TETAU-1.D0),VV,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
!     CALL MATVEC('X=X+AY  ', CV1 , BM2 , PN , C , MESH)
!
!     CORRECTED VERSION JMH: NOW ICONVF(2) IS ALWAYS 5
!
!     IF(ICONVF(2).EQ.5.OR.ICONVF(2).EQ.8) THEN
        FORMULE='MATFGR          '
!     ELSE
!       FORMULE='MATGRF          '
!     ENDIF
!
      FORMULE(16:16)='X'
      CALL MATRIX(BM1,'M=N     ',FORMULE,IELMH,IELMU,
     &           (TETAU-1.D0),PP,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV1 , BM1 , UU , C , MESH)
      FORMULE(16:16)='Y'
      CALL MATRIX(BM2,'M=N     ',FORMULE,IELMH,IELMU,
     &           (TETAU-1.D0),PP,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV1 , BM2 , VV , C , MESH)
!
!                           H
!  T11_4 : BOUNDARY TERM TB1
!                           ADJ
!
!     JMH : I HAVE -1.D0 INSTEAD OF TETAU-1.D0, BUT THEN HE SUBTRACTS TETAU ??
!           SAME THING EXCEPT FOR INCIDENT WAVE ???
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            TETAU-1.D0,PP,T1,T1,UU,VV,T1,MESH,.TRUE.,
     &            MASK%ADR(8)%P)
      CALL OSDB('X=X+Y   ' , CV1 , T2 , T2 , C , MESH)
!     DIRICHLET ON VELOCITY :
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,UU,VV,T1,MESH,
     &            .TRUE.,MASK%ADR(UDIR)%P)
      CALL OSDB('X=X+Y   ' , CV1 , T2 , T2 , C , MESH)
!     FREE FLOW ON VELOCITY :
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,UU,VV,T1,MESH,
     &            .TRUE.,MASK%ADR(UDDL)%P)
      CALL OSDB('X=X+Y   ' , CV1 , T2 , T2 , C , MESH)
!
!-----------------------------------------------------------------------
!  TERM T12
!-----------------------------------------------------------------------
!
!     TERM 2B OF AL
      CALL MATVEC('X=X+CAY ',CV1,TCM1,QQ,(TETAH-1.D0)/TETAH,MESH)
!
!-----------------------------------------------------------------------
!  TERM T13
!-----------------------------------------------------------------------
!
!     TERM 3B OF AL
      CALL MATVEC('X=X+CAY ',CV1,TCM2,RR,(TETAH-1.D0)/TETAH,MESH)
!
!-----------------------------------------------------------------------
!  TERM T14
!-----------------------------------------------------------------------
!
!     TERM 1C OF AL
!     VERSION EB+AL, NOTE JMH : DON'T AGREE
!     CALL MATRIX(BM1,'M=N     ','MATGRF         X',IELMH,IELMH,
!    *            -TETAU,UN,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
!     CALL MATVEC('X=X+AY  ', CV1 , BM1 , PN , C , MESH)
!
!     VERSION JMH+CC
!                        START OF FORMULATION MADE FOR T11_3
      FORMULE(16:16)='X'
      CALL MATRIX(BM1,'M=N     ',FORMULE,IELMH,IELMU,
     &            -TETAU,PP,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV1 , BM1 , UIT1 , C , MESH)
!
!-----------------------------------------------------------------------
!  TERM T15 + T17
!-----------------------------------------------------------------------
!
!          IT+1   IT+1    IT+1    IT+1
!     T3= U    * Q     + V     * R
!
      CALL OS('X=YZ    ', X=T3, Y=UIT1, Z=QQ)
      CALL OS('X=X+YZ  ', X=T3, Y=VIT1, Z=RR)
!
!     T4=-(4/3)/H OR -1/H
      CALL OS('X=1/Y   ', X=T4, Y=HH, IOPT=2, INFINI=0.D0, ZERO=1.D-6)
      IF(KFROT.EQ.3) THEN
        CALL OS('X=CX    ', X=T4, C=-4.D0/3.D0)
      ELSEIF(KFROT.EQ.2) THEN
        CALL OS('X=CX    ', X=T4 , C=-1.D0)
      ELSE
        WRITE(LU,*) 'WRONG FRICTION LAW FOR ESTIMATION'
        CALL PLANTE(1)
        STOP
      ENDIF
      CALL OS('X=XY    ', X=T4 , Y=T11)
!     AND IF T3 IS QUASI-BUBBLE?
      CALL OS('X=X+YZ  ', X=CV1 , Y=T4 , Z=T3)
!
!-----------------------------------------------------------------------
!  TERM T16
!-----------------------------------------------------------------------
!
!     START OF FORMULATION MADE FOR T11_3
      FORMULE(16:16)='Y'
      CALL MATRIX(BM2,'M=N     ',FORMULE,IELMH,IELMU,
     &            -TETAU,PP,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV1 , BM2 , VIT1 , C , MESH)
!
!-----------------------------------------------------------------------
!
!  COMPUTES CV2 AND CV3 :
!
!-----------------------------------------------------------------------
!  TERM  T21
!-----------------------------------------------------------------------
!
!  T21_1 : ADVECTION
!
!  T21_2 : (5B OF EB+AL)
!
      FORMULE(16:16)='X'
      CALL MATRIX(BM1,'M=N     ',FORMULE,IELMH,IELMU,
     &            1.D0,HH,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+CTAY',CV2,BM1,PP,TETAU-1.D0,MESH)
!
!                           U
!  T21_3 : BOUNDARY TERM TB1
!                           ADJ
!
!     JMH : I HAVE 1.D0 INSTEAD OF TETAU-1.D0
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            TETAU-1.D0,PP,T1,T1,HH,T1,T1,MESH,.TRUE.,
     &            MASK%ADR(8)%P)
      CALL OSDB('X=X+Y   ' , CV2 , T2 , T2 , C , MESH)
!     DIRICHLET ON VELOCITY U:
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,HH,T1,T1,MESH,
     &            .TRUE.,MASK%ADR(UDIR)%P)
      CALL OSDB('X=X+Y   ' , CV2 , T2 , T2 , C , MESH)
!     FREE FLOW ON U:
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,HH,T1,T1,MESH,
     &            .TRUE.,MASK%ADR(UDDL)%P)
      CALL OSDB('X=X+Y   ' , CV2 , T2 , T2 , C , MESH)
!
!-----------------------------------------------------------------------
! TERM  T31
!-----------------------------------------------------------------------
!
!  T31_1 : ADVECTION
!
!  T31_2 : (7B OF EB+AL)
!
      FORMULE(16:16)='Y'
      CALL MATRIX(BM2,'M=N     ',FORMULE,IELMH,IELMU,
     &            1.D0,HH,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+CTAY',CV3,BM2,PP,TETAU-1.D0,MESH)
! OLD PROGRAMMING (TBM2 FROM HN AT AN INCORRECT TIMESTEP)
!     CALL MATVEC('X=X+CAY ',CV3,TBM2,PP,(TETAU-1.D0)/TETAU,MESH)
!
!                           V
!  T31_3 : BOUNDARY TERM TB1
!                           ADJ
!
!     JMH : I HAVE 1.D0 INSTEAD OF TETAU-1.D0
      CALL VECTOR(T4,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            TETAU-1.D0,PP,T1,T1,T1,HH,T1,MESH,.TRUE.,
     &            MASK%ADR(8)%P)
      CALL OSDB('X=X+Y   ' , CV3 , T4 , T4 , C , MESH)
!     DIRICHLET ON VELOCITY V:
      CALL VECTOR(T4,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,T1,HH,T1,MESH,
     &            .TRUE.,MASK%ADR(VDIR)%P)
      CALL OSDB('X=X+Y   ' , CV3 , T4 , T4 , C , MESH)
!     FREE FLOW ON V:
      CALL VECTOR(T4,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,T1,HH,T1,MESH,
     &            .TRUE.,MASK%ADR(VDDL)%P)
      CALL OSDB('X=X+Y   ' , CV3 , T4 , T4 , C , MESH)
!
!-----------------------------------------------------------------------
! TERM  T22 (2 PARTS)
!-----------------------------------------------------------------------
!
!     TERM 4B OF AL       T
!     AM2 IS MASS/DT AT TIME IT+1
      CALL MATRIX(AM2,'M=N     ','MATMAS          ',IELMU,IELMU,
     &            1.D0/DT,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV2 , AM2 , QQ , C , MESH)
!
!     MISSES AN ADVECTION TERM
!
!
!-----------------------------------------------------------------------
! TERM  T32 (2 PARTS)
!-----------------------------------------------------------------------
!
!     TERM 6B OF AL
!     AM2 IS MASS/DT AT TIME IT+1
      CALL MATVEC('X=X+AY  ', CV3 , AM2 , RR , C , MESH)
!
!     MISSES AN ADVECTION TERM
!
!-----------------------------------------------------------------------
! TERMS  T23 AND T33
!-----------------------------------------------------------------------
!
!   ADVECTION : NOT YET IMPLEMENTED
!
!-----------------------------------------------------------------------
! TERM  T24+T25 AND T34+T35
!-----------------------------------------------------------------------
!
!     T5=U/(U^2+V^2)  C     T6=V/(U^2+V^2)
      CALL OS('X=YZ    ', X=T7 , Y=UU , Z=UU)
      CALL OS('X=X+YZ  ', X=T7 , Y=VV , Z=VV)
      CALL OS('X=+(Y,C)', X=T7 , Y=T7 , C=1.D-6)
      CALL OS('X=Y/Z   ', X=T5 , Y=UU , Z=T7)
      CALL OS('X=Y/Z   ', X=T6 , Y=VV , Z=T7)
!
!     ADD TERMS TO CV2, CV3
!
!     T3=U*Q+V*R (ALREADY DONE)
      CALL OS('X=XY    ', X=T5 , Y=T11)
      CALL OS('X=XY    ', X=T6 , Y=T11)
      CALL OS('X=X+YZ  ', X=CV2 ,Y=T5 , Z=T3)
      CALL OS('X=X+YZ  ', X=CV3 ,Y=T6 , Z=T3)
!
!=======================================================================
!
!     END OF COMPUTATION OF RIGHT HAND SIDE FOR ADJOINT SYSTEM
!
!=======================================================================
!
!
!     DIRICHLET CONDITIONS FOR ADJOINT VARIABLES
      CALL OS('X=C     ',X=ADJDIR,C=0.D0)
      CALL DIRICH(UNKADJ,MATADJ,RHS,ADJDIR,LIMPRO%I,
     &            TB,MESH,KDIR,MSK,MASKPT)
!
#if defined COMPAD
      CALL AD_SOLVE(UNKADJ,MATADJ,RHS,TB,SLVPRO,INFOGR,MESH,TM1)
#else
      CALL SOLVE(UNKADJ,MATADJ,RHS,TB,SLVPRO,INFOGR,MESH,TM1)
#endif
!
!     CONTRIBUTION TO COST FUNCTION
!
      CALL COST_FUNCTION(C,OPTCOST,'GRD')
!
!     PREPARES NEXT TIMESTEP
!
      CALL OS('X=Y     ', X=HIT1, Y=HH)
      CALL OS('X=Y     ', X=UIT1, Y=UU)
      CALL OS('X=Y     ', X=VIT1, Y=VV)
!
      IF(     INCLU2(ESTIME,'PERMANENT')
     &    .OR.INCLU2(ESTIME,'STEADY'   )  ) THEN
!
!       STEADY STATE : DOES NOT UPDATE DATA AND RESULTS,
!                      ONLY LAST TIMESTEP CONSIDERED
!
!       U AND V MODIFIED BY BORD, RESET HERE (H USEFUL ??)
        CALL OS('X=Y     ', X=H, Y=HN)
        CALL OS('X=Y     ', X=U, Y=UN)
        CALL OS('X=Y     ', X=V, Y=VN)
!
      ELSE
!
!       UNSTEADY STATE : UPDATES DATA AND RESULTS
!
        IF(LT.LT.NIT) THEN
!
!         HIT,.., HH,.. IN INITIAL CONDITIONS, SEE PROPIN_ADJ
          CALL OS('X=Y     ', X=HH, Y=HN)
          CALL OS('X=Y     ', X=UU, Y=UN)
          CALL OS('X=Y     ', X=VV, Y=VN)
!
!         READS TELEMAC2D RESULTS (RESULTS FILE - UNIT NRES)
!         SEE ALSO CONDIN_ADJ
!
          ITER=NIT-LT
!
          CALL GET_MESH_NPOIN(T2D_FILES(T2DRES)%FMT,
     &                        T2D_FILES(T2DRES)%LU,POINT_ELT_TYPE,
     &                        NPOIN,IERR)
          CALL CHECK_CALL(IERR,'PROPAG_ADJ:GET_MESH_NPOIN')
          CALL READ_DATASET(T2D_FILES(T2DRES)%FMT,T2D_FILES(T2DRES)%LU,
     &                      VARSOR,NPOIN,ITER,AT1,TEXTE,TROUVE,ALIRE,
     &                      LISTIN,.FALSE.,MAXVAR)
!
!         READS THE MEASUREMENTS (REFERENCE FILE - UNIT NREF)
!
          CALL MESURES(ITER,AT-DT)
!
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
