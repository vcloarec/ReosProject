!                   **********************
                    SUBROUTINE BIEF_ALLVEC
!                   **********************
!
     &( NAT , VEC , NOM , IELM , DIM2 , STATUT , MESH )
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    ALLOCATES MEMORY FOR A VECTOR STRUCTURE.
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

!history  R.NHEILI (Universite de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+       ALLOCATE THE ELEMENT E IN THE VECTOR STUCTURE (BIEF_OBJ)
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        21/01/2016
!+        V7P2
!+   Adding NAT = 3. With both integers and doube precision arrays
!+   allocated
!
!history  J-M HERVOUET (jubilado)
!+        07/09/2017
!+        V7P3
!+   Allowing several successive allocations of the same BIEF_OBJ.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIM2           |-->| SECOND DIMENSION OF VECTOR
!| IELM           |-->| TYPE OF ELEMENT, OR DIMENSION
!|                |   | (DEPENDING ON 'STATUT')
!| NAT            |<--| 1: DOUBLE PRECISION   2:VECTOR OF INTEGERS
!|                |   | 3: DOUBLE PRECISION AND VECTOR OF INTEGERS
!| NOM            |-->| FORTRAN NAME
!| REFINE         |-->| NUMBER OF REFINEMENT LEVELS
!| STATUT         |-->| VECTOR STATUS:
!|                |   | 0 : FREE VECTOR, IELM IS ITS DIMENSION
!|                |   | 1 : VECTOR DEFINED ON A MESH
!|                |   | IELM IS THEN THE ELEMENT TYPE
!|                |   | CHANGING DISCRETISATION FORBIDDEN
!|                |   | 2 : LIKE 1 BUT CHANGING DISCRETISATION ALLOWED
!| VEC            |<--| VECTOR TO BE ALLOCATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_ALLVEC => BIEF_ALLVEC
      USE DECLARATIONS_TELEMAC, ONLY : MODASS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: VEC
      INTEGER         , INTENT(IN)    :: NAT,IELM,DIM2,STATUT
      CHARACTER(LEN=6), INTENT(IN)    :: NOM
      TYPE(BIEF_MESH) , INTENT(IN)    :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ERR,IMAX,I
      LOGICAL INIT
      DOUBLE PRECISION XMAX
!
      INTRINSIC MAX
!
      ERR = 0
!
!-----------------------------------------------------------------------
!  HEADER COMMON TO ALL OBJECTS
!-----------------------------------------------------------------------
!
!     KEY OF THE OBJECT - TO CHECK MEMORY CRASHES
!
      VEC%KEY = 123456
!
!     TYPE OF THE OBJECT (HERE VECTOR)
!
      VEC%TYPE = 2
!
!     Defines how the object was created
!
      VEC%FATHER = 'XXXXXX'
!
!     NAME OF THE OBJECT
!
      VEC%NAME = NOM
!
!-----------------------------------------------------------------------
!  PART SPECIFIC TO VECTORS
!-----------------------------------------------------------------------
!
!     NATURE
!
      VEC%NAT = NAT
!
!     MAXIMUM SIZE PER DIMENSION
!
      IF(STATUT.EQ.1.OR.STATUT.EQ.2) THEN
        VEC%MAXDIM1 = BIEF_NBMPTS(IELM,MESH)
      ELSE
        VEC%MAXDIM1 = IELM
      ENDIF
!
!     VEC%MAXDIM1 MUST BE AT LEAST 1
!     TO AVOID BOUND CHECKING ERRORS ON SOME COMPILERS
!
      VEC%MAXDIM1=MAX(VEC%MAXDIM1,1)
!
!     DISCRETISES
!
      IF(STATUT.EQ.1.OR.STATUT.EQ.2) THEN
        VEC%ELM = IELM
      ELSE
        VEC%ELM = -1000
      ENDIF
!
!     FIRST DIMENSION OF VECTOR
!
      IF(STATUT.EQ.1.OR.STATUT.EQ.2) THEN
        VEC%DIM1 = BIEF_NBPTS(IELM,MESH)
      ELSE
        VEC%DIM1 = IELM
      ENDIF
!
!     SECOND DIMENSION OF VECTOR (VEC%DIM2 MAY BE CHANGED)
!
      VEC%DIM2    = DIM2
      VEC%MAXDIM2 = DIM2
!
!     CASE OF DISCONTINUITY BETWEEN ELEMENTS
!     (SEE CORRSL, VC13AA, VC13BB)
!
      VEC%DIMDISC = 0
!
!     STATUS
!
      VEC%STATUS = STATUT
!
!     INFORMATION ON CONTENT
!
      VEC%TYPR = '?'
      VEC%TYPI = '?'
!
!     DYNAMICALLY ALLOCATES MEMORY (REAL AND/OR INTEGER, DEPENDING OF NAT)
!
      IF(NAT.EQ.1.OR.NAT.EQ.3) THEN
!
        INIT=.FALSE.
        IF(.NOT.ASSOCIATED(VEC%R)) THEN
          ALLOCATE(VEC%R(VEC%MAXDIM1*VEC%DIM2),STAT=ERR)
          INIT=.TRUE.
        ELSEIF(SIZE(VEC%R).LT.VEC%MAXDIM1*VEC%DIM2) THEN
          WRITE(LU,*) 'DEALLOCATING AND REALLOCATING %R OF ',
     &                VEC%NAME,VEC%MAXDIM1,VEC%DIM2,SIZE(VEC%R)
          DEALLOCATE(VEC%R)
          ALLOCATE(VEC%R(VEC%MAXDIM1*VEC%DIM2),STAT=ERR)
          INIT=.TRUE.
        ENDIF
        CALL CHECK_ALLOCATE(ERR,'VECTOR '//VEC%NAME//'%R')
!
!       FILLS ARRAY WITH BIG NUMBERS
!       TO RAISE QUESTIONS IF NOT INITIALISED
!
        IF(INIT) THEN
          XMAX = HUGE(100.D0)
          CALL OV('X=C     ', X=VEC%R, C=XMAX,
     &            DIM1=VEC%MAXDIM1*VEC%DIM2)
        ENDIF
!
        IF(MODASS .EQ.3) THEN
          ALLOCATE(VEC%E(VEC%MAXDIM1*VEC%DIM2),STAT=ERR)
          CALL OV('X=C     ', X=VEC%E, C=0.D0,
     &            DIM1=VEC%MAXDIM1*VEC%DIM2)
        ENDIF
!
!
      ENDIF
!
      IF(NAT.EQ.2.OR.NAT.EQ.3) THEN
!
        INIT=.FALSE.
        IF(.NOT.ASSOCIATED(VEC%I)) THEN
          ALLOCATE(VEC%I(VEC%MAXDIM1*VEC%DIM2),STAT=ERR)
          INIT=.TRUE.
        ELSEIF(SIZE(VEC%I).LT.VEC%MAXDIM1*VEC%DIM2) THEN
          WRITE(LU,*) 'REALLOCATING %I OF ',VEC%NAME
          WRITE(LU,*) 'OLD SIZE: ',SIZE(VEC%I)
          WRITE(LU,*) 'NEW SIZE: ',VEC%MAXDIM1*VEC%DIM2
          DEALLOCATE(VEC%I)
          ALLOCATE(VEC%I(VEC%MAXDIM1*VEC%DIM2),STAT=ERR)
          INIT=.TRUE.
        ENDIF
        CALL CHECK_ALLOCATE(ERR,'VECTOR '//VEC%NAME//'%I')
!
!       FILLS ARRAY WITH BIG NUMBERS
!       TO RAISE QUESTIONS IF NOT INITIALISED
!
        IF(INIT) THEN
          IMAX = HUGE(100)
          DO I=1,VEC%MAXDIM1*VEC%DIM2
            VEC%I(I) = IMAX
          ENDDO
        ENDIF
!
      ENDIF
!
      IF(NAT.EQ.1) THEN
        NULLIFY(VEC%I)
      ELSEIF(NAT.EQ.2) THEN
        NULLIFY(VEC%R)
      ELSEIF(NAT.NE.3) THEN
        WRITE(LU,*) 'UNKNOWN NAT IN ALLVEC:',NAT
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

