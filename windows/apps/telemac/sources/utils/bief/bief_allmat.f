!                   **********************
                    SUBROUTINE BIEF_ALLMAT
!                   **********************
!
     &( MAT , NOM , IELM1 , IELM2 , CFG , TYPDIA , TYPEXT , MESH )
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    ALLOCATES MEMORY FOR A REAL MATRIX STRUCTURE.
!
!history  J-M HERVOUET (LNH)
!+        01/03/95
!+        V5P1
!+   First version.
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
!history  J-M HERVOUET (jubilado)
!+        04/11/2016
!+        V7P3
!+   Allowing several successive allocations of the same BIEF_OBJ.
!
!history  J-M HERVOUET (jubilado)
!+        08/09/2017
!+        V7P3
!+   Optional argument REFINE removed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CFG            |-->| INFORMATION ON STORAGE OF MATRIX
!|                |   | CFG(1): STORAGE   CFG(2): MATRIX-VECTOR PRODUCT
!| IELM1          |-->| TYPE OF ELEMENT PER LINE
!| IELM2          |-->| TYPE OF ELEMENT PER COLUMN
!| MAT            |<->| THE MATRIX TO BE ALLOCATED
!| MESH           |-->| MESH STRUCTURE
!| NOM            |-->| FORTRAN NAME OF MATRIX
!| TYPDIA         |-->| TYPE OF DIAGONAL ('Q', 'I' OU '0')
!| TYPEXT         |-->| TYPE OF OFF-DIAGONAL TERMS :'Q','S' OU '0'
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_ALLMAT => BIEF_ALLMAT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: MAT
      CHARACTER(LEN=6), INTENT(IN)    :: NOM
      INTEGER         , INTENT(IN)    :: IELM1,IELM2,CFG(2)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPDIA,TYPEXT
      TYPE(BIEF_MESH) , INTENT(IN)    :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELMD
!
      CHARACTER(LEN=6) :: NAME
!
!-----------------------------------------------------------------------
!  HEADER COMMON TO ALL OBJECTS
!-----------------------------------------------------------------------
!
!     KEY OF THE OBJECT - TO CHECK MEMORY CRASHES
!
      MAT%KEY = 123456
!
!     TYPE OF THE OBJECT (HERE MATRIX)
!
      MAT%TYPE = 3
!
!     Defines how the object was created
!
      MAT%FATHER = 'XXXXXX'
!
!     NAME OF THE OBJECT
!
      MAT%NAME = NOM
!
!-----------------------------------------------------------------------
!  PART SPECIFIC TO MATRICES
!-----------------------------------------------------------------------
!
!     ELEMENT OF DIAGONAL (SMALLEST ELEMENT IF MATRIX IS RECTANGULAR)
      IELMD = IELM1
      IF(BIEF_NBPTS(IELM2,MESH).LT.BIEF_NBPTS(IELM1,MESH)) IELMD = IELM2
!
!  TYPE OF STORAGE
!
      MAT%STO = CFG(1)
!
!  TYPES OF ELEMENTS FOR LINE AND COLUMN
!
      MAT%ELMLIN = IELM1
      MAT%ELMCOL = IELM2
!
!     ALLOCATES THE DIAGONAL (UNTIL HERE MAT%D IS ONLY A POINTER)
!
!     MAT%D WILL POINT TO A BIEF_OBJ
      IF(.NOT.ASSOCIATED(MAT%D)) CALL FIRST_ALL_BIEFOBJ(MAT%D)
!
      NAME = 'D' // NOM(1:5)
      IF(TYPDIA(1:1).EQ.'Q') THEN
!       ONLY CASE WHERE THE DIAGONAL DOES EXIST
        CALL BIEF_ALLVEC(1,MAT%D,NAME,IELMD,1,2,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,MAT%D,NAME,0    ,1,0,MESH)
      ENDIF
!     TYPE IS FORGOTTEN UNTIL INITIALISATION OF MATRIX
!     MAT%TYPDIA = TYPDIA(1:1)
      MAT%TYPDIA = '?'
!
!     ALLOCATES OFF-DIAGONAL TERMS (AS FOR DIAGONAL)
!
      IF(.NOT.ASSOCIATED(MAT%X)) CALL FIRST_ALL_BIEFOBJ(MAT%X)
!
      NAME = 'X' // NOM(1:5)
!
      CALL BIEF_ALLVEC(1,MAT%X,NAME,
     &            BIEF_DIM1_EXT(IELM1,IELM2,CFG(1),TYPEXT,MESH),
     &            BIEF_DIM2_EXT(IELM1,IELM2,CFG(1),TYPEXT,MESH),0,MESH)
!
!     TYPE IS FORGOTTEN UNTIL INITIALISATION OF MATRIX
!     MAT%TYPEXT = TYPEXT(1:1)
      MAT%TYPEXT = '?'
!
!     MATRIX X VECTOR PRODUCT
      MAT%PRO = CFG(2)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
