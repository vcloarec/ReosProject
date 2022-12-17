!                   *****************
                    SUBROUTINE VECTOR
!                   *****************
!
     &(VEC,OP,FORMUL,IELM1,XMUL,F,G,H,U,V,W,MESH,MSK,MASKEL,LEGO,ASSPAR)
!
!***********************************************************************
! BIEF   V7P0                                   08/01/2014
!***********************************************************************
!
!brief    COMPUTES VECTORS.
!+
!+            THE VECTOR IS IDENTIFIED BY THE FORMULATION IN
!+                THE CHARACTER STRING 'FORMUL'.
!+
!+            'OP' IS = OR +.
!code
!+  MEANING OF IELM1
!+
!+  TYPE OF ELEMENT      NUMBER OF POINTS
!+
!+   0 : P0 SEGMENT             1
!+   1 : P1 SEGMENT             2
!+
!+  10 : P0 TRIANGLE            1
!+  11 : P1 TRIANGLE            3
!+  12 : QUASI-BUBBLE TRIANGLE  4
!+
!+  20 : Q0 QUADRILATERAL       1
!+  21 : Q1 QUADRILATERAL       4
!+
!+  40 : TELEMAC-3D P0 PRISMS   1
!+  41 : TELEMAC-3D P1 PRISMS   6
!
!
!history  JM HERVOUET (LNHE)
!+        25/06/2008
!+        V5P9
!+   NO CALL TO VECTOS IF THE NUMBER OF ELEMENTS IS 0
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        11/01/2013
!+        V6P3
!+   Arguments added to VECTOS
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        03/05/2013
!+        V6P3
!+   Checking size of vector, and status=0 allowed if OP='='.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        08/01/2014
!+        V7P0
!+   New optional argument ASSPAR, to assemble the vector in parallel.
!+   This will avoid a lot of CALL PARCOM everywhere.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ASSPAR         |-->| IF YES, RARALLEL ASSEMBLY OF THE VECTOR IS DONE
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA (BIEF_OBJ)
!| FORMUL         |-->| STRING WITH THE FORMULA DESCRIBING THE VECTOR
!| G              |-->| FUNCTION USED IN THE VECTOR FORMULA (BIEF_OBJ)
!| H              |-->| FUNCTION USED IN THE VECTOR FORMULA (BIEF_OBJ)
!| IELM1          |-->| TYPE OF ELEMENT
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| OP             |-->| '=' : WE DO VEC= THE VECTOR
!|                |   | '+' : WE DO VEC=VEC+ THE VECTOR
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA (BIEF_OBJ)
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA (BIEF_OBJ)
!| W              |-->| FUNCTION USED IN THE VECTOR FORMULA (BIEF_OBJ)
!| VEC            |<->| RESULTING VECTOR
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VECTOR => VECTOR
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: VEC
      DOUBLE PRECISION,  INTENT(IN)    :: XMUL
      INTEGER,           INTENT(IN)    :: IELM1
      LOGICAL,           INTENT(IN)    :: MSK
      CHARACTER(LEN=16), INTENT(IN)    :: FORMUL
      CHARACTER(LEN=1),  INTENT(IN)    :: OP
      TYPE(BIEF_OBJ),    INTENT(IN)    :: F,G,H,U,V,W,MASKEL
      TYPE(BIEF_MESH),   INTENT(INOUT) :: MESH
      LOGICAL, OPTIONAL, INTENT(IN)    :: LEGO,ASSPAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  :: NPT                  ! NUMBER OF POINTS PER ELEMENT
      INTEGER  :: DIM1T                ! FIRST DIMENSION OF T IN VECTOS
      LOGICAL  :: LLEGO,AASSPAR        ! ASSEMBLY OR NOT
      INTEGER  :: IELM0                ! P0 DISCRETISATION
!
      IF(PRESENT(LEGO)) THEN
        LLEGO=LEGO
      ELSE
        LLEGO=.TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!  POSSIBLE CHANGE OF DISCRETISATION
!-----------------------------------------------------------------------
! IF A VECTOR HAS BEEN ALLOCATED WITH STATUS 1, I.E ITS DISCRETISATION
! CANNOT CHANGE, IT IS NECESSARY TO TEST THE COHERENCE BETWEEN THE
! DISCRETISATION OF THE VECTOR AND THAT PROPOSED IN ARGUMENT.
! MIGHT HAVE SURPRISES OTHERWISE !!!!
!
      IF(VEC%STATUS.EQ.1.AND.VEC%ELM.NE.IELM1) THEN
        WRITE(LU,1002) VEC%NAME,VEC%ELM,IELM1
1002    FORMAT(1X,'VECTOR: CHANGING DISCRETIZATION FORBIDDEN',
     &  ' FOR THE VECTOR ',A6,' : ',1I6,' <=> ',1I6)
        CALL PLANTE(1)
        STOP
      ELSEIF(VEC%STATUS.EQ.2.OR.VEC%STATUS.EQ.1.OR.
     &      (VEC%STATUS.EQ.0.AND.OP.EQ.'=')) THEN
        NPT = BIEF_NBPTS(IELM1,MESH)
        VEC%ELM = IELM1
        IF(NPT.GT.VEC%MAXDIM1) THEN
          WRITE(LU,*) 'VECTOR ',VEC%NAME
          WRITE(LU,*) 'HAS A FIRST DIMENSION OF: ',VEC%MAXDIM1
          WRITE(LU,*) 'IT CANNOT BE USED IN VECTOR'
          WRITE(LU,*) 'FOR A SIZE OF ',NPT
          CALL PLANTE(1)
          STOP
        ELSE
          VEC%DIM1= NPT
          IF(VEC%STATUS.EQ.0) VEC%STATUS=2
        ENDIF
      ELSE
        WRITE(LU,*) 'VECTOR ',VEC%NAME,' HAS A STATUS ',VEC%STATUS,
     &              ' IT CANNOT BE USED IN SUBROUTINE VECTOR'
        CALL PLANTE(1)
        STOP
      ENDIF
!
! ASSEMBLY: NOT PERFORMED FOR VECTORS
! RESULT OF P0 DISCRETISATION
! LLEGO IS SET TO TRUE IF THE RESULTANT VECTOR DISCRETISATION
! IS P1, FALSE OTHERWISE.
!
      IELM0 = 10*(IELM1/10)
      IF(IELM0.EQ.IELM1) LLEGO=.FALSE.
!
!-----------------------------------------------------------------------
!  CALLS THE SUBROUTINE THAT SHUNTS AND ASSEMBLES
!-----------------------------------------------------------------------
!
      IF(DIMENS(IELM1).EQ.MESH%DIM1) THEN
        DIM1T=MESH%NELMAX
      ELSE
        DIM1T=MESH%NELEBX
      ENDIF
!
!-----------------------------------------------------------------------
!  OPTIONAL ASSEMBLY: VALUES OF VEC SUMMED AT INTERFACE POINTS
!-----------------------------------------------------------------------
!
      AASSPAR=.FALSE.
      IF(NCSIZE.GT.1) THEN
        IF(PRESENT(ASSPAR).AND.DIMENS(IELM1).EQ.MESH%DIM1) THEN
          AASSPAR=ASSPAR
        ENDIF
      ENDIF
!
      IF(DIMENS(IELM1).EQ.MESH%DIM1.OR.MESH%NELEB.GT.0) THEN
!
        CALL VECTOS(VEC,VEC%R,OP,FORMUL,XMUL,
     &              F%R,G%R,H%R,U%R,V%R,W%R,
     &              F,G,H,U,V,W,MESH%W%R,LLEGO,
     &              MESH%XEL%R   , MESH%YEL%R   ,
     &              MESH%X%R     , MESH%Y%R     , MESH%Z%R    ,
     &              MESH%SURFAC%R, MESH%LGSEG%R ,
     &              MESH%IKLE%I  , MESH%IKLBOR%I, MESH%NBOR%I ,
     &              MESH%XSGBOR%R, MESH%YSGBOR%R,
     &              NPT,MESH%NELEM,MESH%NELEB,
     &              MESH%NELMAX,MESH%NELEBX,
     &              IELM1,MESH%LV,MSK,MASKEL%R,MESH,DIM1T,
     &              MESH%NELBOR%I,MESH%NULONE%I,AASSPAR)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
