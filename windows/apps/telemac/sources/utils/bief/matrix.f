!                   *****************
                    SUBROUTINE MATRIX
!                   *****************
!
     &(M,OP,FORMUL,IELM1,IELM2,XMUL,F,G,H,U,V,W,MESH,MSK,MASKEL)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS BETWEEN MATRICES.
!+
!+            THE MATRIX IS IDENTIFIED BY THE FORMULATION IN
!+                CHARACTER STRING FORMUL.
!code
!+     OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES HOW M IS
!+     MODIFIED. THE SYNTAX IS THE SAME AS THAT OF OM, FOR EXAMPLE.
!+
!+     OP='M=N     '
!+     OP='M=TN    '
!+     OP='M=M+N   '
!+     OP='M=M+TN  '
!+
!+     ALL THE OPERATIONS IN OM WHICH HAVE N ON THE RIGHT OF THE
!+     = SIGN ARE VALID.
!
!code
!+-----------------------------------------------------------------------
!+  MEANING OF IELM AND IELM2
!+
!+  TYPE OF ELEMENT      NUMBER OF POINTS
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
!+
!+-----------------------------------------------------------------------
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        25/06/2008
!+
!+   DOES NOT CALL MATRIY IF NUMBER OF ELEMENTS IS 0
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        14/10/2009
!+        V6P0
!+   ARGUMENTS ADDED TO ASSEX3
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
!+   Arguments added to MATRIY
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/03/2014
!+        V7P0
!+   Cases where there is no boundary element secured.
!
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+      ADD MODASS=3
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE FORMULA
!| FORMUL         |-->| FORMULA DESCRIBING THE RESULTING MATRIX
!| G              |-->| FUNCTION USED IN THE FORMULA
!| H              |-->| FUNCTION USED IN THE FORMULA
!| IELM1          |-->| TYPE OF ELEMENT FOR LINES
!| IELM2          |-->| TYPE OF ELEMENT FOR COLUMNS
!| M              |<->| MATRIX TO BE BUILT OR MODIFIED
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| OP             |-->| OPERATION TO BE DONE. IF '=' THEN M=...
!|                |   |                       IF '+' THEN M=M+
!| U              |-->| FUNCTION USED IN THE FORMULA (COMPONENT OF VECTOR)
!| V              |-->| FUNCTION USED IN THE FORMULA (COMPONENT OF VECTOR)
!| W              |-->| FUNCTION USED IN THE FORMULA (COMPONENT OF VECTOR)
!| XMUL           |-->| COEFFICIENT FOR MULTIPLICATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MATRIX => MATRIX
      USE DECLARATIONS_TELEMAC, ONLY : MODASS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: IELM1,IELM2
!
      DOUBLE PRECISION, INTENT(IN)   :: XMUL
!
      LOGICAL, INTENT(IN)            :: MSK
!
      CHARACTER(LEN=16), INTENT(IN)  :: FORMUL
      CHARACTER(LEN=8), INTENT(IN)   :: OP
!
      TYPE(BIEF_OBJ), INTENT(IN)     :: F,G,H,U,V,W,MASKEL
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: M
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NELMAX,NELEM,NPT,SS,I
      INTEGER, DIMENSION(:), POINTER :: IKLE
      INTEGER, DIMENSION(:), POINTER :: ELTSEG,ORISEG
      DOUBLE PRECISION C
      LOGICAL LEGO
!
!-----------------------------------------------------------------------
!
!     STORES 1 FOR THE WROKING ARRAY
!     CAN BE MODIFIED BY ASSEXT THEREAFTER
!
      MESH%M%STO = 1
!
!-----------------------------------------------------------------------
!  CALLS THE SHUNTING AND ASSEMBLY SUBROUTINE : MATRIY
!-----------------------------------------------------------------------
!
!     LEGO CAN BE MODIFIED BY MATRIY
      LEGO = .TRUE.
!
      IF(DIMENS(IELM1).EQ.MESH%DIM1) THEN
!       NORMAL MATRIX
        NELEM  = MESH%NELEM
        NELMAX = MESH%NELMAX
        IKLE   =>MESH%IKLE%I
        ELTSEG=>MESH%ELTSEG%I
        ORISEG=>MESH%ORISEG%I
        IF (MODASS .EQ.3) THEN
          DO I=1, MESH%M%D%DIM1
            MESH%M%D%E=0.D0
          ENDDO
        ENDIF
      ELSE
!       BOUNDARY MATRIX
        NELEM  = MESH%NELEB
        NELMAX = MESH%NELEBX
        IKLE   =>MESH%IKLBOR%I
        ELTSEG=>MESH%ELTSEGBOR%I
        ORISEG=>MESH%ORISEGBOR%I
      ENDIF
!
!     MATRIY FILLS THE DIAGONAL AND EXTRA DIAGONAL TERMS
!
!     REFLECTS CHOICE OF PRE-ASSEMBLY STORAGE
      IF(M%STO.EQ.1.OR.M%STO.EQ.3) THEN
        SS = 1
      ELSEIF(M%STO.EQ.2) THEN
        SS = 2
      ELSE
        WRITE(LU,*) 'UNKNOWN STORAGE IN MATRIX'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(NELEM.GT.0) THEN
        CALL MATRIY(FORMUL,MESH%M%X%R,
     &              MESH%M%TYPDIA,MESH%M%TYPEXT,
     &              XMUL,F,G,H,U,V,W,
     &              F%R,G%R,H%R,U%R,V%R,W%R,
     &              MESH%W%R,LEGO,
     &              MESH%XEL%R,MESH%YEL%R,MESH%ZEL%R,
     &              MESH%X%R,MESH%Y%R,MESH%Z%R,
     &              MESH%SURFAC%R,MESH%LGSEG%R,
     &              MESH%IKLE%I,MESH%IKLBOR%I,MESH%NBOR%I,
     &              MESH%NELBOR%I,MESH%NULONE%I,
     &              MESH%NELEM,MESH%NELMAX,
     &              MESH%NELEB,MESH%NELEBX,IELM1,IELM2,SS,
     &              MESH%NPOIN/BIEF_NBPTS(11,MESH),MESH,NELMAX,
     &              MESH%M%STOX)
      ENDIF
!
!  UPDATES THE INFORMATION OF THE MATRIX
!
      NPT = BIEF_NBPTS(MIN(IELM1,IELM2),MESH)
!
      IF(NPT.GT.MESH%M%D%MAXDIM1) THEN
        WRITE(LU,501) MESH%M%NAME
        WRITE(LU,2001) IELM1
        WRITE(LU,3001) IELM2
        CALL PLANTE(1)
        STOP
      ENDIF
!
      MESH%M%D%ELM  = MIN(IELM1,IELM2)
      MESH%M%D%DIM1 = NPT
      MESH%M%ELMLIN = IELM1
      MESH%M%ELMCOL = IELM2
!
!  ASSEMBLES THE DIAGONAL (POSSIBLY)
!
      IF(LEGO.AND.MESH%M%TYPDIA.EQ.'Q'.AND.NELEM.GT.0) THEN
!
        IF (MODASS .EQ.3) THEN
          CALL ASSVEC(MESH%M%D%R,
     &                  IKLE,NPT,NELEM,NELMAX,
     &                  MESH%W%R,LEGO,MESH%LV,MSK,MASKEL%R,
     &                  BIEF_NBPEL(MESH%M%D%ELM,MESH),MESH%M%D%E)
!
        ELSE
          CALL ASSVEC(MESH%M%D%R,
     &                  IKLE,NPT,NELEM,NELMAX,
     &                  MESH%W%R,LEGO,MESH%LV,MSK,MASKEL%R,
     &                  BIEF_NBPEL(MESH%M%D%ELM,MESH))
        ENDIF
!
      ENDIF
!
!  MASKS EXTRA-DIAGONAL TERMS (POSSIBLY)
!
      IF(MSK.AND.NELEM.GT.0) THEN
        CALL OM('M=MSK(M)',M=MESH%M,N=MESH%M,D=MASKEL,MESH=MESH)
      ENDIF
!
!  ASSEMBLES EXTRA-DIAGONAL TERMS (POSSIBLY)
!
      IF(M%STO.EQ.3) THEN
!       COPIES THE DIAGONAL
        CALL OS('X=Y     ',X=MESH%MSEG%D,Y=MESH%M%D)
        MESH%MSEG%TYPDIA(1:1)='Q'
!       ASSEMBLES EXTRA-DIAGONAL TERMS
        IF(MESH%M%TYPEXT.EQ.'Q'.OR.MESH%M%TYPEXT.EQ.'S') THEN
!
!         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!         CASE OF MATRICES WITH INVERTED STORAGE OF OFF-DIAGONAL TERMS
!         SEE INVERSION OF DIM1_EXT AND DIM2_EXT IN CALL TO ASSEX3
!
!         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
          IF(MESH%M%STOX.EQ.2) THEN
            CALL ASSEX3(MESH%MSEG%X%R,MESH%M%STO,MESH%M%NAME,
     &                  MESH%M%ELMLIN,MESH%M%ELMCOL,
     &                  MESH%M%TYPEXT,MESH%M%X%R,
     &                  BIEF_DIM2_EXT(IELM1,IELM2,MESH%M%STO,
     &                                MESH%M%TYPEXT,MESH),
     &                  BIEF_DIM1_EXT(IELM1,IELM2,MESH%M%STO,
     &                                MESH%M%TYPEXT,MESH),
     &                  MESH%M%STOX,
     &                  MESH,NELMAX,ELTSEG,ORISEG)
          ELSEIF(MESH%M%STOX.EQ.1) THEN
            CALL ASSEX3(MESH%MSEG%X%R,MESH%M%STO,MESH%M%NAME,
     &                  MESH%M%ELMLIN,MESH%M%ELMCOL,
     &                  MESH%M%TYPEXT,MESH%M%X%R,
     &                  BIEF_DIM1_EXT(IELM1,IELM2,MESH%M%STO,
     &                                MESH%M%TYPEXT,MESH),
     &                  BIEF_DIM2_EXT(IELM1,IELM2,MESH%M%STO,
     &                                MESH%M%TYPEXT,MESH),
     &                  MESH%M%STOX,
     &                  MESH,NELMAX,ELTSEG,ORISEG)
          ELSE
            WRITE(LU,*) 'MATRIX: UNKNOWN STORAGE:'
            WRITE(LU,*) 'MESH%M%STOX= : ',MESH%M%STOX
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
        MESH%MSEG%TYPEXT=MESH%M%TYPEXT
        MESH%MSEG%ELMLIN = IELM1
        MESH%MSEG%ELMCOL = IELM2
        MESH%MSEG%D%ELM  = MIN(IELM1,IELM2)
        MESH%MSEG%D%DIM1 = NPT
        MESH%MSEG%X%DIM1 = BIEF_DIM1_EXT(IELM1,IELM2,M%STO,
     &                                   MESH%MSEG%TYPEXT,MESH)
        MESH%MSEG%X%DIM2 = BIEF_DIM2_EXT(IELM1,IELM2,M%STO,
     &                                   MESH%MSEG%TYPEXT,MESH)
      ENDIF
!
!     DIMENSIONS OF THE ARRAY WITH EXTRADIAGONAL TERMS
!     BEWARE M%STO (NOT MESH%M%STO BECAUSE IT EQUALS 1)
!                   SEE BEGINNING OF SUBROUTINE
!
      MESH%M%X%DIM1 = BIEF_DIM1_EXT(IELM1,IELM2,M%STO,
     &                              MESH%M%TYPEXT,MESH)
      MESH%M%X%DIM2 = BIEF_DIM2_EXT(IELM1,IELM2,M%STO,
     &                              MESH%M%TYPEXT,MESH)
!
!-----------------------------------------------------------------------
!  UPDATES M AFTER WORK ON MESH%M IS COMPLETE
!-----------------------------------------------------------------------
!
      IF(NELEM.GT.0) THEN
!
        IF(M%STO.EQ.1) THEN
          CALL OM(OP, M, MESH%M, F, C, MESH)
        ELSEIF(M%STO.EQ.3) THEN
          CALL OM(OP, M, MESH%MSEG, F, C, MESH)
        ELSE
          WRITE(LU,*) 'MATRIX: UNKNOWN STORAGE : ',M%STO
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
501   FORMAT(1X,'MATRIX (BIEF) : MATRIX ',A6,' TOO SMALL')
2001  FORMAT(1X,'                FOR IELM1 = ',1I6)
3001  FORMAT(1X,'                AND IELM2 = ',1I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
