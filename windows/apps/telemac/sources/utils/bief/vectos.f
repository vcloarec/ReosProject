!                   *****************
                    SUBROUTINE VECTOS
!                   *****************
!
     &(SVEC,VEC,OP,FORMUL,
     & XMUL,F,G,H,U,V,W,SF,SG,SH,SU,SV,SW,
     & T,LEGO,
     & XEL,YEL,XPT,YPT,ZPT,SURFAC,LGSEG,IKLE,IKLBOR,NBOR,
     & XNOR,YNOR,NPT,NELEM,NELEB,NELMAX,NELEBX,
     & IELM1,LV,MSK,MASKEL,MESH,DIM1T,NELBOR,NULONE,ASSPAR)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    COMPUTES VECTORS.
!+
!+            THE VECTOR IS IDENTIFIED BY THE FORMULATION IN
!+                THE CHARACTER STRING 'FORMUL'.
!code
!+  MEANING OF IELM1
!+
!+  TYPE OF ELEMENT      NUMBER OF POINTS
!+
!+  10 : P0 TRIANGLE            1
!+  11 : P1 TRIANGLE            3
!+  12 : QUASI-BUBBLE TRIANGLE  4
!+  13 : P2 TRIANGLE            6
!+
!+  20 : Q0 QUADRILATERAL       1
!+  21 : Q1 QUADRILATERAL       4
!+
!+  40 : TELEMAC-3D P0 PRISMS   1
!+  41 : TELEMAC-3D P1 PRISMS   6
!
!history  JM HERVOUET (LNHE)
!+        16/07/07
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
!history  J-M HERVOUET (LNHE)
!+        06/12/2011
!+        V6P2
!+   Call of VC13TT modified.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        07/01/2013
!+        V6P3
!+   Arguments XPT, YPT and ZPT added, various XEL, YEL and ZEL changed
!+   into XPT, etc. in the calls to 3D vectors.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        09/05/2014
!+        V7P0
!+   Adaptation to assembly with I8 integers.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        12/05/2014
!+        V7P0
!+   Discontinuous elements better treated: new types 15, 16 and 17 for
!+   discontinuous linear, quasi-bubble, and quadratic, rather than
!+   using component DIMDISC=11, 12 or 13.
!
!history  A. JOLY (EDF LAB, LNHE)
!+        27/08/2015
!+        V7P1
!+   Adding vc05aa.
!
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+        ADD MODASS 3
!
!history  A. BOURGOIN (EDF LAB, LNHE)
!+        01/08/2016
!+        V7P
!+   Adding vc17aa et vc18aa for spalart allmalas.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIM1T          |-->| FIRST DIMENSION OF T (NELMAX OR NELEBX)
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| FORMUL         |-->| STRING WITH THE FORMULA DESCRIBING THE VECTOR
!| G              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| H              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| IELM1          |-->| TYPE OF ELEMENT
!| IKLBOR         |-->| CONNECTIVITY TABLE OF BOUNDARY ELEMENTS.
!| IKLE           |-->| CONNECTIVITY TABLE.
!| LEGO           |-->| IF YES : THE VECTOR WILL BE ASSEMBLED
!| LGSEG          |-->| LENGTH OF BOUNDARY SEGMENTS
!| LV             |-->| VECTOR LENGTH OF THE MACHINE
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELBOR         |-->| ADJACENT ELEMENT NUMBER
!| NELEB          |-->| NUMBER OF BOUNDARY ELEMENTS
!| NELEBX         |-->| MAXIMUM NUMBER OF BOUNDARY ELEMENTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELEM_AS       |-->| NUMBER OF ELEMENTS FOR THE CALL TO ASSVEC
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NELMAX_AS      |-->| MAXIMUM NUMBER OF ELEMENTS FOR THE CALL TO ASSVEC
!| NPT            |-->| NUMBER OF POINTS OF VECTOR.
!| NULONE         |-->| LOCAL NUMBERING OF BOUNDARY ELEMENT IN ADJACENT
!|                |   | ELEMENT.
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SG             |-->| BIEF_OBJ STRUCTURE OF G
!| SH             |-->| BIEF_OBJ STRUCTURE OF H
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| SW             |-->| BIEF_OBJ STRUCTURE OF W
!| SURFAC         |-->| AREA OF TRIANGLES
!| T              |-->| WORK ARRAY WITH THE NON ASSEMBLED VECTOR
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| VEC            |<->| RESULTING VECTOR
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| XNOR           |-->| X-COMPONENT OF NORMAL VECTOR
!| YNOR           |-->| Y-COMPONENT OF NORMAL VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VECTOS => VECTOS
      USE DECLARATIONS_TELEMAC, ONLY : MODASS
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELMAX,NPT,NELEM,IELM1,LV,NELEB,NELEBX
      INTEGER, INTENT(IN) :: DIM1T
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*),NBOR(*),IKLBOR(NELEBX,*)
      INTEGER, INTENT(IN) :: NELBOR(NELEBX),NULONE(NELEBX,*)
!
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX),LGSEG(NELEB)
      DOUBLE PRECISION, INTENT(IN)    :: XEL(*),YEL(*)
      DOUBLE PRECISION, INTENT(IN)    :: XPT(*),YPT(*),ZPT(*)
      DOUBLE PRECISION, INTENT(IN)    :: XNOR(*),YNOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: T(DIM1T,*),VEC(*)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL,MASKEL(*)
!
!     STRUCTURES OF FUNCTIONS F, G, H, U, V, W AND REAL DATA
!
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*),H(*),U(*),V(*),W(*)
!
      TYPE(BIEF_OBJ), INTENT(IN)     :: SF,SG,SH,SU,SV,SW
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: SVEC
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
      LOGICAL, INTENT(IN) :: MSK,LEGO,ASSPAR
!
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
      CHARACTER(LEN=1), INTENT(IN) ::  OP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ICOORD,IDP,NDP,I
      LOGICAL INIT,SPECAD
      INTEGER NELEM2
!
      INTEGER(KIND=K8), POINTER :: PWI8(:)
!
      DOUBLE PRECISION QT
      INTEGER NPLAN
      IF(MESH%DIM1.EQ.2) THEN
        NPLAN=1
      ELSE
        NPLAN=BIEF_NBPTS(41,MESH)/BIEF_NBPTS(11,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CHECKS THE TYPE OF VECTOR
!
!=======================================================================
!     MASS MATRIX VECTOR X VECTOR F EQUALS 1
!=======================================================================
!
      IF(FORMUL(1:16).EQ.'MASBAS          '.OR.
     &   FORMUL(1:16).EQ.'MASBAS2         '     ) THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
          CALL VC00AA(XMUL,SURFAC,NELEM,NELMAX,T(1,1),T(1,2),T(1,3))
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
        ELSEIF(IELM1.EQ.12) THEN
!
          CALL VC00BB(XMUL,SURFAC,NELEM,NELMAX,
     &                T(1,1),T(1,2),T(1,3),T(1,4))
!
!
!-----------------------------------------------------------------------
!
!       ELEMENT P2 TRIANGLE
!
        ELSEIF(IELM1.EQ.13) THEN
!
          CALL VC00CC(XMUL,SURFAC,NELEM,NELMAX,
     &                T(1,1),T(1,2),T(1,3),T(1,4),T(1,5),T(1,6))
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!         CALL VC00OO(XMUL,SURFAC,NELEM,NELMAX,T(1,1),T(1,2))
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 PRISM
!
        ELSEIF(IELM1.EQ.41) THEN
!
          CALL VC00PP(XMUL,ZPT,SURFAC,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                T(1,3),T(1,4),T(1,5),T(1,6),FORMUL)
!
!-----------------------------------------------------------------------
!
!       ELEMENT TETRAHEDRON T1
!
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
          CALL VC00TT(XMUL,XPT,YPT,ZPT,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),FORMUL,
     &                BIEF_NBPTS(11,MESH),BIEF_NBPTS(10,MESH),IELM1)
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE (SIDE FACES OF A MESH OF PRISMS
!                            SPLIT IN TETRAHEDRONS)
!
        ELSEIF(IELM1.EQ.61) THEN
!
          CALL VC00FT(XMUL,XPT,YPT,ZPT,
     &                IKLBOR(1,1),IKLBOR(1,2),IKLBOR(1,3),NBOR,
     &                NELEB,NELEBX,T(1,1),T(1,2),T(1,3))
!
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 QUADRILATERAL
!
        ELSEIF(IELM1.EQ.71) THEN
!
!         FOR VERTICAL RECTANGULAR SIDES OF THE PRISMS
          CALL VC00FF(XMUL,XEL,YEL,ZPT,
     &                IKLBOR(1,1),IKLBOR(1,2),
     &                IKLBOR(1,3),IKLBOR(1,4),NBOR,
     &                NELEB,NELEBX,T(1,1),T(1,2),T(1,3),T(1,4),
     &                NELBOR,NULONE,NELMAX)
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     MASS MATRIX VECTOR X VECTOR F
!=======================================================================
!
      ELSEIF(FORMUL(1:6).EQ.'MASVEC') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
          CALL VC01AA(XMUL,SF,F,SURFAC,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                NELEM,NELMAX,T(1,1),T(1,2),T(1,3) )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE (SIDE FACES OF A MESH OF PRISMS
!                            SPLIT IN TETRAHEDRONS)
!
        ELSEIF(IELM1.EQ.61.OR.IELM1.EQ.81) THEN
!
          IF(FORMUL(7:7).NE.'2') THEN
!
            CALL VC01FT(XMUL,SF,F,XPT,YPT,ZPT,
     &                  IKLBOR(1,1),IKLBOR(1,2),
     &                  IKLBOR(1,3),NBOR,
     &                  NELEB,NELEBX,T(1,1),T(1,2),T(1,3))
          ELSE
!
            CALL VC01FT2(XMUL,SF,F,SG,G,XPT,YPT,ZPT,
     &                   IKLBOR(1,1),IKLBOR(1,2),
     &                   IKLBOR(1,3),NBOR,
     &                   NELEB,NELEBX,T(1,1),T(1,2),T(1,3))
          ENDIF
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
        ELSEIF(IELM1.EQ.12) THEN
!
          CALL VC01BB(XMUL,SF,F,SURFAC,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4) )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
        ELSEIF(IELM1.EQ.1) THEN
!
          CALL VC01OO(XMUL,SF,F,LGSEG,
     &                IKLBOR(1,1),IKLBOR(1,2),NBOR,NELEB,NELEBX,
     &                T(1,1),T(1,2)  )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 QUADRILATERAL (IN A 3D MESH OF PRISMS)
!
        ELSEIF(IELM1.EQ.71) THEN
!
!         FOR VERTICAL RECTANGULAR SIDES OF THE PRISMS
          CALL VC01FF(XMUL,SF,F,XEL,YEL,ZPT,
     &                IKLBOR(1,1),IKLBOR(1,2),
     &                IKLBOR(1,3),IKLBOR(1,4),NBOR,
     &                NELEB,NELEBX,T(1,1),T(1,2),T(1,3),T(1,4),
     &                NELBOR,NULONE,NELMAX)
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 PRISM
!
        ELSEIF(IELM1.EQ.41) THEN
!
          CALL VC01PP(XMUL,SF,F,ZPT,SURFAC,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                T(1,3),T(1,4),T(1,5),T(1,6))
!
!-----------------------------------------------------------------------
!
!       ELEMENT T1 TETRAHEDRON
!
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
          CALL VC01TT(XMUL,SF,F,XPT,YPT,ZPT,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4))
!
!-----------------------------------------------------------------------
!
!       ELEMENT T0 TETRAHEDRON
!
        ELSEIF(IELM1.EQ.30) THEN
!
          CALL VC01TT0(XMUL,SF,F,XPT,YPT,ZPT,
     &                IKLE(:,1),IKLE(:,2),IKLE(:,3),IKLE(:,4),
     &                NELEM,NELMAX,VEC)
!
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!                 -    ---->   --->
!     VECTOR:     F  . GRAD(U).GRAD(PSI)
!
!     F DIFFUSION TENSOR WITH DIAGONAL COMPONENTS F, G, H
!
!     EQUIVALENT OF MATDIF * U
!
!=======================================================================
!
      ELSEIF(FORMUL(1:6).EQ.'VECDIF') THEN
!
!       ELEMENT SEGMENT P1
!
        IF(IELM1.EQ.41.AND.FORMUL(8:8).EQ.'*') THEN
!
          CALL VC02PP_STAR(XMUL,SF,SG,SH,SU,F,G,H,U,XEL,YEL,ZPT,SURFAC,
     &                     IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                     IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),
     &                     T(1,2),T(1,3),T(1,4),T(1,5),T(1,6),FORMUL)
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR K GRAD(PSI) U.GRAD(F)
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'SUPG            ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
          CALL VC03AA(XMUL,SF,SG,SH,SU,SV,F,G,H,U,V,
     &                XEL,YEL,SURFAC,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                T(1,1),T(1,2),T(1,3) )
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
        ELSEIF(IELM1.EQ.12) THEN
!
          CALL VC03BB(XMUL,SF,SG,SH,SU,SV,F,G,H,U,V,
     &                XEL,YEL,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4) )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!         CALL VC03OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    &                XEL,YEL,ZEL,SURFAC,
!    &                IKLE(1,1),IKLE(1,2),NELEM,NELMAX,
!    &                T(1,1),T(1,2)  )
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR U GRAD(PSI)
!=======================================================================
!
      ELSEIF(FORMUL(1:6).EQ.'VGRADP') THEN
!
        SPECAD = .FALSE.
        IF(FORMUL(8:8).EQ.'2') SPECAD = .TRUE.
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
          CALL VC04AA(XMUL,SU,SV,U,V,XEL,YEL,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                NELEM,NELMAX,T(1,1),T(1,2),T(1,3),SPECAD)
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 PRISM
!
        ELSEIF(IELM1.EQ.41) THEN
!
          NELEM2 = BIEF_NBPTS(10,MESH)
          CALL VC04PP(XMUL,SU,SV,SW,U,V,W,F,H(1:NELEM2*2),
     &                XEL,YEL,ZPT,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                T(1,3),T(1,4),T(1,5),T(1,6),SPECAD,FORMUL,
!                     THIS IS TO RETRIEVE THE NUMBER OF TRIANGLES
     &                NELEM2)
!
!-----------------------------------------------------------------------
!
!       ELEMENT T1 TETRAHEDRON
!
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
          CALL VC04TT(XMUL,SU,SV,SW,U,V,W,F,H,XPT,YPT,ZPT,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                NELEM,NELMAX,T(1,1),T(1,2),
     &                T(1,3),T(1,4),FORMUL,SPECAD,
!                     THIS IS TO RETRIEVE THE NUMBER OF 2D POINTS
!                     AND TRIANGLES
     &                BIEF_NBPTS(11,MESH),BIEF_NBPTS(10,MESH))
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR U.N    (N VECTOR NORMAL TO THE ELEMENT)
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'FLUBOR          ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE FOR VERTICAL SIDES OF THE PRISMS SPLIT
!       IN TETRAHEDRONS
!
        IF(IELM1.EQ.61) THEN
!
          CALL VC05FT(XMUL,SU,SV,U,V,XPT,YPT,ZPT,
     &                IKLBOR(1,1),IKLBOR(1,2),IKLBOR(1,3),NBOR,
     &                NELEB,NELEBX,T(1,1),T(1,2),T(1,3))
!
!       ELEMENT P1 QUADRILATERAL FOR VERTICAL SIDES OF THE PRISMS
!
        ELSEIF(IELM1.EQ.71) THEN
!
          CALL VC05FF(XMUL,SU,SV,U,V,XEL,YEL,ZPT,
     &                IKLBOR(1,1),IKLBOR(1,2),IKLBOR(1,3),IKLBOR(1,4),
     &                NBOR,NELEB,NELEBX,T(1,1),T(1,2),T(1,3),T(1,4),
     &                NELBOR,NULONE,NELMAX)

! !
! !       ELEMENT P1 PRISM
! !
!         ELSEIF(IELM1.EQ.41) THEN
!
!           CALL VC05PP(XMUL,SF,F,ZPT,SURFAC,
!      &                IKLE(1,1),IKLE(1,2),IKLE(1,3),
!      &                IKLE(1,4),IKLE(1,5),IKLE(1,6),
!      &                NELEM,NELMAX,T(1,1),T(1,2),T(1,3),
!      &                T(1,4),T(1,5),T(1,6) )
!
!
!       ELEMENT P1 TRIANGLES
!
        ELSEIF(IELM1.EQ.11) THEN
!
          CALL VC05AA(XMUL,SF,F,SURFAC,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                NELEM,NELMAX,T(1,1),T(1,2),T(1,3) )
!
!       ELEMENT LINEAR SEGMENT
!
        ELSEIF(IELM1.EQ.1) THEN
!
          CALL VC05OO(XMUL,SU,SV,U,V,XNOR,YNOR,LGSEG,
     &                IKLBOR,NBOR,NELEB,NELEBX,T(1,1),T(1,2))
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR U GRAD(F)
!=======================================================================
!
      ELSEIF(FORMUL(1:13).EQ.'VGRADF       ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
          CALL VC08AA(XMUL,SF,SU,SV,F,U,V,XEL,YEL,IKLE,
     &                NELEM,NELMAX,T(1,1),T(1,2),T(1,3) , FORMUL )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P2 TRIANGLE
!
        ELSEIF(IELM1.EQ.13) THEN
!
          CALL VC08CC(XMUL,SF,SU,SV,F,U,V,XEL,YEL,
     &         IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &         IKLE(1,5),IKLE(1,6),NELEM,NELMAX,
     &         T(1,1),T(1,2),T(1,3),T(1,4),T(1,5),T(1,6),FORMUL)
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
        ELSEIF(IELM1.EQ.12) THEN
!
          CALL VC08BB(XMUL,SF,SU,SV,F,U,V,XEL,YEL,
     &         IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &         NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),FORMUL)
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!         CALL VC08OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    &                XEL,YEL,ZEL,SURFAC,
!    &                IKLE(1,1),IKLE(1,2),NELEM,NELMAX,
!    &                T(1,1),T(1,2)  )
!
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 PRISM
!
        ELSEIF(IELM1.EQ.41) THEN
!
          CALL VC08PP(XMUL,SF,SU,SV,SW,F,U,V,W,XEL,YEL,ZPT,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                T(1,3),T(1,4),T(1,5),T(1,6))
!
!-----------------------------------------------------------------------
!
!       ELEMENT T1 TETRAHEDRON
!
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
          CALL VC08TT(XMUL,SF,SU,SV,SW,F,U,V,W,XPT,YPT,ZPT,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                NELEM,NELMAX,T(1,1),T(1,2),
     &                T(1,3),T(1,4))
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR U GRAD(F) 2
!=======================================================================
!
      ELSEIF(FORMUL(1:13).EQ.'VGRADF2      ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 PRISM
!
        IF(IELM1.EQ.41) THEN
!
          CALL VC18PP(XMUL,SF,SU,SV,F,U,V,XEL,YEL,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                T(1,3))
!
!-----------------------------------------------------------------------
!
!       ELEMENT T1 TETRAHEDRON
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR Q GRAD(F)
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'QGRADF          ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
          CALL VC09AA(XMUL,SF,SG,SU,SV,F,G,U,V,XEL,YEL,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                T(1,1),T(1,2),T(1,3) )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!         CALL VC09OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    &                XEL,YEL,ZEL,SURFAC,
!    &                IKLE(1,1),IKLE(1,2),NELEM,NELMAX,
!    &                T(1,1),T(1,2)  )
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR F U.N    (N VECTOR NORMAL TO THE ELEMENT)
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'FLUBDF          ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
!       IF(IELM1.EQ.11) THEN
!
!         CALL VC10AA(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    &                XEL,YEL,ZEL,SURFAC,
!    &                IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
!    &                T(1,1)   ,T(1,2)   ,T(1,3))
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
        IF(IELM1.EQ.1) THEN
!
          CALL VC10OO(XMUL,SF,SU,SV,F,U,V,XNOR,YNOR,LGSEG,
     &                IKLBOR,NBOR,NELEB,NELEBX,T(1,1),T(1,2))
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR G GRADIENT(F)
!=======================================================================
!
      ELSEIF(FORMUL(1:15).EQ.'GGRADF         ') THEN
!
!     CHARACTER 16 GIVES THE SELECTED COORDINATE
!
      IF(FORMUL(16:16).EQ.'X') THEN
        ICOORD=1
      ELSEIF(FORMUL(16:16).EQ.'Y') THEN
        ICOORD=2
      ELSEIF(FORMUL(16:16).EQ.'Z') THEN
        ICOORD=3
      ENDIF
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
!         CHECKS IF G IS DISCONTINUOUS P1
!
          IF(SG%ELM.EQ.15) THEN
!
            CALL VC11AA2(XMUL,SF,SG,SH,F,G,H,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3) , ICOORD )
!
!         CLASSICAL CASE: G IS P1
!
          ELSE
!
            CALL VC11AA(XMUL,SF,SG,F,G,XEL,YEL,
     &                  IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                  T(1,1),T(1,2),T(1,3) , ICOORD )
!
          ENDIF
!
!-----------------------------------------------------------------------
!
        ELSEIF(IELM1.EQ.12) THEN
!
          CALL VC11BB(XMUL,SF,SG,F,G,XEL,YEL,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                NELEM,NELMAX,
     &                T(1,1),T(1,2),T(1,3),T(1,4) , ICOORD )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!         CALL VC11OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    &                XEL,YEL,ZEL,XNOR,YNOR,ZNOR,SURFAC,
!    &                IKLE(1,1),IKLE(1,2),NBOR,NELEM,NELMAX,
!    &                T(1,1),T(1,2)  )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 PRISM
!
        ELSEIF(IELM1.EQ.41) THEN
!
          CALL VC11PP(XMUL,SF,SG,F,G,
     &                XEL,YEL,ZPT,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                T(1,3),T(1,4),T(1,5),T(1,6),ICOORD)
!
!-----------------------------------------------------------------------
!
!       ELEMENT T1 TETRAHEDRON
!
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
          CALL VC11TT(XMUL,SF,SG,F,G,
     &                XPT,YPT,ZPT,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                NELEM,NELMAX,T(1,1),T(1,2),
     &                T(1,3),T(1,4),ICOORD)
!
        ELSEIF(IELM1.EQ.30) THEN
!
! COMMENT JP RENAUD 21/09/2005
! BEWARE: THIS CALL CREATES A P0 VECTOR. THEREFORE
! THERE IS NO NEED TO "ASSEMBLE" IT AFTERWARDS. NOTE
! THAT VEC ITSELF (AND NOT T) IS GIVEN TO VC11TT0 AND
! THAT LEGO IS ALSO SET TO .FALSE. TO AVOID
! CALLING ASSVEC AT THE END OF THE SUBROUTINE.
!
          CALL VC11TT0(XMUL,SF,SG,F,G,
     &                 XPT,YPT,ZPT,
     &                 IKLE(1:NELEM,1),IKLE(1:NELEM,2),
     &                 IKLE(1:NELEM,3),IKLE(1:NELEM,4),
     &                 NELEM,MESH%NPOIN,
     &                 VEC,ICOORD)
!
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR GRADIENT(F)
!=======================================================================
!
      ELSEIF(FORMUL(1:5).EQ.'GRADF') THEN
!
!     CHARACTER 16 GIVES THE SELECTED COORDINATE
!
      IF(FORMUL(16:16).EQ.'X') THEN
        ICOORD=1
      ELSEIF(FORMUL(16:16).EQ.'Y') THEN
        ICOORD=2
      ELSEIF(FORMUL(16:16).EQ.'Z') THEN
        ICOORD=3
      ENDIF
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
          CALL VC13AA(XMUL,SF,F,XEL,YEL,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,1),
     &                NELEM,NELMAX,T(1,1),T(1,2),T(1,3),ICOORD)
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
        ELSEIF(IELM1.EQ.12) THEN
!
          CALL VC13BB(XMUL,SF,F,XEL,YEL,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NELEM,
     &                NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),ICOORD)
!
!-----------------------------------------------------------------------
!
!       ELEMENT P2 TRIANGLE
!
        ELSEIF(IELM1.EQ.13) THEN
!
          CALL VC13CC(XMUL,SF,F,XEL,YEL,
     &                 IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                 IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &                 NELEM,NELMAX,
     &                 T(1,1),T(1,2),T(1,3),
     &                 T(1,4),T(1,5),T(1,6),ICOORD)
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!         CALL VC13OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    &                XEL,YEL,ZEL,XNOR,YNOR,ZNOR,SURFAC,
!    &                IKLE(1,1),IKLE(1,2),NBOR,NELEM,NELMAX,
!    &                T(1,1),T(1,2)  )
!
!
!
!-----------------------------------------------------------------------
!
!       ELEMENT P2 SEGMENT
!
!       ELSEIF(IELM1.EQ.2) THEN
!
!         CALL VC13OC(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    &                XEL,YEL,ZEL,XNOR,YNOR,ZNOR,SURFAC,
!    &                IKLE(1,1),IKLE(1,2),NBOR,NELEM,NELMAX,
!    &                T(1,1),T(1,2),T(1,3)  )
!
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 PRISM
!
        ELSEIF(IELM1.EQ.41) THEN
!
          IF(FORMUL(1:15).EQ.'GRADF(X,Y)     ') THEN
!         SIMPLIFIED FORMULATION FOR EFFICIENCY AND ACCURACY
          CALL VC13PP2(XMUL,SF,F,XEL,YEL,ZPT,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                T(1,3),T(1,4),T(1,5),T(1,6),ICOORD)
          ELSEIF( FORMUL(8:15).EQ.'        ') THEN
!                 FORMUL(6:7) IS LEFT FOR OPTIONS
          CALL VC13PP(XMUL,SF,F,XEL,YEL,ZPT,SURFAC,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                T(1,3),T(1,4),T(1,5),T(1,6),ICOORD,FORMUL)
!
          ELSE
            WRITE(LU,1001) FORMUL
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!
!       ELEMENT T1 TETRAHEDRON
!
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
          IF(FORMUL(1:5).EQ.'GRADF'.AND.
     &       FORMUL(8:15).EQ.'        ') THEN
!           FORMUL(6:7) IS LEFT FOR OPTIONS
            CALL VC13TT(XMUL,SF,F,XPT,YPT,ZPT,
     &                  IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                  NELEM,NELMAX,T(1,1),T(1,2),
     &                  T(1,3),T(1,4),ICOORD,FORMUL)
!
          ELSE
            WRITE(LU,1001) FORMUL
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     TURBULENT PRODUCTION VECTOR
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'PRODF           ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
          CALL VC14AA(XMUL,SU,SV,U,V,XEL,YEL,SURFAC,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                T(1,1),T(1,2),T(1,3))
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
!       ELSEIF(IELM1.EQ.12) THEN
!
!         CALL VC14BB(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    &                XEL,YEL,ZEL,SURFAC,
!    &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NELEM,
!    &                NELMAX,T(1,1),T(1,2),T(1,3),T(1,4))
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!         CALL VC14OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    &                XEL,YEL,ZEL,XNOR,YNOR,ZNOR,SURFAC,
!    &                IKLE(1,1),IKLE(1,2),NBOR,NELEM,NELMAX,
!    &                T(1,1),T(1,2))
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     SPALART ALLMARAS PRODUCTION VECTOR
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'PRSAF           ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
          CALL VC17AA(XMUL,SU,SV,U,V,XEL,YEL,SURFAC,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                T(1,1),T(1,2),T(1,3))
!
!
!       ELEMENT P1 PRISM
!
        ELSEIF(IELM1.EQ.41) THEN
          CALL VC17PP(XMUL,SURFAC,SU,SV,SW,U,V,W,XEL,YEL,ZPT,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                T(1,3),T(1,4),T(1,5),T(1,6))

!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     SPALART ALLMARAS DESTRUCTION VECTOR
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'TRSAF           ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
          CALL VC21AA(XMUL,SU,SV,U,V,XEL,YEL,SURFAC,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                T(1,1),T(1,2),T(1,3))
        ELSEIF(IELM1.EQ.41) THEN
          CALL VC21PP(XMUL,SF,F,XEL,YEL,ZPT,SURFAC,IKLE(1,1),IKLE(1,2),
     &                IKLE(1,3),IKLE(1,4),IKLE(1,5),IKLE(1,6),NELEM,
     &                NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),T(1,5),T(1,6))

!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF

      ELSEIF(FORMUL(1:16).EQ.'STRAIN          ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
          CALL VC20AA(XMUL,SU,SV,U,V,XEL,YEL,SURFAC,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                T(1,1),T(1,2),T(1,3))
        ELSEIF(IELM1.EQ.41) THEN
          CALL VC20PP(XMUL,SURFAC,SU,SV,SW,U,V,W,XEL,YEL,ZPT,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                T(1,3),T(1,4),T(1,5),T(1,6))

!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR DIV(HU)
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'DIVQ            ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
          CALL VC15AA(XMUL,SF,SU,SV,F,U,V,XEL,YEL,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                NELEM,NELMAX,T(1,1),T(1,2),T(1,3))
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
!       ELSEIF(IELM1.EQ.12) THEN
!
!         CALL VC15BB(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    &         XEL,YEL,ZEL,SURFAC,
!    &         IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
!    &         NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),FORMUL)
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!         CALL VC15OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    &                XEL,YEL,ZEL,SURFAC,
!    &                IKLE(1,1),IKLE(1,2),NELEM,NELMAX,
!    &                T(1,1),T(1,2)  )
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR K.GRAD(PSI) DIV(U)
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'SUPGDIVU        ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
          CALL VC16AA(XMUL,SF,SG,SU,SV,F,G,U,V,XEL,YEL,SURFAC,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                NELEM,NELMAX,
     &                T(1,1),T(1,2),T(1,3) )
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
!       ELSEIF(IELM1.EQ.12) THEN
!
!         CALL VC16BB(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    &         XEL,YEL,ZEL,SURFAC,
!    &         IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
!    &         NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),FORMUL)
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!         CALL VC16OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    &                XEL,YEL,ZEL,SURFAC,
!    &                IKLE(1,1),IKLE(1,2),NELEM,NELMAX,
!    &                T(1,1),T(1,2)  )
!
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!
!=======================================================================
!     VECTOR H U.GRAD(PSI)
!=======================================================================
!
      ELSEIF(FORMUL(1:7).EQ.'HUGRADP') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
          CALL VC19AA(XMUL,SF,SG,SH,SU,SV,F,G,H,U,V,
     &                XEL,YEL,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                T(1,1),T(1,2),T(1,3),FORMUL)
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          WRITE(LU,1001) FORMUL
          WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     OTHER VECTOR
!=======================================================================
!
!
!=======================================================================
!     ERROR: TYPE OF VECTOR NOT CODED UP
!=======================================================================
!
      ELSE
        WRITE(LU,1001) FORMUL
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  POSSIBLE ASSEMBLY OF THE VECTOR
!
      IF(LEGO) THEN
!
        IF(OP(1:1).EQ.'=') THEN
          INIT = .TRUE.
        ELSEIF(OP(1:1).EQ.'+') THEN
          INIT = .FALSE.
        ELSE
          WRITE(LU,3001) OP
3001      FORMAT(1X,'VECTOS (BIEF) : OP NOT RECOGNISED:',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        NDP=BIEF_NBPEL(IELM1,MESH)
!
        IF(DIMENS(IELM1).EQ.MESH%DIM1) THEN
          IF(MODASS.EQ.1) THEN
            CALL ASSVEC(VEC, IKLE, NPT ,NELEM,NELMAX,
     &                  T,INIT,LV,MSK,MASKEL,NDP)
!
          ELSEIF(MODASS.EQ.3 ) THEN
            CALL ASSVEC(VEC, IKLE, NPT ,NELEM,NELMAX,
     &                  T,INIT,LV,MSK,MASKEL,NDP,SVEC%E)
!
          ELSEIF(MODASS.EQ.2) THEN
            CALL DOUBLE_TO_INTEGER(T,MESH%WI8,NDP*NELEM,QT,MESH%MXELVS)
            DO I=1,NPT
              MESH%TI8(I)=0
            ENDDO
            DO IDP = 1 , NDP
              PWI8=>MESH%WI8(1+(IDP-1)*NELMAX:IDP*NELMAX)
              CALL ASSVE1I8(MESH%TI8,IKLE(1,IDP),PWI8,NELEM)
            ENDDO
            IF(ASSPAR) THEN
              CALL PARCOM2I8(MESH%TI8,MESH%TI8,MESH%TI8,
     &                       NPT,NPLAN,2,1,MESH)
            ENDIF
            IF(INIT) THEN
              CALL INTEGER_TO_DOUBLE('=',MESH%TI8,VEC,NPT,QT)
            ELSE
              CALL INTEGER_TO_DOUBLE('+',MESH%TI8,VEC,NPT,QT)
            ENDIF
          ELSE
            WRITE(LU,*) 'VECTOS: MODASS=',MODASS,' UNEXPECTED CASE'
            CALL PLANTE(1)
            STOP
          ENDIF
        ELSEIF(NELEB.GT.0) THEN
          CALL ASSVEC(VEC, IKLBOR, NPT ,NELEB,NELEBX,
     &                T,INIT,LV,MSK,MASKEL,NDP)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!     OPTIONAL ASSEMBLY IN PARALLEL (ASSPAR DONE IN VECTOR)
!-----------------------------------------------------------------------
!
      IF(ASSPAR.AND.MODASS.EQ.1) CALL PARCOM(SVEC,2,MESH)
      IF(NCSIZE.GT.1) THEN
        IF(ASSPAR .AND. MODASS.EQ.3) THEN
          CALL PARCOM_COMP(SVEC,SVEC%E,2,MESH)
        ENDIF
      ENDIF
!
!     CompSum:Correct the VEC with accumuletad fperrors
!
      IF(ASSPAR.AND. MODASS .EQ. 3) THEN
        DO I = 1 , MESH%NPOIN
          VEC(I)= VEC(I)+SVEC%E(I)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
1000  FORMAT(1X,'VECTOS (BIEF) : VECTEUR NON PREVU : ',A16)
1001  FORMAT(1X,'VECTOS (BIEF) : VECTOR NOT IMPLEMENTED:',A16)
2000  FORMAT(1X,'                POUR IELM1 = ',1I6)
2001  FORMAT(1X,'                FOR IELM1 = ',1I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
