!                   *****************
                    SUBROUTINE MT14PP
!                   *****************
!
     &( T,XM,PPQ,LEGO,XMUL,SW,W,H,
     &  SURFAC,IKLE,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P3                                  21/08/2010
!***********************************************************************
!
!brief    BUILDS COEFFICIENTS LAMBDA(I,J) FOR N-TYPE MURD SCHEME.
!+
!+            THE ELEMENT IS THE P1 PRISM.
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!reference  J-M HERVOUET THESIS OR BOOK: MURD SCHEME IN 3 DIMENSIONS
!+             CHAPTER 6.
!
!history  J-M JANIN (LNH)
!+        28/11/1994
!+
!+
!
!history  JMH
!+        16/08/1999
!+
!+   EPS ADDED FOR THE DIVISION BY 0 TESTS
!
!history  JMH
!+        21/10/2004
!+
!+   MASS-LUMPING FOR COMPATIBILITY WITH NEW VERSION OF TRIDW2,
!
!history  JMH
!+        04/08/2008
!+
!+   INVERTED DIMENSIONS OF XM (SEE ALSO MURD3D)
!
!history  J-M HERVOUET / A. DECOENE (LNHE)
!+        27/04/2010
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        11/01/2013
!+        V6P3
!+   Arguments X,Y,Z removed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| H              |-->| FUNCTION USED IN THE FORMULA
!| IKLE           |-->| CONNECTIVITY TABLE
!| LEGO           |-->| LOGICAL. IF YES RESULT ASSEMBLED.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| PPQ            |-->| STORAGE IN XM OF OFF-DIAGONAL ELEMENTS
!| SW             |-->| BIEF_OBJ STRUCTURE OF W
!| SURFAC         |-->| AREA OF 2D ELEMENTS
!| T              |<->| WORK ARRAY FOR ELEMENT BY ELEMENT DIAGONAL
!| W              |-->| FUNCTION USED IN THE FORMULA
!| XM             |<->| OFF-DIAGONAL TERMS
!| XMUL           |-->| COEFFICIENT FOR MULTIPLICATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT14PP => MT14PP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6),PPQ(6,6)
!
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6),XM(30,NELMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: W(*)
      DOUBLE PRECISION, INTENT(IN) :: H(NELMAX,6)
!
      LOGICAL, INTENT(IN) :: LEGO
!
!     STRUCTURES DE U,V,W
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SW
!
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELEM,IELMW
      DOUBLE PRECISION XSUR3
      DOUBLE PRECISION LI0J0,LI0K0,LJ0I0,LJ0K0,LK0I0,LK0J0
      DOUBLE PRECISION LI3J3,LI3K3,LJ3I3,LJ3K3,LK3I3,LK3J3
      DOUBLE PRECISION LI3J0,LI3K0,LJ3I0,LJ3K0,LK3I0,LK3J0
      DOUBLE PRECISION LI3I0,LJ3J0,LK3K0,LI0I3
      DOUBLE PRECISION ALFA,ALFAJ,ALFAK,ALFA1,ALFA2,BETA,SOM0,SOM3
      DOUBLE PRECISION ALFAI0,ALFAJ0,ALFAK0,ALFAI3,ALFAJ3,ALFAK3
      DOUBLE PRECISION ALFAII,ALFAIJ,ALFAIK,ALFAJI,ALFAJJ,ALFAJK
      DOUBLE PRECISION ALFAKI,ALFAKJ,ALFAKK
      DOUBLE PRECISION, PARAMETER :: EPS = 1.D-10
!
      INTEGER :: IPLUS1(6),IPLUS3(6),INDIC(0:7)
      INTEGER IXM,I0,J0,K0,I3,J3,K3
!
!-----------------------------------------------------------------------
!
!     DONNEES
!
      PARAMETER ( IPLUS1 = (/ 2 , 3 , 1 , 5 , 6 , 4 /) )
      PARAMETER ( IPLUS3 = (/ 4 , 5 , 6 , 1 , 2 , 3 /) )
      PARAMETER ( INDIC  = (/ 4 , 4 , 5 , 3 , 6 , 2 , 1 , 1 /) )
!
!=======================================================================
!
      IELMW = SW%ELM
!                                                         *   *
!     COMPUTES THE COEFFICIENTS A(I) (INTEGRAL OF H U.GRAD(PSI), ONLY
!     CONSIDERING THE HORIZONTAL PART OF U). SEE JMH THESIS
!
!     NOTE: THIS VC04PP CALL IS ALREADY DONE BY FLUX3D IN TELEMAC-3D
!           THROUGH A CALL TO VECTOR. THE EQUIVALENT OF T HAS BEEN
!           SAVED IN WHAT IS HERE H
!
!     FORMUL='             HOR'
!     CALL VC04PP(XMUL,SU,SV,SW,U,V,W,SF,SG,SH,F,G,H,X,Y,Z,
!    * IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),IKLE(1,5),IKLE(1,6),
!    * NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),T(1,5),T(1,6),
!    * SPECAD,FORMUL,NPLAN)
!
!     NOW T IS RETRIEVED BY STORAGE DONE INTO FLUX3D
!     FUNCTION H MUST CONTAIN THE EQUIVALENT OF T ABOVE, WHICH IS
!     THE NON ASSEMBLED VALUE OF FLUINT
!
!     DO IELEM=1,NELEM
!       T(IELEM,1)=H(         IELEM)
!       T(IELEM,2)=H(  NELMAX+IELEM)
!       T(IELEM,3)=H(2*NELMAX+IELEM)
!       T(IELEM,4)=H(3*NELMAX+IELEM)
!       T(IELEM,5)=H(4*NELMAX+IELEM)
!       T(IELEM,6)=H(5*NELMAX+IELEM)
!     ENDDO
!
!     FORMUL NORMALLY STARTS WITH VGRADP OR VGRADP2, OR VGRADP22 TO KNOW
!     SIGMAG AND SPECAD, USELESS HERE.
      XSUR3 = XMUL/3.D0
!
!-----------------------------------------------------------------------
!
      IF(IELMW.NE.41) THEN
        WRITE(LU,*) 'MT14PP DISCRETISATION NOT HANDLED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     COMPUTES THE COEFFICIENTS B(I) BY ELEMENTS
!     AND COMPUTES LAMBDA(I,J)
!
      DO IELEM = 1 , NELEM
!
        IXM = 0
        IF (W(IKLE(IELEM,1)).GT.0.D0) IXM = 1
        IF (W(IKLE(IELEM,2)).GT.0.D0) IXM = IXM + 2
        IF (W(IKLE(IELEM,3)).GT.0.D0) IXM = IXM + 4
!
!       NOTE JMH:
!       IT SEEMS THAT INDIC IS THE POINT THAT RECEIVES THE VERTICAL
!       VELOCITY WHICH IS THE ONLY ONE OF ITS SIGN
!
!       ALL W < 0         INDIC = 4 (RANDOM, WHY NOT 1 ?)
!       ONLY W1 > 0       INDIC = 4
!       ONLY W2 > 0       INCIC = 5
!       ONLY W3 > 0       INDIC = 6
!       ALL W > 0         INDIC = 1 (RANDOM, WHY NOT 4 ?)
!       ONLY W1 < 0       INDIC = 1
!       ONLY W2 < 0       INDIC = 2
!       ONLY W3 < 0       INDIC = 3
!
        I0 = INDIC(IXM)
        J0 = IPLUS1(I0)
        K0 = IPLUS1(J0)
        I3 = IPLUS3(I0)
        J3 = IPLUS1(I3)
        K3 = IPLUS1(J3)
!
        ALFA = XSUR3 * SURFAC(IELEM)
!
        LI3I0 = ALFA * ABS(W(IKLE(IELEM,MIN(I0,I3))))
        LI0I3 = 0.D0
        IF (IXM.GE.1.AND.IXM.LE.6) THEN
          LI0I3 = LI3I0
          LI3I0 = 0.D0
        ENDIF
        LJ3J0 = ALFA * ABS(W(IKLE(IELEM,MIN(J0,J3))))
        XM(PPQ(J0,J3),IELEM) = 0.D0
        LK3K0 = ALFA * ABS(W(IKLE(IELEM,MIN(K0,K3))))
        XM(PPQ(K0,K3),IELEM) = 0.D0
!
        ALFAI0 = -H(IELEM,I0)
        ALFAJ0 = -H(IELEM,J0)
        ALFAK0 = -H(IELEM,K0)
        ALFAI3 = -H(IELEM,I3)
        ALFAJ3 = -H(IELEM,J3)
        ALFAK3 = -H(IELEM,K3)
!
        LJ0I0 = MAX(0.D0,MIN(ALFAI0,-ALFAJ0))
        LK0I0 = MAX(0.D0,MIN(ALFAI0,-ALFAK0))
        LI3J3 = MAX(0.D0,MIN(ALFAJ3,-ALFAI3))
        LI3K3 = MAX(0.D0,MIN(ALFAK3,-ALFAI3))
!
        ALFAJ = MIN(LJ3J0,MAX(LI3J3,LJ0I0))
        ALFAK = MIN(LK3K0,MAX(LI3K3,LK0I0))
        ALFA  = MIN(LI0I3,ALFAJ+ALFAK)
        IF (ALFA.GT.EPS) THEN
!
          BETA = ALFA / (ALFAJ+ALFAK)
          ALFAJ = ALFAJ * BETA
          ALFAK = ALFAK * BETA
!
          LI0I3 = LI0I3-ALFA
          LJ3J0 = LJ3J0-ALFAJ
          LK3K0 = LK3K0-ALFAK
!
          ALFAI3 = ALFAI3+ALFA
          ALFAI0 = ALFAI0-ALFA
          ALFAJ0 = ALFAJ0+ALFAJ
          ALFAJ3 = ALFAJ3-ALFAJ
          ALFAK0 = ALFAK0+ALFAK
          ALFAK3 = ALFAK3-ALFAK
!
          LJ0I0 = MAX(0.D0,MIN(ALFAI0,-ALFAJ0))
          LK0I0 = MAX(0.D0,MIN(ALFAI0,-ALFAK0))
          LI3J3 = MAX(0.D0,MIN(ALFAJ3,-ALFAI3))
          LI3K3 = MAX(0.D0,MIN(ALFAK3,-ALFAI3))
!
        ENDIF
!
        LI0J0 = MAX(0.D0,MIN(ALFAJ0,-ALFAI0))
        LK0J0 = MAX(0.D0,MIN(ALFAJ0,-ALFAK0))
        LI0K0 = MAX(0.D0,MIN(ALFAK0,-ALFAI0))
        LJ0K0 = MAX(0.D0,MIN(ALFAK0,-ALFAJ0))
        LJ3I3 = MAX(0.D0,MIN(ALFAI3,-ALFAJ3))
        LJ3K3 = MAX(0.D0,MIN(ALFAK3,-ALFAJ3))
        LK3I3 = MAX(0.D0,MIN(ALFAI3,-ALFAK3))
        LK3J3 = MAX(0.D0,MIN(ALFAJ3,-ALFAK3))
!
        XM(PPQ(J0,I3),IELEM) = 0.D0
        XM(PPQ(K0,I3),IELEM) = 0.D0
        XM(PPQ(I0,J3),IELEM) = 0.D0
        XM(PPQ(K0,J3),IELEM) = 0.D0
        XM(PPQ(I0,K3),IELEM) = 0.D0
        XM(PPQ(J0,K3),IELEM) = 0.D0
!
        ALFAI0 = 0.D0
        ALFAJ0 = 0.D0
        ALFAK0 = 0.D0
        ALFAI3 = 0.D0
        ALFAJ3 = 0.D0
        ALFAK3 = 0.D0
!
        SOM0 = LJ0I0+LK0I0
        SOM3 = LI3J3+LI3K3
!
        ALFA1 = MIN(LI0I3,SOM0)
        IF (ALFA1.GT.EPS) THEN
!
          BETA = 1.D0 / SOM0
          ALFAJ0 = LJ0I0 * BETA
          ALFAK0 = LK0I0 * BETA
          ALFA = MAX(0.D0,ALFA1-SOM3)
!
          LJ0I0 = LJ0I0 - ALFAJ0*ALFA1
          LK0I0 = LK0I0 - ALFAK0*ALFA1
          XM(PPQ(J0,I3),IELEM) = ALFAJ0*ALFA
          XM(PPQ(K0,I3),IELEM) = ALFAK0*ALFA
!
        ENDIF
!
        ALFA2 = MIN(LI0I3,SOM3)
        IF (ALFA2.GT.EPS) THEN
!
          BETA = 1.D0 / SOM3
          ALFAJ3 = LI3J3 * BETA
          ALFAK3 = LI3K3 * BETA
          ALFA = MAX(0.D0,ALFA2-SOM0)
!
          LI3J3 = LI3J3 - ALFAJ3*ALFA2
          LI3K3 = LI3K3 - ALFAK3*ALFA2
          XM(PPQ(I0,J3),IELEM) = ALFAJ3*ALFA
          XM(PPQ(I0,K3),IELEM) = ALFAK3*ALFA
!
        ENDIF
!
        ALFA = MIN(ALFA1,ALFA2)
        IF (ALFA.GT.0.D0) THEN
!
          ALFAJ3 = ALFAJ3 * ALFA
          ALFAK3 = ALFAK3 * ALFA
          ALFAJJ = ALFAJ3 * ALFAJ0
          ALFAKK = ALFAK3 * ALFAK0
          ALFAJK = ALFAJ3 * ALFAK0
          ALFAKJ = ALFAK3 * ALFAJ0
          ALFA = MIN(ALFAJK,ALFAKJ)
!
          XM(PPQ(J0,J3),IELEM) = ALFAJJ + ALFA
          XM(PPQ(K0,K3),IELEM) = ALFAKK + ALFA
          XM(PPQ(K0,J3),IELEM) = ALFAJK - ALFA
          XM(PPQ(J0,K3),IELEM) = ALFAKJ - ALFA
!
        ENDIF
!
        XM(PPQ(I0,I3),IELEM) = MAX(0.D0,LI0I3-MAX(SOM3,SOM0))
!
        LJ3I0 = 0.D0
        LK3I0 = 0.D0
        LI3J0 = 0.D0
        LK3J0 = 0.D0
        LI3K0 = 0.D0
        LJ3K0 = 0.D0
!
        SOM0 = LI0J0+LI0K0
        SOM3 = LJ3I3+LK3I3
!
        ALFA1 = MIN(LI3I0,SOM0)
        IF (ALFA1.GT.EPS) THEN
!
          BETA = 1.D0 / SOM0
          ALFAJ0 = LI0J0 * BETA
          ALFAK0 = LI0K0 * BETA
          ALFA = MAX(0.D0,ALFA1-SOM3)
!
          LI0J0 = LI0J0 - ALFAJ0*ALFA1
          LI0K0 = LI0K0 - ALFAK0*ALFA1
          LI3J0 = LI3J0 + ALFAJ0*ALFA
          LI3K0 = LI3K0 + ALFAK0*ALFA
!
        ENDIF
!
        XM(PPQ(I0,J0),IELEM) = LI0J0
        XM(PPQ(I0,K0),IELEM) = LI0K0
!
        ALFA2 = MIN(LI3I0,SOM3)
        IF (ALFA2.GT.EPS) THEN
!
          BETA = 1.D0 / SOM3
          ALFAJ3 = LJ3I3 * BETA
          ALFAK3 = LK3I3 * BETA
          ALFA = MAX(0.D0,ALFA2-SOM0)
!
          LJ3I3 = LJ3I3 - ALFAJ3*ALFA2
          LK3I3 = LK3I3 - ALFAK3*ALFA2
          LJ3I0 = LJ3I0 + ALFAJ3*ALFA
          LK3I0 = LK3I0 + ALFAK3*ALFA
!
        ENDIF
!
        XM(PPQ(J3,I3),IELEM) = LJ3I3
        XM(PPQ(K3,I3),IELEM) = LK3I3
!
        ALFA = MIN(ALFA1,ALFA2)
        IF (ALFA.GT.0.D0) THEN
!
          ALFAJ0 = ALFAJ0 * ALFA
          ALFAK0 = ALFAK0 * ALFA
          ALFAJJ = ALFAJ0 * ALFAJ3
          ALFAKK = ALFAK0 * ALFAK3
          ALFAJK = ALFAJ0 * ALFAK3
          ALFAKJ = ALFAK0 * ALFAJ3
          ALFA = MIN(ALFAJK,ALFAKJ)
!
          LJ3J0 = LJ3J0 + ALFAJJ + ALFA
          LK3K0 = LK3K0 + ALFAKK + ALFA
          LK3J0 = LK3J0 + ALFAJK - ALFA
          LJ3K0 = LJ3K0 + ALFAKJ - ALFA
!
        ENDIF
!
        LI3I0 = MAX(0.D0,LI3I0-MAX(SOM0,SOM3))
!
        SOM0 = LJ0I0+LJ0K0
        SOM3 = LI3J3+LK3J3
!
        ALFA1 = MIN(LJ3J0,SOM0)
        IF (ALFA1.GT.EPS) THEN
!
          BETA = 1.D0 / SOM0
          ALFAI0 = LJ0I0 * BETA
          ALFAK0 = LJ0K0 * BETA
          ALFA = MAX(0.D0,ALFA1-SOM3)
!
          LJ0I0 = LJ0I0 - ALFAI0*ALFA1
          LJ0K0 = LJ0K0 - ALFAK0*ALFA1
          LJ3I0 = LJ3I0 + ALFAI0*ALFA
          LJ3K0 = LJ3K0 + ALFAK0*ALFA
!
        ENDIF
!
        XM(PPQ(J0,I0),IELEM) = LJ0I0
        XM(PPQ(J0,K0),IELEM) = LJ0K0
!
        ALFA2 = MIN(LJ3J0,SOM3)
        IF (ALFA2.GT.EPS) THEN
!
          BETA = 1.D0 / SOM3
          ALFAI3 = LI3J3 * BETA
          ALFAK3 = LK3J3 * BETA
          ALFA = MAX(0.D0,ALFA2-SOM0)
!
          LI3J3 = LI3J3 - ALFAI3*ALFA2
          LK3J3 = LK3J3 - ALFAK3*ALFA2
          LI3J0 = LI3J0 + ALFAI3*ALFA
          LK3J0 = LK3J0 + ALFAK3*ALFA
!
        ENDIF
!
        XM(PPQ(I3,J3),IELEM) = LI3J3
        XM(PPQ(K3,J3),IELEM) = LK3J3
!
        ALFA = MIN(ALFA1,ALFA2)
        IF (ALFA.GT.0.D0) THEN
!
          ALFAI0 = ALFAI0 * ALFA
          ALFAK0 = ALFAK0 * ALFA
          ALFAII = ALFAI0 * ALFAI3
          ALFAKK = ALFAK0 * ALFAK3
          ALFAIK = ALFAI0 * ALFAK3
          ALFAKI = ALFAK0 * ALFAI3
          ALFA = MIN(ALFAIK,ALFAKI)
!
          LI3I0 = LI3I0 + ALFAII + ALFA
          LK3K0 = LK3K0 + ALFAKK + ALFA
          LK3I0 = LK3I0 + ALFAIK - ALFA
          LI3K0 = LI3K0 + ALFAKI - ALFA
!
        ENDIF
!
        LJ3J0 = MAX(0.D0,LJ3J0-MAX(SOM0,SOM3))
!
        SOM0 = LK0I0+LK0J0
        SOM3 = LI3K3+LJ3K3
!
        ALFA1 = MIN(LK3K0,SOM0)
        IF (ALFA1.GT.EPS) THEN
!
          BETA = 1.D0 / SOM0
          ALFAI0 = LK0I0 * BETA
          ALFAJ0 = LK0J0 * BETA
          ALFA = MAX(0.D0,ALFA1-SOM3)
!
          LK0I0 = LK0I0 - ALFAI0*ALFA1
          LK0J0 = LK0J0 - ALFAJ0*ALFA1
          LK3I0 = LK3I0 + ALFAI0*ALFA
          LK3J0 = LK3J0 + ALFAJ0*ALFA
!
        ENDIF
!
        XM(PPQ(K0,I0),IELEM) = LK0I0
        XM(PPQ(K0,J0),IELEM) = LK0J0
!
        ALFA2 = MIN(LK3K0,SOM3)
        IF (ALFA2.GT.EPS) THEN
!
          BETA = 1.D0 / SOM3
          ALFAI3 = LI3K3 * BETA
          ALFAJ3 = LJ3K3 * BETA
          ALFA = MAX(0.D0,ALFA2-SOM0)
!
          LI3K3 = LI3K3 - ALFAI3*ALFA2
          LJ3K3 = LJ3K3 - ALFAJ3*ALFA2
          LI3K0 = LI3K0 + ALFAI3*ALFA
          LJ3K0 = LJ3K0 + ALFAJ3*ALFA
!
        ENDIF
!
        XM(PPQ(I3,K3),IELEM) = LI3K3
        XM(PPQ(J3,K3),IELEM) = LJ3K3
!
        ALFA = MIN(ALFA1,ALFA2)
        IF (ALFA.GT.0.D0) THEN
!
          ALFAI0 = ALFAI0 * ALFA
          ALFAJ0 = ALFAJ0 * ALFA
          ALFAII = ALFAI0 * ALFAI3
          ALFAJJ = ALFAJ0 * ALFAJ3
          ALFAIJ = ALFAI0 * ALFAJ3
          ALFAJI = ALFAJ0 * ALFAI3
          ALFA = MIN(ALFAIJ,ALFAJI)
!
          LI3I0 = LI3I0 + ALFAII + ALFA
          LJ3J0 = LJ3J0 + ALFAJJ + ALFA
          LJ3I0 = LJ3I0 + ALFAIJ - ALFA
          LI3J0 = LI3J0 + ALFAJI - ALFA
!
        ENDIF
!
        LK3K0 = MAX(0.D0,LK3K0-MAX(SOM0,SOM3))
!
        XM(PPQ(I3,I0),IELEM) = LI3I0
        XM(PPQ(J3,I0),IELEM) = LJ3I0
        XM(PPQ(K3,I0),IELEM) = LK3I0
        XM(PPQ(I3,J0),IELEM) = LI3J0
        XM(PPQ(J3,J0),IELEM) = LJ3J0
        XM(PPQ(K3,J0),IELEM) = LK3J0
        XM(PPQ(I3,K0),IELEM) = LI3K0
        XM(PPQ(J3,K0),IELEM) = LJ3K0
        XM(PPQ(K3,K0),IELEM) = LK3K0
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE SUM OF EACH ROW (WITH A - SIGN)
!     THE DIAGONAL TERMS ARE 0
!
      IF(LEGO) THEN
!
        DO IELEM = 1,NELEM
!
          T(IELEM,1) = -XM(01,IELEM)-XM(02,IELEM)
     &                 -XM(03,IELEM)-XM(04,IELEM)-XM(05,IELEM)
          T(IELEM,2) = -XM(16,IELEM)-XM(06,IELEM)
     &                 -XM(07,IELEM)-XM(08,IELEM)-XM(09,IELEM)
          T(IELEM,3) = -XM(17,IELEM)-XM(21,IELEM)
     &                 -XM(10,IELEM)-XM(11,IELEM)-XM(12,IELEM)
          T(IELEM,4) = -XM(18,IELEM)-XM(22,IELEM)
     &                 -XM(25,IELEM)-XM(13,IELEM)-XM(14,IELEM)
          T(IELEM,5) = -XM(19,IELEM)-XM(23,IELEM)
     &                 -XM(26,IELEM)-XM(28,IELEM)-XM(15,IELEM)
          T(IELEM,6) = -XM(20,IELEM)-XM(24,IELEM)
     &                 -XM(27,IELEM)-XM(29,IELEM)-XM(30,IELEM)
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
