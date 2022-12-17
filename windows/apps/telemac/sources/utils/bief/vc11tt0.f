!                   ******************
                    SUBROUTINE VC11TT0
!                   ******************
!
     &( XMUL,SF,SG,F,G,X,Y,Z,IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NPOIN,
     &  W,ICOORD)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+   (EXAMPLE OF THE X COMPONENT, WHICH CORRESPONDS TO ICOORD=1)
!+
!+                       /            DF
!+    VEC(I)  =  XMUL   /  ( G  P  *( --  )) D(OMEGA)
!+                     /OMEGA    I    DX
!+
!+
!+    P   IS A LINEAR BASE
!+     I
!+
!+    F IS A VECTOR OF TYPE P1
!+    W IS A VECTOR OF TYPE P0
!+    G IS ALSO A VECTOR OF TYPE P0
!+    IT IS THEREFORE NOT REQUIRED TO ASSEMBLE THE RESULT VECTOR
!+    THE RESULT IS DIRECTLY IN W
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)
!+        **/06/04
!+        V5P5
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| G              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
!| IKLE1          |-->| FIRST POINT OF TETRAHEDRA
!| IKLE2          |-->| SECOND POINT OF TETRAHEDRA
!| IKLE3          |-->| THIRD POINT OF TETRAHEDRA
!| IKLE4          |-->| FOURTH POINT OF TETRAHEDRA
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SG             |-->| BIEF_OBJ STRUCTURE OF G
!| SURFAC         |-->| AREA OF TRIANGLES
!| W              |<--| RESULT IN NON ASSEMBLED FORM
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM          ! NUMBER OF ELEMENTS
      INTEGER, INTENT(IN) :: NPOIN          ! NUMBER OF POINTS
      INTEGER, INTENT(IN) :: ICOORD         ! DIRECTION OF GRAD :
                                            ! 1 - X
                                            ! 2 - Y
                                            ! 3 - Z
!
      INTEGER, INTENT(IN) :: IKLE1(NELEM)  ! NODE NUMBER 1 OF ELEMENTS
      INTEGER, INTENT(IN) :: IKLE2(NELEM)  ! NODE NUMBER 2 OF ELEMENTS
      INTEGER, INTENT(IN) :: IKLE3(NELEM)  ! NODE NUMBER 3 OF ELEMENTS
      INTEGER, INTENT(IN) :: IKLE4(NELEM)  ! NODE NUMBER 4 OF ELEMENTS
!
      DOUBLE PRECISION, DIMENSION(NPOIN), TARGET, INTENT(IN) :: X,Y,Z
      DOUBLE PRECISION, INTENT(IN)         :: XMUL        ! CONSTANT FACTOR
      DOUBLE PRECISION, INTENT(OUT)        :: W(NELEM)   ! RESULT
!
!     STRUCTURES OF F, G, H, U, V, W AND REAL DATA
!
      TYPE(BIEF_OBJ)  , INTENT(IN)   :: SF,SG !
      DOUBLE PRECISION, INTENT(IN)   :: G(*)  ! VECTOR TO MULTIPLY BY
      DOUBLE PRECISION, INTENT(IN)   :: F(*)  ! VECTOR WE COMPUTE THE
                                              ! GRAD OF
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! LOCAL VARIABLES
!
      INTEGER          :: IELEM,IELMF,IELMG ! ELEMENT TYPES
      DOUBLE PRECISION :: F1,F2,F3,F4       ! THE 4 VALUES OF F AT THE
                                            ! NODES OF AN ELEMENT
      DOUBLE PRECISION :: X2,X3,X4,Y2,Y3,Y4 ! DELTA_X, DELTA_Y
      DOUBLE PRECISION :: X1, Y1            ! COORD OF THE FIRST NODE
      INTEGER          :: I1,I2,I3,I4       ! THE NUMBERS OF THE NODES
                                            ! OF AN ELEMENT
!
      DOUBLE PRECISION, DIMENSION(:), POINTER :: PX, PY ! POINTER TO
                                                        ! THE COORD
      DOUBLE PRECISION          :: XSUR24
      DOUBLE PRECISION          :: F2MF1,F3MF1,F4MF1
      DOUBLE PRECISION          :: DET               ! JACOBIAN
!
!-----------------------------------------------------------------------
! INITIALISES
!
      XSUR24 = XMUL/24.D0
!
      IELMF = SF%ELM
      IELMG = SG%ELM
!
!-----------------------------------------------------------------------
! TEST ON THE COMPONENT TO DIFFERENTIATE :
! 1 FOR X, 2 FOR Y, 3 FOR Z. OTHER VALUES ARE NOT ALLOWED.
! THE POINTER POINTS TO THE ARRAYS OF THE COORDINATES THAT
! WILL BE USED.
      SELECT CASE (ICOORD )
        CASE ( 1 )
          PX => Y
          PY => Z
        CASE ( 2 )
          PX => Z
          PY => X
        CASE ( 3 )
          PX => X
          PY => Y
        CASE DEFAULT
          WRITE(LU,203) ICOORD
 203      FORMAT(1X,'VC11TT0 (BIEF) : IMPOSSIBLE COMPONENT ',
     &         1I6,' CHECK ICOORD')
          CALL PLANTE(1)
      END SELECT
!
      IF(IELMF.EQ.31.AND.IELMG.EQ.30) THEN
!
! LOOP ON THE ELEMENTS
      DO  IELEM = 1 , NELEM
! GETS THE ID OF THE FOUR NODES OF THE ELEMENT
        I1 = IKLE1(IELEM)
        I2 = IKLE2(IELEM)
        I3 = IKLE3(IELEM)
        I4 = IKLE4(IELEM)
! GETS THE FOUR NODAL VALUES OF THE VECTOR TO DIFFERENTIATE
        F1 = F(I1)
        F2 = F(I2)
        F3 = F(I3)
        F4 = F(I4)
! DIFFERENCES OF THE NODAL VALUES OF F
        F2MF1 = F2-F1
        F3MF1 = F3-F1
        F4MF1 = F4-F1
!
!  REAL COORDINATES OF THE POINTS OF THE ELEMENT (ORIGIN IN 1)
!
        X1  =  PX(I1)
        X2  =  PX(I2) - X1
        X3  =  PX(I3) - X1
        X4  =  PX(I4) - X1
        Y1  =  PY(I1)
        Y2  =  PY(I2) - Y1
        Y3  =  PY(I3) - Y1
        Y4  =  PY(I4) - Y1
        DET =  (X3*Y4-X4*Y3)*F2MF1 + (Y2*X4-X2*Y4)*F3MF1+
     &          (X2*Y3-Y2*X3)*F4MF1
! RESULT
        W(IELEM) = DET* G(IELEM) * XSUR24
      ENDDO
!
!
!=======================================================================
!     ERROR ON THE ELEMENT TYPES
!
      ELSE
!-----------------------------------------------------------------------
!
        WRITE(LU,1101) IELMF,SF%NAME
        WRITE(LU,1201) IELMG,SG%NAME
        WRITE(LU,1301)
        CALL PLANTE(1)
        STOP
 1101   FORMAT(1X,'VC11TT0 (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F:',1I6,
     &         1X,'REAL NAME: ',A6)
 1201   FORMAT(1X,'DISCRETIZATION OF G:',1I6,
     &         1X,'REAL NAME: ',A6)
 1301   FORMAT(1X,'CASE NOT IMPLEMENTED')
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE VC11TT0
