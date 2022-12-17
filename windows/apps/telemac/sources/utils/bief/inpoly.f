!                   ***********************
                    LOGICAL FUNCTION INPOLY
!                   ***********************
!
     &( X , Y , XSOM , YSOM , NSOM )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INDICATES IF A POINT WITH COORDINATES X AND Y IS
!+                IN A POLYGON WITH GIVEN VERTICES.
!code
!+    PRINCIPLE: TAKES HALF A LINE STARTING FROM THE POINT AND COUNTS THE
!+               NUMBER OF TIMES IT INTERSECTS WITH THE POLYGON
!+
!+    ALSO WORKS IF THE POLYGON IS NOT CONVEX
!+
!+    INTERSECTIONS ARE IDENTIFIED USING THE LINES PARAMETRIC EQUATIONS :
!+
!+
!+    X + A * MU = XDEP + (XARR-XDEP) * LAMBDA
!+    Y + B * MU = YDEP + (YARR-YDEP) * LAMBDA
!+
!+    THE HALF-LINE IS CHARACTERISED BY THE CHOICE OF A AND B, AND THE
!+    SIGN OF MU. THERE IS INTERSECTION IF MU > 0 AND 0 < LAMBDA < 1
!
!warning  THE POLYGON VERTICES MUST BE DISTINCT (NO DUPLICATE NODES)
!
!history  E. DAVID (LHF)
!+
!+
!+   ORIGINAL IDEA AND CODE
!
!history  J.-M. HERVOUET (LNH)
!+        18/06/96
!+        V5P2
!+
!
!history  JEAN-PHILIPPE RENAUD (CSN BRISTOL)
!+        27/07/99
!+
!+   CORRECTION FOR A SPECIAL CASE
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
!| NSOM           |-->| NUMBER OF APICES OF POLYGON
!| X              |-->| ABSCISSA OF POINT
!| Y              |-->| ORDINATE OF POINT
!| XSOM           |-->| ABSCISSAE OF POLYGON APICES
!| YSOM           |-->| ORDINATES OF POLYGON APICES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NSOM
      DOUBLE PRECISION, INTENT(IN) :: X,Y
      DOUBLE PRECISION, INTENT(IN) :: XSOM(NSOM),YSOM(NSOM)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,NSECT
!
      DOUBLE PRECISION A,B,ANGLE,XDEP,YDEP,XARR,YARR,DET,MU,LAMBDA,EPS
!
      DOUBLE PRECISION PI
      INTRINSIC COS,SIN,ABS,MOD
!
!-----------------------------------------------------------------------
!
      EPS = 1.D-9
      ANGLE = -1.D0
!
! CHOOSES A AND B SUCH AS TO AVOID SPECIAL CASES
!
1000  CONTINUE
      ANGLE = ANGLE + 1.D0
      IF(ANGLE.GT.360.D0) THEN
!       SPECIAL CASE OF A POINT ON THE CONTOUR
        INPOLY=.TRUE.
        RETURN
      ENDIF
      PI = 4.D0 * ATAN( 1.D0 )
      A = COS( ANGLE*PI/180.D0 )
      B = SIN( ANGLE*PI/180.D0 )
      NSECT=0
!
! LOOP ON ALL THE SEGMENTS OF THE POLYGON
!
      DO N=1,NSOM
!
!     DEP : 1ST POINT OF THE SEGMENT    ARR : 2ND POINT
!
      XDEP=XSOM(N)
      YDEP=YSOM(N)
      IF(N.LT.NSOM) THEN
        XARR=XSOM(N+1)
        YARR=YSOM(N+1)
      ELSE
        XARR=XSOM(1)
        YARR=YSOM(1)
      ENDIF
!
!     CASE WHERE TWO SUCCESSIVE POINTS ARE DUPLICATES
!
      IF(ABS(XDEP-XARR)+ABS(YDEP-YARR).LT.EPS) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) ' '
        WRITE(LU,*) 'INPOLY: SUPERIMPOSED POINTS IN THE POLYGON'
        WRITE(LU,*) 'AT POINT: ',XDEP,'  AND  ',YDEP,' WITH NUMBER ',N
        IF(N.EQ.NSOM) THEN
          WRITE(LU,*) 'THE LAST POINT MUST NOT BE EQUAL TO THE FIRST'
          WRITE(LU,*) 'FOR EXAMPLE, GIVE 3 POINTS FOR A TRIANGLE'
        ENDIF
        WRITE(LU,*) 'INPOLY IS PROBABLY CALLED BY FILPOL'
        WRITE(LU,*) ' '
        WRITE(LU,*) ' '
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     CASE WHERE THE POINT IS A VERTEX
!     (THE GENERAL ALGORITHM WOULD DEFINE IT AS EXTERNAL WITH 2 INTERSECTIONS)
!
      IF(ABS(X-XDEP).LE.EPS.AND.ABS(Y-YDEP).LE.EPS) THEN
        NSECT=1
        EXIT
      ENDIF
!
!     DETERMINANT OF THE KRAMER SYSTEM
!
      DET = A*(YDEP-YARR)-B*(XDEP-XARR)
      IF(ABS(DET).LT.EPS) GO TO 1000
!
      MU     = ( (XDEP-X)*(YDEP-YARR)-(YDEP-Y)*(XDEP-XARR) ) / DET
      LAMBDA = (    A    *(YDEP-Y   )-    B   *(XDEP-X   ) ) / DET
!
!-------------------------------------------------------
! JP RENAUD (CSN BRISTOL) CORRECTION TO AVOID THAT THE INTERSECTION
! POINT BE ONE OF THE VRTICES
!
! IF THE INTERSECTION POINT IS A VERTEX, INCREASES THE ANGLE
! OTHERWISE THE POINT WOULD BE COUNTED TWICE INSTEAD OF JUST ONCE
!
      IF ((ABS(X+A*MU-XDEP).LE.EPS.AND.ABS(Y+B*MU-YDEP).LE.EPS)
     &    .OR.
     &    (ABS(X+A*MU-XARR).LE.EPS.AND.ABS(Y+B*MU-YARR).LE.EPS))
     &    GOTO 1000
!
! END OF JP RENAUD CORRECTION
!-------------------------------------------------------
!
      IF(MU.GE.-EPS.AND.LAMBDA.GE.-EPS.AND.LAMBDA.LE.1.D0+EPS) THEN
        NSECT=NSECT+1
      ENDIF
!
      ENDDO ! N
!
      INPOLY=(MOD(NSECT,2).EQ.1)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
