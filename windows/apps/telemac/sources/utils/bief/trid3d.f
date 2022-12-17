!                   *****************
                    SUBROUTINE TRID3D
!                   *****************
!
     &(XAUX,X,B,NPOIN,NPOIN2)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES TRIDIAGONAL SYSTEMS FOR EVERY VERTICAL
!+                IN A MESH OF PRISMS.
!
!history  J-M HERVOUET (LNHE)
!+        30/09/05
!+        V5P6
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
!| B              |-->| RIGHT-HAND SIDE OF SYSTEM
!| NPOIN          |-->| NUMBER OF POINTS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| X              |<--| SOLUTION OF SYSTEM
!| XAUX           |-->| TRI-DIAGONAL MATRIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_TRID3D => TRID3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NPOIN2
!
      DOUBLE PRECISION, INTENT(IN)    :: B(NPOIN2,*)
      DOUBLE PRECISION, INTENT(INOUT) :: XAUX(NPOIN,*),X(NPOIN2,*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IPLAN,I3D,NPLAN
      DOUBLE PRECISION EPS
!
      INTRINSIC ABS
!
!-----------------------------------------------------------------------
!
!     XAUX(I,1) IS COEFFICIENT OF POINT BELOW I IN EQUATION OF POINT I
!     XAUX(I,2) IS THE DIAGONAL
!     XAUX(I,3) IS COEFFICIENT OF POINT ABOVE I IN EQUATION OF POINT I
!
!     XAUX(I,4) HERE USED AS WORKING ARRAY
!     XAUX(I,5) HERE USED AS WORKING ARRAY
!
!-----------------------------------------------------------------------
!
      NPLAN=NPOIN/NPOIN2
      EPS=1.D-8
!
!-----------------------------------------------------------------------
!
!     BASIC ALGORITHM TAKEN IN "NUMERICAL RECIPES" PAGE 40 AND ADAPTED
!     TO STORAGE
!
!     XAUX(*,4) : WORKING ARRAY (SIZE NPOIN2)
!     XAUX(*,5) : WORKING ARRAY (SIZE NPOIN)
!
      DO I=1,NPOIN2
        XAUX(I,4)=XAUX(I,2)
        IF(ABS(XAUX(I,4)).LT.EPS) THEN
          WRITE(LU,*) 'TRID3D: SYSTEM ILL-DEFINED'
          CALL PLANTE(1)
          STOP
        ENDIF
        X(I,1)=B(I,1)/XAUX(I,4)
      ENDDO
!
      DO IPLAN=2,NPLAN
      DO I=1,NPOIN2
        I3D=I+NPOIN2*(IPLAN-1)
        XAUX(I3D,5)=XAUX(I3D-NPOIN2,3)/XAUX(I,4)
        XAUX(I,4)=XAUX(I3D,2)-XAUX(I3D,1)*XAUX(I3D,5)
        IF(ABS(XAUX(I,4)).LT.EPS) THEN
          WRITE(LU,*) 'TRID3D: SYSTEM ILL-DEFINED'
          WRITE(LU,*) '        PRECONDITIONING 17 IMPOSSIBLE'
          CALL PLANTE(1)
          STOP
        ENDIF
        X(I,IPLAN)=(B(I,IPLAN)-XAUX(I3D,1)*X(I,IPLAN-1))/XAUX(I,4)
      ENDDO
      ENDDO
!
      DO IPLAN=NPLAN-1,1,-1
      DO I=1,NPOIN2
        I3D=I+NPOIN2*IPLAN   ! PLAN TO THE TOP
        X(I,IPLAN)=X(I,IPLAN)-XAUX(I3D,5)*X(I,IPLAN+1)
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
