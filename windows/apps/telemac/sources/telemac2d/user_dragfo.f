!                   **********************
                    SUBROUTINE USER_DRAGFO
!                   **********************
!
     &(FUDRAG,FVDRAG)
!
!***********************************************************************
! TELEMAC2D   V8P4
!***********************************************************************
!
!brief    ADDS THE DRAG FORCE OF VERTICAL STRUCTURES IN THE
!+                MOMENTUM EQUATION.
!code
!+  FU IS THEN USED IN THE EQUATION AS FOLLOWS :
!+
!+  DU/DT + U GRAD(U) = - G * GRAD(FREE SURFACE) +..... + FU_IMP * U
!+
!+  AND THE TERM FU_IMP * U IS TREATED IMPLICITLY.
!
!warning  USER SUBROUTINE
!
!history  J-M HERVOUET
!+        01/03/1990
!+        V5P2
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
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!history  C.-T. PHAM (LNHE)
!+        14/05/2022
!+        V8P4
!+   Creation of user subroutine from old DRAGFO subroutine to provide
!+   an example of implementation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FUDRAG         |<--| DRAG FORCE ALONG X
!| FVDRAG         |<--| DRAG FORCE ALONG Y
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FUDRAG,FVDRAG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!      INTEGER IELEM,I,I4,NSOM,DISCLIN
!      DOUBLE PRECISION UNORM,AIRE,SOM,XSOM(4),YSOM(4),X4,Y4
!!     DOUBLE PRECISION, PARAMETER :: CD=1.56D0,DIAM=2.D0
!      DOUBLE PRECISION, PARAMETER :: CD=1.34D0,DIAM=2.D0
!      INTEGER, PARAMETER :: N=1
!
!-----------------------------------------------------------------------
!
      WRITE(LU,*) 'SUBROUTINE USER_DRAGFO HAS TO BE MODIFIED'
      WRITE(LU,*) 'IF VERTICAL STRUCTURES = YES'
      WRITE(LU,*) 'SEE EXAMPLES/TELEMAC2D/DRAGFORCE FOR AN EXAMPLE'
      WRITE(LU,*) 'OR THE COMMENTED CODE IN SOURCES/TELEMAC2D'
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
!!     COMPUTES THE MASSE INTEGRALS
!!
!      CALL VECTOR (T1,'=','MASBAS          ',UN%ELM,1.D0,
!     &             S,S,S,S,S,S,MESH,.FALSE.,S)
!!
!      CALL CPSTVC(UN,FUDRAG)
!      CALL CPSTVC(VN,FVDRAG)
!      CALL OS('X=C     ',X=FUDRAG,C=0.D0)
!      CALL OS('X=C     ',X=FVDRAG,C=0.D0)
!
!-----------------------------------------------------------------------
!
!!     EXAMPLE : DRAGFORCE IS SET IN A QUADRILATERAL DEFINED BY
!!               4 NODES
!!     SURFACE OF 20 X 40 CENTERED ON (0,0)
!!
!      NSOM = 4
!      XSOM(1) = -10.D0
!      XSOM(2) =  10.D0
!      XSOM(3) =  10.D0
!      XSOM(4) = -10.D0
!      YSOM(1) = -21.D0
!      YSOM(2) = -21.D0
!      YSOM(3) =  21.D0
!      YSOM(4) =  21.D0
!
!--------------------------------------------------------------
!
!!     P1 POINTS
!!
!      AIRE=0.D0
!      DO I=1,BIEF_NBPTS(11,MESH)
!!
!        IF(INPOLY(X(I),Y(I),XSOM,YSOM,NSOM)) THEN
!          UNORM = SQRT(UN%R(I)**2+VN%R(I)**2)
!          FUDRAG%R(I) =  - 0.5D0 * N * DIAM * CD * UNORM
!          FVDRAG%R(I) =  - 0.5D0 * N * DIAM * CD * UNORM
!          AIRE = AIRE + T1%R(I)
!        ENDIF
!!
!      ENDDO
!!
!!     QUASI-BUBBLE POINTS
!!
!      IF(FU%ELM.EQ.12) THEN
!!
!        DISCLIN=11
!        CALL CHGDIS(FUDRAG,DISCLIN,12,MESH)
!        CALL CHGDIS(FVDRAG,DISCLIN,12,MESH)
!!
!        DO IELEM = 1 , NELEM
!          I4=IKLE%I(IELEM+3*NELMAX)
!          X4=(X(IKLE%I(IELEM         ))+
!     &        X(IKLE%I(IELEM+  NELMAX))+
!     &        X(IKLE%I(IELEM+2*NELMAX)))/3.D0
!          Y4=(Y(IKLE%I(IELEM         ))+
!     &        Y(IKLE%I(IELEM+  NELMAX))+
!     &        Y(IKLE%I(IELEM+2*NELMAX)))/3.D0
!          IF(INPOLY(X4,Y4,XSOM,YSOM,NSOM)) AIRE = AIRE + T1%R(I4)
!        ENDDO
!!
!      ENDIF
!!
!!     IN PARALLEL THE AREA MAY BE SPLIT INTO SEVERAL SUB-DOMAINS
!!
!      IF(NCSIZE.GT.0) AIRE=P_SUM(AIRE)
!!
!!     NOW PREPARING THE DIVISION
!!
!      IF(AIRE.GT.1.D-6) THEN
!        SOM = 1.D0 / AIRE
!      ELSE
!        WRITE(LU,*) 'DRAGFO: AREA OF ZONE EQUAL TO ZERO'
!        CALL PLANTE(1)
!        STOP
!      ENDIF
!!
!!     DIVIDING BY THE AREA
!!
!      CALL OS('X=CX    ',X=FUDRAG,C=SOM)
!      CALL OS('X=CX    ',X=FVDRAG,C=SOM)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
