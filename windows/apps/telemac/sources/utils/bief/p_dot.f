!                   *******************************
                    DOUBLE PRECISION FUNCTION P_DOT
!                   *******************************
!
     &(NPOIN,X,Y,IFAC)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    SCALAR PRODUCT OF VECTORS X AND Y (SIZE NPOIN)
!+                TAKING PARALLELISM INTO ACCOUNT.
!
!history  REINHARD HINKELMANN (HANNOVER UNI.)
!+
!+
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        10/06/2015
!+        V7P1
!+   Moving from double precision FAC to integer IFAC.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FAC            |-->| FAC=1/(NUMBER OF NEIGHBOURING SUB-DOMAINS)
!| NPOIN          |-->| SIZE OF X AND Y
!| X              |-->| VECTOR
!| Y              |-->| VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_P_DOT => P_DOT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER, INTENT(IN) :: NPOIN
!
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN)
      INTEGER, INTENT(IN)          :: IFAC(NPOIN)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      P_DOT = 0.D0
!
      DO I = 1 , NPOIN
        P_DOT = P_DOT + X(I) * Y(I) * IFAC(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

