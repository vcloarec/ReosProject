!                   ****************
                    SUBROUTINE GRADP
!                   ****************
!
     &(NS,NT,IKLE,AIRT,X,Y,DPX,DPY)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE BASES FUNCTIONS GRADIENTS.
!
!history  INRIA
!+
!+        V5P4
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
!| AIRT           |-->| AREA OF TRIANGLES
!| DPX            |<--| GRADIENT OF BASES FUNCTIONS WITH RESPECT TO X
!| DPY            |<--| GRADIENT OF BASES FUNCTIONS WITH RESPECT TO Y
!| IKLE           |-->| CONNECTIVITY TABLE
!| NS             |-->| NUMBER OF POINTS IN THE MESH
!| NT             |-->| NUMBER OF TRIANGLES IN THE MESH
!| X              |-->| ABSCISSAE OF NODES IN THE MESH
!| Y              |-->| ORDINATES OF NODES IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NS,NT,IKLE(NT,3)
      DOUBLE PRECISION, INTENT(IN)  :: X(NS),Y(NS),AIRT(NT)
      DOUBLE PRECISION, INTENT(OUT) :: DPX(3,NT),DPY(3,NT)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER JT,NUBO1,NUBO2,NUBO3
      DOUBLE PRECISION AIRJI,X1,X2,X3,Y1,Y2,Y3
!
!-----------------------------------------------------------------------
!
      DO JT=1,NT
!
        NUBO1 = IKLE(JT,1)
        NUBO2 = IKLE(JT,2)
        NUBO3 = IKLE(JT,3)
!
        AIRJI = 0.5D0/AIRT(JT)
!
!       COMPUTES THE P1-GRADIENTS
!
        X1 = X(NUBO1)
        Y1 = Y(NUBO1)
        X2 = X(NUBO2)
        Y2 = Y(NUBO2)
        X3 = X(NUBO3)
        Y3 = Y(NUBO3)
!
        DPX(1,JT) = AIRJI*(Y2-Y3)
        DPX(2,JT) = AIRJI*(Y3-Y1)
        DPX(3,JT) = AIRJI*(Y1-Y2)
        DPY(1,JT) = AIRJI*(X3-X2)
        DPY(2,JT) = AIRJI*(X1-X3)
        DPY(3,JT) = AIRJI*(X2-X1)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
