!                   *****************
                    SUBROUTINE CORVIS
!                   *****************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CORRECTS THE DIFFUSION COEFFICIENT.
!
!warning  THIS SUBROUTINE IS MERELY AN EXAMPLE; MUST BE CODED BY THE USER
!code
!+   EXAMPLE : SETS THE VALUE TO 0.1 INSIDE A SQUARE.
!+
!+   NSOM = 4
!+   XSOM(1) =     0.D0
!+   YSOM(1) =     0.D0
!+   XSOM(2) =  1000.D0
!+   YSOM(2) =     0.D0
!+   XSOM(3) =  1000.D0
!+   YSOM(3) =  1000.D0
!+   XSOM(4) =     0.D0
!+   YSOM(4) =  1000.D0
!+
!+   CALL FILPOL( VISC , 0.1D0 , XSOM , YSOM , NSOM , MESH )
!warning  THE SUBROUTINE IS CALLED AT EVERY TIME STEP
!
!history  J-M HERVOUET (LNHE)
!+        17/08/1994
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!EX   USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!     EXAMPLE : SETS THE VALUE TO 0.1 INSIDE A SQUARE.
!               REMOVE THE CEX COMMENTS TO IMPLEMENT IT.
!
!
!EX   INTEGER NSOM
!EX   DOUBLE PRECISION XSOM(10),YSOM(10)
!
!-----------------------------------------------------------------------
!
!     DESCRIBES THE SQUARE AS A POLYGON TO CALL ROUTINE FILPOL
!
!EX   NSOM = 4
!EX   XSOM(1) =     0.D0
!EX   YSOM(1) =     0.D0
!EX   XSOM(2) =  1000.D0
!EX   YSOM(2) =     0.D0
!EX   XSOM(3) =  1000.D0
!EX   YSOM(3) =  1000.D0
!EX   XSOM(4) =     0.D0
!EX   YSOM(4) =  1000.D0
!
!EX   CALL FILPOL( VISC , 0.1D0 , XSOM , YSOM , NSOM , MESH )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
