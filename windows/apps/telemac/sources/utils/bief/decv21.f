!                   *****************
                    SUBROUTINE DECV21
!                   *****************
!
     &(TETA,SL,ZF,IKLE,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    IDENTIFIES TIDAL FLATS.
!+
!+            DRYING ELEMENT : TETA = 0,
!+
!+            NORMAL ELEMENT : TETA = 1.
!+
!+            THE CRITERION FOR DRYING ELEMENTS IS THAT OF
!+                J.-M. JANIN : BOTTOM ELEVATION OF A POINT IN AN
!+                ELEMENT BEING HIGHER THAN THE FREE SURFACE
!+                ELEVATION OF ANOTHER.
!
!history  J-M HERVOUET (LNH)
!+        09/12/94
!+        V5P1
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
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN THE MESH
!| SL             |-->| FREE SURFACE
!| TETA           |<--| STATES IF AN ELEMENT IS DRY OR NOT
!| ZF             |-->| BOTTOM ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE BIEF, EX_DECV21 => DECV21
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)  :: NELEM,NELMAX
      INTEGER         , INTENT(IN)  :: IKLE(NELMAX,*)
      DOUBLE PRECISION, INTENT(OUT) :: TETA(NELEM)
      DOUBLE PRECISION, INTENT(IN)  :: SL(*),ZF(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
!
      DOUBLE PRECISION SL1,SL2,SL3,SL4,ZF1,ZF2,ZF3,ZF4
!
      INTRINSIC MAX,MIN
!
!-----------------------------------------------------------------------
!
      CALL OV('X=C     ', X=TETA, C=1.D0, DIM1=NELEM)
!
!-----------------------------------------------------------------------
!
      DO IELEM = 1 , NELEM
!
        SL1 = SL(IKLE(IELEM,1))
        SL2 = SL(IKLE(IELEM,2))
        SL3 = SL(IKLE(IELEM,3))
        SL4 = SL(IKLE(IELEM,4))
!
        ZF1 = ZF(IKLE(IELEM,1))
        ZF2 = ZF(IKLE(IELEM,2))
        ZF3 = ZF(IKLE(IELEM,3))
        ZF4 = ZF(IKLE(IELEM,4))
!
        IF(MAX(ZF1,ZF2,ZF3,ZF4).GT.MIN(SL1,SL2,SL3,SL4)) THEN
          TETA(IELEM) = 0.D0
        ENDIF
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
