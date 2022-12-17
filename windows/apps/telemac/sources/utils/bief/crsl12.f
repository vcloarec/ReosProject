!                   *****************
                    SUBROUTINE CRSL12
!                   *****************
!
     &(NEWSL,OLDSL,ZF,IKLE,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CORRECTS THE FREE SURFACE COMPUTATION BY ELEMENTS
!+                TO TAKE ACCOUNT OF THE TIDAL FLATS.
!+
!+            QUASI-BUBBLE ELEMENT.
!
!history  J-M JANIN    (LNH)
!+        27/11/92
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
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NEWSL          |<->| MODIFIED FREE SURFACE, PER ELEMENT
!| OLDSL          |-->| REAL FREE SURFACE, PER POINT
!| ZF             |-->| BATHYMETRY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      DOUBLE PRECISION, INTENT(INOUT) :: NEWSL(NELMAX,4)
      DOUBLE PRECISION, INTENT(IN)    :: OLDSL(*),ZF(*)
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,4)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IK(4),J(4)
      DOUBLE PRECISION SLM
!
!-----------------------------------------------------------------------
!
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
!  1) SORTS (ASCENDING ORDER) THE BOTTOM ELEVATIONS AND POTENTIALLY
!     CORRECTS THE FREE SURFACE ELEVATION FOR DRYING ELEMENTS
!
!-----------------------------------------------------------------------
!
      DO IELEM = 1 , NELEM
!
      IK(1) = IKLE(IELEM,1)
      J (1) = 1
      IK(2) = IKLE(IELEM,2)
      J (2) = 2
      IK(3) = IKLE(IELEM,3)
      J (3) = 3
      IK(4) = IKLE(IELEM,4)
      J (4) = 4
!
      IF (ZF(IK(2)).LT.ZF(IK(1)))  THEN
        J(2)=1
        J(1)=2
      ENDIF
      IF (ZF(IK(3)).LT.ZF(IK(J(2)))) THEN
        J(3)=J(2)
        J(2)=3
        IF (ZF(IK(3)).LT.ZF(IK(J(1)))) THEN
          J(2)=J(1)
          J(1)=3
        ENDIF
      ENDIF
      IF (ZF(IK(4)).LT.ZF(IK(J(3)))) THEN
        J(4)=J(3)
        J(3)=4
        IF (ZF(IK(4)).LT.ZF(IK(J(2)))) THEN
          J(3)=J(2)
          J(2)=4
          IF (ZF(IK(4)).LT.ZF(IK(J(1)))) THEN
            J(2)=J(1)
            J(1)=4
          ENDIF
        ENDIF
      ENDIF
!
      SLM=OLDSL(IK(J(1)))
      NEWSL(IELEM,J(1))=SLM
      NEWSL(IELEM,J(2))=OLDSL(IK(J(2)))-MAX(0.D0,ZF(IK(J(2)))-SLM)
      SLM=MAX(SLM,NEWSL(IELEM,J(2)))
      NEWSL(IELEM,J(3))=OLDSL(IK(J(3)))-MAX(0.D0,ZF(IK(J(3)))-SLM)
      SLM=MAX(SLM,NEWSL(IELEM,J(3)))
      NEWSL(IELEM,J(4))=OLDSL(IK(J(4)))-MAX(0.D0,ZF(IK(J(4)))-SLM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
