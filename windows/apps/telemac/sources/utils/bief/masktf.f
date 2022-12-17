!                   *****************
                    SUBROUTINE MASKTF
!                   *****************
!
     &(MASKEL,HN,HMIN,IKLE,NELEM,NPOIN)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MASKS DRY ELEMENTS (MASKS TIDAL FLATS).
!code
!+    SIMPLE ALGORITHM: AN ELEMENT IS DRY IF ONE OF ITS DEPTHS IS
!+                      LESS THAN HMIN
!+
!+    USED BY SISYPHE (IN THIS CASE THE DEPTH IS GIVEN BY TELEMAC, SO
!+    A SOPHISTICATED ALGORITHM TO PRESERVE THE DYNAMICS OF FLOODING
!+    IS NOT NEEDED)
!
!history  J-M HERVOUET (LNHE)
!+        20/10/08
!+        V5P9
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
!| HMIN           |-->| MINIMUM VALUE OF DEPTH
!| HN             |-->| WATER DEPTH AT TIME N
!| IKLE           |-->| CONNECTIVITY TABLE.
!| MASKEL         |<--| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM,NPOIN
      INTEGER, INTENT(IN)             :: IKLE(NELEM,3)
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HMIN
      DOUBLE PRECISION, INTENT(INOUT) :: MASKEL(NELEM)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3
!
!-----------------------------------------------------------------------
!
      DO IELEM = 1,NELEM
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        IF(HN(I1).LE.HMIN.OR.HN(I2).LE.HMIN.OR.HN(I3).LE.HMIN) THEN
          MASKEL(IELEM) = 0.D0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
