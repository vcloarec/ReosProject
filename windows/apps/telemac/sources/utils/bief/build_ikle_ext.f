!                   *************************
                    SUBROUTINE BUILD_IKLE_EXT
!                   *************************
!
     &(IKLE_EXT,NELMAX,IKLE,NELEM)
!
!***********************************************************************
! BIEF   V6P3                                   05/10/2012
!***********************************************************************
!
!brief    Copy of IKLE2 into a larger array with a larger number of
!+        elements. Values of 1 put in extended area.
!         This is used in post_interp (module streamline)
!
!history  J-M HERVOUET (EDF-LNHE)
!+        05/10/2012
!+        V6P3
!+   First version
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLE_EXT       |<->| EXTENDED IKLE
!| NELMAX         |-->| FIRST DIMENSION OF EXTENDED IKLE
!| IKLE           |-->| ORIGINAL IKLE
!| NELEM          |-->| FIRST DIMENSION OF ORIGINAL IKLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELMAX,NELEM
      INTEGER, INTENT(IN)    :: IKLE(NELEM,3)
      INTEGER, INTENT(INOUT) :: IKLE_EXT(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
!
!-----------------------------------------------------------------------
!
      DO IELEM=1,NELEM
        IKLE_EXT(IELEM,1)=IKLE(IELEM,1)
        IKLE_EXT(IELEM,2)=IKLE(IELEM,2)
        IKLE_EXT(IELEM,3)=IKLE(IELEM,3)
      ENDDO
!
      IF(NELMAX.GT.NELEM) THEN
        DO IELEM=NELEM+1,NELMAX
          IKLE_EXT(IELEM,1)=1
          IKLE_EXT(IELEM,2)=1
          IKLE_EXT(IELEM,3)=1
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
