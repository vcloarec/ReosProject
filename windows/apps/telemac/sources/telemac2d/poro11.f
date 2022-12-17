!                   *****************
                    SUBROUTINE PORO11
!                   *****************
!
     &(TETA,ZF,HN,IKLE,NELEM,NELMAX)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    IDENTIFIES TIDAL FLATS.
!+
!+
!+            IMPLEMENTS DELFINA ET AL WETTING/DRYING ALGORITHM.
!+
!+            PARTIALLY WET ELEMENT : TETA = 0
!+
!+            WET ELEMENT           : TETA = NU = 1.0
!+
!+            DRY ELEMENT           : TETA = NU = 0.0
!+
!+
!+            THE DRYING CRITERION IS THAT OF J.-M. JANIN :
!+                BOTTOM ELEVATION AT ONE NODE OF THE ELEMENT IS
!+                HIGHER THAN FREE SURFACE ELEVATION AT ANOTHER.
!
!history  J-M HERVOUET (LNHE)     ; PAUL BATES (BRISTOL)
!+        01/08/1997
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
!| HN             |-->| WATER DEPTH AT TIME T(N)
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| TETA           |<--| POROSITY PER ELEMENT
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,*)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(*),HN(*)
      DOUBLE PRECISION, INTENT(INOUT) :: TETA(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
      DOUBLE PRECISION SL1,SL2,SL3,ZF1,ZF2,ZF3,H1,H2,H3,EPS1,EPS2
!
      LOGICAL TEST1, TEST2
!
      INTRINSIC MAX,MIN
!
!-----------------------------------------------------------------------
!
      CALL OV('X=C     ', X=TETA, C=1.D0, DIM1=NELEM)
!
!--------------------------------------------------------------
!
!  OPTION FOR COMPUTATION OF NU
!
!  1 = EXACT CALCULATION BASED ON PROJECTED INUNDATED AREA
!  2 = ROUGH APPROXIMATION TO INUNDATED AREA
!  3 = CONSTANT VALUE
!  4 = USER SPECIFIED
!
      EPS1=1.D-6
      EPS2=0.3D0
!
!-----------------------------------------------------------------------
!
      DO IELEM = 1 , NELEM
!
        ZF1 = ZF(IKLE(IELEM,1))
        ZF2 = ZF(IKLE(IELEM,2))
        ZF3 = ZF(IKLE(IELEM,3))
!
        H1 = HN(IKLE(IELEM,1))
        H2 = HN(IKLE(IELEM,2))
        H3 = HN(IKLE(IELEM,3))
!
        SL1 = H1 + ZF1
        SL2 = H2 + ZF2
        SL3 = H3 + ZF3
!
        TEST1 = .FALSE.
        TEST2 = .FALSE.
!
!       IF TIDAL FLAT SUSPECTED AND SOME WATER IN ELEMENT
!       (POROSITY LEFT TO 1 IN TOTALLY DRY ELEMENTS)
!
        IF(     MAX(ZF1,ZF2,ZF3).GT.MIN(SL1,SL2,SL3)
     &     .AND.MAX(H1,H2,H3).GT.EPS1                ) TEST1 = .TRUE.
!
!
!
        IF(MAX(H1,H2,H3).EQ.H1.AND.SL1.GT.MIN(ZF1,ZF2,ZF3)
     &                        .AND.SL1.LT.MAX(ZF1,ZF2,ZF3).OR.
     &     MAX(H1,H2,H3).EQ.H2.AND.SL2.GT.MIN(ZF1,ZF2,ZF3)
     &                        .AND.SL2.LT.MAX(ZF1,ZF2,ZF3).OR.
     &     MAX(H1,H2,H3).EQ.H3.AND.SL3.GT.MIN(ZF1,ZF2,ZF3)
     &                    .AND.SL3.LT.MAX(ZF1,ZF2,ZF3)) TEST2=.TRUE.
!
!---------------------------------------------------------------------
!
        IF(TEST1.AND.TEST2) THEN
!         ROUGH CALCULATION OF POROSITY BY PERCENTAGE OF ELEMENT INUNDATED
          TETA(IELEM)=MAX(H1,H2,H3)/(MAX(ZF1,ZF2,ZF3)-MIN(ZF1,ZF2,ZF3))
          TETA(IELEM) = MAX(TETA(IELEM),EPS2)
        ENDIF
!
!---------------------------------------------------------------------
!
      ENDDO ! IELEM
!
!---------------------------------------------------------------------
!
      RETURN
      END
