!                   **********************
                    SUBROUTINE BIEF_ININDS
!                   **********************
!
     &(NPOIN,NPTFR,NELEM,NPMAX,NPTFX,NELMAX,NPLAN,NSEGBOR,NDS,NELEB)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE ARRAY NDS, WHICH IS USED BY
!+               FUNCTIONS BIEF_NBPTS, BIEF_NBSEG, BIEF_NBFEL, AND NBPEL.
!+               FUNCTIONS BIEF_NBMPTS, BIEF_NBSEGEL.
!code
!+ INDEX 1 OF NDS : NUMBER OF DEGREES OF FREEDOM IN THE MESH FOR THIS ELEMENT
!+ INDEX 2 OF NDS : NUMBER OF SEGMENTS IN THE MESH FOR THIS ELEMENT
!+ INDEX 3 OF NDS : NUMBER OF POINTS PER ELEMENT
!+ INDEX 4 OF NDS : NUMBER OF FACES PER ELEMENT (EX 3 FOR A TRIANGLE)
!+ INDEX 5 OF NDS : MAXIMUM NUMBER OF DEGREES OF FREEDOM IN THE MESH
!+                  FOR THIS ELEMENT
!+ INDEX 6 OF NDS : NUMBER OF SEGMENTS FOR THIS ELEMENT
!+
!+ FUNCTIONS IN BIEF:
!+
!+ INDEX 1 OF NDS : NBPTS
!+ INDEX 2 OF NDS : NBSEG
!+ INDEX 3 OF NDS : NBPEL
!+ INDEX 4 OF NDS : NBFEL
!+ INDEX 5 OF NDS : NBMPTS
!+ INDEX 6 OF NDS : NBSEGEL
!
!history  J-M HERVOUET (LNH)     ; REGINA NEBAUER; LAM MINH PHUONG
!+        24/10/2008
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
!history  J-M HERVOUET (LNHE)
!+        25/08/2011
!+        V6P2
!+   NBSEG and NBSEGEL added for element 51
!
!history  F. DECUNG (LNHE)
!+        01/01/2013
!+        V6P3
!+   NBSEG added for elements 31 and 81
!+
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        12/05/2014
!+        V7P0
!+   Discontinuous elements better treated: new types 15, 16 and 17 for
!+   discontinuous linear, quasi-bubble, and quadratic, rather than
!+   using component DIMDISC=11, 12 or 13.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NDS            |<--| ARRAY IN BIEF_MESH STRUCTURE FOR STORING
!|                |   | NUMBERS
!| NELEB          |-->| NUMBER OF BOUNDARY ELEMENTS
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN THE MESH
!| NPLAN          |-->| NUMBER OF PLANES
!| NPMAX          |-->| MAXIMUM NUMBER OF NODES IN THE MESH
!| NPOIN          |-->| NUMBER OF NODES IN THE 2D MESH
!|                |   | (EXCEPT TETRAHEDRA WHERE IT IS THE TOTAL NUMBER)
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NPTFX          |-->| MAXIMUM NUMBER OF BOUNDARY POINTS
!| NSEGBOR        |-->| NUMBER OF BOUNDARY SEGMENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER , INTENT(IN)           :: NPOIN
      INTEGER , INTENT(IN)           :: NPTFR
      INTEGER , INTENT(IN)           :: NELEM
      INTEGER , INTENT(IN)           :: NPMAX
      INTEGER , INTENT(IN)           :: NPTFX
      INTEGER , INTENT(IN)           :: NELMAX
      INTEGER , INTENT(IN)           :: NPLAN
      INTEGER , INTENT(IN)           :: NSEGBOR
      INTEGER , INTENT(IN), OPTIONAL :: NELEB
      INTEGER , INTENT(OUT)          :: NDS(0:81,7)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     THE VALUES WHICH WILL NOT BE USED ARE NOT INITIALISED
!
!     0) P0 ELEMENT IN DIMENSION 1
!
!                 NSEGBOR (OVER-DIMENSIONING)
      NDS(00,1) = NPTFR
!     NDS(00,2) = ?????
      NDS(00,3) = 1
!     NDS(00,4) = ?????
      NDS(00,5) = NPTFX
!     NDS(00,6) = ?????
!
!-----------------------------------------------------------------------
!
!     1) P1 ELEMENT IN DIMENSION 1
!
      NDS(01,1) = NPTFR
      NDS(01,2) = NPTFR ! = NSEGBOR
      NDS(01,3) = 2
!     NDS(01,4) = ?????
      NDS(01,5) = NPTFX
      NDS(01,6) = 1
!
!-----------------------------------------------------------------------
!
!     2) QUADRATIC ELEMENT IN DIMENSION 1
!
!                 NPTFR+NSEGBOR (OVER-DIMENSIONING)
      NDS(02,1) = 2*NPTFR
!     NDS(02,2) = ?????
      NDS(02,3) = 3
!     NDS(02,4) = ?????
      NDS(02,5) = 2*NPTFX
!     NDS(02,6) = ?????
!
!     NOTE : THE QUADRATIC BOUNDARY POINT OF SEGMENT K WILL HAVE
!            THE NUMBER K+NPTFR, THUS WE HAVE HERE TO DECLARE
!            NDS(02,1) AS 2*NPTFR, THOUGH IN PARALLEL THERE MAY
!            EXISTING POINT K WITHOUT SEGMENT K IN THE DOMAIN
!
!
!-----------------------------------------------------------------------
!
!     10) P0 ELEMENT ON TRIANGLES
!
      NDS(10,1) = NELEM
!     NDS(10,2) = ?????
      NDS(10,3) = 1
      NDS(10,4) = 3
      NDS(10,5) = NELMAX
!     NDS(10,6) = ??????
!
!-----------------------------------------------------------------------
!
!     11) P1 ELEMENT ON TRIANGLES
!
      NDS(11,1) = NPOIN
      NDS(11,2) = (3*NELEM+NSEGBOR)/2
      NDS(11,3) = 3
      NDS(11,4) = 3
      NDS(11,5) = NPMAX
      NDS(11,6) = 3
!
!-----------------------------------------------------------------------
!
!     12) QUASI-BUBBLE ELEMENT ON TRIANGLES
!
      NDS(12,1) = NPOIN+NELEM
      NDS(12,2) = (9*NELEM+NSEGBOR)/2
      NDS(12,3) = 4
      NDS(12,4) = 3
      NDS(12,5) = NPMAX+NELMAX
      NDS(12,6) = 6
!
!-----------------------------------------------------------------------
!
!     13) QUADRATIC ELEMENT ON TRIANGLES
!
!     NUMBER OF DDL = NUMBER OF NODES + NUMBER OF SEGMENTS OF P1
      NDS(13,1) = NDS(11,1)+NDS(11,2)
!     TOTAL NUMBER OF SEGMENTS (3 PER LINEAR SEGMENT +
!                               6 INTERIOR SEGMENTS FOR EACH ELEMENT)
      NDS(13,2) = 6*NELEM+3*NDS(11,2)
      NDS(13,3) = 6
      NDS(13,4) = 3
      NDS(13,5) = NPMAX+(3*NELMAX+NSEGBOR)/2
      NDS(13,6) = 15
!
!-----------------------------------------------------------------------
!
!     14) P1-ISO P1 ELEMENT ON TRIANGLES
!
!     NUMBER OF DDL = NUMBER OF NODES + NUMBER OF SEGMENTS OF P1
      NDS(14,1) = NDS(11,1)+NDS(11,2)
!     TOTAL NUMBER OF SEGMENTS (3 PER LINEAR SEGMENT +
!                               3 INTERIOR SEGMENTS FOR EACH ELEMENT)
      NDS(14,2) = 3*NELEM+3*NDS(11,2)
      NDS(14,3) = 6
      NDS(14,4) = 3
      NDS(14,5) = NPMAX+(3*NELMAX+NSEGBOR)/2
!     3 LINEAR SEGMENTS FOLLOWED BY THE 9 P1-ISO P1
      NDS(14,6) = 12
!     NDS(10,6) = ??????
!
!-----------------------------------------------------------------------
!
!     15) PIECE-WISE P1 ELEMENT ON TRIANGLES (DISCONTINUOUS ELEMENT)
!         ONLY NUMBER OF POINTS REALLY LOOKED AT, THE REST UNSURE
!
      NDS(15,1) = 3*NELEM
      NDS(15,2) = (3*NELEM+NSEGBOR)/2
      NDS(15,3) = 3
      NDS(15,4) = 3
      NDS(15,5) = 3*NELMAX
      NDS(15,6) = 3
!
!-----------------------------------------------------------------------
!
!     16) PIECE-WISE QUASI-BUBBLE ON TRIANGLES (DISCONTINUOUS ELEMENT)
!         ONLY NUMBER OF POINTS REALLY LOOKED AT, THE REST UNSURE
!
      NDS(16,1) = 4*NELEM
      NDS(16,2) = (9*NELEM+NSEGBOR)/2
      NDS(16,3) = 4
      NDS(16,4) = 3
      NDS(16,5) = 4*NELMAX
      NDS(16,6) = 6
!
!-----------------------------------------------------------------------
!
!     17) PIECE-WISE QUADRATIC ELEMENT ON TRIANGLES (DISCONTINUOUS ELEMENT)
!         ONLY NUMBER OF POINTS REALLY LOOKED AT, THE REST UNSURE
!
      NDS(17,1) = 6*NELEM
      NDS(17,2) = 6*NELEM+3*NDS(11,2)
      NDS(17,3) = 6
      NDS(17,4) = 3
      NDS(17,5) = 6*NELMAX
      NDS(17,6) = 15
!
!-----------------------------------------------------------------------
!
!     20) P0 ELEMENT ON QUADRILATERALS (3D: SPECIAL CASE)
!
      NDS(20,1) = NELEM
!     NDS(20,2) = ?????
      NDS(20,3) = 1
      NDS(20,4) = 4
      NDS(20,5) = NELMAX
!     NDS(20,6) = ??????
!     LATERAL SIDES OF 3D PRISM MESHES
!     IF(NPLAN.GE.2) THEN
!       NDS(20,1) = NPTFR*(NPLAN-1)
!       NDS(20,5) = NPTFX*(NPLAN-1)
!     ENDIF
!
!-----------------------------------------------------------------------
!
!     21) P1 ELEMENT ON QUADRILATERALS (3D: SPECIAL CASE)
!
      NDS(21,1) = NPOIN
!     NDS(21,2) = ?????
      NDS(21,3) = 4
      NDS(21,4) = 4
      NDS(21,5) = NPMAX
!     NDS(21,6) = ?????
!     LATERAL SIDES OF 3D PRISM MESHES
!     IF(NPLAN.GE.2) THEN
!       NDS(21,1) = NPTFR*NPLAN
!       NDS(21,5) = NPTFX*NPLAN
!     ENDIF
!
!-----------------------------------------------------------------------
!
!     THREE-DIMENSIONAL ELEMENTS (ONLY FOR 3D MESHES)
!
!
!     30) T0 ELEMENT ON TETRAHEDRONS
!
      NDS(30,1) = NELEM
!     NDS(30,2) = ?????
      NDS(30,3) = 1
      NDS(30,4) = 4
      NDS(30,5) = NELMAX
!     NDS(30,6) = ?????
!
!-----------------------------------------------------------------------
!
!     31) T1 ELEMENT ON TETRAHEDRONS
!
!C 3D MESH
      NDS(31,1) = NPOIN
      !SEEMS TO BE "EXACT" IN SCALAR BUT NOT IN PARALLEL...
      !NDS(31,2) = NELEM + NPOIN + 0.75*NELEB - 0.5*NPTFR
      NDS(31,2) = NELEM + 3*NPOIN + (3*NELEB)/2 + NPTFR
      NDS(31,3) = 4
      NDS(31,4) = 4
      NDS(31,5) = NPMAX
      NDS(31,6) = 6
!
!-----------------------------------------------------------------------
!
!     IF(NPLAN.GT.1) : AVOIDS ERASING WHAT HAS BEEN DONE BY A PREVIOUS
!                      CALL BY TELEMAC-3D WHEN COUPLING WITH SISYPHE
!
!
      IF(NPLAN.GT.1) THEN
!
!     40) P0 ELEMENT ON PRISMS
!
      NDS(40,1) = NELEM*(NPLAN-1)
!     NDS(40,2) = ??????????
      NDS(40,3) = 1
      NDS(40,4) = 5
      NDS(40,5) = NELMAX*(NPLAN-1)
      NDS(40,6) = 15
!
!-----------------------------------------------------------------------
!
!     41) P1 ELEMENT ON PRISMS
!
      NDS(41,1) = NPOIN*NPLAN
!     HORIZONTAL SEGMENTS : NDS(11,2)*NPLAN
!     VERTICAL SEGMENTS   : NPOIN*(NPLAN-1)
!     CROSSED SEGMENTS    : NDS(11,2)*2*(NPLAN-1)
      NDS(41,2) = NDS(11,2)*(3*NPLAN-2)+NPOIN*(NPLAN-1)
      NDS(41,3) = 6
      NDS(41,4) = 5
      NDS(41,5) = NPMAX*NPLAN
      NDS(41,6) = 15
!
!-----------------------------------------------------------------------
!
!     50) PRISMS SPLIT IN T0 TETRAHEDRONS
!
      NDS(50,1) = NELEM*(NPLAN-1)*3
!     NDS(50,2) = ?????
      NDS(50,3) = 1
      NDS(50,4) = 4
      NDS(50,5) = NELMAX*(NPLAN-1)*3
!     NDS(50,6) = ?????
!
!-----------------------------------------------------------------------
!
!     51) PRISMS SPLIT IN T1 TETRAHEDRONS
!
      NDS(51,1) = NPOIN*NPLAN
!     HORIZONTAL SEGMENTS : NDS(11,2)*NPLAN
!     VERTICAL SEGMENTS   : NPOIN*(NPLAN-1)
!     CROSSED SEGMENTS    : NDS(11,2)*(NPLAN-1)
      NDS(51,2) = NDS(11,2)*(2*NPLAN-1)+NPOIN*(NPLAN-1)
      NDS(51,3) = 4
      NDS(51,4) = 4
      NDS(51,5) = NPMAX*NPLAN
      NDS(51,6) = 6
!
!-----------------------------------------------------------------------
!
!     60) P0 TRIANGLES ON LATERAL SIDE OF 3D PRISM MESHES
!         (PRISM MESH SPLIT IN TETRAHEDRONS)
!
      NDS(60,1) = 2*NPTFR*(NPLAN-1)
!     NDS(60,2) = ?????
      NDS(60,3) = 1
      NDS(60,4) = 3
      NDS(60,5) = 2*NPTFX*(NPLAN-1)
!     NDS(60,6) = ??????
!
!-----------------------------------------------------------------------
!
!     61) P1 TRIANGLES ON LATERAL SIDE OF 3D PRISM MESHES
!         (PRISM MESH SPLIT IN TETRAHEDRONS)
!
      NDS(61,1) = NPTFR*NPLAN
!     NDS(61,2) = ?????????
      NDS(61,3) = 3
      NDS(61,4) = 3
      NDS(61,5) = NPTFX*NPLAN
      NDS(61,6) = 3
!
!-----------------------------------------------------------------------
!
!     70) Q0 QUADRILATERALS ON LATERAL SIDE OF 3D PRISM MESHES
!
      NDS(70,1) = NPTFR*(NPLAN-1)
!     NDS(70,2) = ?????
      NDS(70,3) = 1
      NDS(70,4) = 4
      NDS(70,5) = NPTFX*(NPLAN-1)
!     NDS(70,6) = ??????
!
!-----------------------------------------------------------------------
!
!     71) Q1 QUADRILATERALS ON LATERAL SIDE OF 3D PRISM MESHES
!
      NDS(71,1) = NPTFR*NPLAN
!     NDS(71,2) = ?????????
      NDS(71,3) = 4
      NDS(71,4) = 4
      NDS(71,5) = NPTFX*NPLAN
!     NDS(71,6) = ???????????
!
!     CORRESPONDS TO : IF(NPLAN.GT.1) THEN
      ENDIF
!
!-----------------------------------------------------------------------
!
!     80) P0 BOUNDARY TRIANGLES FOR TETRAHEDRONS IN AN UNSTRUCTURED 3D
!         MESH
!
      NDS(80,1) = NELEB
!     NDS(80,2) = ?????
      NDS(80,3) = 1
      NDS(80,4) = 3
      NDS(80,5) = NELEB
!     NDS(80,6) = ??????
!
!-----------------------------------------------------------------------
!
!     81) P1 BOUNDARY TRIANGLES FOR TETRAHEDRONS IN AN UNSTRUCTURED 3D
!         MESH
!
      NDS(81,1) = NPTFR
!     A BIT DIFFERENT FROM NDS(11,2)
!     ADAPTED TO A SURFACE OF A 3D MESH
!     (WHICH IS A "BIDIMENSIONAL" SURFACE)
      NDS(81,2) = (3*NELEB)/2 + NPTFR
      NDS(81,3) = 3
      NDS(81,4) = 3
      NDS(81,5) = NPTFR
      NDS(81,6) = 3
!
!-----------------------------------------------------------------------
!
      RETURN
      END
