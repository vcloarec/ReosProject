!                   *****************
                    SUBROUTINE GTSH11
!                   *****************
!
     &(SHP,ELT,IKLE,ELTCAR,NPOIN,NELEM,NELMAX,NSEG,QUAB,QUAD)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    Gives the starting element and the barycentric coordinates of
!+        the head of characteristics.
!
!note    In most cases (linear, quadratic) ELT is a mere copy of ELTCAR,
!+       but it will be changed in the computation of charactristics.
!
!history  J-M HERVOUET (LNHE)
!+        19/08/2008
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
!history  C. DENIS (SINETICS) & J-M HERVOUET (LNHE)
!+        09/05/2012
!+        V6P2
!+   New version using ELTCAR (done in MAKE_ELTCAR called by INBIEF)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELT            |<--| ELEMENT CHOSEN FOR EVERY POINT
!| ELTCAR         |-->| STARTING ELEMENT FOR LINEAR AND QUADRATIC POINTS
!|                |   | MUST HAVE THE RELEVANT SIZE.
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| QUAB           |-->| IF YES, THERE ARE QUASI-BUBBLE VARIABLES
!| QUAD           |-->| IF YES, THERE ARE QUADRATIC VARIABLES
!| SHP            |<--| BARYCENTRIC COORDINATES OF NODES IN THEIR
!|                |   | ASSOCIATED ELEMENT "ELT"
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NELEM,NELMAX,NSEG
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,*),ELTCAR(*)
!                                            NPOIN
!                                            NPOIN+NELEM
!                                            NPOIN+NSEG
      INTEGER, INTENT(INOUT)          :: ELT(*)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,*)
      LOGICAL, INTENT(IN)             :: QUAB,QUAD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IELEM
      DOUBLE PRECISION TIERS
      TIERS=1.D0/3.D0
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN
        IELEM=ELTCAR(I)
        ELT(I) = IELEM
        IF(IELEM.NE.0) THEN
          IF(IKLE(IELEM,1).EQ.I) THEN
            SHP(1,I)=1.D0
            SHP(2,I)=0.D0
            SHP(3,I)=0.D0
          ELSEIF(IKLE(IELEM,2).EQ.I) THEN
            SHP(1,I)=0.D0
            SHP(2,I)=1.D0
            SHP(3,I)=0.D0
          ELSEIF(IKLE(IELEM,3).EQ.I) THEN
            SHP(1,I)=0.D0
            SHP(2,I)=0.D0
            SHP(3,I)=1.D0
          ELSE
            WRITE(LU,*) 'PROBLEM IN GTSH11'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ENDDO
      IF(QUAB) THEN
        DO IELEM=1,NELEM
          I=NPOIN+IELEM
          ELT(I)=IELEM
          SHP(1,I)=TIERS
          SHP(2,I)=TIERS
          SHP(3,I)=TIERS
        ENDDO
      ENDIF
      IF(QUAD) THEN
        DO I=NPOIN+1,NPOIN+NSEG
          IELEM=ELTCAR(I)
          ELT(I)=IELEM
          IF(IELEM.NE.0) THEN
            IF(IKLE(IELEM,4).EQ.I) THEN
!             POINT 4
              SHP(1,I)=0.5D0
              SHP(2,I)=0.5D0
              SHP(3,I)=0.D0
            ELSEIF(IKLE(IELEM,5).EQ.I) THEN
!             POINT 5
              SHP(1,I)=0.D0
              SHP(2,I)=0.5D0
              SHP(3,I)=0.5D0
            ELSEIF(IKLE(IELEM,6).EQ.I) THEN
!             POINT 6
              SHP(1,I)=0.5D0
              SHP(2,I)=0.D0
              SHP(3,I)=0.5D0
            ELSE
              WRITE(LU,*) 'PROBLEM IN GTSH11, QUADRATIC CASE'
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
