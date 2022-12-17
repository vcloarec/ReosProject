!                   *****************
                    SUBROUTINE GTSH31
!                   *****************
!
     &(SHP,ELT,IKLE,ELTCAR,NPOIN,NELMAX)
!
!***********************************************************************
! BIEF   V6P3                                   21/09/2012
!***********************************************************************
!
!brief    Gives the starting element and the barycentric coordinates of
!+        the head of characteristics, here for tetrahedra.
!
!history  J-M HERVOUET (LNHE)
!+        21/09/2012
!+        V6P3
!+        First version, inspired from gtsh41.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELT            |<--| ELEMENT CHOSEN FOR EVERY POINT
!| ELTCAR         |-->| STARTING ELEMENT FOR LINEAR AND QUADRATIC POINTS
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| SHP            |<--| BARYCENTRIC COORDINATES OF NODES IN THEIR
!|                |   | ASSOCIATED ELEMENT "ELT"
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,4),ELTCAR(NPOIN)
      INTEGER, INTENT(INOUT)          :: ELT(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(4,NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IELEM,IPLAN
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
            SHP(4,I)=0.D0
          ELSEIF(IKLE(IELEM,2).EQ.I) THEN
            SHP(1,I)=0.D0
            SHP(2,I)=1.D0
            SHP(3,I)=0.D0
            SHP(4,I)=0.D0
          ELSEIF(IKLE(IELEM,3).EQ.I) THEN
            SHP(1,I)=0.D0
            SHP(2,I)=0.D0
            SHP(3,I)=1.D0
            SHP(4,I)=0.D0
          ELSEIF(IKLE(IELEM,4).EQ.I) THEN
            SHP(1,I)=0.D0
            SHP(2,I)=0.D0
            SHP(3,I)=0.D0
            SHP(4,I)=1.D0
          ELSE
            WRITE(LU,*) 'PROBLEM IN GTSH31'
            WRITE(LU,*) 'POINT ',I,' ELEMENT ',IELEM
            WRITE(LU,*) 'NPOIN=',NPOIN
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
