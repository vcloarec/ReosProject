!                   *****************
                    SUBROUTINE GTSH41
!                   *****************
!
     &(SHP,SHZ,SHF,WS,FS,ELT,ETA,FRE,IKLE,ELTCAR,
     & NPOIN2,NELMAX2,NPLAN,JF,NF,YA4D)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    Gives the starting element and the barycentric coordinates of
!+        the head of characteristics.
!
!history  J-M JANIN (LNH)
!+        21/08/2008
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
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELT            |<--| 2D ELEMENT CHOSEN FOR EVERY POINT
!| ETA            |-->| LEVEL CHOSEN FOR EVERY POINT
!| ELTCAR         |-->| STARTING ELEMENT FOR LINEAR AND QUADRATIC POINTS
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELMAX2        |-->| MAXIMUM NUMBER OF 2D ELEMENTS
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN          |-->| NUMBER OF POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| QUAB           |-->| IF YES, THERE ARE QUASI-BUBBLE VARIABLES
!| QUAD           |-->| IF YES, THERE ARE QUADRATIC VARIABLES
!| SHP            |<--| BARYCENTRIC COORDINATES OF NODES IN THEIR
!|                |   | ASSOCIATED ELEMENT "ELT"
!| SHZ            |<--| BARYCENTRIC COORDINATES ON THE VERTICAL
!| WS             |-->| VERTICAL VELOCITY IN TRANSFORMED MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2,NELMAX2,NPLAN,JF,NF
      INTEGER, INTENT(IN)             :: IKLE(NELMAX2,*),ELTCAR(*)
      INTEGER, INTENT(INOUT)          :: ELT(NPOIN2,NPLAN)
      INTEGER, INTENT(INOUT)          :: ETA(NPOIN2,NPLAN)
      INTEGER, INTENT(INOUT)          :: FRE(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: SHZ(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: SHF(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: WS(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: FS(NPOIN2,NPLAN)
      LOGICAL         , INTENT(IN)    :: YA4D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IELEM,IPLAN
!
!-----------------------------------------------------------------------
!
!     POINTS IN THE BOTTOM (LIKE GTSH41)
!
      DO I=1,NPOIN2
        IELEM=ELTCAR(I)
        ELT(I,1) = IELEM
        IF(IELEM.NE.0) THEN
          IF(IKLE(IELEM,1).EQ.I) THEN
            SHP(1,I,1)=1.D0
            SHP(2,I,1)=0.D0
            SHP(3,I,1)=0.D0
          ELSEIF(IKLE(IELEM,2).EQ.I) THEN
            SHP(1,I,1)=0.D0
            SHP(2,I,1)=1.D0
            SHP(3,I,1)=0.D0
          ELSEIF(IKLE(IELEM,3).EQ.I) THEN
            SHP(1,I,1)=0.D0
            SHP(2,I,1)=0.D0
            SHP(3,I,1)=1.D0
          ELSE
            WRITE(LU,*) 'PROBLEM IN GTSH41'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ENDDO
!
!     OTHER PLANES
!
      DO IPLAN=2,NPLAN
        DO I=1,NPOIN2
          ELT(I,IPLAN) = ELT(I,1)
          SHP(1,I,IPLAN)=SHP(1,I,1)
          SHP(2,I,IPLAN)=SHP(2,I,1)
          SHP(3,I,IPLAN)=SHP(3,I,1)
        ENDDO
      ENDDO
!
!     NOW ETA AND SHZ, DONE DEPENDING ON THE VERTICAL VELOCITY
!
      DO IPLAN = 1,NPLAN
        DO I=1,NPOIN2
          IF((WS(I,IPLAN).GT.0.D0.AND.IPLAN.NE.1).OR.
     &                                              IPLAN.EQ.NPLAN) THEN
            ETA(I,IPLAN) = IPLAN-1
            SHZ(I,IPLAN) = 1.D0
          ELSE
            ETA(I,IPLAN) = IPLAN
            SHZ(I,IPLAN) = 0.D0
          ENDIF
        ENDDO
      ENDDO
!
!     NOW FRE AND SHF, DONE DEPENDING ON THE FREQUENCY VELOCITY
!
      IF(YA4D) THEN
        DO IPLAN = 1,NPLAN
          DO I=1,NPOIN2
            IF((FS(I,IPLAN).GT.0.D0.AND.JF.NE.1).OR.JF.EQ.NF) THEN
              FRE(I,IPLAN) = JF-1
              SHF(I,IPLAN) = 1.D0
            ELSE
              FRE(I,IPLAN) = JF
              SHF(I,IPLAN) = 0.D0
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
