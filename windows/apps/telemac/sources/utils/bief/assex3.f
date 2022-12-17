!                   *****************
                    SUBROUTINE ASSEX3
!                   *****************
!
     &(XM,STO,NAME,IELM1,IELM2,TYPEXT,XMT,DIM1XMT,DIM2XMT,STOXMT,
     & MESH,NELMAX,ELTSEG,ORISEG)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    ASSEMBLES MATRICES EXTRA-DIAGONAL TERMS
!+                IN THE CASE OF EDGE-BASED STORAGE.
!
!history  J-M HERVOUET (LNHE)
!+        05/02/2010
!+        V6P0
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
!+   Tetrahedron element added
!
!history  S. PAVAN (LNHE)
!+        01/08/2013
!+        V6P3
!+   Arguments added to AS3_1111_Q
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIM1XMT        |-->| FIRST DIMENSION OF XMT
!| DIM2XMT        |-->| SECOND DIMENSION OF XMT
!| ELTSEG         |-->| SEGMENTS IN AN ELEMENT
!| IELM1          |-->| ELEMENT OF LINES IN THE MATRIX
!| IELM2          |-->| ELEMENT OF ROWS IN THE MATRIX
!| MESH           |-->| MESH-STRUCTURE
!| NAME           |-->| FORTRAN NAME OF THE MATRIX
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN THE MESH
!| ORISEG         |-->| ORIENTATION OF SEGMENTS
!| STO            |-->| STORAGE REQUIRED IN XM 1: EBE  3: EDGE-BASED
!| STOXMT         |-->| STORAGE OF OFF-DIAGONAL TERMS
!|                |   | 1: XMT(NELMAX,*)  2: XMT(*,NELMAX)
!| TYPEXT         |-->| TYPE OF OFF-DIAGONAL TERMS
!| XM             |<->| ASSEMBLED OFF-DIAGONAL TERMS
!| XMT            |<->| OFF-DIAGONAL TERMS OF THE WORK MATRIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ASSEX3 => ASSEX3
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(INOUT) :: STO
      CHARACTER(LEN=6), INTENT(IN)    :: NAME
      INTEGER         , INTENT(IN)    :: IELM1,IELM2,NELMAX
      INTEGER         , INTENT(IN)    :: DIM1XMT,DIM2XMT,STOXMT
      INTEGER         , INTENT(IN)    :: ELTSEG(NELMAX,*)
      INTEGER         , INTENT(IN)    :: ORISEG(NELMAX,*)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT
      DOUBLE PRECISION, INTENT(INOUT) :: XMT(DIM1XMT,DIM2XMT)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(*)
      TYPE(BIEF_MESH) , INTENT(IN)    :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NELEM,STOM
!
!-----------------------------------------------------------------------
!
!  EXTRACTS MATRIX M CHARACTERISTICS
!
      STOM = STO
      IF(STOM.NE.1) THEN
        WRITE(LU,501) NAME,STOM
501     FORMAT(1X,'ASSEX3 (BIEF) : MATRIX  M (REAL NAME:',A6,')',/,1X,
     &            '                UNEXPECTED STORAGE: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF( (DIMENS(IELM1).NE.MESH%DIM1) .AND.
     &   (IELM1.NE.81.AND.IELM2.NE.81) ) THEN
!       BOUNDARY MATRIX : NOT TREATED HERE
        WRITE(LU,101) NAME
        WRITE(LU,201) IELM1,IELM2
        WRITE(LU,301)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(DIMENS(IELM1).EQ.MESH%DIM1) THEN
!       NORMAL MATRIX
        NELEM  = MESH%NELEM
      ELSE
!       BOUNDARY MATRIX
        NELEM  = MESH%NELEB
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(IELM1.EQ.11.AND.IELM2.EQ.11) THEN
!
!       P1-P1 TRIANGLES MATRIX
!
        IF(TYPEXT.EQ.'S') THEN
          CALL AS3_1111_S(XM,BIEF_NBSEG(11,MESH),
     &                    XMT,NELMAX,NELEM,
     &                    ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3))
        ELSEIF(TYPEXT.EQ.'Q') THEN
          CALL AS3_1111_Q(XM,BIEF_NBSEG(11,MESH),
     &                    XMT,DIM1XMT,DIM2XMT,NELMAX,NELEM,STOXMT,
     &                    ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3),
     &                    ORISEG(1,1),ORISEG(1,2),ORISEG(1,3))
        ENDIF
!
      ELSEIF(IELM1.EQ.11.AND.IELM2.EQ.12) THEN
!
!       P1-QB TRIANGLES MATRIX
!
          CALL AS3_1112(XM,BIEF_NBSEG(IELM1,MESH),
     &                  BIEF_NBSEG(IELM2,MESH),
     &                  XMT,NELMAX,NELEM,
     &                  ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3),
     &                  ELTSEG(1,4),ELTSEG(1,5),ELTSEG(1,6),
     &                  ORISEG(1,1),ORISEG(1,2),ORISEG(1,3))
!
      ELSEIF(IELM1.EQ.11.AND.IELM2.EQ.13) THEN
!
!       P1-QUADRATIC TRIANGLES MATRIX
!
          CALL AS3_1113(XM,BIEF_NBSEG(IELM1,MESH),
     &                  BIEF_NBSEG(IELM2,MESH),
     &                  XMT,NELMAX,NELEM,ELTSEG,ORISEG)
!
      ELSEIF(IELM1.EQ.13.AND.IELM2.EQ.11) THEN
!
!       QUADRATIC-P1 TRIANGLES MATRIX
!
          CALL AS3_1311(XM,BIEF_NBSEG(IELM2,MESH),
     &                  BIEF_NBSEG(IELM1,MESH),
     &                  XMT,NELMAX,NELEM,ELTSEG,ORISEG)
!
      ELSEIF(IELM1.EQ.12.AND.IELM2.EQ.11) THEN
!
!       P1-QB TRIANGLES MATRIX
!
          CALL AS3_1211(XM,BIEF_NBSEG(11,MESH),
     &                  BIEF_NBSEG(12,MESH),
     &                  XMT,NELMAX,NELEM,
     &                  ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3),
     &                  ELTSEG(1,4),ELTSEG(1,5),ELTSEG(1,6),
     &                  ORISEG(1,1),ORISEG(1,2),ORISEG(1,3))
!
      ELSEIF(IELM1.EQ.12.AND.IELM2.EQ.12) THEN
!
!       QB-QB TRIANGLES MATRIX
!
        IF(TYPEXT.EQ.'S') THEN
          CALL AS3_1212_S(XM,BIEF_NBSEG(11,MESH),
     &                    BIEF_NBSEG(12,MESH),XMT,NELMAX,NELEM,
     &                    ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3),
     &                    ELTSEG(1,4),ELTSEG(1,5),ELTSEG(1,6))
        ELSEIF(TYPEXT.EQ.'Q') THEN
          CALL AS3_1212_Q(XM,BIEF_NBSEG(11,MESH),
     &                    BIEF_NBSEG(12,MESH),
     &                    XMT,NELMAX,NELEM,
     &                    ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3),
     &                    ELTSEG(1,4),ELTSEG(1,5),ELTSEG(1,6),
     &                    ORISEG(1,1),ORISEG(1,2),ORISEG(1,3))
        ENDIF
!
      ELSEIF(IELM1.EQ.13.AND.IELM2.EQ.13) THEN
!
!       QUADRATIC TRIANGLES MATRIX
!
        IF(TYPEXT.EQ.'S') THEN
          CALL AS3_1313_S(XM,BIEF_NBSEG(IELM1,MESH),
     &                    XMT,DIM1XMT,DIM2XMT,STOXMT,
     &                    NELMAX,NELEM,ELTSEG)
        ELSEIF(TYPEXT.EQ.'Q') THEN
          CALL AS3_1313_Q(XM,BIEF_NBSEG(IELM1,MESH),
     &                    XMT,DIM1XMT,DIM2XMT,STOXMT,
     &                    NELMAX,NELEM,ELTSEG,ORISEG)
        ENDIF
!
      ELSEIF(IELM1.EQ.41.AND.IELM2.EQ.41) THEN
!
!       PRISMS MATRIX
!
        IF(TYPEXT.EQ.'S') THEN
          CALL AS3_4141_S(XM,BIEF_NBSEG(IELM1,MESH),
     &                    XMT,DIM1XMT,DIM2XMT,STOXMT,
     &                    NELMAX,NELEM,ELTSEG)
        ELSEIF(TYPEXT.EQ.'Q') THEN
          CALL AS3_4141_Q(XM,BIEF_NBSEG(IELM1,MESH),
     &                    XMT,DIM1XMT,DIM2XMT,STOXMT,
     &                    NELMAX,NELEM,ELTSEG,ORISEG)
        ENDIF
!
      ELSEIF( (IELM1.EQ.31.AND.IELM2.EQ.31).OR.
     &        (IELM1.EQ.51.AND.IELM2.EQ.51)     ) THEN
!
!       TETRAHEDRONS MATRIX
!
        IF(TYPEXT.EQ.'S') THEN
          CALL AS3_3131_S(XM,BIEF_NBSEG(IELM1,MESH),
     &                    XMT,DIM1XMT,DIM2XMT,STOXMT,
     &                    NELMAX,NELEM,
     &                    ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3),
     &                    ELTSEG(1,4),ELTSEG(1,5),ELTSEG(1,6))
        ELSEIF(TYPEXT.EQ.'Q') THEN
          CALL AS3_3131_Q(XM,BIEF_NBSEG(IELM1,MESH),
     &                    XMT,DIM1XMT,DIM2XMT,STOXMT,
     &                    NELMAX,NELEM,
     &                    ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3),
     &                    ELTSEG(1,4),ELTSEG(1,5),ELTSEG(1,6),
     &                    ORISEG(1,1),ORISEG(1,2),ORISEG(1,3),
     &                    ORISEG(1,4),ORISEG(1,5),ORISEG(1,6))
        ENDIF
!
      ELSEIF(IELM1.EQ.81.AND.IELM2.EQ.81) THEN
!
!       TETRAHEDRONS MATRIX
!
        IF(TYPEXT.EQ.'S') THEN
          CALL AS3_8181_S(XM,BIEF_NBSEG(IELM1,MESH),
     &                    XMT,DIM1XMT,DIM2XMT,STOXMT,
     &                    NELMAX,NELEM,
     &                    ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3))
        ELSEIF(TYPEXT.EQ.'Q') THEN
          CALL AS3_8181_Q(XM,BIEF_NBSEG(IELM1,MESH),
     &                    XMT,DIM1XMT,DIM2XMT,STOXMT,
     &                    NELMAX,NELEM,
     &                    ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3),
     &                    ORISEG(1,1),ORISEG(1,2),ORISEG(1,3))
        ENDIF
!
      ELSE
!
!       IELM1 / IELM2 COMBINATION NOT IMPLEMENTED: ERROR
!
        WRITE(LU,101) NAME
        WRITE(LU,201) IELM1,IELM2
        WRITE(LU,301)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  NEW TYPE OF STORAGE
!
      STO=3
!
!-----------------------------------------------------------------------
!
101   FORMAT(1X,'ASSEX3 (BIEF) : MATRIX  M (REAL NAME:',A6,')')
201   FORMAT(1X,'                IELM1 = ',1I6,' IELM2 = ',1I6)
301   FORMAT(1X,'                THIS CASE IS NOT IMPLEMENTED')
!
!-----------------------------------------------------------------------
!
      RETURN
      END

