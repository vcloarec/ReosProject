!                   ********************
                    SUBROUTINE GETTRIEBE
!                   ********************
!
     &(XAUX,AD,AX,TETA,IKLE,NPOIN,NELEM,NELMAX,MESH,IELM3,NELEM2,NPLAN,
     & KNOLG)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    GETS THE TRIDIAGONAL PART OF A DIFFUSION MATRIX ON
!+                 PRISMS AND REMOVES IT FROM THE INITIAL MATRIX.
!code
!+            IF MTRI IS THIS TRIDIAGONAL PART, MAUX THE RESULT AND MDIF
!+            THE DIFFUSION MATRIX, THIS SUBROUTINE DOES:
!+
!+            MAUX = TETA * MTRI
!+            MDIF CHANGED INTO (1-TETA) * MDIF
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNHE)
!+        13/08/08
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
!+        29/11/2011
!+        V6P2
!+   Element 51 programmed, KNOLG added
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AD             |-->| DIAGONAL TERMS OF MATRIX
!| AX             |-->| OFF-DIAGONAL TERMS OF MATRIX
!| IELM3          |-->| TYPE OF ELEMENT
!| IKLE           |-->| CONNECTIVITY TABLE
!| KNOLG          |-->| ORIGINAL (I.E. SCALAR MODE) GLOBAL NUMBER OF POINTS
!| MESH           |-->| MESH STRUCTURE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELEM2         |-->| NUMBER OF TRIANGLES OF ORIGINAL 2D MESH
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPLAN          |-->| NUMBER OF PLANES IN THE ORIGINAL MESH
!| NPOIN          |-->| NUMBER OF POINTS
!| TETA           |-->| COEFFICIENT USED IN THE RESULT
!| XAUX           |<--| THE RESULTING MATRIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_GETTRIEBE => GETTRIEBE
      USE DECLARATIONS_TELEMAC, ONLY : TETRA,ISEGT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN,IELM3,NELEM2,NPLAN
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*),KNOLG(NPOIN)
!
      DOUBLE PRECISION, INTENT(IN)    :: TETA
      DOUBLE PRECISION, INTENT(INOUT) :: XAUX(NPOIN,*),AX(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: AD(NPOIN)
!
      TYPE(BIEF_MESH) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I1,I2,I3,I4,I5,I6,IELEM,IAN,ICOM,NPOIN2,IPLAN,K
      INTEGER S1,S2,S3,IT1,IT2,IELEM3D,L1,L2,ISEG
!
!     TETRA : WILL GIVE THE LOCAL NUMBERS OF POINTS IN THE PRISM
!             THE 0 CORRESPOND TO SITUATIONS
!             THAT NEVER HAPPEN (TETRA(1,1,1,... OR TETRA(2,2,2,...)
!             SEE ALSO CPIKLE3 AND FLUX_EF_VF_3D, WITH SIMILAR USE
!     INTEGER TETRA(2,2,2,3,4)
!     DATA TETRA / 0,1,1,1,1,1,1,0,0,4,4,4,4,4,4,0,0,6,4,5,5,4,6,0,
!    &             0,2,2,2,2,2,2,0,0,6,6,6,6,6,6,0,0,3,1,2,2,1,3,0,
!    &             0,3,3,3,3,3,3,0,0,5,5,5,5,5,5,0,0,2,3,4,1,6,5,0,
!    &             0,4,5,4,6,6,5,0,0,2,3,3,1,2,1,0,0,4,5,3,6,2,1,0 /
!     EBE SYMMETRIC STORAGE OF OFF-DIAGONAL TERMS FOR TETRAHEDRONS
!     0 ARE PUT FOR DIAGONALS
      INTEGER :: STO(4,4)
      PARAMETER ( STO = RESHAPE( (/
     &           0, 1, 2, 3,
     &           1, 0, 4, 5,
     &           2, 4, 0, 6,
     &           3, 5, 6, 0 /), SHAPE=(/ 4,4 /) ) )
!     CORRESPONDING OFF-DIAGONAL TERMS
!     DATA STO /1-1,2-1,3-1,4-1,
!    &          1-2,2-2,3-2,4-2,
!    &          1-3,2-3,3-3,4-3,
!    &          1-4,2-4,3-4,4-4/
!     INTEGER ISEGT(6,2)
!     DATA ISEGT/1,2,3,1,2,3,2,3,1,4,4,4/
!
!-----------------------------------------------------------------------
!
!     CONSIDERS HERE THAT NPOIN < NELMAX TO USE XAUX AS XAUX(NPOIN,3)
!
!     XAUX(I,1) IS COEFFICIENT OF POINT BELOW I IN EQUATION OF POINT I
!     XAUX(I,2) IS THE DIAGONAL
!     XAUX(I,3) IS COEFFICIENT OF POINT ABOVE I IN EQUATION OF POINT I
!
!-----------------------------------------------------------------------
!     INITIALISES THE DIAGONAL AND OFF-DIAGONAL TERMS
!-----------------------------------------------------------------------
!
      CALL OV('X=C     ', X=XAUX(1,1), C=0.D0, DIM1=NPOIN)
      CALL OV('X=CY    ', X=XAUX(1,2), Y=AD, C=TETA, DIM1=NPOIN)
      CALL OV('X=C     ', X=XAUX(1,3), C=0.D0, DIM1=NPOIN)
!
      CALL OV('X=CX    ',X=AD, C=1.D0-TETA, DIM1=NPOIN)
!
!-----------------------------------------------------------------------
!     ADDS TRIDIAGONAL TERMS
!-----------------------------------------------------------------------
!
      IF(IELM3.EQ.41) THEN
!
        DO IELEM=1,NELEM
!
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          I4=IKLE(IELEM,4)
          I5=IKLE(IELEM,5)
          I6=IKLE(IELEM,6)
          XAUX(I1,3)=XAUX(I1,3)+TETA*AX(IELEM,03) ! TERM 1-4
          XAUX(I2,3)=XAUX(I2,3)+TETA*AX(IELEM,08) ! TERM 2-5
          XAUX(I3,3)=XAUX(I3,3)+TETA*AX(IELEM,12) ! TERM 3-6
          XAUX(I4,1)=XAUX(I4,1)+TETA*AX(IELEM,03) ! TERM 4-1
          XAUX(I5,1)=XAUX(I5,1)+TETA*AX(IELEM,08) ! TERM 5-2
          XAUX(I6,1)=XAUX(I6,1)+TETA*AX(IELEM,12) ! TERM 6-3
!
          AX(IELEM,03)=AX(IELEM,03)*(1.D0-TETA)
          AX(IELEM,08)=AX(IELEM,08)*(1.D0-TETA)
          AX(IELEM,12)=AX(IELEM,12)*(1.D0-TETA)
!
        ENDDO
!
      ELSEIF(IELM3.EQ.51) THEN
!
        DO IPLAN=1,NPLAN-1
          DO IELEM=1,NELEM2
!
!           HERE LOWER LEVEL OF ELEMENTS ALWAYS TAKEN
!           TO FIND THE WAY THE PRISM HAS BEEN CUT BY LOOKING
!           AT GLOBAL NUMBERS OF POINTS
!           THIS PART IS THUS COMMON TO ALL PLANES
!           IKLE 3D IS TAKEN, COULD BE IKLE 2D AS WELL
            I1=IKLE(IELEM,1)
            I2=IKLE(IELEM,2)
            I3=IKLE(IELEM,3)
            IF(NCSIZE.GT.1) THEN
              I1=KNOLG(I1)
              I2=KNOLG(I2)
              I3=KNOLG(I3)
            ENDIF
!           THIS IS DONE LIKE IN CPIKLE3 TO USE ARRAY TETRA
            IF(I1.GT.I2) THEN
              S1=1
            ELSE
              S1=2
            ENDIF
            IF(I2.GT.I3) THEN
              S2=1
            ELSE
              S2=2
            ENDIF
            IF(I3.GT.I1) THEN
              S3=1
            ELSE
              S3=2
            ENDIF
!
!           NOW TAKING CONTRIBUTIONS OF TETRAHEDRON K= 1, 2 AND 3
!
            DO K=1,3
              IELEM3D=3*(IPLAN-1)*NELEM2+IELEM+(K-1)*NELEM2
!             SEGMENTS 1 TO 6
              DO ISEG=1,6
!               LOCAL NUMBERS OF 2 POINTS OF SEGMENT IN THE TETRAHEDRON
                L1=ISEGT(ISEG,1)
                L2=ISEGT(ISEG,2)
!               GLOBAL NUMBERS OF 2 POINTS OF SEGMENT
                I1=IKLE(IELEM3D,L1)
                I2=IKLE(IELEM3D,L2)
!               NUMBERS OF 2 POINTS OF SEGMENT IN THE ORIGINAL PRISM
                IT1=TETRA(S1,S2,S3,K,L1)
                IT2=TETRA(S1,S2,S3,K,L2)
!               WE LOOK FOR VERTICALS OF THE ORIGINAL PRISM
                IF(IT1.EQ.1.AND.IT2.EQ.4) THEN
                  XAUX(I1,3)=XAUX(I1,3)+TETA*AX(IELEM3D,STO(L1,L2)) ! TERM 1-4
                  XAUX(I2,1)=XAUX(I2,1)+TETA*AX(IELEM3D,STO(L1,L2)) ! TERM 4-1
                  AX(IELEM3D,STO(L1,L2))=
     &            AX(IELEM3D,STO(L1,L2))*(1.D0-TETA)
                ELSEIF(IT1.EQ.4.AND.IT2.EQ.1) THEN
                  XAUX(I1,1)=XAUX(I1,1)+TETA*AX(IELEM3D,STO(L1,L2)) ! TERM 4-1
                  XAUX(I2,3)=XAUX(I2,3)+TETA*AX(IELEM3D,STO(L1,L2)) ! TERM 1-4
                  AX(IELEM3D,STO(L1,L2))=
     &            AX(IELEM3D,STO(L1,L2))*(1.D0-TETA)
                ELSEIF(IT1.EQ.2.AND.IT2.EQ.5) THEN
                  XAUX(I1,3)=XAUX(I1,3)+TETA*AX(IELEM3D,STO(L1,L2)) ! TERM 2-5
                  XAUX(I2,1)=XAUX(I2,1)+TETA*AX(IELEM3D,STO(L1,L2)) ! TERM 5-2
                  AX(IELEM3D,STO(L1,L2))=
     &            AX(IELEM3D,STO(L1,L2))*(1.D0-TETA)
                ELSEIF(IT1.EQ.5.AND.IT2.EQ.2) THEN
                  XAUX(I1,1)=XAUX(I1,1)+TETA*AX(IELEM3D,STO(L1,L2)) ! TERM 5-2
                  XAUX(I2,3)=XAUX(I2,3)+TETA*AX(IELEM3D,STO(L1,L2)) ! TERM 2-5
                  AX(IELEM3D,STO(L1,L2))=
     &            AX(IELEM3D,STO(L1,L2))*(1.D0-TETA)
                ELSEIF(IT1.EQ.3.AND.IT2.EQ.6) THEN
                  XAUX(I1,3)=XAUX(I1,3)+TETA*AX(IELEM3D,STO(L1,L2)) ! TERM 3-6
                  XAUX(I2,1)=XAUX(I2,1)+TETA*AX(IELEM3D,STO(L1,L2)) ! TERM 6-3
                  AX(IELEM3D,STO(L1,L2))=
     &            AX(IELEM3D,STO(L1,L2))*(1.D0-TETA)
                ELSEIF(IT1.EQ.6.AND.IT2.EQ.3) THEN
                  XAUX(I1,1)=XAUX(I1,1)+TETA*AX(IELEM3D,STO(L1,L2)) ! TERM 6-3
                  XAUX(I2,3)=XAUX(I2,3)+TETA*AX(IELEM3D,STO(L1,L2)) ! TERM 3-6
                  AX(IELEM3D,STO(L1,L2))=
     &            AX(IELEM3D,STO(L1,L2))*(1.D0-TETA)
                ENDIF
              ENDDO
            ENDDO
!
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,*) 'GETTRIEBE: ELEMENT NOT IMPLEMENTED:',IELM3
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     PARALLEL MODE
!
      IF(NCSIZE.GT.1) THEN
        IAN    = 3
        ICOM   = 2
        NPOIN2 = BIEF_NBPTS(11,MESH)
        CALL PARCOM2(XAUX(1,1),XAUX(1,2),XAUX(1,3),
     &               NPOIN2,NPLAN,ICOM,IAN,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
