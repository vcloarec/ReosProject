!                   ******************
                    SUBROUTINE CPIKLE3
!                   ******************
!
     &(IKLE3,IKLES,NELEM2,NELMAX2,NPOIN2,NPLAN,KNOLG)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    EXTENDS THE CONNECTIVITY TABLE.
!+
!+            BUILDS HERE THE CONNECTIVITY FOR A MESH OF PRISMS
!+                SPLIT IN TETRAHEDRONS.
!code
!+     DIFFERENT WAYS OF SPLITTING PRISMS :
!+
!+     TO ENSURE MATCHING OF TETRAHEDRONS, FACES OF TRIANGLES ARE "SIGNED"
!+     WITH 1 OR 2 DEPENDING OF THE GLOBAL NUMBERS OF THEIR POINTS, TAKEN IN
!+     COUNTER-CLOCKWISE DIRECTION. A FACE 1 IN A TRIANGLE WILL BE 2 IN ITS
!+     NEIGHBOUR AND THIS IS USED TO HAVE A CORRECT SPLITTING. THE SPLITTING
!+     DEPENDING ON THE "SIGNS" OF THE 3 FACES IS GIVEN IN ARRAY TETRA.
!+
!+
!+     TETRA(2,2,2,3,4)
!+
!+     FIRST 3 DIMENSIONS : TYPE OF FACE
!+                      1 : CUT RECTANGLE BETWEEN  LOW-LEFT AND HIGH-RIGHT
!+                      2 : CUT RECTANGLE BETWEEN  HIGH-LEFT AND LOW-RIGHT
!+
!+     4TH DIMENSION : NUMBER OF TETRAHEDRON
!+     5TH DIMENSION : 4 POINTS OF THE TETRAHEDRON (IN LOCAL PRISM NUMBERING)
!+
!+     1 1 2 SPLITTING
!+
!+     TETRA(1,1,2,1,1)= 1
!+     TETRA(1,1,2,1,2)= 2
!+     TETRA(1,1,2,1,3)= 3
!+     TETRA(1,1,2,1,4)= 6
!+
!+     TETRA(1,1,2,2,1)= 4
!+     TETRA(1,1,2,2,2)= 6
!+     TETRA(1,1,2,2,3)= 5
!+     TETRA(1,1,2,2,4)= 1
!+
!+     TETRA(1,1,2,3,1)= 5
!+     TETRA(1,1,2,3,2)= 2
!+     TETRA(1,1,2,3,3)= 1
!+     TETRA(1,1,2,3,4)= 6
!+
!+     2 1 1 SPLITTING
!+
!+     TETRA(2,1,1,1,1)= 1
!+     TETRA(2,1,1,1,2)= 2
!+     TETRA(2,1,1,1,3)= 3
!+     TETRA(2,1,1,1,4)= 4
!+
!+     TETRA(2,1,1,2,1)= 4
!+     TETRA(2,1,1,2,2)= 6
!+     TETRA(2,1,1,2,3)= 5
!+     TETRA(2,1,1,2,4)= 2
!+
!+     TETRA(2,1,1,3,1)= 6
!+     TETRA(2,1,1,3,2)= 3
!+     TETRA(2,1,1,3,3)= 2
!+     TETRA(2,1,1,3,4)= 4
!+
!+     1 2 1 SPLITTING
!+
!+     TETRA(1,2,1,1,1)= 1
!+     TETRA(1,2,1,1,2)= 2
!+     TETRA(1,2,1,1,3)= 3
!+     TETRA(1,2,1,1,4)= 5
!+
!+     TETRA(1,2,1,2,1)= 4
!+     TETRA(1,2,1,2,2)= 6
!+     TETRA(1,2,1,2,3)= 5
!+     TETRA(1,2,1,2,4)= 3
!+
!+     TETRA(1,2,1,3,1)= 4
!+     TETRA(1,2,1,3,2)= 1
!+     TETRA(1,2,1,3,3)= 3
!+     TETRA(1,2,1,3,4)= 5
!+
!+     2 2 1 SPLITTING
!+
!+     TETRA(2,2,1,1,1)= 1
!+     TETRA(2,2,1,1,2)= 2
!+     TETRA(2,2,1,1,3)= 3
!+     TETRA(2,2,1,1,4)= 4
!+
!+     TETRA(2,2,1,2,1)= 4
!+     TETRA(2,2,1,2,2)= 6
!+     TETRA(2,2,1,2,3)= 5
!+     TETRA(2,2,1,2,4)= 3
!+
!+     TETRA(2,2,1,3,1)= 5
!+     TETRA(2,2,1,3,2)= 2
!+     TETRA(2,2,1,3,3)= 4
!+     TETRA(2,2,1,3,4)= 3
!+
!+     1 2 2 SPLITTING
!+
!+     TETRA(1,2,2,1,1)= 1
!+     TETRA(1,2,2,1,2)= 2
!+     TETRA(1,2,2,1,3)= 3
!+     TETRA(1,2,2,1,4)= 5
!+
!+     TETRA(1,2,2,2,1)= 4
!+     TETRA(1,2,2,2,2)= 6
!+     TETRA(1,2,2,2,3)= 5
!+     TETRA(1,2,2,2,4)= 1
!+
!+     TETRA(1,2,2,3,1)= 6
!+     TETRA(1,2,2,3,2)= 3
!+     TETRA(1,2,2,3,3)= 5
!+     TETRA(1,2,2,3,4)= 1
!+
!+     2 1 2 SPLITTING
!+
!+     TETRA(2,1,2,1,1)= 1
!+     TETRA(2,1,2,1,2)= 2
!+     TETRA(2,1,2,1,3)= 3
!+     TETRA(2,1,2,1,4)= 6
!+
!+     TETRA(2,1,2,2,1)= 4
!+     TETRA(2,1,2,2,2)= 6
!+     TETRA(2,1,2,2,3)= 5
!+     TETRA(2,1,2,2,4)= 2
!+
!+     TETRA(2,1,2,3,1)= 4
!+     TETRA(2,1,2,3,2)= 1
!+     TETRA(2,1,2,3,3)= 6
!+     TETRA(2,1,2,3,4)= 2
!
!note     IMPORTANT : ON EACH LAYER THE BOTTOM TETRAHEDRONS MUST BE
!+                     TREATED FIRST, SO THAT IKLE SENT TO SUBROUTINE
!+                     VOISIN BE THE SAME AS WITH PRISMS OR TRIANGLES
!+                     FOR THE NELEM2 FIRST ELEMENTS.
!+                     CONSEQUENTLY THE FIRSt 3 POINTS OF TETRAHEDRON 1
!+                     ARE ALWAYS 1,2 AND 3.
!
!history  J-M HERVOUET (LNH)
!+        23/08/99
!+        V5P3
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
!history  J-M HERVOUET (LNH)
!+        28/11/2011
!+        V6P2
!+   Use of KNOLG in case of parallelism, to get the same splitting of
!+   than in scalar mode. Loops on planes and elements swapped.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLE3          |<->| 3D CONNECTIVITY TABLE
!| IKLES          |-->| 2D CONNECTIVITY TABLE WITH DIMENSION (3,NELEM2)
!| KNOLG          |-->| GIVES THE ORIGINAL GLOBAL NUMBER OF POINTS
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NELMAX2        |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, ONLY : NCSIZE
      USE DECLARATIONS_TELEMAC, ONLY : TETRA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELEM2,NELMAX2,NPOIN2,NPLAN
!                                     NPOIN3 BUT ONLY FILLED TO NPOIN2
      INTEGER, INTENT(IN)    :: KNOLG(*)
      INTEGER, INTENT(INOUT) :: IKLES(3,NELEM2)
      INTEGER, INTENT(INOUT) :: IKLE3(NELMAX2,3,NPLAN-1,4)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I,K,L,IGLOB(6),S1,S2,S3,I1,I2,I3
!
!     TETRA : SEE EXPLANATIONS ABOVE, THE 0 CORRESPOND TO SITUATIONS
!             THAT NEVER HAPPEN (TETRA(1,1,1,... OR TETRA(2,2,2,...)
!     INTEGER TETRA(2,2,2,3,4)
!     DATA TETRA / 0,1,1,1,1,1,1,0,0,4,4,4,4,4,4,0,0,6,4,5,5,4,6,0,
!    &             0,2,2,2,2,2,2,0,0,6,6,6,6,6,6,0,0,3,1,2,2,1,3,0,
!    &             0,3,3,3,3,3,3,0,0,5,5,5,5,5,5,0,0,2,3,4,1,6,5,0,
!    &             0,4,5,4,6,6,5,0,0,2,3,3,1,2,1,0,0,4,5,3,6,2,1,0 /
!
!-----------------------------------------------------------------------
!
!     BOTTOM AND TOP OF ALL LAYERS
!
      IF(NPLAN.GE.2) THEN
!
!       LOOP ON THE TRIANGLES
!
        DO IELEM = 1,NELEM2
!
          I1=IKLES(1,IELEM)
          I2=IKLES(2,IELEM)
          I3=IKLES(3,IELEM)
!
!         IN PARALLEL, IT IS NOT SURE THAT THE MESH PARTITIONER KEEPS
!         THE SAME RANK BETWEEN POINTS, SO WE USE HERE THE ORIGINAL
!         GLOBAL NUMBERS
!
          IF(NCSIZE.GT.1) THEN
            I1=KNOLG(I1)
            I2=KNOLG(I2)
            I3=KNOLG(I3)
          ENDIF
!
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
!         LOOP ON THE PLANES
!
          DO I = 1,NPLAN-1
!
!           GLOBAL NUMBERS OF THE 6 POINTS OF THE PRISM
!
            IGLOB(1) = IKLES(1,IELEM) + (I-1)*NPOIN2
            IGLOB(2) = IKLES(2,IELEM) + (I-1)*NPOIN2
            IGLOB(3) = IKLES(3,IELEM) + (I-1)*NPOIN2
            IGLOB(4) = IKLES(1,IELEM) +  I   *NPOIN2
            IGLOB(5) = IKLES(2,IELEM) +  I   *NPOIN2
            IGLOB(6) = IKLES(3,IELEM) +  I   *NPOIN2
!
            DO K=1,3
            DO L=1,4
              IKLE3(IELEM,K,I,L) = IGLOB(TETRA(S1,S2,S3,K,L))
            ENDDO
            ENDDO
!
          ENDDO
        ENDDO
      ELSE
        WRITE(LU,*) 'CPIKLE3 : MINIMUM OF 2 PLANES NEEDED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
