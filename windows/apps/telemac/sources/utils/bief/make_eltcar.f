!                   **********************
                    SUBROUTINE MAKE_ELTCAR
!                   **********************
!
     &(ELTCAR,IFAC,
     & IKLE,NPOIN2,NELEM2,NELMAX,KNOLG,ISCORE,MESH,NPLAN,IELM)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    For every point in the mesh, gives an element that contains
!+        this point. This element must be the same in scalar and in
!+        parallel mode, ELTCAR(I)=0 means that the element is in another
!+        sub-domain.
!+        A byproduct is array IFAC, for every point it is 1, except
!+        on boundaries between sub-domains, where it is 0 for all
!+        sub-domain but 1.
!
!note     In every triangle a point is followed by another one:
!+        2 follows 1, 3 follows 2, 1 follows 3
!+        A point cannot be followed by the same point in 2 different
!+        triangles (if all triangles are counterclock-wise oriented).
!+        For every point we choose the element where the next point
!+        has the higher rank.
!+        With quadratic interpolation, the next linear point is taken
!+        2 follows 4, 3 follows 5, 1 follows 6
!+        The case of quasi-bubble is obvious and not treated here: the
!+        point is in the middle of an element, so no problem of choice
!+
!+        Choosing the element with highest number would be easier but
!+        there is so far nothing like KNOLG for elements...
!
!history  C. DENIS (SINETICS, EDF R&D), J-M HERVOUET (LNHE, EDF R&D)
!+        27/04/2012
!+        V6P2
!+
!
!history  J-M HERVOUET (LNHE, EDF R&D)
!+        14/06/2012
!+        V6P2
!+        Prisms cut into tetrahedra added, quasi-bubble completed
!
!history  J-M HERVOUET (LNHE, EDF R&D)
!+        21/09/2012
!+        V6P3
!+        Correction of IELEM3D in tetrahedra part (1st use 1st bug...)
!
!history  J-M HERVOUET (LNHE, EDF R&D)
!+        20/11/2013
!+        V7P0
!+        Call of PARCOM2I and PARCOM2I_SEG added for ISCORE, instead of
!+        copying to a double precision SCORE (the latter suppressed).
!
!history  J-M HERVOUET (LNHE, EDF LAB)
!+        10/06/2015
!+        V7P1
!+        Array IFAC built, based on ELTCAR. This is for dot product.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTCAR         |<--| ELEMENT CHOSEN FOR EVERY POINT
!| IELM           |-->| TYPE OF ELEMENT (11: TRIANGLE, 41: PRISM...)
!| IKLE           |-->| CONNECTIVITY TABLE
!| ISCORE         |<->| INTEGER WORK ARRAY
!| KNOLG          |-->| GLOBAL NUMBER OF POINTS IN ORIGINAL MESH
!| MESH           |-->| MESH STRUCTURE
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPLAN          |-->| NUMBER OF PLANES (CASE OF A 3D MESH, OR 1 IN 2D)
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2,NELEM2,NELMAX,NPLAN,IELM
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,*),KNOLG(*)
      INTEGER, INTENT(INOUT)          :: ELTCAR(*),IFAC(*)
      INTEGER, INTENT(INOUT)          :: ISCORE(*)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IELEM,N1,N2,N3,N4,N5,N6,IPLAN,NP,I3D,K,IELEM3D
!
      IF(IELM.EQ.11.OR.IELM.EQ.12.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
        NP=NPOIN2
      ELSEIF(IELM.EQ.13) THEN
        NP=NPOIN2+MESH%NSEG
      ELSE
        WRITE(LU,*) 'MAKE_ELTCAR NOT PROGRAMMED FOR IELM=',IELM
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO I=1,NP
        ISCORE(I)=0
      ENDDO
!
      IF(NCSIZE.LE.1) THEN
!
!       SIMPLE CASE: SCALAR MODE
!
        IF(IELM.EQ.11.OR.IELM.EQ.12.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
!
          DO IELEM = 1,NELEM2
            N1=IKLE(IELEM,1)
            N2=IKLE(IELEM,2)
            N3=IKLE(IELEM,3)
            IF(ISCORE(N1).LT.N2) THEN
              ISCORE(N1)=N2
              ELTCAR(N1)=IELEM
            ENDIF
            IF(ISCORE(N2).LT.N3) THEN
              ISCORE(N2)=N3
              ELTCAR(N2)=IELEM
            ENDIF
            IF(ISCORE(N3).LT.N1) THEN
              ISCORE(N3)=N1
              ELTCAR(N3)=IELEM
            ENDIF
          ENDDO
!
        ELSEIF(IELM.EQ.13) THEN
!
          DO IELEM = 1,NELEM2
            N1=IKLE(IELEM,1)
            N2=IKLE(IELEM,2)
            N3=IKLE(IELEM,3)
            N4=IKLE(IELEM,4)
            N5=IKLE(IELEM,5)
            N6=IKLE(IELEM,6)
            IF(ISCORE(N1).LT.N2) THEN
              ISCORE(N1)=N2
              ELTCAR(N1)=IELEM
            ENDIF
            IF(ISCORE(N2).LT.N3) THEN
              ISCORE(N2)=N3
              ELTCAR(N2)=IELEM
            ENDIF
            IF(ISCORE(N3).LT.N1) THEN
              ISCORE(N3)=N1
              ELTCAR(N3)=IELEM
            ENDIF
            IF(ISCORE(N4).LT.N2) THEN
              ISCORE(N4)=N2
              ELTCAR(N4)=IELEM
            ENDIF
            IF(ISCORE(N5).LT.N3) THEN
              ISCORE(N5)=N3
              ELTCAR(N5)=IELEM
            ENDIF
            IF(ISCORE(N6).LT.N1) THEN
              ISCORE(N6)=N1
              ELTCAR(N6)=IELEM
            ENDIF
          ENDDO
!
        ENDIF
!
      ELSE
!
!       NOW IN PARALLEL, FIRST LIKE IN SCALAR BUT WITH GLOBAL NUMBERS
!
        IF(IELM.EQ.11.OR.IELM.EQ.12.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
!
          DO IELEM = 1,NELEM2
            N1=IKLE(IELEM,1)
            N2=IKLE(IELEM,2)
            N3=IKLE(IELEM,3)
            IF(ISCORE(N1).LT.KNOLG(N2)) THEN
              ISCORE(N1)=KNOLG(N2)
              ELTCAR(N1)=IELEM
            ENDIF
            IF(ISCORE(N2).LT.KNOLG(N3)) THEN
              ISCORE(N2)=KNOLG(N3)
              ELTCAR(N2)=IELEM
            ENDIF
            IF(ISCORE(N3).LT.KNOLG(N1)) THEN
              ISCORE(N3)=KNOLG(N1)
              ELTCAR(N3)=IELEM
            ENDIF
          ENDDO
!
        ELSEIF(IELM.EQ.13) THEN
!
          DO IELEM = 1,NELEM2
            N1=IKLE(IELEM,1)
            N2=IKLE(IELEM,2)
            N3=IKLE(IELEM,3)
            N4=IKLE(IELEM,4)
            N5=IKLE(IELEM,5)
            N6=IKLE(IELEM,6)
            IF(ISCORE(N1).LT.KNOLG(N2)) THEN
              ISCORE(N1)=KNOLG(N2)
              ELTCAR(N1)=IELEM
            ENDIF
            IF(ISCORE(N2).LT.KNOLG(N3)) THEN
              ISCORE(N2)=KNOLG(N3)
              ELTCAR(N2)=IELEM
            ENDIF
            IF(ISCORE(N3).LT.KNOLG(N1)) THEN
              ISCORE(N3)=KNOLG(N1)
              ELTCAR(N3)=IELEM
            ENDIF
            IF(ISCORE(N4).LT.KNOLG(N2)) THEN
              ISCORE(N4)=KNOLG(N2)
              ELTCAR(N4)=IELEM
            ENDIF
            IF(ISCORE(N5).LT.KNOLG(N3)) THEN
              ISCORE(N5)=KNOLG(N3)
              ELTCAR(N5)=IELEM
            ENDIF
            IF(ISCORE(N6).LT.KNOLG(N1)) THEN
              ISCORE(N6)=KNOLG(N1)
              ELTCAR(N6)=IELEM
            ENDIF
          ENDDO
!
        ENDIF
!
!       LARGEST VALUE BETWEEN NEIGHBOURING SUB-DOMAINS TAKEN
        CALL PARCOM2I(ISCORE,ISCORE,ISCORE,NPOIN2,1,1,1,MESH)
        IF(IELM.EQ.13) THEN
          CALL PARCOM2I_SEG(ISCORE(NPOIN2+1:NP),
     &                      ISCORE(NPOIN2+1:NP),
     &                      ISCORE(NPOIN2+1:NP),
     &                      MESH%NSEG,1,1,1,MESH,1,11)
        ENDIF
!
        IF(IELM.EQ.11.OR.IELM.EQ.12.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
!
          DO IELEM = 1,NELEM2
            N1=IKLE(IELEM,1)
            N2=IKLE(IELEM,2)
            N3=IKLE(IELEM,3)
            IF(ISCORE(N1).EQ.KNOLG(N2)) THEN
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N1)=0
            ENDIF
            IF(ISCORE(N2).EQ.KNOLG(N3)) THEN
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N2)=0
            ENDIF
            IF(ISCORE(N3).EQ.KNOLG(N1)) THEN
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N3)=0
            ENDIF
          ENDDO
!
        ELSEIF(IELM.EQ.13) THEN
!
          DO IELEM = 1,NELEM2
            N1=IKLE(IELEM,1)
            N2=IKLE(IELEM,2)
            N3=IKLE(IELEM,3)
            N4=IKLE(IELEM,4)
            N5=IKLE(IELEM,5)
            N6=IKLE(IELEM,6)
            IF(ISCORE(N1).EQ.KNOLG(N2)) THEN
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N1)=0
            ENDIF
            IF(ISCORE(N2).EQ.KNOLG(N3)) THEN
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N2)=0
            ENDIF
            IF(ISCORE(N3).EQ.KNOLG(N1)) THEN
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N3)=0
            ENDIF
            IF(ISCORE(N4).EQ.KNOLG(N2)) THEN
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N4)=0
            ENDIF
            IF(ISCORE(N5).EQ.KNOLG(N3)) THEN
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N5)=0
            ENDIF
            IF(ISCORE(N6).EQ.KNOLG(N1)) THEN
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N6)=0
            ENDIF
          ENDDO
!
        ENDIF
!
!       IF A POINT HAS A BETTER ELEMENT IN ANOTHER SUB-DOMAIN
        DO I=1,NP
          IF(ISCORE(I).NE.0) THEN
            ELTCAR(I)=0
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     NOW DEDUCING IFAC
!
      IF(NCSIZE.GT.1) THEN
        DO I=1,NP
          IF(ELTCAR(I).NE.0) THEN
            IFAC(I)=1
          ELSE
            IFAC(I)=0
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     COMPLETING FOR QUASI-BUBBLE
!
      IF(IELM.EQ.12) THEN
        DO IELEM=1,NELEM2
          ELTCAR(NPOIN2+IELEM)=IELEM
        ENDDO
      ENDIF
      IF(NCSIZE.GT.1.AND.IELM.EQ.12) THEN
        DO IELEM=1,NELEM2
          IFAC(NPOIN2+IELEM)=1
        ENDDO
      ENDIF
!
!     COMPLETING FOR 3D PRISMS
!
      IF(NPLAN.GT.1) THEN
        IF(IELM.EQ.41) THEN
          DO IPLAN=2,NPLAN
            DO I=1,NPOIN2
!             ACCORDING TO POINT AND ELEMENT NUMBERING IN PRISMS
              I3D=I+(IPLAN-1)*NPOIN2
              IF(ELTCAR(I).GT.0) THEN
                ELTCAR(I3D)=ELTCAR(I)+(IPLAN-1)*NELEM2
              ELSE
                ELTCAR(I3D)=0
              ENDIF
            ENDDO
          ENDDO
        ELSEIF(IELM.EQ.51) THEN
          DO IPLAN=2,NPLAN
            DO I=1,NPOIN2
              I3D=I+(IPLAN-1)*NPOIN2
              IF(ELTCAR(I).GT.0) THEN
!               3 TETRAHEDRA POSSIBLE CANDIDATES
                DO K=1,3
!                 SEE ELEMENT NUMBERING IN PRISMS CUT INTO TETRAHEDRA
                  IELEM3D=(IPLAN-2)*3*NELEM2+(K-1)*NELEM2+ELTCAR(I)
!                 THIS MAY HIT SEVERAL TIMES AS A POINT MAY BELONG
!                 TO MORE THAN ONE TETRAHEDRON AT THIS LEVEL,
!                 THE LAST HIT IS KEPT, SAME BEHAVIOUR IN SCALAR OR
!                 PARALLEL. NOT VERY ELEGANT, BETTER IDEA ?
                  IF(IKLE(IELEM3D,1).EQ.I3D) THEN
                    ELTCAR(I3D)=IELEM3D
                  ELSEIF(IKLE(IELEM3D,2).EQ.I3D) THEN
                    ELTCAR(I3D)=IELEM3D
                  ELSEIF(IKLE(IELEM3D,3).EQ.I3D) THEN
                    ELTCAR(I3D)=IELEM3D
                  ELSEIF(IKLE(IELEM3D,4).EQ.I3D) THEN
                    ELTCAR(I3D)=IELEM3D
                  ENDIF
                ENDDO
              ELSE
                ELTCAR(I3D)=0
              ENDIF
            ENDDO
          ENDDO
        ENDIF
!
!       COMPLETING IFAC FOR PRISMS OR PRISMS CUT INTO TETRAHEDRA
!
        IF(NCSIZE.GT.1) THEN
          DO IPLAN=2,NPLAN
            DO I=1,NPOIN2
!             ACCORDING TO POINT AND ELEMENT NUMBERING IN PRISMS
              IFAC(I+(IPLAN-1)*NPOIN2)=IFAC(I)
            ENDDO
          ENDDO
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

