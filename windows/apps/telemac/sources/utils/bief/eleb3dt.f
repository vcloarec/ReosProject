!                   ******************
                    SUBROUTINE ELEB3DT
!                   ******************
!
     &(IKLE3,NBOR,NELBOR,NELBOR2D,IKLBOR,NELEB,NELEBX,NULONE,
     & NELEM2,NPOIN2,NPLAN,NETAGE,NPTFR,IKLBOR2D,NELEB2D,NELEBX2D)
!
!***********************************************************************
! BIEF   V7P0                                   19/03/2014
!***********************************************************************
!
!brief    CASE OF PRISMS SPLIT IN TETRAHEDRONS.
!+                BUILDS THE 3D MESH.
!+
!+            INPUT: 3D MESH ARRAYS FILLED BY A PRELIMINARY CALL
!+                       TO ELEBD.
!+
!+            OUTPUT: ARRAYS COMPLETE IN 3D.
!
!history  J-M HERVOUET(LNH)
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
!history  J-M HERVOUET(LNHE)
!+        12/08/2011
!+        V6P2
!+   Arguments NELEB and NELEBX added.
!
!history  J-M HERVOUET(EDF LAB, LNHE)
!+        27/01/2014
!+        V7P0
!+   In parallel, boundary elements that correspond to another subdomain
!+   are given a IKLBOR that reduces the element to a point.
!+   The previous version had IKLBOR = 0 which causes a crash when
!+   checking bounds.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        19/03/2014
!+        V7P0
!+   Boundary segments have now their own numbering, independent of
!+   boundary points numbering.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLBOR         |<--| CONNECTIVITY TABLE FOR BOUNDARY ELEMENTS
!|                |   | HERE DECLARED AS IKLBOR(NELEBX,3)
!|                |   | BUT FILLED AS IKLBOR(NELEB2D,2,NETAGE,3)
!|                |   | 2 IS THE NUMBER OF TETRAHEDRONS FORMING A
!|                |   | VERTICAL RECTANGLE IN THE BORDER.
!|                |   | 3 IS THE NUMBER OF POINTS IN EVERY TRIANGLE
!|                |   | FORMING THIS RECTANGLE
!| IKLBOR2D       |<--| CONNECTIVITY TABLE FOR BOUNDARY ELEMENTS IN 2D
!| IKLE3          |<--| CONNECTIVITY TABLE IN 3D, FOR TETRAHEDRONS
!|                |   | HERE DECLARED AS IKLE3(NELEM2,3,NETAGE,4)
!|                |   | 3 IS THE NUMBER OF TETRAHEDRONS PER PRISM
!|                |   | (TETRAHEDRONS ARE NUMBERED ACCORDINGLY)
!|                |   | 4 IS THE NUMBER OF POINTS IN A TETRAHEDRA.
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS IN 2D
!| NELBOR         |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT (FROM MESH3D).
!| NELBOR2D       |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT (FROM MESH2D).
!| NELEB          |-->| NUMBER OF BOUNDARY ELEMENTS
!| NELEB2D        |-->| NUMBER OF BOUNDARY ELEMENTS OF 2D MESH
!| NELEBX         |-->| MAXIMUM NUMBER OF BOUNDARY ELEMENTS
!|                |   | USED AS FIRST DIMENSION OF IKLBOR
!| NELEBX2D       |-->| MAXIMUM NUMBER OF BOUNDARY ELEMENTS IN 2D MESH
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NETAGE         |-->| NUMBER OF PLANES - 1
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NULONE         |-->| GOES WITH ARRAY NELBOR. NELBOR GIVES THE
!|                |   | ADJACENT ELEMENT, NULONE GIVES THE LOCAL
!|                |   | NUMBER OF THE FIRST NODE OF THE BOUNDARY EDGE
!|                |   | I.E. 1, 2 OR 3 FOR TRIANGLES.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ELEB3DT => ELEB3DT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELEM2,NPOIN2,NPLAN,NETAGE,NPTFR
      INTEGER, INTENT(IN)    :: NELEBX,NELEB2D,NELEBX2D
      INTEGER, INTENT(INOUT) :: NELEB
      INTEGER, INTENT(INOUT) :: IKLE3(NELEM2,3,NETAGE,4)
      INTEGER, INTENT(INOUT) :: IKLBOR(NELEBX,3)
      INTEGER, INTENT(IN)    :: IKLBOR2D(NELEBX2D,2),NELBOR2D(NELEBX2D)
      INTEGER, INTENT(INOUT) :: NULONE(NELEBX,3),NELBOR(NELEBX)
      INTEGER, INTENT(INOUT) :: NBOR(NPTFR*NPLAN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL OK(2)
!
      INTEGER IELEM,IPOIN,T(3),IELEB,IELEB3,IPTFR2
      INTEGER IETAGE,IPTFR,IL1,IL2,IL3,IL4,IG(2,2,3),IL(2,2,3),IPLAN
      INTEGER :: IG1,IG2,IG3,IG4,NUM1(12),NUM2(12),NUM3(12),K,L,M,N
!
      PARAMETER ( NUM1 = (/
     &    1 , 2 , 4 , 1 , 3 , 2 , 2 , 3 , 4 , 3 , 1 , 4 /) )
      PARAMETER ( NUM2 = (/
     &    2 , 4 , 1 , 3 , 2 , 1 , 3 , 4 , 2 , 1 , 4 , 3 /) )
      PARAMETER ( NUM3 = (/
     &    4 , 1 , 2 , 2 , 1 , 3 , 4 , 2 , 3 , 4 , 3 , 1 /) )
!
!***********************************************************************
!
! CONNECTIVITY TABLES FOR BOUNDARY FACES --> IKLBOR , NBOR3 ,
! CORRESPONDENCE BETWEEN LOCAL BOUNDARY NUMBERS AND 3D LOCAL NUMBERS --> NULONE
!
!     COMPLETING NBOR
!
      DO IPTFR = 1,NPTFR
        IPOIN = NBOR(IPTFR)
        DO IPLAN = 2,NPLAN
          NBOR(IPTFR +(IPLAN-1)*NPTFR)=IPOIN+(IPLAN-1)*NPOIN2
        ENDDO
      ENDDO
!
!     LATERAL BOUNDARIES :
!     FOR EACH RECTANGULAR FACE SPLIT IN TWO TRIANGLES
!     THE LOWER TRIANGLE IS NUMBER 1, THE HIGHER IS NUMBER 2
!
      DO IELEB = 1,NELEB2D
!
        IPTFR =IKLBOR2D(IELEB,1)
        IPTFR2=IKLBOR2D(IELEB,2)
!
!       TRIANGLE TOUCHING THE BOUNDARY IN 2D
        IELEM = NELBOR2D(IELEB)
!
        DO IETAGE = 1,NETAGE
!
!         3D BOUNDARY NUMBERING OF THE 4 POINTS OF THE RECTANGULAR FACE
!
          IL1 = IPTFR  + (IETAGE-1)*NPTFR
          IL2 = IPTFR2 + (IETAGE-1)*NPTFR
          IL3 = IL2 + NPTFR
          IL4 = IL1 + NPTFR
!
!         3D GLOBAL NUMBERING OF THE 4 POINTS OF THE RECTANGULAR FACE
!
          IG1 = NBOR(IPTFR)  + (IETAGE-1)*NPOIN2
          IG2 = NBOR(IPTFR2) + (IETAGE-1)*NPOIN2
          IG3 = IG2 + NPOIN2
          IG4 = IG1 + NPOIN2
!
!         NUMBERS OF THE 3 TETRAHEDRONS POSSIBLY TOUCHING THE FACE
!
          T(1) = (IETAGE-1)*3*NELEM2+IELEM
          T(2) = T(1) + NELEM2
          T(3) = T(2) + NELEM2
!
!         LOOKS FOR THE LOWER TRIANGLE (CAN BE 1-2-4 OR 1-2-3)
!
!         2 POSSIBLE FORMS OF THE LOWER TRIANGLE (GLOBAL AND BOUNDARY)
          IG(1,1,1)=IG1
          IG(1,1,2)=IG2
          IG(1,1,3)=IG4
          IG(1,2,1)=IG1
          IG(1,2,2)=IG2
          IG(1,2,3)=IG3
          IL(1,1,1)=IL1
          IL(1,1,2)=IL2
          IL(1,1,3)=IL4
          IL(1,2,1)=IL1
          IL(1,2,2)=IL2
          IL(1,2,3)=IL3
!         2 POSSIBLE FORMS OF THE HIGHER TRIANGLE (GLOBAL AND BOUNDARY)
          IG(2,1,1)=IG1
          IG(2,1,2)=IG3
          IG(2,1,3)=IG4
          IG(2,2,1)=IG2
          IG(2,2,2)=IG3
          IG(2,2,3)=IG4
          IL(2,1,1)=IL1
          IL(2,1,2)=IL3
          IL(2,1,3)=IL4
          IL(2,2,1)=IL2
          IL(2,2,2)=IL3
          IL(2,2,3)=IL4
!
          OK(1)=.FALSE.
          OK(2)=.FALSE.
!
!         K=1 LOWER TRIANGLE   K=2 HIGHER TRIANGLE
          DO K=1,2
!           2 POSSIBLE SPLITS
            DO L=1,2
!             12 WAYS FOR A TETRAHEDRON OF PRESENTING ITS FACES
              DO M=1,12
!               3 POSSIBLE TETRAHEDRONS
                DO N=1,3
                  IF(IG(K,L,1).EQ.IKLE3(IELEM,N,IETAGE,NUM1(M)).AND.
     &               IG(K,L,2).EQ.IKLE3(IELEM,N,IETAGE,NUM2(M)).AND.
     &               IG(K,L,3).EQ.IKLE3(IELEM,N,IETAGE,NUM3(M))) THEN
!                   STORAGE LIKE IKLBOR(NELEB2D,2,NETAGE,3)
                    IELEB3=(2*IETAGE+K-3)*NELEB2D+IELEB
                    IKLBOR(IELEB3,1) = IL(K,L,1)
                    IKLBOR(IELEB3,2) = IL(K,L,2)
                    IKLBOR(IELEB3,3) = IL(K,L,3)
                    NELBOR(IELEB3)   = T(N)
                    NULONE(IELEB3,1) = NUM1(M)
                    NULONE(IELEB3,2) = NUM2(M)
                    NULONE(IELEB3,3) = NUM3(M)
                    OK(K) = .TRUE.
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
          ENDDO
          IF(.NOT.OK(1).OR..NOT.OK(2)) THEN
            WRITE(LU,*) 'PB IN ELEB3DT IELEM=',IELEM,' IPTFR=',IPTFR
            CALL PLANTE(1)
            STOP
          ENDIF
!
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     NELEB IS THE 3D VALUE
!
      NELEB=2*NELEB2D*NETAGE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
