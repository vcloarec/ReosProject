!                   *******************
                    SUBROUTINE STOSEG31
!                   *******************
!
     &(NPOIN,NELEM,NELEB,NELMAX,IELM,MXELVS,IKLE,IKLBOR,
     & NBOR,NPTFR,
     & GLOSEG,MAXSEG,GLOSEGBOR,MAXSEGBOR,NSEG,NSEGBOR,
     & ELTSEG,ELTSEGBOR,ORISEG,ORISEGBOR,
     & KNOLG,NDS)
!
!***********************************************************************
! BIEF   V6P3                                   01/01/2013
!***********************************************************************
!
!brief    BUILDS THE DATA STRUCTURE FOR EDGE-BASED STORAGE.
!
!history  F. DECUNG (LNHE)
!+        01/01/13
!+        V6P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTSEG         |<--| SEGMENTS OF EVERY TRIANGLE
!| GLOSEG         |<--| GLOBAL NUMBERS OF POINTS OF SEGMENTS
!| IELM           |-->| 31: TETRAHEDRON
!| MXELVS         |-->| MAXIMUM NUMBER OF NEIGHBOURING ELEMENTS
!| IKLE           |-->| CONNECTIVITY TABLE
!| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| KNOLG          |-->| GLOBAL NUMBER OF A LOCAL POINT IN PARALLEL
!| MAXSEG         |<--| MAXIMUM NUMBER OF SEGMENTS (INTERNALS + EXTERNALS)
!| MAXSEGBOR      |<--| MAXIMUM NUMBER OF SEGMENTS (EXTERNALS)
!| NPOIN          |-->| NUMBER OF POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 3D
!| NSEG           |<--| NUMBER OF SEGMENTS OF THE MESH
!| NSEGBOR        |<--| NUMBER OF BORDER SEGMENTS OF THE MESH
!| ORISEG         |<--| ORIENTATION OF SEGMENTS OF EVERY TRIANGLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_STOSEG31 => STOSEG31
      USE DECLARATIONS_TELEMAC, ONLY : ISEGT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELMAX,IELM
      INTEGER, INTENT(IN)    :: NPOIN,NELEM,NELEB,MXELVS,NPTFR
      INTEGER, INTENT(IN)    :: MAXSEG,MAXSEGBOR
      INTEGER, INTENT(INOUT) :: NSEG,NSEGBOR
      INTEGER, INTENT(IN)    :: IKLE(NELMAX,4),IKLBOR(NELEB,3)
      INTEGER, INTENT(IN)    :: NBOR(NPTFR)
      INTEGER, INTENT(INOUT) :: GLOSEG(MAXSEG,2),GLOSEGBOR(MAXSEGBOR,2)
      INTEGER, INTENT(INOUT) :: ELTSEG(NELMAX,6),ELTSEGBOR(NELEB,3)
      INTEGER, INTENT(INOUT) :: ORISEG(NELMAX,6),ORISEGBOR(NELEB,3)
      INTEGER, INTENT(IN)    :: KNOLG(NPOIN)
      INTEGER, INTENT(INOUT) :: NDS(0:81,7)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NSE,NSEBOR,NSEBOR2,XSEG,COUNT
      INTEGER ISEG,JSEG
      INTEGER I,J,IKL,I1,I2,J1,J2,IG1,IG2,IK1,IK2
      INTEGER IELEM,IVOIS,IELEB,IVOISB
!
      INTEGER,DIMENSION(:,:),ALLOCATABLE ::IND_ELEM, IND_ELEB
      INTEGER :: IPOBO(NPOIN)
!
      LOGICAL FOUND, ERR
!
!
!-----------------------------------------------------------------------
!
      IF(IELM.NE.31) THEN
        WRITE(LU,501) IELM
501     FORMAT(1X,'STOSEG31 (BIEF) : UNEXPECTED ELEMENT: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     INITIALISES ELTSEG, ORISEG, GLOSEG
!
      DO IELEB = 1, NELEB
        DO ISEG = 1, 3 ! BIEF_NBSEGEL(IELM2, MESH) ie. IELM2 = 81
          ELTSEGBOR(IELEB,ISEG) = 0
          ORISEGBOR(IELEB,ISEG) = 0
        ENDDO
      ENDDO
!
      DO ISEG = 1, NSEGBOR
        GLOSEGBOR(ISEG,1) = 0
        GLOSEGBOR(ISEG,2) = 0
      ENDDO
!
      ALLOCATE(IND_ELEM(NPOIN,MXELVS+1))
      ALLOCATE(IND_ELEB(NPOIN,MXELVS+1))
!
!-----------------------------------------------------------------------
!
! IND_ELEM GIVES THE NUMBER OF ELEMENTS AROUND A NODE AND THEIR NUMBERS
! ALGO FROM MXPTEL31.f
! IDEM IND_ELEB FOR BORDER NODE
!
      DO I = 1, NPOIN
        IND_ELEM(I,1) = 0
      ENDDO
      DO I = 1, NPOIN
        IND_ELEB(I,1) = 0
        DO J = 2, MXELVS+1
          IND_ELEB(I,J) = -99
        ENDDO
      ENDDO
!
      DO I = 1, 4
        DO IELEM = 1, NELEM
          IKL = IKLE(IELEM,I)
          IND_ELEM(IKL,1)=IND_ELEM(IKL,1)+1
          IND_ELEM(IKL,IND_ELEM(IKL,1)+1)=IELEM
        ENDDO
      ENDDO
!
      DO I = 1, 3
        DO IELEB = 1, NELEB
          IKL = NBOR(IKLBOR(IELEB,I))
          IND_ELEB(IKL,1)=IND_ELEB(IKL,1)+1
          IND_ELEB(IKL,IND_ELEB(IKL,1)+1)=IELEB
        ENDDO
      ENDDO
!
      DO IELEB = 1 , NELEB
        DO ISEG = 1 , 3
          I1=NBOR(IKLBOR(IELEB,ISEGT(ISEG,1)))
          I2=NBOR(IKLBOR(IELEB,ISEGT(ISEG,2)))
          FOUND = .FALSE.
          DO I = 2, IND_ELEB(I1,1)+1
            IVOISB = IND_ELEB(I1,I)
            DO J = 2, IND_ELEB(I2,1)+1
              IF (IVOISB.NE.IND_ELEB(I2,J)) THEN
                CYCLE
              ELSE
                FOUND = .TRUE.
              ENDIF
            ENDDO
          ENDDO
          IF (.NOT. FOUND) THEN
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
! BUILDS IPOBO TO GO FROM GLOBAL NUMBERING TO LOCAL NUMBERING
!
      DO I=1,NPOIN
        IPOBO(I) = 0
      ENDDO
      DO I = 1, NPTFR
        IPOBO(NBOR(I)) = I
      ENDDO
!
!-----------------------------------------------------------------------
!
!     LOOP ON ELEMENTS FOR NUMBERING INTERNAL SEGMENTS AND FILLING:
!     GLOSEGBOR, ELTSEGBOR, ORISEGBOR
!
      NSEBOR = 0
      COUNT = 0
      DO IELEB = 1, NELEB
        !
        DO ISEG = 1, 3 ! BIEF_NBSEGEL(IELM2,
          !
          IF(ELTSEGBOR(IELEB,ISEG).EQ.0) THEN
!     BOTH NEIGHBOURING ELEBENTS ARE TREATED FOR THIS SEGMENT
            I1=NBOR(IKLBOR(IELEB,ISEGT(ISEG,1)))
            I2=NBOR(IKLBOR(IELEB,ISEGT(ISEG,2)))
!
            IF(I1.EQ.I2) THEN
            WRITE(LU,*) 'STOSEG31 : BORDER EDGE MADE OF ONLY ONE POINT'
              WRITE(LU,*) '         ELEMENT ',IELEB,' SEGMENT ',ISEG
              CALL PLANTE(1)
              STOP
            ENDIF
!
!     INTERNAL SEGMENT
            NSEBOR = NSEBOR + 1
            ELTSEGBOR(IELEB,ISEG) = NSEBOR
!
!     STORE LOCAL SEGMENT NUMBER (LOCAL BORDER NODE) BUT SORT ACCORDING TO GLOBAL NUMBER
            IF(NCSIZE.GT.1) THEN
              IG1=KNOLG(I1)
              IG2=KNOLG(I2)
            ELSE
              IG1=I1
              IG2=I2
            ENDIF
!
!     SEGMENT ORIENTED LOWER RANK TO HIGHER RANK
!     SORTING EVEN FOR BORDER SEGMENT (SLIGHT DIFFERENCE WITH STOSEG.f)
            IF(IG1.LT.IG2) THEN
              GLOSEGBOR(NSEBOR,1) = I1
              GLOSEGBOR(NSEBOR,2) = I2
              ORISEGBOR(IELEB,ISEG) = 1
            ELSE
              GLOSEGBOR(NSEBOR,1) = I2
              GLOSEGBOR(NSEBOR,2) = I1
              ORISEGBOR(IELEB,ISEG) = 2
            ENDIF
!
!     LOOKING FOR THE NEIGHBOUR ELEMENTS WHERE BOTH I1 & I2 BELONG TO
!     FIRST STEP : I1 BELONGS TO IVOIS
            FOUND = .FALSE.
            DO I = 2, IND_ELEB(I1,1)+1
              IVOISB = IND_ELEB(I1,I)
!     IVOIS <= IELEB IS ALREADY SET
              IF (IVOISB.LE.IELEB) CYCLE
!     SECOND STEP : I1 BELONGS TO IVOISB
              DO J = 2, IND_ELEB(I2,1)+1
                IF (IVOISB.EQ.IND_ELEB(I2,J)) THEN
!     GOT IT : [I1;I2] BELONGS TO IVOISB
!
                  DO JSEG= 1, 3 ! BIEF_NBSEGEL(IELM2, MESH)
!     LOOKS FOR THE RIGHT VERTICE OF ELEMENT IVOISB
                    J1=NBOR(IKLBOR(IVOISB,ISEGT(JSEG,1)))
                    J2=NBOR(IKLBOR(IVOISB,ISEGT(JSEG,2)))
!     IN STOSEG : ALL ELEMENTS HAVE A COUNTER-CLOCKWISE NUMBERING
!     HOWEVER, IT DOESN'T WORK HERE...
                    IF ( (I1.EQ.J1.AND.I2.EQ.J2) .OR.
     &                   (I1.EQ.J2.AND.I2.EQ.J1)) THEN
!
                      IF(ELTSEGBOR(IVOISB,JSEG).EQ.0) THEN
                        ELTSEGBOR(IVOISB,JSEG) = NSEBOR
                      ENDIF
!
                      IF(NCSIZE.GT.1) THEN
                        IK1=KNOLG(J1)
                        IK2=KNOLG(J2)
                      ELSE
                        IK1=J1
                        IK2=J2
                      ENDIF
!
!     SEGMENT ORIENTED LOWER RANK TO HIGHER RANK
                      IF(IK1.LT.IK2) THEN
                        ORISEGBOR(IVOISB,JSEG) = 1
                      ELSE
                        ORISEGBOR(IVOISB,JSEG) = 2
                      ENDIF
                      FOUND = .TRUE.
                    ENDIF ! IF I1.EQ.J1...
                  ENDDO   ! JSEG = 1, 6
!
!-----------------------------------------------------------------------
!
!     VERTICE NOT FOUND, THIS IS AN ERROR
                  IF (.NOT. FOUND) THEN
              WRITE(LU,*) 'STOSEG31 : WRONG MESH'
              WRITE(LU,*) '         ELEMENTS ',IELEB,' AND ',IVOISB
              WRITE(LU,*) '         LINKED BY POINTS ',I1,' AND ',I2
              WRITE(LU,*) '         BUT THESE POINTS ARE NOT AN EDGE'
              WRITE(LU,*) '         OF ELEMENT ',IVOISB
                    CALL PLANTE(1)
                    STOP
                  ENDIF   ! IF .NOT. FOUND
                ENDIF      ! IF FOUND THE GOOD NEIGHBOUR (IVOISB.EQ.IELEB...)
              ENDDO         !
            ENDDO
            !IF ( (.NOT. FOUND).AND.(NCSIZE.LE.1) ) THEN
!     PATHOLOGIC CASE (AT THIS TIME BEING AT LEAST)
!     FOUND A BORDER VERTICE WHICH IS NOT CONNECTED TO ANY OTHER ONE
            !   CALL PLANTE()
            !   STOP
            !ELSE
            COUNT = COUNT + 1
            !ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!     CHECKING (ACCORDING GLOSEG51.f)
!
!     LOOP ON TETRAHEDRA
      DO IELEB = 1, NELEB
!     LOOP ON LOCAL SEGMENTS
        DO ISEG = 1, 3
!     GLOBAL POINTS SEEN BY TRIANGLES
          J1=NBOR(IKLBOR(IELEB,ISEGT(ISEG,1)))
          J2=NBOR(IKLBOR(IELEB,ISEGT(ISEG,2)))
!     GLOBAL POINTS SEEN BY GLOSEG
          JSEG=ELTSEGBOR(IELEB,ISEG)
          I1=GLOSEGBOR(JSEG,1)
          I2=GLOSEGBOR(JSEG,2)
          IF(ORISEGBOR(IELEB,ISEG).EQ.1) THEN
            IF(J1.NE.I1.OR.J2.NE.I2) THEN
              WRITE(LU,*) ' '
              WRITE(LU,*) 'ERROR IN STOSEG31'
              WRITE(LU,*) 'ELEMENT ',IELEB,' SEGMENT ',ISEG
              WRITE(LU,*) 'POINTS ',J1,J2
              WRITE(LU,*) 'GLOBAL SEGMENT ',JSEG
              WRITE(LU,*) 'POINTS ',I1,I2
              CALL PLANTE(1)
              STOP
            ENDIF
          ELSE IF(ORISEGBOR(IELEB,ISEG).EQ.2) THEN
            IF(J1.NE.I2.OR.J2.NE.I1) THEN
              WRITE(LU,*) ' '
              WRITE(LU,*) 'ERROR IN STOSEG31'
              WRITE(LU,*) 'ELEMENT ',IELEB,' SEGMENT ',ISEG
              WRITE(LU,*) 'POINTS ',J1,J2
              WRITE(LU,*) 'GLOBAL SEGMENT ',JSEG
              WRITE(LU,*) 'POINTS ',I1,I2
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!     INITIALISES ELTSEG, ORISEG, GLOSEG
!
      DO IELEM = 1, NELEM
        DO ISEG = 1, 6 ! BIEF_NBSEGEL(IELM1, MESH) ie. IELM1 = 31
          ELTSEG(IELEM,ISEG) = 0
          ORISEG(IELEM,ISEG) = 0
        ENDDO
      ENDDO
!
      DO ISEG = 1, NSEG
        GLOSEG(ISEG,1) = 0
        GLOSEG(ISEG,2) = 0
      ENDDO
!
!     LOOP ON ELEMENTS FOR NUMBERING INTERNAL SEGMENTS AND FILLING:
!     GLOSEG, ELTSEG, ORISEG
!
      NSE     = NSEBOR
      NSEBOR2 = 0
      DO IELEM = 1, NELEM
        DO ISEG = 1, 6         ! BIEF_NBSEGEL(IELM, MESH)
          !
          FOUND = .FALSE.
          !
          IF(ELTSEG(IELEM,ISEG).EQ.0) THEN
!     BOTH NEIGHBOURING ELEMENTS ARE TREATED FOR THIS SEGMENT
            I1=IKLE(IELEM,ISEGT(ISEG,1))
            I2=IKLE(IELEM,ISEGT(ISEG,2))
!
            IF(I1.EQ.I2) THEN
              WRITE(LU,*) 'STOSEG31 : EDGE MADE OF ONLY ONE POINT'
              WRITE(LU,*) '         ELEMENT ',IELEM,' SEGMENT ',ISEG
              CALL PLANTE(1)
              STOP
            ENDIF
!
!     THIS SEGMENT IS A BORDER SEGMENT ? PATHOLOGIC CASES ?
            IF (IPOBO(I1).NE.0 .AND. IPOBO(I2).NE.0) THEN
              FOUND = .FALSE.
!     FOR DOUBLE CHECKING COUNTING OF BORDER SEGMENT
              DO I = 2, IND_ELEB(I1,1)+1
                IELEB = IND_ELEB(I1,I)
                DO J = 2, IND_ELEB(I2,1)+1
                  IF (IELEB.NE.IND_ELEB(I2,J)) CYCLE
                  DO JSEG = 1, 3
                    J1=NBOR(IKLBOR(IELEB,ISEGT(JSEG,1)))
                    J2=NBOR(IKLBOR(IELEB,ISEGT(JSEG,2)))
                    IF ( (I1.EQ.J1.AND.I2.EQ.J2)   .OR.
     &                   (I1.EQ.J2.AND.I2.EQ.J1) ) THEN
                      XSEG = ELTSEGBOR(IELEB,JSEG)
                      FOUND = .TRUE.
                      NSEBOR2 = NSEBOR2 + 1
                      FOUND = .TRUE.
                    ENDIF
                    IF (FOUND) EXIT
                  ENDDO
                ENDDO
                IF (FOUND) EXIT
              ENDDO
              IF (.NOT. FOUND) THEN
                NSE  = NSE + 1
                XSEG = NSE
              ENDIF
            ELSE
              NSE  = NSE + 1
              XSEG = NSE
            ENDIF
!
!     INTERNAL SEGMENT
            ELTSEG(IELEM,ISEG) = XSEG
!
!     STORE LOCAL SEGMENT NUMBER BUT SORT ACCORDING TO GLOBAL NUMBER
            IF(NCSIZE.GT.1) THEN
              IG1=KNOLG(I1)
              IG2=KNOLG(I2)
            ELSE
              IG1=I1
              IG2=I2
            ENDIF
!
!     SEGMENT ORIENTED LOWER RANK TO HIGHER RANK
            IF(IG1.LT.IG2) THEN
              GLOSEG(XSEG,1) = I1
              GLOSEG(XSEG,2) = I2
              ORISEG(IELEM,ISEG) = 1
            ELSE
              GLOSEG(XSEG,1) = I2
              GLOSEG(XSEG,2) = I1
              ORISEG(IELEM,ISEG) = 2
            ENDIF
!
!     LOOKING FOR THE NEIGHBOUR ELEMENTS WHERE BOTH I1 & I2 BELONG TO
!     FIRST STEP : I1 BELONGS TO IVOIS
            DO I = 2, IND_ELEM(I1,1)+1
              IVOIS = IND_ELEM(I1,I)
! IVOIS <= IELEM IS ALREADY SET
              IF (IVOIS.LE.IELEM) CYCLE
! SECOND STEP : I1 BELONGS TO IVOIS
              DO J = 2, IND_ELEM(I2,1)+1
                IF (IVOIS.EQ.IND_ELEM(I2,J)) THEN
!     GOT IT : [I1;I2] BELONGS TO IVOIS
!
                  FOUND = .FALSE.
                  DO JSEG= 1, 6
!     LOOKS FOR THE RIGHT VERTICE OF ELEMENT IVOIS
                    J1=IKLE(IVOIS,ISEGT(JSEG,1))
                    J2=IKLE(IVOIS,ISEGT(JSEG,2))
!     IN STOSEG : ALL ELEMENTS HAVE A COUNTER-CLOCKWISE NUMBERING
!     HOWEVER, IT DOESN'T WORK HERE...
                    IF ( (I1.EQ.J1.AND.I2.EQ.J2) .OR.
     &                   (I1.EQ.J2.AND.I2.EQ.J1)) THEN
!
                      ELTSEG(IVOIS,JSEG) = XSEG
!
                      IF(NCSIZE.GT.1) THEN
                        IG1=KNOLG(J1)
                        IG2=KNOLG(J2)
                      ELSE
                        IG1=J1
                        IG2=J2
                      ENDIF
!
!     SEGMENT ORIENTED LOWER RANK TO HIGHER RANK
                      IF(IG1.LT.IG2) THEN
                        ORISEG(IVOIS,JSEG) = 1
                      ELSE
                        ORISEG(IVOIS,JSEG) = 2
                      ENDIF
!
                      FOUND = .TRUE.
                      EXIT
                    ENDIF ! IF I1.EQ.J1...
                  ENDDO   ! JSEG = 1, 6
!
!-----------------------------------------------------------------------
!
!     VERTICE NOT FOUND, THIS IS AN ERROR
                  IF (.NOT. FOUND) THEN
              WRITE(LU,*) 'STOSEG31 : WRONG MESH'
              WRITE(LU,*) '         ELEMENTS ',IELEM,' AND ',IVOIS
              WRITE(LU,*) '         LINKED BY POINTS ',I1,' AND ',I2
              WRITE(LU,*) '         BUT THESE POINTS ARE NOT AN EDGE'
              WRITE(LU,*) '         OF ELEMENT ',IVOIS
            CALL PLANTE(1)
            STOP
                  ENDIF   ! IF .NOT. FOUND
                ENDIF      ! IF FOUND THE GOOD NEIGHBOUR (IVOIS.EQ.IELEM...)
              ENDDO         !
            ENDDO
          ENDIF
        ENDDO
      ENDDO
!
      DEALLOCATE (IND_ELEM)
!
!-----------------------------------------------------------------------
!
!     MEMROY CHECKS (NSEG & NSEGBOR HAVE BEEN OVER-ESTIMATED IN NDS)
!
      ERR = .FALSE.
!
!      IF(NSEG.NE.NSE) THEN
      IF(NSEG.LT.NSE) THEN
        WRITE(LU,503) NSE,NSEG
503     FORMAT(1X,'STOSEG31 (BIEF): WRONG NUMBER OF SEGMENTS : ',1I12,
     &  ' INSTEAD OF ',1I12,' ESTIMATED')
!        CALL PLANTE(1)
!        STOP
        ERR = .TRUE.
      ENDIF
!
!      IF(NSEGBOR.NE.NSEBOR) THEN
      IF(NSEGBOR.LT.NSEBOR) THEN
        WRITE(LU,505) NSEBOR,NSEGBOR
505    FORMAT(1X,'STOSEG31 (BIEF): WRONG NUMBER OF BORDER SEGMENTS : ',
     &  1I12,' INSTEAD OF ',1I12,' ESTIMATED')
!        CALL PLANTE(1)
!        STOP
        ERR = .TRUE.
      ENDIF

      IF (ERR) THEN
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     UPDATE MESH/BIEF STRUCTURE WITH PRECISE SEGMENTS NUMBERS
!     NSE CONTAINS BOTH INTERNAL AND BORDER SEGMENTS...
      NSEG    = NSE
      NSEGBOR = NSEBOR
!     UPDATE NDS WHICH OVER-ESTIMATES NSEG ?
      NDS(31,2) = NSE
      NDS(81,2) = NSEBOR
!
!-----------------------------------------------------------------------
!
!     CHECKING (ACCORDING GLOSEG51.f)
!
!     LOOP ON TETRAHEDRA
      DO IELEM = 1, NELEM
!     LOOP ON LOCAL SEGMENTS
        DO ISEG = 1, 6
!     GLOBAL POINTS SEEN BY TETRAHEDRON
          J1=IKLE(IELEM,ISEGT(ISEG,1))
          J2=IKLE(IELEM,ISEGT(ISEG,2))
!     GLOBAL POINTS SEEN BY GLOSEG
          JSEG=ELTSEG(IELEM,ISEG)
          I1=GLOSEG(JSEG,1)
          I2=GLOSEG(JSEG,2)
          IF (JSEG.LE.NSEBOR) THEN
            IG1=GLOSEGBOR(JSEG,1)
            IG2=GLOSEGBOR(JSEG,2)
          ELSE
            IG1 = 0
            IG2 = 0
          ENDIF
          IF(ORISEG(IELEM,ISEG).EQ.1) THEN
            IF(J1.NE.I1.OR.J2.NE.I2) THEN
              WRITE(LU,*) ' '
              WRITE(LU,*) 'ERROR IN STOSEG31'
              WRITE(LU,*) 'ELEMENT ',IELEM,' SEGMENT ',ISEG
              WRITE(LU,*) 'POINTS ',J1,J2
              WRITE(LU,*) 'GLOBAL SEGMENT ',JSEG
              WRITE(LU,*) 'POINTS ',I1,I2
              WRITE(LU,*) 'BORDER POINTS  ',IG1,IG2
              WRITE(LU,*) 'BORDER POINTS2 ',IPOBO(I1),IPOBO(I2)
              CALL PLANTE(1)
              STOP
            ENDIF
          ELSE
            IF(J1.NE.I2.OR.J2.NE.I1) THEN
              WRITE(LU,*) ' '
              WRITE(LU,*) 'ERROR IN STOSEG31'
              WRITE(LU,*) 'ELEMENT ',IELEM,' SEGMENT ',ISEG
              WRITE(LU,*) 'POINTS ',J1,J2
              WRITE(LU,*) 'GLOBAL SEGMENT ',JSEG
              WRITE(LU,*) 'POINTS ',I1,I2
              WRITE(LU,*) 'POINTS BORD  ',IG1,IG2
              WRITE(LU,*) 'POINTS BORD2 ',IPOBO(I1),IPOBO(I2)
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
