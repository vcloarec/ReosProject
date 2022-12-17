!                   *************************
                    SUBROUTINE READ_MESH_INFO
!                   *************************
!
     &(FFORMAT,NFIC,TITLE,NVAR,NPOIN,TYP_ELEM,NELEM,NPTFR,NPTIR,
     & NDP,NPLAN,X_ORIG,Y_ORIG,TYP_BND_ELEM,NELEBD)
!
!***********************************************************************
! HERMES   V6P3                                   21/08/2010
!***********************************************************************
!
!BRIEF    READS OR COMPUTES THE VALUES OF NPOIN, NELEM, NPTFR,
!+                MXPTVS, MXELVS IN THE GEOMETRY FILE (CHANNEL NGEO).
!
!
!history  Y AUDOUIN (LNHE)
!+        21/05/2015
!+        V7P0
!+   First version.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT      |-->| FORMAT OF THE FILE
!| TITLE        |<--| TITLE OF THE MESH
!| NVAR         |<--| NUMBER OF VARIABLES
!| NPOIN        |<--| NUMBER OF MESH NODES
!| TYP_ELEM     |<--| TYPE OF ELEMENT
!| NELEM        |<--| NUMBER OF ELEMENTS
!| NDP          |<--| NUMBER OF ELEMENT FACES
!| NPLAN        |<--| NUMBER OF PLAN
!| NPTFR        |<--| NUMBER OF BOUNDARY NODES
!| NPTIR        |<--| NUMBER OF INTERFACES
!| NFIC         |<--| FILE TO READ
!| X_ORIG       |<--| Off set of the X coordinates
!| Y_ORIG       |<--| Off set of the Y coordinates
!| TYP_BND_ELEM |<--| TYPE OF BOUNDARY ELEMENTS
!| NELEBD       |<--| NUMBER OF BOUNDARY ELEMENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES
      USE BIEF, ONLY: NCSIZE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN)   :: FFORMAT ! FILE FORMAT
      CHARACTER(LEN=80), INTENT(OUT) :: TITLE
      INTEGER, INTENT(OUT)           :: NVAR   ! NUMBER OF VARIABLES
      INTEGER, INTENT(OUT)           :: NPOIN  ! NUMBER OF MESH NODES
      INTEGER, INTENT(OUT)           :: TYP_ELEM  ! TYPE OF ELEMENT
      INTEGER, INTENT(OUT)           :: NELEM  ! NUMBER OF ELEMENTS
      INTEGER, INTENT(OUT)           :: NDP    ! NUMBER OF ELEMENT FACES
      INTEGER, INTENT(OUT)           :: NPLAN  ! NUMBER OF PLAN
      INTEGER, INTENT(OUT)           :: NPTFR  ! NUMBER OF BOUNDARY NODES
      INTEGER, INTENT(OUT)           :: NPTIR  ! NUMBER OF INTERFACES
      INTEGER, INTENT(OUT)           :: X_ORIG  ! X ORIGIN
      INTEGER, INTENT(OUT)           :: Y_ORIG  ! Y_ORIGIN
      INTEGER, INTENT(IN)            :: NFIC   ! FILE TO READ
      INTEGER,OPTIONAL,INTENT(OUT)   :: TYP_BND_ELEM ! TYPE OF BND ELEMENTS
      INTEGER,OPTIONAL,INTENT(OUT)   :: NELEBD ! NUMBER OF BOUNDARY ELEMENTS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IERR,NDIM
      INTEGER NNELEBD, TYP, TYP_BND
      INTEGER NNELEM
!
!-----------------------------------------------------------------------
!
      CALL GET_MESH_TITLE(FFORMAT,NFIC,TITLE,IERR)
      CALL CHECK_CALL(IERR,'READ_MESH_INFO:GET_MESH_TITLE')
!
      CALL GET_DATA_NVAR(FFORMAT,NFIC,NVAR,IERR)
      CALL CHECK_CALL(IERR,'READ_MESH_INFO:GET_DATA_NVAR')
!
!     IDENTIFY THE MESH ELEMENT TYPE AND READ THE NUMBER OF ELEMENT
!
      CALL GET_MESH_DIMENSION(FFORMAT,NFIC,NDIM,IERR)
      CALL CHECK_CALL(IERR,'READ_MESH_INFO:GET_MESH_DIMENSION')
      IF(NDIM.EQ.3) THEN
        ! TEST FOR TETRAEDRON
        TYP = TETRAHEDRON_ELT_TYPE
        CALL GET_MESH_NELEM(FFORMAT,NFIC,TYP,NNELEM,IERR)
        CALL CHECK_CALL(IERR,
     &                  'READ_MESH_INFO:GET_MESH_NELEM:TETRAHEDRON')
        IF(NNELEM.EQ.0) THEN
          ! TEST FOR PRISM
          TYP = PRISM_ELT_TYPE
          CALL GET_MESH_NELEM(FFORMAT,NFIC,TYP,NNELEM,IERR)
          CALL CHECK_CALL(IERR,
     &                    'READ_MESH_INFO:GET_MESH_NELEM:PRISM')
          IF(NNELEM.EQ.0) THEN
            WRITE(LU,*) 'NO 3D ELEMENTS IN A 3D MESH'
            CALL PLANTE(1)
          ENDIF
        ENDIF
      ELSE
        ! TEST FOR TRIANGLE
        TYP = TRIANGLE_ELT_TYPE
        CALL GET_MESH_NELEM(FFORMAT,NFIC,TYP,NNELEM,IERR)
        CALL CHECK_CALL(IERR,'READ_MESH_INFO:GET_MESH_NELEM:TRIANGLE')
        IF(NNELEM.EQ.0) THEN
          ! TEST FOR QUADRANGLE
          TYP = QUADRANGLE_ELT_TYPE
          CALL GET_MESH_NELEM(FFORMAT,NFIC,TYP,NNELEM,IERR)
          CALL CHECK_CALL(IERR,
     &                    'READ_MESH_INFO:GET_MESH_NELEM:QUADRANGLE')
          IF(NNELEM.EQ.0) THEN
            WRITE(LU,*) 'NO 2D ELEMENTS IN A 2D MESH'
            CALL PLANTE(1)
          ENDIF
        ENDIF
      ENDIF
      TYP_ELEM = TYP
      NELEM = NNELEM
!
!     IDENTIFY THE BOUNDARY ELEMENT TYPE AND READ THE NUMBER OF ELEMENT
!
      IF(NDIM.EQ.3) THEN
        ! TEST FOR TRIANGLE
        TYP_BND = TRIANGLE_BND_ELT_TYPE
        CALL GET_BND_NELEM(FFORMAT,NFIC,TYP_BND,NNELEBD,IERR)
        CALL CHECK_CALL(IERR,
     &                  'READ_MESH_INFO:GET_MESH_NELEM:TRIANGLE_BND')
        IF(NNELEBD.EQ.0) THEN
          WRITE(LU,*) 'NO BND ELEMENTS IN A 3D MESH'
        ENDIF
      ELSE
        ! TEST FOR EDGE
        TYP_BND = EDGE_BND_ELT_TYPE
        IF(NCSIZE.GT.1) TYP_BND = POINT_BND_ELT_TYPE
        CALL GET_BND_NELEM(FFORMAT,NFIC,TYP_BND,NNELEBD,IERR)
        CALL CHECK_CALL(IERR,
     &                  'READ_MESH_INFO:GET_MESH_NELEM:EDGE')
        IF(NNELEBD.EQ.0) THEN
          ! TEST FOR POINTS
          TYP_BND = POINT_BND_ELT_TYPE
          CALL GET_BND_NELEM(FFORMAT,NFIC,TYP_BND,NNELEBD,IERR)
          CALL CHECK_CALL(IERR,
     &                    'READ_MESH_INFO:GET_MESH_NELEM:POINT')
          IF(NNELEBD.EQ.0) THEN
            WRITE(LU,*) 'NO BND ELEMENTS IN A 2D MESH'
            ! Only stop if in serial cause the
            TYP_BND = TYPE_NULL
          ENDIF
        ENDIF
      ENDIF
      IF(PRESENT(NELEBD)) NELEBD = NNELEBD
      IF(PRESENT(TYP_BND_ELEM)) TYP_BND_ELEM = TYP_BND
!
      CALL GET_MESH_NPOIN(FFORMAT,NFIC,TYP,NPOIN,IERR)
      CALL CHECK_CALL(IERR,'READ_MESH_INFO:GET_MESH_NPOIN')
!
      CALL GET_MESH_NPOIN_PER_ELEMENT(FFORMAT,NFIC,TYP,NDP,IERR)
      CALL CHECK_CALL(IERR,'READ_MESH_INFO:GET_MESH_NPOIN_PER_ELEMENT')
!
      CALL GET_MESH_NPLAN(FFORMAT,NFIC,NPLAN,IERR)
      CALL CHECK_CALL(IERR,'READ_MESH_INFO:GET_MESH_NPLAN')
!
      ! Getting the number of boundary points
      ! If we are in parallel partel will have compute boundary on node
      IF(NCSIZE.GT.1) THEN
        TYP_BND = POINT_BND_ELT_TYPE
      ENDIF
      CALL GET_BND_NPOIN(FFORMAT,NFIC,TYP_BND,NPTFR,IERR)
      CALL CHECK_CALL(IERR,'READ_MESH_INFO:GET_MESH_NPOIN:NPTFR')
!
      ! TODO: See If nptir is actually used before leclim same for nptfr
      NPTIR = 0
      IF(NCSIZE.GT.1) THEN
        CALL GET_MESH_NPTIR(FFORMAT,NFIC,NPTIR,IERR)
        CALL CHECK_CALL(IERR,'READ_MESH_INFO:GET_MESH_NPTIR')
      ENDIF

      CALL GET_MESH_ORIG(FFORMAT,NFIC,X_ORIG,Y_ORIG,IERR)
      CALL CHECK_CALL(IERR,'READ_MESH_INFO:GET_MESH_ORIG')
!
!     IF(PRESENT(NELEBD).AND.) THEN
!       CALL GET_MESH_NELEM(FFORMAT,NFIC,TYP_BND,NELEBD,IERR)
!       CALL CHECK_CALL(IERR,'READ_MESH_INFO:GET_MESH_NELEM:BND_ELEM')
!     ENDIF
!
!  PRINTOUT FORMATS:
!
      WRITE(LU,301) TITLE(1:72)
      WRITE(LU,501) NELEM,NPOIN
      WRITE(LU,601) TYP2STR(TYP), TYP2STR(TYP_BND)
      IF(TITLE(73:80).EQ.'SERAFIN ') THEN
        WRITE(LU,*) '           SINGLE PRECISION FORMAT (R4)'
      ELSEIF(TITLE(73:80).EQ.'SERAFIND') THEN
        WRITE(LU,*) '           DOUBLE PRECISION FORMAT (R8)'
      ELSE
        WRITE(LU,*) '           FORMAT NOT INDICATED IN TITLE'
      ENDIF
      WRITE(LU,*) ' '
!
      IF(NPOIN.LT.3) THEN
        WRITE(LU,24) NPOIN
        CALL PLANTE(1)
        STOP
      ENDIF
!
24    FORMAT(1X,'READ_MESH_INFO : NUMBER OF POINTS IN THE MESH: ',
     &       1I9,/,1X,
     &          '           NUMBER OF BOUNDARY POINTS: ',1I8,/,1X,
     &          '           WRONG DATA, PROGRAMME STOPPED')
301   FORMAT(1X,//,1X,'READ_MESH_INFO: TITLE= ',A72)
501   FORMAT(12X,'NUMBER OF ELEMENTS:',1I9,/,
     &       12X,'NUMBER OF POINTS:',1I9,/)
601   FORMAT(12X,'TYPE OF ELEMENT: ',1A,/,
     &       12X,'TYPE OF BND ELEMENT: ',1A,/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
