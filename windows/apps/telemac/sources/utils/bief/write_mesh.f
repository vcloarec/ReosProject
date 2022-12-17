!                   *********************
                    SUBROUTINE WRITE_MESH
!                   *********************
!
     &(FFORMAT,NFILE,MESH,NPLAN,DATE,TIME,T1,T2,
     & PARALL,NPTIR,NGEO,GEOFORMAT,LATLONG)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    WRITES THE MESH, DESCRIBED BY THE BIEF_MESH STRUCTURE
!+        INTO THE FILE. BIEF_MESH STRUCTURE CONTAINS INFORMATIONS
!+        ABOUT CONNECTIVITY, COORDINATES, BOUNDARY NODES. OTHER
!+        INFORMATIONS NEEDED : THE DATE AND TIME INFORMATION, AND
!+        THE ORIGIN OF THE COORDINATE SYSTEM (X_ORIG,Y_ORIG).
!
!history  R NEBAUER (LNHE)
!+        25/11/08
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
!history  U.H.Merkel
!+        21/07/2012
!+        V6P2
!+   Changed to work with NAG
!
!history  Y AUDOUIN
!+        21/05/2015
!+        V7P0
!+   Adapt code to work with the hermes module
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        26/06/2015
!+        V7P1
!+   Deallocate of IPOBO must always be done.
!
!history  R. ATA (EDF LAB, LNHE)
!+        03/08/2017
!+        V7P3
!+   Add option that allows to give results in long/lat
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FILE FORMAT
!| NFILE          |-->| LOGICAL UNIT OF FILE
!| MESH           |-->| MESH STRUCTURE
!| NPLAN          |-->| NUMBER OF PLANES (3D)
!| DATE           |-->| 3 INTEGERS (YEAR, MONTH, DAY)
!| TIME           |-->| 3 INTEGERS (HOUR, MINUTE, SECOND)
!| PARALL         !-->! If True the file we are writing is a
!|                |   | partitionned file
!| NPTIR          !-->! Number of interfaces (only for partitionned file)
!| NGEO           |-->| ID of the geometry file if given group
!|                |   | informations are tranfered from it to nfile
!| GEOFORMAT      |-->| Format of the geometry file
!| LATLONG        |-->| LAT-LONG coordinate
!| T1,T2          |<->| Working arrays
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY :BIEF_MESH, BIEF_OBJ
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8),          INTENT(IN   ) :: FFORMAT
      INTEGER        ,           INTENT(IN   ) :: NFILE,NPLAN
      TYPE(BIEF_MESH),           INTENT(IN   ) :: MESH
      TYPE(BIEF_OBJ ),           INTENT(INOUT):: T1,T2
      INTEGER, DIMENSION(3),     INTENT(IN   ) :: DATE
      INTEGER, DIMENSION(3),     INTENT(IN   ) :: TIME
      LOGICAL,                   INTENT(IN   ) :: PARALL
      INTEGER,                   INTENT(IN   ) :: NPTIR
      INTEGER         ,OPTIONAL, INTENT(IN   ) :: NGEO
      CHARACTER(LEN=8),OPTIONAL, INTENT(IN   ) :: GEOFORMAT
      LOGICAL         ,OPTIONAL, INTENT(IN   ) :: LATLONG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: IERR, I, NDP,NDIM,NPOIN,IPLAN
      INTEGER, ALLOCATABLE :: IPOBO(:)
      INTEGER, ALLOCATABLE :: IKLE_BND(:)
      INTEGER :: NELEBD
      LOGICAL :: YESLL
!
!-----------------------------------------------------------------------
!
      YESLL=.FALSE.
      IF(PRESENT(LATLONG))YESLL=LATLONG
!
!     BUILDING IPOBO ONLY IN SERIAL RUN
      IF(.NOT.PARALL) THEN
        ALLOCATE(IPOBO(MESH%NPOIN),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'IPOBO')
        DO I=1,MESH%NPOIN
          IPOBO(I) = 0
        END DO
        DO I=1,MESH%NPTFR
          IPOBO(MESH%NBOR%I(I)) = I
        END DO
      ELSE
        ! In case if nptir = 0 because then ipobo is written instead of knolg
        ALLOCATE(IPOBO(MESH%NPOIN),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'IPOBO')
        DO I=1,MESH%NPOIN
          IPOBO(I) = MESH%KNOLG%I(I)
        END DO
      ENDIF
!
      CALL CHECK_ALLOCATE(IERR,'IPOBO')
!
      NDP = MESH%NDS(MESH%TYPELM+1,3)
!     New option to write results with LAT-LONG coordinates
!
      IF(YESLL.AND.PRESENT(GEOFORMAT).AND.PRESENT(NGEO))THEN
        NDIM=MESH%DIM1
        NPOIN=MESH%NPOIN/NPLAN
        CALL CPSTVC(MESH%X,T1)
        CALL CPSTVC(MESH%X,T2)
!       GET X COORDINATE
        CALL GET_MESH_COORD(GEOFORMAT,NGEO,1,NDIM,NPOIN,T1%R,IERR)
        IF(IERR.NE.0) THEN
          WRITE(LU,*) 'WRITE_MESH : ERROR WHILE READING X ARRAY'
          CALL PLANTE(1)
        ENDIF
!       GET Y COORDINATE
        CALL GET_MESH_COORD(GEOFORMAT,NGEO,2,NDIM,NPOIN,T2%R,IERR)
        IF(IERR.NE.0) THEN
          WRITE(LU,*) 'WRITE_MESH : ERROR WHILE READING Y ARRAY'
          CALL PLANTE(1)
        ENDIF
!       CALL OF CORXY
        CALL CORRXY(T1%R,T2%R,NPOIN)
!
        IF(MESH%DIM1.EQ.3) THEN
          DO IPLAN=2,NPLAN
            DO I=1,NPOIN
              T1%R(I+(IPLAN-1)*NPOIN) = T1%R(I)
              T2%R(I+(IPLAN-1)*NPOIN) = T2%R(I)
            ENDDO
          ENDDO
!
          CALL SET_MESH(FFORMAT,NFILE,MESH%DIM1,MESH%TYPELM,NDP,
     &                  MESH%NPTFR,NPTIR,MESH%NELEM,MESH%NPOIN,
     &                 MESH%IKLE%I,IPOBO,MESH%KNOLG%I,T1%R,T2%R,
     &                  NPLAN,DATE,TIME,MESH%X_ORIG,MESH%Y_ORIG,
     &                  IERR,Z=MESH%Z%R)
          CALL CHECK_CALL(IERR,'WRITE_MESH:SET_MESH')
        ELSE
          CALL SET_MESH(FFORMAT,NFILE,MESH%DIM1,MESH%TYPELM,NDP,
     &                  MESH%NPTFR,NPTIR,MESH%NELEM,MESH%NPOIN,
     &                 MESH%IKLE%I,IPOBO,MESH%KNOLG%I,T1%R,T2%R,
     &                  NPLAN,DATE,TIME,MESH%X_ORIG,MESH%Y_ORIG,
     &                  IERR)
          CALL CHECK_CALL(IERR,'WRITE_MESH:SET_MESH')
        ENDIF
!
      ELSE
        IF(MESH%DIM1.EQ.3) THEN
          CALL SET_MESH(FFORMAT,NFILE,MESH%DIM1,MESH%TYPELM,NDP,
     &                  MESH%NPTFR,NPTIR,MESH%NELEM,MESH%NPOIN,
     &                 MESH%IKLE%I,IPOBO,MESH%KNOLG%I,MESH%X%R,MESH%Y%R,
     &                  NPLAN,DATE,TIME,MESH%X_ORIG,MESH%Y_ORIG,
     &                  IERR,Z=MESH%Z%R)
          CALL CHECK_CALL(IERR,'WRITE_MESH:SET_MESH')
        ELSE
          CALL SET_MESH(FFORMAT,NFILE,MESH%DIM1,MESH%TYPELM,NDP,
     &                  MESH%NPTFR,NPTIR,MESH%NELEM,MESH%NPOIN,
     &                 MESH%IKLE%I,IPOBO,MESH%KNOLG%I,MESH%X%R,MESH%Y%R,
     &                  NPLAN,DATE,TIME,MESH%X_ORIG,MESH%Y_ORIG,
     &                  IERR)
          CALL CHECK_CALL(IERR,'WRITE_MESH:SET_MESH')
        ENDIF
      ENDIF
!


      DEALLOCATE(IPOBO)
!
      IF(PRESENT(NGEO)) THEN
        IF((.NOT.PARALL).AND.GEOFORMAT.EQ.FFORMAT) THEN
          ! Transfering boundary information
          CALL GET_BND_NELEM(FFORMAT,NGEO,MESH%TYPELMBND,NELEBD,IERR)
          CALL CHECK_CALL(IERR,'WRITE_MESH:GET_BND_NELEM')
          ! Getting boundary connectivity
          ALLOCATE(IKLE_BND(NELEBD*2), STAT=IERR)
          CALL GET_BND_CONNECTIVITY(FFORMAT, NGEO, MESH%TYPELMBND,
     &                              NELEBD, 2, IKLE_BND, IERR)
          CALL CHECK_CALL(IERR,'WRITE_MESH:GET_BND_CONNECTIVITYO')

          CALL TRANSFER_GROUP_INFO(FFORMAT, NGEO, NFILE, MESH%TYPELM,
     &     MESH%TYPELMBND,IKLE_BND,NELEBD,2,.TRUE.,.FALSE.,IERR)
          CALL CHECK_CALL(IERR,'WRITE_MESH:TRANSFER_GROUP_INFO')
          DEALLOCATE(IKLE_BND)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
