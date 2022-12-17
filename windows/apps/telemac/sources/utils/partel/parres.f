!                   *****************
                    SUBROUTINE PARRES
!                   *****************
     & (NAMEGEO, NAMEINP, NPARTS, GEOFORMAT, INPFORMAT)
!
!
!***********************************************************************
! PARALLEL   V7P1
!***********************************************************************
!
!BRIEF    PARTIONNING A FILE USING AN ALREADY PARTIONNED GEOMETRY FILE
!
!HISTORY   Y AUDOUIN (LNHE)
!+         26/05/2015
!+         V7P1
!+         CREATION OF THE FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NAMEGEO        |<--| NAME OF THE GEOMETRY FILE
!| NAMEINP        |<--| NAME OF THE FILE TO BE PARTITIONNED
!| NPARTS         |<--| NUMBER OF PARTITIONS
!| GEOFORMAT      |<--| FORMAT OF THE GEOMETRY FILE
!| INPFORMAT      |<--| FORMAT OF THE FILE TO BE PARTITIONED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_PARTEL
      USE INTERFACE_HERMES
      USE INTERFACE_PARALLEL
      USE BIEF, ONLY : READ_MESH_INFO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: NAMEGEO
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: NAMEINP
      INTEGER, INTENT(IN) :: NPARTS
      CHARACTER(LEN=8), INTENT(INOUT) :: GEOFORMAT
      CHARACTER(LEN=8), INTENT(INOUT) :: INPFORMAT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      INTEGER :: NGEO=7,NINP=8,NINP_PAR=9
      CHARACTER(LEN=PATH_LEN) :: NAMEGEO_PAR
      CHARACTER(LEN=PATH_LEN) :: NAMEINP_PAR
      INTEGER :: I,J,IERR
      INTEGER :: IPART
!     VARIABLE INFORMATION
      CHARACTER(LEN=32),ALLOCATABLE :: VARLIST(:)
      CHARACTER(LEN=16),ALLOCATABLE :: VAR_NAME(:)
      CHARACTER(LEN=16),ALLOCATABLE :: VAR_UNIT(:)
      CHARACTER(LEN=80) :: TITLE
      INTEGER NVAR_INP,NVAR_GEO
      INTEGER NTIMESTEP,IVAR,ITIME
      DOUBLE PRECISION :: TIMES
      DOUBLE PRECISION,ALLOCATABLE :: VAL(:),VAL_INP(:)
!     GEOMETRY INFORMATION
      INTEGER NPOIN_GEO,TYP_ELM_GEO,NELEM_GEO,NPTFR_GEO,NPTIR_GEO,
     &        NDP_GEO,NPLAN_GEO
      INTEGER NPOIN_INP,TYP_ELM_INP,NELEM_INP,NPTFR_INP,NPTIR_INP,
     &        NDP_INP,NPLAN_INP
      INTEGER :: NPOIN2,NELEM2
      INTEGER :: NPOIN_P,NELEM_P
!     Connectivity table
      INTEGER, ALLOCATABLE :: IKLES(:),IKLE3D(:)
      INTEGER, ALLOCATABLE :: IKLE(:)
!     local to global numbering array
      INTEGER, ALLOCATABLE :: KNOLG(:),KNOLG3D(:)
!     Coordinates 2d and 3d
      DOUBLE PRECISION, ALLOCATABLE :: X(:),X3D(:)
      DOUBLE PRECISION, ALLOCATABLE :: Y(:),Y3D(:)
      INTEGER :: FULLDATE(6),DATE(3),TIME(3)
      INTEGER :: MESH_DIM
!     TIME MEASURING
      INTEGER  TDEB, TFIN, TEMPS, PARSEC
      LOGICAL TIMECOUNT
!
!     EXTENS
      CHARACTER(LEN=11) :: EXTENS
      EXTERNAL EXTENS
!
!----------------------------------------------------------------------
!
      WRITE(LU,*) '+---- PARRES: BEGINNING -------------+'
      WRITE(LU,*) '+---- USING GEOMETRY FILE: ',TRIM(NAMEGEO)
      CALL SYSTEM_CLOCK (COUNT=TEMPS, COUNT_RATE=PARSEC)
      TIMECOUNT = .TRUE.
      IF (PARSEC==0) TIMECOUNT = .FALSE.  ! COUNT_RATE == 0 : NO CLOCK
      IF (TIMECOUNT) TDEB = TEMPS
!
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!
!
!     INITIALISING MPI
!
!     OPEN THE FULL INPUT FILE
      CALL OPEN_MESH(INPFORMAT,NAMEINP,NINP,'READ     ',IERR)
      CALL CHECK_CALL(IERR,'PARRES:OPEN_MESH:NINP')
      ! LOOPING
      DO IPART = 1,NPARTS
        WRITE(LU,*) 'TREATING SUB-DOMAIN ', IPART
        IF(PARTEL_CONCAT)THEN
          NAMEGEO_PAR = TRIM(NAMEGEO)//'-CONCAT'
          NAMEINP_PAR = TRIM(NAMEINP)//'-CONCAT'
!         OPEN THE PARTITIONNED GEO FILE
          CALL OPEN_MESH(GEOFORMAT,NAMEGEO_PAR,NGEO,'READ     ',
     &                   IERR,IPART)
          CALL CHECK_CALL(IERR,'PARRES:OPEN_MESH:NGEO')
!         OPEN THE PARTIONNED INPUT FILE
          CALL OPEN_MESH(INPFORMAT,NAMEINP_PAR,NINP_PAR,'WRITE    ',
     &                   IERR,IPART)
          CALL CHECK_CALL(IERR,'PARRES:OPEN_MESH:NINP_PAR')
        ELSE
          NAMEGEO_PAR = TRIM(NAMEGEO)//EXTENS(NPARTS-1,IPART-1)
          NAMEINP_PAR = TRIM(NAMEINP)//EXTENS(NPARTS-1,IPART-1)
!         OPEN THE PARTITIONNED GEO FILE
          CALL OPEN_MESH(GEOFORMAT,NAMEGEO_PAR,NGEO,'READ     ',IERR)
          CALL CHECK_CALL(IERR,'PARRES:OPEN_MESH:NGEO')
!         OPEN THE PARTIONNED INPUT FILE
          CALL OPEN_MESH(INPFORMAT,NAMEINP_PAR,NINP_PAR,'WRITE    ',
     &                   IERR)
          CALL CHECK_CALL(IERR,'PARRES:OPEN_MESH:NINP_PAR')
        ENDIF
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!       HEADER INFORMATION
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!        BUILDING THE PARTIONNED INP FILE HEADER SAME AS THE ONE IN THE FULL INP FILE
!
        CALL GET_MESH_TITLE(INPFORMAT,NINP,TITLE,IERR)
        CALL CHECK_CALL(IERR,'PARRES:GET_MESH_TITLE:NINP')
!
        CALL GET_DATA_NVAR(INPFORMAT,NINP,NVAR_INP,IERR)
        CALL CHECK_CALL(IERR,'PARRES:GET_DATA_NVAR:NINP')
!
        ALLOCATE(VARLIST(NVAR_INP),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'PARRES:VARLIST')
        ALLOCATE(VAR_NAME(NVAR_INP),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'PARRES:VAR_NAME')
        ALLOCATE(VAR_UNIT(NVAR_INP),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'PARRES:VAR_UNIT')
        CALL GET_DATA_VAR_LIST(INPFORMAT,NINP,NVAR_INP,VAR_NAME,
     &                         VAR_UNIT,IERR)
        CALL CHECK_CALL(IERR,'PARRES:GET_DATA_CAR_LIST:NINP')
!       merging name and unit in one variable for the writing later on
        DO I=1,NVAR_INP
          VARLIST(I)(1:16) = VAR_NAME(I)
          VARLIST(I)(17:32) = VAR_UNIT(I)
        ENDDO
        DEALLOCATE(VAR_UNIT)
        DEALLOCATE(VAR_NAME)
!       WRITING PARTITIONNED INP FILE HEADER
        CALL SET_HEADER(INPFORMAT,NINP_PAR,TITLE,NVAR_INP,VARLIST,IERR)
        CALL CHECK_CALL(IERR,'PARRRES:SET_HEADER:NINP_PAR')
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!       MESH INFORMATION
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       GET MESH INFORMATION FROM THE GEOMETRY
        CALL READ_MESH_INFO(GEOFORMAT,NGEO,TITLE,NVAR_GEO,NPOIN_GEO,
     &                      TYP_ELM_GEO,NELEM_GEO,NPTFR_GEO,NPTIR_GEO,
     &                      NDP_GEO,NPLAN_GEO,X_ORIG,Y_ORIG)
!       GET MESH INFORMATION FROM THE INP FILE
        CALL READ_MESH_INFO(INPFORMAT,NINP,TITLE,NVAR_INP,NPOIN_INP,
     &                      TYP_ELM_INP,NELEM_INP,NPTFR_INP,NPTIR_INP,
     &                      NDP_INP,NPLAN_INP,X_ORIG,Y_ORIG)
        IF(NPLAN_INP.GT.1) THEN
          WRITE(LU,*) ' '
          WRITE(LU,*) '3D MESH DETECTED'
          NPOIN2 = NPOIN_INP/NPLAN_INP
          NELEM2 = NELEM_INP/(NPLAN_INP-1)
          WRITE(LU,*) 'NDP NODES PER ELEMENT:             ',NDP_INP
          WRITE(LU,*) 'ELEMENT TYPE :                     ',TYP_ELM_INP
          WRITE(LU,*) 'NPLAN NUMBER OF MESH LEVELS:       ',NPLAN_INP
          WRITE(LU,*) 'NPOIN2 NUMBER OF 2D MESH NODES:    ',NPOIN2
          WRITE(LU,*) 'NPOIN NUMBER OF 3D MESH NODES:     ',NPOIN_INP
          WRITE(LU,*) 'NELEM2 NUMBER OF 2D MESH ELEMENTS: ',NELEM2
          WRITE(LU,*) 'NELEM NUMBER OF 3D MESH ELEMENTS:  ',NELEM_INP
          IF (MOD(NPOIN_INP,NPLAN_INP).NE.0) THEN
            WRITE (LU,*) 'BUT NPOIN2 /= NPOIN3/NPLAN!'
            CALL PLANTE(1)
            STOP
          ENDIF
          IF (MOD(NELEM_INP,(NPLAN_INP-1)).NE.0) THEN
            WRITE (LU,*) 'BUT NELEM2 /= NELEM3/NPLAN!'
            CALL PLANTE(1)
            STOP
          ENDIF
          WRITE(LU,*) ' '
          WRITE(LU,*) 'THE INPUT FILE ASSUMED TO BE 3D'
          MESH_DIM = 3
          ! NUMBER OF POINT IN THE PARTIOTIONNED MESH
          NPOIN_P = NPOIN_GEO*NPLAN_INP
          NELEM_P = NELEM_GEO*(NPLAN_INP-1)
        ELSE
          WRITE(LU,*) ' '
          WRITE(LU,*) 'ONE-LEVEL MESH.'
          WRITE(LU,*) 'NDP NODES PER ELEMENT:         ',NDP_INP
          WRITE(LU,*) 'ELEMENT TYPE :                 ',TYP_ELM_INP
          WRITE(LU,*) 'NPOIN NUMBER OF MESH NODES:    ',NPOIN_INP
          WRITE(LU,*) 'NELEM NUMBER OF MESH ELEMENTS: ',NELEM_INP
          WRITE(LU,*) ' '
          NPOIN2 = NPOIN_INP
          NELEM2 = NELEM_INP
          WRITE(LU,*) 'THE INPUT FILE ASSUMED TO BE 2D'
          MESH_DIM = 2
          ! NUMBER OF POINT IN THE PARTIOTIONNED MESH
          NPOIN_P = NPOIN_GEO
          NELEM_P = NELEM_GEO
        ENDIF
!       ALLOCATION OF MESH ARRAYS
        ALLOCATE (IKLE(NELEM_GEO*NDP_GEO),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'PARRES:IKLES')
        ALLOCATE (IKLES(NELEM_GEO*NDP_GEO),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'PARRES:IKLES')
        ALLOCATE (KNOLG(NPOIN_GEO),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'PARRES:KNOLG')
        ALLOCATE (X(NPOIN_GEO),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'PARRES:X')
        ALLOCATE (Y(NPOIN_GEO),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'PARRES:Y')
!
!       READING THE CONNECTIVITY FROM GEO
!
        CALL GET_MESH_CONNECTIVITY(GEOFORMAT,NGEO,TYP_ELM_GEO,IKLES,
     &                             NELEM_GEO,NDP_GEO,IERR)
        CALL CHECK_CALL(IERR,'PARRES:GET_MESH_CONNECTIVITY:NGEO')
!
!       READING THE COORDINATES FROM GEO
!
        CALL GET_MESH_COORD(GEOFORMAT,NGEO,1,MESH_DIM,NPOIN_GEO,X,IERR)
        CALL CHECK_CALL(IERR,'PARRES:GET_MESH_COORD:NGEO:X')
        CALL GET_MESH_COORD(GEOFORMAT,NGEO,2,MESH_DIM,NPOIN_GEO,Y,IERR)
        CALL CHECK_CALL(IERR,'PARRES:GET_MESH_COORD:NGEO:Y')
!
!       READING LOCAL TO GLOBAL NUMBERING I.E. KNOLG FORM GEO
!
        CALL GET_MESH_L2G_NUMBERING(GEOFORMAT,NGEO,KNOLG,NPOIN_GEO,IERR)
        CALL CHECK_CALL(IERR,'PARRES:GET_MESH_L2G_NUMBERING:NGEO')
!
!       READING THE DATE AND TIME FROM THE INP FILE
!
        FULLDATE(:) = 0
        CALL GET_MESH_DATE(INPFORMAT,NINP,FULLDATE,IERR)
        CALL CHECK_CALL(IERR,'PARRES:GET_MESH_DATE:NINP')
        DATE(1) = FULLDATE(1)
        DATE(2) = FULLDATE(2)
        DATE(3) = FULLDATE(3)
        TIME(1) = FULLDATE(4)
        TIME(2) = FULLDATE(5)
        TIME(3) = FULLDATE(6)
!
!       WRITING MESH IN PARTITIONNED INP FILE
!
!       When in 3d building 3d connectivity, coordinates, knolg from 2d geometry
!       No such thing as 3d geometry in telemac (only in estel)
        IF(NPLAN_INP.GT.1) THEN
!         BUILDING 3D IKLES (TRANSFORMING TRIANGLE INTO PRISM)
          ALLOCATE (IKLE3D(NELEM_GEO*(NPLAN_INP-1)*NDP_INP),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR, 'PARRES:IKLE3D')
          DO I=1,NPLAN_INP-1
            DO J=1,NELEM_GEO
              IKLE3D(J + (I-1)*NELEM_GEO + (1-1)*NELEM_P) =
     &               IKLES(1+(J-1)*NDP_GEO) + (I-1)*NPOIN_GEO
              IKLE3D(J + (I-1)*NELEM_GEO + (2-1)*NELEM_P) =
     &               IKLES(2+(J-1)*NDP_GEO) + (I-1)*NPOIN_GEO
              IKLE3D(J + (I-1)*NELEM_GEO + (3-1)*NELEM_P) =
     &               IKLES(3+(J-1)*NDP_GEO) + (I-1)*NPOIN_GEO
              IKLE3D(J + (I-1)*NELEM_GEO + (4-1)*NELEM_P) =
     &               IKLES(1+(J-1)*NDP_GEO) + (I  )*NPOIN_GEO
              IKLE3D(J + (I-1)*NELEM_GEO + (5-1)*NELEM_P) =
     &               IKLES(2+(J-1)*NDP_GEO) + (I  )*NPOIN_GEO
              IKLE3D(J + (I-1)*NELEM_GEO + (6-1)*NELEM_P) =
     &               IKLES(3+(J-1)*NDP_GEO) + (I  )*NPOIN_GEO
            ENDDO
          ENDDO
!         BUILDING 3D X AND Y
          ALLOCATE (X3D(NPOIN_GEO*NPLAN_INP),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR, 'PARRES:IKLE3D')
          ALLOCATE (Y3D(NPOIN_GEO*NPLAN_INP),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR, 'PARRES:IKLE3D')
          DO I=1,NPLAN_INP
            DO J=1,NPOIN_GEO
              X3D(J + (I-1)*NPOIN_GEO) = X(J)
              Y3D(J + (I-1)*NPOIN_GEO) = Y(J)
            ENDDO
          ENDDO

!         BUILDING 3D KNOLG
          ALLOCATE (KNOLG3D(NPOIN_GEO*NPLAN_INP),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR, 'PARRES:IKLE3D')
          DO I=1,NPLAN_INP
            DO J=1,NPOIN_GEO
              KNOLG3D(J + (I-1)*NPOIN_GEO) =
     &                 KNOLG(J) + (I-1)*NPOIN2
            ENDDO
          ENDDO
!         WRITING MESH INFORMATION INTO PARTIONNED INPUT
          CALL SET_MESH(INPFORMAT,NINP_PAR,MESH_DIM,TYP_ELM_INP,NDP_INP,
     &                  NPTFR_GEO,NPTIR_GEO,NELEM_P,NPOIN_P,
     &                  IKLE3D,KNOLG3D,KNOLG3D,X3D,Y3D,NPLAN_INP,
     &                  DATE,TIME,X_ORIG,Y_ORIG,IERR)
          DEALLOCATE(IKLE3D)
          DEALLOCATE(X3D,Y3D)

        ELSE
          ! 2D writing of the mesh
          ! Switching from ikle(ndp,nelem) to ikle(nelem,ndp)
          DO I=1,NELEM_GEO
            DO J=1,NDP_GEO
              IKLE(I + (J-1)*NELEM_GEO) = IKLES(J + (I-1)*(NDP_GEO))
            ENDDO
          ENDDO

          CALL SET_MESH(INPFORMAT,NINP_PAR,MESH_DIM,TYP_ELM_GEO,NDP_GEO,
     &                  NPTFR_GEO,NPTIR_GEO,NELEM_GEO,NPOIN_GEO,
     &                  IKLE,KNOLG,KNOLG,X,Y,NPLAN_GEO,DATE,TIME,
     &                  X_ORIG,Y_ORIG,IERR)
        ENDIF
        DEALLOCATE(X,Y)
        DEALLOCATE(IKLES)
        DEALLOCATE(IKLE)
!       CLOSING GEO FILE NOT NEEDED ANYMORE
        CALL CLOSE_MESH(GEOFORMAT,NGEO,IERR)
        CALL CHECK_CALL(IERR,'PARRES:CLOSE_MESH:NGEO')
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!       DATA VALUES
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        CALL GET_DATA_NTIMESTEP(INPFORMAT,NINP,NTIMESTEP,IERR)
        CALL CHECK_CALL(IERR,'PARRES:GET_DATA_NTIMESTEP:NINP')
!
        ALLOCATE(VAL_INP(NPOIN_INP),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'PARRES:VAL_INP')
        ALLOCATE(VAL(NPOIN_P),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'PARRES:VAL')
!       LOOPING ON THE TIMESTEP AND VARIABLE OF INP FILE
        DO ITIME=1,NTIMESTEP
          CALL GET_DATA_TIME(INPFORMAT,NINP,ITIME-1,TIMES,IERR)
          CALL CHECK_CALL(IERR,'PARTEL:GET_DATA_TIME:NINP')
          WRITE(LU,*) ' -- WRITING TIMESTEP',ITIME-1,' AT',REAL(TIMES)
          ! Loop on all the variables
          DO IVAR=1,NVAR_INP
            CALL GET_DATA_VALUE(INPFORMAT,NINP,ITIME-1,
     &                          VARLIST(IVAR)(1:16),VAL_INP,
     &                          NPOIN_INP,IERR)
            ! GETTING THE VALUE NEEDED FOR THAT PARTITION
            IF(NPLAN_INP.GT.1) THEN
              DO I=1,NPOIN_P
                VAL(I) = VAL_INP(KNOLG3D(I))
              ENDDO
            ELSE
              DO I=1,NPOIN_P
                VAL(I) = VAL_INP(KNOLG(I))
              ENDDO
            ENDIF
            CALL ADD_DATA(INPFORMAT,NINP_PAR,VARLIST(IVAR),TIMES,
     &                    ITIME-1,IVAR.EQ.1,VAL,NPOIN_P,IERR)
            CALL CHECK_CALL(IERR,'PARRES:ADD_DATA:NINP_PAR')
          ENDDO
        ENDDO
!
        DEALLOCATE(VAL)
        DEALLOCATE(VAL_INP)
        DEALLOCATE(VARLIST)
        DEALLOCATE(KNOLG)
        IF(NPLAN_INP.GT.1) DEALLOCATE(KNOLG3D)

        CALL CLOSE_MESH(INPFORMAT,NINP_PAR,IERR,IPART)
        CALL CHECK_CALL(IERR,'PARRES:CLOSE_MESH:NINP_PAR')
!
      ENDDO ! IPART
!
      CALL CLOSE_MESH(INPFORMAT,NINP,IERR)
      CALL CHECK_CALL(IERR,'PARRES:CLOSE_MESH:NINP')
!
!     END OF RUN
!
!     CALL P_EXIT()
      IF (TIMECOUNT) THEN
        CALL SYSTEM_CLOCK (COUNT=TEMPS, COUNT_RATE=PARSEC)
        TFIN = TEMPS
        WRITE(LU,*) 'OVERALL TIMING: ',
     &    (1.0*(TFIN-TDEB))/(1.0*PARSEC),' SECONDS'
        WRITE(LU,*) ' '
      ENDIF

      WRITE(LU,*) '+---- PARRES: NORMAL TERMINATION ----+'
      WRITE(LU,*) ' '
!
!----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE PARRES
