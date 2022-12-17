!                   ***********************
                    SUBROUTINE GRETEL_AUTOP
!                   ***********************
     &(GEO,GEOFORMAT,BND,RES,RESFORMAT,NPROC,NPLAN_RES,METHOD)
!
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    MERGES THE RESULTS OF A PARALLEL COMPUTATION
!+                TO WRITE A SINGLE FILE IN A GIVEN FORMAT.
!
!
!history  Y. Audouin
!+        02/09/2014
!+
!+   Creation of the file
!+   This version of gretel now handles multiple format and has a better
!+   memory organisation
!+   It also uses the hermes module for I/O
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] GEO Name of the geometry file
!>@param[in,out] GEOFORMAT Format of the geometry file
!>@param[in] BND Name of the boudnary file
!>@param[in] RES Name of the result file
!>@param[in,out] RESFORMAT Format of the result file
!>@param[in] NPROC Number of processors
!>@param[in] NPLAN_RES Number of planes for the result file
!>@param[in] METHOD method for merging data information:
!!                  1: No more dans npoin*nvar in memory loop on time
!!                  2: Max memory npoin*nvar*ntimestep loop on files
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
      USE BIEF, ONLY: READ_MESH_INFO
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: GEO
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: BND
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: RES
      CHARACTER(LEN=8),   INTENT(INOUT) :: GEOFORMAT,RESFORMAT
      INTEGER,            INTENT(IN) :: NPROC
      INTEGER,            INTENT(INOUT) :: NPLAN_RES
      INTEGER,            INTENT(IN) :: METHOD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      INTEGER IPID
      INTEGER I,J,IELEM
      INTEGER NPLAN_GEO,NELEM_GEO,NDP,NELEBD, NPTFR, NPTIR
      INTEGER :: NDIM, X_ORIG, Y_ORIG
!
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: IPOBO_GEO,IPOBO3D
      INTEGER, DIMENSION(:), ALLOCATABLE :: KNOLG, TMP2
      INTEGER, DIMENSION(:), ALLOCATABLE :: IKLE_GEO,IKLE3D
      INTEGER, DIMENSION(:), ALLOCATABLE :: IKLE_BND
!
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: TMP, X, Y
!
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: X3D, Y3D, Z3D
!
      CHARACTER(LEN=300) :: RESPAR
!
      CHARACTER(LEN=80) TITSEL
      CHARACTER(LEN=32),ALLOCATABLE :: TEXTELU(:)
      CHARACTER(LEN=16),ALLOCATABLE :: VAR_NAME(:), VAR_UNIT(:)
      CHARACTER(LEN=16) :: VARNAME
      CHARACTER(LEN=11) EXTENS
      EXTERNAL  EXTENS
!
      INTEGER IERR, NRES, NRESPAR, NGEO
      INTEGER TYP_ELEM, TYP_BND_ELEM
      INTEGER DATE(3), TIME(3)
      INTEGER NTIMESTEP_RES
      INTEGER NPOIN_GEO, NPOIN_RES, NPOIN_PAR
      INTEGER NVAR_RES,NVAR_GEO
      INTEGER NPOIN3D, NELEM3D
      INTEGER DATE_TMP(6)
!
!-------------------------------------------------------------------------
!
!
!|==================================================================|
!|                                                                  |
!| START: MERGES FILES RESULTING FROM THE DOMAIN DECOMPOSITION      |
!|                                                                  |
!|==================================================================|
!
! READS FILE NAMES AND THE NUMBER OF PROCESSORS / PARTITIONS
!
!
!
!     Header information
!
      RESPAR = TRIM(RES) // EXTENS(NPROC-1,0)
      CALL OPEN_MESH(RESFORMAT,RESPAR,NRESPAR,'READ     ',IERR)
      CALL CHECK_CALL(IERR,"GRETEL:OPEN_MESH:RESPAR")
!     Opening result file after so that it will be the same endianess as
!     the partitionned result files
      CALL OPEN_MESH(RESFORMAT,RES,NRES,'WRITE    ',IERR)
      CALL CHECK_CALL(IERR,"GRETEL:OPEN_MESH:RES")
!
      ! GET THE MESH TITLE
      CALL GET_MESH_TITLE(RESFORMAT,NRESPAR,TITSEL,IERR)
      CALL CHECK_CALL(IERR,"GRETEL:GET_MESH_TITLE")

!     Get the number of variable
      CALL GET_DATA_NVAR(RESFORMAT,NRESPAR,NVAR_RES,IERR)
      CALL CHECK_CALL(IERR,"GRETEL:GET_DATA_NVAR")
!
      ALLOCATE(TEXTELU(NVAR_RES),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:TEXTELU')
      ALLOCATE(VAR_NAME(NVAR_RES),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:VAR_NAME')
      ALLOCATE(VAR_UNIT(NVAR_RES),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:VAR_UNIT')
!
      CALL GET_DATA_VAR_LIST(RESFORMAT,NRESPAR,NVAR_RES,VAR_NAME,
     &                       VAR_UNIT,IERR)
      CALL CHECK_CALL(IERR,"GRETEL:GET_DATA_VAR_LIST")
!
      WRITE(LU,*) 'TITLE=',TITSEL
      WRITE(LU,*) 'NBVAR=',NVAR_RES
      DO I=1,NVAR_RES
        TEXTELU(I)(1:16) = VAR_NAME(I)
        TEXTELU(I)(17:32) = VAR_UNIT(I)
        WRITE(LU,*) 'VARIABLE ',I,' : ',TEXTELU(I)
      ENDDO ! I
!
      DEALLOCATE(VAR_NAME)
      DEALLOCATE(VAR_UNIT)
!
!     WE NEED TO GET THE NUMBER OF PLANES IN THE PARTITIONNED FILE
!     TO KNOW IF WE NEED TO TRANFORM THE GEOMETRY IN 3D GEOMETRY
      CALL GET_MESH_NPLAN(RESFORMAT,NRESPAR,NPLAN_RES,IERR)
      CALL CHECK_CALL(IERR,"GRETEL:MESH_NPLAN")
!
!     Get the number of timestep
      CALL GET_DATA_NTIMESTEP(RESFORMAT,NRESPAR,NTIMESTEP_RES,IERR)
      CALL CHECK_CALL(IERR,"GRETEL:GET_DATA_NTIMESTEP")

      CALL CLOSE_MESH(RESFORMAT,NRESPAR,IERR)
      CALL CHECK_CALL(IERR,"GRETEL:CLOSE_MESH:RESPAR")

      CALL SET_HEADER(RESFORMAT,NRES,TITSEL,NVAR_RES,TEXTELU,IERR)
!
!     Geometry information
!
      CALL OPEN_MESH(GEOFORMAT,GEO,NGEO,'READ     ',IERR)
      CALL CHECK_CALL(IERR,"GRETEL:OPEN_MESH:GEO")
!

!     Boundary file
      CALL OPEN_BND(GEOFORMAT,BND,NGEO,'READ     ',IERR)
      CALL CHECK_CALL(IERR,"GRETEL:OPEN_BND:GEO")

      CALL READ_MESH_INFO(GEOFORMAT,NGEO,TITSEL,NVAR_GEO,NPOIN_GEO,
     &                    TYP_ELEM,NELEM_GEO,NPTFR,NPTIR,NDP,NPLAN_GEO,
     &                    X_ORIG,Y_ORIG,TYP_BND_ELEM,NELEBD)
!
      WRITE(LU,*) 'GEO MESH INFORMATIONS:'
      WRITE(LU,*) 'NELEM=',NELEM_GEO
      WRITE(LU,*) 'NPOIN=',NPOIN_GEO
      WRITE(LU,*) 'NDP=',NDP
      WRITE(LU,*) 'TYP_ELEM=',TYP_ELEM
!
      ALLOCATE(IKLE_GEO(NELEM_GEO*NDP),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:IKLE_GEO')
      ALLOCATE(IPOBO_GEO(NPOIN_GEO),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:IPOBO')
!
      CALL READ_MESH_CONN(GEOFORMAT,NGEO,NPOIN_GEO,TYP_ELEM,NELEM_GEO,
     &                    NDP,TYP_BND_ELEM,NELEBD,IKLE_GEO, IPOBO_GEO)

      IF(NPLAN_RES.LE.1) THEN
        NDIM = 2
      ELSE
        NDIM = 3
      ENDIF
      ! GET 2D COORDINATES
      ALLOCATE(X(NPOIN_GEO),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:X')
      ALLOCATE(Y(NPOIN_GEO),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:Y')
      ALLOCATE(Z3D(NPOIN_GEO*NPLAN_RES),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:Z3D')
      ! GET MESH COORDINATES FROM THE GEO MESH
      CALL GET_MESH_COORD(GEOFORMAT,NGEO,1,2,NPOIN_GEO,X,IERR)
      CALL CHECK_CALL(IERR,'GRETEL:GET_MESH_COORD:X:GEO')
      CALL GET_MESH_COORD(GEOFORMAT,NGEO,2,2,NPOIN_GEO,Y,IERR)
      CALL CHECK_CALL(IERR,'GRETEL:GET_MESH_COORD:Y:GEO')
      !
      ! Update coordiantes with coordinates from the partitionned files
      ! as they could have been modified by corrxy
      DO IPID = 0, NPROC-1
!
        RESPAR = TRIM(RES) // EXTENS(NPROC-1,IPID)
        CALL OPEN_MESH(RESFORMAT,RESPAR,NRESPAR,'READ     ',IERR)
        CALL CHECK_CALL(IERR,"GRETEL:OPEN_MESH:RESPAR2")
!
        CALL GET_MESH_NPOIN(RESFORMAT,NRESPAR,TYP_ELEM,NPOIN_PAR,IERR)
        CALL CHECK_CALL(IERR,"GRETEL:GET_MESH_NPOIN:RESPAR")
!
        ALLOCATE(KNOLG(NPOIN_PAR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GRETEL:KNOLG')
        ALLOCATE(TMP(NPOIN_PAR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GRETEL:TMP')
!
        CALL GET_MESH_L2G_NUMBERING(RESFORMAT,NRESPAR,KNOLG,
     &                              NPOIN_PAR,IERR)
        CALL CHECK_CALL(IERR,'GRETEL:GET_MESH_L2G_NUMBERING:RESPAR')
!
        CALL GET_MESH_COORD(RESFORMAT,NRESPAR,1,NDIM,NPOIN_PAR,TMP,IERR)
        CALL CHECK_CALL(IERR,'GRETEL:GET_MESH_COORD:X:RESPAR')
        DO I=1,NPOIN_PAR/(MAX(NPLAN_RES,1))
          X(KNOLG(I)) = TMP(I)
        ENDDO
        CALL GET_MESH_COORD(RESFORMAT,NRESPAR,2,NDIM,NPOIN_PAR,TMP,IERR)
        CALL CHECK_CALL(IERR,'GRETEL:GET_MESH_COORD:Y:RESPAR')
        DO I=1,NPOIN_PAR/(MAX(NPLAN_RES,1))
          Y(KNOLG(I)) = TMP(I)
        ENDDO
        ! If 3d getting Z from result file (First variable at time=0.0)
        IF(NDIM.EQ.3) THEN
          VARNAME = TEXTELU(1)(1:16)
          CALL GET_DATA_VALUE(RESFORMAT,NRESPAR,0,VARNAME,TMP,
     &                        NPOIN_PAR,IERR)
          DO I=1,NPOIN_PAR
            Z3D(KNOLG(I)) = TMP(I)
          ENDDO
        ENDIF
        ! Getting the date from result file
        CALL GET_MESH_DATE(RESFORMAT,NRESPAR,DATE_TMP,IERR)
        CALL CHECK_CALL(IERR,'GRETEL:GET_MESH_DATE;RESPAR')
        DO I=1,3
          DATE(I) = DATE_TMP(I)
          TIME(I) = DATE_TMP(I+3)
        ENDDO
        !
        CALL CLOSE_MESH(RESFORMAT,NRESPAR,IERR)
        CALL CHECK_CALL(IERR,'GRETEL:CLOSEMESH:RESPAR')
        DEALLOCATE(TMP)
        DEALLOCATE(KNOLG)
      ENDDO ! IPID
      !
      ! IF WE HAVE A 3D RESULT WE NEED TO TRANSFORM THE MESH IN 3D
      ! WRITES THE MESH INFORMATION TO THE MERGED FILE
      WRITE(LU,*) 'WRITING MESH'
      IF(NDIM.EQ.2) THEN
        ! 2D
        ALLOCATE(TMP2(NELEM_GEO*NDP),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GRETEL:TMP0')
        DO I = 1,NDP
          DO IELEM = 1,NELEM_GEO
            TMP2((I-1)*NELEM_GEO + IELEM) = IKLE_GEO((IELEM-1)*NDP+I)
          ENDDO
        ENDDO

        CALL SET_MESH(RESFORMAT,NRES,2,TYP_ELEM,NDP,NPTFR,NPTIR,
     &                NELEM_GEO,NPOIN_GEO,TMP2,IPOBO_GEO,IPOBO_GEO,X,Y,
     &                NPLAN_RES,DATE,TIME,X_ORIG,Y_ORIG,IERR,
     &                IN_PLACE=.TRUE.)
        CALL CHECK_CALL(IERR,'GRETEL:SET_MESH:RES')
        DEALLOCATE(IKLE_GEO)
        DEALLOCATE(TMP2)
        DEALLOCATE(IPOBO_GEO)
        DEALLOCATE(X)
        DEALLOCATE(Y)
        NPOIN_RES = NPOIN_GEO
      ELSE
        ! 3D
        NDP = 6
        TYP_ELEM = PRISM_ELT_TYPE
        NPOIN3D = NPOIN_GEO * NPLAN_RES
        NELEM3D = NELEM_GEO * (NPLAN_RES-1)
        NPTFR = 0
        NPTIR = 0
        NPOIN_RES = NPOIN3D
        ALLOCATE(IKLE3D(NELEM3D*NDP),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'GRETEL:IKLE3D')
        ALLOCATE(IPOBO3D(NPOIN3D),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'GRETEL:IPOBO3D')
!
        ! BUILDING 3D IKLE
        DO I=1,NPLAN_RES-1
          DO J=1,NELEM_GEO
            IKLE3D(J+(I-1)*NELEM_GEO + (0*NELEM3D)) =
     &                    IKLE_GEO((J-1)*3+1) + (I-1)*NPOIN_GEO
            IKLE3D(J+(I-1)*NELEM_GEO + (1*NELEM3D)) =
     &                    IKLE_GEO((J-1)*3+2) + (I-1)*NPOIN_GEO
            IKLE3D(J+(I-1)*NELEM_GEO + (2*NELEM3D)) =
     &                    IKLE_GEO((J-1)*3+3) + (I-1)*NPOIN_GEO
            IKLE3D(J+(I-1)*NELEM_GEO + (3*NELEM3D)) =
     &                    IKLE_GEO((J-1)*3+1) + (I)*NPOIN_GEO
            IKLE3D(J+(I-1)*NELEM_GEO + (4*NELEM3D)) =
     &                    IKLE_GEO((J-1)*3+2) + (I)*NPOIN_GEO
            IKLE3D(J+(I-1)*NELEM_GEO + (5*NELEM3D)) =
     &                    IKLE_GEO((J-1)*3+3) + (I)*NPOIN_GEO
          ENDDO
        ENDDO
        ! DEFAULT IPOBO
        DO I=1,NPOIN3D
          IPOBO3D(I) = 0
        ENDDO
        ! WE DONT NEED THEM ANYMORE
        DEALLOCATE(IKLE_GEO)
        DEALLOCATE(IPOBO_GEO)
        ! BUILD THE COORDINATES FROM THE 2D COORDINATES
        ALLOCATE(X3D(NPOIN_GEO*NPLAN_RES),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GRETEL:X3D')
        ALLOCATE(Y3D(NPOIN_GEO*NPLAN_RES),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GRETEL:Y3D')
        !
        DO I=1,NPOIN_GEO
          DO J=1,NPLAN_RES
            X3D(I+(J-1)*NPOIN_GEO) = X(I)
            Y3D(I+(J-1)*NPOIN_GEO) = Y(I)
          ENDDO
        ENDDO
        DEALLOCATE(X)
        DEALLOCATE(Y)
        DATE = (/0,0,0/)
        TIME = (/0,0,0/)
        CALL SET_MESH(RESFORMAT,NRES,3,TYP_ELEM,NDP,NPTFR,NPTIR,NELEM3D,
     &                NPOIN3D,IKLE3D,IPOBO3D,IPOBO3D,X3D,Y3D,
     &                NPLAN_RES,DATE,TIME,X_ORIG,Y_ORIG,IERR,
     &                Z=Z3D,IN_PLACE=.TRUE.)
        CALL CHECK_CALL(IERR,'GRETEL:SET_MESH:RES')
        DEALLOCATE(IKLE3D)
        DEALLOCATE(IPOBO3D)
        DEALLOCATE(X3D)
        DEALLOCATE(Y3D)
        DEALLOCATE(Z3D)


      ENDIF
      ! Transfering boundary information
      ! Getting boundary connectivity
      IF(GEOFORMAT.EQ.RESFORMAT) THEN
        ALLOCATE(IKLE_BND(NELEBD*2), STAT=IERR)
        CALL GET_BND_CONNECTIVITY(GEOFORMAT, NGEO, TYP_BND_ELEM, NELEBD,
     &                            2, IKLE_BND, IERR)
        CALL CHECK_CALL(IERR,'GRETEL:GET_BND_CONNECTIVITY:GEO')

        CALL TRANSFER_GROUP_INFO(GEOFORMAT, NGEO, NRES, TYP_ELEM,
     &   TYP_BND_ELEM,IKLE_BND,NELEBD,2,.TRUE.,.TRUE.,IERR)
        CALL CHECK_CALL(IERR,'GRETEL:TRANSFER_GROUP_INFO:GEO')
        DEALLOCATE(IKLE_BND)
      ENDIF
!
!     Read results informations from partitioned files
!
      CALL MERGE_DATA
     &(NPOIN_RES, NVAR_RES, NTIMESTEP_RES, NPROC, RESFORMAT, NRES,
     & TYP_ELEM, TEXTELU, RES, NDIM, NPLAN_RES, NPOIN_GEO, METHOD)

      ! DONE
      CALL CLOSE_MESH(RESFORMAT,NRES,IERR)
      CALL CHECK_CALL(IERR,'GRETEL:CLOSEMESH:RES')
      CALL CLOSE_BND(GEOFORMAT,NGEO,IERR)
      CALL CHECK_CALL(IERR,'GRETEL:CLOSE_BND:GEO')
      CALL CLOSE_MESH(GEOFORMAT,NGEO,IERR)
      CALL CHECK_CALL(IERR,'GRETEL:CLOSE_MESH:GEO')

      DEALLOCATE(TEXTELU)
      WRITE(LU,*) 'END OF PROGRAM, ',NTIMESTEP_RES,' DATASETS FOUND'

      END SUBROUTINE GRETEL_AUTOP
