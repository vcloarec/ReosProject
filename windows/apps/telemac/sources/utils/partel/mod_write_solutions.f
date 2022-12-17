!     **************************
      MODULE MOD_WRITE_SOLUTIONS
!     **************************
!
!***********************************************************************
! PARTEL
!***********************************************************************
!
!BRIEF    Writing field informations for partel
!
        IMPLICIT NONE
        CONTAINS
!     **************************
      SUBROUTINE WRITE_SOLUTIONS
!     **************************
     &   (FFORMAT, NBOR, KNOGL, NDP_BND,
     &    NELEBD, NINP, NPLAN, IKLES, TIMES, KNOLG, DATE,
     &    NELEM_P, NPOIN_P, NPTFR_P, UBOR, HBOR, CHBORD, TBOR,
     &    VBOR, BTBOR, ATBOR, LIUBOR, LIHBOR, LITBOR, LIVBOR,
     &    DATAVAL, IKLE_BND, ELELG, TYP_ELEM, TIME, TITLE, NVAR,
     &    NTIMESTEP, NPTFR, NPOIN2, NPARTS, NPOIN, NOUT, NDP, F,
     &    VARIABLE, TYP_BND_ELEM, NPTIR_P, NAMEINP, NAMECLI,
     &    COLOR)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |<--| Format of the result file
!| NBOR           |<--| Boundary file numbering
!| KNOGL          |<--| Global to local numbering
!| NDP_BND        |<--| Number of points for a boundary element
!| NELEBD         |<--| Number of boundary elements
!| NINP           |<--| Input file descriptor
!| NPLAN          |<--| Number of planes
!| IKLES          |<--| Connectivity table
!| TIMES          |<--| List of times in the file
!| KNOLG          |<--| Local to global numbering
!| DATE           |<--| Date of the file
!| NELEM_P        |<--| Number of elements in partitionned file
!| NPOIN_P        |<--| Number of points in partitionned file
!| NPTFR_P        |<--| Number of boundary points in partitionned file
!| UBOR           |<--| U velocity on boundary points
!| HBOR           |<--| Heigh on boundary points
!| TBOR           |<--| Tracer values on boundary points
!| VBOR           |<--| V velocity on boundary points
!| BTBOR          |<--| Second thermal exchange coeff
!| ATBOR          |<--| First thermal exchange coeff
!| LIUBOR         |<--| Type of boundary for U
!| LIHBOR         |<--| Type of boundary for H
!| LITBOR         |<--| Type of boundary for Tracers
!| LIVBOR         |<--| Type of boundary for V
!| DATAVAL        |<--| Results array
!| IKLE_BND       |<--| Connectivity array for boundary elements
!| ELELG          |<--| Local to global numbering for elements
!| TYP_ELEM       |<--| Type of the elements (TRIANGLE, ...)
!| TIME           |<--| Time of the file
!| TITLE          |<--| Title fo the output file
!| NVAR           |<--| Number fo variables
!| NTIMESTEP      |<--| Number of time-steps
!| NPTFR          |<--| Number of boundary_points
!| NPOIN2         |<--| Number of 2D points
!| NPARTS         |<--| Number of partitions
!| NPOIN          |<--| Number of points
!| NOUT           |<--| Output file descriptor
!| NDP            |<--| Number of points per element
!| F              |<--| Coordinates
!| VARIABLE       |<--| List of variable names
!| TYP_BND_ELEM   |<--| Type of boundary elements
!| NPTIR_P        |<--| Number of Interface points in partitionned file
!| NAMEINP        |<--| Name of input file
!| NAMECLI        |<--| Name of boundary file
!| COLOR          |<--| Colour for each boundary point
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        USE DECLARATIONS_SPECIAL
        USE DECLARATIONS_PARTEL
        USE INTERFACE_HERMES
        USE MOD_HASH_TABLE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        INTEGER, ALLOCATABLE, INTENT(INOUT) :: NBOR(:)
!
        DOUBLE PRECISION, ALLOCATABLE, INTENT(IN) :: DATAVAL(:,:,:)
        DOUBLE PRECISION, ALLOCATABLE, INTENT(IN) :: F(:,:)
        TYPE(HASH_TABLE), INTENT(IN) :: KNOGL
        CHARACTER(LEN=8), INTENT(INOUT) :: FFORMAT
        CHARACTER(LEN=80), INTENT(IN) :: TITLE
        CHARACTER(LEN=32), ALLOCATABLE, INTENT(IN) :: VARIABLE(:)
        CHARACTER(LEN=PATH_LEN), INTENT(IN) :: NAMEINP, NAMECLI
        INTEGER, INTENT(IN) :: NDP_BND, NELEBD, NINP, NPLAN, TIME(3),
     &   TYP_BND_ELEM, DATE(3), TYP_ELEM, NVAR, NTIMESTEP, NPTFR,
     &   NPOIN2, NPARTS, NPOIN, NDP
        INTEGER, INTENT(INOUT) :: NOUT
        INTEGER, ALLOCATABLE, INTENT(IN) :: IKLES(:), KNOLG(:,:),
     &    NELEM_P(:), NPOIN_P(:), NPTFR_P(:), LIUBOR(:), LIHBOR(:),
     &    LITBOR(:), LIVBOR(:), IKLE_BND(:), ELELG(:,:), NPTIR_P(:),
     &    COLOR(:)
        DOUBLE PRECISION, INTENT(INOUT) :: TIMES
        DOUBLE PRECISION, ALLOCATABLE, INTENT(IN) :: HBOR(:),UBOR(:),
     &    VBOR(:), CHBORD(:), TBOR(:),ATBOR(:),BTBOR(:)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        INTEGER :: I, J, K, L, M, IERR, ITIME, EF, IVAR, NELEBD_P
        INTEGER :: IPTFR, IPTFR_P
        INTEGER, ALLOCATABLE :: IKLE_BND_P(:)
        INTEGER, ALLOCATABLE :: KNOLG_P(:), IKLE_P(:), IKLE3D_P(:)
        DOUBLE PRECISION, ALLOCATABLE :: HBOR_P(:),UBOR_P(:),VBOR_P(:)
        DOUBLE PRECISION, ALLOCATABLE :: TBOR_P(:),ATBOR_P(:)
        DOUBLE PRECISION, ALLOCATABLE :: BTBOR_P(:), DATAVAL_P(:)
        DOUBLE PRECISION, ALLOCATABLE :: CHBORD_P(:), X(:), Y(:)
        INTEGER, ALLOCATABLE :: LIVBOR_P(:),LITBOR_P(:),COLOR_P(:)
        INTEGER, ALLOCATABLE :: LIHBOR_P(:),LIUBOR_P(:),NBOR_P(:)
        INTEGER, ALLOCATABLE :: KNOGL_BND(:)
        LOGICAL FULLY_IN
        CHARACTER(LEN=PATH_LEN) :: NAMEOUT, NAMECLM
        CHARACTER(LEN=11) :: EXTENS
        EXTERNAL EXTENS
!
        DO I=1,NPARTS
!
          WRITE(LU,*) 'TREATING SUB-DOMAIN ', I
!
!         WRITING GEOMETRY FILES FOR ALL PARTS/PROCESSORS
!
          IF(PARTEL_CONCAT)THEN
            NAMEOUT = TRIM(NAMEINP)//'-CONCAT'
            NAMECLM = TRIM(NAMECLI)//'-CONCAT'
            CALL OPEN_MESH(FFORMAT,NAMEOUT,NOUT,'WRITE    ',IERR, I)
          ELSE
            NAMEOUT = TRIM(NAMEINP)//EXTENS(NPARTS-1,I-1)
            NAMECLM = TRIM(NAMECLI)//EXTENS(NPARTS-1,I-1)
            CALL OPEN_MESH(FFORMAT,NAMEOUT,NOUT,'WRITE    ',IERR)
          ENDIF
          CALL CHECK_CALL(IERR,'PARTEL:OPEN_MESH:NOUT')
!
!         TITLE, THE NUMBER OF VARIABLES
!
          CALL SET_HEADER(FFORMAT,NOUT,TITLE,NVAR,VARIABLE,IERR)
!
!       THE MESH
!
          ALLOCATE(IKLE_P(NELEM_P(I)*3),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:IKLE_P')
          DO J=1,NELEM_P(I)
            EF=ELELG(J,I)
            DO K=1,3
              IKLE_P(J+(K-1)*NELEM_P(I)) =
     &          HASH_TABLE_GET(KNOGL,IKLES((EF-1)*3+K),I)
            ENDDO
          ENDDO
          IF(NPLAN.GT.1) THEN
            ALLOCATE(IKLE3D_P(NELEM_P(I)*6*(NPLAN-1)),STAT=IERR)
            CALL CHECK_ALLOCATE(IERR,'PARTEL:IKLE_P')
            DO K = 1,NPLAN-1
              DO J = 1,NELEM_P(I)
                IKLE3D_P(J+(K-1)*NELEM_P(I) + (0*NELEM_P(I)*(NPLAN-1)))=
     &       IKLE_P(J + (0*NELEM_P(I))) + (K-1)*NPOIN_P(I)
                IKLE3D_P(J+(K-1)*NELEM_P(I) + (1*NELEM_P(I)*(NPLAN-1)))=
     &       IKLE_P(J + (1*NELEM_P(I))) + (K-1)*NPOIN_P(I)
                IKLE3D_P(J+(K-1)*NELEM_P(I) + (2*NELEM_P(I)*(NPLAN-1)))=
     &       IKLE_P(J + (2*NELEM_P(I))) + (K-1)*NPOIN_P(I)
                IKLE3D_P(J+(K-1)*NELEM_P(I) + (3*NELEM_P(I)*(NPLAN-1)))=
     &       IKLE_P(J + (0*NELEM_P(I))) + (K)*NPOIN_P(I)
                IKLE3D_P(J+(K-1)*NELEM_P(I) + (4*NELEM_P(I)*(NPLAN-1)))=
     &       IKLE_P(J + (1*NELEM_P(I))) + (K)*NPOIN_P(I)
                IKLE3D_P(J+(K-1)*NELEM_P(I) + (5*NELEM_P(I)*(NPLAN-1)))=
     &       IKLE_P(J + (2*NELEM_P(I))) + (K)*NPOIN_P(I)
              ENDDO
            ENDDO
          ENDIF
!
! INSTEAD OF IRAND, KNOLG IS WRITTEN !!!
! I.E. THE TABLE PROCESSOR-LOCAL -> PROCESSOR-GLOBAL NODE NUMBERS
!
          IF(NPLAN.EQ.0) THEN
            ALLOCATE(KNOLG_P(NPOIN_P(I)),STAT=IERR)
            CALL CHECK_CALL(IERR,'PARTEL:KNOLG_P:2D')
            DO J=1,NPOIN_P(I)
              KNOLG_P(J) = KNOLG(J,I)
            ENDDO
          ELSE
            ALLOCATE(KNOLG_P(NPOIN_P(I)*NPLAN),STAT=IERR)
            CALL CHECK_CALL(IERR,'PARTEL:KNOLG_P:3D')
            DO J=1,NPOIN_P(I)*NPLAN
              ! WE FILL KNOLG WITH DUMMY VALUES BECAUSE 3D KNOLG IS NEVER USED
              KNOLG_P(J) = KNOLG(J,I)
            ENDDO
          ENDIF
!
! NODE COORDINATES X AND Y
!
          IF(NPLAN.EQ.0) THEN
            ALLOCATE(X(NPOIN_P(I)),STAT=IERR)
            CALL CHECK_ALLOCATE(IERR,'PARTEL:X')
            ALLOCATE(Y(NPOIN_P(I)),STAT=IERR)
            CALL CHECK_ALLOCATE(IERR,'PARTEL:Y')
            DO J=1,NPOIN_P(I)
              X(J) = F(KNOLG(J,I),1)
              Y(J) = F(KNOLG(J,I),2)
            ENDDO
          ELSE
            ALLOCATE(X(NPOIN_P(I)*NPLAN),STAT=IERR)
            CALL CHECK_ALLOCATE(IERR,'PARTEL:X')
            ALLOCATE(Y(NPOIN_P(I)*NPLAN),STAT=IERR)
            CALL CHECK_ALLOCATE(IERR,'PARTEL:Y')
            DO J=1,NPOIN_P(I)
              DO L=1,NPLAN
                X(J+(L-1)*NPLAN) = F(KNOLG(J,I)+(L-1)*NPOIN2,1)
                Y(J+(L-1)*NPLAN) = F(KNOLG(J,I)+(L-1)*NPOIN2,2)
              ENDDO
            ENDDO
          ENDIF
          IF(NPLAN.EQ.0) THEN
            CALL SET_MESH(FFORMAT,NOUT,2,TYP_ELEM,NDP,NPTFR_P(I),
     &                    NPTIR_P(I),NELEM_P(I),NPOIN_P(I),IKLE_P,
     &                    KNOLG_P,KNOLG_P,X,Y,NPLAN,DATE,TIME,
     &                    X_ORIG,Y_ORIG,
     &                    IERR)
            CALL CHECK_CALL(IERR,'PARTEL:SET_MESH:NOUT')
          ELSE
            CALL SET_MESH(FFORMAT,NOUT,3,TYP_ELEM,6,NPTFR_P(I),
     &                    NPTIR_P(I),NELEM_P(I)*(NPLAN-1),
     &                    NPOIN_P(I)*NPLAN,IKLE3D_P,
     &                    KNOLG_P,KNOLG_P,X,Y,NPLAN,DATE,TIME,
     &                    X_ORIG,Y_ORIG,
     &                    IERR)
            CALL CHECK_CALL(IERR,'PARTEL:SET_MESH:NOUT')
            DEALLOCATE(IKLE3D_P)
          ENDIF
          DEALLOCATE(X)
          DEALLOCATE(Y)
          DEALLOCATE(KNOLG_P)
          DEALLOCATE(IKLE_P)
!
!         WORK ON THE BOUNDARY CONDITIONS INFORMATIONS
!
          ! COMPUTE THE NUMBER OF BOUNDARY_ELEMENTS
          NELEBD_P = 0
          DO K=1,NELEBD
            FULLY_IN = .TRUE.
            DO L=1,NDP_BND
            ! IF EVERY POINT OF THE BOUNDARY ELEMENT IS IN THE PARTITION THEN THE ELEMENT IS AS WELL
              IF(HASH_TABLE_GET(KNOGL,
     &            IKLE_BND(K+(L-1)*NELEBD),I).EQ.0)THEN
                FULLY_IN = .FALSE.
                EXIT
              ENDIF
            ENDDO
            ! IF
            IF(FULLY_IN) NELEBD_P = NELEBD_P + 1
          ENDDO
!
          ! WRITING THE BOUNDARY CONDITIONS
          IF(PARTEL_CONCAT)THEN
            CALL OPEN_BND(FFORMAT,NAMECLM,NOUT,'WRITE    ',IERR,I)
          ELSE
            CALL OPEN_BND(FFORMAT,NAMECLM,NOUT,'WRITE    ',IERR)
          ENDIF
          CALL CHECK_CALL(IERR,'PARTEL:OPEN_BND')
          !
          ALLOCATE(IKLE_BND_P(NELEBD_P*NDP_BND),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:IKLE_BND_P')
          ALLOCATE(LIHBOR_P(NPTFR_P(I)),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:LIHBOR_P')
          ALLOCATE(LIUBOR_P(NPTFR_P(I)),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:LIUBOR_P')
          ALLOCATE(LIVBOR_P(NPTFR_P(I)),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:LIVBOR_P')
          ALLOCATE(HBOR_P(NPTFR_P(I)),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:HBOR_P')
          ALLOCATE(UBOR_P(NPTFR_P(I)),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:UBOR_P')
          ALLOCATE(VBOR_P(NPTFR_P(I)),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:VBOR_P')
          ALLOCATE(CHBORD_P(NPTFR_P(I)),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:CHBORD_P')
          ALLOCATE(LITBOR_P(NPTFR_P(I)),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:LITBOR_P')
          ALLOCATE(TBOR_P(NPTFR_P(I)),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:TBOR_P')
          ALLOCATE(ATBOR_P(NPTFR_P(I)),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:ATBOR_P')
          ALLOCATE(BTBOR_P(NPTFR_P(I)),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:BTBOR_P')
          ALLOCATE(KNOGL_BND(NELEBD),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:KNOGL_BND')
          ALLOCATE(COLOR_P(NPTFR_P(I)),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:COLOR')
          ALLOCATE(NBOR_P(NPTFR_P(I)),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'PARTEL:NBOR')
          J = 0
          DO K=1,NELEBD
            KNOGL_BND(K) = 0
            ! IF BOTH POINT OF THE SEGMENT ARE IN THE PARTITION THEN THE SEGMENT IS IN THE PARTITION
            FULLY_IN = .TRUE.
            DO L=1,NDP_BND
              IF(HASH_TABLE_GET(KNOGL,
     &            IKLE_BND(K+(L-1)*NELEBD),I).EQ.0) THEN
                FULLY_IN = .FALSE.
                EXIT
              ENDIF
            ENDDO
            IF(FULLY_IN) THEN
              J = J + 1
              DO L=1,NDP_BND
                M = IKLE_BND(K+(L-1)*NELEBD)
                IKLE_BND_P(J+(L-1)*NELEBD_P) = HASH_TABLE_GET(KNOGL,M,I)
              ENDDO
              KNOGL_BND(K) = J
            ENDIF
          ENDDO
!
          IPTFR_P = 0
          DO IPTFR=1,NPTFR
            IF(HASH_TABLE_GET(KNOGL,NBOR(IPTFR),I).EQ.0) CYCLE
            IPTFR_P = IPTFR_P + 1
            NBOR_P(IPTFR_P) = HASH_TABLE_GET(KNOGL,NBOR(IPTFR),I)
            HBOR_P(IPTFR_P) = HBOR(IPTFR)
            UBOR_P(IPTFR_P) = UBOR(IPTFR)
            VBOR_P(IPTFR_P) = VBOR(IPTFR)
            CHBORD_P(IPTFR_P) = CHBORD(IPTFR)
            TBOR_P(IPTFR_P) = TBOR(IPTFR)
            ATBOR_P(IPTFR_P) = ATBOR(IPTFR)
            BTBOR_P(IPTFR_P) = BTBOR(IPTFR)
            LIHBOR_P(IPTFR_P) = LIHBOR(IPTFR)
            LIUBOR_P(IPTFR_P) = LIUBOR(IPTFR)
            LIVBOR_P(IPTFR_P) = LIVBOR(IPTFR)
            LITBOR_P(IPTFR_P) = LITBOR(IPTFR)
            COLOR_P(IPTFR_P) = COLOR(IPTFR)
          ENDDO
          ! Transfering group information from geometry to result file
          CALL TRANSFER_GROUP_PART_INFO(FFORMAT,NINP,NOUT,
     &             TYP_BND_ELEM, IKLE_BND_P, NELEBD_P, NDP_BND,
     &             NELEBD, KNOGL_BND, .TRUE., NPOIN, NPOIN_P(I),
     &             KNOLG(:,I), IERR)
          CALL CHECK_CALL(IERR,'PARTEL:TRANSFER_GROUP_PART_INFO')

          CALL SET_BND(FFORMAT,NOUT,POINT_BND_ELT_TYPE,NPTFR_P(I),1,
     &                 NBOR_P,NPTFR_P(I),LIHBOR_P,LIUBOR_P,
     &                 LIVBOR_P,HBOR_P,UBOR_P,VBOR_P,CHBORD_P,
     &                 LITBOR_P,TBOR_P,ATBOR_P,BTBOR_P,
     &                 COLOR_P,IERR)
          CALL CHECK_CALL(IERR,'PARTEL:SET_BND')
          !

          IF(PARTEL_CONCAT)THEN
            CALL CLOSE_BND(FFORMAT,NOUT,IERR,I)
          ELSE
            CALL CLOSE_BND(FFORMAT,NOUT,IERR)
          ENDIF
          CALL CHECK_CALL(IERR,'PARTEL:CLOSE_BND')

          DEALLOCATE(IKLE_BND_P)
          DEALLOCATE(LIHBOR_P)
          DEALLOCATE(LIUBOR_P)
          DEALLOCATE(LIVBOR_P)
          DEALLOCATE(HBOR_P)
          DEALLOCATE(UBOR_P)
          DEALLOCATE(VBOR_P)
          DEALLOCATE(CHBORD_P)
          DEALLOCATE(LITBOR_P)
          DEALLOCATE(TBOR_P)
          DEALLOCATE(ATBOR_P)
          DEALLOCATE(BTBOR_P)
          DEALLOCATE(COLOR_P)
          DEALLOCATE(NBOR_P)
          DEALLOCATE(KNOGL_BND)
!
! TIME STAMP (SECONDS)
!
!!
!!   -------------------------------------------------------------------
!!   MODIFICATION TO PUT ALL THE RECORDINGS IN PARALLEL
!!   GEO FILE 08/06/2011
!!   -------------------------------------------------------------------
!!
!         EACH RECORDING IS READ AND ONLY THE LOCAL VARIABLES ARE STORED
!         INTO THE PARALLEL GEO FILE
!
!
          IF(NPLAN.LE.1) THEN
            ALLOCATE(DATAVAL_P(NPOIN_P(I)),STAT=IERR)
            CALL CHECK_ALLOCATE(IERR,'PARTEL:DATAVAL_P')
          ELSE
            ALLOCATE(DATAVAL_P(NPOIN_P(I)*NPLAN),STAT=IERR)
            CALL CHECK_ALLOCATE(IERR,'PARTEL:DATAVAL_P')
          ENDIF
          DO ITIME=1,NTIMESTEP
            CALL GET_DATA_TIME(FFORMAT,NINP,ITIME-1,TIMES,IERR)
            CALL CHECK_CALL(IERR,'PARTEL:GET_DATA_TIME:NINP')
            WRITE(LU,*) ' -- WRITING TIMESTEP',ITIME-1,' AT',REAL(TIMES)
            DO IVAR=1,NVAR
              IF(NPLAN.EQ.0) THEN
                ! 2D
                DO J=1,NPOIN_P(I)
                  DATAVAL_P(J) = DATAVAL(KNOLG(J,I),IVAR,ITIME)
                ENDDO
                CALL ADD_DATA(FFORMAT,NOUT,VARIABLE(IVAR),TIMES,ITIME-1,
     &                        IVAR.EQ.1,DATAVAL_P,NPOIN_P(I),IERR)
                CALL CHECK_CALL(IERR,'PARTEL:ADD_DATA')
              ELSE
                ! 3D
                DO J=1,NPOIN_P(I)
                  DO L=1,NPLAN
                    DATAVAL_P(J + (L-1)*NPOIN_P(I)) =
     &                       DATAVAL(KNOLG(J,I)+(L-1)*NPOIN2,IVAR,ITIME)
                  ENDDO
                ENDDO
                CALL ADD_DATA(FFORMAT,NOUT,VARIABLE(IVAR),TIMES,ITIME-1,
     &                        IVAR.EQ.1,DATAVAL_P,NPOIN_P(I)*NPLAN,IERR)
                CALL CHECK_CALL(IERR,'PARTEL:ADD_DATA')
              ENDIF
            ENDDO
          ENDDO
          DEALLOCATE(DATAVAL_P)
          IF(PARTEL_CONCAT)THEN
            CALL CLOSE_MESH(FFORMAT,NOUT,IERR,I)
          ELSE
            CALL CLOSE_MESH(FFORMAT,NOUT,IERR)
          ENDIF
          CALL CHECK_CALL(IERR,'PARTEL:CLOSE_MESH:NOUT')
        ENDDO
        CALL CLOSE_BND(FFORMAT,NINP,IERR)
        CALL CHECK_CALL(IERR,'PARTEL:CLOSE_BND:NCLI')
        CALL CLOSE_MESH(FFORMAT,NINP,IERR)
        CALL CHECK_CALL(IERR,'PARTEL:CLOSE_MESH:NINP')
      END SUBROUTINE
      END MODULE MOD_WRITE_SOLUTIONS
