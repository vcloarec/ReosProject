!                   *********************
                    SUBROUTINE WRITE_DATA
!                   *********************
!
     &(FFORMAT,FILERES,NVARS,TIME,TIMESTEP,OUTVAR,NOMVAR,BVARSOR,N,MESH)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    WRITES DATA VALUES ON A MESH INTO THE DATA FILE OF THE
!+                GIVEN FILE FORMAT.
!+
!+            DATA VALUES ARE STORED IN A BIEF_OBJ BLOCK (BVARSOR),
!+                AND THE LOGICAL OUTVAR INDICATES FOR EACH VARIABLE IF
!+                WE SHOULD PRINT IT OUT OR NOT.
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
!history  Y. AUDOUIN (EDF), V.STOBIAC (EDF)
!+        10/12/2014
!+        V7P0
!+
!+   Use of Hermes interface to write data and mesh update option
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT         |-->| FILE FORMAT
!| FILERES         |-->| LOGICAL UNIT OF FILE
!| NVARS           |-->| NUMBER OF VARIABLES
!| TIME            |-->| TIME
!| TIMESTEP        |-->| TIME STEP (INTEGER), NOT DT.
!| OUTVAR          |-->| VARIABLES TO BE PUT IN THE FILE
!| NOMVAR          |-->| NAME OF VARIABLES
!| BVARSOR         |-->| BIEF BLOCK CONTAINING THE VARIABLES VARIABLES
!| N               |-->| NUMBER OF VALUES (MAY BE DIFFERENT FROM
!|                 |   | THE NUMBER OF DEGREES OF FREEDOM, E.G. FOR
!|                 |   | QUADRATIC ELEMENTS ONLY THE LINEAR VALUES
!|                 |   | ARE EXITED)
!| MESH (OPTIONAL) |-->| MESH STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_WRITE_DATA => WRITE_DATA
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN)           :: FFORMAT
      INTEGER,          INTENT(IN)           :: FILERES,N
      INTEGER,          INTENT(IN)           :: NVARS
      DOUBLE PRECISION, INTENT(IN)           :: TIME
      INTEGER,          INTENT(IN)           :: TIMESTEP
      CHARACTER(LEN=32),DIMENSION(NVARS), INTENT(IN) :: NOMVAR
      LOGICAL, DIMENSION(NVARS), INTENT(IN)  :: OUTVAR
      TYPE(BIEF_OBJ),   INTENT(IN)           :: BVARSOR
      TYPE(BIEF_MESH),  INTENT(IN), OPTIONAL :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL :: FIRST_VAR
      INTEGER :: I,IERR, NPT, NB_DIM
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: COORD
      CHARACTER(LEN=32) :: VAR_NAME
!
!-----------------------------------------------------------------------
!
      ! CONSTRUCT COORDINATES TABLE
      IF(PRESENT(MESH)) THEN
        ! SET VARIABLES
        NPT    = MESH%NPOIN
        NB_DIM = MESH%DIM1
        ! SET COORDINATES
        ALLOCATE(COORD(NB_DIM*NPT))
        DO I=1,NPT
          COORD(I)     = MESH%X%R(I)
          COORD(I+NPT) = MESH%Y%R(I)
          IF(NB_DIM.EQ.3) COORD(I+2*NPT) = MESH%Z%R(I)
        ENDDO
        ! UPDATE COORDINATES IN 3D RES FILES
        CALL UPDATE_DATA_MESH(FFORMAT,FILERES,TIME,TIMESTEP,
     &                        NB_DIM,NPT,COORD,IERR)
        DEALLOCATE(COORD)
      ENDIF
!
      ! LOOP ON ALL THE VARIABLES
      FIRST_VAR = .TRUE.
      DO I=1,NVARS
        ! IF THE VARIABLE MUST BE WRITTEN
        IF(OUTVAR(I)) THEN
          IF(ASSOCIATED(BVARSOR%ADR(I)%P%R)) THEN
            VAR_NAME = NOMVAR(I)
            CALL ADD_DATA(FFORMAT,FILERES,VAR_NAME,TIME,TIMESTEP,
     &                    FIRST_VAR,BVARSOR%ADR(I)%P%R,N,IERR)
            CALL CHECK_CALL(IERR,'WRITE_DATASET:ADD_DATA')
            FIRST_VAR = .FALSE.
          ELSE
            WRITE(LU,*) 'WRITE_DATA: VARIABLE NO: ',I
            WRITE(LU,*) '        NOT OR NOT WELL ALLOCATED'
            WRITE(LU,*) '        OR POINTER NOT ASSOCIATED '
          ENDIF
        ENDIF
      ENDDO

!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
