!                   ****************
                    MODULE C_BINDING
!                   ****************
!
!***********************************************************************
! PARTEL
!***********************************************************************
!
!brief    call to the partionning software
!
!history   Y.AUDOUIN
!+
!+
!+         created
!
!
!
        USE, INTRINSIC :: ISO_C_BINDING
!
        IMPLICIT NONE
!
        INTERFACE
!
          ! Metis function
          SUBROUTINE C_MYMETIS_PARTMESHDUAL
     &        (NELEM, NPOIN, EPTR, EIND, VWGT,
     &         VSIZE, NCOMMONNODES, NPARTS, TPWGTS,
     &         OPTION, EDGECUT, EPART, NPART)
#if defined HAVE_MPI
     &        BIND(C, NAME='METIS_PartMeshDual')
#endif

            USE,INTRINSIC :: ISO_C_BINDING
            IMPLICIT NONE
            TYPE(C_PTR),VALUE :: NELEM
            TYPE(C_PTR),VALUE :: NPOIN
            INTEGER(C_INT) :: EPTR(*)
            INTEGER(C_INT) :: EIND(*)
            TYPE(C_PTR), VALUE :: VWGT
            TYPE(C_PTR), VALUE :: VSIZE
            TYPE(C_PTR),VALUE :: NCOMMONNODES
            TYPE(C_PTR),VALUE :: NPARTS
            TYPE(C_PTR), VALUE :: TPWGTS
            TYPE(C_PTR), VALUE :: OPTION
            TYPE(C_PTR),VALUE :: EDGECUT
            INTEGER(C_INT) :: EPART(*)
            INTEGER(C_INT) :: NPART(*)
!
          END SUBROUTINE
        END INTERFACE
      CONTAINS
          SUBROUTINE MYMETIS_PARTMESHDUAL
     &        (NELEM, NPOIN, EPTR, EIND,
     &          NCOMMONNODES, NPARTS,
     &          EDGECUT, EPART, NPART)

            USE,INTRINSIC :: ISO_C_BINDING
            IMPLICIT NONE
            INTEGER(C_INT),TARGET :: NELEM
            INTEGER(C_INT),TARGET :: NPOIN
            INTEGER(C_INT),TARGET :: EPTR(*)
            INTEGER(C_INT),TARGET :: EIND(*)
            INTEGER(C_INT),TARGET :: NCOMMONNODES
            INTEGER(C_INT),TARGET :: NPARTS
            INTEGER(C_INT),TARGET :: EDGECUT
            INTEGER(C_INT),TARGET :: EPART(*)
            INTEGER(C_INT),TARGET :: NPART(*)
!
#if defined HAVE_MPI
            CALL C_MYMETIS_PARTMESHDUAL
     &        (C_LOC(NELEM), C_LOC(NPOIN), EPTR,
     &         EIND, C_NULL_PTR,
     &         C_NULL_PTR, C_LOC(NCOMMONNODES), C_LOC(NPARTS),
     &         C_NULL_PTR,
     &         C_NULL_PTR, C_LOC(EDGECUT), EPART, NPART)
#endif
          END SUBROUTINE

      END MODULE C_BINDING
