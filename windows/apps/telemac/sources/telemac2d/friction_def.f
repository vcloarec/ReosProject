!                   *******************
                    MODULE FRICTION_DEF
!                   *******************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!history
!+
!+        V5P6
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      TYPE POINTER_TO_FRICTION
!         SEQUENCE
        TYPE(FRICTION_OBJ), POINTER :: P
      END TYPE POINTER_TO_FRICTION
!
      TYPE FRICTION_OBJ
!         SEQUENCE
        INTEGER          :: GNUMB(2) ! GLOBAL NUMBER OF THE ZONE
        INTEGER          :: RTYPE ! TYPE OF LAW USED
        INTEGER          :: VTYPE
        ! USE REAL BECAUSE CHESTR IS SAVED AS SIMPLE PRECISION IN SELAFIN DATA
        ! --------------------------------------------------------------------
        DOUBLE PRECISION :: RCOEF ! FRICTION PARAMETER
        DOUBLE PRECISION :: NDEF  ! DEFAULT MANNING (FOR C-W LAW)
        DOUBLE PRECISION :: VCOEF(15)    ! 15 COEFFICIENTS 
        
        TYPE(POINTER_TO_FRICTION), POINTER, DIMENSION(:) :: ADR
      END TYPE FRICTION_OBJ
!
      END MODULE FRICTION_DEF
