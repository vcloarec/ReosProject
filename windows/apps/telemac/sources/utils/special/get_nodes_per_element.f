!                   ********************************
                    SUBROUTINE GET_NODES_PER_ELEMENT
!                   ********************************
!
     &(TYP_ELT, NDP)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    GIVES THE EXTENSION FOR NAMING FILES IN PARALLEL
!+
!
!history  Y AUDOUIN (LNHE)
!+        25/05/2015
!+        V7P0
!+        Creation of the file
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| TYP_ELT        |-->| TYPE OF THE ELEMENT
!| NDP            |-->| NUMBER OF NODES COMPOSING THE ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: TYP_ELT
      INTEGER, INTENT(INOUT) :: NDP
!
!-----------------------------------------------------------------------
!
      NDP = 0
      SELECT CASE(TYP_ELT)
      CASE(TYPE_NULL)
        NDP = 0
      CASE(POINT_ELT_TYPE)
        NDP = 1
      CASE(POINT_BND_ELT_TYPE)
        NDP = 1
      CASE(TRIANGLE_ELT_TYPE)
        NDP = 3
      CASE(QUADRANGLE_ELT_TYPE)
        NDP = 4
      CASE(TETRAHEDRON_ELT_TYPE)
        NDP = 4
      CASE(PRISM_ELT_TYPE)
        NDP = 6
      CASE(SPLIT_PRISM_ELT_TYPE)
        NDP = 7
      CASE(EDGE_BND_ELT_TYPE)
        NDP = 2
      CASE(TRIANGLE_BND_ELT_TYPE)
        NDP = 3
      CASE(QUADRANGLE_BND_ELT_TYPE)
        NDP = 4
      CASE(TRIANGLE_3D_BND_ELT_TYPE)
        NDP = 3
      CASE DEFAULT
        WRITE(LU,*) 'UNKNWOWN ELEMENT TYPE ',TYP_ELT
        CALL PLANTE(1)
      END SELECT
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE

