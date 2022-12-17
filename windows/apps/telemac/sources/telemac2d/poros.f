!                   ****************
                    SUBROUTINE POROS
!                   ****************
!
     &(TETA,ZF,HN,MESH)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    IDENTIFIES TIDAL FLATS.
!+
!+
!+            IMPLEMENTS DELFINA ET AL WETTING/DRYING ALGORITHM.
!+
!+            PARTIALLY WET ELEMENT : TETA = 0
!+
!+            WET ELEMENT           : TETA = NU = 1.0
!+
!+            DRY ELEMENT           : TETA = NU = 0.0
!+
!+
!+            THE DRYING CRITERION IS THAT OF J.-M. JANIN :
!+                BOTTOM ELEVATION AT ONE NODE OF THE ELEMENT IS
!+                HIGHER THAN FREE SURFACE ELEVATION AT ANOTHER.
!
!history  J-M HERVOUET (LNHE)     ; PAUL BATES (BRISTOL)
!+        01/08/1997
!+        V5P2
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
!| HN             |-->| WATER DEPTH AT TIME T(N)
!| MESH           |-->| MESH STRUCTURE
!| TETA           |<--| POROSITY PER ELEMENT
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_POROS => POROS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: ZF,HN
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TETA
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELMZ,IELMH
!
      IELMZ=ZF%ELM
      IELMH=HN%ELM
!
!-----------------------------------------------------------------------
!
!     1) COMPUTES POROSITY ON TIDAL FLATS
!
      IF(IELMZ.EQ.11.AND.IELMH.EQ.11) THEN
!
        CALL PORO11(TETA%R,ZF%R,
     &              HN%R,MESH%IKLE%I,MESH%NELEM,MESH%NELMAX)
!
      ELSE
!
        WRITE(LU,11) IELMH,IELMZ
11      FORMAT(1X,'POROS : DISCRETIZATION NOT IMPLEMENTED:',I6,' ',I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!     2) CORRECTED BY A USER SUBROUTINE
!
      CALL CORPOR(TETA)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
