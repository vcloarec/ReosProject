!                   *****************
                    SUBROUTINE CORRSL
!                   *****************
!
     &(NEWSL,OLDSL,ZF,MESH)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    CORRECTS THE FREE SURFACE COMPUTATION BY ELEMENTS
!+                TO TAKE ACCOUNT OF THE TIDAL FLATS.
!
!history  J-M JANIN (LNH)    ; J-M HERVOUET (LNH)
!+        27/11/92
!+        V5P1
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        12/05/2014
!+        V7P0
!+   Discontinuous elements better treated: new types 15, 16 and 17 for
!+   discontinuous linear, quasi-bubble, and quadratic, rather than
!+   using component DIMDISC=11, 12 or 13.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MESH           |-->| MESH STRUCTURE
!| NEWSL          |<->| MODIFIED FREE SURFACE, GIVEN PER ELEMENT
!| OLDSL          |-->| ORIGINAL FREE SURFACE
!| ZF             |-->| BATHYMETRY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CORRSL => CORRSL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: NEWSL
      TYPE(BIEF_OBJ) , INTENT(IN)    :: OLDSL,ZF
      TYPE(BIEF_MESH), INTENT(IN)    :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NELEM,NELMAX,IELM
!
!-----------------------------------------------------------------------
!
      NELEM = MESH%NELEM
      NELMAX= MESH%NELMAX
!
      IELM=OLDSL%ELM
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.11) THEN
!
        CALL CRSL11(NEWSL%R,OLDSL%R,ZF%R,MESH%IKLE%I,NELEM,NELMAX)
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELM.EQ.12) THEN
!
        CALL CRSL12(NEWSL%R,OLDSL%R,ZF%R,MESH%IKLE%I,NELEM,NELMAX)
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,11) IELM
11      FORMAT(1X,'CORRSL: UNKNOWN DISCRETIZATION :',I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
