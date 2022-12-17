!                   *****************
                    SUBROUTINE GESTIO
!                   *****************
!
     &(U,V,C,T,AK,EP,VISCSA,UTILD,VTILD,CTILD,TTILD,AKTILD,EPTILD,
     & NUTILD,TRAC,PROPA,CONVV,ITURB,IETAPE)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MANAGES THE ALLOCATION OF ARRAYS
!+                DEPENDING ON THE SELECTED EQUATIONS.
!
!warning  DOES NOT WORK IF USES SCHEMES OTHER THAN
!+            CHARACTERISTICS FOR ADVECTION
!
!history  J-M HERVOUET (LNHE)
!+        17/08/1994
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
!history  R. ATA
!+        28/07/2016
!+        V7P2
!+   add SA turbulence model
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK             |<->| TURBULENT KINETIC ENERGY
!| AKTILD         |-->| TURBULENT KINETIC BEFORE CURRENT STEP
!| C              |<->| CELERITY
!| CONVV          |-->| LOGICAL ARRAY SAYING IF A VARIABLE IS ADVECTED OR NOT
!| CTILD          |-->| CELERITY BEFORE CURRENT STEP
!| EP             |<->| TURBULENT ENERGY DISSIPASSION
!| EPTILD         |-->| TURBULENT ENERGY DISSIPASSION BEFORE CURRENT STEP
!| IETAPE         |-->| FRACTIONAL STEP NUMBER
!| ITURB          |-->| TURBULENCE MODEL 1: LAMINAR, CONSTANT COEFFICIENT
!|                |   |                  2: MIXING LENGTH
!|                |   |                  3: K-EPSILON
!|                |   |                  6: SPALART-ALLMARAS
!| NUTILD         |-->| NU OF SA MODEL BEFORE CURRENT STEP
!| PROPA          |-->| IF PROPA=.FALSE. : NO PROPAGATION STEP.
!| T              |<->| BLOCK OF TRACERS
!| TRAC           |-->| LOGICAL, YES IF THERE ARE TRACERS
!| TTILD          |-->| BLOCK OF TRACERS BEFORE CURRENT STEP
!| U              |<->| X-COMPONENT OF VELOCITY
!| V              |<->| Y-COMPONENT OF VELOCITY
!| UTILD          |-->| X-COMPONENT OF VELOCITY BEFORE CURRENT STEP
!| VTILD          |-->| Y-COMPONENT OF VELOCITY BEFORE CURRENT STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: ITURB,IETAPE
      LOGICAL, INTENT(IN)           :: TRAC,CONVV(4),PROPA
      TYPE(BIEF_OBJ), INTENT(IN)    :: T,AK,EP,VISCSA
      TYPE(BIEF_OBJ), INTENT(INOUT) :: U,V,C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: UTILD,VTILD,CTILD,TTILD
      TYPE(BIEF_OBJ), INTENT(INOUT) :: AKTILD,EPTILD,NUTILD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!----------------------------------------------------------------------
!    ARRAYS DEPENDING ON THE SELECTED EQUATIONS
!-----------------------------------------------------------------------
!
!    ADVECTION
!
      IF(IETAPE.EQ.3) THEN
!
        IF(.NOT.CONVV(1)) THEN
          CALL OS( 'X=Y     ' , X=UTILD , Y=U )
          CALL OS( 'X=Y     ' , X=VTILD , Y=V )
        ENDIF
        IF(.NOT.CONVV(2)) THEN
          CALL OS( 'X=Y     ' , X=CTILD , Y=C )
        ENDIF
        IF(TRAC.AND.(.NOT.CONVV(3))) THEN
          CALL OS( 'X=Y     ' , X=TTILD , Y=T )
        ENDIF
        IF(ITURB.EQ.3.AND.(.NOT.CONVV(4))) THEN
          CALL OS( 'X=Y     ' , X=AKTILD , Y=AK )
          CALL OS( 'X=Y     ' , X=EPTILD , Y=EP )
        ENDIF
        IF(ITURB.EQ.6.AND.(.NOT.CONVV(4))) THEN
          CALL OS( 'X=Y     ' , X=NUTILD , Y=VISCSA )
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!    PROPAGATION
!
      IF(IETAPE.EQ.6) THEN
!
            IF(.NOT.PROPA) THEN
!
              CALL OS( 'X=Y     ' , X=U , Y=UTILD )
              CALL OS( 'X=Y     ' , X=V , Y=VTILD )
              CALL OS( 'X=Y     ' , X=C , Y=CTILD )
!
            ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
