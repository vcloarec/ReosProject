!                   ************************
                    SUBROUTINE FRICTION_QUAD
!                   ************************
!
     &(IKLE, NELEM, NELMAX, VEGETATION, NKFROT, CHESTR, NDEFMA, VCOEFF,
     & VEGLAW)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    COMPUTES THE FRICTION VECTOR FOR THE QUADRATIC ELEMENT.
!
!history  F. HUVELIN
!+        22/12/2004
!+
!+
!
!history  JACEK JANKOWSKI (BAW)
!+
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
!history  R. KOPMANN (BAW)
!+        19/11/2019
!+        V8P2
!+   Adaption to multiple vegetation laws
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHESTR         |<->| FRICTION COEFFICIENTS
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NDEFMA         |<->| DEFAULT MANNING COEFFICIENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NKFROT         |<->| LAW OF BOTTOM FRICTION FOR EVERY POINT
!| NPOIN          |-->| NUMBER OF POINTS
!| VCOEFF         |<->| COEFFICIENTS FOR VEGETATION LAW, UP TO 15
!| VEGETATION     |-->| IF YES, THERE IS VEGETATION FRICTION
!| VEGLAW         |<->| VEGETATION LAW
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,        INTENT(IN)    :: NELEM,NELMAX
      INTEGER,        INTENT(IN)    :: IKLE(NELMAX,6)
      LOGICAL,        INTENT(IN)    :: VEGETATION
      TYPE(BIEF_OBJ), INTENT(INOUT) :: NKFROT,CHESTR,NDEFMA
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VCOEFF,VEGLAW
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,K
!
!-----------------------------------------------------------------------
!
! IF THE 11 NODES HAVE THE SAME FRICTION LAW, INTERPOLATION A LA CG1113
! FOR THE VALUES AT 13 ADDITIONAL NODES IN THE MIDDLE OF THE EDGES
!
!        X(IKLE(IELEM,4)) = 0.5D0 * ( X(IKLE(IELEM,1))
!     &                             + X(IKLE(IELEM,2)) )
!        X(IKLE(IELEM,5)) = 0.5D0 * ( X(IKLE(IELEM,2))
!     &                             + X(IKLE(IELEM,3)) )
!        X(IKLE(IELEM,6)) = 0.5D0 * ( X(IKLE(IELEM,3))
!     &                             + X(IKLE(IELEM,1)) )
!
! WELL, IF THE THE FRICTION LAWS DIFFER, TAKE THE VALUE ON THE PREVIOUS
! NODE BY CIRCUMVENTING THE ELEMENT...
!
!        X(IKLE(IELEM,4)) = X(IKLE(IELEM,1))
!        X(IKLE(IELEM,5)) = X(IKLE(IELEM,2))
!        X(IKLE(IELEM,6)) = X(IKLE(IELEM,3))
!
! ASSUMED THE TRIVIAL CASE OF -ONE- ZONE ONLY IS NATURALLY EXCLUDED
!
      DO IELEM = 1,NELEM
        IF(NKFROT%I(IKLE(IELEM,1)).EQ.NKFROT%I(IKLE(IELEM,2))) THEN
          NKFROT%I(IKLE(IELEM,4)) = NKFROT%I(IKLE(IELEM,1))
          CHESTR%R(IKLE(IELEM,4)) =
     &      0.5D0*(CHESTR%R(IKLE(IELEM,1))+CHESTR%R(IKLE(IELEM,2)))
          NDEFMA%R(IKLE(IELEM,4)) =
     &      0.5D0*(NDEFMA%R(IKLE(IELEM,1))+NDEFMA%R(IKLE(IELEM,2)))
          IF(.NOT.VEGETATION) 
     &       VEGLAW%I(IKLE(IELEM,4)) = VEGLAW%I(IKLE(IELEM,1))
        ELSE
          NKFROT%I(IKLE(IELEM,4)) = NKFROT%I(IKLE(IELEM,1))
          CHESTR%R(IKLE(IELEM,4)) = CHESTR%R(IKLE(IELEM,1))
          NDEFMA%R(IKLE(IELEM,4)) = NDEFMA%R(IKLE(IELEM,1))
          IF(.NOT.VEGETATION)
     &       VEGLAW%I(IKLE(IELEM,4)) = VEGLAW%I(IKLE(IELEM,1))
        ENDIF
        IF(NKFROT%I(IKLE(IELEM,2)).EQ.NKFROT%I(IKLE(IELEM,3))) THEN
          NKFROT%I(IKLE(IELEM,5)) = NKFROT%I(IKLE(IELEM,2))
          CHESTR%R(IKLE(IELEM,5)) =
     &      0.5D0*(CHESTR%R(IKLE(IELEM,2))+CHESTR%R(IKLE(IELEM,3)))
          NDEFMA%R(IKLE(IELEM,5)) =
     &      0.5D0*(NDEFMA%R(IKLE(IELEM,2))+NDEFMA%R(IKLE(IELEM,3)))
          IF(.NOT.VEGETATION)
     &       VEGLAW%I(IKLE(IELEM,5)) = VEGLAW%I(IKLE(IELEM,2))
        ELSE
          NKFROT%I(IKLE(IELEM,5)) = NKFROT%I(IKLE(IELEM,2))
          CHESTR%R(IKLE(IELEM,5)) = CHESTR%R(IKLE(IELEM,2))
          NDEFMA%R(IKLE(IELEM,5)) = NDEFMA%R(IKLE(IELEM,2))
          IF(.NOT.VEGETATION)
     &       VEGLAW%I(IKLE(IELEM,5)) = VEGLAW%I(IKLE(IELEM,2))
        ENDIF
        IF(NKFROT%I(IKLE(IELEM,3)).EQ.NKFROT%I(IKLE(IELEM,1))) THEN
          NKFROT%I(IKLE(IELEM,6)) = NKFROT%I(IKLE(IELEM,3))
          CHESTR%R(IKLE(IELEM,6)) =
     &      0.5D0*(CHESTR%R(IKLE(IELEM,3))+CHESTR%R(IKLE(IELEM,1)))
          NDEFMA%R(IKLE(IELEM,6)) =
     &      0.5D0*(NDEFMA%R(IKLE(IELEM,3))+NDEFMA%R(IKLE(IELEM,1)))
          IF(.NOT.VEGETATION)
     &       VEGLAW%I(IKLE(IELEM,6)) = VEGLAW%I(IKLE(IELEM,3))
        ELSE
          NKFROT%I(IKLE(IELEM,6)) = NKFROT%I(IKLE(IELEM,3))
          CHESTR%R(IKLE(IELEM,6)) = CHESTR%R(IKLE(IELEM,3))
          NDEFMA%R(IKLE(IELEM,6)) = NDEFMA%R(IKLE(IELEM,3))
          IF(.NOT.VEGETATION)
     &       VEGLAW%I(IKLE(IELEM,6)) = VEGLAW%I(IKLE(IELEM,3))
        ENDIF
      ENDDO
      ! FOR VEGETATION
      IF(VEGETATION) THEN
        DO IELEM = 1,NELEM
          IF(VEGLAW%I(IKLE(IELEM,1)).EQ.VEGLAW%I(IKLE(IELEM,2))) THEN
            VEGLAW%I(IKLE(IELEM,4)) = VEGLAW%I(IKLE(IELEM,1))
            DO K = 1,15
              VCOEFF%ADR(K)%P%R(IKLE(IELEM,4)) =
     &          0.5D0*(VCOEFF%ADR(K)%P%R(IKLE(IELEM,1)) +
     &                 VCOEFF%ADR(K)%P%R(IKLE(IELEM,2)))
            ENDDO
          ELSE
            VEGLAW%I(IKLE(IELEM,4)) = VEGLAW%I(IKLE(IELEM,1))
            DO K = 1,15
              VCOEFF%ADR(K)%P%R(IKLE(IELEM,4)) =
     &          VCOEFF%ADR(K)%P%R(IKLE(IELEM,1))
            ENDDO
          ENDIF
          IF(VEGLAW%I(IKLE(IELEM,2)).EQ.VEGLAW%I(IKLE(IELEM,3))) THEN
            VEGLAW%I(IKLE(IELEM,5)) = VEGLAW%I(IKLE(IELEM,2))
            DO K = 1,15
              VCOEFF%ADR(K)%P%R(IKLE(IELEM,5)) =
     &          0.5D0*(VCOEFF%ADR(K)%P%R(IKLE(IELEM,2)) +
     &                 VCOEFF%ADR(K)%P%R(IKLE(IELEM,3)))
            ENDDO
          ELSE
            VEGLAW%I(IKLE(IELEM,5)) = VEGLAW%I(IKLE(IELEM,2))
            DO K = 1,15
              VCOEFF%ADR(K)%P%R(IKLE(IELEM,5)) =
     &          VCOEFF%ADR(K)%P%R(IKLE(IELEM,2))
            ENDDO
          ENDIF
          IF(VEGLAW%I(IKLE(IELEM,3)).EQ.VEGLAW%I(IKLE(IELEM,1))) THEN
            VEGLAW%I(IKLE(IELEM,6)) = VEGLAW%I(IKLE(IELEM,3))
            DO K = 1,15
              VCOEFF%ADR(K)%P%R(IKLE(IELEM,6)) =
     &          0.5D0*(VCOEFF%ADR(K)%P%R(IKLE(IELEM,3)) +
     &                 VCOEFF%ADR(K)%P%R(IKLE(IELEM,1)))
            ENDDO
          ELSE
            VEGLAW%I(IKLE(IELEM,6)) = VEGLAW%I(IKLE(IELEM,3))
            DO K = 1,15
              VCOEFF%ADR(K)%P%R(IKLE(IELEM,6)) =
     &          VCOEFF%ADR(K)%P%R(IKLE(IELEM,3))
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
