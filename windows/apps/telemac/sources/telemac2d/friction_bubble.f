!                   **************************
                    SUBROUTINE FRICTION_BUBBLE
!                   **************************
!
     &(IKLE, NPOIN, NELEM, NELMAX, VEGETATION, NKFROT, CHESTR, NDEFMA,
     & VCOEFF, VEGLAW)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    COMPUTES THE FRICTION VECTOR FOR THE QUASI-BUBBLE ELEMENT.
!
!history  F. HUVELIN
!+        22/12/2004
!+
!+
!
!history  J-M HERVOUET (LNHE)
!+
!+        V5P5
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
!| VEGETATION     |-->| IF YES, THERE IS VEGETATION FRICTION
!| VCOEFF         |<->| COEFFICIENTS FOR VEGETATION LAW, UP TO 15
!| VEGLAW         |<->| VEGETATION LAW
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: IKLE
      INTEGER,        INTENT(IN)    :: NPOIN,NELEM,NELMAX
      LOGICAL,        INTENT(IN)    :: VEGETATION
      TYPE(BIEF_OBJ), INTENT(INOUT) :: NKFROT,CHESTR,NDEFMA
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VCOEFF,VEGLAW
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,I1,I2,I3,K
!
!-----------------------------------------------------------------------
!
      DO I = NPOIN + 1, NPOIN + NELEM
!
        I1 = IKLE%I(I - NPOIN           )
        I2 = IKLE%I(I - NPOIN +   NELMAX)
        I3 = IKLE%I(I - NPOIN + 2*NELMAX)
!
        ! COMPUTING THE VALUES OF THE MIDDLE-NODE
        ! ---------------------------------------
        IF (NKFROT%I(I1).EQ.NKFROT%I(I2)) THEN
          ! THE 3 NODES HAVE THE SAME LAW !
          IF (NKFROT%I(I1).EQ.NKFROT%I(I3)) THEN
!
            NKFROT%I(I) = NKFROT%I(I1)
            CHESTR%R(I) = (CHESTR%R(I3) + CHESTR%R(I2) + CHESTR%R(I1))
     &                  / 3.D0
            NDEFMA%R(I) = (NDEFMA%R(I3) + NDEFMA%R(I2) + NDEFMA%R(I1))
     &                  / 3.D0
            IF(.NOT.VEGETATION) VEGLAW%I(I) = VEGLAW%I(I1)
!
          ! THE NODES "1" AND "2" HAVE THE SAME LAW !
          ELSE
            NKFROT%I(I) = NKFROT%I(I1)
            CHESTR%R(I) = (CHESTR%R(I2) + CHESTR%R(I1))/2.D0
            NDEFMA%R(I) = (NDEFMA%R(I2) + NDEFMA%R(I1))/2.D0
            IF(.NOT.VEGETATION) VEGLAW%I(I) = VEGLAW%I(I1)
!
          ENDIF
!
        ! THE NODES "2" AND "3" HAVE THE SAME LAW !
        ELSEIF(NKFROT%I(I2).EQ.NKFROT%I(I3)) THEN
!
          NKFROT%I(I) = NKFROT%I(I2)
          CHESTR%R(I) = (CHESTR%R(I3) + CHESTR%R(I2))/2.D0
          NDEFMA%R(I) = (NDEFMA%R(I3) + NDEFMA%R(I2))/2.D0
          IF(.NOT.VEGETATION) VEGLAW%I(I) = VEGLAW%I(I2)
!
        ! THE 3 NODES HAVE DIFFERENT LAWS: VALUE OF THE NODE "1" KEPT !
        ELSE
          NKFROT%I(I) = NKFROT%I(I1)
          CHESTR%R(I) = CHESTR%R(I1)
          NDEFMA%R(I) = NDEFMA%R(I1)
          IF(.NOT.VEGETATION) VEGLAW%I(I) = VEGLAW%I(I1)
!
        ENDIF
        IF (VEGETATION) THEN
        ! COMPUTING THE VALUES OF THE MIDDLE-NODE
        ! ---------------------------------------
          IF (VEGLAW%I(I1).EQ.VEGLAW%I(I2)) THEN
          ! THE 3 NODES HAVE THE SAME VEGETATION LAW !
            IF (VEGLAW%I(I1).EQ.VEGLAW%I(I3)) THEN
!
              VEGLAW%I(I) = VEGLAW%I(I1)
              DO K = 1,15
                VCOEFF%ADR(K)%P%R(I) =
     &          ( VCOEFF%ADR(K)%P%R(I3) + VCOEFF%ADR(K)%P%R(I2)
     &                          + VCOEFF%ADR(K)%P%R(I1) ) /3.D0
              ENDDO
!
          ! THE NODES "1" AND "2" HAVE THE SAME VEGETATION LAW !
            ELSE
              VEGLAW%I(I) = VEGLAW%I(I1)
              DO K = 1,15
                VCOEFF%ADR(K)%P%R(I) = ( VCOEFF%ADR(K)%P%R(I2) + 
     &                               VCOEFF%ADR(K)%P%R(I1))/2.D0
              ENDDO
            ENDIF
!
        ! THE NODES "2" AND "3" HAVE THE SAME VEGETATION LAW !
          ELSEIF(VEGLAW%I(I2).EQ.VEGLAW%I(I3)) THEN
            VEGLAW%I(I) = VEGLAW%I(I2)
            DO K = 1,15
              VCOEFF%ADR(K)%P%R(I) = ( VCOEFF%ADR(K)%P%R(I3) +
     &                              VCOEFF%ADR(K)%P%R(I2))/2.D0
            ENDDO
!
        ! THE 3 NODES HAVE DIFFERENT LAWS: VALUE OF THE NODE "1" KEPT !
          ELSE
            VEGLAW%I(I) = VEGLAW%I(I1)
            DO K = 1,15
              VCOEFF%ADR(K)%P%R(I) = VCOEFF%ADR(K)%P%R(I1)
            ENDDO
          ENDIF
        ENDIF !VEGETATION
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
