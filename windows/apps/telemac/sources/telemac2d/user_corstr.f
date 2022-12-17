!                   **********************
                    SUBROUTINE USER_CORSTR
!                   **********************
!
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    USER CORRECTS THE FRICTION COEFFICIENT ON THE BOTTOM
!+                WHEN IT IS VARIABLE IN TIME.
!
!history  J-M HERVOUET (LNHE)
!+        17/08/1994
!+        V5P6
!+
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!     !2D: EXAMPLE FOR TELEMAC-2D
!     !3D: EXAMPLE FOR TELEMAC-3D
!
!2D   USE DECLARATIONS_TELEMAC2D
!3D   USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!2D   DO I = 1 , NPOIN
!2D     IF(AT.GT.1200.D0) THEN
!2D       CHESTR%R(I) = 40.D0
!2D     ELSE
!2D       CHESTR%R(I) = 60.D0
!2D     ENDIF
!2D   ENDDO
!
!-----------------------------------------------------------------------
!
!3D   DO I = 1 , NPOIN2
!3D     IF(AT.GT.1200.D0) THEN
!3D       RUGOF%R(I) = 40.D0
!3D     ELSE
!3D       RUGOF%R(I) = 60.D0
!3D     ENDIF
!3D   ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
