!                   ***************************
                    SUBROUTINE OUTPUT_TELEMAC2D
!                   ***************************
!
     &(TIME)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    "WRAPPER" FOR DESIMP SO THAT OUTPUTS CAN BE DONE
!+                FROM WITHIN ESTEL-3D WHEN USING THE COUPLED MODEL
!+                RATHER THAN RELYING ON DESIMP (AND ITS FUNNY +
!+                RELIANCE ON LT) DIRECTLY.
!
!history  JP RENAUD
!+
!+        V5P7
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
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| TIME           |-->| TIME IN SECONDS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!----------------------------------------------------------------------
!
!     PREPARES THE RESULTS
!
      CALL PRERES_TELEMAC2D
!
!     OUTPUTS A STANDARD TIME STEP
!
      CALL BIEF_DESIMP(T2D_FILES(T2DRES)%FMT,VARSOR,
     &                 NPOIN,T2D_FILES(T2DRES)%LU,
     &                 TIME,1,1,1,
     &                 SORLEO,SORIMP,MAXVAR,TEXTE,0,0)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
