!                   *****************************
                    DOUBLE PRECISION FUNCTION VIT
!                   *****************************
!
     &( I, N )
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    PRESCRIBES THE VELOCITY FOR VEL IMPOSED LIQUID BOUNDARIES.
!
!history  J-M HERVOUET (LNHE)
!+        17/08/1994
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
!history  C. COULET (ARTELIA GROUP)
!+        08/11/2011
!+        V6P2
!+   Modification size FCT due to modification of TRACER numbering
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| NUMBER OF LIQUID BOUNDARY
!| N              |-->| GLOBAL NUMBER OF POINT
!|                |   | IN PARALLEL NUMBER IN THE ORIGINAL MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_VIT => VIT
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: I, N
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=9) FCT
!
!     IF THE LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OKVIT REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OKVIT SET     TO .FALSE.
!
      IF(OKVIT(I).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE VIT(1), VIT(2), ETC, VIT(99), DEPENDING ON I
        FCT='VIT(     '
        IF(I.LT.10) THEN
          WRITE(FCT(5:5),FMT='(I1)') I
          FCT(6:6)=')'
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(5:6),FMT='(I2)') I
          FCT(7:7)=')'
        ELSE
          WRITE(LU,*) 'VIT NOT PROGRAMMED FOR MORE
     &                 THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_FRLIQ(VIT,FCT,AT,T2D_FILES(T2DIMP)%LU,
     &                      ENTET,OKVIT(I))
!
      ENDIF
!
      IF(.NOT.OKVIT(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
!
!     VIT IS TAKEN FROM THE STEERING FILE, BUT MAY BE CHANGED
!
        IF(NVITES.GE.I) THEN
          VIT = VITES(I)
!
          ! USER UPDATE
          CALL USER_VIT(VIT, I, N)
        ELSE
          WRITE(LU,201) I
201       FORMAT(1X,/,1X,'VIT : MORE PRESCRIBED VELOCITIES ARE REQUIRED'
     &             ,/,1X,'      IN THE PARAMETER FILE'
     &             ,/,1X,'      AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
