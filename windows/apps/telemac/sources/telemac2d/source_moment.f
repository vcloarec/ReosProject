!                   ************************
                    SUBROUTINE SOURCE_MOMENT
!                   ************************
!
     &(UA,YASMO)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief    COMPUTES SOURCE TERMS FOR MOMENTUM EQUATION (CASE OF FINITE
!!          VOLUMES). ARE CONSIDERED:
!!           - FRICTION TERM.
!!           - CORIOLIS FORCE
!!           - ALL OTHER TERMS ARE COMPUTED BEFORE IN PROSOU
!
!>@history  R. ATA (EDF CHATOU LAB)
!!
!!        CREATION
!!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  UA        (H,HU,HV) AT TN+1
!>@param  [in]      YASMO     LOGIC: IF YES CONIDER REMAINING FORCES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_SOUMOM => SOURCE_MOMENT
      USE DECLARATIONS_TELEMAC2D, ONLY:CF,NPOIN,LT,NKFROT,SPHERI,
     &                            CORIOL,DT,FCOR,HN,QU,QV,
     &                            MESH,FU,FV,NONNEWTMODEL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN)             :: YASMO
      DOUBLE PRECISION, INTENT(INOUT) :: UA(3,NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS
      DOUBLE PRECISION AKAP,SM1,SM2,DETER
      DOUBLE PRECISION A,A2SUR4,ASUR2,PI,WROT
!
!-----------------------------------------------------------------------
!
      SM1  = 0.D0
      SM2  = 0.D0
      A    = 0.D0
      AKAP = 1.D0
!
      IF(CORIOL)THEN
        A  = FCOR*DT
        PI = 4.D0 * ATAN(1.D0)
        IF(SPHERI)THEN
          WROT = 2.D0*PI/86164.D0
          A    = 2.D0*WROT*DT
          IF(LT.EQ.1) THEN
            WRITE(LU,12)
          ENDIF
12        FORMAT(1X,'SOURCE_MOMENT : IN SPHERICAL COORDINATES, THE ',/,
     &           1X,'    CORIOLIS  PARAMETER DEPENDS ON THE LATITUDE',/,
     &           1X,'    THE KEY WORD ''CORIOLIS COEFFICIENT''',/,
     &           1X,'    IS CONSEQUENTLY IGNORED.')

        ENDIF
      ENDIF
!
      A2SUR4    = 0.25D0*(A**2)
      ASUR2     = 0.5D0 *A
      DO IS =1,NPOIN
!       INITIALIZE SM1 AND SM2
        SM1 = UA(2,IS)
        SM2 = UA(3,IS)
!
!       1-  FRICTION - AKAP WILL CONTAIN THE FRICTION SOURCE TERM
!
        IF((HN%R(IS).LE.1.D-12).OR.
     &     (UA(1,IS).LE.1.D-12).OR.
     &     (CF%R(IS).LE.1.D-12)) THEN
          AKAP = 1.D0
        ELSE
          IF(NKFROT%I(IS).NE.0) THEN
            AKAP = 1.D0 + CF%R(IS)*DT*SQRT(QU%R(IS)**2+QV%R(IS)**2)/
     &           (2.D0*HN%R(IS)*UA(1,IS))
          ELSE
            AKAP = 1.D0
            CF%R(IS) = 0.D0
          ENDIF
        ENDIF
!
!       1-B NON NEWTONIAN
        IF(NONNEWTMODEL.NE.0) THEN
          CALL NONNEWT_FV(IS,UA,AKAP)
        ENDIF
!
!       2- CORIOLIS
!
        IF(CORIOL)THEN
          IF(SPHERI)THEN
            SM1 = UA(2,IS) + A*MESH%SINLAT%R(IS)*QV%R(IS)
            SM2 = UA(3,IS) - A*MESH%SINLAT%R(IS)*QU%R(IS)
          ELSE
            SM1 = UA(2,IS) + A*QV%R(IS)
            SM2 = UA(3,IS) - A*QU%R(IS)
          ENDIF
        ENDIF
!
!       3- REMAINNING FORCES
!
!       warning: not consistant with semi implicit in time
!                to adapt if not satisfactory
        IF(YASMO)THEN
          SM1 = SM1 + 0.5D0*DT*(HN%R(IS)+UA(1,IS))*FU%R(IS)
          SM2 = SM2 + 0.5D0*DT*(HN%R(IS)+UA(1,IS))*FV%R(IS)
        ENDIF
!
!       DETERMINANT OF THE CRAMER SYSTEM
!
        DETER= A2SUR4+AKAP**2
!
!       FINAL VALUES OF HU AND HV
!
        UA(2,IS)= (AKAP*SM1 + ASUR2*SM2)/DETER
        UA(3,IS)= (AKAP*SM2 - ASUR2*SM1)/DETER
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
