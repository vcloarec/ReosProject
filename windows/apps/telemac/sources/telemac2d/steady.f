!                   *****************
                    SUBROUTINE STEADY
!                   *****************
!
     &(H1,H2,NPH,U1,U2,NPU,V1,V2,NPV,NTRAC,T1,T2,NPT,CRIPER,ARRET)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CHECKS IF A STEADY STATE IS REACHED.
!
!warning  ARRET IS NOT INITIALISED
!
!history  J-M HERVOUET (LNHE)
!+        05/09/2007
!+        V5P8
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
!| ARRET          |<--| .TRUE. IF STEADY STATE REACHED
!| CRIPER         |-->| STOP CRITERIA IN FOLLOWING ORDER:
!|                |   | H , U , V , T
!| H1             |-->| DEPTH TO BE COMPARED WITH H2
!| H2             |-->| DEPTH TO BE COMPARED WITH H1
!| NPH            |-->| NUMBER OF POINTS FOR DEPTH
!| NPT            |-->| NUMBER OF POINTS FOR TRACERS
!| NPU            |-->| NUMBER OF POINTS FOR VELOCITY U
!| NPV            |-->| NUMBER OF POINTS FOR VELOCITY V
!| NTRAC          |-->| NUMBER OF TRACERS
!| T1             |-->| TRACERS TO BE COMPARED WITH T2
!| T2             |-->| TRACERS TO BE COMPARED WITH T1
!| U1             |-->| VELOCITY U TO BE COMPARED WITH U2
!| U2             |-->| VELOCITY U TO BE COMPARED WITH U1
!| V1             |-->| VELOCITY V TO BE COMPARED WITH V2
!| V2             |-->| VELOCITY V TO BE COMPARED WITH V1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NPH,NPU,NPV,NPT,NTRAC
      LOGICAL, INTENT(INOUT)       :: ARRET
      DOUBLE PRECISION, INTENT(IN) :: H1(NPH),H2(NPH),U1(NPU),U2(NPU)
      DOUBLE PRECISION, INTENT(IN) :: V1(NPV),V2(NPV)
      DOUBLE PRECISION, INTENT(IN) :: CRIPER(3)
      TYPE(BIEF_OBJ)  , INTENT(IN) :: T1,T2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ITRAC
!
!-----------------------------------------------------------------------
!
!     CHECKS THE WATER DEPTH
!
      DO I = 1 , NPH
        IF(ABS(H1(I)-H2(I)).GT.CRIPER(1)) GO TO 1000
      ENDDO
!
!-----------------------------------------------------------------------
!
!     CHECKS U
!
      DO I = 1 , NPU
        IF(ABS(U1(I)-U2(I)).GT.CRIPER(2)) GO TO 1000
      ENDDO
!
!-----------------------------------------------------------------------
!
!     CHECKS V
!
      DO I = 1 , NPV
        IF(ABS(V1(I)-V2(I)).GT.CRIPER(2)) GO TO 1000
      ENDDO
!
!-----------------------------------------------------------------------
!
!  CHECKS THE TRACER
!
      IF(NTRAC.GT.0) THEN
!
      DO ITRAC=1,NTRAC
        DO I = 1 , NPT
          IF(ABS(T1%ADR(ITRAC)%P%R(I)
     &          -T2%ADR(ITRAC)%P%R(I)).GT.CRIPER(3)) GO TO 1000
        ENDDO
      ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      ARRET=.TRUE.
      WRITE(LU,200)
200   FORMAT(/,1X,'THE STEADY STATE HAS BEEN REACHED')
!
!-----------------------------------------------------------------------
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
