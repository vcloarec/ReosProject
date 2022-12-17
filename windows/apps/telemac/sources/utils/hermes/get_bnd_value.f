!                   ************************
                    SUBROUTINE GET_BND_VALUE
!                   ************************
!
     &(FFORMAT,FID,TYP_BND_ELEM,NELEBD,LIHBOR,LIUBOR,
     & LIVBOR,HBOR,UBOR,VBOR,CHBORD,TRAC,
     & LITBOR,TBOR,ATBOR,BTBOR,NPTFR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns an array containing the boundary type for each
!+        boundary point
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| TYP_BND_ELEM   |-->| TYPE OF THE BOUNDARY ELEMENTS
!| NELEBD         |-->| NUMBER OF BOUNDARY ELEMENTS
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| CHBORD         |<--| FRICTION COEFFICIENT AT BOUNDARY
!| TRAC           |-->| IF YES, THERE ARE TRACERS
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| ATBOR,BTBOR    |<--| THERMAL EXCHANGE COEFFICIENTS.
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE UTILS_SERAFIN
      USE UTILS_MED
      USE UTILS_CGNS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN)    :: FFORMAT
      INTEGER,          INTENT(IN)    :: FID
      INTEGER,          INTENT(IN)    :: TYP_BND_ELEM
      INTEGER,          INTENT(IN)    :: NELEBD
      INTEGER,          INTENT(IN)    :: NPTFR
      INTEGER,          INTENT(INOUT) :: LIHBOR(NPTFR)
      INTEGER,          INTENT(INOUT) :: LIUBOR(NPTFR),LIVBOR(NPTFR)
      INTEGER,          INTENT(INOUT) :: LITBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(*),VBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR),CHBORD(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: TBOR(*),ATBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: BTBOR(*)
      LOGICAL,          INTENT(IN)    :: TRAC
      INTEGER,          INTENT(OUT)   :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I
!
!-----------------------------------------------------------------------
!
      IF((TYP_BND_ELEM.EQ.TYPE_NULL).OR.(NELEBD.EQ.0)) RETURN
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL GET_BND_VALUE_SRF(FID, TYP_BND_ELEM,NPTFR,LIHBOR,LIUBOR,
     &                           LIVBOR,HBOR,UBOR,VBOR,CHBORD,TRAC,
     &                           LITBOR,TBOR,ATBOR,BTBOR, IERR)
        CASE ('MED    ')
          CALL GET_BND_VALUE_MED(FID,TYP_BND_ELEM,NELEBD,LIHBOR,
     &                           LIUBOR,LIVBOR,TRAC,LITBOR,
     &                           NPTFR,IERR)
          DO I=1,NPTFR
            HBOR(I) = 0.D0
            UBOR(I) = 0.D0
            VBOR(I) = 0.D0
            CHBORD(I) = 0.D0
            IF(TRAC) THEN
              TBOR(I) = 0.D0
              ATBOR(I) = 0.D0
              BTBOR(I) = 0.D0
            ENDIF
          ENDDO
        CASE ('CGNS   ')
          CALL GET_BND_VALUE_CGNS(FID, TYP_BND_ELEM,NPTFR,LIHBOR,LIUBOR,
     &                           LIVBOR,HBOR,UBOR,VBOR,CHBORD,TRAC,
     &                           LITBOR,TBOR,ATBOR,BTBOR, IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*)
     &         'GET_BND_VALUE: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
