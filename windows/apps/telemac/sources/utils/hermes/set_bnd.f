!                   ******************
                    SUBROUTINE SET_BND
!                   ******************
!
     &(FFORMAT,FID,TYPE_BND_ELT,NELEBD,NDP,IKLE,NPTFR,LIHBOR,LIUBOR,
     & LIVBOR,HBOR,UBOR,VBOR,CHBORD,
     & LITBOR,TBOR,ATBOR,BTBOR,COLOR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Writes the boundary information into the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| TYPE_BND_ELT   |-->| TYPE OF THE BOUNDARY ELEMENTS
!| NELEBD         |-->| NUMBER OF BOUNDARY ELEMENTS
!| NDP            |-->| NUMBER OF POINTS PER BOUNDARY ELEMENT
!| IKLE           |-->| CONNECTIVITY ARRAY FOR THE BOUNDARY ELEMENTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| CHBORD         |<--| FRICTION COEFFICIENT AT BOUNDARY
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| ATBOR,BTBOR    |<--| THERMAL EXCHANGE COEFFICIENTS.
!| COLOR          |<--| Boundary color of the boundary element
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
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
      INTEGER,          INTENT(IN)  :: FID
      INTEGER,          INTENT(IN)  :: TYPE_BND_ELT
      INTEGER,          INTENT(IN)  :: NELEBD
      INTEGER,          INTENT(IN)  :: NDP
      INTEGER,          INTENT(IN)  :: IKLE(NELEBD*NDP)
      INTEGER,          INTENT(IN)  :: NPTFR
      INTEGER,          INTENT(IN)  :: LIUBOR(NPTFR),LIVBOR(NPTFR)
      INTEGER,          INTENT(IN)  :: LIHBOR(NPTFR),LITBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)  :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)  :: HBOR(NPTFR),CHBORD(NPTFR)
      DOUBLE PRECISION, INTENT(IN)  :: TBOR(NPTFR),ATBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)  :: BTBOR(NPTFR)
      INTEGER,          INTENT(IN)  :: COLOR(NPTFR)
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL SET_BND_SRF(FID,TYPE_BND_ELT,NELEBD,NDP,IKLE,
     &                     LIHBOR,LIUBOR,
     &                     LIVBOR,HBOR,UBOR,VBOR,CHBORD,
     &                     LITBOR,TBOR,ATBOR,BTBOR,COLOR,IERR)
        CASE ('MED    ')
          CALL SET_BND_MED(FID,TYPE_BND_ELT,NELEBD,NDP,IKLE,NPTFR,
     &                     LIHBOR,LIUBOR,LIVBOR,LITBOR,IERR)
        CASE ('CGNS')
          CALL SET_BND_CGNS(FID,TYPE_BND_ELT,NELEBD,NDP,IKLE,
     &                     LIHBOR,LIUBOR,
     &                     LIVBOR,HBOR,UBOR,VBOR,CHBORD,
     &                     LITBOR,TBOR,ATBOR,BTBOR,IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*)
     &         'GET_SET_BND: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
