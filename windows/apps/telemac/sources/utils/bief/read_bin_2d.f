!                    *************************
                      SUBROUTINE READ_BIN_2D
!                    *************************
!
     &( Q ,VARNAME,AT,NFIC,FFORMAT,NPOIN,FOUND,OFFSET)
!
!***********************************************************************
! BIEF   V7P2                                       01/09/2016
!***********************************************************************
!
!brief    READS AND INTERPOLATES VALUES FROM A BINARY
!+        FILE ON THE WHOLE 2D MESH.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| NFIC           |-->| LOGICAL UNIT OF FILE
!| Q              |<--| ARRAY WHERE THE VARIABLE IS STORED
!| FOUND          |<--| IF FALSE: VARIABLE NOT FOUND
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_READ_BIN_2D => READ_BIN_2D
      USE INTERFACE_HERMES
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=16), INTENT(IN)      :: VARNAME
      INTEGER         , INTENT(IN)       :: NFIC,NPOIN
      DOUBLE PRECISION, INTENT(IN)       :: AT
      DOUBLE PRECISION, INTENT(INOUT)    :: Q(NPOIN)
      CHARACTER(LEN=8), INTENT(IN)       :: FFORMAT
      LOGICAL         , INTENT(OUT)      :: FOUND
      DOUBLE PRECISION, INTENT(IN)       :: OFFSET
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: J,NPOINATM,IERR
!
!-----------------------------------------------------------------------
!
!     INITIALISE THE ARRAY TO BE FILLED
      DO J=1,NPOIN
        Q(J) = 0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
!     READ THE FILE
!     AND STORE VARIABLE VALUE AT ALL 2D POINTS
!
!-----------------------------------------------------------------------
!
!     GET THE NUMBER OF POINTS IN THE FILE
!
      CALL GET_MESH_NPOIN(FFORMAT,NFIC,TRIANGLE_ELT_TYPE,
     &                    NPOINATM,IERR)
      CALL CHECK_CALL(IERR,"READ_BIN_FRILQ: GET_MESH_NPOIN")
      IF(NPOINATM.NE.NPOIN) THEN
        WRITE(LU,*) "READ_BIN_2D: ",
     &              "INCORRECT NUMBER OF POINTS IN THE FILE"
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     STORE THE VARIABLE VALUES IN Q
!
      CALL FIND_VARIABLE(FFORMAT,NFIC,VARNAME,Q,NPOIN,IERR,
     &                 TIME=AT,EPS_TIME=1.D-6,OFFSET=OFFSET)
      IF(IERR.EQ.HERMES_VAR_UNKNOWN_ERR) THEN
        FOUND = .FALSE.
      ELSE
        FOUND = .TRUE.
      ENDIF
      CALL CHECK_CALL(IERR,"READ_BIN_2D: FIND_VARIABLE")

      RETURN
      END
