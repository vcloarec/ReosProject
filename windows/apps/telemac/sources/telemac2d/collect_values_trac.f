!                       ******************************
                        SUBROUTINE COLLECT_VALUES_TRAC
!                       ******************************
!
!***********************************************************************
! TELEMAC2D   V7P1                                   05/10/2015
!***********************************************************************
!
!brief    COLLECT TRACER VALUES ON WEIRS NODES
!+
!
!+
!history  C.COULET (ARTELIA)
!+        05/10/2015
!+        V7P1
!+   Creation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_PARALLEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IK,IL,II,I,J,K,IP
!
      INTEGER SEND_REQ(100),RECV_REQ(100)
!
!-----------------------------------------------------------------------
!
      IF (NCSIZE.GT.0) THEN
!
        SEND_REQ(:) = 0
        RECV_REQ(:) = 0
!
!       MESSAGE TAG UPDATE
!
        IF(MSG_TAG_TRA.LT.1000000) THEN
          MSG_TAG_TRA = MSG_TAG_TRA + 1
        ELSE
          MSG_TAG_TRA = 5001
        ENDIF
!
!
!== RECEIVE STEP
!
        DO IL=1,N_NGHB_W_NODES
          IP = WNODES_PROC(IL)%NUM_NEIGH
          IK = WNODES_PROC(IL)%NB_NODES
!         AD: DISTINGUISHING THE NUMBER OF ELEMENTS AND THEIR SIZE
          CALL P_READ(WT_BUF_RECV(1:LEN_WT_BUF,IL),NTRAC*IK,8,
     &                 IP,MSG_TAG_TRA,RECV_REQ(IL))
        ENDDO
!
!== SEND STEP
!
        DO IL=1,N_NGHB_WN_SEND
          IP = WN_SEND_PROC(IL)%NUM_NEIGH
          IK = WN_SEND_PROC(IL)%NB_NODES
!
!** INITIALISES THE COMMUNICATION ARRAYS
!
          K = 1
          DO I=1,IK
            II = WN_SEND_PROC(IL)%NUM_LOC(I)
            DO J=1, NTRAC
              WT_BUF_SEND(K+J-1,IL) = T%ADR(J)%P%R(II)
            ENDDO
            K = K+NTRAC
          ENDDO
!         AD: DISTINGUISHING THE NUMBER OF ELEMENTS AND THEIR SIZE
          CALL P_WRITE(WT_BUF_SEND(1:LEN_WT_BUF,IL),NTRAC*IK,8,
     &                 IP,MSG_TAG_TRA,SEND_REQ(IL))
!
        ENDDO
!
!== WAIT RECEIVED MESSAGES (POSSIBILITY OF COVERING)
!
        CALL P_WAIT_PARACO(RECV_REQ,N_NGHB_W_NODES)
!
        DO IL=1,N_NGHB_W_NODES
          IK = WNODES_PROC(IL)%NB_NODES
          IP = WNODES_PROC(IL)%NUM_NEIGH
!
          K = 1
          DO I=1,IK
            II = WNODES_PROC(IL)%LIST_NODES(I)
            DO J=1, NTRAC
              WNODES(II)%TRAC0(J)=WT_BUF_RECV(K+J-1,IL)
            ENDDO
            K = K + NTRAC
          ENDDO
!
        ENDDO
!
!== WAIT SENT MESSAGES
!
        CALL P_WAIT_PARACO(SEND_REQ,N_NGHB_WN_SEND)
!
!-----------------------------------------------------------------------
      ELSE
        DO IL=1,N_NGHB_WN_SEND
          IK = WN_SEND_PROC(IL)%NB_NODES
          DO I=1,IK
            II = WN_SEND_PROC(IL)%NUM_LOC(I)
            DO J=1, NTRAC
              WNODES(I)%TRAC0(J) = T%ADR(J)%P%R(II)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!
      RETURN
      END
