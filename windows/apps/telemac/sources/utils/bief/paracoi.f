!                   ******************
                    SUBROUTINE PARACOI
!                   ******************
!
     &(V1,V2,V3,NPOIN,ICOM,IAN,NPLAN,NB_NEIGHB,NB_NEIGHB_PT,LIST_SEND,
     & NH_COM,DIMNHCOM,BUF_SEND,BUF_RECV,DIMBUF)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    ASSEMBLES INTEGER DATA SHARED BY SEVERAL PROCESSORS.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        19/11/2013
!+        V7P0
!+     A mere copy of PARACO by Pascal Vezolles for integers.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BUF_RECV       |<->| BUFFER FOR RECEIVING DATA
!| BUF_SEND       |<->| BUFFER FOR SENDING DATA
!| DIMBUF         |-->| FIRST DIMENSION OF BUFFERS
!| DIMNHCOM       |---|
!| IAN            |-->| NUMBER OF VECTORS TO BE CONDIDERED (1, 2 OR 3)
!| ICOM           |-->| OPTION OF COMMUNICATION :
!|                |   | = 1 : VALUE WITH MAXIMUM ABSOLUTE VALUE
!|                |   | = 2 : CONTRIBUTIONS ADDED
!|                |   | = 3 : MAXIMUM CONTRIBUTION RETAINED
!|                |   | = 4 : MINIMUM CONTRIBUTION RETAINED
!| LIST_SEND      |-->| LIST OF PROCESSORS NUMBERS
!| NB_NEIGHB      |-->| NUMBER OF NEIGHBOURING SUB-DOMAINS
!| NB_NEIGHB_PT   |-->| NUMBER OF POINTS SHARED WITH A SUB-DOMAIN
!| NH_COM         |-->| NH_COM(I,IL) : GLOBAL NUMBER IN THIS
!|                |   | SUB-DOMAIN OF THE POINT NUMBER I IN THE LIST
!|                |   | OF POINTS SHARED WITH PROCESSOR NUMBER IL
!|                |   | WHOSE REAL NUMBER IS LIST_SEND(IL)
!| NPLAN          |-->| SECOND DIMENSION OF V1,V2,V3
!| NPOIN          |-->| FIRST DIMENSION OF V1,V2,V3
!| V1             |<->| INTEGER VECTOR TO BE COMPLETED
!| V2             |<->| INTEGER VECTOR TO BE COMPLETED
!| V3             |<->| INTEGER VECTOR TO BE COMPLETED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE DECLARATIONS_TELEMAC, ONLY : PARACOI_MSG_TAG
      USE INTERFACE_PARALLEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,ICOM,IAN,NPLAN,NB_NEIGHB
      INTEGER, INTENT(IN) :: DIMNHCOM,DIMBUF
      INTEGER, INTENT(IN) :: NB_NEIGHB_PT(NB_NEIGHB)
      INTEGER, INTENT(IN) :: LIST_SEND(NB_NEIGHB),NH_COM(DIMNHCOM,*)
!
      INTEGER, INTENT(INOUT) :: BUF_SEND(DIMBUF,*)
      INTEGER, INTENT(INOUT) :: BUF_RECV(DIMBUF,*)
      INTEGER, INTENT(INOUT) :: V1(NPOIN,NPLAN)
      INTEGER, INTENT(INOUT) :: V2(NPOIN,NPLAN)
      INTEGER, INTENT(INOUT) :: V3(NPOIN,NPLAN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IKA,IL,II,I,J,K,IPA
!
      INTRINSIC ABS
!
      INTEGER SEND_REQ(100),RECV_REQ(100)
!
!----------------------------------------------------------------------
!
      IF(IAN.NE.1.AND.IAN.NE.2.AND.IAN.NE.3) THEN
        WRITE(LU,*) 'FALSCHE FREIWERTZAHL BEI KOMMUNIKATION',IAN,
     &              ' AUF PROZESSOR',IPID
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     MESSAGE TAG UPDATE
!
      IF(PARACOI_MSG_TAG.LT.1000000) THEN
        PARACOI_MSG_TAG = PARACOI_MSG_TAG + 1
      ELSE
        PARACOI_MSG_TAG = 5001
      ENDIF
!
!== RECEIVE STEP
!
      DO IL=1,NB_NEIGHB
        IKA = NB_NEIGHB_PT(IL)
        IPA = LIST_SEND(IL)
        CALL P_READ(BUF_RECV(1:DIMBUF,IL),IAN*IKA*NPLAN,
     &                IPA,PARACOI_MSG_TAG,RECV_REQ(IL))
      ENDDO
!
!== SEND STEP
!
      DO IL=1,NB_NEIGHB
        IKA = NB_NEIGHB_PT(IL)
        IPA = LIST_SEND(IL)
!
!** INITIALISES THE COMMUNICATION ARRAYS
!
        K = 1
        IF(IAN.EQ.3) THEN
          DO J=1,NPLAN
            DO I=1,IKA
              II=NH_COM(I,IL)
              BUF_SEND(K,IL)  =V1(II,J)
              BUF_SEND(K+1,IL)=V2(II,J)
              BUF_SEND(K+2,IL)=V3(II,J)
              K=K+3
            ENDDO
          ENDDO
        ELSEIF(IAN.EQ.2) THEN
          DO J=1,NPLAN
            DO I=1,IKA
              II=NH_COM(I,IL)
              BUF_SEND(K,IL)  =V1(II,J)
              BUF_SEND(K+1,IL)=V2(II,J)
              K=K+2
            ENDDO
          ENDDO
        ELSEIF(IAN.EQ.1) THEN
          DO J=1,NPLAN
            DO I=1,IKA
              II=NH_COM(I,IL)
              BUF_SEND(K,IL)  =V1(II,J)
              K=K+1
            ENDDO
          ENDDO
        ENDIF
!
        CALL P_WRITE(BUF_SEND(1:DIMBUF,IL),IAN*IKA*NPLAN,
     &                IPA,PARACOI_MSG_TAG,SEND_REQ(IL))
!
      ENDDO
!
!== WAIT RECEIVED MESSAGES (POSSIBILITY OF COVERING)
!
      DO IL=1,NB_NEIGHB
        IKA = NB_NEIGHB_PT(IL)
        IPA = LIST_SEND(IL)
        CALL P_WAIT_PARACO(RECV_REQ(IL),1)
!
        K=1
!
        IF(ICOM.EQ.1) THEN
          IF(IAN.EQ.3) THEN
            DO J=1,NPLAN
              DO I=1,IKA
                II=NH_COM(I,IL)
                IF(ABS(BUF_RECV(K,IL)).GT.ABS(V1(II,J)))
     &                          V1(II,J)=BUF_RECV(K  ,IL)
                IF(ABS(BUF_RECV(K+1,IL)).GT.ABS(V2(II,J)))
     &                          V2(II,J)=BUF_RECV(K+1,IL)
                IF(ABS(BUF_RECV(K+2,IL)).GT.ABS(V3(II,J)))
     &                          V3(II,J)=BUF_RECV(K+2,IL)
                K=K+3
              ENDDO
            ENDDO
          ELSEIF(IAN.EQ.2) THEN
            DO J=1,NPLAN
              DO I=1,IKA
                II=NH_COM(I,IL)
                IF(ABS(BUF_RECV(K,IL)).GT.ABS(V1(II,J)))
     &                          V1(II,J)=BUF_RECV(K  ,IL)
                IF(ABS(BUF_RECV(K+1,IL)).GT.ABS(V2(II,J)))
     &                          V2(II,J)=BUF_RECV(K+1,IL)
                K=K+2
              ENDDO
            ENDDO
          ELSEIF(IAN.EQ.1) THEN
            DO J=1,NPLAN
              DO I=1,IKA
                II=NH_COM(I,IL)
                IF(ABS(BUF_RECV(K,IL)).GT.ABS(V1(II,J)))
     &                          V1(II,J)=BUF_RECV(K  ,IL)
                K=K+1
              ENDDO
            ENDDO
          ENDIF
        ELSEIF(ICOM.EQ.2) THEN
          IF(IAN.EQ.3) THEN
            DO J=1,NPLAN
              DO I=1,IKA
                II=NH_COM(I,IL)
                V1(II,J)=V1(II,J)+BUF_RECV(K  ,IL)
                V2(II,J)=V2(II,J)+BUF_RECV(K+1,IL)
                V3(II,J)=V3(II,J)+BUF_RECV(K+2,IL)
                K=K+3
              ENDDO
            ENDDO
          ELSEIF(IAN.EQ.2) THEN
            DO J=1,NPLAN
              DO I=1,IKA
                II=NH_COM(I,IL)
                V1(II,J)=V1(II,J)+BUF_RECV(K  ,IL)
                V2(II,J)=V2(II,J)+BUF_RECV(K+1,IL)
                K=K+2
              ENDDO
            ENDDO
          ELSEIF(IAN.EQ.1) THEN
            DO J=1,NPLAN
              DO I=1,IKA
                II=NH_COM(I,IL)
                V1(II,J)=V1(II,J)+BUF_RECV(K  ,IL)
                K=K+1
              ENDDO
            ENDDO
          ENDIF
        ELSEIF(ICOM.EQ.3) THEN
          IF(IAN.EQ.3) THEN
            DO J=1,NPLAN
              DO I=1,IKA
                II=NH_COM(I,IL)
                IF(BUF_RECV(K  ,IL).GT.V1(II,J))
     &            V1(II,J)=BUF_RECV(K  ,IL)
                IF(BUF_RECV(K+1,IL).GT.V2(II,J))
     &            V2(II,J)=BUF_RECV(K+1,IL)
                IF(BUF_RECV(K+2,IL).GT.V3(II,J))
     &            V3(II,J)=BUF_RECV(K+2,IL)
                K=K+3
              ENDDO
            ENDDO
          ELSEIF(IAN.EQ.2) THEN
            DO J=1,NPLAN
              DO I=1,IKA
                II=NH_COM(I,IL)
                IF(BUF_RECV(K  ,IL).GT.V1(II,J))
     &            V1(II,J)=BUF_RECV(K  ,IL)
                IF(BUF_RECV(K+1,IL).GT.V2(II,J))
     &            V2(II,J)=BUF_RECV(K+1,IL)
                K=K+2
              ENDDO
            ENDDO
          ELSEIF(IAN.EQ.1) THEN
            DO J=1,NPLAN
              DO I=1,IKA
                II=NH_COM(I,IL)
                IF(BUF_RECV(K  ,IL).GT.V1(II,J))
     &            V1(II,J)=BUF_RECV(K  ,IL)
                K=K+1
              ENDDO
            ENDDO
          ENDIF
        ELSEIF(ICOM.EQ.4) THEN
          IF(IAN.EQ.3) THEN
            DO J=1,NPLAN
              DO I=1,IKA
                II=NH_COM(I,IL)
                IF(BUF_RECV(K  ,IL).LT.V1(II,J))
     &            V1(II,J)=BUF_RECV(K  ,IL)
                IF(BUF_RECV(K+1,IL).LT.V2(II,J))
     &            V2(II,J)=BUF_RECV(K+1,IL)
                IF(BUF_RECV(K+2,IL).LT.V3(II,J))
     &            V3(II,J)=BUF_RECV(K+2,IL)
                K=K+3
              ENDDO
            ENDDO
          ELSEIF(IAN.EQ.2) THEN
            DO J=1,NPLAN
              DO I=1,IKA
                II=NH_COM(I,IL)
                IF(BUF_RECV(K  ,IL).LT.V1(II,J))
     &            V1(II,J)=BUF_RECV(K  ,IL)
                IF(BUF_RECV(K+1,IL).LT.V2(II,J))
     &            V2(II,J)=BUF_RECV(K+1,IL)
                K=K+2
              ENDDO
            ENDDO
          ELSEIF(IAN.EQ.1) THEN
            DO J=1,NPLAN
              DO I=1,IKA
                II=NH_COM(I,IL)
                IF(BUF_RECV(K  ,IL).LT.V1(II,J))
     &            V1(II,J)=BUF_RECV(K  ,IL)
                K=K+1
              ENDDO
            ENDDO
          ENDIF
        ENDIF
!
      ENDDO
!
!== WAIT SENT MESSAGES
!
      CALL P_WAIT_PARACO(SEND_REQ,NB_NEIGHB)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
