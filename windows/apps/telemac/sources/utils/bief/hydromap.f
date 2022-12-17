!                   *******************
                    SUBROUTINE HYDROMAP
!                   *******************
!
     &(CN,X,Y,NPOIN,NCN,NBOR,KP1BOR,NPTFR)
!
!***********************************************************************
! BIEF   V7P2
!***********************************************************************
!
!brief    INTERPOLATE CN (CURVE NUMBER, RUNOFF PARAMETER) ON THE MESH
!         (INSPIRED FROM SUBROUTINE FOND (BIEF))
!
!history  RIADH ATA (LNHE)
!+        29/06/16
!+        V7P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| KP1BOR         |-->| GIVES THE NEXT BOUNDARY POINT IN A CONTOUR
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NCN            |-->| LOGICAL UNIT OF FILE FOR CN VALUES
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| CN             |<--| INTERPOLATED CN VALUES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_HYDROMAP => HYDROMAP
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: NCN,NPOIN,NPTFR
      DOUBLE PRECISION, INTENT(INOUT) :: CN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      INTEGER,          INTENT(IN)    :: NBOR(NPTFR),KP1BOR(NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NP,ERR
!
      DOUBLE PRECISION BID
!
      CHARACTER(LEN=1) C
!
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: XRELV,YRELV,LUCN
!
!-----------------------------------------------------------------------
!                    READS THE DIGITISED POINTS
!                      FROM LOGICAL UNIT NCN
!-----------------------------------------------------------------------
!
      CALL OV('X=C     ',X=CN, C=0.D0, DIM1=NPOIN)
!
!  ASSESSES THE EXTENT OF DATA
!
      NP = 0
20    READ(NCN,120,END=24,ERR=124) C
120   FORMAT(A1)
      IF(C(1:1).NE.'#') THEN
        BACKSPACE ( UNIT = NCN )
        NP = NP + 1
        READ(NCN,*) BID,BID,BID
      ENDIF
      GO TO 20
124   CONTINUE
      WRITE(LU,19) NP
19    FORMAT(1X,'HYDROMAP (BIEF):'
     &      ,/,1X,'ERROR IN THE CURVE NUMBERS FILE'
     &      ,/,1X,'AT LINE ',I7)
      CALL PLANTE(1)
      STOP
24    CONTINUE
!
!  DYNAMICALLY ALLOCATES THE ARRAYS
!
      ALLOCATE(XRELV(NP),STAT=ERR)
      ALLOCATE(YRELV(NP),STAT=ERR)
      ALLOCATE(LUCN(NP) ,STAT=ERR)
!
      IF(ERR.NE.0) THEN
        WRITE(LU,11) NP
11      FORMAT(1X,'HYDROMAP (BIEF):'
     &      ,/,1X,'ERROR DURING ALLOCATION OF 3 ARRAYS'
     &      ,/,1X,'OF SIZE ',I7)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!  READS THE DATA
!
      REWIND(NCN)
      NP = 0
23    READ(NCN,120,END=22,ERR=122) C
      IF(C(1:1).NE.'#') THEN
        BACKSPACE ( UNIT = NCN )
        NP = NP + 1
        READ(NCN,*) XRELV(NP) , YRELV(NP) , LUCN(NP)
      ENDIF
      GO TO 23
!
122   CONTINUE
      WRITE(LU,13) NP
13    FORMAT(1X,'HYDROMAP (BIEF):'
     &      ,/,1X,'ERROR IN THE CURVE NUMBERS FILE'
     &      ,/,1X,'AT LINE ',I7)
      CALL PLANTE(1)
      STOP
!
22    CONTINUE
!
      WRITE(LU,113) NP
113   FORMAT(1X,'HYDROMAP (BIEF):'
     &      ,/,1X,'NUMBER OF POINTS IN THE CURVE NUMBERS FILE:',I7
     &      ,/,1X,'INTERPOLATING ........')
!
!-----------------------------------------------------------------------
!   THE BOTTOM ELEVATION IS COMPUTED BY INTERPOLATION ONTO THE
!                      DOMAIN INTERIOR POINTS
!-----------------------------------------------------------------------
!
      CALL FASP(X,Y,CN,NPOIN,XRELV,YRELV,LUCN,NP,NBOR,KP1BOR,NPTFR,0.D0)
!
!-----------------------------------------------------------------------
!
!     VERIFICATION
!      DO NP=1,10
!        WRITE(LU,*)'DANS HYDROMAP:',CN(NP),LUCN(NP)
!      ENDDO
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(XRELV)
      DEALLOCATE(YRELV)
      DEALLOCATE(LUCN)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
