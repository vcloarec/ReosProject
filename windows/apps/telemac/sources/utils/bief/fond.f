!                   ***************
                    SUBROUTINE FOND
!                   ***************
!
     &(ZF  ,X,Y,NPOIN,NFON,NBOR,KP1BOR,NPTFR)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE BOTTOM ELEVATION.
!
!history  J-M HERVOUET (LNHE)
!+        20/03/08
!+        V5P9
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
!| KP1BOR         |-->| GIVES THE NEXT BOUNDARY POINT IN A CONTOUR
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NFON           |-->| LOGICAL UNIT OF FILE FOR BOTTOM BATHYMETRY
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FOND => FOND
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NFON,NPOIN,NPTFR
      DOUBLE PRECISION, INTENT(OUT) :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)  :: X(NPOIN),Y(NPOIN)
      INTEGER, INTENT(IN) :: NBOR(NPTFR),KP1BOR(NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NP,ERR
!
      DOUBLE PRECISION BID
!
      CHARACTER(LEN=1) C
!
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: XRELV,YRELV,COTE
!
!-----------------------------------------------------------------------
!                    READS THE DIGITISED POINTS
!                      FROM LOGICAL UNIT NFON
!-----------------------------------------------------------------------
!
!  ASSESSES THE EXTENT OF DATA
!
      NP = 0
20    READ(NFON,120,END=24,ERR=124) C
120   FORMAT(A1)
      IF(C(1:1).NE.'C'.AND.C(1:1).NE.'B') THEN
        BACKSPACE ( UNIT = NFON )
        NP = NP + 1
        READ(NFON,*) BID,BID,BID
      ENDIF
      GO TO 20
124   CONTINUE
      WRITE(LU,19) NP
19    FORMAT(1X,'FOND (BIEF)'
     &      ,/,1X,'ERROR IN THE BOTTOM FILE'
     &      ,/,1X,'AT LINE ',I7)
      CALL PLANTE(1)
      STOP
24    CONTINUE
!
!  DYNAMICALLY ALLOCATES THE ARRAYS
!
      ALLOCATE(XRELV(NP),STAT=ERR)
      ALLOCATE(YRELV(NP),STAT=ERR)
      ALLOCATE(COTE(NP) ,STAT=ERR)
!
      IF(ERR.NE.0) THEN
        WRITE(LU,11) NP
11      FORMAT(1X,'FOND (BIEF)'
     &      ,/,1X,'ERROR DURING ALLOCATION OF 3 ARRAYS'
     &      ,/,1X,'OF SIZE ',I7)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!  READS THE DATA
!
      REWIND(NFON)
      NP = 0
23    READ(NFON,120,END=22,ERR=122) C
      IF(C(1:1).NE.'C'.AND.C(1:1).NE.'B') THEN
        BACKSPACE ( UNIT = NFON )
        NP = NP + 1
        READ(NFON,*) XRELV(NP) , YRELV(NP) , COTE(NP)
      ENDIF
      GO TO 23
!
122   CONTINUE
      WRITE(LU,13) NP
13    FORMAT(1X,'FOND (BIEF)'
     &      ,/,1X,'ERROR IN THE BOTTOM FILE'
     &      ,/,1X,'AT LINE ',I7)
      CALL PLANTE(1)
      STOP
!
22    CONTINUE
!
      WRITE(LU,113) NP
113   FORMAT(1X,'FOND (BIEF):'
     &      ,/,1X,'NUMBER OF POINTS IN THE BOTTOM FILE: ',I7)
!
!-----------------------------------------------------------------------
!   THE BOTTOM ELEVATION IS COMPUTED BY INTERPOLATION ONTO THE
!                      DOMAIN INTERIOR POINTS
!-----------------------------------------------------------------------
!
      CALL FASP(X,Y,ZF,NPOIN,XRELV,YRELV,COTE,NP,NBOR,KP1BOR,NPTFR,0.D0)
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(XRELV)
      DEALLOCATE(YRELV)
      DEALLOCATE(COTE)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
