!                   **********************
                    SUBROUTINE BIEF_VALIDA
!                   **********************
!
     &(VARREF,TEXTREF,UREF,REFFORMAT,VARRES,TEXTRES,URES,RESFORMAT,
     & MAXTAB,NP,IT,MAXIT,ACOMPARER)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    VALIDATES THE RESULTS AGAINST AN ANALYTICAL SOLUTION
!+                OR AGAINST RESULTS IN THE COMPUTATION REFERENCE FILE.
!
!note     THE LAST TIMESTEP ONLY IS COMPARED.
!note  EXCEPT FOR THE BOTTOM, ASSUMES THAT THE REFERENCE
!+         FILE DOES HOLD THE VARIABLES TO BE COMPARED.
!
!warning  THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO EACH
!+            PARTICULAR CASE
!
!history  J-M HERVOUET (LNHE)
!+        05/08/2009
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
!history  J-M HERVOUET (LNHE)
!+        05/08/2009
!+        V6P3
!+   Name of code now printed in the listing.
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ACOMPARER      |-->| INDICATES WHICH VARIABLE TO COMPARE
!| IT             |-->| TIME STEP NUMBER
!| MAXIT          |-->| MAXIMUM NUMBER OF ITERATIONS
!| MAXTAB         |-->| TOTAL NUMBER OF VARIABLES
!| NP             |-->| NUMBER OF POINTS TO BE CHECKED
!| REFFORMAT      |-->| FORMAT OF REFERENCE FILE
!| RESFORMAT      |-->| FORMAT OF RESULTS FILE
!| TEXTREF        |-->| NAMES & UNITS OF VARIABLES IN THE REFERENCE FILE
!| TEXTRES        |-->| NAMES & UNITS OF VARIABLES IN THE RESULTS FILE
!| UREF           |-->| LOGICAL UNIT OF REFERENCE FILE
!| URES           |-->| LOGICAL UNIT OF RESULTS FILE
!| VARREF         |-->| BLOCK OF VARIABLES IN REFERENCE FILE
!| VARRES         |-->| BLOCK OF VARIABLES IN RESULTS FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_VALIDA => BIEF_VALIDA
      USE BIEF_DEF
      USE DECLARATIONS_TELEMAC
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NP,MAXTAB,IT,MAXIT,URES,UREF
      INTEGER, INTENT(IN) :: ACOMPARER(MAXTAB)
!
      CHARACTER(LEN=32), INTENT(IN) :: TEXTREF(MAXTAB),TEXTRES(MAXTAB)
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VARREF,VARRES
      CHARACTER(LEN=*), INTENT(IN)  :: REFFORMAT,RESFORMAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IVAR,I,IREF,IRES
!
      DOUBLE PRECISION TIMEREF,TIMERES,ERMAX,ERR
!
!-----------------------------------------------------------------------
!
      INTEGER FINDREF(500),FINDRES(500)
      IF(MAXTAB.GT.500) THEN
        WRITE(LU,*) 'WRONG SIZE OF FINDREF AND FINDRES IN BIEF_VALIDA'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(IT.EQ.MAXIT) THEN
!
!  CALLS SUITE TO READ THE REFERENCE FILE
!
      WRITE(LU,11) NAMECODE
      CALL READ_DATASET(REFFORMAT,UREF,VARREF,NP,IREF,TIMEREF,
     &                  TEXTREF,FINDREF,ACOMPARER,.TRUE.,.TRUE.,MAXTAB)
!
!  CALLS SUITE TO READ THE RESULTS FILE
!
      WRITE(LU,13)
      CALL READ_DATASET(RESFORMAT,URES,VARRES,NP,IRES,TIMERES,
     &                  TEXTRES,FINDRES,ACOMPARER,.TRUE.,.TRUE.,MAXTAB)
!
!-----------------------------------------------------------------------
!
      WRITE(LU,15)
!
      IF(ABS(TIMERES-TIMEREF)/MAX(TIMEREF,1.D-4).GT.1.D-2) THEN
        WRITE(LU,17)
      ENDIF
      IF(IRES.NE.IREF) THEN
        WRITE(LU,19)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     LOOP ON THE VARIABLES TO COMPARE
!
      DO IVAR=1,MIN(MAXTAB,VARREF%N,VARRES%N)
!
        IF(ACOMPARER(IVAR).EQ.1) THEN
!
!       COMPARES THE VARIABLE IVAR
!
          IF(FINDREF(IVAR).EQ.1.AND.FINDRES(IVAR).EQ.1) THEN
!
            ERMAX = 0.D0
            DO I = 1 , NP
              ERR=ABS(VARREF%ADR(IVAR)%P%R(I)-VARRES%ADR(IVAR)%P%R(I))
              IF(ERR.GT.ERMAX) THEN
                ERMAX=ERR
              ENDIF
            ENDDO
!
            IF(NCSIZE.GT.1) ERMAX=P_MAX(ERMAX)
            WRITE(LU,61) TEXTRES(IVAR)(1:16),ERMAX
!
          ELSEIF(FINDREF(IVAR).EQ.1) THEN
!
            WRITE(LU,71) TEXTRES(IVAR)(1:16)
!
          ENDIF
!
        ENDIF
!
      ENDDO
!
      WRITE(LU,51)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
11    FORMAT(1X,////,1X,80('='),/,
     &       25X,' VALIDATION PROCEDURE OF ',A24,/,
     &       1X,80('-'),//,
     &       1X,' 1) READING THE REFERENCE FILE :',/,
     &       1X,' ------------------------------',/)
13    FORMAT(1X,///,
     &       1X,' 2) READING THE RESULTS FILE :',/,
     &       1X,' --------------------------------',/)
15    FORMAT(1X,///,
     &       1X,' 3) COMPARISON:',/,
     &       1X,' --------------',/)
17    FORMAT(1X,///,
     &       1X,' BEWARE: TIMES ARE DIFFERENT',/,
     &       1X,' ---------------------------',/)
19    FORMAT(1X,///,
     &       1X,' BEWARE: RECORD NUMBERS ARE DIFFERENT',/,
     &       1X,' ------------------------------------',/)
!
51    FORMAT(1X,80('-'),/,23X,'END OF VALIDATION REPORT',/,
     &       1X,80('='),////)
!
61    FORMAT(1X,'VARIABLE: ' ,A16,'  DIFFERENCE: ',G16.7,/)
!
71    FORMAT(1X,'VARIABLE: ' ,A16,'  NOT FOUND'  ,/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
