!                   ******************
                    SUBROUTINE SD_NROC
!                   ******************
!
     &(N, IC, IA, JA, A, JAR, AR, P, FLAG)
!
!***********************************************************************
! BIEF   V6P3                                   30/06/2013
!***********************************************************************
!
!brief   SPARSE MATRIX PACKAGE - NONSYMMETRIC CODES
!                  REORDERS ROWS OF A, LEAVING ROW ORDER UNCHANGED
!
!  REORDERS ROWS OF A, LEAVING ROW ORDER UNCHANGED
!
!
!        PARAMETERS USED INTERNALLY..
!  NIA     P     - AT THE KTH STEP, P IS A LINKED LIST OF THE REORDERED
!                    COLUMN INDICES OF THE KTH ROW OF A.  P(N+1) POINTS
!                    TO THE FIRST ENTRY IN THE LIST.
!                    SIZE = N+1.
!  NIA     JAR   - AT THE KTH STEP,JAR CONTAINS THE ELEMENTS OF THE
!                    REORDERED COLUMN INDICES OF A.
!                    SIZE = N.
!  FIA     AR    - AT THE KTH STEP, AR CONTAINS THE ELEMENTS OF THE
!                    REORDERED ROW OF A.
!                    SIZE = N.
!
!note     IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
!
!         DON'T HESITATE TO CHANGE IN/OUTPUT VARIABLES COMMENTS
!         FOR CLARITY
!
!history  C. PEYRARD (LNHE)
!+        30/06/13
!+        V6P3
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->|NONZERO ENTRIES OF THE COEFFICIENT MATRIX M,
!|                |   |STORED BY ROWS
!| IA ,JA         |-->|POINTERS TO DELIMIT THE ROWS IN A ; SIZE = N+1
!| IL, JL         |-->| STRUCTURE OF LOWER FACTORISED TRIANGULAR MATRIX
!| IU, JU         |-->| STRUCTURE OF UPPER FACTORISED TRIANGULAR MATRIX
!| IJU,IJL        |-->| USED TO COMPRESS STORAGE OF JU and JL
!| IC             |-->|INVERSE OF THE ORDERING OF THE COLUMNS OF MATRIX
!| N              |-->| RANK OF MATRIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  IC(*), IA(*), JA(*), JAR(*), P(*), FLAG
      INTEGER  K,N,JMAX,J,NEWJ,I,JMIN
      DOUBLE PRECISION  A(*), AR(*)
!
!  ******  FOR EACH NONEMPTY ROW  *******************************
      DO K=1,N
        JMIN = IA(K)
        JMAX = IA(K+1) - 1
        IF(JMIN .GT. JMAX) CYCLE
        P(N+1) = N + 1
!  ******  INSERT EACH ELEMENT IN THE LIST  *********************
        DO J=JMIN,JMAX
          NEWJ = IC(JA(J))
          I = N + 1
          DO
            IF(P(I) .GE. NEWJ) EXIT
            I = P(I)
          ENDDO
          IF(P(I) .EQ. NEWJ) THEN
! ** ERROR.. DUPLICATE ENTRY IN A
            FLAG = N + K
            RETURN
          ENDIF
          P(NEWJ) = P(I)
          P(I) = NEWJ
          JAR(NEWJ) = JA(J)
          AR(NEWJ) = A(J)
        ENDDO ! J
!  ******  REPLACE OLD ROW IN JA AND A  *************************
        I = N + 1
        DO J=JMIN,JMAX
          I = P(I)
          JA(J) = JAR(I)
          A(J) = AR(I)
        ENDDO ! J
      ENDDO ! K
      FLAG = 0
!
      RETURN
!
      END
