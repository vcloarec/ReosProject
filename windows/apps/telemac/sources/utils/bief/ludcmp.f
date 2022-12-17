!                   *****************
                    SUBROUTINE LUDCMP
!                   *****************
!
     &(A,N,NP,INDX)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    GIVEN A MATRIX A(1:N,1:N), WITH PHYSICAL DIMENSION NP
!+                BY NP, THIS ROUTINE REPLACES IT BY THE LU FACTORISATION
!+                OF A ROWWISE PERMUTATION OF ITSELF. A AND N ARE INPUT.
!+                A IS OUTPUT, ARRANGED AS IN EQUATION (2.3.14) ABOVE;
!+                INDX(1:N) IS AN OUTPUT VECTOR THAT RECORDS THE ROW
!+                PERMUTATION EFFECTED BY THE PARTIAL PIVOTING; D IS
!+                OUTPUT AS SIGMA1 DEPENDING ON WHETHER THE NUMBER OF
!+                ROW INTERCHANGES WAS EVEN OR ODD, RESPECTIVELY. THIS
!+                SUBROUTINE IS USED IN COMBINATION WITH LUBKSB TO SOLVE
!+                LINEAR EQUATIONS OR INVERT A MATRIX.
!
!history  CHUN WANG
!+        28/07/2006
!+        V5P7
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
!+        25/07/2012
!+        V6P2
!+   Correction of one test of division by 0.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| MATRIX OF THE SYSTEM
!| INDX           |-->| ADDRESS IN RIGHT-HAND SIDE
!| N              |-->| SIZE OF B
!| NP             |-->| RANK OF MATRIX A
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: N,NP
      INTEGER, INTENT(INOUT)          :: INDX(N)
      DOUBLE PRECISION, INTENT(INOUT) :: A(NP,NP)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION D
      INTEGER I,IMAX,J,K
      DOUBLE PRECISION AAMAX,DUM,XSOM,VV(500)
      DOUBLE PRECISION, PARAMETER:: CHOUIA=1.D-20
!
!------------------------------------------------------------------------
!
      D=1.D0 ! NO ROW INTERCHANGES YET
!
!     LOOP OVER ROWS TO GET THE IMPLICIT SCALING INFORMATION
!
      DO I=1,N
        AAMAX=0.D0
        DO J=1,N
          IF(ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
        ENDDO
        IF(AAMAX.LT.CHOUIA) THEN
          WRITE(LU,*) 'SINGULAR MATRIX IN LUDCMP'
          CALL PLANTE(1)
          STOP
        ENDIF
        VV(I)=1.D0/AAMAX ! SAVES THE SCALING
      ENDDO
!
!     LOOP OVER COLUMNS OF CROUT'S METHOD
!
      DO J=1,N
        DO I=1,J-1 ! EQUATION (2.3.12) EXCEPT FOR I = J
          XSOM=A(I,J)
          DO K=1,I-1
            XSOM=XSOM-A(I,K)*A(K,J)
          ENDDO
          A(I,J)=XSOM
        ENDDO
        AAMAX=0.D0 ! INITIALISES FOR THE SEARCH OF LARGEST PIVOT ELEMENT
        DO I=J,N   ! THIS IS I = J OF EQUATION (2.3.12) AND
                  ! I = J +1 : : : N OF EQUATION (2.3.13)
          XSOM=A(I,J)
          DO K=1,J-1
            XSOM=XSOM-A(I,K)*A(K,J)
          ENDDO
          A(I,J)=XSOM
          DUM=VV(I)*ABS(XSOM) ! FIGURE OF MERIT FOR THE PIVOT
          IF (DUM.GE.AAMAX) THEN ! IS IT BETTER THAN THE BEST SO FAR?
            IMAX=I
            AAMAX=DUM
          ENDIF
        ENDDO
        IF (J.NE.IMAX) THEN ! NEEDS TO INTERCHANGE ROWS?
          DO K=1,N
            DUM=A(IMAX,K)
            A(IMAX,K)=A(J,K)
            A(J,K)=DUM
          ENDDO
          D=-D !...AND CHANGES THE PARITY OF D
          VV(IMAX)=VV(J) ! ALSO INTERCHANGES THE SCALE FACTOR
        ENDIF
        INDX(J)=IMAX
        IF(ABS(A(J,J)).LT.CHOUIA) A(J,J)=CHOUIA
!
!  IF THE PIVOT ELEMENT IS 0 THE MATRIX IS SINGULAR (AT LEAST TO THE
!  PRECISION OF THE ALGORITHM)
!
        IF(J.NE.N) THEN ! DIVIDES BY THE PIVOT ELEMENT
          DUM=1.D0/A(J,J)
          DO I=J+1,N
            A(I,J)=A(I,J)*DUM
          ENDDO
        ENDIF
!
      ENDDO
!
!------------------------------------------------------------------------
!
      RETURN
      END
