!                   *****************
                    SUBROUTINE LUBKSB
!                   *****************
!
     &(A,N,NP,INDX,B)
!
!***********************************************************************
! BIEF   V6P1                                  21/08/2010
!***********************************************************************
!
!brief    SOLVES THE SET OF N LINEAR EQUATIONS A \ DELTA X = B
!+                HERE A IS INPUT, NOT AS THE MATRIX A BUT AS ITS LU
!+                FACTORISATION, GIVEN BY THE SUBROUTINE LUDCMP. INDX
!+                IS INPUT AS THE PERMUTATION VECTOR RETURNED BY LUDCMP.
!+                B(1:N) IS INPUT AS THE RIGHT-HAND SIDE VECTOR B, AND
!+                RETURNS WITH THE SOLUTION VECTOR X. A, N, NP, AND
!+                INDX ARE NOT MODIFIED BY THIS SUBROUTINE AND CAN BE LEFT
!+                IN PLACE FOR SUCCESSIVE CALLS WITH DIFFERENT RIGHT-HAND
!+                SIDES B. THIS ROUTINE TAKES INTO ACCOUNT THE POSSIBILITY
!+                THAT B WILL BEGIN WITH A LOT OF 0 ELEMENTS, SO IT IS
!+                EFFICIENT FOR USE IN MATRIX INVERSION.
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| MATRIX OF THE SYSTEM
!| B              |<->| RIGHT-HAND SIDE, THEN SOLUTION
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
      INTEGER, INTENT(IN) :: N,NP
      INTEGER, INTENT(IN) :: INDX(N)
      DOUBLE PRECISION, INTENT(INOUT) :: B(N)
      DOUBLE PRECISION, INTENT(IN)    :: A(NP,NP)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,II,J,LL
      DOUBLE PRECISION XSOM
!
!-----------------------------------------------------------------------
!
      II=0
      ! WHEN II HAS A POSITIVE VALUE, IT WILL BECOME THE INDEX
      ! OF THE FIRST NONVANISHING ELEMENT OF B.
      ! DOES THE FORWARD SUBSTITUTION, EQUATION (2.3.6). THE ONLY
      ! NEW WRINKLE IS TO UNSCRAMBLE THE PERMUTATION AS WE GO.
!
      DO I=1,N
        LL=INDX(I)
        XSOM=B(LL)
        B(LL)=B(I)
        IF(II.NE.0) THEN
          DO J=II,I-1
            XSOM=XSOM-A(I,J)*B(J)
          ENDDO
        ELSEIF(XSOM.NE.0.D0) THEN
          II=I
          ! A NONZERO ELEMENT WAS ENCOUNTERED, SO FROM NOW ON
          ! WILL HAVE TO DO THE SUMS IN THE ABOVE LOOP
        ENDIF
        B(I)=XSOM
      ENDDO
      DO I=N,1,-1 ! DOES THE BACKSUBSTITUTION, EQUATION (2.3.7)
        XSOM=B(I)
        DO J=I+1,N
          XSOM=XSOM-A(I,J)*B(J)
        ENDDO
        B(I)=XSOM/A(I,I) ! STORES A COMPONENT OF THE SOLUTION VECTOR X
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
