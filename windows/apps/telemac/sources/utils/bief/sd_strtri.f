!                   ********************
                    SUBROUTINE SD_STRTRI
!                   ********************
!
     &(IS,N,IND)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    SORTS IN ASCENDING ORDER THE INTEGER ARRAY 'IS'.
!+                OUTPUT :  IS(IND(I+1) >= IS(IND(I).
!
!note     IMPORTANT: INSPIRED FROM N3S 3.3  22/04/92  B.THOMAS
!
!history  E. RAZAFINDRAKOTO (LNH)
!+        20/11/06
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
!| IND            |<--| PERMUTATION ARRAY FOR ASCENDING ORDER IS
!| IS             |-->| INTEGER ARRAY TO SORT IN ASCENDING ORDER
!| N              |-->| LENGTH OF IS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_STRTRI => SD_STRTRI
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: N
      INTEGER, INTENT(IN)    :: IS(N)
      INTEGER, INTENT(INOUT) :: IND(N)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,I1,K
!
!-----------------------------------------------------------------------
!
      IND(1) = 1
!
      DO I = 2 , N
!
!--->   IS(1:I-1) IS SORTED
!
        I1 = I-1
        DO K = I1 , 1 , -1
!
!--->     FOR L > K+1  IS(IND(L)) > IS(I)
!
          IF(IS(IND(K)).GT.IS(I)) THEN
            IND(K+1) = IND(K)
          ELSE
            EXIT
          ENDIF
!
        ENDDO ! K
!
!--->   ASSERTION : IS(IND(K))
!
        IND(K+1)=I
!
      ENDDO ! I
!
!-----------------------------------------------------------------------
!
      RETURN
      END
