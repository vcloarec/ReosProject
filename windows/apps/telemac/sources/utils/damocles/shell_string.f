      ! brief Sorting function
      !
      ! param N Size of the array
      ! param A Array to sort
      ! param B Reordering array
      SUBROUTINE SHELL_STRING
!
     &                          (N, A, B)
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)              :: N
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT)  :: A(N)
      INTEGER, INTENT(OUT)             :: B(N)
!
      INTEGER                          :: I, J, INC
      CHARACTER(LEN=PATH_LEN)               :: V
      INTEGER                          :: W
!
      INTEGER                          :: ALPHA
!
      ALPHA=2
!
      DO I=1,N
        B(I)=I
      ENDDO
!
      INC=1
 1    INC=ALPHA*INC+1
      IF (INC.LE.N) GOTO 1
 2    CONTINUE
        INC=INC/ALPHA
        DO I=INC+1,N
          V=A(I)
          W=B(I)
          J=I
 3        IF (A(J-INC).GT.V) THEN
            A(J)=A(J-INC)
            B(J)=B(J-INC)
            J=J-INC
            IF (J.LE.INC) GOTO 4
          GOTO 3
          ENDIF
 4        A(J)=V
          B(J)=W
        ENDDO
!
      IF (INC.GT.1) GOTO 2
!
      RETURN
      END SUBROUTINE

