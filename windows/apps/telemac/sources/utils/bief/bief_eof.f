!                   *************************
                    LOGICAL FUNCTION BIEF_EOF
!                   *************************
!
     &(LUNIT)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    DETECTS THE END OF A FILE:
!+
!+            IF EOF = .TRUE.  : HAS REACHED END OF FILE,
!+
!+            IF EOF = .FALSE. : CAN CARRY ON.
!
!history
!+        17/08/94
!+        V5P1
!+   ORIGINAL IDEA : ANTOINE YESSAYAN (THANK YOU TONIO)
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
!history J-M HERVOUET (LNHE)
!+        11/06/2012
!+        V6P2
!+   Name changed from eof to bief_eof (eof is a Fortran extension of
!+   some compilers, let us avoid conflicts...)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LUNIT          |-->| LOGICAL INUT OF FILE TO BE READ
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: LUNIT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      BIEF_EOF = .FALSE.
!
      READ(UNIT=LUNIT,ERR=100,END=100)
      BACKSPACE(UNIT=LUNIT,ERR=101)
      RETURN
!
100   CONTINUE
!
      BIEF_EOF=.TRUE.
      RETURN
!
101   CONTINUE
!
      WRITE(LU,*) 'ERROR IN FUNCTION EOF (BIEF) ERROR IN BACKSPACE'
      WRITE(LU,*) 'AFTER A CORRECT READ, COMPILER ERROR ?'
      WRITE(LU,*) 'A TENTATIVE BIEF_EOF=.TRUE. IS RETURNED'
      BIEF_EOF=.TRUE.
!
!-----------------------------------------------------------------------
!
      RETURN
      END
