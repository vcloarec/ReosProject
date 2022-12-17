!                   ***************
                    SUBROUTINE CLIP
!                   ***************
!
     &(F,XMIN,CLPMIN,XMAX,CLPMAX,NPOIN)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CAPS THE VALUES IN ARRAY F.
!+
!+            MINIMUM OF F: XMIN (IF CLPMIN=.TRUE.).
!+
!+            MAXIMUM OF F: XMAX (IF CLPMAX=.TRUE.).
!
!note     IMPORTANT : IF NPOIN IS NEGATIVE, WILL TREAT -NPOIN VALUES.
!+
!+                 IF NPOIN IS POSITIVE, WILL TAKE THE SIZE OF F.
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        12/01/95
!+        V5P1
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
!| CLPMAX         |-->| LOGICAL, IF YES, CLIPPING OF MAXIMUM VALUE
!| CLPMIN         |-->| LOGICAL, IF YES, CLIPPING OF MINIMUM VALUE
!| F              |<->| ATTAY OF VALUES TO BE CHECKED
!| NPOIN          |-->| NUMBER OF POINTS IN THE DOMAIN
!| XMAX           |-->| MAXIMUM THRESHOLD VALUE
!| XMIN           |-->| MINIMUM THRESHOLD VALUE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CLIP => CLIP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: F
      DOUBLE PRECISION, INTENT(IN)    :: XMIN,XMAX
      LOGICAL         , INTENT(IN)    :: CLPMIN,CLPMAX
      INTEGER         , INTENT(IN)    :: NPOIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NP
!
!-----------------------------------------------------------------------
!
      IF(F%TYPE.EQ.2) THEN
!       F IS A VECTOR STRUCTURE
        IF(NPOIN.LT.0) THEN
          NP = - NPOIN
        ELSE
          NP = F%DIM1
        ENDIF
        IF(CLPMIN) CALL OV('X=+(Y,C)', X=F%R, Y=F%R, C=XMIN, DIM1=NP)
        IF(CLPMAX) CALL OV('X=-(Y,C)', X=F%R, Y=F%R, C=XMAX, DIM1=NP)
      ELSE
        WRITE(LU,*) F%NAME,' IS NOT A VECTOR'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
