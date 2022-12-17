!                   *****************
                    SUBROUTINE CPSTVC
!                   *****************
!
     &( X , Y )
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    COPIES A VECTOR STRUCTURE ONTO ANOTHER.
!
!history  J-M HERVOUET (LNH)
!+        01/03/1995
!+        V5P1
!+   First version.
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
!+        22/05/2013
!+        V6P3
!+   STATUS now also considered.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        26/06/2015
!+        V7P1
!+   More accurate messages.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| X              |-->| STRUCTURE TO BE COPIED
!| Y              |<--| STRUCTURE THAT RECEIVES X ATTRIBUTES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CPSTVC => CPSTVC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: X
      TYPE(BIEF_OBJ), INTENT(INOUT) :: Y
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER SIZEX,SIZEY
!
!-----------------------------------------------------------------------
!  TREATS ONLY VECTORS HERE :
!-----------------------------------------------------------------------
!
      IF(Y%TYPE.NE.2.OR.X%TYPE.NE.2) THEN
        WRITE(LU,201) X%NAME,X%TYPE,Y%NAME,Y%TYPE
 201    FORMAT(1X,'CPSTVC : FORBIDDEN CASE FOR X AND Y:',/,1X,
     &            'X=',A6,' TYPE :',1I6                 ,/,1X,
     &            'Y=',A6,' TYPE :',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      SIZEX = X%DIM1*X%DIM2
      SIZEY = Y%MAXDIM1*Y%MAXDIM2
!
      IF(SIZEX.GT.SIZEY) THEN
        WRITE(LU,301) X%NAME,SIZEX,Y%NAME,SIZEY
 301    FORMAT(1X,'CPSTVC : FORBIDDEN CASE FOR X AND Y:',/,1X,
     &            'X=',A6,' SIZE        :',1I8,/,1X,
     &            'Y=',A6,' MAXIMUM SIZE:',1I8)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     DISCRETISATION
      IF(Y%ELM.NE.X%ELM.AND.Y%STATUS.EQ.1) THEN
        WRITE(LU,401) X%NAME,Y%NAME
401     FORMAT(1X,'CPSTVC : COPY OF ',A6,' FORBIDDEN ON ',A6,
     &  ' BECAUSE ITS STATUS IS 1')
        CALL PLANTE(1)
        STOP
      ELSE
        Y%ELM = X%ELM
      ENDIF
!     FIRST VECTOR DIMENSION
      Y%DIM1 = X%DIM1
!     SECOND VECTOR DIMENSION
      Y%DIM2 = X%DIM2
!     CASE OF DISCONTINUOUS VECTORS
      Y%DIMDISC = X%DIMDISC
!
!     A VECTOR WITH PREVIOUS STATUS 0 NOW DEFINED ON A MESH
!     IT IS DECLARED AS STATUS 2 (DISCRETISATION THAT CAN BE CHANGED)
!
      IF(Y%STATUS.EQ.0.AND.(X%STATUS.EQ.1.OR.X%STATUS.EQ.2)) THEN
        Y%STATUS=2
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

