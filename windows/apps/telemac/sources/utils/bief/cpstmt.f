!                   *****************
                    SUBROUTINE CPSTMT
!                   *****************
!
     &( X , Y , TRANS )
!
!***********************************************************************
! BIEF   V7P2
!***********************************************************************
!
!brief    COPIES A MATRIX STRUCTURE ONTO ANOTHER.
!+                X COPIED ONTO Y.
!
!history  J-M HERVOUET (LNHE)
!+        03/02/2010
!+        V6P0
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
!+        22/03/2016
!+        V7P2
!+   Adding the copy of STOX.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| TRANS          |-->| IF YES, Y WILL BE CONSIDERED TRANSPOSED OF X
!| X              |-->| THE STRUCTURE OF X WILL BE COPIED ON Y
!| Y              |<->| THE MODIFIED MATRIX STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CPSTMT => CPSTMT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: X
      TYPE(BIEF_OBJ), INTENT(INOUT) :: Y
      LOGICAL, INTENT(IN), OPTIONAL :: TRANS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELM1,IELM2,IELN1,IELN2
      LOGICAL TR
!
!-----------------------------------------------------------------------
!  TREATS ONLY MATRICES HERE :
!-----------------------------------------------------------------------
!
      IF(X%TYPE.NE.3.OR.Y%TYPE.NE.3) THEN
        WRITE(LU,201) X%NAME,X%TYPE,Y%NAME,Y%TYPE
201     FORMAT(1X,'CPSTMT : FORBIDDEN CASE FOR X AND Y:',/,1X,
     &            'X=',A6,' TYPE :',1I6                 ,/,1X,
     &            'Y=',A6,' TYPE :',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(PRESENT(TRANS)) THEN
        TR = TRANS
      ELSE
        TR = .FALSE.
      ENDIF
!
      IF(.NOT.TR) THEN
        IELM1 = X%ELMLIN
        IELM2 = X%ELMCOL
      ELSE
        IELM1 = X%ELMCOL
        IELM2 = X%ELMLIN
      ENDIF
!
! CONTROLS MEMORY SIZE FOR DIAGONAL AND EXTRA-DIAGONAL TERMS :
!
      IF(X%D%DIM1.GT.Y%D%MAXDIM1.OR.
     &   X%X%DIM2*X%X%DIM1.GT.Y%X%MAXDIM1*Y%X%MAXDIM2) THEN
        IELN1 = Y%ELMLIN
        IELN2 = Y%ELMCOL
        WRITE(LU,401) X%NAME,IELM1,IELM2,Y%NAME,IELN1,IELN2
        WRITE(LU,403) X%TYPDIA,X%D%DIM1,X%TYPEXT,
     &                X%X%DIM2*X%X%DIM1,
     &                Y%TYPDIA,Y%D%MAXDIM1,
     &                Y%TYPEXT,Y%X%MAXDIM1*Y%X%MAXDIM2
 401    FORMAT(1X,'CPSTMT : FORBIDDEN CASE FOR X AND Y:',/,1X,
     &            'X=',A6,/,1X,'ELEMENTS ',1I3,' AND ',1I3,/,1X,
     &            'Y=',A6,/,1X,'ELEMENTS ',1I3,' AND ',1I3,/,1X,
     &            'Y IS SMALLER THAN X')
 403    FORMAT(1X,'X HAS A DIAGONAL OF TYPE ',A1,/,1X,
     &            'WITH A SIZE OF ',1I8,/,1X,
     &            'AND OFF-DIAGONAL TERMS OF TYPE ',A1,/,1X,
     &            'WITH A SIZE OF ',1I8,/,1X,
     &            'Y HAS A DIAGONAL OF TYPE ',A1,/,1X,
     &            'AND A MAXIMUM SIZE OF ',1I8,/,1X,
     &            'AND OFF-DIAGONAL TERMS OF TYPE ',A1,/,1X,
     &            'AND A MAXIMUM SIZE OF ',1I8)
        CALL PLANTE(1)
        STOP
      ENDIF
!
! COPIES TYPES OF ELEMENTS
!
      Y%ELMLIN = IELM1
      Y%ELMCOL = IELM2
!
! 4) COPIES TYPES OF DIAGONAL AND EXTRADIAGONAL TERMS
!
      CALL CPSTVC(X%D,Y%D)
      CALL CPSTVC(X%X,Y%X)
!
! 5) COPIES THE MATRIX CHARACTERISTICS
!
      Y%TYPDIA = X%TYPDIA
      Y%TYPEXT = X%TYPEXT
      Y%STOX   = X%STOX
!
!-----------------------------------------------------------------------
!
      RETURN
      END
