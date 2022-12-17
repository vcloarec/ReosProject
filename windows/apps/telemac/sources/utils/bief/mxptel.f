!                   *****************
                    SUBROUTINE MXPTEL
!                   *****************
!
     &(MXPTVS,MXELVS,IKLES,IELM,NPOIN,NELEM,NDP,IPOBO,LISTIN)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE MAXIMUM NUMBER OF POINTS AND ELEMENTS
!+                NEIGHBOURING A POINT FOR A GIVEN TRIANGULAR MESH.
!
!note     ALLOCATES ITRAV HERE, INTERNALLY.
!+         IT'S A LOCAL WORKING VARIABLE ANYWAY.
!+         COULD ALSO PASS THE ELEMENT TYPE IN ARGUMENT TO TREAT
!+         THE 3D SPECIFICALLY.
!
!history  J-M HERVOUET (LNH)
!+        24/08/95
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        18/04/2014
!+        V7P0
!+   Automatic array ITRAV now allocatable.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IELM           |-->| TYPE OF ELEMENT
!| IKLES          |-->| LIKE CONNECTIVITY TABLE BUT IN SELAFIN FORMAT
!|                |   | IKLES(3,NELEM) INSTEAD OF IKLE(NELEM,3)
!| IPOBO          |-->| 0 FOR INNER POINTS
!|                |   | NOT 0 FOR BOUNDARY POINTS (THEIR RANK ACTUALLY)
!| LISTIN         |-->| IF YES : MXELVS AND MXPTVS WILL BE PRINTED
!| MXELVS         |-->| MAXIMUM NUMBER OF NEIGHBOURING ELEMENTS
!| MXPTVS         |-->| MAXIMUM NUMBER OF NEIGHBOURS OF A POINT
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MXPTEL => MXPTEL
      USE INTERFACE_PARALLEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(INOUT) :: MXPTVS,MXELVS
      INTEGER, INTENT(IN)    :: IELM,NDP,NPOIN,NELEM
      INTEGER, INTENT(IN)    :: IKLES(NDP,NELEM),IPOBO(NPOIN)
      LOGICAL, INTENT(IN)    :: LISTIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,IELEM
      INTEGER, ALLOCATABLE :: ITRAV(:)
      ALLOCATE(ITRAV(NPOIN))
!
!-----------------------------------------------------------------------
!
! 1) INITIALISES THE NUMBER OF NEIGHBOURING ELEMENTS TO 0:
!
      DO I = 1 , NPOIN
        ITRAV(I) = 0
      ENDDO
!
! 2) COUNTS THE NUMBER OF NEIGHBOURING ELEMENTS PER ASSEMBLY OPERATION:
!
      DO J = 1, NDP
        DO IELEM = 1 , NELEM
          ITRAV(IKLES(J,IELEM)) = ITRAV(IKLES(J,IELEM)) + 1
        ENDDO
      ENDDO
!
! 3) LOOKS FOR THE MAXIMUM :
!
      MXELVS = ITRAV(1)
      DO I = 2 , NPOIN
        MXELVS = MAX(MXELVS,ITRAV(I))
      ENDDO
!
! 4) NUMBER OF NEIGHBOURING POINTS: NEED TO ADD 1 TO THIS NUMBER
!                                   FOR BOUNDARY NODES.
!    SIMULTANEOUSLY LOOKS FOR THE MAXIMUM
!
      IF(IELM.EQ.31) THEN
        CALL MXPTEL31(NELEM,NPOIN,MXELVS,IKLES,MXPTVS)
      ELSE
        MXPTVS = MXELVS
        DO I = 1 , NPOIN
          IF(IPOBO(I).NE.0) MXPTVS = MAX(MXPTVS,ITRAV(I)+1)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1) THEN
        MXPTVS=P_MAX(MXPTVS)
        MXELVS=P_MAX(MXELVS)
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(LISTIN) THEN
        WRITE(LU,98) MXELVS,MXPTVS
        IF(NCSIZE.GT.1) THEN
          WRITE(LU,*) '(GLOBAL MESH)'
        ENDIF
      ENDIF
98    FORMAT(1X,'MXPTEL (BIEF) : MAXIMUM NUMBER OF ELEMENTS AROUND A POI
     &NT: ',1I3,/,1X,
     &          '                MAXIMUM NUMBER OF POINTS AROUND A POINT
     &: ',1I3)
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(ITRAV)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
