!                   ********************
                    SUBROUTINE PREVEREBE
!                   ********************
!
     &(XAUX,AX,TYPDIA,TYPEXT,IKLE,NPOIN,NELEM,NELMAX,MESH,TYPEMESH)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS TRIDIAGONAL SYSTEMS FOR EVERY VERTICAL,
!+                BY LUMPING A MATRIX DEFINED ON PRISMS.
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNHE)
!+        02/06/08
!+        V5P9
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
!| AX             |-->| MATRIX OFF-DIAGONAL TERMS
!| IKLE           |-->| CONNECTIVITY TABLE.
!| MESH           |-->| MESH STRUCTURE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| TYPDIA         |-->| TYPE OF DIAGONAL:
!|                |   | TYPDIA = 'Q' : ANY VALUE
!|                |   | TYPDIA = 'I' : IDENTITY
!|                |   | TYPDIA = '0' : ZERO
!| TYPEXT         |-->| TYPE OF OFF-DIAGONAL TERMS
!|                |   | TYPEXT = 'Q' : ANY VALUE
!|                |   | TYPEXT = 'S' : SYMMETRIC
!|                |   | TYPEXT = '0' : ZERO
!| TYPEMESH       |-->| TYPE OF MESH (40: PRISMS, 50: PRISMS CUT INTO
!|                |   | TETRAHEDRONS)
!| XAUX           |<--| TRIDIAGONAL MATRIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PREVEREBE => PREVEREBE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN,TYPEMESH
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
!
      DOUBLE PRECISION, INTENT(IN) :: AX(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: XAUX(NPOIN,*)
!
      CHARACTER(LEN=1), INTENT(IN) :: TYPDIA,TYPEXT
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I1,I2,I3,I4,I5,I6,IELEM,NPLAN,IAN,ICOM,NPOIN2
!
!-----------------------------------------------------------------------
!
!     HERE WE CONSIDER THAT NPOIN < NELMAX TO USE XAUX AS XAUX(NPOIN,3)
!
!     XAUX(I,1) IS COEFFICIENT OF POINT BELOW I IN EQUATION OF POINT I
!     XAUX(I,2) IS THE DIAGONAL
!     XAUX(I,3) IS COEFFICIENT OF POINT ABOVE I IN EQUATION OF POINT I
!
!-----------------------------------------------------------------------
!     INITIALISES THE DIAGONAL AND OFF-DIAGONAL TERMS
!-----------------------------------------------------------------------
!
!     OFF-DIAGONAL
!
      CALL OV('X=C     ', X=XAUX(1,1), C=0.D0, DIM1=NPOIN)
      CALL OV('X=C     ', XAUX(1,3), C=0.D0, DIM1=NPOIN)
!
!     DIAGONAL
!
      IF(TYPDIA(1:1).EQ.'0') THEN
        CALL OV('X=C     ', X=XAUX(1,2), C=0.D0, DIM1=NPOIN)
      ELSEIF(TYPDIA(1:1).EQ.'I') THEN
        CALL OV('X=C     ', X=XAUX(1,2), C=1.D0, DIM1=NPOIN)
      ELSEIF(TYPDIA(1:1).EQ.'Q') THEN
        CALL OV('X=Y     ', X=XAUX(1,2), C=0.D0, DIM1=NPOIN)
      ELSE
        WRITE(LU,*) TYPDIA
        WRITE(LU,*) 'UNKNOWN TYPE OF DIAGONAL IN PREVEREBE'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!     LUMPS THE OFF-DIAGONAL TERMS
!
!     ONLY VERTICAL OF POINT, LUMPING ALL TERMS HAS BEEN TESTED, SEE
!     LINE COMMENTED, BUT DOES NOT WORK (E.G. TRY SOLITARY WAVE)
!
!
!-----------------------------------------------------------------------
!
      IF(TYPEMESH.EQ.40) THEN
!
      IF(TYPEXT.EQ.'Q') THEN
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          I4=IKLE(IELEM,4)
          I5=IKLE(IELEM,5)
          I6=IKLE(IELEM,6)
          XAUX(I1,3)=XAUX(I1,3)+AX(IELEM,03) ! TERM 1-4
          XAUX(I2,3)=XAUX(I2,3)+AX(IELEM,08) ! TERM 2-5
          XAUX(I3,3)=XAUX(I3,3)+AX(IELEM,12) ! TERM 3-6
          XAUX(I4,1)=XAUX(I4,1)+AX(IELEM,18) ! TERM 4-1
          XAUX(I5,1)=XAUX(I5,1)+AX(IELEM,23) ! TERM 5-2
          XAUX(I6,1)=XAUX(I6,1)+AX(IELEM,27) ! TERM 6-3
        ENDDO
      ELSEIF(TYPEXT.EQ.'S') THEN
        DO IELEM=1,NELEM
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
          I4=IKLE(IELEM,4)
          I5=IKLE(IELEM,5)
          I6=IKLE(IELEM,6)
!         XAUX(I1,2)=XAUX(I1,2)+AX(IELEM,01) ! TERM 1-2
!         XAUX(I2,2)=XAUX(I2,2)+AX(IELEM,01) ! TERM 2-1
!         XAUX(I1,2)=XAUX(I1,2)+AX(IELEM,02) ! TERM 1-3
!         XAUX(I2,2)=XAUX(I2,2)+AX(IELEM,02) ! TERM 3-1
          XAUX(I1,3)=XAUX(I1,3)+AX(IELEM,03) ! TERM 1-4
          XAUX(I4,1)=XAUX(I4,1)+AX(IELEM,03) ! TERM 4-1
!         XAUX(I1,3)=XAUX(I1,3)+AX(IELEM,04) ! TERM 1-5
!         XAUX(I5,1)=XAUX(I5,1)+AX(IELEM,04) ! TERM 5-1
!         XAUX(I1,3)=XAUX(I1,3)+AX(IELEM,05) ! TERM 1-6
!         XAUX(I6,1)=XAUX(I6,1)+AX(IELEM,05) ! TERM 6-1
!         XAUX(I2,2)=XAUX(I2,2)+AX(IELEM,06) ! TERM 2-3
!         XAUX(I3,2)=XAUX(I3,2)+AX(IELEM,06) ! TERM 3-2
!         XAUX(I2,3)=XAUX(I2,3)+AX(IELEM,07) ! TERM 2-4
!         XAUX(I4,1)=XAUX(I4,1)+AX(IELEM,07) ! TERM 4-2
          XAUX(I2,3)=XAUX(I2,3)+AX(IELEM,08) ! TERM 2-5
          XAUX(I5,1)=XAUX(I5,1)+AX(IELEM,08) ! TERM 5-2
!         XAUX(I2,3)=XAUX(I2,3)+AX(IELEM,09) ! TERM 2-6
!         XAUX(I6,1)=XAUX(I6,1)+AX(IELEM,09) ! TERM 6-2
!         XAUX(I3,3)=XAUX(I3,3)+AX(IELEM,10) ! TERM 3-4
!         XAUX(I4,1)=XAUX(I4,1)+AX(IELEM,10) ! TERM 4-3
!         XAUX(I3,3)=XAUX(I3,3)+AX(IELEM,11) ! TERM 3-5
!         XAUX(I5,1)=XAUX(I5,1)+AX(IELEM,11) ! TERM 5-3
          XAUX(I3,3)=XAUX(I3,3)+AX(IELEM,12) ! TERM 3-6
          XAUX(I6,1)=XAUX(I6,1)+AX(IELEM,12) ! TERM 6-3
!         XAUX(I4,2)=XAUX(I4,2)+AX(IELEM,13) ! TERM 4-5
!         XAUX(I5,2)=XAUX(I5,2)+AX(IELEM,13) ! TERM 5-4
!         XAUX(I4,2)=XAUX(I4,2)+AX(IELEM,14) ! TERM 4-6
!         XAUX(I6,2)=XAUX(I6,2)+AX(IELEM,14) ! TERM 6-4
!         XAUX(I5,2)=XAUX(I5,2)+AX(IELEM,15) ! TERM 5-6
!         XAUX(I6,2)=XAUX(I6,2)+AX(IELEM,15) ! TERM 6-5
        ENDDO
      ELSEIF(TYPEXT.EQ.'0') THEN
!       NOTHING TO DO (BUT WHAT'S THE USE OF AN ITERATIVE SOLVER ?)
      ELSE
        WRITE(LU,*) TYPEXT
        WRITE(LU,*) 'UNKNOWN TYPE OF OFF-DIAGONAL TERMS'
        WRITE(LU,*) 'IN PREVEREBE'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      ELSE
        WRITE(LU,*) TYPEMESH
        WRITE(LU,*) 'UNKNOWN TYPE OF MESH'
        WRITE(LU,*) 'IN PREVEREBE: ',TYPEMESH
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     PARALLEL MODE
!
      IF(NCSIZE.GT.1) THEN
        IAN    = 3
        ICOM   = 2
        NPOIN2 = BIEF_NBPTS(11,MESH)
        NPLAN=NPOIN/NPOIN2
        CALL PARCOM2(XAUX(1,1),XAUX(1,2),XAUX(1,3),
     &               NPOIN2,NPLAN,ICOM,IAN,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
