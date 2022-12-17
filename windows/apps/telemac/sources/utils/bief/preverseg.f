!                   ********************
                    SUBROUTINE PREVERSEG
!                   ********************
!
     &(XAUX,AD,AX,TYPDIA,TYPEXT,NPOIN,MESH,NSEG3D,TYPEMESH)
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
!history  JMH
!+        11/08/09
!+
!+   CROSSED AND VERTICAL SEGMENTS SWAPPED (SEE STOSEG41)
!
!history  J-M HERVOUET (LNHE)
!+        11/08/09
!+        V6P0
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
!| AD             |-->| MATRIX DIAGONAL
!| AX             |-->| MATRIX OFF-DIAGONAL TERMS
!| MESH           |-->| MESH STRUCTURE
!| NPOIN          |-->| NUMBER OF POINTS
!| NSEG3D         |-->| NUMBER OF SEGMENTS IN 3D MESH
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
      USE BIEF, EX_PREVERSEG => PREVERSEG
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NSEG3D,TYPEMESH
!
      DOUBLE PRECISION, INTENT(IN)    :: AD(NPOIN),AX(NSEG3D,*)
      DOUBLE PRECISION, INTENT(INOUT) :: XAUX(NPOIN,*)
!
      CHARACTER(LEN=1), INTENT(IN) :: TYPDIA,TYPEXT
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I2,I3,NPLAN,IAN,ICOM,NPOIN2,SEGUP,SEGDOWN,NSEG2D
      INTEGER IPLAN,NSEGH
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
!     INITIALISES THE DIAGONAL
!-----------------------------------------------------------------------
!
      IF(TYPDIA(1:1).EQ.'0') THEN
        CALL OV('X=C     ', X=XAUX(1,2), C=0.D0, DIM1=NPOIN)
      ELSEIF(TYPDIA(1:1).EQ.'I') THEN
        CALL OV('X=C     ', X=XAUX(1,2), C=1.D0, DIM1=NPOIN)
      ELSEIF(TYPDIA(1:1).EQ.'Q') THEN
        CALL OV('X=Y     ', X=XAUX(1,2), Y=AD, DIM1=NPOIN)
      ELSE
        WRITE(LU,*) TYPDIA
        WRITE(LU,*) 'UNKNOWN TYPE OF DIAGONAL IN PREVERSEG'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!     LUMPS THE OFF-DIAGONAL TERMS CORRESPONDING TO VERTICAL SEGMENTS
!-----------------------------------------------------------------------
!
      NPOIN2 = BIEF_NBPTS(11,MESH)
      NPLAN  = NPOIN/NPOIN2
      NSEG2D = BIEF_NBSEG(11,MESH)
      NSEGH  = NSEG2D*NPLAN
!
      IF(TYPEMESH.EQ.40.OR.TYPEMESH.EQ.50) THEN
!
      IF(TYPEXT.EQ.'Q') THEN
!       PLANE ON THE BOTTOM
        DO I2=1,NPOIN2
          SEGUP=NSEGH+I2
          XAUX(I2,1)=0.D0
          XAUX(I2,3)=AX(SEGUP,1)
        ENDDO
!       PLANE AT THE FREE SURFACE
        DO I2=1,NPOIN2
          I3=I2+(NPLAN-1)*NPOIN2
          SEGDOWN=NSEGH+NPOIN2*(NPLAN-2)+I2
          XAUX(I3,1)=AX(SEGDOWN,2)
          XAUX(I3,3)=0.D0
        ENDDO
!       OTHER PLANES
        IF(NPLAN.GT.2) THEN
        DO IPLAN=2,NPLAN-1
          DO I2=1,NPOIN2
            I3=I2+(IPLAN-1)*NPOIN2
            SEGDOWN=NSEGH+NPOIN2*(IPLAN-2)+I2
            SEGUP  =SEGDOWN+NPOIN2
            XAUX(I3,1)=AX(SEGDOWN,2)
            XAUX(I3,3)=AX(SEGUP,1)
          ENDDO
        ENDDO
        ENDIF
      ELSEIF(TYPEXT.EQ.'S') THEN
!       PLANE ON THE BOTTOM
        DO I2=1,NPOIN2
          SEGUP=NSEGH+I2
          XAUX(I2,1)=0.D0
          XAUX(I2,3)=AX(SEGUP,1)
        ENDDO
!       PLANE AT THE FREE SURFACE
        DO I2=1,NPOIN2
          I3=I2+(NPLAN-1)*NPOIN2
          SEGDOWN=NSEGH+NPOIN2*(NPLAN-2)+I2
          XAUX(I3,1)=AX(SEGDOWN,1)
          XAUX(I3,3)=0.D0
        ENDDO
!       OTHER PLANES
        IF(NPLAN.GT.2) THEN
        DO IPLAN=2,NPLAN-1
          DO I2=1,NPOIN2
            I3=I2+(IPLAN-1)*NPOIN2
            SEGDOWN=NSEGH+NPOIN2*(IPLAN-2)+I2
            SEGUP  =SEGDOWN+NPOIN2
            XAUX(I3,1)=AX(SEGDOWN,1)
            XAUX(I3,3)=AX(SEGUP,1)
          ENDDO
        ENDDO
        ENDIF
      ELSEIF(TYPEXT.EQ.'0') THEN
!       NOTHING TO DO (BUT WHAT'S THE USE OF AN ITERATIVE SOLVER ?)
      ELSE
        WRITE(LU,*) TYPEXT
        WRITE(LU,*) 'UNKNOWN TYPE OF OFF-DIAGONAL TERMS'
        WRITE(LU,*) 'IN PREVERSEG'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      ELSE
        WRITE(LU,*) TYPEMESH
        WRITE(LU,*) 'UNKNOWN TYPE OF MESH'
        WRITE(LU,*) 'IN PREVERSEG: ',TYPEMESH
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
        CALL PARCOM2(XAUX(1,1),XAUX(1,2),XAUX(1,3),
     &               NPOIN2,NPLAN,ICOM,IAN,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
