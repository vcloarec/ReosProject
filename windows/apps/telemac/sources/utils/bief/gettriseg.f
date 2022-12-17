!                   ********************
                    SUBROUTINE GETTRISEG
!                   ********************
!
     &(XAUX,AD,AX,TETA,NPOIN,MESH,NSEG3D,NSEG2D,NPLAN,NPOIN2,IELM3)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GETS THE TRIDIAGONAL PART OF A DIFFUSION MATRIX ON
!+                PRISMS AND REMOVES IT FROM THE INITIAL MATRIX.
!code
!+           IF MTRI IS THIS TRIDIAGONAL PART, MAUX THE RESULT AND MDIF
!+           THE DIFFUSION MATRIX, THIS SUBROUTINE DOES:
!+
!+           MAUX = TETA * MTRI
!+           MDIF CHANGED INTO (1-TETA) * MDIF
!+
!+           SEGMENT STORAGE FOR MDIFF HERE !!!!!!!!!!!!!!!!!!!!!!!!
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNHE)
!+        11/08/09
!+        V6P0
!+   CROSSED AND VERTICAL SEGMENTS SWAPPED (SEE STOSEG41)
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
!| AD             |-->| DIAGONAL TERMS OF MATRIX
!| AX             |-->| OFF-DIAGONAL TERMS OF MATRIX
!|                |   | HERE DIMENSION 1 BECAUSE SYMMETRY
!| IELM3          |-->| TYPE OF ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN          |-->| NUMBER OF POINTS
!| NPOIN2         |-->| NUMBER OF POINTS OF 2D MESH
!| NSEG2D         |-->| NUMBER OF SEGMENTS IN 2D
!| NSEG3D         |-->| NUMBER OF SEGMENTS IN 3D
!| TETA           |-->| COEFFICIENT USED IN THE RESULT
!| XAUX           |<--| THE RESULTING MATRIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_GETTRISEG => GETTRISEG
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NSEG3D,NSEG2D,NPLAN,NPOIN2,IELM3
!
      DOUBLE PRECISION, INTENT(IN)    :: TETA
      DOUBLE PRECISION, INTENT(INOUT) :: XAUX(NPOIN,*),AX(NSEG3D)
      DOUBLE PRECISION, INTENT(INOUT) :: AD(NPOIN)
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I2,I3,IPLAN,IAN,ICOM,SEGUP,SEGDOWN,NSEGH,NSEGV
!
!-----------------------------------------------------------------------
!
!     CONSIDERS HERE THAT NPOIN < NELMAX TO USE XAUX AS XAUX(NPOIN,3)
!
!     XAUX(I,1) IS COEFFICIENT OF POINT BELOW I IN EQUATION OF POINT I
!     XAUX(I,2) IS THE DIAGONAL
!     XAUX(I,3) IS COEFFICIENT OF POINT ABOVE I IN EQUATION OF POINT I
!
!-----------------------------------------------------------------------
!     INITIALISES THE DIAGONAL TERMS
!-----------------------------------------------------------------------
!
      CALL OV('X=CY    ', X=XAUX(1,2), Y=AD, C=TETA, DIM1=NPOIN)
      CALL OV('X=CX    ', X=AD, C=1.D0-TETA, DIM1=NPOIN)
!
!-----------------------------------------------------------------------
!     TRIDIAGONAL TERMS
!-----------------------------------------------------------------------
!
!     VERTICAL SEGMENTS HAVE THE SAME POSITION, SEE STOSEG41,STOSEG51
!
      IF(IELM3.EQ.41.OR.IELM3.EQ.51) THEN
!
        NSEGH=NSEG2D*NPLAN
        NSEGV=NPOIN2*(NPLAN-1)
!
!       PLANE ON THE BOTTOM
!
        DO I2=1,NPOIN2
          SEGUP=NSEGH+I2
          XAUX(I2,1)=0.D0
          XAUX(I2,3)=TETA*AX(SEGUP)
        ENDDO
!
!       PLANE AT THE FREE SURFACE
!
        DO I2=1,NPOIN2
          I3=I2+(NPLAN-1)*NPOIN2
          SEGDOWN=NSEGH+NPOIN2*(NPLAN-2)+I2
          XAUX(I3,1)=TETA*AX(SEGDOWN)
          XAUX(I3,3)=0.D0
        ENDDO
!
!       OTHER PLANES
!
        IF(NPLAN.GT.2) THEN
          DO IPLAN=2,NPLAN-1
            DO I2=1,NPOIN2
              I3=I2+(IPLAN-1)*NPOIN2
              SEGDOWN=NSEGH+NPOIN2*(IPLAN-2)+I2
              SEGUP  =SEGDOWN+NPOIN2
              XAUX(I3,1)=TETA*AX(SEGDOWN)
              XAUX(I3,3)=TETA*AX(SEGUP)
            ENDDO
          ENDDO
        ENDIF
!
        CALL OV('X=CX    ', X=AX(NSEGH+1:NSEGH+NSEGV),
     &          C=1.D0-TETA, DIM1=NSEGV)
!
      ELSE
        WRITE(LU,*) 'GETTRISEG: UNKNOWN ELEMENT:',IELM3
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
