!                   ********************
                    SUBROUTINE SD_FABSG4
!                   ********************
!
     &(NPOIN,NSEG,DAB1,DAB2,DAB3,DAB4,XAB1,XAB2,XAB3,XAB4,
     & NPBLK,NSEGBLK,DA,XA,TYPEXT1,TYPEXT2,TYPEXT3,TYPEXT4)
!
!***********************************************************************
! BIEF   V6P2                                   08/2012
!***********************************************************************
!
!brief    TRANSFORMS A 4-MATRIX SYSTEM INTO A SINGLE BLOCK.
!
!history  E. RAZAFINDRAKOTO (LNHE)
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
!history  J. PARISI (LNHE)
!+        09/08/2012
!+        V6P2
!+   Considers the matrix types (symmetric or not) for each block.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DA             |<--| RESULTING MATRIX DIAGONAL
!| DAB1           |-->| MATRIX DIAGONAL IN THE ORIGINAL SYSTEM
!| DAB2           |-->| MATRIX DIAGONAL IN THE ORIGINAL SYSTEM
!| DAB3           |-->| MATRIX DIAGONAL IN THE ORIGINAL SYSTEM
!| DAB4           |-->| MATRIX DIAGONAL IN THE ORIGINAL SYSTEM
!| NPBLK          |-->| RANK OF FINAL BLOCK MATRIX
!| NPOIN          |-->| NUMBER OF POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| NSEGBLK        |-->| NUMBER OF SEGMENTS IN FINAL BLOCK
!| Q              |-->| NON-SYMETRIC MATRIX
!| S              |-->| SYMETRIC MATRIX
!| TYPEXT1        |-->| TYPE OF MATRIX STORAGE : BLOCK 1
!| TYPEXT2        |-->| TYPE OF MATRIX STORAGE : BLOCK 2
!| TYPEXT3        |-->| TYPE OF MATRIX STORAGE : BLOCK 3
!| TYPEXT4        |-->| TYPE OF MATRIX STORAGE : BLOCK 4
!| XA             |<--| RESULTING OFF-DIAGONAL TERMS OF MATRIX
!| XAB1           |-->| OFF-DIAGONAL TERMS IN ORIGINAL SYSTEM
!| XAB2           |-->| OFF-DIAGONAL TERMS IN ORIGINAL SYSTEM
!| XAB3           |-->| OFF-DIAGONAL TERMS IN ORIGINAL SYSTEM
!| XAB4           |-->| OFF-DIAGONAL TERMS IN ORIGINAL SYSTEM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!      USE BIEF, EX_SD_FABSG4 => SD_FABSG4
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEGBLK,NPBLK,NSEG,NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: XAB1(*),XAB2(*)
      DOUBLE PRECISION, INTENT(IN)    :: XAB3(*),XAB4(*)
      DOUBLE PRECISION, INTENT(IN)    :: DAB1(NPOIN),DAB2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: DAB3(NPOIN),DAB4(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: XA(2*NSEGBLK),DA(NPBLK)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT1,TYPEXT2
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT3,TYPEXT4
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ISEG,JSEG
!
!----------------------------------------------
!     INFO :      NPBLK   = NPOIN*NBLOC
!                 NSEGBLK = NSEG*4 + 2*NPOIN
!----------------------------------------------
!
!
!
!-------------------
! 1.  BLOCK DIAGONAL
!-------------------
!
      DO I=1,NPOIN
        DA(I) = DAB1(I)
        DA(I+NPOIN) = DAB4(I)
      ENDDO
!
!---------------------------
! 2.   EXTRADIAGONAL TERMS
!---------------------------
!
!
!     BLOCK 1
!     ------
!
      JSEG=0
!
      IF(TYPEXT1.EQ.'S') THEN
!
        DO ISEG=1,NSEG
          JSEG=JSEG+1
          XA(JSEG)        =XAB1(ISEG)
          XA(JSEG+NSEGBLK)=XAB1(ISEG)
        ENDDO
!
      ELSEIF(TYPEXT1.EQ.'Q') THEN
!
        DO ISEG=1,NSEG
          JSEG=JSEG+1
          XA(JSEG)        =XAB1(ISEG)
          XA(JSEG+NSEGBLK)=XAB1(ISEG+NSEG)
        ENDDO

      ELSE
!
        WRITE(LU,*) 'SD_FABSG4: UNEXPECTED CASE'
        WRITE(LU,*) 'TYPEXT1=',TYPEXT1
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!     BLOCKS 2 AND 3 (EXTRA-DIAG)
!     ------------------------
      DO I=1,NPOIN
        JSEG=JSEG+1
        XA(JSEG)        =DAB2(I)
        XA(JSEG+NSEGBLK)=DAB3(I)
      ENDDO
!
      IF(TYPEXT2.EQ.'S'.AND.TYPEXT3.EQ.'S') THEN
!
        DO ISEG=1,NSEG
          JSEG=JSEG+1
          XA(JSEG)        =XAB2(ISEG)
          XA(JSEG+NSEGBLK)=XAB3(ISEG)
          JSEG=JSEG+1
          XA(JSEG)        =XAB2(ISEG)
          XA(JSEG+NSEGBLK)=XAB3(ISEG)
        ENDDO
!
      ELSEIF(TYPEXT2.EQ.'Q'.AND.TYPEXT3.EQ.'Q') THEN
!
        DO ISEG=1,NSEG
          JSEG=JSEG+1
          XA(JSEG)        =XAB2(ISEG)
          XA(JSEG+NSEGBLK)=XAB3(ISEG+NSEG)
          JSEG=JSEG+1
          XA(JSEG)        =XAB2(ISEG+NSEG)
          XA(JSEG+NSEGBLK)=XAB3(ISEG)
        ENDDO
!
      ELSE
!
        WRITE(LU,*) 'SD_FABSG4: UNEXPECTED CASE'
        WRITE(LU,*) 'TYPEXT2=',TYPEXT2,' TYPEXT3=',TYPEXT3
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!     BLOCK 4 (EXTRA)
!     --------------
!
      IF(TYPEXT4.EQ.'S') THEN
!
        DO ISEG=1,NSEG
          JSEG=JSEG+1
          XA(JSEG)        =XAB4(ISEG)
          XA(JSEG+NSEGBLK)=XAB4(ISEG)
        ENDDO
!
      ELSEIF(TYPEXT4.EQ.'Q') THEN
!
        DO ISEG=1,NSEG
          JSEG=JSEG+1
          XA(JSEG)        =XAB4(ISEG)
          XA(JSEG+NSEGBLK)=XAB4(ISEG+NSEG)
        ENDDO
!
      ELSE
!
        WRITE(LU,*) 'SD_FABSG4: UNEXPECTED CASE'
        WRITE(LU,*) 'TYPEXT4=',TYPEXT4
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
