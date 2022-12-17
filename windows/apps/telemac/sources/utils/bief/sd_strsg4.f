!                   ********************
                    SUBROUTINE SD_STRSG4
!                   ********************
!
     &(NPOIN,NSEG,GLOSEGB,NSEGBLK,GLOSEG4)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    TRANSFORMS THE SEGMENTS OF A 4-MATRIX SYSTEM INTO
!          THE SEGMENTS OF A SINGLE BLOCK.
!
!history  E. RAZAFINDRAKOTO (LNH)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| GLOSEG4        |<--| FIRST AND SECOND POINT OF SEGMENTS IN BLOCK
!| GLOSEGB        |-->| FIRST AND SECOND POINT OF SEGMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| NSEGBLK        |-->| NUMBER OF SEGMENTS IN THE FINAL BLOCK
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_STRSG4 => SD_STRSG4
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NSEGBLK,NSEG,NPOIN
      INTEGER, INTENT(IN)    :: GLOSEGB(NSEG,2)
      INTEGER, INTENT(INOUT) :: GLOSEG4(2*NSEGBLK)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ISEG,JSEG
!
!----------------------------------------------
!     INFO :     NSEGBLK=NSEG*4 +   NPOIN !!!!
!----------------------------------------------
!
!     MATRIX ASSEMBLES TOTAL BLOCKS:
!
!----------------------------------------------
!
      JSEG=0
!
!     BLOCK 1 (EXTRA-DIAGONAL TERMS)
!     ------------------------------
!
      DO ISEG=1,NSEG
        JSEG=JSEG+1
        GLOSEG4(JSEG)= GLOSEGB(ISEG,1)
        GLOSEG4(JSEG+NSEGBLK)= GLOSEGB(ISEG,2)
      ENDDO
!
!     BLOCKS 2 AND 3 (PREVIOUSLY DIAGONAL TERMS)
!     ------------------------------------------
!
      DO I=1,NPOIN
        JSEG=JSEG+1
        GLOSEG4(JSEG)= I
        GLOSEG4(JSEG+NSEGBLK)= I+NPOIN
      ENDDO
!
!     BLOCKS 2 AND 3 (EXTRA-DIAGONAL TERMS)
!     -------------------------------------
!
      DO ISEG=1,NSEG
        JSEG=JSEG+1
        GLOSEG4(JSEG)= GLOSEGB(ISEG,1)
        GLOSEG4(JSEG+NSEGBLK)= GLOSEGB(ISEG,2)+ NPOIN
        JSEG=JSEG+1
        GLOSEG4(JSEG)= GLOSEGB(ISEG,2)
        GLOSEG4(JSEG+NSEGBLK)= GLOSEGB(ISEG,1)+ NPOIN
      ENDDO
!
!     BLOCK 4 (EXTRA-DIAGONAL TERMS)
!     ------------------------------
!
      DO ISEG=1,NSEG
        JSEG=JSEG+1
        GLOSEG4(JSEG)         = GLOSEGB(ISEG,1)+NPOIN
        GLOSEG4(JSEG+NSEGBLK) = GLOSEGB(ISEG,2)+NPOIN
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
