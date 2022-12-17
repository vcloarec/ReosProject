!                   ********************
                    SUBROUTINE SD_STRSSD
!                   ********************
!
     &(NPBLK,NSEGBLK,GLOSEG1,GLOSEG2,IN,IP,ISEGIP,IW)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS COMPACT STORAGE
!+               (IN,IP) = (XADJ, ADJNCY)OF EXTRADIAGONAL TERMS
!+                VIA SEGMENT STORAGE.
!
!warning  NSEG AND IW ARE THE SAME ARRAY IN THE MEMORY
!
!history  E.RAZAFINDRAKOTO (LNH)
!+        18/02/08
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
!history  J-M HERVOUET (LNHE)
!+        08/06/2012
!+        V6P2
!+   Dimensions changed in declarations and allocation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| GLOSEG1        |-->| FIRST POINT OF SEGMENTS
!| GLOSEG2        |-->| SECOND POINT OF SEGMENTS
!| IN             |<--| (IN, IP) COMPACT STORAGE OF EXTRADIAGONAL TERMS
!| IP             |<--| (IN, IP) COMPACT STORAGE OF EXTRADIAGONAL TERMS
!| ISEGIP         |<--| INVERSE TABLE OF CONNECTIVITY: POINT-->SEGMENT
!| IW             |<--| NUMBER OF NEIGHBOURS OF POINTS
!| NPBLK          |-->| NUMBER OF POINTS
!| NSEGBLK        |-->| NUMBER OF SEGMENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF,EX_SD_STRSSD => SD_STRSSD
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NSEGBLK,NPBLK
      INTEGER, INTENT(IN)    :: GLOSEG1(NSEGBLK),GLOSEG2(NSEGBLK)
      INTEGER, INTENT(INOUT) :: IN(NPBLK+1),IP(NSEGBLK*2)
      INTEGER, INTENT(INOUT) :: ISEGIP(NSEGBLK*2)
      INTEGER, INTENT(INOUT) :: IW(NPBLK)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J1,J2,ISEG
!
!  -----------------------
!
!---> DEGREE OF A POINT: NUMBER OF NEIGHBOURS
!
      DO I=1,NPBLK
        IW(I)=0
      ENDDO
!
!     IW : NUMBER OF NEIGHBOURS FOR EACH POINT
!
      DO ISEG=1,NSEGBLK
        J1 = GLOSEG1(ISEG)
        J2 = GLOSEG2(ISEG)
        IW(J1) = IW(J1) + 1
        IW(J2) = IW(J2) + 1
      ENDDO
!
!---> COMPACT STORAGE WITHOUT THE DIAGONAL: (XADJ,ADJNCY) = (IN,IP)
!
!     COEFFICIENTS FOR POINT I: FROM IN(I) TO IN(I+1)-1
!
      IN(1)=1
      DO I=1,NPBLK
        IN(I+1)=IN(I)+IW(I)
      ENDDO
!
!     IW IS NO LONGER THE NUMBER OF NEIGHBOURS
!
      DO I=1,NPBLK
        IW(I)=IN(I)
      ENDDO
!
      DO ISEG=1,NSEGBLK
        J1 = GLOSEG1(ISEG)
        J2 = GLOSEG2(ISEG)
!
!-->    TABLE OF CONNECTIVITY: SEGMENT ---> POINT
!       WILL SAY TO WHAT POINT A COEFFICIENT REFERS TO
!
        IP(IW(J1))=J2
        IP(IW(J2))=J1
!
!-->    INVERSE TABLE OF CONNECTIVITY: POINT ---> SEGMENT
!       WILL SAY TO WHAT SEGMENT A COEFFICIENT REFERS TO
!
!       NOTATION FOR TRIANGULAR SUPERIOR COEFF.
        ISEGIP(IW(J1))=-ISEG
!       NOTATION FOR TRIANGULAR INFERIOR COEFF.
        ISEGIP(IW(J2))= ISEG
!
        IW(J1) = IW(J1) + 1
        IW(J2) = IW(J2) + 1
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
