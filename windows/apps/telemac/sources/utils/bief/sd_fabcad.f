!                   ********************
                    SUBROUTINE SD_FABCAD
!                   ********************
!
     &(NPBLK,NSEGBLK,IN,IP,ISEGIP,
     & INDTRI,ISTRI,INX,IPX,ACTRI,XA1,XA2,DA,AC)
!
!***********************************************************************
! BIEF   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    BUILDS A COMPACT STORAGE
!+               (INX,IPX) STRUCTURE WITH THE DIAGONAL
!+                VIA (IN,IP) = (XADJ, ADJNCY) OF EXTRADIAGONAL TERMS
!+                AND THE SEGMENT STORAGE (ISEGIP, XA, DA).
!
!note     IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
!
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
!history  E. RAZAFINDRAKOTO (LNH)
!+        21/11/11
!+        V6P1
!+
!
!history  J-M HERVOUET (LNHE)
!+        08/06/2012
!+        V6P2
!+   Dimensions and algorithm slightly changed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<--|COMPACT STORED MATRIX WITH DIAGONAL
!| ACTRI          |---|REAL WORKING STORAGE
!| DA             |-->|MATRIX DIAGONAL COEFFICIENTS
!| (IN,IP)        |-->|STRUCTURE WITHOUT THE DIAGONAL
!| INDTRI         |---|INTEGER WORKING STORAGE
!| (INX,IPX)      |<--|STRUCTURE WITH THE DIAGONAL
!| ISEGIP         |-->|INVERSE TABLE OF CONNECTIVITY: POINT ---> SEGMENT
!| ISTRI          |---|INTEGER WORKING STORAGE
!| NPBLK          |-->| SIZE OF MATRIX DIAGONAL
!| NSEGBLK        |-->| NUMBER OF SEGMENTS IN ORIGINAL MATRIX
!| XA1            |-->| OFF-DIAGONAL TERM OF MATRIX A
!| XA2            |-->| OFF-DIAGONAL TERM OF MATRIX A
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_FABCAD => SD_FABCAD
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPBLK,NSEGBLK
      INTEGER, INTENT(IN)             :: IN(NPBLK+1),IP(NSEGBLK*2)
      INTEGER, INTENT(IN)             :: ISEGIP(NSEGBLK*2)
      INTEGER, INTENT(INOUT)          :: INDTRI(NPBLK)
      INTEGER, INTENT(INOUT)          :: ISTRI(NPBLK)
      INTEGER, INTENT(INOUT)          :: INX(NPBLK+1)
      INTEGER, INTENT(INOUT)          :: IPX(NSEGBLK*2+NPBLK)
      DOUBLE PRECISION, INTENT(INOUT) :: ACTRI(NPBLK)
      DOUBLE PRECISION, INTENT(IN)    :: XA1(NSEGBLK),XA2(NSEGBLK)
      DOUBLE PRECISION, INTENT(IN)    :: DA(NPBLK)
      DOUBLE PRECISION, INTENT(INOUT) :: AC(NSEGBLK*2+NPBLK)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,J1,J2,JN,ISEG,ND
!
!-----------------------------------------------------------------------
!
!---> COMPACT STORAGE WITH THE DIAGONAL ADDED : (XADJ, ADJNCY) = (INX,IPX)
!
      DO I = 1, NPBLK+1
        INX(I) = IN(I)+I-1
      ENDDO
!
!     J2 WILL BE THE ADDRESS IN AC
      J2=0
      DO I = 1, NPBLK
        IPX(INX(I)) = I
!       DIAGONAL AS FIRST COEFFICIENT OF THE LIST
        J2=J2+1
!       NOTE: HERE J2=INX(I)
        AC(INX(I)) = DA(I)
!       LOOP ON MATRIX COEFFICIENTS OF POINT I
!       EXCLUDING DIAGONAL TERMS AT ADDRESS INX(I)
        DO J1 = INX(I)+1, INX(I+1)-1
!         BACK TO ADDRESS WITHOUT THE DIAGONAL
          JN = J1-I
          J = IP(JN)
          J2=J2+1
          ISEG = ISEGIP(JN)
          IPX(J2) = J
          IF(ISEG.LT.0) THEN
            AC(J2) = XA1(-ISEG)
          ELSEIF(ISEG.GT.0) THEN
            AC(J2) = XA2(ISEG)
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, NPBLK
        ND = INX(I+1)-INX(I)
        DO J = 1,ND
          ISTRI(J) = IPX(INX(I)+J-1)
          ACTRI(J) = AC(INX(I)+J-1)
        ENDDO
        CALL SD_STRTRI(ISTRI,ND,INDTRI)
        DO J = 1,ND
          J1 = INDTRI(J)
          IPX(INX(I)+J-1) = ISTRI(J1)
          AC(INX(I)+J-1) = ACTRI(J1)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
