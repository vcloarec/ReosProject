!                   *******************
                    SUBROUTINE COMP_FAC
!                   *******************
!
     &(ELTSEG,ORISEG,IFABOR,NELEM,NPOIN,IFAC)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    COMPLETES THE ARRAY IFAC FOR QUADRATIC POINTS
!+                AT THE INTERFACE BETWEEN 2 SUBDOMAINS.
!+
!
!history  J-M HERVOUET (LNHE)
!+        24/10/08
!+        V5P9
!+   First version
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
!+        30/07/2015
!+        V7P1
!+   FAC replaced by IFAC
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTSEG         |-->| GIVES THE SEGMENT NUMBER OF EDGES OF ELEMENTS
!| ORISEG         |-->| ORIENTATION OF SEGMENTS (1 OR 2).
!| FAC            |<->| COEFFICIENT FOR COMPUTING DOT PRODUCTS IN //
!| IFABOR         |-->| -2 MEANS INTERFACE WITH ANOTHER SUB-DOMAIN
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_COMP_FAC => COMP_FAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELEM,NPOIN
      INTEGER, INTENT(IN)    :: IFABOR(NELEM,3),ELTSEG(NELEM,3)
      INTEGER, INTENT(IN)    :: ORISEG(NELEM,3)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: IFAC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
!
!-----------------------------------------------------------------------
!
      DO IELEM=1,NELEM
!
!       ALL INITIALISED WITH 1
!
        IFAC%I(NPOIN+ELTSEG(IELEM,1))=1
        IFAC%I(NPOIN+ELTSEG(IELEM,2))=1
        IFAC%I(NPOIN+ELTSEG(IELEM,3))=1
!
!       ON PARALLEL INTERFACES, SEGMENTS WITH ORISEG=2 ARE SET TO 0
!       THE SEGMENT ON THE OTHER SIDE WILL HAVE 1
!
        IF(IFABOR(IELEM,1).EQ.-2.AND.ORISEG(IELEM,1).EQ.2) THEN
          IFAC%I(NPOIN+ELTSEG(IELEM,1))=0
        ENDIF
        IF(IFABOR(IELEM,2).EQ.-2.AND.ORISEG(IELEM,2).EQ.2) THEN
          IFAC%I(NPOIN+ELTSEG(IELEM,2))=0
        ENDIF
        IF(IFABOR(IELEM,3).EQ.-2.AND.ORISEG(IELEM,3).EQ.2) THEN
          IFAC%I(NPOIN+ELTSEG(IELEM,3))=0
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
