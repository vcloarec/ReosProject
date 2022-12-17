!                       ***************************
                        SUBROUTINE GREDEL_FDNRST
!                       ***************************
     &(IFRM,ITO,X,Y,NODENRS,NPOIN2,IFRM1,ITOP1)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    FINDS THE NEAREST FROM -1 AND TO +1 POINTER.
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
!history  LEO POSTMA (DELFT HYDRAULICS)
!+        03/04/2007
!+        V5P7
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IFRM           |-->|
!| IFRM1          |---|
!| ITO            |-->|
!| ITOP1          |---|
!| NODENRS        |-->| IF > 0 : NODE NUMBER
!|                |   | IF
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES
!| X,Y            |-->| NODE COORDINATES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: IFRM,ITO,NPOIN2
      INTEGER, INTENT(IN)          :: NODENRS(NPOIN2)
      INTEGER, INTENT(INOUT)       :: IFRM1,ITOP1
      REAL, INTENT(IN) :: X(NPOIN2), Y(NPOIN2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN
      REAL XFRM1,XTOP1,YFRM1,YTOP1,DISFRM,DISTO,DX,DY
!
!-----------------------------------------------------------------------
!
      XFRM1  = X(IFRM)
      YFRM1  = Y(IFRM)
      XTOP1  = X(ITO )
      YTOP1  = Y(ITO )
      DISFRM = XFRM1-XTOP1
      XFRM1  = XFRM1 + DISFRM
      XTOP1  = XTOP1 - DISFRM
      DISFRM = YFRM1-YTOP1
      YFRM1  = YFRM1 + DISFRM
      YTOP1  = YTOP1 - DISFRM
!
      DX     = XFRM1-X(1)
      DY     = YFRM1-Y(1)
      DISFRM = DX*DX + DY*DY
      DX     = XTOP1-X(1)
      DY     = YTOP1-Y(1)
      DISTO  = DX*DX + DY*DY
      IFRM1  = 1
      ITOP1  = 1
      DO IPOIN = 2, NPOIN2
        DX     = XFRM1-X(IPOIN)
        DY     = YFRM1-Y(IPOIN)
        DX     = DX*DX + DY*DY
        IF(DX.LT.DISFRM) THEN
          DISFRM = DX
          IFRM1  = IPOIN
        ENDIF
        DX     = XTOP1-X(IPOIN)
        DY     = YTOP1-Y(IPOIN)
        DX     = DX*DX + DY*DY
        IF(DX.LT.DISTO) THEN
          DISTO  = DX
          ITOP1  = IPOIN
        ENDIF
      ENDDO
      IFRM1 = NODENRS(IFRM1)
      ITOP1 = NODENRS(ITOP1)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE GREDEL_FDNRST

