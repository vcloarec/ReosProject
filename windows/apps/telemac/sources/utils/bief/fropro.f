!                   *****************
                    SUBROUTINE FROPRO
!                   *****************
!
     &(NBOR,IKLE,NELEM,NELMAX,NPOIN,NPMAX,NPTFR,IELM,
     & IKLEM1,LIMVOI,OPTASS,PRODUC,MXPTVS,T1,
     & GLOSEG,SIZGLO,NSEG)
!
!***********************************************************************
! BIEF   V6P3                                  21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE ARRAYS GIVING ADRESSES FOR
!+                FRONTAL MATRIX-VECTOR PRODUCT.
!
!history  J-M HERVOUET (LNHE)
!+        20/03/08
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        11/03/2013
!+        V6P3
!+   Dimension of LIMVOI now set to 11.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| GLOSEG         |-->| FIRST AND SECOND POINT OF SEGMENTS
!| IELM           |-->| TYPE OF ELEMENT.
!|                |   | 11 : TRIANGLES.
!|                |   | 21 : QUADRANGLES.
!| IKLE           |-->| CONNECTIVITY TABLE.
!| IKLEM1         |<--| VARIOUS ADDRESSES IN OFF-DIAGONAL PART
!|                |   | OF A MATRIX
!| LIMVOI         |<--| LIMVOI(K,1) : BEGINNING OF SERIES WITH K NEIGHBOURS
!|                |   | LIMVOI(K,2) : END       OF SERIES WITH K NEIGHBOURS
!| MXPTVS         |-->| MAXIMUM NUMBER OF NEIGHBOURS OF A POINT
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPMAX          |-->| MAXIMUM NUMBER OF POINTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| OPTASS         |-->| OPTION OF MATRIX STORAGE
!|                |   | 1: ELEMENT PER ELEMENT 3: EDGE-BASED
!| PRODUC         |-->| CHOICE OF MATRIX-VECTOR PRODUCT
!|                |   | 1: NORMAL 2: FRONTAL
!| SIZGLO         |-->| FIRST DIMENSION OF GLOSEG
!| T1             |-->| INTEGER WORK ARRAY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FROPRO => FROPRO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELMAX,NPMAX,MXPTVS,NELEM
      INTEGER, INTENT(IN)    :: NPOIN,NPTFR,IELM,OPTASS,PRODUC
      INTEGER, INTENT(IN)    :: NSEG,SIZGLO,NBOR(*)
      INTEGER, INTENT(IN)    :: IKLE(NELMAX,*),GLOSEG(SIZGLO,2)
      INTEGER, INTENT(OUT)   :: IKLEM1(NPMAX,MXPTVS,4,2)
!                                      11: SEE ALMESH AND OPASS
      INTEGER, INTENT(OUT)   :: LIMVOI(11,2)
      INTEGER, INTENT(OUT)   :: T1(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IPTFR,IPOIN,ISG,K,I,I1,I2,NBVOIS
!
!-----------------------------------------------------------------------
!
      IF(IELM.NE.11) THEN
        WRITE(LU,901) IELM
901     FORMAT(1X,'FROPRO: IELM=',1I6,' UNKNOWN TYPE OF ELEMENT')
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(PRODUC.EQ.2) THEN
!
!=======================================================================
! COMPUTES THE NUMBER OF NEIGHBOURING POINTS AND ELEMENTS
!=======================================================================
!
        DO IPOIN = 1,NPOIN
          T1(IPOIN) = 0
        ENDDO
!
!       NUMBER OF ELEMENTS NEIGHBOURING A POINT
!
        DO IELEM = 1,NELEM
          T1(IKLE(IELEM,1)) = T1(IKLE(IELEM,1)) + 1
          T1(IKLE(IELEM,2)) = T1(IKLE(IELEM,2)) + 1
          T1(IKLE(IELEM,3)) = T1(IKLE(IELEM,3)) + 1
        ENDDO
!
!       NUMBER OF POINTS NEIGHBOURING A POINT
!     = NUMBER OF ELEMENTS NEIGHBOURING A POINT
!     + 1 ON BOUNDARIES
!
        DO IPTFR = 1,NPTFR
          T1(NBOR(IPTFR)) = T1(NBOR(IPTFR)) + 1
        ENDDO
!
!=======================================================================
! CHECKS THAT THE RENUMBERING WAS MADE CORRECTLY IN STBTEL
! FILLS IN LIMVOI AND IKLEM1
!=======================================================================
!
        IF(T1(1).EQ.0) THEN
          WRITE(LU,97)
97        FORMAT(1X,'FROPRO: POINT NUMBER 1 HAS NO NEIGHBOUR')
          CALL PLANTE(1)
          STOP
        ENDIF
!
        DO IPOIN = 2,NPOIN
          IF(T1(IPOIN).LT.T1(IPOIN-1)) THEN
            WRITE(LU,99)
99          FORMAT(1X,'FROPRO: FRONTAL PRODUCT REQUIRES A',/,1X,
     &                'RENUMBERING OF POINTS WITH STBTEL')
            CALL PLANTE(1)
            STOP
          ELSEIF(T1(IPOIN).GT.MXPTVS) THEN
            WRITE(LU,95) IPOIN
95          FORMAT(1X,'FROPRO: POINT ',1I6,' HAS TOO MANY NEIGHBOURS')
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
!
!  BUILDS ARRAY LIMVOI
!  LIMVOI(K,1) : BEGINNING OF SERIES WITH K NEIGHBOURS
!  LIMVOI(K,2) : END       OF SERIES WITH K NEIGHBOURS
!
        DO K=1,MXPTVS
          LIMVOI(K,1) = 0
          LIMVOI(K,2) = 0
        ENDDO
!       POINT 1 IS THE BEGINNING OF SERIES WITH T1(1) NEIGHBOURS
        NBVOIS = T1(1)
        LIMVOI(NBVOIS,1) = 1
        DO I=2,NPOIN
          IF(T1(I).NE.NBVOIS) THEN
!         PREVIOUS POINT WAS AN END OF A SERIES
          LIMVOI(NBVOIS,2) = I-1
!         CURRENT POINT IS THE BEGINNING OF A SERIES
          NBVOIS = T1(I)
          LIMVOI(NBVOIS,1) = I
          ENDIF
        ENDDO
!       POINT NPOIN IS THE END OF ITS SERIES
        LIMVOI(NBVOIS,2) = NPOIN
!
!   ARRAYS FOR FRONTAL MATRIX-VECTOR PRODUCT :
!
        DO IPOIN = 1,NPOIN
          T1(IPOIN) = 1
        ENDDO
!
        IF(OPTASS.EQ.3) THEN
!
!       IY DOES NOT DEPEND HERE ON THE DIRECT OR TRANSPOSE CHARACTER
        DO ISG = 1,NSEG
          I1 = GLOSEG(ISG,1)
          I2 = GLOSEG(ISG,2)
!
!         ANY MATRIX
!         IXM IN DIRECT PRODUCT
          IKLEM1(I1,T1(I1),1,1) = ISG
          IKLEM1(I2,T1(I2),1,1) = ISG + NSEG
!         IY IN DIRECT PRODUCT
          IKLEM1(I1,T1(I1),2,1) = I2
          IKLEM1(I2,T1(I2),2,1) = I1
!         IXM IN TRANSPOSE PRODUCT
          IKLEM1(I1,T1(I1),3,1) = ISG + NSEG
          IKLEM1(I2,T1(I2),3,1) = ISG
!         IY IN TRANSPOSE PRODUCT
          IKLEM1(I1,T1(I1),4,1) = I2
          IKLEM1(I2,T1(I2),4,1) = I1
!
!         SYMMETRICAL MATRIX
!         IXM IN DIRECT PRODUCT
          IKLEM1(I1,T1(I1),1,2) = ISG
          IKLEM1(I2,T1(I2),1,2) = ISG
!         IY IN DIRECT PRODUCT
          IKLEM1(I1,T1(I1),2,2) = I2
          IKLEM1(I2,T1(I2),2,2) = I1
!         IXM IN TRANSPOSE PRODUCT
          IKLEM1(I1,T1(I1),3,2) = ISG
          IKLEM1(I2,T1(I2),3,2) = ISG
!         IY IN TRANSPOSE PRODUCT
          IKLEM1(I1,T1(I1),4,2) = I2
          IKLEM1(I2,T1(I2),4,2) = I1
!
!         UPDATES THE NUMBER OF NEIGHBOURS
          T1(I1) = T1(I1) + 1
          T1(I2) = T1(I2) + 1
        ENDDO
!
        ELSE
          WRITE(LU,*) 'UNKNOWN STORAGE IN FROPRO :',OPTASS
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
