!                   ******************
                    SUBROUTINE ELEBD31
!                   ******************
!
     &(NELBOR,NULONE,IKLBOR,IFABOR,NBOR,IKLE,
     & NELEM,NELEB,NELMAX,NPOIN,NPTFR,IELM)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS NELBOR, NULONE, IKLBORD.
!
!history  LAM MINH-PHUONG
!+
!+
!+
!
!history  J-M HERVOUET (LNH)
!+        09/04/04
!+        V5P5
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
!history  S.E.BOURBAN (HRW)
!+        21/03/2017
!+        V7P3
!+   Replacement of the DATA declarations by the PARAMETER associates
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IELM           |-->| TYPE D'ELEMENT.
!| IFABOR         |-->| TABLEAU DES VOISINS DES FACES.
!| IKLBOR         |<--| NUMERO LOCAL DES NOEUDS A PARTIR D'UN ELEMENT
!|                |   | DE BORD
!| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
!| NBOR           |-->| NUMERO GLOBAL D'UN NOEUD A PARTIR DU NUMERO LOCAL
!| NELBOR         |<--| NUMERO DE L'ELEMENT ADJACENT AU KIEME SEGMENT
!| NELEB          |-->| NOMBRE D'ELEMENTS DE BORD.
!| NELEM          |-->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE.
!| NELMAX         |---|
!| NPOIN          |-->| NOMBRE TOTAL DE POINTS DU DOMAINE.
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
!| NULONE         |<--| NUMERO LOCAL D'UN POINT DE BORD DANS
!|                |   | L'ELEMENT ADJACENT DONNE PAR NELBOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ELEBD31 => ELEBD31
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELEM,NELEB,NELMAX
      INTEGER, INTENT(IN)    :: NPOIN,NPTFR,IELM
      INTEGER, INTENT(IN)    :: NBOR(NPTFR)
      INTEGER, INTENT(IN)    :: IFABOR(NELMAX,4)
      INTEGER, INTENT(IN)    :: IKLE(NELEM,4)
      INTEGER, INTENT(OUT)   :: NELBOR(NELEB),NULONE(NELEB,3)
      INTEGER, INTENT(OUT)   :: IKLBOR(NELEB,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER   :: IELEM, IELEB, J,K,IPOIN
      INTEGER   :: IPOBO(NPOIN)
!
      INTEGER :: SOMFAC(3,4)
      PARAMETER ( SOMFAC = RESHAPE( (/
     &           1,2,3 , 4,1,2 , 2,3,4 , 3,4,1  /), SHAPE=(/ 3,4 /) ) )
!     SIDE NUMBER:   1       2       3       4
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(IELM /= 31) THEN
        WRITE(LU,99) IELM
99      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE OF ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
! BUILDS IPOBO TO GO FROM GLOBAL NUMBERING TO LOCAL NUMBERING
      DO IPOIN=1,NPOIN
        IPOBO(IPOIN) = 0
      ENDDO
      DO K = 1, NPTFR
        IPOBO(NBOR(K)) = K
      ENDDO
!
! BUILDS NELBOR, NULONE, IKLBORD
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IELEB = 0
      DO IELEM = 1,NELEM
        DO J = 1,4
          IF(IFABOR(IELEM,J).EQ.0.OR.IFABOR(IELEM,J).EQ.-1) THEN
            IELEB           = IELEB + 1
            IF ( IELEB .GT. NELEB ) THEN
              WRITE(LU,102)
102           FORMAT(1X,'ELEBD31 : ERROR IN MESH')
              CALL PLANTE(1)
              STOP
            END IF
            NELBOR(IELEB)   = IELEM
            NULONE(IELEB,1) = SOMFAC(1,J)
            NULONE(IELEB,2) = SOMFAC(2,J)
            NULONE(IELEB,3) = SOMFAC(3,J)
            IKLBOR(IELEB,1) = IPOBO(IKLE(NELBOR(IELEB),SOMFAC(1,J)))
            IKLBOR(IELEB,2) = IPOBO(IKLE(NELBOR(IELEB),SOMFAC(2,J)))
            IKLBOR(IELEB,3) = IPOBO(IKLE(NELBOR(IELEB),SOMFAC(3,J)))
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE ELEBD31
