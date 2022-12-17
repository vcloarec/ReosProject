!                   *****************
                    SUBROUTINE SEGBOR
!                   *****************
!
     &(NSEGBOR,IKLES,NELEM,NPOIN)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    DETERMINES THE NUMBER OF BOUNDARY SEGMENTS OF THE MESH
!+               (INCLUDES INTERNAL BOUNDARIES IN PARALLEL MODE).
!+
!+            BASED UPON THE PRINCIPLE OF VOISIN,
!+                WHICH WILL BE CALLED LATER.
!
!history  J-M HERVOUET (LNHE)
!+        19/06/2008
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
!+        07/02/2013
!+        V6P3
!+   Removing argument NELMAX.
!
!history  S.E.BOURBAN (HRW)
!+        21/03/2017
!+        V7P3
!+   Replacement of the DATA declarations by the PARAMETER associates
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLES          |-->| LIKE CONNECTIVITY TABLE BUT IN SELAFIN FORMAT
!|                |   | IKLES(3,NELEM) INSTEAD OF IKLE(NELEM,3)
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NSEGBOR        |<--| NUMBER OF BOUNDARY SEGMENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SEGBOR => SEGBOR
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NPOIN,NELEM
      INTEGER, INTENT(OUT)           :: NSEGBOR
      INTEGER, INTENT(IN)            :: IKLES(3,NELEM)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NFACE,NDP,KEL,IMAX,IFACE,IELEM,M1,M2,IV,IELEM2,IFACE2
      INTEGER I,ERR,I1,I2,IDIMAT
      INTEGER :: SOMFAC(2,4,2)
      PARAMETER ( SOMFAC = RESHAPE( (/
     &            1,2 , 2,3 , 3,1 , 0,0   ,
     &            1,2 , 2,3 , 3,4 , 4,1 /), SHAPE=(/ 2,4,2 /) ) )
!
!     DYNAMICALLY ALLOCATES WORKING ARRAYS
!
      INTEGER, ALLOCATABLE :: IFABOR(:,:),MAT1(:),MAT2(:),MAT3(:)
      INTEGER, ALLOCATABLE :: NVOIS(:),IADR(:)
!
!-----------------------------------------------------------------------
!
      NFACE = 3
!     NUMBER OF POINTS PER ELEMENT
      NDP = 3
!     ADDRESS IN SOMFAC
      KEL = 1
!
!     IDIMAT IS BIGGER THAN THE SUM OF THE NUMBER OF NEIGHBOURS FOR
!     ALL THE POINTS (NEIGHBOUR = LINKED BY A SEGMENT)
!
      IDIMAT = NDP*2*NELEM
!
      ALLOCATE(MAT1(IDIMAT),STAT=ERR)
      ALLOCATE(MAT2(IDIMAT),STAT=ERR)
      ALLOCATE(MAT3(IDIMAT),STAT=ERR)
      ALLOCATE(IFABOR(NELEM,3),STAT=ERR)
      ALLOCATE(NVOIS(NPOIN),STAT=ERR)
      ALLOCATE(IADR(NPOIN),STAT=ERR)
!
      IF(ERR.NE.0) THEN
        WRITE(LU,2000) ERR
2000    FORMAT(1X,'SEGBOR: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &            'ERROR CODE: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  COMPUTES THE ARRAY NVOIS FOR EACH POINT
!  BEWARE : NVOIS IS BIGGER THAN THE NUMBER OF NEIGHBOURS
!           THE SUM OF NVOIS FOR ALL THE POINTS WILL GIVE IDIMAT
!
      DO I=1,NPOIN
        NVOIS(I) = 0
      ENDDO
!
      DO IFACE = 1,NFACE
        DO IELEM=1,NELEM
          I1 = IKLES( SOMFAC(1,IFACE,KEL) , IELEM )
          I2 = IKLES( SOMFAC(2,IFACE,KEL) , IELEM )
          NVOIS(I1) = NVOIS(I1) + 1
          NVOIS(I2) = NVOIS(I2) + 1
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!  COMPUTES THE ADDRESSES OF EACH POINT IN A STRUCTURE OF TYPE
!  COMPACT MATRIX
!
      IADR(1) = 1
      DO I= 2,NPOIN
        IADR(I) = IADR(I-1) + NVOIS(I-1)
      ENDDO ! I
!
      IMAX = IADR(NPOIN) + NVOIS(NPOIN) - 1
      IF(IMAX.GT.IDIMAT) THEN
        WRITE(LU,52) IDIMAT,IMAX
52      FORMAT(1X,'SEGBOR: SIZE OF MAT1,2,3 (',1I9,') TOO SHORT',/,
     &         1X,'MINIMUM SIZE: ',1I9)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  INITIALISES THE COMPACT MATRIX TO 0
!
      DO I=1,IMAX
        MAT1(I) = 0
      ENDDO
!
!-----------------------------------------------------------------------
!
!  LOOP ON THE SIDES OF EACH ELEMENT:
!
      DO IFACE = 1 , NFACE
      DO IELEM = 1 , NELEM
!
        IFABOR(IELEM,IFACE) = 0
!
!       GLOBAL NUMBERS OF THE POINTS OF THE SIDE:
!
        I1 = IKLES( SOMFAC(1,IFACE,KEL) , IELEM )
        I2 = IKLES( SOMFAC(2,IFACE,KEL) , IELEM )
!
!       ORDERED GLOBAL NUMBERS :
!
        M1 = MIN(I1,I2)
        M2 = MAX(I1,I2)
!
        DO IV = 1,NVOIS(M1)
!
          IF(MAT1(IADR(M1)+IV-1).EQ.0) THEN
            MAT1(IADR(M1)+IV-1)=M2
            MAT2(IADR(M1)+IV-1)=IELEM
            MAT3(IADR(M1)+IV-1)=IFACE
            GO TO 81
          ELSEIF(MAT1(IADR(M1)+IV-1).EQ.M2) THEN
            IELEM2 = MAT2(IADR(M1)+IV-1)
            IFACE2 = MAT3(IADR(M1)+IV-1)
            IFABOR(IELEM,IFACE) = IELEM2
            IFABOR(IELEM2,IFACE2) = IELEM
            GO TO 81
          ENDIF
!
        ENDDO ! IV
!
        WRITE(LU,83)
83      FORMAT(1X,'SEGBOR : ERROR IN THE MESH             ',/,1X,
     &            '         MAYBE SUPERIMPOSED POINTS     ')
        CALL PLANTE(1)
        STOP
!
81      CONTINUE
!
      ENDDO ! IELEM
      ENDDO ! IFACE
!
      NSEGBOR = 0
      DO IFACE=1,NFACE
        DO IELEM=1,NELEM
          IF(IFABOR(IELEM,IFACE).EQ.0) NSEGBOR=NSEGBOR+1
        ENDDO
      ENDDO
!
      WRITE(LU,501) NSEGBOR
501   FORMAT(1X,'SEGBOR (BIEF) : NUMBER OF BOUNDARY SEGMENTS = ',1I6,/,
     &       1X,'INCLUDING THOSE DUE TO DOMAIN DECOMPOSITION')
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(MAT1)
      DEALLOCATE(MAT2)
      DEALLOCATE(MAT3)
      DEALLOCATE(IFABOR)
      DEALLOCATE(NVOIS)
      DEALLOCATE(IADR)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
