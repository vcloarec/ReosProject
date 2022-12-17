!                   *****************
                    SUBROUTINE VOISIN
!                   *****************
!
     &(IFABOR,NELEM,NELMAX,IELM,IKLE,SIZIKL,
     & NPOIN,NACHB,NBOR,NPTFR,IADR,NVOIS)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE ARRAY IFABOR, WHERE IFABOR(IELEM, IFACE) IS
!+        THE GLOBAL NUMBER OF THE NEIGHBOUR OF SIDE IFACE OF
!+        ELEMENT IELEM (IF THIS NEIGHBOUR EXISTS) AND 0 IF THE
!+        SIDE IS ON THE DOMAIN BOUNDARY.
!
!history  OLIVIER BOITEAU (SINETICS)
!+        19/02/08
!+
!+   SIZE OF NACHB
!
!history  J-M HERVOUET (LNHE)
!+        16/06/08
!+        V5P9
!+   MODIFICATION TO THE MAX NUMBER OF NEIGHBOURS
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
!| IADR           |<->| WORK ARRAY
!| IELM           |-->| 11: TRIANGLES
!|                |   | 21: QUADRILATERALS
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID
!|                |   | BOUNDARY
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NACHB          |-->| SUB-DOMAINS NEIGHBOURS OF INTERFACE POINTS IN PARALLEL
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NVOIS          |<--| NUMBER OF NEIGHBOURS OF POINTS
!| SIZIKL         |-->| FIRST DIMENSION OF IKLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VOISIN => VOISIN
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NPTFR,SIZIKL,NELEM,NELMAX,IELM,NPOIN
      INTEGER, INTENT(IN)    :: NBOR(NPTFR),NACHB(NBMAXNSHARE,NPTIR)
      INTEGER, INTENT(IN)    :: IKLE(SIZIKL,*)
      INTEGER, INTENT(INOUT) :: IFABOR(NELMAX,*)
      INTEGER, INTENT(INOUT) :: NVOIS(NPOIN),IADR(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NFACE,NDP,KEL,IMAX,IFACE,IELEM,M1,M2,IV,IELEM2,IFACE2
      INTEGER I,J,ERR,IR1,IR2,IR3,IR4,I1,I2,IDIMAT
!
      INTEGER :: SOMFAC(2,4,2)
      PARAMETER ( SOMFAC = RESHAPE( (/
     &      1,2 , 2,3 , 3,1 , 0,0   ,
     &      1,2 , 2,3 , 3,4 , 4,1 /), SHAPE=(/ 2,4,2 /) ) )
!
!     DYNAMICALLY ALLOCATES THE WORKING ARRAYS
!
      INTEGER, DIMENSION(:), ALLOCATABLE :: MAT1,MAT2,MAT3
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.21) THEN
!       QUADRILATERALS
        NFACE = 4
!       NUMBER OF POINTS PER ELEMENT
        NDP = 4
!       ADDRESS IN SOMFAC
        KEL = 2
      ELSEIF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
!       TRIANGLES
        NFACE = 3
!       NUMBER OF POINTS PER ELEMENT
        NDP = 3
!       ADDRESS IN SOMFAC
        KEL = 1
      ELSE
        WRITE(LU,99) IELM
99      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE OF ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     IDIMAT IS BIGGER THAN THE SUM OF THE NUMBER OF NEIGHBOURS OF
!     ALL THE POINTS (NEIGHBOUR = CONNECTED BY A SEGMENT)
!
      IDIMAT = NDP*2*NELEM
!
      ALLOCATE(MAT1(IDIMAT),STAT=ERR)
      ALLOCATE(MAT2(IDIMAT),STAT=ERR)
      ALLOCATE(MAT3(IDIMAT),STAT=ERR)
!
      IF(ERR.NE.0) THEN
        WRITE(LU,2000) ERR
2000    FORMAT(1X,'VOISIN: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &            'ERROR CODE: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  ARRAY NVOIS FOR EACH POINT
!  BEWARE : NVOIS IS BIGGER THAN THE ACTUAL NUMBER OF NEIGHBOURS
!           THE SUM OF NVOIS WILL GIVE IDIMAT
!
      DO I=1,NPOIN
        NVOIS(I) = 0
      ENDDO
!
      DO IFACE = 1,NFACE
        DO IELEM=1,NELEM
          I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
          I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
          NVOIS(I1) = NVOIS(I1) + 1
          NVOIS(I2) = NVOIS(I2) + 1
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!  ADDRESSES OF EACH POINT IN A STRUCTURE OF TYPE COMPACT MATRIX
!
!
      IADR(1) = 1
      DO I= 2,NPOIN
        IADR(I) = IADR(I-1) + NVOIS(I-1)
      ENDDO ! I
!
      IMAX = IADR(NPOIN) + NVOIS(NPOIN) - 1
      IF(IMAX.GT.IDIMAT) THEN
        WRITE(LU,52) IDIMAT,IMAX
52      FORMAT(1X,'VOISIN: SIZE OF MAT1,2,3 (',1I9,') TOO SHORT',/,
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
      IFABOR(IELEM,IFACE) = -1
!
!       GLOBAL NODE NUMBERS FOR THE SIDE:
!
        I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
        I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
!
!       ORDERED GLOBAL NUMBERS:
!
        M1 = MIN0(I1,I2)
        M2 = MAX0(I1,I2)
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
83      FORMAT(1X,'VOISIN : ERROR IN THE MESH             ',/,1X,
     &            '         MAYBE SUPERIMPOSED POINTS     ')
        CALL PLANTE(1)
        STOP
!
81      CONTINUE
!
      ENDDO ! IELEM
      ENDDO ! IFACE
!
!  COULD TRY SOMETHING A BIT LIGHTER
!  USING INDPU FOR EXAMPLE
!
      IF(NCSIZE.GT.1) THEN
!
      DO IFACE=1,NFACE
      DO IELEM=1,NELEM
!
!  SOME BOUNDARY SIDES ARE INTERFACES BETWEEN SUB-DOMAINS IN
!  ACTUAL FACT: THEY ARE ASSIGNED A VALUE -2 INSTEAD OF -1
!
      IF(IFABOR(IELEM,IFACE).EQ.-1) THEN
!
        I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
        I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
!
        IR1=0
        IR2=0
!
        IF(NPTIR.GT.0) THEN
          DO J=1,NPTIR
            IF(I1.EQ.NACHB(1,J)) IR1=1
            IF(I2.EQ.NACHB(1,J)) IR2=1
          ENDDO ! J
        ENDIF
!
        IF(IR1.EQ.1.AND.IR2.EQ.1) THEN
!         INTERFACE SEGMENT DETECTED, CHECKS WHETHER IT IS NOT
!         ALSO A TRUE BOUNDARY SIDE
          IR3=0
          IR4=0
          DO J=1,NPTFR
            IF(I1.EQ.NBOR(J)) IR3=1
            IF(I2.EQ.NBOR(J)) IR4=1
          ENDDO ! J
!         PRIORITY TO THE TRUE BOUNDARY SIDES
          IF(IR3.EQ.0.OR.IR4.EQ.0) THEN
            IFABOR(IELEM,IFACE)=-2
          ENDIF
        ENDIF
!
      ENDIF
!
      ENDDO ! IELEM
      ENDDO ! IFACE
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(MAT1)
      DEALLOCATE(MAT2)
      DEALLOCATE(MAT3)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
