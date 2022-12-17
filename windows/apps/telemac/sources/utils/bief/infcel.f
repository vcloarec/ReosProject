!                   *****************
                    SUBROUTINE INFCEL
!                   *****************
!
     &(XX,YY,NUBO,VNOIN,NPOIN,NELEM,NSEG,CMI,AIRST,
     & GLOSEG,COORD_G,ELTSEG,ORISEG,IFABOR)
!
!***********************************************************************
! BIEF   V6P3                                   21/12/20112
!***********************************************************************
!
!brief     REPLACE  OLD INFCEL: NOW COMMON NUBO (GLOSEG) WITH FE
!          COMPUTES  CMI, AIRST AND NORMALS (VNOIN)
!
!history
!+        18/06/03
!+        V5P4
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
!history R. ATA (EDF R&D - LNHE)
!+        21/01/2013
!+        V6P3
!+   rewritten for new data structure of finite volumes
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IFAPAR         |-->| IFAPAR(1:3,IELEM)=PROCESSOR NUMBERS BEHIND THE
!|                |   | 3 ELEMENT EDGES  (NUMBERS FROM 0 TO NCSIZE-1)
!|                |   | IFAPAR(4:6,IELEM): -LOCAL- ELEMENT NUMBERS
!|                |   | BEHIND THE 3 EDGES
!| AIRST          |<--| AREAS OF SUB-TRIANGLES IG1G2
!| CMI            |<--| COORDINATES OF MID-INTERFACE POINTS
!| INDPU          |-->| INDEX TABLE : IF 0: NOT AN INTERFACE POINT
!|                |   |               IF NOT 0: ADDRESS IN THE LIST
!|                |   |               OF BOUNDARY POINTS.
!| IFABOR         |-->| IFABOR(IEL,I) IS THE ELEMENT BEHIND THE EDGE I OF
!|                |---|  ELEMENT IEL, OTHERWISE
!|                |---|     IFABOR(IEL,I) = -2 : THIS IS INTERFACE EDGE
!|                |---|     IFABOR(IEL,I) = 0  : THIS IS BOUNDARY EDGE
!|                |---|     IFABOR(IEL,I) = -1 : THIS IS LIQUID BOUNDARY EDGE
!| KNOLG          |-->| CONVERSION FROM LOCAL TO GLOBAL NUMEBEING
!| KP1BOR         |<--| NUMBER OF FOLLOWING AND PRECEDING NODE OF BOUNDARY POINT K.
!| MESH           |-- | MESH STRUCTURE
!| NB_NEIGHB_SEG  |-->| NUMBER OF NEIGHBOURING SUB-DOMAINS (FOR EDGES)
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| NUBO           |<--| FIRST AND SECOND POINT OF SEGMENTS (GLOSEG ?)
!| VNOIN          |<--| NORMAL TO THE INTERFACE
!|                |   | (2 FIRST COMPONENTS) AND
!|                |   | SEGMENT LENGTH (3RD COMPONENT)
!| XX             |-->| ABSCISSAE OF POINTS IN THE MESH
!| YY             |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      IMPLICIT  NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG,NPOIN,NELEM
!     TO REPLACE IN NEXT RELEASE
      INTEGER, INTENT(INOUT)          :: NUBO(2,NSEG)
      INTEGER, INTENT(IN)             :: GLOSEG(NSEG,2)
      INTEGER, INTENT(IN)             :: ELTSEG(NELEM,3)
      INTEGER, INTENT(IN)             :: ORISEG(NELEM,3)
      DOUBLE PRECISION, INTENT(IN)    :: XX(NPOIN),YY(NPOIN),CMI(2,NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: VNOIN(3,NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: AIRST(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: COORD_G(NSEG,4)
      INTEGER, INTENT(IN)             :: IFABOR(NELEM,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NB1,NB2,ISEG,IEL,FACT,I,IER
      DOUBLE PRECISION,PARAMETER :: EPS= 1.D-14
      DOUBLE PRECISION X1,Y1,X2,Y2,RNORM,XGG,YGG,XG1,YG1,XG2,YG2
!
      INTRINSIC ABS,SQRT
!
      LOGICAL, ALLOCATABLE :: YESNO(:)
      ALLOCATE(YESNO(NSEG),STAT=IER)
      CALL CHECK_ALLOCATE(IER, 'INFCEL')
!
!-----------------------------------------------------------------------
!
!-----------------
! 1. INITIALISES
!-----------------
!
      DO ISEG = 1 , NSEG
        VNOIN(1,ISEG) = 0.D0
        VNOIN(2,ISEG) = 0.D0
        VNOIN(3,ISEG) = 0.D0
        YESNO(ISEG)   =.FALSE.
      ENDDO
!
!-----------------
! 2. MAIN LOOP
!-----------------
!
      DO IEL=1, NELEM
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
            FACT=1
            ISEG = ELTSEG(IEL,I)
!           LET S RECUPERATE NODE NUMBERS
            NB1 = GLOSEG(ISEG,1)
            NB2 = GLOSEG(ISEG,2)
!           TO REMOVE REDUNDANT
            NUBO(1,ISEG) = NB1
            NUBO(2,ISEG) = NB2
!           THEIR COORDINATES
            X1 = XX(NB1)
            Y1 = YY(NB1)
            X2 = XX(NB2)
            Y2 = YY(NB2)
!           CENTER OF GRAVITY OF NEIGHBORING ELEMENTS
            XG1 = COORD_G(ISEG,1)
            YG1 = COORD_G(ISEG,2)
            XG2 = COORD_G(ISEG,3)
            YG2 = COORD_G(ISEG,4)
            IF(IFABOR(IEL,I).EQ.-1.OR.IFABOR(IEL,I).EQ.0)THEN  ! BOUNDARY SEGMENT
              IF(ABS(XG1).LT.EPS.AND.ABS(YG1).LT.EPS)THEN   !BOUNDARY SEGMENT (TO IMPROVE THE TEST!!!)
                XG1=CMI(1,ISEG)                   ! THIS CASE COULD REALLY HAPPEN ?!
                YG1=CMI(2,ISEG)
              ELSEIF(ABS(XG2).LT.EPS.AND.ABS(YG2).LT.EPS)THEN
                XG2=CMI(1,ISEG)
                YG2=CMI(2,ISEG)
              ENDIF
            ENDIF
!           SURFACE OF TRIANGLES (NB1,G1,G2) AND (NB2,G1,G2)
            AIRST(1,ISEG)=0.5D0*ABS((X1-XG1)*(Y1-YG2)-
     &                              (Y1-YG1)*(X1-XG2))
            AIRST(2,ISEG)=0.5D0*ABS((X2-XG1)*(Y2-YG2)-
     &                              (Y2-YG1)*(X2-XG2))
!           NORMAL VECTORS TO INTERFACES AND THEIR LENGHT
            XGG = XG1-XG2
            YGG = YG1-YG2
            RNORM=SQRT(XGG**2+YGG**2)
            IF(RNORM.GT.EPS) THEN
              IF(ORISEG(IEL,I).EQ.2) FACT=-FACT
              VNOIN(1,ISEG) =  FACT*YGG/RNORM
              VNOIN(2,ISEG) = -FACT*XGG/RNORM
              VNOIN(3,ISEG) = RNORM
            ELSE
              WRITE(LU,*)'**************************************'
              WRITE(LU,*)'INFCEL: INTERFACE LENGTH NIL',RNORM
              WRITE(LU,*)'FOR SEGMENT',ISEG
              WRITE(LU,*)'WITH GLOBAL NODES',NB1,NB2
              WRITE(LU,*)'**************************************'
              CALL PLANTE(1)
              STOP
            ENDIF
            YESNO(ISEG)=.TRUE.
          ENDIF
        ENDDO
      ENDDO
!
!---------------------------------------------------------------------
!
      RETURN
      END
