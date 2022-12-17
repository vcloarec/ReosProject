!                   ***************
                    SUBROUTINE HLOC
!                   ***************
!
     &(NPOIN,NSEG,NELEM,NUBO,VNOCL,AIRS,DTHAUT,MESH,ELTSEG,IFABOR)
!
!***********************************************************************
! BIEF   V6P3                                   25/05/2013
!***********************************************************************
!
!brief    COMPUTES THE LOCAL SPACE STEP [ |CI|/SUM(LIJ) ].
!
!
!history  INRIA
!+
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
!+
! history R. ATA
!+         25/02/2012
!+         V6P3
!+   remove nubo and change by gloseg
!+   parallelization
!+
!history R. ATA
!+         25/05/2013
!+         V6P3
!+   clean unused variables and loop changed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AIRS           |-->| AREAS OF CELLS IN THE MESH.
!| DTHAUT         |<--| SPACE STEP
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| NUBO           |-->| FIRST AND SECOND POINT OF SEGMENTS
!| VNOCL          |-->| NORMAL VECTOR TO INTERFACE
!|                |   | (2 FIRST COMPONENTS) AND
!|                |   | SEGMENT LENGTH (3RD COMPONENT)
!| XNEBOR         |-->| X-COMPONENT OF NORMAL VECTOR AT BOUNDARY POINT
!| YNEBOR         |-->| Y-COMPONENT OF NORMAL VECTOR AT BOUNDARY POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE BIEF, EX_HLOC => HLOC
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NSEG,NPOIN
      INTEGER, INTENT(IN)            :: NUBO(2,*)
      INTEGER, INTENT(IN)            :: NELEM
      INTEGER, INTENT(IN)            :: ELTSEG(NELEM,3)
      DOUBLE PRECISION, INTENT(IN)   :: VNOCL(3,*)
      DOUBLE PRECISION, INTENT(IN)   :: AIRS(NPOIN)
      DOUBLE PRECISION, INTENT(OUT)  :: DTHAUT(NPOIN)
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      INTEGER, INTENT(IN)            :: IFABOR(NELEM,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,NSG,NUBO1,NUBO2,IELEM,IER
      LOGICAL, ALLOCATABLE :: YESNO(:)
      ALLOCATE(YESNO(NSEG),STAT=IER)
      CALL CHECK_ALLOCATE(IER, 'HLOC')
!
!-----------------------------------------------------------------------
!
!     INITIALISES
!
      DO I=1,NPOIN
        DTHAUT(I) = 0.D0
      ENDDO
!     INITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO
!
      DO IELEM=1, NELEM
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IELEM,I)))THEN
            NSG = ELTSEG(IELEM,I)
            NUBO1 = NUBO(1,NSG)
            NUBO2 = NUBO(2,NSG)
!           NUBO OR GLOSEG ARE THE SAME .. TO REMOVE NUBO (REDUNDANCE)
            IF(NCSIZE.GT.1.AND.IFABOR(IELEM,I).EQ.-2)THEN !THIS IS AN INTERFACE EDGE
              DTHAUT(NUBO1)=DTHAUT(NUBO1) + 0.5D0*VNOCL(3,NSG)
              DTHAUT(NUBO2)=DTHAUT(NUBO2) + 0.5D0*VNOCL(3,NSG)
            ELSE
              DTHAUT(NUBO1)=DTHAUT(NUBO1) + VNOCL(3,NSG)
              DTHAUT(NUBO2)=DTHAUT(NUBO2) + VNOCL(3,NSG)
            ENDIF
            YESNO(NSG)=.TRUE.
          ENDIF
        ENDDO
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM2(DTHAUT,DTHAUT,DTHAUT,NPOIN,1,2,1,MESH)
      ENDIF
!
      DO I=1,NPOIN
        DTHAUT(I) = AIRS(I)/ DTHAUT(I)
      ENDDO
!
      DEALLOCATE(YESNO)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
