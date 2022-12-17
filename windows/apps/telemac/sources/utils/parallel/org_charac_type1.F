!                   ***************************
                    SUBROUTINE ORG_CHARAC_TYPE1
!                   ***************************
!
     &(NOMB,CHARACTERISTIC)
!
!***********************************************************************
! PARALLEL   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    MPI TYPE FOR TYPE CHARAC_TYPE - CHARACTERISTICS /
!
!history  C. DENIS
!+        01/07/2011
!+        V6P1
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
!history  J-M HERVOUET
!+        22/06/2012
!+        V6P2
!+   DX,DY and DZ added. Problem of integer 8 treated as a double
!+   solved. Much clearer now.
!+
!history  J-M HERVOUET
!+        04/10/2012
!+        V6P3
!+   NOMB = 0 now allowed
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NOMB           |<---| NUMBER OF VARIABLES
!| CHARACTERISTIC |--->| DATATYPE FOR CHARACTERISTIC
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MPI_TYPE_GET_EXTENT,
     &     P_MPI_TYPE_CREATE_STRUCT, P_MPI_TYPE_COMMIT
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NOMB
      INTEGER, INTENT(INOUT) :: CHARACTERISTIC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_MPI
      INTEGER, PARAMETER :: MAX_BASKET_SIZE=10
!
!     ARRAY OF DISPLACEMENTS BETWEEN BASIC COMPONENTS, HERE INITIALISED ONLY
!
!      INTEGER (KIND=MPI_ADDRESS_KIND), DIMENSION(18) :: CH_DELTA
      INTEGER (KIND=MY_ADDRESS_KIND), DIMENSION(18) :: CH_DELTA
!
!     ARRAY OF BLOCKLENGTHS OF TYPE COMPONENTS, BASKET INITIALISED TO 1
!
      INTEGER, DIMENSION(18) :: CH_BLENGTH
!     ARRAY OF COMPONENT TYPES IN TERMS OF THE MPI COMMUNICATION
      INTEGER, DIMENSION(18) :: CH_TYPES
      INTEGER IER
!      INTEGER (KIND=MPI_ADDRESS_KIND) :: EXTENT,ILB,IUB,INTEX
      INTEGER (KIND=MY_ADDRESS_KIND) :: EXTENT,ILB,IUB,INTEX
!
      INTEGER I
!
      CH_BLENGTH=(/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/)
      CH_DELTA=  (/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
!
!     TERMINATE ON MPI PARTS NOT HANDLED BY COMPAD
#  if defined COMPAD
      WRITE(LU,*) 'ORG_CHARAC_TYPE1: COMPAD MAYBE CRITICAL',
     &     'MPI OPERATION'
      WRITE(LU,*) '  PLEASE CONTACT JR @ ADJOINTWARE'
      CALL PLANTE(1)
      STOP
#  endif
!     INTEGERS IN THE STRUCTURE
!
      CALL P_MPI_TYPE_GET_EXTENT(MPI_INTEGER,ILB,INTEX,IER)
      CH_DELTA(1)=0
!     9 IS THE FIRST DOUBLE PRECISION THAT COMES AFTER AN INTEGER
      DO I=2,9
        CH_DELTA(I)=CH_DELTA(I-1)+INTEX
      ENDDO
!
!     DOUBLE PRECISION IN THE STRUCTURE
!
!     Handle components of active data type correctly
#  if defined COMPAD
      CALL P_MPI_TYPE_GET_EXTENT(AMPI_TYPE,ILB,INTEX,IER)
#  else
      CALL P_MPI_TYPE_GET_EXTENT(MPI_DOUBLE_PRECISION,ILB,INTEX,IER)
#  endif
!     THE SIX REMAINING DOUBLE PRECISION (INCLUDING BASKET)
      DO I=10,17
        CH_DELTA(I)=CH_DELTA(I-1)+INTEX
      ENDDO
!     ADDRESS AFTER THE BASKET
      CH_DELTA(18)=CH_DELTA(17)+INTEX*MAX_BASKET_SIZE
!
      IF(NOMB.GE.0.AND.NOMB.LE.MAX_BASKET_SIZE) THEN
        CH_BLENGTH(17) = NOMB  ! CH%BASKET RANGE APPLIED FOR COMMUNICATION
      ELSE
        WRITE(LU,*) ' PARALLEL::ORG_CHARAC_TYPE1::',
     &        ' NOMB NOT IN RANGE [0..MAX_BASKET_SIZE]'
        WRITE(LU,*) ' MAX_BASKET_SIZE, NOMB: ',MAX_BASKET_SIZE,NOMB
        CALL PLANTE(1)
        STOP
      ENDIF
      CH_TYPES(1)=MPI_INTEGER
      CH_TYPES(2)=MPI_INTEGER
      CH_TYPES(3)=MPI_INTEGER
      CH_TYPES(4)=MPI_INTEGER
      CH_TYPES(5)=MPI_INTEGER
      CH_TYPES(6)=MPI_INTEGER
      CH_TYPES(7)=MPI_INTEGER
      CH_TYPES(8)=MPI_INTEGER
!     Handle components of active data type correctly
#  if defined COMPAD
      CH_TYPES(9)=AMPI_TYPE
      CH_TYPES(10)=AMPI_TYPE
      CH_TYPES(11)=AMPI_TYPE
      CH_TYPES(12)=AMPI_TYPE
      CH_TYPES(13)=AMPI_TYPE
      CH_TYPES(14)=AMPI_TYPE
      CH_TYPES(15)=AMPI_TYPE
      CH_TYPES(16)=AMPI_TYPE
      CH_TYPES(17)=AMPI_TYPE
#  else
      CH_TYPES(9)=MPI_DOUBLE_PRECISION
      CH_TYPES(10)=MPI_DOUBLE_PRECISION
      CH_TYPES(11)=MPI_DOUBLE_PRECISION
      CH_TYPES(12)=MPI_DOUBLE_PRECISION
      CH_TYPES(13)=MPI_DOUBLE_PRECISION
      CH_TYPES(14)=MPI_DOUBLE_PRECISION
      CH_TYPES(15)=MPI_DOUBLE_PRECISION
      CH_TYPES(16)=MPI_DOUBLE_PRECISION
      CH_TYPES(17)=MPI_DOUBLE_PRECISION
#  endif
      CH_TYPES(18)=MPI_UB       ! THE TYPE UPPER BOUND MARKER
      CALL P_MPI_TYPE_CREATE_STRUCT(18,CH_BLENGTH,CH_DELTA,CH_TYPES,
     &                              CHARACTERISTIC,IER)
      CALL P_MPI_TYPE_COMMIT(CHARACTERISTIC,IER)
      CALL P_MPI_TYPE_GET_EXTENT(CHARACTERISTIC,ILB,EXTENT,IER)
      IUB=ILB+EXTENT
!
      IF(ILB.NE.CH_DELTA(1).OR.IUB.NE.CH_DELTA(18)) THEN
        WRITE(LU,*) ' PARALLEL::ORG_CHARAC_TYPE1:'
        WRITE(LU,*) ' MEMORY PROBLEM WITH THIS COMPILER: '
        WRITE(LU,*) ' ILB=',ILB,' NOT EQUAL TO CH_DELTA(1)=',
     &        CH_DELTA(1)
        WRITE(LU,*) ' OR'
        WRITE(LU,*) ' IUB=',IUB,' NOT EQUAL TO CH_DELTA(18)=',
     &        CH_DELTA(18)
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      WRITE(LU,*) 'CALL OF ORG_CHARAC_TYPE1, VOID VERSION'
#endif
!
!----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE ORG_CHARAC_TYPE1
