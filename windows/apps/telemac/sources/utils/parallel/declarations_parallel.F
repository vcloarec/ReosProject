!                   ****************************
                    MODULE DECLARATIONS_PARALLEL
!                   ****************************
!
!***********************************************************************
! PARALLEL VERSION V7P3
!***********************************************************************
!
!brief     Creating a declaration wrapper for some of the MPI keywords
!+   that are used in other declarations.
!+   The variables defined in the MPI module / include are also
!+   declared with dummy values when used in scalar mode.
!
!history  JAN RIEHME  (ADJOINTWARE)
!+
!+        V7P3
!+   Allowing for a switch from the include 'mpif.h'
!+     to the 'USE MPI' module.
!
!history  S.E. BOURBAN (HRW)
!+        17/03/2017
!+        V7P3
!+   Further simplifying the switch with USE MPI and debugging the
!+     use in scalar mode.
!
!warning  JAN RIEHME  (ADJOINTWARE )
!+   For a system-wide installed OpenMPI the parameter MPI_ADDRESS_KIND
!+     has a compiler dependent value (for instance, gfortran :8).
!+   This value might not be invalid for others, so it is no good idea
!+     to use any sort of kind parameters exported by MPI, if a
!+     compiler independent MPI installation should be used.
!+   Solution: Define your own constants ... MY_ADDRESS_KIND
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_MPI
!
#  if defined HAVE_MPI_MOD
        USE MPI
        IMPLICIT NONE
        INTEGER, PARAMETER :: MY_ADDRESS_KIND = MPI_ADDRESS_KIND
#  else
        IMPLICIT NONE
        INCLUDE 'mpif.h'
!       DEFINE 4-BYTE INTEGER KIND (PORTABLE) FOR MPI ADDRESSES
        INTEGER, PARAMETER :: MY_ADDRESS_KIND = SELECTED_INT_KIND(16)
#  endif
!
        INTEGER WAIT_REQ_PARACO(MPI_STATUS_SIZE,100)
        INTEGER :: COMM = MPI_COMM_WORLD
        CHARACTER(LEN=MPI_MAX_ERROR_STRING):: MPI_ERROR_MSG
        INTEGER MPI_ERROR_LEN
#else
        IMPLICIT NONE
        INTEGER, PARAMETER :: MY_ADDRESS_KIND = SELECTED_INT_KIND(16)
        INTEGER, PARAMETER :: MPI_INTEGER = 0
        INTEGER, PARAMETER :: MPI_DOUBLE_PRECISION = 0
        INTEGER, PARAMETER :: MPI_UB = 0
        INTEGER, PARAMETER :: MPI_COMM_WORLD = -1
        INTEGER, PARAMETER :: MPI_SUCCESS =0
        INTEGER :: COMM=0
!
!   Define AMPI_TYPE parameter for non-MPI compilation,
!     only required for oil_org_charac_type.F (there is
!       a  #ifdef HAVE_MPI missing ...)
#  ifdef COMPAD
        INTEGER, PARAMETER :: AMPI_TYPE = -333
#  endif
#endif
!
!------------------------------------------------------------------------
!
        TYPE CHARAC_TYPE
          SEQUENCE   ! BUT SEEMS USELESS (HENCE TRICK BELOW WITH VOID)
          INTEGER :: MYPID ! PARTITION OF THE TRACEBACK ORIGIN (HEAD)
          INTEGER :: NEPID ! THE NEIGHBOUR PARTITION THE TRACEBACK ENTERS TO
          INTEGER :: INE   ! THE LOCAL 2D ELEMENT NR THE TRACEBACK ENTERS IN THE NEIGBOUR PARTITION
          INTEGER :: KNE   ! THE LOCAL LEVEL THE TRACEBACK ENTERS IN THE NEIGBOUR PARTITION
          INTEGER :: IOR   ! THE POSITION OF THE TRAJECTORY -HEAD- IN MYPID [THE 2D/3D NODE OF ORIGIN]
          INTEGER :: ISP   ! CURRENT RUNGE-KUTTA STEPS PASSED AS COLLECTED
          INTEGER :: NSP   ! TOTAL RUNGE-KUTTA STEPS
          INTEGER :: IFR   ! TRICK FOR ALIGNMENT
          DOUBLE PRECISION :: XP,YP,ZP,FP             ! THE (X,Y,Z)-POSITION NOW
          DOUBLE PRECISION :: DX,DY,DZ,DF             ! THE DISPLACEMENTS
          DOUBLE PRECISION :: BASKET(10) ! VARIABLES INTERPOLATED AT THE FOOT
        END TYPE CHARAC_TYPE
!
!------------------------------------------------------------------------
!
        TYPE ALG_TYPE
          SEQUENCE   ! NECESSARY TO DEFINE MPI TYPE ALG_CHAR
          INTEGER :: MYPID ! PARTITION OF THE TRACEBACK ORIGIN (HEAD)
          INTEGER :: NEPID ! THE NEIGHBOUR PARTITION THE TRACEBACK ENTERS TO
          INTEGER :: IGLOB  ! THE GLOBAL NUMBER OF THE PARTICLES
          INTEGER :: FLAG  ! USED TO ALIGN FIELDS
          INTEGER :: ICLASS ! THE ALGAE CLASS
          INTEGER :: DISLODGE ! 0 : NOT DISLODGED. 1 : DISLODGED.
          DOUBLE PRECISION :: VX,VY,VZ  ! THE (X,Y,Z) PARTICLE VELOCITY
          DOUBLE PRECISION :: UX,UY,UZ  ! THE (X,Y,Z) FLUID VELOCITY
          DOUBLE PRECISION :: UX_AV,UY_AV,UZ_AV  ! THE (X,Y,Z) AVERAGE FLUID VELOCITY
          DOUBLE PRECISION :: K_AV,EPS_AV  ! THE VALUES OF K AND EPS
          DOUBLE PRECISION :: H_FLU  ! THE WATER DEPTH AT POSITION OF VELOCITY
          DOUBLE PRECISION :: TEFF ! EFFECTIVE TIME OF ALGAE DEGRADATION
          DOUBLE PRECISION :: PSI(3*101) ! VARIABLE PSI USED FOR THE BASSET FORCE
        END TYPE ALG_TYPE
!
!------------------------------------------------------------------------
!
        TYPE OIL_TYPE
          SEQUENCE
          INTEGER :: MYPID ! PARTITION OF THE TRACEBACK ORIGIN (HEAD)
          INTEGER :: NEPID ! THE NEIGHBOUR PARTITION THE TRACEBACK ENTERS TO
          INTEGER :: INE   ! THE LOCAL 2D ELEMENT NR THE TRACEBACK ENTERS IN THE NEIGBOUR PARTITION
          INTEGER :: KNE   ! THE LOCAL LEVEL THE TRACEBACK ENTERS IN THE NEIGBOUR PARTITION
          INTEGER :: IOR   ! THE POSITION OF THE TRAJECTORY -HEAD- IN MYPID [THE 2D/3D NODE OF ORIGIN]
          INTEGER :: STATE   ! CURRENT RUNGE-KUTTA STEPS PASSED AS COLLECTED
          INTEGER :: TPSECH   ! TOTAL RUNGE-KUTTA STEPS
          INTEGER :: IFR   ! FREQUENCY
          DOUBLE PRECISION :: SURFACE
          DOUBLE PRECISION :: MASS0
          DOUBLE PRECISION :: MASS
          DOUBLE PRECISION :: MASS_EVAP
          DOUBLE PRECISION :: MASS_DISS
          DOUBLE PRECISION :: MASS_HAP(10)
          DOUBLE PRECISION :: MASS_COMPO(10)
          DOUBLE PRECISION :: TB_HAP(10)
          DOUBLE PRECISION :: TB_COMPO(10)
          DOUBLE PRECISION :: SOL_HAP(10)
          DOUBLE PRECISION :: SOL_COMPO(10)
        END TYPE OIL_TYPE
!
!=======================================================================
!
      END MODULE
