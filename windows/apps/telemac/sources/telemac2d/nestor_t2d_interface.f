!                       ******************************
                        SUBROUTINE NESTOR_T2D_INTERFACE
!                       ******************************
!
     &(OPTION)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    COUPLING WITH NESTOR
!
!+  CALL THE NESTOR INTEFACE SUBROUTINE
!
!history  B. GLANDER (BAW)
!+        28/11/2017
!+        V7P2
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|             |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY :  NPOIN, IELMH
     &                                  , T2D_FILES,T2NACT,T2NPOL
     &                                  , T2NREF,T2NRST
     &                                  , ZF, HN, T13, T14
     &                                  , MESH
     &                                  , LT, DT, AT, LEOPRD
     &                                  , MARDAT, MARTIM
     &                                  , NES_DZ, AVAIL
     &                                  , ZRL, VOLU2D

      USE DECLARATIONS_SPECIAL
!
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: OPTION
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ::  NSICLA         ! Number of grain SIze CLAsses (dummy)
      INTEGER ::  NOMBLAY        ! Number of LAYers (dummy)

      DOUBLE PRECISION :: MOFAC  ! MOrphological FACtor (dummy)
      LOGICAL          :: CALLEDBY_T2D
!-----------------------------------------------------------------------
!
      NSICLA       = 1        ! number of classes    in context with t2d always 1
      NOMBLAY      = 1        ! number of layers     in context with t2d always 1
      MOFAC        = 1.0D0    ! morphological factor in context with t2d always 1.0
! 
      CALLEDBY_T2D = .TRUE.
!
      IF(OPTION.EQ.1) THEN
!
!       NO QUASI BUBBLE SUPPORTED
        IF(IELMH.NE.11) THEN
          WRITE(LU,*)'ONLY LINEAR 2D ELEMENTS ARE SUPPORTED'
          WRITE(LU,*)'NESTOR -STOP, PLEASE START WITHOUT'
          WRITE(LU,*)'NESTOR OR USE ANOTHER ELEMENT TYPE'
          WRITE(LU,*)'ACTUAL ELEMENT TYPE OF ZF: ',IELMH
          CALL PLANTE(1)
          STOP
        ENDIF
!
        WRITE(LU,*)'?> -------- initialisation of Nestor ----------'
!
        !  generate the bief objekt NES_DZ which contains the
        !  bottom modification (per time step) caused by NESTOR
        CALL ALLBLO(NES_DZ,'NES_DZ')
        CALL BIEF_ALLVEC_IN_BLOCK
     &           (NES_DZ,NSICLA,1,'NES_DZ',IELMH,1,2,MESH)
        CALL OS('X=0     ', X=NES_DZ )
!
        ALLOCATE(AVAIL(NPOIN,NOMBLAY,NSICLA)) ! here a dummy,  in Sisyhe: fraction of each class for each layer (npoin,nomblay,nsicla)
!
!       ======= INITIALISATION =============
!
        AVAIL(:,:,:) = 1.0D0
!
!          
        CALL INTERFACEINITNESTOR(    NCSIZE, IPID, NPOIN
     &                             , NSICLA                   ! Number of SIze CLAsses (dummy)
     &                             , MARDAT, MARTIM           ! start time: DATe , TIMe
     &                             , MOFAC                    ! MOrphological FACtor (dummy)
     &                             , LEOPRD                   ! period of graphical outputs
     &                             , MESH%X%R, MESH%Y%R
     &                             , VOLU2D%R                 ! node area [m**2]
     &                             , MAXVAL(MESH%KNOLG%I(:))  ! max index
     &                             , ZF%R                     ! bottom at time 0 [m+NN]
     &                             , LU                       ! logical unit for standard output
     &                             , T2D_FILES(T2NACT)%LU     ! logical unit to NESTOR ACTION FILE
     &                             , T2D_FILES(T2NPOL)%LU     ! logical unit to NESTOR POLYGON FILE
     &                             , T2D_FILES(T2NREF)%LU     ! logical unit to NESTOR SURFACE REFERENCE FILE
     &                             , T2D_FILES(T2NRST)%LU     ! logical unit to NESTOR RESTART FILE
     &                             , CALLEDBY_T2D
     &                             , ZRL%R                    ! reference level [m+NN]
     &                            )
!
!
      ELSEIF(OPTION.EQ.2) THEN
        !#########################################
        !
        ! Run Nestor   CALL FROM WITHIN TELEMAC2D
        !
        !#########################################
!
        T14%R(:) = 0.1D0  !> [m] This value delimits the vertical movement of the bottom per time step
                          !  In case of exceedance Nestor will stop the computation.
!                            
        T13%R(:) = ZF%R(:) - 1000.0D0   !> [m] dummy: Nestor will terminate the computation
                                        !  in case of dredging below this level
!
        !> When Nestor is coupled with Sisyphe:  
        !   the value of T14 represents the active layer thickness ES1 at each node.
        !   the value of T13 represents the ridgid bed ZR at each node.
!
!
        CALL INTERFACERUNNESTOR(   NPOIN           !  Number of POINts (NODES)
     &                           , NOMBLAY         !  Number of LAYers
     &                           , NSICLA          !  Number of SIze CLAsses
     &                           , LT              !  Telemac time step
     &                           , DT              !  Duration of Telemac time step [s]
     &                           , AT              !  time
     &                           , T14%R           !  limit of the vertical movement of the bottom per time step [m]
     &                           , ZF%R            !  bottom [m+NN]
     &                           , NES_DZ          !  bottom cange per time step by nestor [m]
     &                           , AVAIL           !  grain composition (part of grainClass per node per layer)
     &                           , MESH%KNOLG%I    !  index list: Local to Global node index
     &                           , HN%R            !  water depth [m]
     &                           , T13%R           !  dummy ridged bed [m+NN]
     &                           , ZRL%R           ! reference level [m+NN]
     &                          )
!
        CALL OS('X=X+Y   ', X=ZF, Y=NES_DZ%ADR(1)%P)   ! well  it happens here
!
        CALL OS('X=0     ', X=NES_DZ )
!
!
      ELSE
!
!       ------ ERROR: the subroutine is called with a bad value for the parameter "OPTION"
        WRITE(LU,*) 'BAD OPTION FOR NESTOR'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
