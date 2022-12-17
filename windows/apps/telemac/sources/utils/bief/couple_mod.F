      MODULE COUPLE_MOD
!******************************************************************
! THAT MODULE CONTAINS THE SUBROUTINES NEEDED TO EXCHANGE VALUES OF VARIABLE
! BETWEEN TWO MODULES (TILL NOW TELEMAC2D AND TOMAWAC) WHEN THEY DEAL
! WITH DIFFERENT MESH.
! ASSUMPTIONS:
! SAME NUMBER OF PROCESSES ON BOTH COUPLED MODELS
!******************************************************************

      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_PARALLEL
      USE INTERFACE_PARALLEL

      TYPE COUPLE_DATA_TYPE
        ! RECEIVER DATA

        ! SENDER DATA
        INTEGER, POINTER, DIMENSION(:,:) :: SEND_LIST
        INTEGER, POINTER, DIMENSION(:)   :: NPOIN_ALL

        ! LIST WITH FOR EACH PROCESS
        ! LOCAL NODE ID WHERE TO GET DATA FROM FOR SENDING
        INTEGER, POINTER, DIMENSION(:,:)   :: SEND_MAP

        ! LIST WITH FOR EACH PROCESS
        ! LOCAL NODE ID WHERE TO PUT DATA THAT IS RECEIVED
        ! THIRD DIMENSION IS USED TO HAVE INFLUENCE OF MULTIPLE POINTS.
        ! E.G. FOR INTERPOLATION
        INTEGER, POINTER, DIMENSION(:,:,:)   :: RECV_MAP

        !NUMBER OF POINTS TO SEND TO OTHER PROCESSES
        INTEGER, POINTER, DIMENSION(:)   :: POIN2SEND
        !NUMBER OF POINTS TO SRECEIVE FROM OTHER PROCESSES
        INTEGER, POINTER, DIMENSION(:)   :: POIN2RECV

        ! MAXIMUM NUMBER OF POINTS ON ANY PROCESS
        INTEGER:: MAXPOIN
        ! NUMBER OF POINTS ON EACH PROCESS SENDING DATA
        INTEGER, POINTER, DIMENSION(:)   :: SEND_NPOIN
        !GLOBAL NODE NUMBER OF THE SENDER
        INTEGER, POINTER, DIMENSION(:)   :: SEND_KNOLG

        !NUMBER OF PROCCESS TO SEND TO (INCLUDING LOCAL)
        INTEGER :: NP_SEND
        !NUMBRE OF PROCESSES TO RECEIVE FROM (INCLUDING LOCAL)
        INTEGER :: NP_RECV
        !NUMBER OF PROCCESS TO SEND TO (EXCLUDING LOCAL)
        INTEGER :: NP_SEND_NOLOC
        !NUMBRE OF PROCESSES TO RECEIVE FROM (EXCLUDING LOCAL)
        INTEGER :: NP_RECV_NOLOC

        !BUFFER FOR SENDING DATA; FIRST DIMENSION: ID; SECOND DIMENSION: PROCESS
        DOUBLE PRECISION, POINTER, DIMENSION(:,:)   :: SEND_BUF
        !BUFFER FOR RECEIVING DATA; FIRST DIMENSION: ID; SECOND DIMENSION: PROCESS
        DOUBLE PRECISION, POINTER, DIMENSION(:,:)   :: RECV_BUF
        !LIST OF MPI_REQUEST FOR MPI WAIT_ALL
        INTEGER, POINTER, DIMENSION(:) :: REQLIST

        !MAXIMUM NUMBER OF NODES USED TO CALCULATE VALUES (USING INTERPOLATION)
        INTEGER :: NPP
        !INTERPOLATION WEIGHTS
        DOUBLE PRECISION, POINTER, DIMENSION(:,:) :: WEIGHTS

      END TYPE

      !RECV_LIST: NPX2 LIST WITH IN THE FIRST COLUMN THE LOCAL POINT NUMBER
      ! ON THE SENDER AND ON THE SECOND COLUMN THE PROCESS ID

      TYPE (COUPLE_DATA_TYPE), ALLOCATABLE, DIMENSION(:) :: CPLD
      INTEGER :: NRCOUPLE
! MAXIMUM OF MODULES COUPLING 2 ARE NEEDED FOR THE MOMENT
      INTEGER, PARAMETER :: MAXCOUPLE = 4
! MAXIMUM OF VARIABLES TO EXCHANGE
      INTEGER, PARAMETER :: MAXVAR    = 10
! MAXIMUM OF POINTS TO INTERPOLATE ON A POINT ON THE OTHER MESH.
      INTEGER, PARAMETER :: MAXNPP    = 20

      LOGICAL :: DO_COUPLE = .FALSE.

      SAVE
      PRIVATE

      PUBLIC :: INIT_COUPLE,ADD_SENDER, ADD_RECEIVER,
     &    SEND_COUPLE, RECEIVE_COUPLE, END_COUPLE, DO_COUPLE

      CONTAINS

!                   **********************
                    SUBROUTINE INIT_COUPLE
!                   **********************
     &()
!
!***********************************************************************
!BRIEF    CONSTRUCTOR FOR COUPLING ROUTINE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      INTEGER :: ICOUPLE, IERR
!
!-----------------------------------------------------------------------
!
!INITIALIZE COUPLING DATA

      DO_COUPLE = .TRUE.
      NRCOUPLE =  0
      ALLOCATE(CPLD(MAXCOUPLE),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'CPLD')

      !TODO CHECK IF NEEDED
      DO ICOUPLE=1,MAXCOUPLE
        ALLOCATE(CPLD(ICOUPLE)%SEND_NPOIN(MAX(NCSIZE,1)),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'SEND_NPOIN')
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE

!                   **********************
                    SUBROUTINE  ADD_SENDER
!                   **********************
     &(MESH_SEND,CID)
!
!***********************************************************************
!
!BRIEF    .
!
!NOTE: SENDER MUST BE ADDED BEFORE RECIEVER IS ADDED
!

!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CID          |-->| ID OF THE COUPLING (STARTS AT 1)
!| MESH_SEND    |-->| MESH (LOCAL) OF THE SENDER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)   :: CID
      TYPE (BIEF_MESH), INTENT(IN)   :: MESH_SEND
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

      INTEGER :: NPOIN, IERR , IPOIN, IPROC, ITAG, IREQ
      INTEGER , DIMENSION(1) :: SEND_BUF
      INTEGER , DIMENSION(:), ALLOCATABLE :: RECV_BUF
!
!-----------------------------------------------------------------------
!
      ALLOCATE(CPLD(CID)%REQLIST(MAX(NCSIZE-1,1)),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'REQLIST')

      ALLOCATE (RECV_BUF(MAX(NCSIZE,1)),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'RECV_BUF')
      RECV_BUF = 0
      !COUNT NUMBER OF COUPLINGS
      NRCOUPLE = NRCOUPLE +1
      NPOIN = MESH_SEND%NPOIN

      !ADD NUMBER OF POINTS OF SENDER TO ALL PROCESSES
      SEND_BUF(1) = NPOIN

#if defined (HAVE_MPI)
      IF (NCSIZE.GT.1) THEN
        CALL MPI_ALLGATHER(SEND_BUF, 1, MPI_INTEGER, RECV_BUF, 1,
     &   MPI_INTEGER, COMM, IERR)
      ELSE
          RECV_BUF(1) = SEND_BUF(1)
      ENDIF
#else
      RECV_BUF(1) = SEND_BUF(1)
#endif

      IF (IERR.NE.0) THEN
        WRITE (LU,*) 'ALL TO ALL PROBLEM IN ADD_SENDER'
        CALL PLANTE(1)
      ENDIF


      IF (NCSIZE.GT.1) THEN
        CALL P_SYNC
      ENDIF

      CPLD(CID)%SEND_NPOIN(1:MAX(NCSIZE,1)) = RECV_BUF(1:MAX(NCSIZE,1))
      DEALLOCATE (RECV_BUF)
      CPLD(CID)%MAXPOIN = MAXVAL(CPLD(CID)%SEND_NPOIN(1:MAX(NCSIZE,1)))

      ! SAVE GLOBAL POINT NUMBERS

      ALLOCATE(CPLD(CID)%SEND_KNOLG(NPOIN),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'SEND_KNOLG')
      IF (NCSIZE.GT.1) THEN
        DO IPOIN = 1,NPOIN
          CPLD(CID)%SEND_KNOLG(IPOIN)=MESH_SEND%KNOLG%I(IPOIN)
        ENDDO
      ELSE
        ! NOT SURE IF THIS IS NEEDED
        DO IPOIN = 1,NPOIN
          CPLD(CID)%SEND_KNOLG(IPOIN)= IPOIN
        ENDDO
      ENDIF

      ! SEND GLOBAL POINT NUMBERS TO ALL OTHER PROCS
      !  ALREADY WORKS ON 1 PROCESSOR
      IREQ = 0
      DO IPROC = 0,MAX(NCSIZE,1)-1
        IF (IPROC.EQ.IPID) THEN
          CYCLE
        ENDIF
        ITAG = IPID+IPROC*NCSIZE
        IREQ = IREQ+1
        CALL P_IWRITI(CPLD(CID)%SEND_KNOLG,NPOIN,
     &      IPROC,ITAG,CPLD(CID)%REQLIST(IREQ))
      ENDDO

      RETURN
      END SUBROUTINE

!-----------------------------------------------------------------------
!
!                   ***********************
                    SUBROUTINE READ_RECV
!                   ***********************
     &        (FILE_RECV,RECV_VAR,CID,NPOIN,SEND_NODE,NPP)
!
!***********************************************************************
!
!BRIEF    READS THE VARIABLE(S) IN THE SELAFIN CONTAINING THE MAPPING
!         BETWEEN TWO COUPLED MODELS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] FILE_RECV    FILE WITH INFORMATION FROM WHERE TO RECEIVE DATA
!>@param[in] RECV_VAR     NAME OF VARIABLE  WHICH CONTAINS THE NODE NR OF THE SENDER
!>@param[in] CID          ID OF THE COUPLING (STARTS AT 1)
!>@param[in] NPOIN        NUMBER OF 2D MESH POINTS
!>@param[in] SEND_NODE    ARRAY OF NODES SEND
!>@param[in] NPP          MAXIMUM NUMBER OF NODES USED IN DETERMINATION OF A NODE


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         ,  INTENT(IN) :: CID, NPOIN
      TYPE(BIEF_FILE) ,  INTENT(IN) :: FILE_RECV
      CHARACTER(LEN=16), INTENT(IN) :: RECV_VAR
      DOUBLE PRECISION,  INTENT(OUT), DIMENSION(:,:), ALLOCATABLE ::
     & SEND_NODE
      INTEGER,  INTENT(INOUT) :: NPP
      CHARACTER(LEN=2)  :: STRI
      CHARACTER(LEN=16), ALLOCATABLE :: VAR_LIST(:)
      CHARACTER(LEN=16), ALLOCATABLE :: UNIT_LIST(:)
      CHARACTER(LEN=16) :: VARNAME
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      INTEGER :: IERR,  NVAR, IVAR, INUM, I, INDVAR, IPOIN, NRFOUND
      DOUBLE PRECISION :: TMP

      ! DETERMINE NUMBER OF POINTS USED IN INTERPOLATIONS   (FROM FIND_VARIABLE)

      ! READ VARIABLE NAMES FROM SLF FILE
      CALL GET_DATA_NVAR(FILE_RECV%FMT,FILE_RECV%LU,NVAR,IERR)
      CALL CHECK_CALL(IERR, 'FIND_VARIABLE:GET_DATA_NVAR')
      ALLOCATE(VAR_LIST(NVAR),STAT=IERR)
      ALLOCATE(UNIT_LIST(NVAR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'VAR_LIST')
      CALL GET_DATA_VAR_LIST(FILE_RECV%FMT,FILE_RECV%LU,NVAR,
     &                   VAR_LIST,UNIT_LIST,IERR)
      CALL CHECK_CALL(IERR, 'FIND_VARIABLE:GET_DATA_VAR_LIST')

      ! LOOK FOR SPACE IN VARIABLE NAME
      INUM = 0
      DO I=1,16
        IF (RECV_VAR(I:I).EQ.' ') THEN
            INUM = I
            EXIT
        ENDIF
      ENDDO
      IF (INUM.EQ.0) THEN
        WRITE (LU,*) 'INVALID VARIABLE NAME FOR COUPLING: ', RECV_VAR
        CALL PLANTE (1)
      ENDIF
      NRFOUND = 0
      DO IVAR = 1,NVAR
        IF (VAR_LIST(IVAR)(1:INUM-1).EQ.RECV_VAR(1:INUM-1).AND.
     &    VAR_LIST(IVAR)(INUM:INUM).NE. 'W')  THEN
          NRFOUND = NRFOUND + 1
        ENDIF
      ENDDO
      IF (NRFOUND.EQ.0) THEN
        WRITE (LU,*) 'ERROR: COUPLE_MOD NO COUPLING INFO FOR ',
     &        RECV_VAR
        CALL PLANTE(1)
      ENDIF


      ! LOOK FOR THE HIGHEST VARIABLE NUMBER.
      ! IT IS ASSUMED THAT VARIABLES ARE NUMBERED CONTINUOUSLY
      ! THIS IS CHECKED LATER WHEN READING THE VARIABLES
      NPP = 0
      DO IVAR = 1,NVAR
        IF (VAR_LIST(IVAR)(1:INUM-1).EQ.RECV_VAR(1:INUM-1).AND.
     &     VAR_LIST(IVAR)(INUM:INUM).NE. 'W')  THEN
          READ (VAR_LIST(IVAR)(INUM:INUM+1),*) INDVAR
          IF (INDVAR.GT.MAXNPP .OR. INDVAR.LE.0) THEN
            WRITE (LU,*) 'INVALID NUMBER FOR ',RECV_VAR,': ',INDVAR
            CALL PLANTE (1)
          ENDIF
          NPP = MAX(NPP,INDVAR)
        ENDIF
      ENDDO

      DEALLOCATE(VAR_LIST)
      DEALLOCATE(UNIT_LIST)

!----------------------------------------------------------------------
! ALLOCATE MEMORY FOR WEIGHTS AND SEND NODES

      ALLOCATE(SEND_NODE(NPOIN,NPP),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'SEND_NODE')

      ALLOCATE(CPLD(CID)%WEIGHTS(NPOIN,NPP),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'WEIGHTS')


      ! LOOP OVER ALL NEEDED VARIABLES
      DO I=1,NPP
        WRITE(STRI,'(I2.2)') I
        VARNAME = RECV_VAR(1:INUM-1)//STRI
      ! READ  ID FROM WHERE TO RECEIVE DATA (GLOBAL ID OF THE SENDER)
        CALL FIND_VARIABLE(FILE_RECV%FMT,FILE_RECV%LU,VARNAME,
     &      SEND_NODE(:,I),NPOIN,IERR)
        IF (IERR.NE.0) THEN
          WRITE (LU,*) 'VARIABLE ',VARNAME, ' CANNOT BE READ '
          CALL PLANTE(1)
        ENDIF

        ! READ INTERPOLATION WEIGHTS
        VARNAME = RECV_VAR(1:INUM-1)//'WTS'//STRI
        CALL FIND_VARIABLE(FILE_RECV%FMT,FILE_RECV%LU,VARNAME,
     &      CPLD(CID)%WEIGHTS(:,I),NPOIN,IERR)
        IF (IERR.NE.0) THEN
          WRITE (LU,*) 'VARIABLE ',VARNAME, ' CANNOT BE READ '
          CALL PLANTE(1)
        ENDIF
      ENDDO
      ! CHECK WEIGHTS SUM TO ONE IN CASE NO DEFAULT IS USED
      DO IPOIN=1,NPOIN
        IF (ANY(SEND_NODE(IPOIN,:).GT.0)) THEN
          TMP = SUM(CPLD(CID)%WEIGHTS(IPOIN,:))
      ! NOTE THRESHOLD IS HIGH. PROBABLY DUE TO SINGLE PRECISION IN SLF FILE
          IF (ABS(TMP-1.0D0).GT.1.0D-4) THEN
            WRITE (LU,*) 'WRONG WEIGHTS IN POINT ', NPOIN
            WRITE (LU,*) 'WEIGHST ARE: ', CPLD(CID)%WEIGHTS(IPOIN,:)
            CALL PLANTE(1)
          ELSE ! MAKE SURE THEY ARE ALSO ACCURATE IN DOUBLE PRECISION
            CPLD(CID)%WEIGHTS(IPOIN,:)=CPLD(CID)%WEIGHTS(IPOIN,:)/TMP
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
!                   ***********************
                    SUBROUTINE ADD_RECEIVER
!                   ***********************
     &         (MESH_RECV,FILE_RECV,RECV_VAR,CID)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MESH_RECV         |-->| MESH (LOCAL) OF THE RECEIVER
!| FILE_RECV         |-->| FILE WITH INFORMATION FROM WHERE TO RECEIVE DATA
!| RECV_VAR          |-->| NAME OF VARIABLE  WHICH CONTAINS THE NODE NR OF THE SENDER
!| CID               |-->| ID OF THE COUPLING (STARTS AT 1)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         ,  INTENT(IN) :: CID
      TYPE(BIEF_FILE) ,  INTENT(IN) :: FILE_RECV
      TYPE(BIEF_MESH),   INTENT(IN) :: MESH_RECV
      CHARACTER(LEN=16), INTENT(IN) :: RECV_VAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      INTEGER :: IERR, NPOIN , IPROC, IP_S, IP_R, IP_P
      INTEGER :: MAXP, IPOIN, NP, ITMP, ITAG , IREQ, NPR,NPS, NPP

      INTEGER, ALLOCATABLE, DIMENSION(:) :: NSEND

      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: SEND_NODE
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: TMP_SEND, LOC_SEND_MAP
      LOGICAL, ALLOCATABLE, DIMENSION(:,:) :: DO_SEND
      INTEGER, ALLOCATABLE, DIMENSION(:) :: TMP_KNOLG
      LOGICAL, ALLOCATABLE, DIMENSION(:) :: TMP_LOG

      ! LIST WITH FOR EACH POINT, FROM WHERE TO RECEIVE DATA
      !1.) LOCAL NODE NR
      !2.) PROCES ID
      INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: RECV_LIST
!
!-----------------------------------------------------------------------
!
      CALL DEBUG_INFO('add_receiver: '//RECV_VAR,CID)

      NPOIN = MESH_RECV%NPOIN
      MAXP  = CPLD(CID)%MAXPOIN

      ! READ WEIGHTS AND NODEINDICES FROM SELAFIN FILE
      CALL READ_RECV(FILE_RECV,RECV_VAR,CID,NPOIN,SEND_NODE,NPP)
      CPLD(CID)%NPP = NPP

      !ALLOCATE DATA
      ALLOCATE(RECV_LIST(NPOIN,2,NPP),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'RECV_LIST')
      RECV_LIST = 0

      ALLOCATE(TMP_KNOLG(MAXP),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TMP_KNOLG')

!     FIND CORRESPONDING LOCAL NUMBERS ON SENDER
!     (ONLY WORKS IF SENDER IS ALREADY ADDED)

      RECV_LIST = 0
      DO IPROC = 0,MAX(NCSIZE,1)-1
      ! RECEIVE KNOLG FROM SENDER
      ! BLOCKING RECEIVE TO LIMIT AMOUNT OF DATA
        NP = CPLD(CID)%SEND_NPOIN(IPROC+1)
        IF (IPROC.NE.IPID) THEN
          ITAG = IPID*NCSIZE+IPROC
          !DEFACTO BLOCKING RECEIVE
          CALL  P_IREADI(TMP_KNOLG,NP,
     &         IPROC,ITAG,CPLD(CID)%REQLIST(1))
          CALL P_WAIT_PARACO(CPLD(CID)%REQLIST(1:1),1)
        ELSE
      ! DATA IS ALREADY ON THE SAME PROCESS
          TMP_KNOLG(1:NP) = CPLD(CID)%SEND_KNOLG(1:NP)
        ENDIF

        !FIND MATCHING POINTS
        DO  IP_P = 1,NPP
          DO IP_R = 1,NPOIN
            DO IP_S = 1,NP
              IF (NINT(SEND_NODE(IP_R,IP_P)).EQ.TMP_KNOLG(IP_S)) THEN
                RECV_LIST(IP_R,1,IP_P) = IP_S
                RECV_LIST(IP_R,2,IP_P) = IPROC+1
                EXIT
              ENDIF
            ENDDO  !IP_R
          ENDDO  !IP_S
        ENDDO   ! IP_P
      ENDDO ! IPROC

!----------------------------------------------------------------------
! BUILD UP SEND LIST AND RECEIVE LIST

      !COMMUNICATED DATA
      ALLOCATE(CPLD(CID)%SEND_MAP(MAXP,MAX(NCSIZE,1)),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'SEND_MAP')
      ALLOCATE(LOC_SEND_MAP(MAXP,MAX(NCSIZE,1)),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'_LOC_SEND_MAP')
      LOC_SEND_MAP = 0

      ALLOCATE(CPLD(CID)%RECV_MAP(MAX(NCSIZE,1),NPOIN,NPP),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'RECV_MAP')

      !NUMBER OF POINTS TO COMMUNICATE
      ALLOCATE(CPLD(CID)%POIN2SEND(MAX(NCSIZE,1)),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'POIN2SEND')
      ALLOCATE(CPLD(CID)%POIN2RECV(MAX(NCSIZE,1)),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'POIN2RECV')


      ALLOCATE(DO_SEND(MAX(NCSIZE,1),MAXP),STAT=IERR)
      DO_SEND = .FALSE.
      ALLOCATE(TMP_SEND(MAX(NCSIZE,1),MAXP),STAT=IERR)
      TMP_SEND =0

      ALLOCATE(NSEND(MAX(NCSIZE,1)),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'NSEND')
      NSEND = 0

      CPLD(CID)%POIN2SEND = 0
      CPLD(CID)%POIN2RECV = 0
      CPLD(CID)%RECV_MAP = 0
      CPLD(CID)%SEND_MAP = 0

!----------------------------------------------------------------------
      ! STORE NUMBER OF POINTS
      ! THIS VARIABLE MAPS NUMBER OF POINTS ON SENDER TO NUMBER OF POINTS ON RECEIVER

      !LOOK FOR EACH POINT WHERE IT COMES FROM
      DO IP_P = 1,NPP
        DO IP_R=1,NPOIN
        ! VALUES FROM GLOBAL LIST
          IP_S  = RECV_LIST(IP_R,1,IP_P)
          IPROC = RECV_LIST(IP_R,2,IP_P)
          IF (IP_S.GT.0) THEN
            IF (.NOT.DO_SEND(IPROC,IP_S)) THEN
          !POINT NOT YET SEND TO THIS PROCESS. ADD TO THE LISTS
              DO_SEND(IPROC,IP_S) = .TRUE.
              NSEND(IPROC)  = NSEND(IPROC)+1
              CPLD(CID)%RECV_MAP(IPROC,IP_R,IP_P) = NSEND(IPROC)
              ! NOTE ORDER OF ARGUMENTS IS OPPOSITE IN LOC_SEND_MAP
              ! THIS IS  NEEDED FOR MPI_SCATTER LATER
              LOC_SEND_MAP(NSEND(IPROC),IPROC) = IP_S
              TMP_SEND(IPROC,IP_S) = NSEND(IPROC)
            ELSE
              ! POINT IS ALREADY ON THE LIST. LOOK UP ITS ID
              CPLD(CID)%RECV_MAP(IPROC,IP_R,IP_P) = TMP_SEND(IPROC,IP_S)
            ENDIF
          ENDIF
        ENDDO !IP_R
      ENDDO !IP_P

      ! STORE NUMBER OF POINTS
      DO IPROC = 1,MAX(NCSIZE,1)
        CPLD(CID)%POIN2RECV(IPROC) = NSEND(IPROC)
      ENDDO

!----------------------------------------------------------------------

      ! COMMUNICATE NUMBER OF POINTS TO OTHER PROCESSES
#if defined (HAVE_MPI)
      DO IPROC = 0,MAX(NCSIZE,1)-1
        CALL MPI_SCATTER(CPLD(CID)%POIN2RECV,1,MPI_INTEGER,
     &  CPLD(CID)%POIN2SEND(IPROC+1),1,MPI_INTEGER,IPROC,COMM,IERR)
        CALL P_SYNC()
      ENDDO

      !NUMBER OF PROCESSES FOR COMMUNICATION
      CPLD(CID)%NP_SEND = COUNT(CPLD(CID)%POIN2SEND.GT.0)
      CPLD(CID)%NP_RECV = COUNT(CPLD(CID)%POIN2RECV.GT.0)
      IF (CPLD(CID)%POIN2SEND(IPID+1).GT.0) THEN
        CPLD(CID)%NP_SEND_NOLOC = CPLD(CID)%NP_SEND - 1
      ELSE
        CPLD(CID)%NP_SEND_NOLOC = CPLD(CID)%NP_SEND
      ENDIF
      IF (CPLD(CID)%POIN2RECV(IPID+1).GT.0) THEN
        CPLD(CID)%NP_RECV_NOLOC = CPLD(CID)%NP_RECV - 1
      ELSE
        CPLD(CID)%NP_RECV_NOLOC = CPLD(CID)%NP_RECV
      ENDIF

#else
      CPLD(CID)%POIN2SEND(1)  = CPLD(CID)%POIN2RECV(1)
      CPLD(CID)%NP_SEND = 1
      CPLD(CID)%NP_RECV = 1
#endif
      IF (IERR.NE.0) THEN
        WRITE (LU,*) 'ERROR IN MPI_SCATTER POIN2SEND'
        CALL PLANTE(1)
      ENDIF

!----------------------------------------------------------------------
!     COMMUNICATE SEND LIST TO SENDER  (DUMMY VALUES ARE ALSO COPIED)
      IREQ = 0
      DO IPROC = 0,MAX(NCSIZE,1)-1
        NP =  CPLD(CID)%POIN2RECV(IPROC+1)
        IF (NP.GT.0) THEN
          IF (IPROC.NE.IPID) THEN
            IREQ = IREQ + 1
            ITAG = IPID+IPROC*NCSIZE
            CALL  P_IWRITI(LOC_SEND_MAP(1:NP,IPROC+1),
     &           NP,IPROC,ITAG,CPLD(CID)%REQLIST(IREQ))
          ENDIF
        ENDIF
      ENDDO

      ! RECEIVE LIST AT SENDER
      IREQ = 0
      DO IPROC = 0,MAX(NCSIZE,1)-1
        NP =  CPLD(CID)%POIN2SEND(IPROC+1)
        IF (NP.GT.0) THEN
          IF (IPROC.NE.IPID) THEN
            IREQ = IREQ + 1
            ITAG = IPROC+IPID*NCSIZE
            CALL  P_IREADI(CPLD(CID)%SEND_MAP(1:NP,IPROC+1),NP,
     &           IPROC,ITAG,CPLD(CID)%REQLIST(IREQ))
          ELSE
            ! LOCAL COPY
            DO IPOIN = 1,NP
              CPLD(CID)%SEND_MAP(IPOIN,IPROC+1) =
     &            LOC_SEND_MAP(IPOIN,IPROC+1)
            ENDDO
          ENDIF
        ENDIF
      ENDDO

      IF (NCSIZE.GT.1) THEN
        IF (IREQ.GT.0) THEN
          CALL P_WAIT_PARACO(CPLD(CID)%REQLIST(1:IREQ),IREQ)
        ENDIF
      ENDIF

      ! THIS P_SYNC SOLVES A SEG_FAULT; NO IDEA WHY
      IF (NCSIZE.GT.1) THEN
        CALL P_SYNC
      ENDIF

!----------------------------------------------------------------------

      !MAXIMUM NUMBER OF POINTS SEND TO THIS PROCESS

      NPS  = CPLD(CID)%NP_SEND
      NPR  = CPLD(CID)%NP_RECV

      !ALLOCATE DATA FOR SENDING AND RECEIVING
      IF (NPS.GT.0) THEN
        MAXP = MAXVAL(CPLD(CID)%POIN2SEND(1:MAX(NCSIZE,1)))
        ALLOCATE(CPLD(CID)%SEND_BUF(MAXVAR*MAXP,NPS),STAT=IERR)
        CPLD(CID)%SEND_BUF = -99
        CALL CHECK_ALLOCATE(IERR,'SEND_DATA')
      ENDIF

      IF (NPR.GT.0) THEN
        MAXP = MAXVAL(CPLD(CID)%POIN2RECV(1:MAX(NCSIZE,1)))
        ALLOCATE(CPLD(CID)%RECV_BUF(MAXVAR*MAXP,NPR),STAT=IERR)
        CPLD(CID)%RECV_BUF = -999
        CALL CHECK_ALLOCATE(IERR,'RECV_BUF')
      ENDIF

      !DEALLOCATE DATA THAT IS NOT NEEDED ANYMORE

      DEALLOCATE (CPLD(CID)%SEND_NPOIN,STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'DEAL:SEND_NPOIN')

      DEALLOCATE (CPLD(CID)%SEND_KNOLG,STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'DEAL:SEND_KNOLG')

      DEALLOCATE(TMP_KNOLG,STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'DEAL:TMP_KNOLG')

      DEALLOCATE(SEND_NODE,STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'DEAL:SEND_NODE')

      DEALLOCATE(RECV_LIST,STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'DEAL:RECV_LIST')

      DEALLOCATE(NSEND,STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'DEAL:NSEND')

      DEALLOCATE(DO_SEND,STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'DEAL:DO_SEND')

      DEALLOCATE(TMP_SEND,STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'DEAL:TMP_SEND')

      DEALLOCATE(LOC_SEND_MAP,STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'DEAL:LOC_SEND_MAP')

      !----------------------------------
      ! CHECK THAT ALL POINTS RECEIVE INFORMATION

      ALLOCATE(TMP_LOG(NPOIN),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'AL:TMPLOG')
      TMP_LOG = .FALSE.
      DO IP_P=1,NPP
        DO   IPROC =1,MAX(NCSIZE,1)
          NP  = CPLD(CID)%POIN2RECV(IPROC)
          IF (NP.GT.0) THEN
          ! EXTRACT DATA FROM SEND STRUCTURE
            DO IPOIN = 1,NPOIN
              ITMP = CPLD(CID)%RECV_MAP(IPROC,IPOIN,IP_P)
              IF (ITMP.GT.0) THEN
                TMP_LOG(IPOIN) = .TRUE.
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO

      IF (ALL(TMP_LOG)) THEN
        WRITE (LU,*) 'ALL NODES RECEIVE INFORMATION'
      ELSE
        WRITE (LU,*) 'WARNING: NO INFORMATION FOR ',
     &      COUNT(.NOT.TMP_LOG), ' NODES OUT OF ', NPOIN
      ENDIF
      DEALLOCATE(TMP_LOG,STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'DEAL:TMPLOG')

      !----------------------------------
      CALL DEBUG_INFO('END: ADD RECEIVER')
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE


!                   **********************
                    SUBROUTINE SEND_COUPLE
!                   **********************
     &(CID,NPOIN,NVAR,VARCOUPLE)
!
!BRIEF    .
!
!
! NOTES: IT IS ASSUMED THAT THE RECEIVE CALL FOLLOWS THE SEND CALL
!         AND THAT TWO CONSECUTIVE SENDS DO NOT OCCUR. HOWEVER, THIS IS
!         NOT CHECKED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] NPOIN         NUMBER OF POINTS
!>@param[in] NVAR          NUMBER OF VARIABLES
!>@param[in] CID           COUPLE ID
!>@param[in] VARCOUPLE     STRUCTURE WITH VARIABLES TO BE SEND
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER       , INTENT(IN) :: NPOIN, NVAR,CID
      TYPE(BIEF_OBJ), INTENT(IN) :: VARCOUPLE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      INTEGER :: IPOIN, IVAR, I, ITMP, NP, IPROC,IP,IPB, DUMMY
      INTEGER :: IREQ, ITAG , NPV, NOERR, IERR

!
!-----------------------------------------------------------------------

!     CHECK INPUT
      IF (NVAR.GT.MAXVAR) THEN
        WRITE (LU,*) 'TOO MANY VARIABLES'
        CALL PLANTE(1)
      ENDIF
      IF (NVAR.EQ.0) THEN
        RETURN
      ENDIF

!     ALLOCATE REQLIST, AS IT IS AUTOMATICALLY DEALLOCATED BY WAIT
      IF (NCSIZE.GT.1) THEN
          ALLOCATE(CPLD(CID)%REQLIST(MAX(NCSIZE,1)-1),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'REQLIST')
      ENDIF
!     ALREADY RECEIVE THE DATA TO OPTIMIZE MPI COMMUNICATION
!     THE REAL RECEIVE WILL BE FINISHED BY THE MPI_WAIT CALL
      IP  = 0
      IPB = 0
      DO   IPROC =1,MAX(NCSIZE,1)
        NP  = CPLD(CID)%POIN2RECV(IPROC)
        NPV = NP*NVAR
        IF (NP.GT.0) THEN
          IPB  = IPB + 1
          IF (IPID.NE.IPROC-1) THEN
            !RECEIVE DATA   (NON-BLOCKING)
            IP  = IP + 1
            ITAG = (IPROC-1)+IPID*NCSIZE
            CALL  P_IREAD(CPLD(CID)%RECV_BUF(1:NPV,IPB),NPV,8,IPROC-1,
     &               ITAG,CPLD(CID)%REQLIST(IP))
          ENDIF
        ENDIF
      ENDDO

      IF (NCSIZE.GT.1) THEN
        CALL P_SYNC
      ENDIF

      IREQ = 0
!     ADD DATA TO SEND STRUCTURE
      IP = 0
      NOERR = 0
      DO IPROC=1,MAX(NCSIZE,1)
        I = 1
        NP = CPLD(CID)%POIN2SEND(IPROC)
        IF (NP.GT.0) THEN
          IP = IP +1
          DO IVAR =1,NVAR
            DO IPOIN=1,NP
              ITMP = CPLD(CID)%SEND_MAP(IPOIN,IPROC)
              IF (ITMP.GT.NPOIN.OR. ITMP.LT.1) THEN
                WRITE (LU,*) 'WRONG VALUE FOR SENDMAP: ',
     &             ITMP, ' FOR ',IPOIN,IVAR,IP,I
                NOERR = NOERR + 1
                CYCLE
              ENDIF
              CPLD(CID)%SEND_BUF(I,IP) = VARCOUPLE%ADR(IVAR)%P%R(ITMP)
              I = I+1
            ENDDO
          ENDDO
        ENDIF
      ENDDO
      IF (NCSIZE.GT.1) THEN
        CALL P_SYNC
      ENDIF

!      SEND DATA (NON BLOCKING)
      IP = 0
      DO IPROC=1,MAX(NCSIZE,1)
        NP = CPLD(CID)%POIN2SEND(IPROC)*NVAR
        IF (NP.GT.0) THEN
          IP = IP + 1
          IF (IPROC-1.NE.IPID) THEN
            ITAG = (IPROC-1)*NCSIZE+IPID
            IREQ = IREQ+1
            CALL  P_IWRIT(CPLD(CID)%SEND_BUF(1:NP,IP),
     &           NP,8,IPROC-1,ITAG,DUMMY)
          ELSE
            ! COPY DATA ON THE SAME PROCESSOR
            ITMP = COUNT(CPLD(CID)%POIN2RECV(1:IPROC).GT.0)
            CPLD(CID)%RECV_BUF(1:NP,ITMP) =
     &             CPLD(CID)%SEND_BUF(1:NP,IP)
          ENDIF
        ENDIF
      ENDDO

      ! SHUT DOWN IF THERE IS AN ERROR
      IF (NCSIZE.GT.1) THEN
        CALL P_SYNC
      ENDIF
      IF (NOERR.GT.0) THEN
        CALL PLANTE(1)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
!                   *************************************
                            SUBROUTINE RECEIVE_COUPLE
!                   *************************************
     &               (CID,NPOIN,NVAR,VARCOUPLE,DEFAULT_VAL)
!
!BRIEF    .
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     NPOIN       NUMBER OF POINTS
!>@param[in]     NVAR        NUMBER OF VARIABLES
!>@param[in]     CID         COUPLE ID
!>@param[in/out] VARCOUPLE   STRUCTURE WITH VARIABLES TO BE SEND
!>@param[in]     DEFAULT_VAL DEFAULT VALUE (IF NOT 0)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         ,INTENT(IN   ) :: NPOIN, CID, NVAR
      DOUBLE PRECISION, INTENT(IN), DIMENSION(NVAR), OPTIONAL ::
     &   DEFAULT_VAL
      TYPE (BIEF_OBJ), INTENT(INOUT) :: VARCOUPLE

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      INTEGER :: IPROC, ITMP , NP, I, IP, IVAR , IPR, NPR
      INTEGER :: NOERR, IPP,NPP
      DOUBLE PRECISION :: WP
      DOUBLE PRECISION, DIMENSION(NVAR) ::  DEF_VAR
!-----------------------------------------------------------------------
!
! TODO: CHECK THAT NUMBER OF VARIABLES SEND AND RECEIVED IS EQUAL


!     CHECK INPUT
      IF (NVAR.GT.MAXVAR) THEN
        WRITE (LU,*) 'TOO MANY VARIABLES'
        CALL PLANTE(1)
      ENDIF
      IF (NVAR.EQ.0) THEN
        RETURN
      ENDIF

!     SET DEFAULT VALUES (IN CASE NO VALUE IS SPECIFIED)
      IF (PRESENT(DEFAULT_VAL)) THEN
        DEF_VAR =  DEFAULT_VAL
      ELSE
        DEF_VAR =0.0
      ENDIF

      DO IP = 1,NPOIN
        DO IVAR=1,NVAR
          VARCOUPLE%ADR(IVAR)%P%R(IP) = DEF_VAR(IVAR)
        ENDDO
      ENDDO

      NPP = CPLD(CID)%NPP

      ! WAIT UNTIL ALL DATA ARE RECEIVED
      NPR  = CPLD(CID)%NP_RECV_NOLOC
      IF (NCSIZE.GT.1) THEN
        IF (NPR.GT.0) THEN
          CALL P_WAIT_PARACO(CPLD(CID)%REQLIST(1:NPR),NPR)
        ENDIF
      ENDIF

      !UNPACK DATA
      NOERR = 0

      DO IPP = 1,NPP
        IPR =  0
        DO   IPROC =1,MAX(NCSIZE,1)
          NP = CPLD(CID)%POIN2RECV(IPROC)
          IF (NP.GT.0) THEN
            IPR = IPR + 1
          ! EXTRACT DATA FROM SEND STRUCTURE
            DO IP = 1,NPOIN
              ITMP = CPLD(CID)%RECV_MAP(IPROC,IP,IPP)
              IF (ITMP.GT.0) THEN
                ! ERROR CHECKING
                IF (ITMP.GT.NP) THEN
                  NOERR = NOERR + 1
                  CYCLE
                ENDIF
                WP =  CPLD(CID)%WEIGHTS(IP,IPP)
                IF (IPP.EQ.1) THEN
                  DO IVAR=1,NVAR
                    I = ITMP+(IVAR-1)*NP
                    VARCOUPLE%ADR(IVAR)%P%R(IP) =
     &                 WP*CPLD(CID)%RECV_BUF(I,IPR)
                  ENDDO
                ELSE
                  DO IVAR=1,NVAR
                    I = ITMP+(IVAR-1)*NP
                    VARCOUPLE%ADR(IVAR)%P%R(IP) =
     &                 VARCOUPLE%ADR(IVAR)%P%R(IP) +
     &                 WP*CPLD(CID)%RECV_BUF(I,IPR)
                  ENDDO
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO

      ! SHUT DOWN IF THERE IS AN ERROR
      IF (NCSIZE.GT.1) THEN
          CALL P_SYNC
      ENDIF
      IF (NOERR.GT.0) THEN
        CALL PLANTE(1)
      ENDIF

!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE

!                   *********************
                    SUBROUTINE END_COUPLE
!                   *********************
!
!BRIEF  DEALLOCATE VARIABLE NEEDED FOR COUPLING
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      INTEGER :: ICPL, IERR
!
!-----------------------------------------------------------------------
!
      !DEACCLOCATE DATA FROM COUPLING STRUCTURES

      DO  ICPL =1,NRCOUPLE
        !MAPPING
        DEALLOCATE(CPLD(ICPL)%POIN2RECV)
        DEALLOCATE(CPLD(ICPL)%POIN2SEND)
        DEALLOCATE(CPLD(ICPL)%SEND_MAP)
        DEALLOCATE(CPLD(ICPL)%RECV_MAP)

        !DATA BUFFERS
        !TODO: THESE TWO POINTERS MAY NOT BE ALLOCATED
        DEALLOCATE(CPLD(ICPL)%RECV_BUF)
        DEALLOCATE(CPLD(ICPL)%SEND_BUF)
        DEALLOCATE(CPLD(ICPL)%REQLIST)
        DEALLOCATE(CPLD(ICPL)%WEIGHTS)
      ENDDO

      DO ICPL =1,MAXCOUPLE
        DEALLOCATE(CPLD(ICPL)%SEND_NPOIN)
      ENDDO
      !
      DEALLOCATE(CPLD)

!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE

!-----------------------------------------------------------------------
      SUBROUTINE DEBUG_INFO (DEBUG_STRING,DEBUG_INT)

!BRIEF  PRINT INFORMATION ABOUT A VARIABLE.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] DEBUG_STRING   MESH (LOCAL) OF THE RECEIVER
!>@param[in] DEBUG_INT    FILE_RECV   FILE WITH INFORMATION FROM WHERE TO RECEIVE DATA
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      IMPLICIT NONE

      CHARACTER (LEN=*), INTENT(IN) :: DEBUG_STRING
      INTEGER, OPTIONAL, INTENT(IN) :: DEBUG_INT
      WRITE (LU,*) ''
      WRITE (LU,*) '***********************************************'
      IF (PRESENT(DEBUG_INT)) THEN
        WRITE (LU,*) DEBUG_STRING, DEBUG_INT
      ELSE
        WRITE (LU,*) DEBUG_STRING
      ENDIF
      WRITE (LU,*) '***********************************************'
      WRITE (LU,*)''
      END SUBROUTINE

      END MODULE COUPLE_MOD
