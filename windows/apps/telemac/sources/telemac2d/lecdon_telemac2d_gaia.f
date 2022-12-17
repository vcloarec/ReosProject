!                   *********************************
                    SUBROUTINE LECDON_TELEMAC2D_GAIA
!                   *********************************
!
     &(MOTCAR,FILE_DESC,PATH,NCAR,CAS_FILE,DICO_FILE,LOCAL_NSUSP_TEL)
!
!***********************************************************************
! TELEMAC-2D
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_DESC      |<--| STORES STRINGS 'SUBMIT' OF DICTIONARY
!| MOTCAR         |<--| VALUES OF KEY-WORDS OF TYPE CHARACTER
!| NCAR           |-->| NUMBER OF LETTERS IN STRING PATH
!| PATH           |-->| FULL PATH TO CODE DICTIONARY
!| LOCAL_NSUSP_TEL|<->| NUMBER OF SUSPENDED SEDIMENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D, ONLY: NAMETRAC,NTRAC,IND_SED,MAXSCE,
     &    MAXTRA
      USE DECLARATIONS_GAIA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)               :: NCAR
      INTEGER, INTENT(INOUT)            :: LOCAL_NSUSP_TEL
      CHARACTER(LEN=250), INTENT(IN)    :: PATH
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: MOTCAR(MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
!     API
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: CAS_FILE
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: DICO_FILE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            :: I,K,ICO,INCO
      INTEGER            :: MOTINT(MAXKEYWORD)
      INTEGER            :: TROUVE(4,MAXKEYWORD)
      INTEGER            :: ADRESS(4,MAXKEYWORD)
      INTEGER            :: DIMENS(4,MAXKEYWORD)
      DOUBLE PRECISION   :: MOTREA(MAXKEYWORD)
      LOGICAL            :: DOC
      LOGICAL            :: MOTLOG(MAXKEYWORD)
      CHARACTER(LEN=250) :: NOM_CAS
      CHARACTER(LEN=250) :: NOM_DIC
      CHARACTER(LEN=72)  :: MOTCLE(4,MAXKEYWORD,2)
      CHARACTER(LEN=3)   :: SEDTYPE

      INTEGER :: ID_DICO, ID_CAS
!
!-----------------------------------------------------------------------
!
      CHARACTER(LEN=2) CHAR2
!
!-----------------------------------------------------------------------
!
! INITIALISES THE VARIABLES FOR DAMOCLES CALL :
!
      DO K = 1, MAXKEYWORD
!       A FILENAME NOT GIVEN BY DAMOCLES WILL BE RECOGNIZED AS A WHITE SPACE
!       (IT MAY BE THAT NOT ALL COMPILERS WILL INITIALISE LIKE THAT)
        MOTCAR(K)(1:1)=' '
!
        DIMENS(1,K) = 0
        DIMENS(2,K) = 0
        DIMENS(3,K) = 0
        DIMENS(4,K) = 0
      ENDDO
!
!     WRITES OUT INFO
      DOC = .FALSE.
!
!-----------------------------------------------------------------------
!     OPENS DICTIONNARY AND STEERING FILES
!-----------------------------------------------------------------------
!
      IF(NCAR.GT.0) THEN
!
        NOM_DIC=PATH(1:NCAR)//'GAIDICO'
        NOM_CAS=PATH(1:NCAR)//'GAICAS'
!
      ELSE
!
        NOM_DIC='GAIDICO'
        NOM_CAS='GAICAS'
!
      ENDIF
      IF((CAS_FILE(1:1).NE.' ').AND.(DICO_FILE(1:1).NE.' ')) THEN
        WRITE(LU,*) 'FIXED DICO AND STEERING FILE PRESENT'
        NOM_DIC=DICO_FILE
        NOM_CAS=CAS_FILE
        WRITE(LU,*) 'NOM_DIC',NOM_DIC
        WRITE(LU,*) 'NOM_CAS',NOM_CAS
      ENDIF
!
      CALL GET_FREE_ID(ID_DICO)
      OPEN(ID_DICO,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
      CALL GET_FREE_ID(ID_CAS)
      OPEN(ID_CAS,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
!
!-----------------------------------------------------------------------
!     CALLS DAMOCLES
!-----------------------------------------------------------------------
!
      CALL DAMOCLE( ADRESS, DIMENS  ,MAXKEYWORD, DOC    , LNG , LU  ,
     &              MOTINT, MOTREA ,MOTLOG , MOTCAR ,
     &              MOTCLE, TROUVE ,ID_DICO, ID_CAS,.FALSE. ,FILE_DESC)
!
!-----------------------------------------------------------------------
!     CLOSES DICTIONNARY AND STEERING FILES
!-----------------------------------------------------------------------
!
      CLOSE(ID_DICO)
      CLOSE(ID_CAS)
!
!-----------------------------------------------------------------------
!     MAXIMUM NUMBER OF BOUNDARIES
      MAXFRO   = MOTINT( ADRESS(1,58) )
      SORTIS   = MOTCAR( ADRESS(4, 2) )(1:72)
!
      SUSP         = MOTLOG( ADRESS(3,  7) )
      NSICLA       = DIMENS(4,59)
      MVIST_SED    = MOTINT( ADRESS(1,1))
!     TYPE OF SEDIMENT AND NSICLA
      ICO = 1
      INCO = 1
      IF(NSICLA.GT.0) THEN
        IND_SED =  NTRAC+1
        DO I=1,NSICLA
          SEDTYPE = MOTCAR(ADRESS(4,59)+I-1)(1:3)
          IF(SEDTYPE.EQ.'CO') THEN
            WRITE(CHAR2,'(I2)') ICO
            ICO = ICO + 1
            LOCAL_NSUSP_TEL = LOCAL_NSUSP_TEL + 1
            CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,K,.TRUE.,
     &                       'SEDIMENT COH' //ADJUSTL(CHAR2)//'  ',
     &                       'COH SEDIMENT' //ADJUSTL(CHAR2)//'  ',
     &                       'g/l             ')
          ELSE IF(SEDTYPE.EQ.'NCO') THEN
            IF(SUSP) THEN
              WRITE(CHAR2,'(I2)') INCO
              INCO = INCO + 1
              LOCAL_NSUSP_TEL = LOCAL_NSUSP_TEL + 1
              CALL ADDTRACER(NAMETRAC,NTRAC,MAXTRA,K,.TRUE.,
     &                       'SEDIMENT NCOH' //ADJUSTL(CHAR2)//' ',
     &                       'NCOH SEDIMENT' //ADJUSTL(CHAR2)//' ',
     &                       'g/l             ')
            ENDIF
          ELSE IF(SEDTYPE.NE.'CO'.AND.SEDTYPE.NE.'NCO') THEN
            WRITE(LU,*)'LECDON_T2D_GAIA: CHECK TYPE OF SEDIMENT'
            WRITE(LU,*)'POSSIBLE CHOICES ARE: CO AND NCO'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
      ENDIF
!
!     MAIN SETTINGS FOR SUSPENDED SEDIMENTS WHICH ARE NECESSARY IN T2D
!
      IF(LOCAL_NSUSP_TEL.GT.0) THEN
        ALLOCATE(SCHADVSED(LOCAL_NSUSP_TEL))
        ALLOCATE(OPTADV_SED(LOCAL_NSUSP_TEL))
        ALLOCATE(SLVSED(LOCAL_NSUSP_TEL))
        ALLOCATE(SED0(LOCAL_NSUSP_TEL))
        ALLOCATE(PRESED(LOCAL_NSUSP_TEL*MAXFRO))
        ALLOCATE(SEDSCE(MAXSCE,LOCAL_NSUSP_TEL))
        DO I=1,LOCAL_NSUSP_TEL
!         DEFAULT VALUE FOR ADVECTION SCHEME FOR SUSPENDED SEDIMENTS: PSI SCHEME
          SCHADVSED(I)=5
!         DEFAULT VALUE FOR ADVECTION SCHEME OPTION: LOCAL IMPLICIT SCHEME
          OPTADV_SED(I)=4
!         FIRST ASSUMING A SINGLE VALUE FOR SOLVER FOR DIFFUSION OF SUSPENDED SEDIMENTS
          SLVSED(I)%SLV= MOTINT(ADRESS(1,26))
!         FIRST ASSUMING A SINGLE VALUE FOR SOLVER OPTION FOR DIFFUSION OF SUSPENDED SEDIMENTS
          SLVSED(I)%KRYLOV= MOTINT(ADRESS(1,27))
!         FIRST ASSUMING A SINGLE VALUE FOR PRECONDITIONING FOR SOLVER FOR DIFFUSION OF SUSPENDED SEDIMENTS
          SLVSED(I)%PRECON= MOTINT(ADRESS(1,28))
!         FIRST ASSUMING A SINGLE VALUE FOR MAXIMUM NUMBER OF ITERATIONS FOR SOLVER FOR DIFFUSION OF SUSPENDED SEDIMENTS
          SLVSED(I)%NITMAX= MOTINT(ADRESS(1,29))
!         CHOICE OF ACCURACY FOR SOLVER FOR DIFFUSION OF SUSPENDED SEDIMENTS:
!         ASSUMING A SINGLE VALUE FOR ALL SOLVERS
          SLVSED(I)%EPS= MOTREA(ADRESS(2,21))
        ENDDO
!       CHOICE OF ADVECTION SCHEME FOR SUSPENDED SEDIMENTS
        IF(TROUVE(1,8).EQ.2) THEN
          DO I=1,DIMENS(1,8)
            SCHADVSED(I)=MOTINT(ADRESS(1,8)+I-1)
          ENDDO
        ENDIF
!       CHOICE OF ADVECTION SCHEME OPTION FOR SUSPENDED SEDIMENTS
        IF(TROUVE(1,9).EQ.2) THEN
          DO I=1,DIMENS(1,9)
            OPTADV_SED(I)=MOTINT(ADRESS(1,9)+I-1)
          ENDDO
        ENDIF
!       NOW TAKING INTO ACCOUNT MULTIPLE VALUES FOR SOLVER FOR DIFFUSION OF SUSPENDED SEDIMENTS
        IF(TROUVE(1,26).EQ.2) THEN
          DO I=1,DIMENS(1,26)
            SLVSED(I)%SLV=MOTINT(ADRESS(1,26)+I-1)
          ENDDO
        ENDIF
!       NOW TAKING INTO ACCOUNT MULTIPLE VALUES FOR SOLVER OPTION FOR DIFFUSION OF SUSPENDED SEDIMENTS
        IF(TROUVE(1,27).EQ.2) THEN
          DO I=1,DIMENS(1,27)
            SLVSED(I)%KRYLOV=MOTINT(ADRESS(1,27)+I-1)
          ENDDO
        ENDIF
!       NOW TAKING INTO ACCOUNT MULTIPLE VALUES FOR PRECONDIT FOR DIFFUSION OF SUSPENDED SEDIMENTS
        IF(TROUVE(1,28).EQ.2) THEN
          DO I=1,DIMENS(1,28)
            SLVSED(I)%PRECON=MOTINT(ADRESS(1,28)+I-1)
          ENDDO
        ENDIF
!       NOW TAKING INTO ACCOUNT MULTIPLE VALUES FOR MAXIMUM NUMBER OF ITERATIONS FOR DIFFUSION OF SUSPENDED SEDIMENTS
        IF(TROUVE(1,29).EQ.2) THEN
          DO I=1,DIMENS(1,29)
            SLVSED(I)%NITMAX=MOTINT(ADRESS(1,29)+I-1)
          ENDDO
        ENDIF
        DO I=1,LOCAL_NSUSP_TEL
!         INITIAL VALUES OF SUSPENDED SEDIMENT INITIALIZE TO ZERO
          SED0(I)=0.D0
!         INITIAL VALUES READ FROM GAIA STEERING FILE
          IF(DIMENS(2,12).GE.I) THEN
            SED0(I) = MOTREA(ADRESS(2,12)+I-1)
          ENDIF
        ENDDO
        NPRESED = DIMENS(2,13)
        IF(NPRESED.NE.0) THEN
          DO I=1,NPRESED
            PRESED(I)=MOTREA(ADRESS(2,13)+I-1)
          ENDDO
        ENDIF
        NSEDSCE=DIMENS(2,38)
        IF(NSEDSCE.NE.0) THEN
          DO I=1,LOCAL_NSUSP_TEL
!           AT THIS STAGE WE DON'T KNOW HOW MANY SOURCES THERE ARE
!           (BY REGIONS OR BY COORD OR BY SOURCE NODES) SINCE THEY ARE
!            READ LATER IN THE LECDON_TELEMAC2D
            DO K=1,MAXSCE
              SEDSCE(K,I)=MOTREA(ADRESS(2,38)+(K-1)*LOCAL_NSUSP_TEL+I-1)
            ENDDO
          ENDDO
        ENDIF
        DIFSEDNU=MOTREA(ADRESS(2,43))
!       WARNING:COEFFICIENT FOR HORIZONTAL DIFFUSION OF SUSPENDED SEDIMENTS
!       IS ONLY FOR 3D SIMULATIONS
        IF(TROUVE(2,44).EQ.2) THEN
          WRITE(LU,*)'WARNING: COEFFICIENT FOR HORIZONTAL DIFFUSION OF'
          WRITE(LU,*)'SUSPENDED SEDIMENTS IS NOT USED IN 2D.'
          WRITE(LU,*)'YOU NEED TO SET THE COEFFICIENT FOR DIFFUSION OF'
          WRITE(LU,*)'SUSPENDED SEDIMENTS'
        ENDIF
      ENDIF
!----------------------------------------------------------------
!
      RETURN
      END
