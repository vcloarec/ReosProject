!                   **********************************
                    SUBROUTINE LECDON_TELEMAC2D_KHIONE
!                   **********************************
!
     &(MOTCAR,FILE_DESC,PATH,NCAR,CAS_FILE,DICO_FILE)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D, ONLY: NAMETRAC, NTRAC,MAXTRA
      USE DECLARATIONS_KHIONE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=250), INTENT(IN)    :: PATH
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: MOTCAR(MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
!     API
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: CAS_FILE
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: DICO_FILE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            :: K,I
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

      INTEGER :: ID_DICO, ID_CAS
      INTEGER NPRECONC,NPRETHI
      DOUBLE PRECISION, ALLOCATABLE :: PREFRZL_TMP(:)
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
        NOM_DIC=PATH(1:NCAR)//'ICEDICO'
        NOM_CAS=PATH(1:NCAR)//'ICECAS'
!
      ELSE
!
        NOM_DIC='ICEDICO'
        NOM_CAS='ICECAS'
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
!
      THERMAL_BUDGET = MOTLOG(ADRESS(3,1))
      ICOVER_IMPACT = MOTLOG(ADRESS(3,2))
      CLOGGING = MOTLOG(ADRESS(3,3))
      BD_ICE = MOTLOG(ADRESS(3,4))
      SALINITY = MOTLOG(ADRESS(3,5))
      DYN_ICOVER = MOTLOG(ADRESS(3,6))
      IPREC = MOTINT(ADRESS(1,25))
      NC_FRA = MOTINT(ADRESS(1,22))
      ITGM = MOTINT( ADRESS(1,11) )
      MVIST_FRZL = MOTINT(ADRESS(1,20))
!
!----------------------------------------------------------------
!
      CALL NAMETRAC_KHIONE(NAMETRAC,NTRAC,MAXTRA)
!
!----------------------------------------------------------------
!
      IF (NC_FRA.GT.0) THEN
        ALLOCATE(FRZL0(NC_FRA))
        ALLOCATE(DIFFRZLNU(NC_FRA))
        ALLOCATE(PREFRZL((NC_FRA+2)*MAXFRO))
        ALLOCATE(PREFRZL_TMP(NC_FRA*MAXFRO))
        ALLOCATE(PRECONC(MAXFRO))
        ALLOCATE(PRETHI(MAXFRO))
        ALLOCATE(SLVFRZL(1))
!       DEFAULT VALUE FOR ADVECTION SCHEME FOR FRAZIL: PSI SCHEME
        SCHADVFRZL=MOTINT(ADRESS(1,5))
!       DEFAULT VALUE FOR ADVECTION SCHEME OPTION: LOCAL IMPLICIT SCHEME
        OPTADV_FRZL=MOTINT(ADRESS(1,6))
!       FIRST ASSUMING A SINGLE VALUE FOR SOLVER FOR DIFFUSION OF         FRAZIL
        SLVFRZL(1)%SLV= MOTINT(ADRESS(1,21))
!       FIRST ASSUMING A SINGLE VALUE FOR SOLVER OPTION FOR DIFFUSION OF FRAZIL
        SLVFRZL(1)%KRYLOV= MOTINT(ADRESS(1,50))
!       FIRST ASSUMING A SINGLE VALUE FOR PRECONDITIONING FOR SOLVER FOR DIFFUSION OF FRAZIL
        SLVFRZL(1)%PRECON= MOTINT(ADRESS(1,52))
!       FIRST ASSUMING A SINGLE VALUE FOR MAXIMUM NUMBER OF ITERATIONS FOR SOLVER FOR DIFFUSION OF FRAZIL
        SLVFRZL(1)%NITMAX= MOTINT(ADRESS(1,51))
!       CHOICE OF ACCURACY FOR SOLVER FOR DIFFUSION OF FRAZIL:
!       ASSUMING A SINGLE VALUE FOR ALL SOLVERS
        SLVFRZL(1)%EPS= MOTREA(ADRESS(2,7))

        DO I=1,NC_FRA
!         INITIAL VALUES OF FRAZIL INITIALIZED TO ZERO
          FRZL0(I)=0.D0
!         INITIAL VALUES READ FROM KHIONE STEERING FILE
          IF(DIMENS(2,2).GE.I) THEN
            FRZL0(I) = MOTREA(ADRESS(2,2)+I-1)
          ENDIF
        ENDDO
        NPREFRZL = DIMENS(2,6)
        IF(NPREFRZL.NE.0) THEN
          DO I=1,NPREFRZL
            PREFRZL(I)=MOTREA(ADRESS(2,6)+I-1)
          ENDDO
        ENDIF
        DO I=1,NC_FRA
          DIFFRZLNU(I) = 1.D-6
          IF(DIMENS(2,9).GE.I) THEN
            DIFFRZLNU(I) = MOTREA(ADRESS(2,9)+I-1)
          ENDIF
        ENDDO
!
        IF(DYN_ICOVER) THEN
!         TRACERS KEYWORDS FOR PRECIPITATION
          CONC0 = MOTREA(ADRESS(2,38))
          THI0 = MOTREA(ADRESS(2,46))
          NPRECONC = DIMENS(2,41)
          NPRETHI = DIMENS(2,47)
          IF(NPRECONC.GT.0) THEN
            DO I=1,NPRECONC
              PRECONC(I) = MOTREA(ADRESS(2,41)+I-1)
            ENDDO
          ENDIF
          IF(NPRETHI.GT.0) THEN
            DO I=1,NPRETHI
              PRETHI(I) = MOTREA(ADRESS(2,47)+I-1)
            ENDDO
          ENDIF
          DIFCONCNU = MOTREA(ADRESS(2,43))
          DIFTHINU = MOTREA(ADRESS(2,48))
!         SAVES PRESCRIBED VALUES OF PRECIPITATION TRACERS IN PREFRZL
!         IN THE RIGHT ORDER BND1(T1;T2;...); BND2(T1....
          DO K=1,NPREFRZL
            PREFRZL_TMP(K) = PREFRZL(K)
          ENDDO
          NPREFRZL = NPREFRZL + NPRECONC + NPRETHI
          DO K=1,NPREFRZL
            I = (K - 1) / (NC_FRA + 2)
            IF(K.LE.NC_FRA+(NC_FRA+2)*I) THEN
              PREFRZL(K) = PREFRZL_TMP(K-I*2)
            ELSEIF(K.EQ.(NC_FRA+1)+(NC_FRA+2)*I) THEN
              PREFRZL(K) = PRECONC(I+1)
            ELSE
              PREFRZL(K) = PRETHI(I+1)
            ENDIF
          ENDDO
        ENDIF
!
!       WARNING:COEFFICIENT FOR HORIZONTAL DIFFUSION OF FRAZIL
!       IS ONLY FOR 3D SIMULATIONS
        IF(TROUVE(2,44).EQ.2) THEN
          WRITE(LU,*)'WARNING: COEFFICIENT FOR HORIZONTAL DIFFUSION OF'
          WRITE(LU,*)'FRAZIL IS NOT USED IN 2D.'
          WRITE(LU,*)'YOU NEED TO SET THE COEFFICIENT FOR DIFFUSION OF'
          WRITE(LU,*)'FRAZIL'
        ENDIF
      ENDIF
!
      IF(NC_FRA.GT.0) DEALLOCATE(PREFRZL_TMP)
!
!----------------------------------------------------------------
!
      RETURN
      END
