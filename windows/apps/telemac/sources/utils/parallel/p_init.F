!                   *****************
                    SUBROUTINE P_INIT
!                   *****************
!
     &(CHAINE,NCAR,IPID,NCSIZE)
!
!***********************************************************************
! PARALLEL   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES.
!+                REGISTERS PROGRAM WITH PARASTATION.
!
!history  HANS HERRMANN (HANOVER)
!+        **/06/1996
!+
!+
!
!history  REINHARD HINKELMANN (HANOVER)
!+        08/06/1996
!+
!+   MODIFIED
!
!history  J-M HERVOUET (LNH)
!+        17/12/1996
!+
!+   MODIFIED
!
!history  RAINER JOHANNI (SGI MUNICH)
!+        **/10/1999
!+
!+   ADAPTED FOR MPI
!
!history  J.A. JANKOWSKI (BAW KARLSRUHE)
!+        28/12/1999
!+
!+   RELEASE 5.0 MODIFIED
!
!history  P. VEZOLLE (IBM)
!+        16/05/2008
!+        V5P9
!+   MODIFIED (SIZE OF EXTENSION)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHAINE         |---| WORKING DIRECTORY
!| IPID           |---| PROCESSUS ID
!| NCAR           |---| SIZE OF THE CHARACTER STRING
!| NCSIZE         |---| NUMBER OF MPI PROCESSUS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(OUT)            :: NCAR,IPID,NCSIZE
      CHARACTER(LEN=MAXLENTMPDIR), INTENT(OUT) :: CHAINE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_MPI
      INTEGER IER,NPREAD
!
      CHARACTER(LEN=MAXLENTMPDIR+5) EXE
!
      LOGICAL YAPARA,INIT
      INTEGER :: ID
      CHARACTER(LEN=11) PNUMBER
      CHARACTER(LEN=13) MYNAM
!
!-----------------------------------------------------------------------
! MPI IS SILENT WHEN EVERYTHING IS GOING ON PROPERLY
! DEFAULT LANGUAGE 2 AND STANDARD OUTPUT 6
! IN ORDER TO SEE ERROR MESSAGES (TO THE MASTER!)
! THE SUBROUTINE CALLED NEXT IS READ_CONFIG !
! THIS IS NOT PRETTY...
!
      NCAR = 0
      CHAINE =' '
      IPID=0
      NCSIZE=0
!
      LNG=2
!
! INITIALISES MPI
!
      INIT = .FALSE.
      CALL MPI_INITIALIZED(INIT,IER)
      IF(.NOT.INIT) THEN
#  ifdef COMPAD
        CALL AMPI_INIT(IER)
#  else
        CALL MPI_INIT(IER)
#  endif
        IF(IER.NE.0) THEN
          WRITE(LU,*) 'P_INIT: ERROR IN MPI_INIT'
          WRITE(LU,*) 'MPI ERROR ',IER
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
! OBTAINS IPID, IT IS VIRTUALLY THE PROCESSOR NUMBER (RANK)
!
      CALL MPI_COMM_RANK(COMM,IPID,IER)
      IF(IER.NE.0) THEN
        WRITE(LU,*) 'P_INIT: ERROR IN MPI_COMM_RANK'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
!
! OBTAINS NCSIZE
!
      CALL MPI_COMM_SIZE(COMM,NCSIZE,IER)
      IF(IER.NE.0) THEN
        WRITE(LU,*) 'P_INIT: ERROR IN MPI_COMM_SIZE'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
!
! MANIPULATES MASTER'S AND SLAVES' STANDART OUTPUT TO FILES PE#LOG.TXT
!
      IF(NCSIZE.GE.1) THEN
        PNUMBER = '00000-00000'
!
        IF((NCSIZE-1).LT.10) THEN
          WRITE(PNUMBER(05:05),'(I1)') NCSIZE-1
        ELSEIF((NCSIZE-1).LT.100) THEN
          WRITE(PNUMBER(04:05),'(I2)') NCSIZE-1
        ELSEIF((NCSIZE-1).LT.1000) THEN
          WRITE(PNUMBER(03:05),'(I3)') NCSIZE-1
        ELSEIF((NCSIZE-1).LT.10000) THEN
          WRITE(PNUMBER(02:05),'(I4)') NCSIZE-1
        ELSE
          WRITE(PNUMBER(01:05),'(I5)') NCSIZE-1
        ENDIF
!
        IF(IPID.LT.10) THEN
          WRITE(PNUMBER(11:11),'(I1)') IPID
        ELSEIF(IPID.LT.100) THEN
          WRITE(PNUMBER(10:11),'(I2)') IPID
        ELSEIF(IPID.LT.1000) THEN
          WRITE(PNUMBER(9:11),'(I3)') IPID
        ELSEIF(IPID.LT.10000) THEN
          WRITE(PNUMBER(8:11),'(I4)') IPID
        ELSE
          WRITE(PNUMBER(7:11),'(I5)') IPID
        ENDIF
        WRITE(MYNAM,'("PE", A11)') PNUMBER
!
      ENDIF
!
      IF(IPID.EQ.0) THEN
        IF(.NOT. STD_OUTPUT) THEN
          CALL GET_FREE_ID(LU)
          OPEN(UNIT=LU,FILE=MYNAM//'.LOG', FORM='FORMATTED',
     &           STATUS='UNKNOWN')
        ENDIF
        WRITE(LU,*) 'MASTER PROCESSOR NUMBER ',
     &              IPID,' OF THE GROUP OF ',NCSIZE
      ELSE
!     THIS IS NOW HANDLED VIA mpirun OPTIONS
        CALL GET_FREE_ID(LU)
        OPEN(UNIT=LU,FILE=MYNAM//'.LOG', FORM='FORMATTED',
     &         STATUS='UNKNOWN')
        WRITE(LU,*) 'SLAVE  PROCESSOR NUMBER ',
     &              IPID,' IN THE GROUP OF ',NCSIZE
      ENDIF
!
! READS THE NUMBER OF PROCESSORS AND NAME OF THE EXECUTABLE
!
      NCAR=0
      NPREAD=1
      YAPARA=.FALSE.
      INQUIRE(FILE='./PARAL',EXIST=YAPARA)
      IF(YAPARA) THEN
        CALL GET_FREE_ID(ID)
        OPEN(ID,FILE='PARAL',FORM='FORMATTED',ACTION='READ')
        READ(ID,*) NPREAD
        IF(NPREAD.NE.NCSIZE) THEN
          WRITE (LU,*)
     &      'P_INIT: FILE PARAL IS INCONSISTENT WITH MPI PARAMETERS'
          WRITE (LU,*) 'MPI NCSIZE   = ',NCSIZE
          WRITE (LU,*) 'PARAL NCSIZE = ',NPREAD
        ENDIF
        READ(ID,*) NCAR
        IF(NCAR.GT.MAXLENTMPDIR) THEN
          WRITE(LU,*) 'LENGTH OF TMP DIR STRING IS TO BIG'
          WRITE(LU,*) 'IT IS ',NCAR,' LONG AND SHOULD NOT EXCEED',
     &                MAXLENTMPDIR
          CALL PLANTE(1)
        ENDIF
        READ(ID,100) CHAINE
100     FORMAT(A250)
        EXE(1:NCAR+5)=CHAINE(1:NCAR) // 'A.EXE'
        WRITE(LU,*) 'EXECUTABLE FILE: ',EXE(1:NCAR+5)
        CLOSE(ID)
      ENDIF
!
! THE BARRIER USUALLY COMES UNEXPECTED
!
      CALL MPI_BARRIER(COMM,IER)
      IF (IER.NE.0) THEN
        WRITE(LU,*) 'P_INIT: ERROR MPI_BARRIER'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      NCAR = 0
      CHAINE =' '
      IPID=0
      NCSIZE=0
      IF(.NOT. STD_OUTPUT) THEN
        CALL GET_FREE_ID(LU)
        OPEN(UNIT=LU,FILE='PE.LOG', FORM='FORMATTED',
     &         STATUS='UNKNOWN')
      ENDIF
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
