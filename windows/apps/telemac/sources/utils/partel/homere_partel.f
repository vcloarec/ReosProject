!                   *********************
                    PROGRAM HOMERE_PARTEL
!                   *********************
!
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    PREPROCESSING STEP BEFORE A PARALLEL COMPUTATION
!
!history  Y. AUDOUIN (EDF)
!+        09/2012
!+        V7P0
!+        FIRST  VERSION
!
!history  V STOBIAC (EDF)
!+        09/2014
!+        V7P0
!+        READING OF THE MESH FORMAT
!
!history  Y. AUDOUIN (EDF)
!+        09/2012
!+        V7P0
!+        Reorganizing reading of parameters
!
!history  Y. AUDOUIN (EDF)
!+        21/05/2015
!+        V7P0
!+        Adding call to partel_resonly
!
!history C. COULET (ARTELIA)
!+       01/09/2016
!+       V7P2
!+       Modification to add the weir file management
!
!history  J. GRASSET (Daresbury Lab & EDF)
!+        01/05/2018
!+        Add code for managing concatenated mesh
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARTEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      CHARACTER(LEN=PATH_LEN) :: NAMEINP,NAMEGEO
      CHARACTER(LEN=PATH_LEN) :: NAMECLI
      INTEGER :: NPARTS
      INTEGER :: PMETHOD
      CHARACTER(LEN=PATH_LEN) :: NAMESEC
      CHARACTER(LEN=PATH_LEN) :: NAMEZFI
      CHARACTER(LEN=PATH_LEN) :: NAMESEU
      CHARACTER(LEN=8) :: GEOFORMAT,INPFORMAT
      CHARACTER(LEN=3) :: CONCATSTR
!
      LOGICAL :: RES_ONLY
      LOGICAL :: IS
      INTEGER I_LENCLI, I_LENINP, I_LENGEO
!
      CHARACTER(LEN=11) :: EXTENS
      EXTERNAL EXTENS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      LNG=2 ! JE NE PARLE PAS FRANCAIS, JE SUIS BARBARIEN
      LU=6  ! FORTRAN STANDARD OUPUT CHANNEL
      LI=5  ! FORTRAN STANDARD INPUT CHANNEL
!
!----------------------------------------------------------------------
! INTRODUCE YOURSELF
!
      WRITE(LU,*) ' '
      WRITE(LU,*) '+-------------------------------------------------+'
      WRITE(LU,*) '  PARTEL/PARRES: TELEMAC METISOLOGIC PARTITIONER'
      WRITE(LU,*) '                                                   '
      WRITE(LU,*) '  REBEKKA KOPMANN & JACEK A. JANKOWSKI (BAW)'
      WRITE(LU,*) '                 JEAN-MICHEL HERVOUET (LNHE)'
      WRITE(LU,*) '                 CHRISTOPHE DENIS     (SINETICS) '
      WRITE(LU,*) '                 YOANN AUDOUIN        (LNHE) '
      WRITE(LU,*) '  PARTEL (C) COPYRIGHT 2000-2002 '
      WRITE(LU,*) '  BUNDESANSTALT FUER WASSERBAU, KARLSRUHE'
      WRITE(LU,*) ' '
      WRITE(LU,*) '  METIS 5.0.2 (C) COPYRIGHT 2012 '
      WRITE(LU,*) '  REGENTS OF THE UNIVERSITY OF MINNESOTA '
      WRITE(LU,*) ' '
      WRITE(LU,*) '  BIEF ',VERSION,' (C) COPYRIGHT 2012 EDF'
      WRITE(LU,*) '+-------------------------------------------------+'
      WRITE(LU,*) ' '
      WRITE(LU,*) ' '
      WRITE(LU,*) '  MAXIMUM NUMBER OF PARTITIONS: ',MAXNPROC
      WRITE(LU,*) ' '
      WRITE(LU,*) '+--------------------------------------------------+'
      WRITE(LU,*) ' '
!
!----------------------------------------------------------------------
! NAMES OF THE INPUT FILES:
!
!
      WRITE(LU,*) '--INPUT FILE NAME <INPUT_NAME>: '
      READ(LI,*) NAMEINP
!
      IF (NAMEINP.EQ.' ') THEN
        WRITE (LU,*) ' NO FILENAME '
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) 'INPUT: ',TRIM(NAMEINP)
      ENDIF

      INQUIRE (FILE=NAMEINP,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*)' FILE DOES NOT EXIST: ',TRIM(NAMEINP)
        CALL PLANTE(1)
        STOP
      ENDIF

      IF (CODE(1:1).EQ.' ') THEN
        CODE = NAMEINP(1:3)
      ENDIF
!
      WRITE(LU,*)
     & '--INPUT FILE FORMAT <INPFORMAT> [MED,SERAFIN,SERAFIND]: '
      READ(LI,*) INPFORMAT
      IF ( (INPFORMAT .NE. 'MED     ') .AND.
     &     (INPFORMAT(1:7) .NE. 'SERAFIN') ) THEN
        WRITE(LU,*)
     &  ' FILE FORMAT MUST BE "MED" OR "SERAFIN" OR "SERAFIND" '
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) ' INPUT: ', INPFORMAT
      ENDIF
!
      WRITE(LU, *) '--BOUNDARY CONDITIONS FILE NAME: '
      READ(LI,*) NAMECLI
      IF (NAMECLI.EQ.' ') THEN
        WRITE (LU,*) ' NO FILENAME '
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) 'INPUT: ',TRIM(NAMECLI)
      ENDIF
!
      INQUIRE (FILE=NAMECLI,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ',TRIM(NAMECLI)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      WRITE(LU,FMT='(A,I6,A)')
     &  '--NUMBER OF PARTITIONS <NPARTS> [2 -',MAXNPROC,']: '
      READ(LI,*) NPARTS
!
      IF ( (NPARTS > MAXNPROC) .OR. (NPARTS < 2) ) THEN
        WRITE(LU,FMT='(A,I6,A)')
     &  ' NUMBER OF PARTITIONS MUST BE IN [2 -',MAXNPROC,']'
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*)' INPUT: ', NPARTS
      ENDIF
!
      WRITE(LU,*) ' PARTITIONING METHOD <PMETHOD>
     &  [1 (METIS) OR 2 (SCOTCH)]: '
      READ(LI,*) PMETHOD
      IF ( (PMETHOD > 2) .OR. (PMETHOD < 1) ) THEN
        WRITE(LU,
     &  '('' PARTITIONING METHOD MUST BE 1 OR 2'')')
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) '--INPUT: ', PMETHOD
      ENDIF

!
! #### THE SECTIONS FILE NAME
!
      NAMESEC = ' '
      WRITE(LU,*)'--CONTROL SECTIONS FILE NAME (OR RETURN) : '
      READ(LI,'(A)') NAMESEC
!
      IF(NAMESEC(1:1) .NE. ' ') THEN
        WRITE(LU,*) 'INPUT: ',TRIM(NAMESEC)
        INQUIRE (FILE=NAMESEC,EXIST=IS)
        IF (.NOT.IS) THEN
          WRITE (LU,*) ' FILE DOES NOT EXIST: ',TRIM(NAMESEC)
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        WRITE(LU,*) ' NO SECTIONS '
      ENDIF
!
! #### THE ZONES FILE NAME
!
      NAMEZFI = ' '
      WRITE(LU,*) '--CONTROL ZONES FILE NAME (OR RETURN) : '
      READ(LI,'(A)') NAMEZFI
!
      IF(NAMEZFI(1:1) .NE. ' ') THEN
        WRITE(LU,*) 'INPUT: ',TRIM(NAMEZFI)
        INQUIRE (FILE=NAMEZFI,EXIST=IS)
        IF (.NOT.IS) THEN
          WRITE (LU,*) ' FILE DOES NOT EXIST: ', TRIM(NAMEZFI)
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        WRITE(LU,*) ' NO ZONES '
      ENDIF
!
! #### THE WEIRS FILE NAME
!
      NAMESEU = ' '
      WRITE(LU,*) '--WEIR FILE NAME (OR RETURN) : '
      READ(LI,'(A)') NAMESEU
!
      IF(NAMESEU(1:1) .NE. ' ') THEN
        WRITE(LU,*) 'INPUT: ',TRIM(NAMESEU)
        INQUIRE (FILE=NAMESEU,EXIST=IS)
        IF (.NOT.IS) THEN
          WRITE (LU,*) ' FILE DOES NOT EXIST: ', TRIM(NAMESEU)
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        WRITE(LU,*) ' NO WEIRS '
      ENDIF
!
!     Geometry file
!
      WRITE(LU,*) '--GEOMETRY FILE NAME <INPUT_NAME>: '
      READ(LI,*) NAMEGEO
!
      IF (NAMEGEO.EQ.' ') THEN
        WRITE (LU,*) ' NO FILENAME '
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) 'INPUT: ',TRIM(NAMEGEO)
      ENDIF

      INQUIRE (FILE=NAMEGEO,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*)' FILE DOES NOT EXIST: ',TRIM(NAMEGEO)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      WRITE(LU,*)
     & '--GEOMETRY FILE FORMAT <GEOFORMAT> [MED,SERAFIN,SERAFIND]: '
      READ(LI,*) GEOFORMAT
      IF ( (GEOFORMAT .NE. 'MED     ') .AND.
     &     (GEOFORMAT(1:7) .NE. 'SERAFIN') ) THEN
        WRITE(LU,*)
     &  ' FILE FORMAT MUST BE "MED" OR "SERAFIN" OR "SERAFIND" '
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) ' INPUT: ', GEOFORMAT
      ENDIF
!
!     Concatenate files
!
      WRITE(LU,*) '--CONCATENATE FILES <YES-NO>: '
      READ(LI,*) CONCATSTR
!
      IF (CONCATSTR.NE.'YES'.AND.CONCATSTR.NE.'NO') THEN
        WRITE (LU,*) 'DONT KNOW IF I SHOUD CONCATENATE:'
     &               //TRIM(CONCATSTR)
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) 'CONCATENATE: ',TRIM(CONCATSTR)
        IF(CONCATSTR=='YES')THEN
          PARTEL_CONCAT=.TRUE.
        ELSE
          PARTEL_CONCAT=.FALSE.
        ENDIF
      ENDIF
!
      ! Check if the geometry has already been partitionned
      IF(PARTEL_CONCAT)THEN
        INQUIRE(FILE=TRIM(NAMEGEO)//'-CONCAT', EXIST=RES_ONLY)
      ELSE
        INQUIRE(FILE=TRIM(NAMEGEO)//EXTENS(NPARTS-1,0), EXIST=RES_ONLY)
      ENDIF
      IF(NAMEGEO.EQ.NAMEINP) RES_ONLY = .FALSE.

!
! FIND THE INPUT FILE CORE NAME LENGTH
!
      I_LENINP = LEN(TRIM(NAMEINP))
!
      IF (I_LENINP > PATH_LEN) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) 'ATTENTION:'
        WRITE(LU,*) 'THE NAME OF THE INPUT FILE:'
        WRITE(LU,*) NAMEINP
        WRITE(LU,*) 'IS LONGER THAN ',PATH_LEN,' CHARACTERS'
        WRITE(LU,*) 'WHICH IS THE LONGEST APPLICABLE NAME FOR TELEMAC '
        WRITE(LU,*) 'INPUT AND OUTPUT FILES. STOPPED. '
        CALL PLANTE(1)
        STOP
      ENDIF
!
! CORE NAME LENGTH
!
      I_LENCLI = LEN(TRIM(NAMECLI))
!
      IF (I_LENCLI > PATH_LEN) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) 'ATTENTION:'
        WRITE(LU,*) 'THE NAME OF THE BOUNDARY CONDITIONS FILE:'
        WRITE(LU,*) NAMECLI
        WRITE(LU,*) 'IS LONGER THAN ',PATH_LEN,' CHARACTERS'
        WRITE(LU,*) 'WHICH IS THE LONGEST APPLICABLE NAME FOR TELEMAC '
        WRITE(LU,*) 'INPUT AND OUTPUT FILES. STOPPED. '
        CALL PLANTE(1)
        STOP
      ENDIF
!
! FIND THE GEOMETRY FILE CORE NAME LENGTH
!
      I_LENGEO = LEN(TRIM(NAMEGEO))
!
      IF (I_LENGEO > PATH_LEN) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) 'ATTENTION:'
        WRITE(LU,*) 'THE NAME OF THE GEOMETRY FILE:'
        WRITE(LU,*) NAMEGEO
        WRITE(LU,*) 'IS LONGER THAN ',PATH_LEN,' CHARACTERS'
        WRITE(LU,*) 'WHICH IS THE LONGEST APPLICABLE NAME FOR TELEMAC '
        WRITE(LU,*) 'INPUT AND OUTPUT FILES. STOPPED. '
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(RES_ONLY) THEN
        ! Reading geometry from xxxGEO
        CALL PARRES(NAMEGEO, NAMEINP, NPARTS, GEOFORMAT, INPFORMAT)
      ELSE
        CALL PARTEL(NAMEINP, NAMECLI, NPARTS, PMETHOD, INPFORMAT,
     &              NAMESEC, NAMEZFI, NAMESEU)
      ENDIF
!
      STOP 0
      END PROGRAM
