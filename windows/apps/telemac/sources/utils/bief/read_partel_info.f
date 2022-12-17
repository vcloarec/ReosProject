!                   ***************************
                    SUBROUTINE READ_PARTEL_INFO
!                   ***************************
!
     &(CODE,NPTFR,NUMLIQ,BOUNDARY_COLOUR,MESH)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    READS IFAPAR AND NACHB
!
!history  Y AUDOUIN (LNHE)
!+        25/05/2015
!+        V7P0
!+   First version.
!
!history  J. GRASSET (Daresbury Lab & EDF)
!+        01/05/2018
!+        Add code for managing concatenated PAR file
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CODE           |-->| 3 LETTER NAME OF THE CODE
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUMLIQ         |<->| NUMBER OF LIQUID BOUNDARY
!| BOUNDARY_COLOUR|<->| COLOUR OF BOUNDARY POINTS
!| MESH           |<->| MESH STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=3),   INTENT(IN)    :: CODE
      INTEGER,            INTENT(IN)    :: NPTFR
      INTEGER,            INTENT(INOUT) :: NUMLIQ(NPTFR)
      INTEGER,            INTENT(INOUT) :: BOUNDARY_COLOUR(NPTFR)
      TYPE(BIEF_MESH),    INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER PTIR,I,K,IF1,IF2,IF3,IF4,IF5,IF6,IF7
      INTEGER NPTFR_BND
      INTEGER NPAR, NPAR_IDX
      INTEGER OFFSET_BEGIN
      CHARACTER(LEN=PATH_LEN) NAMEPAR
      CHARACTER(LEN=PATH_LEN) NAMEPAR_IDX
      CHARACTER(LEN=11) EXTENS
      EXTERNAL EXTENS
!
!-----------------------------------------------------------------------
!
!  PARALLEL MODE : READS NPTIR AND NACHB
!
!-----------------------------------------------------------------------
!
      IF(PARTEL_CONCAT)THEN
        NAMEPAR = CODE//'PAR'//'-CONCAT'
        NAMEPAR_IDX = CODE//'PAR'//'-INDEX'
      ELSE
        NAMEPAR = CODE//'PAR'//EXTENS(NCSIZE-1,IPID)
      ENDIF
!     Look for an available unit
      CALL GET_FREE_ID(NPAR)
      OPEN(NPAR,FILE=NAMEPAR,STATUS='OLD',FORM='FORMATTED')
      REWIND(NPAR)
      IF(PARTEL_CONCAT)THEN
        CALL GET_FREE_ID(NPAR_IDX)
        OPEN(NPAR_IDX,FILE=NAMEPAR_IDX,ACTION='READ')
        !GET OUT OFFSET FROM THE INDEX FILE
        READ(NPAR_IDX,*)(OFFSET_BEGIN,K=0,IPID)
        CLOSE(NPAR_IDX)
        !MOVE TO THE BEGINING OF OUR PART
        DO K=1,OFFSET_BEGIN-1
          READ(NPAR,*)
        ENDDO
      ENDIF
!
      READ(NPAR,*) NPTFR_BND
      IF(NPTFR.NE.NPTFR_BND) THEN
        WRITE(LU,24) NPTFR_BND,NPTFR
24      FORMAT(1X,'READ_PARTEL_INFO: ERROR IN THE PARALLEL FILE,',
     &       /,9X,1I5,' LINES INSTEAD OF ',I5,' REQUESTED')
        CALL PLANTE(1)
        STOP
      ENDIF
      DO K=1,NPTFR_BND
        READ(NPAR,*) BOUNDARY_COLOUR(K),MESH%NBOR%I(K),MESH%ISEG%I(K),
     &               MESH%XSEG%R(K),MESH%YSEG%R(K),NUMLIQ(K)
      ENDDO
!
      READ(NPAR,*) PTIR
      IF(NPTIR.NE.PTIR) THEN
        WRITE(LU,152) NPTIR,PTIR
152     FORMAT(1X,'READ_PARTEL_INFO : DIFFERENCE BETWEEN GEOMETRY ',/,
     &         1X,'         AND BOUNDARY CONDITIONS'   ,/,1X,I6,
     &  ' INTERFACE POINTS IN GEOMETRY',/,1X,I6,
     &  ' INTERFACE POINTS IN CONPAR FILE')
      ENDIF
!     NACHB(NBMAXNSHARE,NPTIR), HERE NACHB(I,K)
!     HERE NACHB IS IN LOCAL NUMBERING
      IF(NPTIR.GT.0) THEN
        DO K=1,NPTIR
          READ(NPAR,*) (MESH%NACHB%I((K-1)*NBMAXNSHARE+I),
     &                          I=1,NBMAXNSHARE)
        ENDDO
      ENDIF
!
!     JAJ //// READS THE NEIGHBOURHOODS FOR HALO CELLS ALONG THE INTERFACES
!     FILLING PATTERN: IFAPAR(1:7,K), K=1:NHALO
!                      -> NHALO: NUMBER OF HALO CELLS IN THIS PARTITION
!
!     IFAPAR(1,K)   : HALO ELEMENT -LOCAL- NUMBER IN THIS PARTITION
!     IFAPAR(2:4,K) : PROCESSOR NUMBERS BEHIND THE 3 ELEMENT EDGES
!                     NUMBER FROM 0 TO NCSIZE-1
!     IFAPAR(5:7,K) : -LOCAL- ELEMENT NUMBERS BEHIND THE 3 EDGES
!                     IN THE NUMBERING OF PARTITIONS THEY BELONG TO
!     ACTUALLY, NOT ALL OF THAT IS REQUIRED AND CAN BE OPTIMISED
!     AFTER THE DEVELOPMENT STAGE IS OVER
!
!     IN TELEMAC, IFAPAR IS REORGANISED IN IFAPAR(6,NELEM2)
!                 AND INITIALISED TO 0 IN ALMESH
!
      READ(NPAR,*) NHALO
      IF(NHALO.GT.2*NPTIR) THEN ! SEE BIEF LIB, SUBROUTINE ALMESH
        WRITE(LU,*) ' => NHALO>2*NPTIR DETECTED IN BC FILE'
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(NHALO.GT.0) THEN
        DO K=1,NHALO
          READ(NPAR,*) IF1,IF2,IF3,IF4,IF5,IF6,IF7
!
!         CORRECTS A BUG (IN IFAPAR THERE IS A CONFUSION BETWEEN PROCESSOR 0
!                         AND LIQUID BOUNDARY BUT
!                         IN CASE OF LIQUID BOUNDARY, THE ELEMENT BEHIND
!                         IS GIVEN AS 0, SO BOTH CASES MAY BE DISTINGUISHED
!                         HERE ALL BOUNDARIES (LIQUID OR SOLID) ARE SET AT -1
!
          IF(IF5.EQ.0) IF2=-1
          IF(IF6.EQ.0) IF3=-1
          IF(IF7.EQ.0) IF4=-1
!
          MESH%IFAPAR%I(6*(IF1-1)+1)=IF2
          MESH%IFAPAR%I(6*(IF1-1)+2)=IF3
          MESH%IFAPAR%I(6*(IF1-1)+3)=IF4
          MESH%IFAPAR%I(6*(IF1-1)+4)=IF5
          MESH%IFAPAR%I(6*(IF1-1)+5)=IF6
          MESH%IFAPAR%I(6*(IF1-1)+6)=IF7
        ENDDO
      ENDIF
!
      CLOSE(NPAR)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
