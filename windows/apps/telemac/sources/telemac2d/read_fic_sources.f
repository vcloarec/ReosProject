!                   ***************************
                    SUBROUTINE READ_FIC_SOURCES
!                   ***************************
!
     &( Q , WHAT , AT , NFIC , LISTIN , FOUND)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   07/10/2011
!***********************************************************************
!
!brief    READS AND INTERPOLATES VALUES IN THE SOURCE FILE.
!
!note     IMPORTANT: THIS SUBROUTINE IS A COPY OF
!+            SUBROUTINE READ_FIC_FRLIQ BECAUSE IT USES THE SAME
!+            FILE FORMAT (LISTING MESSAGES ONLY ARE CHANGED).
!+            THE ONLY DIFFERENCE IS THAT
!+            THE ALLOCATABLE ARRAYS TIME_RFS AND INFIC_RFS WILL HERE
!+            STORE DIFFERENT DATA.
!+
!note     THE PROBLEM IS : WHERE TO STORE THESE DATA BECAUSE
!+            THESE ROUTINES MAY BE CALLED BY TELEMAC-2D OR 3D
!+            A SPECIFIC MODULE COULD BE DONE
!
!history  J-M HERVOUET (LNHE)
!+        17/03/2004
!+        V5P9
!+
!
!history  J-M HERVOUET (LNHE)
!+        28/06/2010
!+        V6P0
!+   SIZE OF LINE PARAMETERIZED (SEE SIZELIGN)
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
!history  C. COULET (ARTELIA GROUP)
!+        07/10/2011
!+        V6P2
!+   Modification size WHAT and CHOIX_RFS due to modification of TRACER
!+    numbering TRACER is now identified by 2 values (Isource, Itracer)
!+   So MAXVAL is now equal to MAXSCE+MAXSCE*MAXTRA
!
!history  U.H.Merkel
!+        17/07/2012
!+        V6P2
!+   NAG: MAXVAL intrinsic! -> MAXVALUE_RFS
!
!history  J-M HERVOUET (LNHE)
!+        13/12/2012
!+        V6P3
!+   Now works with tabs as well as spaces as delimiters
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME_RFS IN SECONDS
!| LISTIN         |-->| IF YES, PRINTS INFORMATION
!| NFIC           |-->| LOGICAL UNIT OF FILE
!| Q              |<--| VARIABLE READ AND INTERPOLATED
!| FOUND          |<--| IF FALSE: VARIABLE NOT FOUND
!| WHAT           |-->| VARIABLE TO LOOK FOR IN 9 CHARACTERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC2D, ONLY : DEJA_RFS,INFIC_RFS,TIME_RFS,
     &                                   CHOIX_RFS,IL1_RFS,IL2_RFS,
     &                                   TL1_RFS,TL2_RFS,NVALUE_RFS,
     &                                   LASTWHAT_RFS,LASTAT_RFS,
     &                                   NLIG_RFS, MAXVALUE_RFS,
     &                                   SRC_REF_DATE, SRC_TEL_OFFSET
      USE BIEF, ONLY : DATE_MJD2SEC
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=9), INTENT(IN)       :: WHAT
      DOUBLE PRECISION, INTENT(IN)       :: AT
      DOUBLE PRECISION, INTENT(INOUT)    :: Q
      INTEGER         , INTENT(IN)       :: NFIC
      LOGICAL         , INTENT(IN)       :: LISTIN
      LOGICAL         , INTENT(OUT)      :: FOUND
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     MAXIMUM NUMBER OF CHARACTERS PER LIGN (MAY BE CHANGED)
!
      INTEGER, PARAMETER :: SIZELIGN = 3000
!
      INTEGER IVALUE,ILIG,OK,J,IWHAT,IDEB,IFIN
      DOUBLE PRECISION TETA
!
      CHARACTER(LEN=SIZELIGN) :: LIGNE
!
      DOUBLE PRECISION, PARAMETER :: TOL = 1.D-3
!
      DOUBLE PRECISION OFFSET
      DOUBLE PRECISION AT_DATE
!
      INTRINSIC ABS,CHAR
!
!-----------------------------------------------------------------------
!
!     1) (AT FIRST CALL)
!        READS THE SOURCE FILE
!        INITIALISES CURRENT LINES AND INTERVAL OF TIME_RFS
!
      IF(.NOT.DEJA_RFS) THEN
        REWIND(NFIC)
!       SKIPS COMMENTS
1       READ(NFIC,FMT='(A)') LIGNE
        IF(LIGNE(1:8).EQ.'#REFDATE') THEN
          CALL READ_REF_DATE(LIGNE, SRC_REF_DATE)
          IF (TEL_OFFSET.LE.1.D-16) THEN
            WRITE(LU,*) 'REFERENCE DATE IN SOURCE FILE'
            WRITE(LU,*) 'MISSING ORIGINAL DATE OF TIME IN STEERING FILE'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
        IF(LIGNE(1:1).EQ.'#') GO TO 1
!
!       FINDS OUT WHAT AND HOW MANY VALUES ARE GIVEN IN THE FILE
!
        NVALUE_RFS = -1
        IFIN = 1
40      IDEB = IFIN
!
!       IDENTIFIES FIRST CHARACTER OF NAME
!
!       SKIPPING SPACES AND TABS
50      IF((LIGNE(IDEB:IDEB).EQ.' '.OR.LIGNE(IDEB:IDEB).EQ.CHAR(9))
     &     .AND.IDEB.LT.SIZELIGN) THEN
          IDEB=IDEB+1
          GO TO 50
        ENDIF
!       IDENTIFIES LAST CHARACTER OF NAME
        IFIN = IDEB
60      IF(LIGNE(IFIN:IFIN).NE.' '.AND.LIGNE(IFIN:IFIN).NE.CHAR(9)
     &     .AND.IFIN.LT.SIZELIGN) THEN
          IFIN=IFIN+1
          GO TO 60
        ENDIF
!
        IF(IDEB.EQ.IFIN) GO TO 4
!
        NVALUE_RFS = NVALUE_RFS + 1
        IF(NVALUE_RFS.EQ.0) THEN
          IF(LIGNE(IDEB:IFIN-1).NE.'T') THEN
            WRITE(LU,*) 'FIRST VALUE MUST BE TIME_RFS, DENOTED T'
            WRITE(LU,*) 'IN SOURCES FILE'
            CALL PLANTE(1)
            STOP
          ENDIF
        ELSEIF(NVALUE_RFS.LE.MAXVALUE_RFS) THEN
          CHOIX_RFS(NVALUE_RFS)='         '
          CHOIX_RFS(NVALUE_RFS)(1:IFIN-IDEB+1)=LIGNE(IDEB:IFIN-1)
        ELSE
          WRITE(LU,*)
     &        'INCREASE MAXVALUE_RFS IN DECLARATIONS_TELEMAC2D'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(IFIN.LT.SIZELIGN) GO TO 40
!
!       SKIPS THE LINE WITH UNITS OR NAMES
4       READ(NFIC,FMT='(A)') LIGNE
        IF(LIGNE(1:1).EQ.'#') GO TO 4
!
!       COUNTS LINES OF DATA
        NLIG_RFS = 0
998     READ(NFIC,*,END=1000,ERR=999) LIGNE
        IF(LIGNE(1:1).NE.'#') NLIG_RFS=NLIG_RFS+1
        GO TO 998
999     CONTINUE
        WRITE(LU,*) 'READING ERROR ON THE SOURCES FILE'
        WRITE(LU,*) 'AT LINE OF DATA : ',NLIG_RFS
        WRITE(LU,*) '(COMMENTS EXCLUDED)'
        CALL PLANTE(1)
        STOP
1000    CONTINUE
!
!       DYNAMICALLY ALLOCATES TIME_RFS AND INFIC_RFS
!
        ALLOCATE(TIME_RFS(NLIG_RFS),STAT=OK)
        IF(OK.NE.0) WRITE(LU,*) 'MEMORY ALLOCATION ERROR FOR TIME_RFS'
        ALLOCATE(INFIC_RFS(NVALUE_RFS,NLIG_RFS),STAT=OK)
        IF(OK.NE.0) WRITE(LU,*) 'MEMORY ALLOCATION ERROR FOR INFIC_RFS'
!
!       FINAL READ OF TIME_RFS AND INFIC_RFS
!
        REWIND(NFIC)
!       SKIPS COMMENTS AND FIRST TWO MANDATORY LINES
2       READ(NFIC,FMT='(A)') LIGNE
        IF(LIGNE(1:1).EQ.'#') GO TO 2
        READ(NFIC,FMT='(A)') LIGNE
!
        DO ILIG=1,NLIG_RFS
3         READ(NFIC,FMT='(A)') LIGNE
          IF(LIGNE(1:1).EQ.'#') THEN
            GO TO 3
          ELSE
            BACKSPACE(NFIC)
            READ(NFIC,*) TIME_RFS(ILIG),
     &                   (INFIC_RFS(IVALUE,ILIG),IVALUE=1,NVALUE_RFS)
          ENDIF
        ENDDO
!
        CLOSE(NFIC)
        DEJA_RFS = .TRUE.
        ! If reference date is available adding offset (from date in
        ! file)
        IF (ANY(SRC_REF_DATE.NE.0)) THEN
          OFFSET = DATE_MJD2SEC(SRC_REF_DATE(1:3), SRC_REF_DATE(4:6))
          TIME_RFS = TIME_RFS + OFFSET
          SRC_TEL_OFFSET = TEL_OFFSET
          WRITE(LU,*) 'USING REFERENCE DATE FOR SOURCES:'
          WRITE(LU,666) SRC_REF_DATE
666       FORMAT(5X,1I4,'-',1I0.2,'-',1I0.2,' ',
     &           1I0.2,':',1I0.2,':',1I0.2)
        ELSE
          SRC_TEL_OFFSET = 0.D0
        ENDIF
!
        IL1_RFS = 1
        IL2_RFS = 2
        TL1_RFS = TIME_RFS(1)
        TL2_RFS = TIME_RFS(2)
!
        LASTWHAT_RFS = '         '
        LASTAT_RFS = 0.D0
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     2) INTERPOLATES THE DATA TO GET THE CORRECT TIME_RFS
!
!     2.A) FINDS THE ADDRESS IN THE ARRAY OF STORED DATA
!
!     2.B) INTERPOLATES DATA OF THE ARRAY INFIC_RFS
!
!-----------------------------------------------------------------------
!
!
      AT_DATE = AT + SRC_TEL_OFFSET
!     WHICH VARIABLE ?
      IWHAT = 0
      DO J=1,NVALUE_RFS
        IF(WHAT.EQ.CHOIX_RFS(J)) IWHAT=J
      ENDDO
      IF(IWHAT.EQ.0) THEN
        FOUND=.FALSE.
        RETURN
      ENDIF
!
70    IF(AT_DATE.GE.TL1_RFS-TOL.AND.AT_DATE.LE.TL2_RFS+TOL) THEN
        TETA = (AT_DATE-TL1_RFS)/(TL2_RFS-TL1_RFS)
      ELSE
        DO J=1,NLIG_RFS-1
          IF(AT_DATE.GE.TIME_RFS(J)-TOL.AND.
     &       AT_DATE.LE.TIME_RFS(J+1)+TOL) THEN
            TL1_RFS=TIME_RFS(J)
            TL2_RFS=TIME_RFS(J+1)
            IL1_RFS=J
            IL2_RFS=J+1
            GO TO 70
          ENDIF
        ENDDO
        IL1_RFS=IL2_RFS
        IL2_RFS=IL2_RFS+1
        IF(IL2_RFS.GT.NLIG_RFS) THEN
          WRITE(LU,*) 'T=',AT_DATE,' OUT OF RANGE'
          WRITE(LU,*) 'OF THE SOURCES FILE'
          CALL PLANTE(1)
          STOP
        ENDIF
        TL1_RFS=TIME_RFS(IL1_RFS)
        TL2_RFS=TIME_RFS(IL2_RFS)
        GO TO 70
      ENDIF
!
      Q = (1.D0-TETA)*INFIC_RFS(IWHAT,IL1_RFS)
     &  +       TETA *INFIC_RFS(IWHAT,IL2_RFS)
!
      FOUND=.TRUE.
!
!     PRINTS ONLY IF NEW TIME_RFS OR NEW VALUE IS ASKED
!
      IF(LISTIN) THEN
        IF(ABS(AT_DATE-LASTAT_RFS).GT.TOL.OR.LASTWHAT_RFS.NE.WHAT) THEN
          WRITE(LU,*) 'SOURCES : ',WHAT,'=',Q
        ENDIF
      ENDIF
      LASTAT_RFS=AT_DATE
      LASTWHAT_RFS=WHAT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
