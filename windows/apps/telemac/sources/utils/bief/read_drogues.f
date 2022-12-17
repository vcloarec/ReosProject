!                   ***********************
                    SUBROUTINE READ_DROGUES
!                   ***********************
!
     &( NP,NP_MAX,XP,YP,ZP,TAGP,CLSP,ELTP,SHPP,
     &  NPOIN2,NPOIN3,NELEM,NELMAX,IKLE,X,Y,T2DPLO )
!
!***********************************************************************
! BIEF   V8P0
!***********************************************************************
!
!brief    Reads the results for a given time step from a drogues file.
!+        The drogues file will have to have been created previously
!+        by the TELEMAC system
!
!history  S.E. BOURBAN (HRW)
!+        21/08/2018
!+        V8P0
!+        New subroutine created to allow for continuous simulation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CLSP           |<--| CLASS OF DROGUES
!| ELTP           |<--| ELEMENT FOR EACH DROGUE
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN THE MESH
!| NP             |<--| NUMBER OF DROGUES
!| NPOIN2         |-->| NUMBER OF POINTS OF 2D MESH
!| NPOIN3         |-->| NUMBER OF POINTS OF 3D MESH
!| NP_MAX         |-->| MAXIMUM NUMBER OF DROGUES
!| SHPP           |<--| BARYCENTRIC COORDINATES OF DROGUES IN THEIR
!|                |   | ELEMENTS.
!| TAGP           |<--| TAG OF DROGUES
!| T2DPLO         |-->| PREVIOUS DROGUES COMPUTATION FILE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XP             |<--| ABSCISSAE OF DROGUES
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YP             |<--| ORDINATES OF DROGUES
!| ZP             |<--| ELEVATIONS OF DROGUES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INITIAL_DROGUES, ONLY : NDRG_TAGS
      USE INTERFACE_PARALLEL, ONLY : P_MAX
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_FILE) , INTENT(IN) :: T2DPLO
      INTEGER         , INTENT(IN)    :: NP_MAX,NELEM,NELMAX
      INTEGER         , INTENT(IN)    :: NPOIN2,NPOIN3
      INTEGER         , INTENT(INOUT) :: NP
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3)
      INTEGER, INTENT(INOUT)          :: TAGP(NP_MAX),ELTP(NP_MAX)
      INTEGER, INTENT(INOUT)          :: CLSP(NP_MAX)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: XP(NP_MAX),YP(NP_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPP(3,NP_MAX),ZP(NP_MAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, PARAMETER      :: SIZELIGN = 3000
      CHARACTER(LEN=SIZELIGN) :: LIGNE
!
      INTEGER          LUPLO, I,J,ILINE,ICHAR, ITIME(9), IFLOT,MFLOT
      INTEGER           JPID, N1,N2,N3
      DOUBLE PRECISION  X0,Y0,X1,Y1,X2,Y2,X3,Y3,DET1,DET2,DET3,SURDET
      REAL              :: RVALUE
      REAL, ALLOCATABLE :: RARRAY(:)
      DOUBLE PRECISION, PARAMETER :: CHOUIA = 1.D-10
!
!-----------------------------------------------------------------------
!
!     HEADER OF BLUE KENUE PARCEL FILE IN ASCII FORM
!
      IF( T2DPLO%FMT .EQ.'BKBINPCL' .AND. T2DPLO%LU.NE.0 ) THEN
        LUPLO = T2DPLO%LU
        CLOSE(LUPLO)
!
!-----------------------------------------------------------------------
!
!     > READING THE HEADER OF THE DROGUES FILE
!
        OPEN( LUPLO, FILE=TRIM(T2DPLO%TELNAME),FORM='FORMATTED',
     &        ACCESS='STREAM', ACTION='READ' )
        ILINE = 0
        ICHAR = 0
        REWIND(LUPLO)
!       COUNTS COMMENTS FOR NOW (TODO: RECORD ATTRIBUTES)
        DO WHILE( .TRUE. )
          ILINE = ILINE + 1
          READ( LUPLO,FMT='(A)',END=1001,ERR=1901 ) LIGNE
          ICHAR = ICHAR + LEN(TRIM(LIGNE)) + 2
          IF( LIGNE(1:10).EQ.':EndHeader' ) EXIT
        ENDDO
        CLOSE(LUPLO)
!
!       CORE OF BLUE KENUE PARCEL FILE IN BINARY LITTLE ENDIAN FORM
        OPEN( LUPLO, FILE=TRIM(T2DPLO%TELNAME),FORM='UNFORMATTED',
     &        ACTION='READ',CONVERT='LITTLE_ENDIAN',
     &        ACCESS='STREAM', STATUS='OLD' )
!
!       LOOKING FOR THE MAXIMUM NUMBER OF DROGUES FOR MEMORY ALLOCATION
        MFLOT = 0
        ILINE = 0
        REWIND(LUPLO)
        READ(LUPLO) LIGNE(1:ICHAR)
        DO WHILE( .TRUE. )
          READ(LUPLO,END=23,ERR=1902) ITIME
          ILINE = ILINE + 1
          READ(LUPLO,ERR=1902) IFLOT
          MFLOT = MAX( MFLOT,IFLOT )
          READ(LUPLO) ( RVALUE, I = 1,IFLOT*4 )
          IF( NPOIN2.NE.NPOIN3 ) THEN
            READ(LUPLO) ( RVALUE, I = 1,IFLOT )
          ENDIF
          READ(LUPLO) IFLOT
        ENDDO
 23     CONTINUE
!
!       CHECK AGAINST NUMBER OF PARCELS POSSIBLE
        IF( MFLOT.GT.NP_MAX ) THEN
          WRITE(LU,33) MFLOT,NP_MAX
 33       FORMAT(1X,'READ_DROGUES:',/,
     &    1X,'     REQUIRED NUMBER OF DROGUES (',I8,')',/,
     &    1X,'     LARGER THAN THE MAXIMUM NUMBER OF DROGUES',/,
     &    1X,'     POSSIBLE (',I8,')',/,
     &    1X,'     INCREASE THE MAXIMUM OR REDUCE YOUR DENSITY.')
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
!     > READING ALL RECORDS BUT THE LAST ONE
!
        ALLOCATE( RARRAY(MFLOT) )
        REWIND(LUPLO)
!
        READ(LUPLO) LIGNE(1:ICHAR)
        DO J = 1,ILINE-1
          READ(LUPLO) ITIME
          READ(LUPLO) IFLOT
          READ(LUPLO) ( RVALUE, I = 1,IFLOT*4 )
          IF( NPOIN2.NE.NPOIN3 ) THEN
            READ(LUPLO) ( RVALUE, I = 1,IFLOT )
          ENDIF
          READ(LUPLO) IFLOT
        ENDDO
!
!-----------------------------------------------------------------------
!
!     > READING THE LAST RECORD
!
        READ(LUPLO) ITIME
        READ(LUPLO) IFLOT
!       X
        READ(LUPLO) ( RARRAY(I), I = 1,IFLOT )
        DO I = 1,IFLOT
          XP(I) = RARRAY(I)
        ENDDO
!       Y
        READ(LUPLO) ( RARRAY(I), I = 1,IFLOT )
        DO I = 1,IFLOT
          YP(I) = RARRAY(I)
        ENDDO
!       Z
        IF( NPOIN2.NE.NPOIN3 ) THEN
          READ(LUPLO) ( RARRAY(I), I = 1,IFLOT )
          DO I = 1,IFLOT
            ZP(I) = RARRAY(I)
          ENDDO
        ENDIF
!       TAG
        READ(LUPLO) ( RARRAY(I), I = 1,IFLOT )
        DO I = 1,IFLOT
          TAGP(I) = INT( RARRAY(I) )
        ENDDO
!       CLASS
        READ(LUPLO) ( RARRAY(I), I = 1,IFLOT )
        DO I = 1,IFLOT
          CLSP(I) = INT( RARRAY(I) )
        ENDDO
!
        DEALLOCATE(RARRAY)
        CLOSE(LUPLO)
!
!-----------------------------------------------------------------------
!
!     > POSITIONING THE DORGUES WITHIN THE MESH
!
        NP = 0
        IF( IFLOT.GT.0 ) THEN
          DO I = 1,IFLOT
!
            X0 = XP(I)
            Y0 = YP(I)
!
!       IDENTIFYING THE TRIANGLE WHERE THE PARCELS FELL IN
!       (FOR EACH PROCESSOR, AND IT MAY BE ON THE EDGE OF TWO OR MORE)
!
            JPID = -1
            DO J = 1,NELEM
!
              N1 = IKLE( J,1 )
              N2 = IKLE( J,2 )
              N3 = IKLE( J,3 )
              X1 = X(N1)
              Y1 = Y(N1)
              X2 = X(N2)
              Y2 = Y(N2)
              X3 = X(N3)
              Y3 = Y(N3)
!
              DET1 = ( X3-X2 )*( Y0-Y2 ) - ( Y3-Y2 )*( X0-X2 )
              DET2 = ( X1-X3 )*( Y0-Y3 ) - ( Y1-Y3 )*( X0-X3 )
              DET3 = ( X2-X1 )*( Y0-Y1 ) - ( Y2-Y1 )*( X0-X1 )
              IF( DET1.GE.-CHOUIA .AND.
     &            DET2.GE.-CHOUIA .AND.
     &            DET3.GE.-CHOUIA ) THEN
                JPID = IPID
                EXIT
              ENDIF
!
            ENDDO
!
!        /!\ IT IS IMPORTANT TO NOTE THAT NP .LE. IFLOT
!
            IF( NCSIZE.GT.1 ) THEN
              JPID = P_MAX(JPID)
            ENDIF
            IF( JPID.EQ.IPID ) THEN
              NP = NP + 1
              XP(NP) = X0
              YP(NP) = Y0
              ELTP(NP) = J
              SURDET = 1.D0 / ( (X2-X1)*(Y3-Y1) - (X3-X1)*(Y2-Y1) )
              SHPP(1,NP) = DET1*SURDET
              SHPP(2,NP) = DET2*SURDET
              SHPP(3,NP) = DET3*SURDET
              TAGP(NP) = TAGP(I)
              CLSP(NP) = CLSP(I)
            ENDIF
!
          ENDDO
        ENDIF
!
        NDRG_TAGS = NP
        IF( NCSIZE.GT.1 ) NDRG_TAGS = P_MAX(NDRG_TAGS)
        WRITE(LU,34) NP
 34     FORMAT(1X,'READ_DROGUES:',/,I8,
     &    1X,'     DROGUES HAVE BEEN READ FROM THE',/,
     &    1X,'     PREVIOUS DROGUES FILE.')
!
!-----------------------------------------------------------------------
!
      ENDIF
      RETURN
!
!-----------------------------------------------------------------------
!
 1001 CONTINUE
      WRITE(LU,*) LIGNE
      WRITE(LU,*) 'REACHED THE END OF THE DROGUES FILE'
      WRITE(LU,*) 'BEFORE FINDING THE END OF THE HEADER.'
      WRITE(LU,*) 'MAYBE THIS IS NOT A DROGUES FILE.'
      CALL PLANTE(1)
      STOP
 1901 CONTINUE
      WRITE(LU,*) 'READING ERROR ON THE DROGUES FILE'
      WRITE(LU,*) 'AT LINE OF DATA : ',ILINE
      CALL PLANTE(1)
      STOP
 1902 CONTINUE
      WRITE(LU,*) 'READING ERROR ON THE DROGUES FILE'
      WRITE(LU,*) 'AFTER RECORD : ',ITIME
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
