!                         **********************
                          MODULE INITIAL_DROGUES
!                         **********************
!
!***********************************************************************
! TELEMAC V8P0
!***********************************************************************
!
!brief    Module containing all subroutines to deal with drogues in
!+        general including randomly sampling parcels, etc.
!+        Module variables are mainly used for generalised input and
!+        output access.
!
!history  S.E. BOURBAN (HRW)
!+        21/08/2018
!+        V8P0
!+        Initial developments - inspired from ALGAE_TRANSP
!+        by A.Joly (EDF)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY : BIEF_OBJ, BIEF_MESH, IPID,NCSIZE
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_SUM,P_MAX
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     SUBROUTINES MADE AVAILABLE
!      PUBLIC :: SAMPLE_WPOIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     MODULE VARIABLES
!     ( MAINLY USE FOR INPUT & OUTPUT AS A GENERALISATION OF THE
!       SPECIFCS VARIABLES INCLUDED IN MODULES DROGUES_* )
!
!     NUMBER OF DIFFERENT CLASSES OF DROGUES (ALL TYPES OF CLASSES)
      INTEGER :: NDRG_CLSS
!
!     INCREMENTAL HIGHEST TAG NUMBER FOR DROGUES
!     TAG NUMBERS ARE UNIQUE TO ALL PARCELS
      INTEGER :: NDRG_TAGS = 0
!
!     CLASS DEFINED FOR EACH NODE ON THE MESH
!     THE SIZE IS NELMAX, BOTH %I AND %R ARE ALLOCATED FOR FILE
!       EXCHANGES AND THESE ARE ALLOCATED IN POINT_*
      TYPE(BIEF_OBJ), TARGET :: NODCLSS
!
!     CLASS DEFINED FOR EACH PARCEL
!     THE SIZE IS NFLOT_MAX, BOTH %I AND %R ARE ALLOCATED FOR FILE
!       EXCHANGES AND THESE ARE ALLOCATED IN POINT
      TYPE(BIEF_OBJ), TARGET :: PARCLSS
!
!
!     DROGUES PROPERTIES, DIFFERENT FOR DIFFERENT TYPES OF DROGUES
!     - DRG_DENSITY: COUNTING DROGUES PER SURFACE AREA
!     - DRG_RELEASE: TIME TO RELEASE IN SECOND
      DOUBLE PRECISION, ALLOCATABLE :: DRG_DENSITY(:)
      DOUBLE PRECISION, ALLOCATABLE :: DRG_RELEASE(:)
!
!------------------------------------------------------------------------
!
      SAVE
!
      CONTAINS
!
!=======================================================================
!
!       1) RANDOM SAMPLING
!
!
!                   ***********************
                    SUBROUTINE SAMPLE_WPOIN
!                   ***********************
!
     &( NP,NP_MAX,NCLS,NTAG,XP,YP,TAGP,CLSP,ELTP,SHPP,DSTY,
     &  NPOIN,NELEM,NELMAX,IKLE,CLSN,X,Y )
!
!***********************************************************************
! TELEMAC V8P0
!***********************************************************************
!
!brief    Randomly sample parcels within mesh, around a set of points.
!+    The method goes through all points in the mesh where NODCLSS
!+      is not zero, and sample parts of the surrounding triangles.
!+    In order to be "fair" and to account for variable density and
!+      triangle areas, the sampling in first carried out on a sorted
!+      list (or cumulated required number of parcels per triangle).
!
!history  S.E. BOURBAN (HRW)
!+   21/08/2016
!+   V8P0
!+   Initial implementation
!
!history  M.S.TURNBULL (HRW)
!+   04/11/2019
!+   V8P2
!+   Corrections
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CLSN    |-->| CLASS AT NODES
!| CLSP    |<->| CLASS OF EACH DROGUE
!| DSTY    |-->| DROGUE DENSITY FOR EACH CLASS
!| ELTP    |<->| ELEMENT FOR EACH DROGUE
!| IKLE    |-->| CONNECTIVITY MATRIX
!| NELEM   |-->| NUMBER OF ELEMENTS IN IKLE
!| NELMAX  |-->| MAXIMUM NUMBER OF ELEMENTS IN IKLE
!| NCLS    |-->| NUMBER OF CLASSES
!| NP      |-->| NUMBER OF DROGUES TO BE CREATED
!| NPOIN   |-->| NUMBER OF NODES IN THE MESH
!| NP_MAX  |-->| MAXIMUM NUMBER OF DROGUES TO BE CREATED
!| NTAG    |<->| NUMBER OF TAGS
!| SHPP    |<->| BARYCENTRIC COORDINATES OF DROGUES
!| TAGP    |<->| TAG OF EACH DROGUE
!| X       |-->| ABSCISSAE OF POINTS IN THE MESH
!| XP      |<->| ABSCISSAE OF DROGUES
!| Y       |-->| ORDINATES OF POINTS IN THE MESH
!| YP      |-->| ORDINATES OF DROGUES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!***********************************************************************
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(INOUT)          :: NP,NTAG
      INTEGER, INTENT(IN)             :: NP_MAX,NELEM,NELMAX,NPOIN,NCLS
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3),CLSN(NPOIN)
      INTEGER, INTENT(INOUT)          :: TAGP(NP_MAX),CLSP(NP_MAX)
      INTEGER, INTENT(INOUT)          :: ELTP(NP_MAX)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),DSTY(NCLS)
      DOUBLE PRECISION, INTENT(INOUT) :: XP(NP_MAX),YP(NP_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPP(3,NP_MAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          I,J,K,IEL,NEL,IP,NJ, K1,K2,KI,N1,N2
      INTEGER          JPID,MT,NPI,NPP, ITAG,ICLS
      DOUBLE PRECISION R0,LENGTH,A
      DOUBLE PRECISION X1,Y1,X2,Y2,X3,Y3, DX1,DY1,DX2,DY2,DX3,DY3
!
      INTEGER, ALLOCATABLE          :: PPKLE(:),PELEM(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: AKLE(:)
!
!-----------------------------------------------------------------------
!
!     SET THE VARIABLE DROGUES' CLASS
      MT = 0
      DO I = 1,NPOIN
        MT = MAX( MT,CLSN(I) )
      ENDDO
      IF( NCSIZE.GT.1 ) MT = P_MAX( MT )
      IF( MT.NE.NCLS ) THEN
        WRITE(LU,22) MT,NCLS
 22     FORMAT(1X,'CONDIN_DROGUES:',/,
     &    1X,'     NUMBER OF CLASSES OF DROGUES READ FROM',/,
     &    1X,'     THE GEOMETRY FILE ',I8,' DIFFERENT FROM THE',/,
     &    1X,'     NUMBER SET IN THE CAS FILE ',I8)
        IF( MT.GT.NCLS ) THEN
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!     LOCAL MEMORY ALLOCATION - NUMBER OF TRIANGLES CONTAINING PARCELS
!
      NEL = 0
      DO J = 1,NELEM
        DO K = 1,3
          I = IKLE( J,K )
          IF( CLSN(I).GT.0 ) NEL = NEL + 1
        ENDDO
      ENDDO
      ALLOCATE( PELEM(2,NEL) )
      ALLOCATE( PPKLE(2*NEL) )
      ALLOCATE( AKLE(2*NEL) )
!
!     EVALUATE TOTAL NUMBER OF PARCELS REQUIRED FOR ONE PROCESSOR
!     BY COMPUTING THE CUMULATED SURFACE AREA WEIGHTED BY TARGET
!     PARCEL DENSITY
!
      IEL = 0
      A = 0.D0
      DO J = 1,NELEM
        DO K = 1,3
          I = IKLE( J,K )
          IF( CLSN(I).GT.0 ) THEN
            IEL = IEL + 1
            PELEM(1,IEL) = J
            PELEM(2,IEL) = K
!
            N1 = IKLE( J,MOD(K,3)+1 )
            N2 = IKLE( J,MOD(K+1,3)+1 )
!
            DX1 = ( X(I)-X(N1) )/2.D0
            DX2 = ( X(I)-X(N2) )/2.D0
            DY1 = ( Y(I)-Y(N1) )/2.D0
            DY2 = ( Y(I)-Y(N2) )/2.D0
            DX3 = ( 2.D0*X(I)-X(N1)-X(N2) )/3.D0
            DY3 = ( 2.D0*Y(I)-Y(N1)-Y(N2) )/3.D0
!
            AKLE(2*IEL-1) = A +
     &        DSTY(CLSN(I)) * ( DX1*DY3-DX3*DY1 )/2.D0
            A = AKLE(2*IEL-1)
            PPKLE(2*IEL-1) = 0
!
            AKLE(2*IEL) = A +
     &        DSTY(CLSN(I)) * ( DX3*DY2-DX2*DY3 )/2.D0
            A = AKLE(2*IEL)
            PPKLE(2*IEL) = 0
!
          ENDIF
        ENDDO
      ENDDO
!
!     TOTAL NUMBER OF PARCELS FOR CURRENT PROCESSOR
      NPI = INT(A)
      NPP = NPI + NP
      IF( NCSIZE.GT.1 ) NPP = P_SUM( NPP )
!     CHECK AGAINST NUMBER OF PARCELS POSSIBLE
      IF( NPP.GT.NP_MAX ) THEN
        WRITE(LU,32) NPP,NP_MAX
 32     FORMAT(1X,'DROGUES::SAMPLE_WPOIN:',/,
     &    1X,'     REQUIRED NUMBER OF DROGUES (',I8,')',/,
     &    1X,'     LARGER THAN THE MAXIMUM NUMBER OF DROGUES',/,
     &    1X,'     POSSIBLE (',I8,')',/,
     &    1X,'     INCREASE THE MAXIMUM OR REDUCE YOUR DENSITY.')
        CALL PLANTE(1)
        STOP
      ENDIF
      IF (NEL.GT.0) LENGTH = AKLE(2*NEL)
!
!         ALLOCATING UNIQUE TAG NUMBERS FOR EACH PROCESSOR
!
      IF( NCSIZE.GT.1 ) THEN
        NPP = 0
        DO JPID = 0,NCSIZE-1
          IF( JPID.EQ.IPID ) THEN
            NPP = NPP + NPI
            ITAG = NPP
          ENDIF
          NPP = P_MAX( NPP )
        ENDDO
        ITAG = ITAG - NPI
      ELSE
        ITAG = 0
      ENDIF
!
!-----------------------------------------------------------------------
!
!     WEIGHTED SAMPLING BY POINTS (SUB-ELEMENT TRIANGLES)
!
      DO IP = 1,NPI
!       RANDOM SAMPLE
        CALL RANDOM_NUMBER(R0)
        R0 = R0 * LENGTH
        IF( R0.LT.AKLE(1) ) THEN
!         SPECIAL CASE FOR 1ST QUADRILATERAL
          PPKLE(1) = PPKLE(1) + 1
        ELSE
!         DICHOTOMIE TO FIND WHERE THE PARCEL FALLS
          K1 = 1
          K2 = 2*NEL
          DO WHILE( .TRUE. )
            KI = INT((K1+K2)/2)
            IF( R0.GT.AKLE(KI) ) THEN
              K1 = KI
            ELSE
              K2 = KI
            ENDIF
            IF( K1+1.EQ.K2 ) EXIT
          ENDDO
          PPKLE(K2) = PPKLE(K2) + 1
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     SAMPLING AROUND WEIGHTED POINTS, ONE TRIANGLE AT A TIME
      IP = NP
      DO IEL = 1,NEL
!
        J = PELEM(1,IEL)
        K = PELEM(2,IEL)
        I = IKLE( J,K )
        ICLS = CLSN(I)
!
        N1 = IKLE( J,MOD(K,3)+1 )
        N2 = IKLE( J,MOD(K+1,3)+1 )
!
        X1 = ( X(I)+X(N1) )/2.D0
        Y1 = ( Y(I)+Y(N1) )/2.D0
        X2 = ( X(I)+X(N2) )/2.D0
        Y2 = ( Y(I)+Y(N2) )/2.D0
        X3 = ( X(I)+X(N1)+X(N2) )/3.D0
        Y3 = ( Y(I)+Y(N1)+Y(N2) )/3.D0
!
!       SAMPLE FIRST TRIANGLE
        NJ = PPKLE(2*IEL-1)
        CALL SAMPLE_TRIANGLE( IP+1,NJ,NP_MAX,J,ITAG,ICLS,
     &        TAGP,CLSP,ELTP,SHPP,
     &        XP,YP, X(I),Y(I),X1,Y1,X3,Y3 )
        IP = IP + NJ
!
!       SAMPLE SECOND TRIANGLE
        NJ = PPKLE(2*IEL)
        CALL SAMPLE_TRIANGLE( IP+1,NJ,NP_MAX,J,ITAG,ICLS,
     &        TAGP,CLSP,ELTP,SHPP,
     &        XP,YP, X(I),Y(I),X3,Y3,X2,Y2 )
        IP = IP + NJ
!
      ENDDO
      IF( NCSIZE.GT.1 ) ITAG = P_MAX( ITAG )
      NTAG = NTAG + ITAG
      NP = NP + NPI
!
!-----------------------------------------------------------------------
!
!     LOCAL MEMORY DE-ALLOCATION
!
      DEALLOCATE( PELEM )
      DEALLOCATE( PPKLE )
      DEALLOCATE( AKLE )
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE SAMPLE_WPOIN
!
!                 **************************
                  SUBROUTINE SAMPLE_TRIANGLE
!                 **************************
!
     &( IP,NP,NP_MAX,IELM,ITAG,ICLS,TAGP,CLSP,ELTP,SHPP,
     &  XP,YP,X1,Y1,X2,Y2,X3,Y3 )
!
!***********************************************************************
! TELEMAC V8P0
!***********************************************************************
!
!brief    Randomly sample parcels within a triangle.
!+    In order to be "fair" from a placement view point, R1 and R2
!+      are generated along the two edges P0P1 and P0P2 respectively,
!+      therefore covering a parallelogram with parcels (assuming
!+      the lengths of the edges P0P1 and P0P2 are not too different).
!+    Half of the parallelogram is then moved back into the triangle
!+      by symmetry with the mid-point between P1 and P2.
!
!history  S.E. BOURBAN (HRW)
!+   21/08/2016
!+   V8P0
!+   Initial implementation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CLSP    |<->| CLASS OF EACH DROGUE
!| ELTP    |<->| ELEMENT FOR EACH DROGUE
!| ICLS    |<->| CLASS TO BE ASSIGNED
!| IELM    |-->| ELEMENT TO BE ASSIGNED
!| IP      |-->| INDEX NUMBER OF FIRST DROGUE
!| ITAG    |-->| TAGGING RANGE FOR DROGUES PER PROCESSOR
!| NP      |-->| NUMBER OF DROGUES TO BE CREATED
!| NP_MAX  |-->| MAXIMUM NUMBER OF DROGUES TO BE CREATED
!| SHPP    |<->| BARYCENTRIC COORDINATES OF DROGUES
!| TAGP    |<->| TAG OF EACH DROGUE
!| XN,YN   |-->| WHERE N = 1,2,3, COORDINATES OF THE TRIANGLE WITHIN
!|         |---|   WHICH THE NEW NP DROGUES WILL BE RANDOMLY SELECTED
!| XP,YP   |<--| COORDINATES OF THE NP NEW (RANDOMLY SAMPLED) DROGUES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!***********************************************************************
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: IP,NP,NP_MAX, IELM
      INTEGER, INTENT(INOUT)          :: TAGP(NP_MAX),CLSP(NP_MAX)
      INTEGER, INTENT(INOUT)          :: ELTP(NP_MAX),ITAG,ICLS
      DOUBLE PRECISION, INTENT(INOUT) :: XP(NP_MAX),YP(NP_MAX)
      DOUBLE PRECISION, INTENT(IN)    :: X1,Y1,X2,Y2,X3,Y3
      DOUBLE PRECISION, INTENT(INOUT) :: SHPP(3,NP_MAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          I
      DOUBLE PRECISION R1,R2, SURDET, X0,Y0
!
!-----------------------------------------------------------------------
!
      DO I = IP,IP+NP-1
!
        CALL RANDOM_NUMBER(R1)
        CALL RANDOM_NUMBER(R2)
        XP(I) = X1 + R1*( X2-X1 ) + R2*( X3-X1 )
        YP(I) = Y1 + R1*( Y2-Y1 ) + R2*( Y3-Y1 )
        IF( R2 .GT. ( 1.D0-R1 ) ) THEN
          XP(I) = ( X3+X2 ) - XP(I)
          YP(I) = ( Y3+Y2 ) - YP(I)
        ENDIF
!
        X0 = XP(I)
        Y0 = YP(I)
        ELTP(I) = IELM
        SHPP(1,I) = ( X3-X2 )*( Y0-Y2 ) - ( Y3-Y2 )*( X0-X2 )
        SHPP(2,I) = ( X1-X3 )*( Y0-Y3 ) - ( Y1-Y3 )*( X0-X3 )
        SHPP(3,I) = ( X2-X1 )*( Y0-Y1 ) - ( Y2-Y1 )*( X0-X1 )
        SURDET = 1.D0 / ( ( X2-X1 )*( Y3-Y1 ) - ( X3-X1 )*( Y2-Y1 ) )
        SHPP(1,I) = SHPP(1,I) * SURDET
        SHPP(2,I) = SHPP(2,I) * SURDET
        SHPP(3,I) = SHPP(3,I) * SURDET
        ITAG = ITAG + 1
        TAGP(I) = ITAG
        CLSP(I) = ICLS
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE SAMPLE_TRIANGLE
!
!                 **************************
                  SUBROUTINE SAMPLE_POLYLINE
!                 **************************
!
     &( NP,NP_MAX,NCLS,NTAG, XP,YP,TAGP,CLSP,ELTP,SHPP,DSTY,
     &  NY,IY,VY,NG,XG,YG, NPOIN,NELEM,NELMAX,IKLE,X,Y )
!
!***********************************************************************
! TELEMAC V8P0
!***********************************************************************
!
!brief    Randomly sample parcels within a polygon defined by (XG,YG).
!+    The sampling method generate a parcel location within the bounds
!+      of the polygon, and then check if the point location is within
!+      the polygon.
!+    A better way to do it could be to use a tessalation of the
!+      polygon and then call on SAMPLE_WELEM
!
!history  S.E. BOURBAN (HRW)
!+   21/08/2016
!+   V8P0
!+   Initial implementation
!
!history  M.S.TURNBULL (HRW)
!+   21/11/2019
!+   V8P2
!+   Corrections
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CLSP    |<->| CLASS OF EACH DROGUE
!| DSTY    |-->| DROGUE DENSITY FOR EACH CLASS
!| ELTP    |<->| ELEMENT FOR EACH DROGUE
!| IKLE    |-->| CONNECTIVITY MATRIX
!| IY      |-->| ARRAYS OF NUMBER OF POINT PER POLYGONS
!| NCLS    |-->| NUMBER OF CLASSES
!| NELEM   |-->| NUMBER OF ELEMENTS IN IKLE
!| NELMAX  |-->| MAXIMUM NUMBER OF ELEMENTS IN IKLE
!| NG      |-->| NUMBER OF POLYGON VERTICES
!| NP      |-->| NUMBER OF DROGUES TO BE CREATED
!| NPOIN   |-->| NUMBER OF NODES IN THE MESH
!| NP_MAX  |-->| MAXIMUM NUMBER OF DROGUES TO BE CREATED
!| NTAG    |<->| NUMBER OF TAGS
!| NY      |-->| NUMBER OF POLYGONS
!| SHPP    |<->| BARYCENTRIC COORDINATES OF DROGUES
!| TAGP    |<->| TAG OF EACH DROGUE
!| X       |-->| ABSCISSAE OF POINTS IN THE MESH
!| XG,YG   |-->| COORDINATES OF THE VERTICES OF THE POLYGON WITHIN
!|         |---|   WHICH THE NEW NP DROGUESS WILL BE RANDOMLY SELECTED
!| XP,YP   |<--| COORDINATES OF THE NP NEW (RANDOMLY SAMPLED) DROGUES
!| VY      |-->| ARRAYS OF VALUES PER POLYGONS
!| Y       |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!***********************************************************************
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(INOUT)          :: NP,NTAG
      INTEGER, INTENT(IN)             :: NP_MAX,NELEM,NELMAX,NPOIN,NCLS
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3)
      INTEGER, INTENT(INOUT)          :: TAGP(NP_MAX),CLSP(NP_MAX)
      INTEGER, INTENT(INOUT)          :: ELTP(NP_MAX)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),DSTY(NCLS)
      DOUBLE PRECISION, INTENT(INOUT) :: XP(NP_MAX),YP(NP_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPP(3,NP_MAX)
      INTEGER, INTENT(IN)             :: NY,IY(NY), NG
      DOUBLE PRECISION, INTENT(IN)    :: VY(NY), XG(NG),YG(NG)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          I,J,K,IEL,NEL, N1,N2,N3, NPP, JPID, MT
      INTEGER          NG1,NG2, JY
      LOGICAL          FOUND
      DOUBLE PRECISION A, R1,R2, XA,YA,XI,YI, X0,Y0,X1,Y1,X2,Y2,X3,Y3
      DOUBLE PRECISION SURDET, DET1,DET2,DET3
      DOUBLE PRECISION, PARAMETER :: CHOUIA = 1.D-10
!
      INTEGER, ALLOCATABLE        :: PPKLE(:),PELEM(:)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL INPOLY
      EXTERNAL INPOLY
!
!-----------------------------------------------------------------------
!
!     SET THE VARIABLE DROGUES' CLASS
      MT = 0
      DO I = 1,NY
        MT = MAX( MT,INT(VY(I)) )
      ENDDO
      IF( NCSIZE.GT.1 ) MT = P_MAX( MT )
      IF( MT.NE.NCLS ) THEN
        WRITE(LU,22) MT,NCLS
 22     FORMAT(1X,'CONDIN_DROGUES:',/,
     &    1X,'     NUMBER OF CLASSES OF DROGUES READ FROM',/,
     &    1X,'     THE POLYGON FILE ',I8,' DIFFERENT FROM THE',/,
     &    1X,'     NUMBER SET IN THE CAS FILE ',I8)
        IF( MT.GT.NCLS ) THEN
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      ALLOCATE( PPKLE(NELEM) )
!
!-----------------------------------------------------------------------
!
      NG1 = 1
      DO JY = 1,NY
!       NUMBER OF POINTS IN THE JY POLYGON
        NG2 = INT( IY(JY) )
!
!-----------------------------------------------------------------------
!
!     EVALUATING THE NUMBER OF PARCELS WITHIN THE POLYGONE
!     (ACROSS ALL PROCESSORS)
!
!       SIZE AND AREA OF THE POLYGON
        A = ( XG(NG2)*YG(NG1) - XG(NG1)*YG(NG2) )
        XA = XG(NG2)
        YA = YG(NG2)
        XI = XG(NG2)
        YI = YG(NG2)
        DO I = NG1,NG2-1
          A = A + ( XG(I)*YG(I+1) - XG(I+1)*YG(I) )
          XA = MAX( XA,XG(I) )
          YA = MAX( YA,YG(I) )
          XI = MIN( XI,XG(I) )
          YI = MIN( YI,YG(I) )
        ENDDO
        A = A / 2.D0
!
!       NUMBER OF PARCELS GIVEN A SPATIAL DENSITY
        NPP = INT(A*DSTY(INT(VY(JY)))+1.D0)
!       CHECK AGAINST NUMBER OF PARCELS POSSIBLE
        IF( (NP+NPP).GT.NP_MAX ) THEN
          WRITE(LU,33) (NP+NPP),NP_MAX
 33       FORMAT(1X,'DROGUES::SAMPLE_POLYLINE:',/,
     &      1X,'     REQUIRED NUMBER OF DROGUES (',I8,')',/,
     &      1X,'     LARGER THAN THE MAXIMUM NUMBER OF DROGUES',/,
     &      1X,'     POSSIBLE (',I8,')',/,
     &      1X,'     INCREASE THE MAXIMUM OR REDUCE YOUR DENSITY.')
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
!     IDENTIFYING THOSE ELEMENTS COVERED BY THE POLYGON
!     (FOR EACH PROCESSOR)
!
!       LOCAL MEMORY ALLOCATION - NUMBER OF TRIANGLES COVERED
!
        NEL = 0
        DO J = 1,NELEM
          FOUND = .FALSE.
          PPKLE(J) = 0
          DO K = 1,3
            I = IKLE( J,K )
            FOUND = FOUND .OR.
     &        INPOLY( X(I),Y(I), XG(NG1:NG2),YG(NG1:NG2),NG2-NG1+1 )
          ENDDO
          IF( FOUND ) THEN
            NEL = NEL + 1
            PPKLE(J) = 1
          ENDIF
        ENDDO
!
        ALLOCATE( PELEM(NEL) )
!
        IEL = 0
        DO J = 1,NELEM
          IF( PPKLE(J).NE.0 ) THEN
            IEL = IEL + 1
            PELEM(IEL) = J
          ENDIF
        ENDDO
!
!-----------------------------------------------------------------------
!
!     SAMPLING THE POLYGON USING A HIT AND MISS STRATEGY
!
        DO I = 1,NPP
!
!       SAMPLING OVER THE ENTIRE POLYGON
          FOUND = .FALSE.
          DO WHILE( .NOT.FOUND )
            IF (NCSIZE.GT.1) THEN
              X0 = -1.D20
              Y0 = -1.D20
              IF (IPID.EQ.0) THEN
                CALL RANDOM_NUMBER(R1)
                CALL RANDOM_NUMBER(R2)
                X0 = XI + R1*( XA-XI )
                Y0 = YI + R2*( YA-YI )
              ENDIF
              X0 = P_MAX(X0)
              Y0 = P_MAX(Y0)
            ELSE
              CALL RANDOM_NUMBER(R1)
              CALL RANDOM_NUMBER(R2)
              X0 = XI + R1*( XA-XI )
              Y0 = YI + R2*( YA-YI )
            ENDIF
            FOUND = INPOLY( X0,Y0, XG(NG1:NG2),YG(NG1:NG2),NG2-NG1+1 )
          ENDDO
!
!       IDENTIFYING THE TRIANGLE WHERE THE PARCELS FELL IN
!       (FOR EACH PROCESSOR, AND IT MAY BE ON THE EDGE OF TWO OR MORE)
!
          JPID = -1
          DO IEL = 1,NEL
!
            J = PELEM(IEL)
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
     &          DET2.GE.-CHOUIA .AND.
     &          DET3.GE.-CHOUIA ) THEN
              JPID = IPID
              EXIT
            ENDIF
!
          ENDDO
!
          IF( NCSIZE.GT.1 ) THEN
            JPID = P_MAX(JPID)
            NTAG = P_MAX(NTAG)
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
            NTAG = NTAG + 1
            TAGP(NP) = NTAG
            CLSP(NP) = INT( VY(JY) )
          ENDIF
!
        ENDDO
!
        NG1 = NG2 + 1
        DEALLOCATE( PELEM )
!
      ENDDO
!
      DEALLOCATE( PPKLE )
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE SAMPLE_POLYLINE
!
!                 ************************
                  SUBROUTINE SAMPLE_POINTS
!                 ************************
!
     &( NP,NP_MAX,NCLS,NTAG, XP,YP,TAGP,CLSP,ELTP,SHPP, NG,XG,YG,VG,
     &  NPOIN,NELEM,NELMAX,IKLE,X,Y )
!
!***********************************************************************
! TELEMAC V8P0
!***********************************************************************
!
!brief    Use the X-Y-Z provided to place particles (XG,YG).
!
!history  S.E. BOURBAN (HRW)
!+   21/08/2016
!+   V8P0
!+   Initial implementation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CLSP    |<->| CLASS OF EACH DROGUE
!| ELTP    |<->| ELEMENT FOR EACH DROGUE
!| IKLE    |-->| CONNECTIVITY MATRIX
!| NCLS    |-->| NUMBER OF CLASSES
!| NELEM   |-->| NUMBER OF ELEMENTS IN IKLE
!| NELMAX  |-->| MAXIMUM NUMBER OF ELEMENTS IN IKLE
!| NG      |-->| TOTAL NUMBER OF POINTS
!| NP      |-->| NUMBER OF DROGUES TO BE CREATED
!| NPOIN   |-->| NUMBER OF NODES IN THE MESH
!| NP_MAX  |-->| MAXIMUM NUMBER OF DROGUES TO BE CREATED
!| NTAG    |<->| NUMBER OF TAGS
!| SHPP    |<->| BARYCENTRIC COORDINATES OF DROGUES
!| TAGP    |<->| TAG OF EACH DROGUE
!| X       |-->| ABSCISSAE OF POINTS IN THE MESH
!| XG      |-->| X-COORDINATE FOR ALL POINTS
!| YG      |-->| Y-COORDINATE FOR ALL POINTS
!| XP,YP   |<--| COORDINATES OF THE NP NEW (RANDOMLY SAMPLED) DROGUES
!| VG      |-->| ARRAYS OF VALUES PER POINT
!| Y       |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!***********************************************************************
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(INOUT)          :: NP,NTAG
      INTEGER, INTENT(IN)             :: NP_MAX,NELEM,NELMAX,NPOIN,NCLS
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3)
      INTEGER, INTENT(INOUT)          :: TAGP(NP_MAX),CLSP(NP_MAX)
      INTEGER, INTENT(INOUT)          :: ELTP(NP_MAX)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: XP(NP_MAX),YP(NP_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPP(3,NP_MAX)
      INTEGER, INTENT(IN)             :: NG
      DOUBLE PRECISION, INTENT(IN)    :: XG(NG),YG(NG),VG(NG)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          I,J, N1,N2,N3, JPID, MT
      DOUBLE PRECISION X0,Y0,X1,Y1,X2,Y2,X3,Y3
      DOUBLE PRECISION SURDET, DET1,DET2,DET3
      DOUBLE PRECISION, PARAMETER :: CHOUIA = 1.D-10
!
!-----------------------------------------------------------------------
!
!     SET THE VARIABLE DROGUES' CLASS
!
      MT = 0
      DO I = 1,NG
        MT = MAX( MT,INT(VG(I)) )
      ENDDO
      IF( NCSIZE.GT.1 ) MT = P_MAX( MT )
      IF( MT.NE.NCLS ) THEN
        WRITE(LU,22) MT,NCLS
 22     FORMAT(1X,'CONDIN_DROGUES:',/,
     &    1X,'     NUMBER OF CLASSES OF DROGUES READ FROM',/,
     &    1X,'     THE X-Y-Z FILE ',I8,' DIFFERENT FROM THE',/,
     &    1X,'     NUMBER SET IN THE CAS FILE ',I8)
        IF( MT.GT.NCLS ) THEN
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     NUMBER OF PARCELS GIVEN A SPATIAL DENSITY
!
!     CHECK AGAINST NUMBER OF PARCELS POSSIBLE
      IF( (NP+NG).GT.NP_MAX ) THEN
        WRITE(LU,33) (NP+NG),NP_MAX
 33     FORMAT(1X,'DROGUES::SAMPLE_POLYLINE:',/,
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
!     POSITIONING THE PROVIDED POINTS
!
      DO I = 1,NG
!
        X0 = XG(I)
        Y0 = YG(I)
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
     &        DET2.GE.-CHOUIA .AND.
     &        DET3.GE.-CHOUIA ) THEN
            JPID = IPID
            EXIT
          ENDIF
!
        ENDDO
!
        IF( NCSIZE.GT.1 ) THEN
          JPID = P_MAX(JPID)
          NTAG = P_MAX(NTAG)
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
          NTAG = NTAG + 1
          TAGP(NP) = NTAG
          CLSP(NP) = INT( VG(NP) )
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE SAMPLE_POINTS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      END MODULE INITIAL_DROGUES
