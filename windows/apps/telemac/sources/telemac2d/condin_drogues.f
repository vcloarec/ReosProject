!               *************************
                SUBROUTINE CONDIN_DROGUES
!               *************************
!
!
!***********************************************************************
! TELEMAC2D   V8P0
!***********************************************************************
!
!brief    Initialise the physical characteristics of drogues given
!+          mapped locations set from the GEO file.
!
!history  S.E. BOURBAN (HRW)
!+        21/08/2018
!+        V8P0
!+        New subroutine created to let users define initial layout of
!+        drogues.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D, ONLY : T2D_FILES,T2DGEO, MESH,
     &  NFLOT,NFLOT_MAX,XFLOT,YFLOT, TAGFLO,CLSFLO,ELTFLO,SHPFLO,
     &  T2DPLY, NPLY,NPOIN_PLY,VALUE_PLY,X_PLY,Y_PLY
      USE INITIAL_DROGUES, ONLY: NDRG_CLSS,NODCLSS,NDRG_TAGS,
     &  DRG_DENSITY,SAMPLE_WPOIN,SAMPLE_POLYLINE,SAMPLE_POINTS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IERR,IPOIN
!
!-----------------------------------------------------------------------
!
!     ALGAE / DROGUES
!
!     ACQUIRE SPATIAL SETTINGS FOR DROGUES FROM THE GEO FILE
      IF( NDRG_CLSS.GT.0 ) THEN
!
        CALL FIND_VARIABLE( T2D_FILES(T2DGEO)%FMT,T2D_FILES(T2DGEO)%LU,
     &    'DROGUES CLASSES ', NODCLSS%R, MESH%NPOIN, IERR )
!
        IF( IERR.EQ.0 ) THEN
!
!         FOUND THE VARIABLE DROGUES CLASSES
          WRITE(LU,12)
 12       FORMAT(1X,'CONDIN_DROGUES:',/,
     &         1X,'     CLASSES OF DROGUES READ FROM',/,
     &         1X,'     THE GEOMETRY FILE')
!
!         CONVERTING REAL TO INTERGER
          DO IPOIN = 1,MESH%NPOIN
            NODCLSS%I(IPOIN) = INT( NODCLSS%R(IPOIN)+1.D-3 )
          ENDDO
!
!         SAMPLING IN PARALLEL
          CALL SAMPLE_WPOIN( NFLOT,NFLOT_MAX,NDRG_CLSS,NDRG_TAGS,
     &      XFLOT%R,YFLOT%R, TAGFLO%I,CLSFLO%I,ELTFLO%I,SHPFLO%R,
     &      DRG_DENSITY,
     &      MESH%NPOIN,MESH%NELEM,MESH%NELMAX,
     &      MESH%IKLE%I,NODCLSS%I,MESH%X%R,MESH%Y%R )
!
        ENDIF ! ( IERR.EQ.0 )
!
      ENDIF
!
!     ACQUIRE SPATIAL SETTINGS FOR DROGUES FROM A POLYGON FILE
!
      IF( NDRG_CLSS.GT.0 .AND. T2D_FILES(T2DPLY)%NAME(1:1).NE.' '
     &  .AND. T2D_FILES(T2DPLY)%FMT(1:8) .EQ. 'BKASCI2S' ) THEN
!
        CALL READ_FIC_POLYGON( T2D_FILES(T2DPLY)%FMT,
     &    T2D_FILES(T2DPLY)%LU,
     &    NPLY,NPOIN_PLY,VALUE_PLY,X_PLY,Y_PLY, IERR )
!
        IF( IERR.EQ.0 ) THEN
!
!         FOUND THE VARIABLE DROGUES CLASSES
          WRITE(LU,13)
 13       FORMAT(1X,'CONDIN_DROGUES:',/,
     &         1X,'     CLASSES OF DROGUES READ FROM',/,
     &         1X,'     THE POLYGON FILE')
!
!         SAMPLING IN PARALLEL
          CALL SAMPLE_POLYLINE( NFLOT,NFLOT_MAX,NDRG_CLSS,NDRG_TAGS,
     &      XFLOT%R,YFLOT%R, TAGFLO%I,CLSFLO%I,ELTFLO%I,SHPFLO%R,
     &      DRG_DENSITY,
     &      NPLY,NPOIN_PLY,VALUE_PLY,NPOIN_PLY(NPLY),X_PLY,Y_PLY,
     &      MESH%NPOIN,MESH%NELEM,MESH%NELMAX,
     &      MESH%IKLE%I,MESH%X%R,MESH%Y%R )
!
        ENDIF ! ( IERR.EQ.0 )
!
!     ACQUIRE SPATIAL SETTINGS FOR DROGUES FROM AN X-Y-Z FILE
!
      ELSEIF( NDRG_CLSS.GT.0 .AND. T2D_FILES(T2DPLY)%NAME(1:1).NE.' '
     &  .AND. T2D_FILES(T2DPLY)%FMT(1:8) .EQ. 'BKASCXYZ' ) THEN
!
        CALL READ_FIC_POINTS( T2D_FILES(T2DPLY)%FMT,
     &    T2D_FILES(T2DPLY)%LU,
     &    NPLY,VALUE_PLY,X_PLY,Y_PLY, IERR )
!
        IF( IERR.EQ.0 ) THEN
!
!         FOUND THE VARIABLE DROGUES CLASSES
          WRITE(LU,14)
 14       FORMAT(1X,'CONDIN_DROGUES:',/,
     &         1X,'     CLASSES OF DROGUES READ FROM',/,
     &         1X,'     THE X-Y-Z FILE')
!
!         SAMPLING IN PARALLEL
          CALL SAMPLE_POINTS( NFLOT,NFLOT_MAX,NDRG_CLSS,NDRG_TAGS,
     &      XFLOT%R,YFLOT%R, TAGFLO%I,CLSFLO%I,ELTFLO%I,SHPFLO%R,
     &      NPLY,X_PLY,Y_PLY,VALUE_PLY,
     &      MESH%NPOIN,MESH%NELEM,MESH%NELMAX,
     &      MESH%IKLE%I,MESH%X%R,MESH%Y%R )
!
        ENDIF ! ( IERR.EQ.0 )
!
      ENDIF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      RETURN
      END
