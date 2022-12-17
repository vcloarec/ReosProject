!                   **************************
                    SUBROUTINE READ_MESH_COORD
!                   **************************
!
     &(FFORMAT,NFIC,X,Y,NPOIN,PROJECTION,LATI0,LONGI0,Z)
!
!***********************************************************************
! HERMES   V7P1
!***********************************************************************
!
!brief    Reads the coordinates in the geometry file.
!+        Latitude-longitude coordinates transformed into mercator.
!
!history  Y. AUDOUIN (EDF LAB, LNHE)
!+        06/05/2015
!+        V7P1
!+   First version.
!
!history  M.S.TURNBULL (HRW)
!+        18/09/2015
!+        V7P1
!+   Correction to the conversion from degree to Mercator projection.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT FOR GEOMETRY FILE
!| NFIC           |-->| LOGICAL UNIT FOR GEOMETRY FILE
!| X              |<--| X COORDINATES
!| Y              |<--| Y COORDINATES
!| PROJECTION     |<--| TYPE OF PROJECTION
!| LATI0          |<--| LATITUDE
!| LONGI0         |<--| LONGITUDE
!| Z              |<--| Z COORDINATES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NPOIN,NFIC
      DOUBLE PRECISION, INTENT(OUT) :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(OUT), OPTIONAL :: Z(NPOIN)
      INTEGER, INTENT(INOUT)        :: PROJECTION
      DOUBLE PRECISION, INTENT(IN)  :: LATI0,LONGI0
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: IERR, I, NDIM
!
      DOUBLE PRECISION, PARAMETER :: R=6.37D6
      DOUBLE PRECISION PS4,TAN1,TAN2,LONGIRAD
!
      INTRINSIC LOG,TAN,ATAN
!
!-----------------------------------------------------------------------
!
!     total number of dimension
      NDIM = 2
      IF (PRESENT(Z)) NDIM = 3

      CALL GET_MESH_COORD(FFORMAT,NFIC,1,NDIM,NPOIN,X,IERR)
      IF(IERR.NE.0) THEN
        WRITE(LU,*) 'ERROR WHILE READING X ARRAY'
        CALL PLANTE(1)
      ENDIF
      CALL GET_MESH_COORD(FFORMAT,NFIC,2,NDIM,NPOIN,Y,IERR)
      IF(IERR.NE.0) THEN
        WRITE(LU,*) 'ERROR WHILE READING Y ARRAY'
        CALL PLANTE(1)
      ENDIF
      IF(PRESENT(Z)) THEN
        CALL GET_MESH_COORD(FFORMAT,NFIC,3,NDIM,NPOIN,Z,IERR)
        IF(IERR.NE.0) THEN
          WRITE(LU,*) 'ERROR WHILE READING Z ARRAY'
          CALL PLANTE(1)
        ENDIF
      ENDIF
!
      CALL CORRXY(X,Y,NPOIN)
!
!     LATITUDE-LONGITUDE COORDINATES (DEGREES)
!     CHANGED INTO MERCATOR PROJECTION
!
      IF(PROJECTION.EQ.3) THEN
        PS4=ATAN(1.D0)
        LONGIRAD=LONGI0*PS4/45.D0
        TAN2=TAN(LATI0*PS4/90.D0+PS4)
        IF(TAN2.LT.0.D0) THEN
          WRITE(LU,*) 'LATI0=',LATI0,' IS PROBABLY WRONG'
          CALL PLANTE(1)
          STOP
        ENDIF
        DO I=1,NPOIN
          X(I)=R*(X(I)*PS4/45.D0-LONGIRAD)
          TAN1=TAN(Y(I)*PS4/90.D0+PS4)
          IF(TAN1.LT.0.D0) THEN
            WRITE(LU,*) 'LATITUDE MUST BE GIVEN IN DEGREES'
            WRITE(LU,*) 'HERE Y(I)=',Y(I),' FOR I=',I
            WRITE(LU,*) 'USE CORRXY (BIEF) FOR CONVERSION'
            CALL PLANTE(1)
            STOP
          ENDIF
          Y(I)=R*(LOG(TAN1)-LOG(TAN2))
        ENDDO
!       NOW IT IS MERCATOR
        PROJECTION=2
        WRITE(LU,*) ' '
        WRITE(LU,*) 'READ_MESH_COORD :'
        WRITE(LU,*) 'COORDINATES CHANGED INTO MERCATOR PROJECTION'
        WRITE(LU,*) 'WITH LATITUDE OF ORIGIN POINT = ',LATI0
        WRITE(LU,*) 'AND LONGITUDE OF ORIGIN POINT = ',LONGI0
        WRITE(LU,*) ' '
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
