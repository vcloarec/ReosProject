!                   *****************
                    SUBROUTINE MARAST
!                   *****************
!
     &(MARDAT,MARTIM,PHI0,NPOIN,AT,FU1,FV1,X,SINLAT,COSLAT,GRAV)
!
!***********************************************************************
! TELEMAC2D   V8P4
!***********************************************************************
!
!brief    COMPUTES THE TIDAL FORCE.
!
!history  E. DAVID (LHF)    ; F LEPEINTRE (LNH)    ; J-M JANIN (LNH)
!+        01/03/1994
!+        V5P2
!+   First version.
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
!history  U.H.Merkel (BAW Karlsruhe)
!+        17/07/2012
!+        V6P2
!+    Adaption to NAG
!
!history  T.LACKRIET (IMDC)
!+        20/01/2015
!+        V7P0
!+   Correction of a bug for the signs of FXL AND FXS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| COSLAT         |-->| COSINUS OF LATITUDE (IN SPHERICAL COORDINATES)
!| FU1            |<--| X-COMPONENT OF TIDE GENERATING FORCE
!| FV1            |<--| Y-COMPONENT OF TIDE GENERATING FORCE
!| GRAV           |-->| GRAVITY
!| MARDAT         |-->| DATE (YEAR, MONTH,DAY)
!| MARTIM         |-->| TIME (HOUR, MINUTE,SECOND)
!| NPOIN          |-->| NUMBER OF POINTS
!| PHI0           |-->| LONGITUDE OF ORIGIN POINT
!| SINLAT         |-->| SINUS OF DE LATITUDE (IN SPHERICAL COORDINATES)
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_MARAST => MARAST
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: MARDAT(3),MARTIM(3),NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: FU1(NPOIN),FV1(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: COSLAT(NPOIN),SINLAT(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),GRAV,AT,PHI0
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SEC,I
!
      DOUBLE PRECISION ARL,ARS,DL,DS,AL,AS
      DOUBLE PRECISION RT,LONG,AHL,AHS,MLT,MST,LONG0
      DOUBLE PRECISION F0L,F0S,FXL,FYL,FXS,FYS
      DOUBLE PRECISION TLOC,TSLOC,TLOC1
      DOUBLE PRECISION K2,H2
!
      EXTERNAL TSLOC
!
      INTRINSIC ACOS,COS,SIN
!
!-----------------------------------------------------------------------
!
!     SPLITS ARRAYS MARDAT AND MARTIM
!
      YEAR  = MARDAT(1)
      MONTH = MARDAT(2)
      DAY   = MARDAT(3)
      HOUR  = MARTIM(1)
      MINUTE = MARTIM(2)
      SEC   = MARTIM(3)
!
! REMINDER : HOUR IN UNIVERSAL TIME |
! GENERAL REMARK : T=TERRE, L=LUNE , S=SOLEIL
!
! LONG0: REFERENCE LONGITUDE IN RADIAN (0,2PI)
!
      LONG0=PHI0*ACOS(-1.D0)/180.D0
!
! CALLS THE MAIN FUNCTION COMPUTING THE LUNAR AND SOLAR ANGLES
!
      CALL ASTRO(YEAR,MONTH,DAY,HOUR,MINUTE,SEC,AT,ARL,ARS,DL,DS,AL,AS)
!
! RT: EARTH RADIUS IN M
!
      RT   = 6378000.D0
!
! MASS RATIO MOON/EARTH
!
      MLT  = 1.D0 / 81.53D0
!
! MASS RATIO SUN/EARTH
!
      MST  = 331954.D0
!
! AMPLITUDE OF THE FORCE INDUCED BY :
!
!     - THE MOON
!
      F0L  = GRAV * MLT * ARL**2
!
!     - THE SUN
!
      F0S  = GRAV * MST * ARS**2
!
! SIDEREAL TIME
!
      TLOC1 = TSLOC(YEAR,MONTH,DAY,HOUR,MINUTE,SEC,AT)
!
      DO I=1,NPOIN
!
! LONGITUDE OF THE CONSIDERED NODE
!
        LONG = X(I)/RT+LONG0
!
! LOCAL SIDEREAL TIME
!
        TLOC = TLOC1 + LONG
!
! TIME ANGLE OF THE MOON
!
        AHL  = TLOC - AL
!
! TIME ANGLE OF THE SUN
!
        AHS  = TLOC - AS
!
! FORCE INDUCED BY THE ASTRONOMICAL POTENTIAL ONLY
!
!    FXL : FORCE ALONG X FOR THE MOON
!     Y  : ALONG Y
!     S  : SAME THING FOR THE SUN
!
        FXL = -F0L * COS(DL) * SIN(AHL) *
     &         ( ( 1.D0-2*ARL*(SINLAT(I)*SIN(DL)+
     &           COSLAT(I)*COS(DL)*COS(AHL))+ARL*ARL )**(-1.5D0) -1.D0 )
!
        FXS = -F0S * COS(DS) * SIN(AHS) *
     &         ( ( 1.D0-2*ARS*(SINLAT(I)*SIN(DS)+
     &           COSLAT(I)*COS(DS)*COS(AHS))+ARS*ARS )**(-1.5D0) -1.D0 )
!
        FYL =  F0L*(COSLAT(I)*SIN(DL)-SINLAT(I)*COS(DL)*COS(AHL))*
     &         ( ( 1.D0-2*ARL*(SINLAT(I)*SIN(DL)+
     &           COSLAT(I)*COS(DL)*COS(AHL))+ARL*ARL )**(-1.5D0) -1.D0 )
!
        FYS =  F0S*( COSLAT(I)*SIN(DS)-SINLAT(I)*COS(DS)*COS(AHS))*
     &         ( ( 1.D0-2*ARS*(SINLAT(I)*SIN(DS)+
     &           COSLAT(I)*COS(DS)*COS(AHS))+ARS*ARS )**(-1.5D0) -1.D0 )
!
! TAKES INTO ACCOUNT :
!
!    - THE TERRESTRIAL TIDE (LOVE NUMBER H2)
!
        H2=0.61D0
!
!    - THE STATIC PERTUBATIONS (LOVE NUMBER K2)
!
        K2=0.30D0
!
! MISSES :
!
!    - DYNAMIC PERTUBATION OF AUTO-ATTRACTION
!    - DYNAMIC PERTUBATION OF THE EFFECTS OF LOADS
!
! FINAL FORCE
!
        FU1(I)=FU1(I)+(1.D0+K2-H2)*(FXL+FXS)
        FV1(I)=FV1(I)+(1.D0+K2-H2)*(FYL+FYS)
!
      ENDDO ! I
!
!-----------------------------------------------------------------------
!
      RETURN
      END
