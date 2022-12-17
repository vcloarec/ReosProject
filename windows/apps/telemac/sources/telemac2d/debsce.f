!                   ********************************
                    DOUBLE PRECISION FUNCTION DEBSCE
!                   ********************************
!
     &( TIME , I , DISCE )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   07/10/2011
!***********************************************************************
!
!brief    GIVES THE PRESCRIBED DISCHARGE OF EVERY SOURCE POINT.
!+
!+            VARIATIONS WRT TIME AND SPACE MAY BE IMPLEMENTED.
!
!note     T2DVEF IS THE SOURCES FILE IN TELEMAC-2D
!
!history  J-M HERVOUET (LNHE)
!+        03/04/2008
!+        V6P0
!+
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
!+   Modification size FCT due to modification of TRACER numbering
!
!history  J-M HERVOUET (LNHE)
!+        243/02/2012
!+        V6P2
!+   Discharge taken at mid distance between AT-DT AND AT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DISCE          |-->| ARRAY OF DISCHARGES OF SOURCES.
!|                |   | READ IN THE PARAMETER FILE.
!|                |   | NAME OF DISCE IS DSCE IN TELEMAC-2D.
!| I              |-->| NUMBER OF THE SOURCE OR OF THE SOURCE REGION
!| TIME           |-->| TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY: AT,ENTET,DT,
     &                                  T2D_FILES,T2DVEF,OKDEBSCE
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME,DISCE(*)
      INTEGER         , INTENT(IN) :: I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=9) FCT
      DOUBLE PRECISION DEBSCE1,DEBSCE2
!
!     IF SOURCES FILE EXISTING, ATTEMPT TO FIND
!     THE VALUE IN IT. IF YES, OKDEBSCE REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OKDEBSCE SET     TO .FALSE.
!
      IF(OKDEBSCE(I).AND.T2D_FILES(T2DVEF)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE Q(1), Q(2), ETC, Q(99), DEPENDING ON I
        FCT='Q(       '
        IF(I.LT.10) THEN
          WRITE(FCT(3:3),FMT='(I1)') I
          FCT(4:4)=')'
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(3:4),FMT='(I2)') I
          FCT(5:5)=')'
        ELSE
          WRITE(LU,*) 'DEBSCE NOT PROGRAMMED FOR MORE THAN 99 SOURCES'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_SOURCES(DEBSCE1,FCT,AT-DT,T2D_FILES(T2DVEF)%LU,
     &                        ENTET,OKDEBSCE(I))
        CALL READ_FIC_SOURCES(DEBSCE2,FCT,AT   ,T2D_FILES(T2DVEF)%LU,
     &                        ENTET,OKDEBSCE(I))
        DEBSCE=(DEBSCE1+DEBSCE2)*0.5D0
!
      ENDIF
!
!     BEWARE, AN ERROR IN THE SOURCES FILE MAY REMAIN UNNOTICED
!     BECAUSE WE RESORT HERE TO THE PARAMETER FILE
!
      IF(.NOT.OKDEBSCE(I).OR.T2D_FILES(T2DVEF)%NAME(1:1).EQ.' ') THEN
!
!       DISCE IS TAKEN IN THE PARAMETER FILE
!
!       GLOBAL NUMBER OF SOURCE I IS ISCE(I) IN TELEMAC-2D
        DEBSCE = DISCE(I)
!
        ! USER UPDATE OF VALUE
        CALL USER_DEBSCE(TIME, I, DISCE, DEBSCE)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
