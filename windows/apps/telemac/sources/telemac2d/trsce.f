!                   *******************************
                    DOUBLE PRECISION FUNCTION TRSCE
!                   *******************************
!
     &( TIME , I , ITRAC )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   07/10/2011
!***********************************************************************
!
!brief    PRESCRIBES THE TRACER VALUES AT THE SOURCES.
!+                THIS VALUE MAY VARY IN TIME.
!
!history  J-M HERVOUET (LNHE)
!+        08/04/2008
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
!+   Modification size FCT and OK due to modification of TRACER
!+    numbering TRACER is now identified by 2 values (Isource, Itracer)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| SOURCE RANK
!| ITRAC          |-->| TRACER RANK
!| TIME           |-->| TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY: AT,ENTET,TSCE,
     &                                  T2D_FILES,T2DVEF,OKTRSCE
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME
      INTEGER         , INTENT(IN) :: I,ITRAC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=9) FCT
      INTEGER IRANK
!
!     IF A SOURCE FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OKTRSCE REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OKTRSCE SET     TO .FALSE.
!
      IF(OKTRSCE(I,ITRAC).AND.T2D_FILES(T2DVEF)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE TR(I,ITRAC) DEPENDING ON I AND ITRAC
        FCT='TR(      '
        IRANK=4
        IF(I.LT.10) THEN
          WRITE(FCT(IRANK:IRANK),FMT='(I1)') I
          IRANK=IRANK+1
          FCT(IRANK:IRANK)=','
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(IRANK:IRANK+1),FMT='(I2)') I
          IRANK=IRANK+2
          FCT(IRANK:IRANK)=','
        ELSE
          WRITE(LU,*) 'TRSCE NOT PROGRAMMED FOR MORE THAN 99 SOURCES'
          CALL PLANTE(1)
          STOP
        ENDIF
        IRANK=IRANK+1
        IF(ITRAC.LT.10) THEN
          WRITE(FCT(IRANK:IRANK),FMT='(I1)') ITRAC
          IRANK=IRANK+1
          FCT(IRANK:IRANK)=')'
        ELSEIF(ITRAC.LT.100) THEN
          WRITE(FCT(IRANK:IRANK+1),FMT='(I2)') ITRAC
          IRANK=IRANK+2
          FCT(IRANK:IRANK)=')'
        ELSE
          WRITE(LU,*) 'TRSCE NOT PROGRAMMED FOR MORE THAN 99 TRACERS'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_SOURCES(TRSCE,FCT,AT,T2D_FILES(T2DVEF)%LU,
     &                        ENTET,OKTRSCE(I,ITRAC))
!
      ENDIF
!
!     BEWARE, AN ERROR IN THE SOURCE FILE MAY REMAIN UNNOTICED
!     BECAUSE WE RESORT HERE TO THE STEERING FILE
!
      IF(.NOT.OKTRSCE(I,ITRAC).OR.
     &   T2D_FILES(T2DVEF)%NAME(1:1).EQ.' ') THEN
!
!       TSCE IS TAKEN FROM THE STEERING FILE
!
!       GLOBAL NUMBER OF SOURCE I IS ISCE(I) IN TELEMAC-2D
        TRSCE = TSCE(I,ITRAC)
!
        ! USER UPDATE OF VALUE
        CALL USER_TRSCE(TIME, I, ITRAC, TRSCE)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
