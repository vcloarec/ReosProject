!                   *****************
                    SUBROUTINE ISITOK
!                   *****************
!
     &(H,NPH,U,NPU,V,NPV,NTRAC,T,NPT,X,Y,BORNES,ARRET)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CHECKS THAT THE PHYSICAL PARAMETERS ARE CREDIBLE.
!
!warning  ARRET IS NOT INITIALISED
!
!history  J-M HERVOUET (LNHE)
!+        05/09/2007
!+        V5P8
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ARRET          |<--| WILL BE TRUE IF LIMITS TRESPASSED
!| BORNES         |-->| LIMIT VALUES OF H,U,V,T
!|                |   | IN FOLLOWING ORDER: HMIN,HMAX,UMIN,UMAX,...
!| H              |-->| WATER DEPTH
!| NPH            |-->| NUMBER OF POINTS OF DEPTH
!| NPT            |-->| NUMBER OF POINTS FOR TRACERS
!| NPU            |-->| NUMBER OF POINTS FOR U
!| NPV            |-->| NUMBER OF POINTS FOR V
!| NTRAC          |-->| NUMBER OF TRACERS
!| T              |-->| BLOCK OF TRACERS ET NOMBRE DE POINTS DE TRACEUR.
!| U              |<->| X-COMPONENT OF VELOCITY
!| V              |<->| Y-COMPONENT OF VELOCITY
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
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
      INTEGER, INTENT(IN)          :: NPH,NPU,NPV,NPT,NTRAC
      LOGICAL, INTENT(INOUT)       :: ARRET
      DOUBLE PRECISION, INTENT(IN) :: H(NPH),U(NPU),V(NPV)
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),BORNES(8)
      TYPE(BIEF_OBJ)  , INTENT(IN) :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ITRAC
!
!-----------------------------------------------------------------------
!
!  CHECKS THE DEPTH
!
      DO I = 1 , NPH
        IF(H(I).LT.BORNES(1)) THEN
          ARRET = .TRUE.
          WRITE(LU,101) 'LOWER','H',I,X(I),Y(I),'H',H(I),BORNES(1)
        ENDIF
        IF(H(I).GT.BORNES(2)) THEN
          ARRET = .TRUE.
          WRITE(LU,101) 'UPPER','H',I,X(I),Y(I),'H',H(I),BORNES(2)
        ENDIF
      ENDDO ! I
!
!-----------------------------------------------------------------------
!
!  CHECKS THE VELOCITY U
!
      DO I = 1 , NPU
        IF(U(I).LT.BORNES(3)) THEN
          ARRET = .TRUE.
          WRITE(LU,101) 'LOWER','U',I,X(I),Y(I),'U',U(I),BORNES(3)
        ENDIF
        IF(U(I).GT.BORNES(4)) THEN
          ARRET = .TRUE.
          WRITE(LU,101) 'UPPER','U',I,X(I),Y(I),'U',U(I),BORNES(4)
        ENDIF
      ENDDO ! I
!
!-----------------------------------------------------------------------
!
!  CHECKS THE VELOCITY V
!
      DO I = 1 , NPV
        IF(V(I).LT.BORNES(5)) THEN
          ARRET = .TRUE.
          WRITE(LU,101) 'LOWER','V',I,X(I),Y(I),'V',V(I),BORNES(5)
        ENDIF
        IF(V(I).GT.BORNES(6)) THEN
          ARRET = .TRUE.
          WRITE(LU,101) 'UPPER','V',I,X(I),Y(I),'V',V(I),BORNES(6)
        ENDIF
      ENDDO ! I
!
!-----------------------------------------------------------------------
!
!  CHECKS THE TRACER
!
      IF(NTRAC.GT.0) THEN
!
      DO ITRAC=1,NTRAC
!
      DO I = 1 , NPT
        IF(T%ADR(ITRAC)%P%R(I).LT.BORNES(7)) THEN
          ARRET = .TRUE.
          WRITE(LU,101) 'LOWER','T',I,X(I),Y(I),
     &                  'T',T%ADR(ITRAC)%P%R(I),BORNES(7)
        ENDIF
        IF(T%ADR(ITRAC)%P%R(I).GT.BORNES(8)) THEN
          ARRET = .TRUE.
          WRITE(LU,101) 'UPPER','T',I,X(I),Y(I),
     &                  'T',T%ADR(ITRAC)%P%R(I),BORNES(8)
        ENDIF
      ENDDO
!
      ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
101   FORMAT(/,1X,A5,' LIMIT ON ',A1,' REACHED AT POINT ',I6,/,1X,
     &         'WITH COORDINATES',G16.7,' AND ',G16.7,/,1X,
     &         'THE VALUE OF ',A1,' IS ',G16.7,' THE LIMIT IS: ',G16.7)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
