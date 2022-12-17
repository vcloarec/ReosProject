!                       *************************
                        SUBROUTINE CALCUL_TANG_W2
!                       *************************
!
     &(IP,NKFROT,CHESTR,H,PENTE,KARMAN,UTAN)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   10/07/2013
!***********************************************************************
!
!brief    COMPUTE TANGENT VELOCITIES VALUES ON A NODE OF A WEIR (TYPE 2)
!+
!
!+
!history  C.COULET (ARTELIA)
!+        10/07/2013
!+        V6P3
!+   Creation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHESTR         |-->| FRICTION COEFFICIENT
!| H              |-->| WATER DEPTH ON THE WEIR
!| IP             |-->| INDEX OF THE NODE
!| KARMAN         |-->| VON KARMAN CONSTANT.
!| NKFROT         |-->| FRICTION LAW, PER POINT
!| PENTE          |-->| LATERAL SLOPE OF WATER ON THE WEIR
!| UTAN           |<--| TANGENTIAL VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)           :: IP
      INTEGER, INTENT(IN)           :: NKFROT(*)
      DOUBLE PRECISION, INTENT(IN)  :: H,PENTE,KARMAN
      DOUBLE PRECISION, INTENT(IN)  :: CHESTR(*)
      DOUBLE PRECISION, INTENT(OUT) :: UTAN
!
      DOUBLE PRECISION CZ,HH
!
!-----------------------------------------------------------------------
!
      IF(NKFROT(IP).EQ.2) THEN
        UTAN = CHESTR(IP)*SQRT(H*ABS(PENTE))*SIGN(1.D0,PENTE)
      ELSEIF(NKFROT(IP).EQ.3) THEN
        UTAN = CHESTR(IP)*H**(2.D0/3.D0)*SQRT(ABS(PENTE))
     &          *SIGN(1.D0,PENTE)
      ELSEIF(NKFROT(IP).EQ.4) THEN
        UTAN = H**(2.D0/3.D0)*SQRT(ABS(PENTE))
     &          *SIGN(1.D0,PENTE)/CHESTR(IP)
      ELSEIF(NKFROT(IP).EQ.5) THEN
        HH    = MAX(H,1.D-9)
        CZ    = MAX(1.D-9,LOG(11.D0*HH/MAX(CHESTR(IP),1.D-9))
     &          /KARMAN)
        UTAN = CZ*SQRT(HH*ABS(PENTE))*SIGN(1.D0,PENTE)
      ELSE
        WRITE(LU,*)'CALCUL_TANG_W2 : UNKNOWN OPTION:',NKFROT(IP)
        WRITE(LU,*)'                 FOR THE FRICTION LAW'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      RETURN
      END
