!                   *******************
                    SUBROUTINE INTERPOL
!                   *******************
!
     &(RO,R02,R03,JCOUT1,JCOUT2,JCOUT3)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES RO = -B/2A, MINIMUM OF THE FUNCTION :
!+                A * (RO**2) + B * RO +C.
!
!history  E. BARROS
!+        27/04/1993
!+
!+
!
!history  A. LEOPARDI (UNINA)
!+        05/10/2000
!+        V5P2
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
!| JCOUT1         |-->| J(RO)
!| JCOUT2         |-->| COST FUNCTION AT R02
!| JCOUT3         |-->| COST FUNCTION AT R03
!| R02            |-->| COEFFICIENT CORRESPONDING TO JCOUT2
!| R03            |-->| COEFFICIENT CORRESPONDING TO JCOUT3
!| RO             |<--| COEFFICIENT OF DESCENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION , INTENT(IN)    :: R02,R03,JCOUT1,JCOUT2,JCOUT3
      DOUBLE PRECISION , INTENT(INOUT) :: RO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION COEFA,COEFB
      DOUBLE PRECISION ROMAX
!
      INTRINSIC ABS
!
!-----------------------------------------------------------------------
!
      COEFA = ((JCOUT1*(R02-R03))+(R03*JCOUT2)-(JCOUT3*R02))/(R02*R03
     &     *(R02-R03))
!
      COEFB = ((-JCOUT1*((R02*R02)-(R03*R03)))-(JCOUT2*R03*R03)
     &     + (JCOUT3*R02*R02))/(R02*R03*(R02-R03))
!
      IF(COEFA.LE.0.D0) THEN
        WRITE(LU,*) 'INTERPOL : COEFFICIENT A LESS THAN ZERO:',COEFA
        CALL PLANTE(1)
        STOP
      ENDIF
!
      RO = - COEFB / (2.D0 * COEFA)
!
!     CAPS THE VALUE OF RO :
!
      IF(ABS(R02).GE.ABS(R03)) THEN
        ROMAX = 2.D0*R02
      ELSEIF(ABS(R03).GE.ABS(R02)) THEN
        ROMAX = 2.D0*R03
      ENDIF
!
      IF(ABS(RO).GT.ABS(ROMAX)) THEN
        WRITE(LU,*) 'INTERPOL : LIMIT VALUE OF RHO'
        RO = ROMAX
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
