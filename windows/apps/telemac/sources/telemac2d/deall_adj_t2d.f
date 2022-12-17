!                   ************************
                    SUBROUTINE DEALL_ADJ_T2D
!                   ************************
!
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    DEALLOCATES THE STRUCTURES FOR THE ADJOINT SYSTEM.
!
!history  Y AUDOUIN (LNHE)
!+        16/02/2016
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      IF(LISTIN) THEN
        WRITE(LU,21)
      ENDIF
21    FORMAT(1X,///,26X,'***************************************',/,
     &26X,              '*    MEMORY DEORGANIZATION  (ADJOINT) *',/,
     &26X,              '***************************************',/)
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
!                     ******************
!                     *  REAL ARRAYS   *
!                     ******************
!
!-----------------------------------------------------------------------
!
      CALL BIEF_DEALLOBJ(VARSORA)
!  ARRAYS CONTAINING THE VARIABLES WHICH WILL BE OUTPUT TO THE RESULT FILE:
!
      CALL BIEF_DEALLOBJ(UNKADJ)
      CALL BIEF_DEALLOBJ(PP)
      CALL BIEF_DEALLOBJ(QQ)
      CALL BIEF_DEALLOBJ(RR)
!
      CALL BIEF_DEALLOBJ(UU  )
      CALL BIEF_DEALLOBJ(VV  )
      CALL BIEF_DEALLOBJ(HH  )
      CALL BIEF_DEALLOBJ(UIT1)
      CALL BIEF_DEALLOBJ(VIT1)
      CALL BIEF_DEALLOBJ(HIT1)
!
!  MATRICES
!
      CALL BIEF_DEALLOBJ(MATADJ)
      CALL BIEF_DEALLOBJ(TAM1)
      CALL BIEF_DEALLOBJ(TAM2)
      CALL BIEF_DEALLOBJ(TAM3)
      CALL BIEF_DEALLOBJ(TBM1)
      CALL BIEF_DEALLOBJ(TBM2)
      CALL BIEF_DEALLOBJ(TCM1)
      CALL BIEF_DEALLOBJ(TCM2)
!
! DIRICHLET POINTS:
!
      CALL BIEF_DEALLOBJ(ADJDIR)
      CALL BIEF_DEALLOBJ(QBOR)
      CALL BIEF_DEALLOBJ(RBOR)
      CALL BIEF_DEALLOBJ(PBOR)
!
!
! ARRAYS FOR THE DESCENT METHOD (UP TO NPOIN PARAMETERS)
!
      CALL BIEF_DEALLOBJ(GRADJ  )
      CALL BIEF_DEALLOBJ(GRADJN )
      CALL BIEF_DEALLOBJ(DESC   )
      CALL BIEF_DEALLOBJ(SETSTR )
      CALL BIEF_DEALLOBJ(SETSTR2)
!
!_______________________________________________________________________
!
!  POINTERS FOR THE SECOND MEMBERS OF THE PROPAGATION STEP
!
!_______________________________________________________________________
!
!
      CALL BIEF_DEALLOBJ(ALPHA1)
      CALL BIEF_DEALLOBJ(ALPHA2)
      CALL BIEF_DEALLOBJ(ALPHA3)
      CALL BIEF_DEALLOBJ(HD    )
      CALL BIEF_DEALLOBJ(UD    )
      CALL BIEF_DEALLOBJ(VD    )
!
!  COMPUTES THE BLOCK WHICH CONNECTS A VARIABLE NAME TO ITS ARRAY
!
!
! 01
!
!***********************************************************************
!
! WRITES OUT TO LISTING :
!
      IF(LISTIN) THEN
        WRITE(LU,23)
      ENDIF
23    FORMAT(1X,///,21X,'**************************************',/,
     &21X,              '*    END OF MEMORY DEORGANIZATION ADJ*',/,
     &21X,              '**************************************',/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

