!                   ************************
                    SUBROUTINE POINT_ADJ_T2D
!                   ************************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ALLOCATES THE STRUCTURES FOR THE ADJOINT SYSTEM.
!
!history  J-M HERVOUET (LNHE)
!+        24/04/1997
!+
!+
!
!history  A LEOPARDI (UNINA)
!+        18/09/2000
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE METEO_TELEMAC !, ONLY : WINDX,WINDY,PATMOS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELBU,IELBH,CFG(2)
!
!-----------------------------------------------------------------------
!
      IF(LISTIN) THEN
        WRITE(LU,21)
      ENDIF
21    FORMAT(1X,///,26X,'*************************************',/,
     &26X,              '*    MEMORY ORGANIZATION  (ADJOINT) *',/,
     &26X,              '*************************************',/)
!
!-----------------------------------------------------------------------
!
!     TYPES OF DISCRETISATIONS
!
      IELM0 = 10*(IELMH/10)
      IELM1 = IELM0 + 1
!
      IELBU = IELBOR(IELMU,1)
      IELBH = IELBOR(IELMH,1)
!
! TYPE OF STORAGE AND PRODUCT MATRIX X VECTOR
!
      CFG(1) = OPTASS
      CFG(2) = PRODUC
!
!-----------------------------------------------------------------------
!
!                     ******************
!                     *  REAL ARRAYS   *
!                     ******************
!
!-----------------------------------------------------------------------
!
!  ARRAYS CONTAINING THE VARIABLES WHICH WILL BE OUTPUT TO THE RESULT FILE:
!
      CALL BIEF_ALLVEC(1,PP,'PP    ',IELMH,1,1,MESH)
      CALL BIEF_ALLVEC(1,QQ,'QQ    ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,RR,'RR    ',IELMU,1,1,MESH)
!
      CALL BIEF_ALLVEC(1,UU  ,'UU    ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,VV  ,'VV    ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,HH  ,'HH    ',IELMH,1,1,MESH)
      CALL BIEF_ALLVEC(1,UIT1,'UIT1  ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,VIT1,'VIT1  ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,HIT1,'HIT1  ',IELMH,1,1,MESH)
!
!  BLOCK OF THE UNKNOWNS IN PROPAG
!
      CALL ALLBLO(UNKADJ,'UNKADJ')
      CALL ADDBLO(UNKADJ, PP)
      CALL ADDBLO(UNKADJ, QQ)
      CALL ADDBLO(UNKADJ, RR)
!
!  MATRICES
!
      CALL BIEF_ALLMAT(TAM1,'TAM1  ',IELMH,IELMH,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(TAM2,'TAM2  ',IELMU,IELMU,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(TAM3,'TAM3  ',IELMU,IELMU,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(TBM1,'TBM1  ',IELMU,IELMH,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(TBM2,'TBM2  ',IELMU,IELMH,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(TCM1,'TCM1  ',IELMU,IELMH,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(TCM2,'TCM2  ',IELMU,IELMH,CFG,'Q','Q',MESH)
!
! BLOCK OF THE MATRICES IN PROPAG
!
      CALL ALLBLO(MATADJ,'MATADJ')
      CALL ADDBLO(MATADJ,TAM1)
      CALL ADDBLO(MATADJ,TCM1)
      CALL ADDBLO(MATADJ,TCM2)
      CALL ADDBLO(MATADJ,TBM1)
      CALL ADDBLO(MATADJ,TAM2)
      CALL ADDBLO(MATADJ,A23)
      CALL ADDBLO(MATADJ,TBM2)
      CALL ADDBLO(MATADJ,A32)
      CALL ADDBLO(MATADJ,TAM3)
!
! DIRICHLET POINTS:
!
      CALL BIEF_ALLVEC(1,QBOR ,'QBOR  ',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(1,RBOR ,'RBOR  ',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(1,PBOR ,'PBOR  ',IELBH,1,1,MESH)
!
      CALL ALLBLO(ADJDIR,'ADJDIR')
      CALL ADDBLO(ADJDIR,PBOR)
      CALL ADDBLO(ADJDIR,QBOR)
      CALL ADDBLO(ADJDIR,RBOR)
!
! ARRAYS FOR THE DESCENT METHOD (UP TO NPOIN PARAMETERS)
!
      CALL BIEF_ALLVEC(1,GRADJ  ,'GRADJ ',IELM1,1,1,MESH)
      CALL BIEF_ALLVEC(1,GRADJN ,'GRADJN',IELM1,1,1,MESH)
      CALL BIEF_ALLVEC(1,DESC   ,'DESC  ',IELM1,1,1,MESH)
      CALL BIEF_ALLVEC(1,SETSTR ,'SETSTR',IELM1,1,1,MESH)
      CALL BIEF_ALLVEC(1,SETSTR2,'SETST2',IELM1,1,1,MESH)
!
!_______________________________________________________________________
!
!  POINTERS FOR THE SECOND MEMBERS OF THE PROPAGATION STEP
!
!_______________________________________________________________________
!
!
      CALL BIEF_ALLVEC(1,ALPHA1  ,'ALPHA1',IELMH,1,1,MESH)
      CALL BIEF_ALLVEC(1,ALPHA2  ,'ALPHA2',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,ALPHA3  ,'ALPHA3',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,HD      ,'HD    ',IELMH,1,1,MESH)
      CALL BIEF_ALLVEC(1,UD      ,'UD    ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,VD      ,'VD    ',IELMU,1,1,MESH)
!
!  COMPUTES THE BLOCK WHICH CONNECTS A VARIABLE NAME TO ITS ARRAY
!
!
      CALL ALLBLO(VARSORA ,'VARSORA')
! 01
      CALL ADDBLO(VARSORA,U)
! 02
      CALL ADDBLO(VARSORA,V)
! 03
      CALL ADDBLO(VARSORA,FU)
! 04
      CALL ADDBLO(VARSORA,H)
! 05
      CALL ADDBLO(VARSORA,FV)
! 06
      CALL ADDBLO(VARSORA,ZF)
! 07
      CALL ADDBLO(VARSORA,T2)
! 08
      CALL ADDBLO(VARSORA,T3)
! 09
      CALL ADDBLO(VARSORA,T)
! 10
      CALL ADDBLO(VARSORA,AK)
! 11
      CALL ADDBLO(VARSORA,EP)
! 12
      CALL ADDBLO(VARSORA,VISC)
! 13
      CALL ADDBLO(VARSORA,T4)
! 14
      CALL ADDBLO(VARSORA,T5)
! 15
      CALL ADDBLO(VARSORA,T6)
! 16
      CALL ADDBLO(VARSORA,WINDX)
! 17
      CALL ADDBLO(VARSORA,WINDY)
! 18
      CALL ADDBLO(VARSORA,PATMOS)
! 19
      CALL ADDBLO(VARSORA,CHESTR)
! 20
      CALL ADDBLO(VARSORA,CV1)
! 21
      CALL ADDBLO(VARSORA,CV2)
! 22
      CALL ADDBLO(VARSORA,CV3)
! 23
      CALL ADDBLO(VARSORA,PP)
! 24
      CALL ADDBLO(VARSORA,QQ)
! 25
      CALL ADDBLO(VARSORA,RR)
! 26
      CALL ADDBLO(VARSORA,PRIVE%ADR(4)%P)
!
!***********************************************************************
!
! WRITES OUT TO LISTING :
!
      IF(LISTIN) THEN
        WRITE(LU,23)
      ENDIF
23    FORMAT(1X,///,21X,'************************************',/,
     &21X,              '*    END OF MEMORY ORGANIZATION ADJ*',/,
     &21X,              '************************************',/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
