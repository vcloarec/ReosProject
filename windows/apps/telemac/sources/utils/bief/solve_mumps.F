!                   **********************
                    SUBROUTINE SOLVE_MUMPS
!                   **********************
!
     &(NPOIN,NSEGB,GLOSEG,MAXSEG,DA,XA,XINC,RHS,INFOGR,TYPEXT,LT)
!
!***********************************************************************
! MUMPSVOID   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    CALLS THE DIRECT SOLVER MUMPS
!+                IF MUMPS IS NOT INSTALLED : EMPTY SUBROUTINES ARE USED INSTEAD.
!
!history  F. ZAOUI / C. DENIS (LNHE/SINETICS)
!+        14/10/2009
!+
!+
!
!history  C. DENIS (SINETICS)
!+        02/11/2009
!+        V5P7
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
!| DA,XA          |-->| DIAGONALE ET TERMES EXTRA-DIAGONAUX DE LA MATRICE
!| GLOSEG         |-->| NUMEROS GLOBAUX DES POINTS DES SEGMENTS
!| INFOGR         |-->| IF, YES INFORMATIONS ON LISTING
!| LT             |---|
!| MAXSEG         |---|
!| NPOIN          |-->| NOMBRE D'INCONNUES
!| NSEGB          |-->| NOMBRE DE SEGMENTS
!| RHS            |-->| SECOND MEMBRE
!| TYPEXT         |---|
!| XINC           |<--| SOLUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NSEGB,MAXSEG
      INTEGER, INTENT(IN)             :: GLOSEG(MAXSEG,2)
      INTEGER, INTENT(IN)             :: LT
      LOGICAL, INTENT(IN)             :: INFOGR
      DOUBLE PRECISION, INTENT(INOUT) :: XA(*),RHS(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: XINC(NPOIN),DA(NPOIN)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT
!
!-----------------------------------------------------------------------
!
!
#if defined HAVE_MUMPS
#else
      WRITE(LU,2019)
2019  FORMAT(1X,'MUMPS NOT INSTALLED IN THIS SYSTEM',/,1X,
     &     'CHOOSE OTHER METHOD ',///)
      CALL PLANTE(1)
      STOP
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
