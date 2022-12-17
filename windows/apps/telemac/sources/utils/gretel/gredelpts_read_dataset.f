!                         ******************************
                          SUBROUTINE GREDELPTS_READ_DATASET
!                         ******************************
     &(LOCAL_VALUE,NPOINMAX,NPOIN,IT,FU,ENDE)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief
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
!| AT             |---|
!| ENDE           |---|
!| FU             |---|
!| LOCAL_VALUE    |---|
!| NPOIN          |---|
!| NPOINMAX       |---|
!| NVALUE         |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPOINMAX,NPOIN,FU
      INTEGER, INTENT(OUT) :: IT
!
      REAL, INTENT(OUT) :: LOCAL_VALUE(NPOINMAX)
!
      LOGICAL, INTENT(OUT) :: ENDE

      INTEGER IPOIN
!
      ENDE = .TRUE.
!
      READ(FU,END=999) IT, (LOCAL_VALUE(IPOIN),IPOIN=1,NPOIN)
!
      ENDE = .FALSE.
!
 999  RETURN
      END SUBROUTINE GREDELPTS_READ_DATASET
