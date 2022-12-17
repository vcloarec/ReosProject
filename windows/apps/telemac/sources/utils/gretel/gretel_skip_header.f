!                       *****************************
                        SUBROUTINE GRETEL_SKIP_HEADER
!                       *****************************
!
     &(FU,NPOIN,NVALUE,ERR,LU)
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
!| ERR            |---|
!| FU             |---|
!| LU             |---|
!| NPOIN          |---|
!| NVALUE         |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(OUT) :: NPOIN
      INTEGER, INTENT(IN) :: FU,ERR,LU,NVALUE
      INTEGER NELEM,ECKEN,NDUM,NBV1,I,NPLAN
      INTEGER PARAM(10)
!
!  1 : SKIPS TITLE
!
      READ(FU,ERR=999)
!
!  2 : READS NBV1
!
      READ(FU,ERR=999) NBV1
      IF (NBV1.NE.NVALUE) THEN
        WRITE(LU,*)  'NBV1.NE.NVALUE! CHECK OUTPUT FILES ...'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!  3 : SKIPS NAMES AND UNITS OF THE VARIABLES
!
      DO I=1,NBV1
        READ(FU,ERR=999)
      END DO
!
!  4 : 10 PARAMETERS
!
      READ(FU,ERR=999) (PARAM(I),I=1,10)
      NPLAN=PARAM(7)
!  READS THE DATE (OPTIONAL) AND WRITES IT OUT
      IF(PARAM(10).EQ.1) THEN
        READ(FU,ERR=999)  (PARAM(I),I=1,6)
      ENDIF
!
!  5 : 4 PARAMETERS
!
      READ(FU,ERR=999) NELEM,NPOIN,ECKEN,NDUM
!
!  6 : IKLE
!
      READ(FU,ERR=999)
!
 999  RETURN
      END SUBROUTINE GRETEL_SKIP_HEADER
