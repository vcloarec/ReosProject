!                   *********************
                    SUBROUTINE PRE4_MUMPS
!                   *********************
!
     &(NPOIN,NSEGB,GLOSEGB,DAB1,DAB2,DAB3,DAB4,XAB1,XAB2,XAB3,XAB4,
     & XX1,XX2,CVB1,CVB2,TYPEXT1,TYPEXT2,TYPEXT3,TYPEXT4,
     & KNOLG,NPOIN_TOT)
!
!***********************************************************************
! MUMPSVOID   V7P1
!***********************************************************************
!
!brief    CALLS THE DIRECT SOLVER MUMPS.
!+        IF MUMPS IS NOT INSTALLED : EMPTY SUBROUTINES ARE USED INSTEAD.
!
!history  E. RAZAFINDRAKOTO (LNH)
!+        20/11/2006
!+
!+
!
!history  C. DENIS (SINETICS)
!+        14/10/2009
!+        V5P9
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        23/02/2015
!+        V7P1
!+   Arguments TYPEXT1,2,3,4 added, for calling SD_FABSG4
!
!history  S.E.BOURBAN (HRW)
!+        21/08/2015
!+        V7P1
!+   Final tweak to the arguments of SOLVE_MUMPS_PAR for linkage with
!+   MUMPS ... It now works !
!+   Although letting MUMPS recreate a new split of the mesh is not
!+   helpful at this stage.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CVB1,CVB2      |-->| SECONDS MEMBRES
!| DAB1           |---|
!| DAB2           |---|
!| DAB3           |---|
!| DAB4           |---|
!| GLOSEGB        |---|
!| NPOIN          |-->| NOMBRE D'INCONNUES
!| NSEGB          |-->| NOMBRE DE SEGMENTS
!| TYPEXT         |-->| TYPE OF EXTRA-DIAGONAL TERMS OF MATRICES
!| XAB1           |---|
!| XAB2           |---|
!| XAB3           |---|
!| XAB4           |---|
!| XX1,XX2        |<--| SOLUTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC, ONLY : GLOSEG4_P4M,XA_P4M,DA_P4M,
     &                                 RHS_P4M,XINC_P4M,SIZE_RHS_P4M,
     &                                 SIZE_DA_P4M,SIZE_XA_P4M,
     &                                 SIZE_XINC_P4M,SIZE_GLOSEG4_P4M
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NSEGB,NPOIN_TOT
      INTEGER, INTENT(IN) :: GLOSEGB(NSEGB*2),KNOLG(*)
      DOUBLE PRECISION, INTENT(IN)    :: DAB1(NPOIN),DAB2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: DAB3(NPOIN),DAB4(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XAB1(2*NSEGB),XAB2(2*NSEGB)
      DOUBLE PRECISION, INTENT(IN)    :: XAB3(2*NSEGB),XAB4(2*NSEGB)
      DOUBLE PRECISION, INTENT(INOUT) :: XX1(NPOIN),XX2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: CVB1(NPOIN),CVB2(NPOIN)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT1,TYPEXT2
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT3,TYPEXT4
!
#if defined HAVE_MUMPS
      INTEGER NPBLK,NSEGBLK,I
!
!-----------------------------------------------------------------------
!
      NPBLK=NPOIN*2
      NSEGBLK=4*NSEGB+NPOIN
!
      IF(SIZE_GLOSEG4_P4M.EQ.0) THEN
        ALLOCATE(GLOSEG4_P4M(2*NSEGBLK))
        SIZE_GLOSEG4_P4M=    2*NSEGBLK
      ELSEIF(            2*NSEGBLK.GT.SIZE_GLOSEG4_P4M) THEN
        DEALLOCATE(GLOSEG4_P4M)
        ALLOCATE(GLOSEG4_P4M(2*NSEGBLK))
        SIZE_GLOSEG4_P4M=    2*NSEGBLK
      ENDIF
      IF(SIZE_DA_P4M.EQ.0) THEN
        ALLOCATE(DA_P4M(NPBLK))
        SIZE_DA_P4M=    NPBLK
      ELSEIF(       NPBLK.GT.SIZE_DA_P4M) THEN
        DEALLOCATE(DA_P4M)
        ALLOCATE(DA_P4M(NPBLK))
        SIZE_DA_P4M=    NPBLK
      ENDIF
      IF(SIZE_XA_P4M.EQ.0) THEN
        ALLOCATE(XA_P4M(2*NSEGBLK))
        SIZE_XA_P4M=    2*NSEGBLK
      ELSEIF(       2*NSEGBLK.GT.SIZE_XA_P4M) THEN
        DEALLOCATE(XA_P4M)
        ALLOCATE(XA_P4M(2*NSEGBLK))
        SIZE_XA_P4M=    2*NSEGBLK
      ENDIF
      IF(SIZE_RHS_P4M.EQ.0) THEN
        ALLOCATE(RHS_P4M(NPBLK))
        SIZE_RHS_P4M=    NPBLK
      ELSEIF(        NPBLK.GT.SIZE_RHS_P4M) THEN
        DEALLOCATE(RHS_P4M)
        ALLOCATE(RHS_P4M(NPBLK))
        SIZE_RHS_P4M=    NPBLK
      ENDIF
      IF(SIZE_XINC_P4M.EQ.0) THEN
        ALLOCATE(XINC_P4M(NPBLK))
        SIZE_XINC_P4M=    NPBLK
      ELSEIF(         NPBLK.GT.SIZE_XINC_P4M) THEN
        DEALLOCATE(XINC_P4M)
        ALLOCATE(XINC_P4M(NPBLK))
        SIZE_XINC_P4M=    NPBLK
      ENDIF
!
!-----------------------------------------------------------------------
!
!     1. SECOND MEMBRE DU SYSTEME
!     ===========================
!
      DO I=1,NPOIN
        RHS_P4M(I)      = CVB1(I)
        RHS_P4M(I+NPOIN)= CVB2(I)
      ENDDO
!
!     2. CONSTRUCTION STOCKAGE SEGMENT MATRICE BLOCK (DE 4)
!     =====================================================
!
      CALL SD_STRSG4(NPOIN,NSEGB,GLOSEGB,NSEGBLK,GLOSEG4_P4M)
!
      CALL SD_FABSG4(NPOIN,NSEGB,DAB1,DAB2,DAB3,DAB4,
     &               XAB1,XAB2,XAB3,XAB4,NPBLK,NSEGBLK,DA_P4M,XA_P4M,
     &               TYPEXT1,TYPEXT2,TYPEXT3,TYPEXT4)
!
!     3. RESOLUTION : APPEL A MUMPS
!     =============================
!

!
      CALL SOLVE_MUMPS_PAR(NPBLK,NSEGBLK,GLOSEG4_P4M,NSEGBLK,DA_P4M,
     &                     XA_P4M,XINC_P4M,RHS_P4M,'Q',KNOLG,
     &                     NPOIN_TOT)
!
!     4. RECUPERATION DES INCONNUES
!     =============================
!
      DO I=1,NPOIN
        XX1(I)= XINC_P4M(I)
        XX2(I)= XINC_P4M(I+NPOIN)
      ENDDO
!
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
