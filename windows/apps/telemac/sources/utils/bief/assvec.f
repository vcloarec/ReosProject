!                   *****************
                    SUBROUTINE ASSVEC
!                   *****************
!
     &(X, IKLE,NPOIN,NELEM,NELMAX,W,INIT,LV,MSK,MASKEL,NDP,ERRX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    VECTOR ASSEMBLY.
!
!warning  THIS VECTOR IS ONLY INITIALISED TO 0 IF INIT = .TRUE.
!
!history  J-M HERVOUET (LNH)
!+        29/02/08
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
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+      COMPENSATED ASSEMBLY (MODASS=3)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLE           |-->| CONNECTIVITY TABLE
!| INIT           |-->| LOGICAL : IF TRUE X IS INITIALISED TO 0.
!| LV             |-->| VECTOR LENGTH OF THE COMPUTER
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| FIRST DIMENSION OF IKLE AND W.
!| NPOIN          |-->| NUMBER OF POINTS IN X
!| W              |-->| WORK ARRAY WITH A NON ASSEMBLED FORM OF THE
!|                |   | RESULT
!|                |   | W HAS DIMENSION NELMAX * NDP
!|                |   | NDP IS THE NUMBER OF POINTS IN THE ELEMENT
!| X              |<->| ASSEMBLED VECTOR
!| ERRX           |<->| ERRORS OF ASSEMBLED VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ASSVEC => ASSVEC
      USE DECLARATIONS_TELEMAC, ONLY : MODASS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      INTEGER         , INTENT(IN)    :: NELEM,NELMAX,NPOIN,LV,NDP
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(IN)    :: W(NELMAX,NDP),MASKEL(*)
      LOGICAL         , INTENT(IN)    :: INIT,MSK
      DOUBLE PRECISION,OPTIONAL, INTENT(INOUT) :: ERRX(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IDP,IELEM
      DOUBLE PRECISION ERREUR
      DOUBLE PRECISION TMP
!
!-----------------------------------------------------------------------
!   INITIALISES VECTOR X TO 0 IF(INIT)
!-----------------------------------------------------------------------
!
      IF(INIT) THEN
        CALL OV('X=C     ', X=X, C=0.D0, DIM1=NPOIN)
      ENDIF
!
!-----------------------------------------------------------------------
!   ASSEMBLES, CONTRIBUTION OF LOCAL POINTS 1,... TO NDP
!-----------------------------------------------------------------------
!
!     FpSum
      IF(MODASS.EQ.1) THEN
        DO IDP = 1 , NDP
          CALL ASSVE1(X,IKLE(1,IDP),W(1,IDP),NELEM,NELMAX,LV,MSK,
     &      MASKEL)
        ENDDO
!     CompSum
      ELSEIF(MODASS.EQ.3) THEN
        IF(PRESENT(ERRX)) THEN
          IF(MSK) THEN
            IF(LV.EQ.1) THEN
              DO IDP = 1 , NDP
                DO IELEM = 1 , NELEM
                  ERREUR=0.D0
                  TMP = X(IKLE(IELEM,IDP))
                  CALL TWOSUM(TMP,
     &                W(IELEM,IDP)* MASKEL(IELEM),
     &                X(IKLE(IELEM,IDP)),ERREUR)
!                  CALL TWOSUM(ERRX(IKLE(IELEM,IDP)),ERREUR,
!     &                ERRX(IKLE(IELEM,IDP)),ERREUR)
                  ERRX(IKLE(IELEM,IDP)) = ERRX(IKLE(IELEM,IDP))+ERREUR
                ENDDO
              ENDDO
!            DO IDP = 1 , NDP
!              CALL ASSVE1(X,IKLE(1,IDP),W(1,IDP),NELEM,NELMAX,LV,MSK
!     &          ,MASKEL,NPOIN)
!            ENDDO
            ELSE
              DO IDP = 1 , NDP
                CALL ASSVE1(X,IKLE(1,IDP),W(1,IDP),NELEM,NELMAX,LV,MSK
     &            ,MASKEL)
              ENDDO
            ENDIF
          ELSE
            IF(LV.EQ.1) THEN
              DO IDP = 1 , NDP
                DO IELEM = 1 , NELEM
                  ERREUR=0.D0
                  TMP = X(IKLE(IELEM,IDP))
                  CALL TWOSUM(TMP,W(IELEM,IDP),
     &                X(IKLE(IELEM,IDP)),ERREUR)
!                  CALL TWOSUM(ERRX(IKLE(IELEM,IDP)),ERREUR,
!     &                ERRX(IKLE(IELEM,IDP)),ERREUR)
                  ERRX(IKLE(IELEM,IDP)) = ERRX(IKLE(IELEM,IDP))+ERREUR
                ENDDO
              ENDDO
            ELSE
              DO IDP = 1 , NDP
                CALL ASSVE1(X,IKLE(1,IDP),W(1,IDP),NELEM,NELMAX,LV,MSK
     &          ,MASKEL)
              ENDDO
            ENDIF
          ENDIF
        ELSE
          DO IDP = 1 , NDP
            CALL ASSVE1(X,IKLE(1,IDP),W(1,IDP),NELEM,NELMAX,LV,MSK
     &        ,MASKEL)
          ENDDO
!
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
