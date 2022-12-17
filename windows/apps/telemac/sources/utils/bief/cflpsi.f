!                   *****************
                    SUBROUTINE CFLPSI
!                   *****************
!
     &(SYGMA,U,V,DT,IELM,MESH,MSK,MASKEL)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COURANT NUMBER AT EACH POINT OF THE MESH
!+                AND FOR EACH TIMESTEP.
!
!warning  THE COORDINATES ARE HERE GIVEN BY ELEMENTS
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
!+        07/05/2014
!+        V7P0
!+   Correction in //, SYGMA was not assembled.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP.
!| IELM           |-->| TYPE OF ELEMENT.
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| SYGMA          |<--| COURANT NUMBER.
!| U              |-->| VELOCITY ALONG X.
!| V              |-->| VELOCITY ALONG Y.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CFLPSI => CFLPSI
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: SYGMA
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: U,V,MASKEL
      DOUBLE PRECISION, INTENT(IN)    :: DT
      INTEGER         , INTENT(IN)    :: IELM
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      LOGICAL         , INTENT(IN)    :: MSK
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     MASS OF THE BASES IN BIEF WORKING ARRAY
!
      CALL VECTOR(MESH%T,'=','MASBAS          ',
     &            IELM,1.D0,U,U,U,U,U,U,MESH,MSK,MASKEL)
!
      IF(NCSIZE.GT.1) CALL PARCOM(MESH%T,2,MESH)
!
      CALL CPSTVC(MESH%T,SYGMA)
!
!-----------------------------------------------------------------------
!
!     P1 TRIANGLES
!
      IF(IELM.EQ.11) THEN
!
        CALL CFLP11(U%R,V%R,MESH%XEL%R,MESH%YEL%R,
     &              MESH%IKLE%I,MESH%NELEM,MESH%NELMAX,MESH%W%R)
!
!-----------------------------------------------------------------------
!
!     QUASI-BUBBLE TRIANGLES
!
      ELSEIF(IELM.EQ.12) THEN
!
        CALL CFLP12(U%R,V%R,MESH%XEL%R,MESH%YEL%R,
     &              MESH%IKLE%I,MESH%NELEM,MESH%NELMAX,MESH%W%R)
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,101) IELM
101     FORMAT(1X,'CFLPSI: IELM = ',1I6,' CASE NOT IMPLEMENTED.')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
! ASSEMBLES THE LIJ
!
      CALL ASSVEC(SYGMA%R,MESH%IKLE%I,BIEF_NBPTS(IELM,MESH),
     &            MESH%NELEM,MESH%NELMAX,
     &            MESH%W%R,.TRUE.,MESH%LV,MSK,MASKEL%R,
     &            BIEF_NBPEL(IELM,MESH))
      IF(NCSIZE.GT.1) CALL PARCOM(SYGMA,2,MESH)
!
!-----------------------------------------------------------------------
!
! FINAL RESULT, DIVIDES BY THE MASS OF THE BASES
!
      CALL OS('X=CY/Z  ', X=SYGMA, Y=SYGMA, Z=MESH%T, C=DT,
     &                    IOPT=2, INFINI=0.D0, ZERO=1.D-6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
