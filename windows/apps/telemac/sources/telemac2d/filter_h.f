!                   *******************
                    SUBROUTINE FILTER_H
!                   *******************
!
     &(VEC,T1,MESH,MSK,MASKEL,N,FLODEL,YAFLODEL,DT,W1,UNSV2D)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SMOOTHES NEGATIVE DEPTHS AND COMPUTES CORRESPONDING
!+                FLUXES IN THE EQUATION OF CONTINUITY.
!
!history  J-M HERVOUET (LNHE)
!+        20/05/2008
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
!history  J-M HERVOUET (LNHE)
!+        16/07/2012
!+        V6P2
!+   FLODEL no longer initialised, due to a different use.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP IN SECONDS
!| FLODEL         |<->| FLUXES ALONG SEGMENTS
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| N              |-->| OPERATION WILL BE REPEATED N TIMES.
!| T1             |-->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| UNSV2D         |-->| 1/(INTEGRAL OF TEST FUNCTIONS)
!| VEC            |<->| VECTOR TO BE FILTERED
!| W1             |<->| WORK ARRAY
!| YAFLODEL       |-->| LOGICAL INDICATING IF FLODEL MUST BE UPDATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: N
      DOUBLE PRECISION, INTENT(IN)  :: DT
      LOGICAL, INTENT(IN)           :: MSK,YAFLODEL
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VEC,T1,FLODEL,W1
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,UNSV2D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,NELEM
!
!-----------------------------------------------------------------------
!
      IF(YAFLODEL) CALL OV('X=C     ', X=W1%R, C=0.D0,
     &                     DIM1=3*MESH%NELMAX)
!
      DO I=1,N
!
!-----------------------------------------------------------------------
!
!     COMPUTES FLUXES DUE TO SMOOTHING (SEE RELEASE NOTES 5.9)
!
      IF(YAFLODEL) THEN
        NELEM=MESH%NELEM
        CALL SMOOTHING_FLUX(-1.D0/DT,VEC,VEC%R,MESH%SURFAC%R,
     &                      MESH%IKLE%I(      1  :  NELEM),
     &                      MESH%IKLE%I(NELEM+1  :2*NELEM),
     &                      MESH%IKLE%I(2*NELEM+1:3*NELEM),
     &                      NELEM,MESH%NELMAX,W1%R)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE MATRIX/VECTOR PRODUCT OF MASS X VEC
!
      CALL VECTOR(T1 ,'=','MASVEC          ',VEC%ELM,
     &            1.D0,VEC,VEC,VEC,VEC,VEC,VEC,MESH,MSK,MASKEL)
      IF(NCSIZE.GT.1) CALL PARCOM(T1,2,MESH)
!
!-----------------------------------------------------------------------
!
!     DIVIDES BY THE MASS OF THE BASES: F = M * F / (ASSEMBLED M)
!
      CALL OS('X=YZ    ',X=VEC,Y=T1,Z=UNSV2D)
!
!-----------------------------------------------------------------------
!
      ENDDO ! I
!
!     TAKES FLUXES DUE TO THE SMOOTHING OF NEGATIVE DEPTHS INTO ACCOUNT
!     FLODEL NOT INITIALISED
!
      IF(YAFLODEL) THEN
!
        CALL FLUX_EF_VF(FLODEL%R,W1%R,MESH%NSEG,MESH%NELEM,MESH%NELMAX,
     &                  MESH%ELTSEG%I,MESH%ORISEG%I,
     &                  MESH%IKLE%I,.FALSE.,0)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
