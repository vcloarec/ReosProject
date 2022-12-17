!                   *******************
                    SUBROUTINE MASBAS2D
!                   *******************
!
     &(VOLU2D,V2DPAR,UNSV2D,IELM,MESH,MSK,MASKEL,T1,S)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES VARIOUS VOLUMES OF 2D BASIS AND THE INVERSE.
!
!history  J-M HERVOUET (LNHE)
!+        14/06/2006
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
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+   COMPENSATED THE V2DPAR VECTORS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IELM           |-->| TYPE OF ELEMENT (11 FOR LINEAR)
!| MASKEL         |-->| ARRAY OF MASKS, PER ELEMENT
!| MESH           |<->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKING, MASKEL IS TO BE USED
!| S              |-->| EMPTY BIEF_OBJ STRUCTURE
!| T1             |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| UNSV2D         |<->| INVERSE OF INTEGRAL OF TEST FUNCTIONS
!|                |   | WITHOUT MASKING
!| V2DPAR         |<->| AS VOLU2D IF NOT PARALLEL
!|                |   | IN PARALLEL COMPLETED WITH OTHER SUBDOMAINS
!| VOLU2D         |<->| INTEGRAL OF TEST FUNCTIONS, WITH MASKING
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : MODASS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: IELM
      LOGICAL, INTENT(IN)            :: MSK
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: VOLU2D,V2DPAR,UNSV2D,T1
      TYPE(BIEF_OBJ) , INTENT(IN)    :: MASKEL,S
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VOLU2D : VOLUME WITH POSSIBLE MASKING
!
      CALL VECTOR(VOLU2D,'=','MASBAS          ',IELM,1.D0,
     &            S,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     V2DPAR : LIKE VOLU2D BUT IN PARALLEL VALUES COMPLETED AT
!              INTERFACES BETWEEN SUBDOMAINS
!
      CALL OS('X=Y     ',X=V2DPAR,Y=VOLU2D)
      IF(NCSIZE.GT.1 ) THEN
        IF(MODASS .EQ. 1) CALL PARCOM(V2DPAR,2,MESH)
        IF(MODASS .EQ. 3)
     &    CALL PARCOM_COMP(V2DPAR,V2DPAR%E,2,MESH)
      ENDIF
      IF(MODASS .EQ. 3) THEN
        V2DPAR%R=V2DPAR%R+V2DPAR%E
      ENDIF
!
!     INVERSE OF VOLUMES (DONE WITHOUT MASKING), THERE SHOULD BE
!     NO DIVISION BY ZERO, UNLESS ELEMENT WITH NO AREA
!
      IF(MSK) THEN
        CALL VECTOR(T1,'=','MASBAS          ',IELM,1.D0,
     &              S,S,S,S,S,S,MESH,.FALSE.,MASKEL)
        IF(NCSIZE.GT.1) CALL PARCOM(T1,2,MESH)
        CALL OS('X=1/Y   ',X=UNSV2D,Y=T1)
      ELSE
        CALL OS('X=1/Y   ',X=UNSV2D,Y=V2DPAR)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
