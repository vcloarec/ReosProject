!                   *****************
                    SUBROUTINE ZSLOPE
!                   *****************
!
     &(SLOPE,ZF,T1,T2,MSK,MASKEL,IELM,MESH)
!
!***********************************************************************
! SISYPHE   V7P2                                         29/06/2016
!***********************************************************************
!
!brief    COMPUTES THE BOTTOM SLOPE
!+
!
!history  R. ATA (LNHE)
!+        29/06/2016
!+        V7P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IELM           |-->| TYPE OF ELEMENT
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| SLOPE          |<->| BOTTOM SLOPE
!| T1,T2          |-->| WORKING ARRAYS
!| ZF             |-->| BOTTOM ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_ZSLOPE => ZSLOPE
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: IELM
      LOGICAL, INTENT(IN)             :: MSK
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,ZF
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: SLOPE,T1,T2
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     INITIALISES THE SLOPE TO ZERO
!
      CALL CPSTVC(ZF,SLOPE)
      CALL OS('X=0     ',X=SLOPE)
!
!     COMPUTES THE BOTTOM GRADIENT (COMPONENTS STOCKED IN T1 AND T2 )
!
      CALL VECTOR(T1,'=','GRADF          X',IELM,
     &            1.D0,ZF,ZF,ZF,ZF,ZF,ZF,MESH,MSK,MASKEL)
      CALL VECTOR(T2,'=','GRADF          Y',IELM,
     &            1.D0,ZF,ZF,ZF,ZF,ZF,ZF,MESH,MSK,MASKEL)
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(T1,2,MESH)
        CALL PARCOM(T2,2,MESH)
      ENDIF
!
!     NORM OF THE GRADIENT
!
      CALL OS('X=N(Y,Z)' ,X=SLOPE,Y=T1,Z=T2)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
