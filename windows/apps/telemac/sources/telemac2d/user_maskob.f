!                   **********************
                    SUBROUTINE USER_MASKOB
!                   **********************
!
     &(MASKEL,X,Y,IKLE,NELEM,NELMAX,NPOIN,AT,LT)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FORMALLY REMOVES ELEMENTS FROM THE MESH,
!+                USING THE MASKING ARRAY MASKEL:
!+
!+            MASKEL = 0.D0 FOR MASKED ELEMENTS
!+
!+            MASKEL = 1.D0 FOR'NORMAL' ELEMENTS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| IKLE           |-->| CONNECTIVITY TABLE.
!| LT             |-->| CURRENT TIME STEP
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NPOIN,NELMAX,LT
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*)
!
      DOUBLE PRECISION, INTENT(INOUT) :: MASKEL(NELEM)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN),AT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      WRITE(LU,*) 'SUBROUTINE USER_MASKOB HAS TO BE MODIFIED'
      WRITE(LU,*) 'IF ELEMENTS MASKED BY USER = YES'
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
