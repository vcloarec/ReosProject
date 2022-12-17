!                   *****************
                    SUBROUTINE MASKTO
!                   *****************
!
     &(MASKEL,MASKPT,IFAMAS,IKLE,IFABOR,ELTSEG,NSEG,
     & NELEM,IELM,MESH)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CALLED AFTER MASKEL HAS BEEN FILLED IN.
!+
!+          - FILLS IN MASKPT (A POINT IS MASKED IF SURROUNDED BY
!+                MASKED ELEMENTS).
!+
!+          - FILLS IN IFAMAS (SIMILAR ARRAY TO IFABOR, BUT
!+                CONSIDERS ANY FACE SEPARATING A MASKED FROM A
!+                NON-MASKED ELEMENT SUCH AS A SOLID BOUNDARY).
!+
!+          - COMPUTES THE NEW COMPATIBLE NORMALS XNEBOR AND
!+                YNEBOR FOR PRISMS.
!
!history  J-M HERVOUET (LNHE)
!+        21/10/08
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTSEG         |-->| SEGMENTS GIVEN PER ELEMENT
!| IELM           |-->| TYPE OF ELEMENT.
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID
!|                |   | BOUNDARY
!| IFAMAS         |<--| LIKE IFABOR BUT WITH MASK
!| IKLE           |-->| CONNECTIVITY TABLE.
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MESH           |-->| MESH STRUCTURE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MASKTO => MASKTO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER, INTENT(IN)    :: NELEM,IELM,NSEG
      INTEGER, INTENT(IN)    :: IKLE(NELEM,3),IFABOR(NELEM,3)
      INTEGER, INTENT(IN)    :: ELTSEG(NELEM,3)
      INTEGER, INTENT(INOUT) :: IFAMAS(NELEM,3)
!
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELEM)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: MASKPT
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER IELEM,N,I1,I2,I3
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: WSEG
!
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
      WSEG => MESH%MSEG%X%R
!
      CALL OS('X=0     ',X=MASKPT)
!
      IF(IELM.EQ.11.OR.IELM.EQ.41) THEN
!
! 1) MASKS THE POINTS WHICH DO NOT BELONG TO NON-FROZEN ELEMENTS
!    (THOSE WHICH BELONG TO A 'NORMAL' ELEMENT ARE SET BACK TO 1)
!
        DO IELEM = 1,NELEM
          I1 = IKLE(IELEM,1)
          I2 = IKLE(IELEM,2)
          I3 = IKLE(IELEM,3)
          MASKPT%R(I1) = MAX(MASKPT%R(I1),MASKEL(IELEM))
          MASKPT%R(I2) = MAX(MASKPT%R(I2),MASKEL(IELEM))
          MASKPT%R(I3) = MAX(MASKPT%R(I3),MASKEL(IELEM))
        ENDDO
!
!       IN PARALLEL MODE FOR INTERFACE POINTS, MAXIMUM RETAINED
!
        IF(NCSIZE.GT.1) CALL PARCOM(MASKPT,3,MESH)
!
! 2) COPIES IFABOR IN IFAMAS
!
        DO IELEM = 1,NELEM
          IFAMAS(IELEM,1) = IFABOR(IELEM,1)
          IFAMAS(IELEM,2) = IFABOR(IELEM,2)
          IFAMAS(IELEM,3) = IFABOR(IELEM,3)
        ENDDO
!
! 3) IDENTIFIES THE EDGES OF FROZEN ELEMENTS WITH 0 (LIQUID BOUNDARY)
!    TO STOP THE CHARACTERISTIC CURVES
!
!    USES AN ARRAY DEFINED BY SEGMENT TO COMMUNICATE IN PARALLEL MODE
!
!       WSEG SET TO 1
!
        DO N=1,NSEG
          WSEG(N)=1.D0
        ENDDO
!
!       THEN WSEG PUT TO 0 FOR DRY ELEMENTS
!
        DO IELEM=1,NELEM
          IF(MASKEL(IELEM).LT.0.5D0) THEN
            WSEG(ELTSEG(IELEM,1))=0.D0
            WSEG(ELTSEG(IELEM,2))=0.D0
            WSEG(ELTSEG(IELEM,3))=0.D0
          ENDIF
        ENDDO
!
!       IN PARALLEL MODE FOR INTERFACE EDGES, MINIMUM RETAINED
!
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM2_SEG(WSEG,WSEG,WSEG,NSEG,1,4,1,MESH,1,11)
        ENDIF
!
!       WSEG = 0.D0 TRANSLATED INTO IFAMAS = 0
!
        DO IELEM=1,NELEM
          IF(WSEG(ELTSEG(IELEM,1)).LT.0.5D0) IFAMAS(IELEM,1)=0
          IF(WSEG(ELTSEG(IELEM,2)).LT.0.5D0) IFAMAS(IELEM,2)=0
          IF(WSEG(ELTSEG(IELEM,3)).LT.0.5D0) IFAMAS(IELEM,3)=0
        ENDDO
!
      ELSE
!
        WRITE(LU,1100) IELM
1100    FORMAT(1X,'MASKTO: UNKNOWN TYPE OF ELEMENT :',1I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
