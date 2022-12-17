!                   *****************
                    SUBROUTINE VGFPSI
!                   *****************
!
     &(RES,IELM,U,V,F,DT,XMUL,CFLMAX,T1,T2,MESH,MSK,MASKEL)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SAME AS VGRADF BUT WITH THE PSI SCHEME AND
!+                SUB-ITERATIONS TO REACH STABILITY.
!
!history  J-M HERVOUET (LNHE)
!+        18/02/08
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
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CFLMAX         |<--| MAXIMUM CFL NUMBER
!| DT             |-->| TIME-STEP
!| F              |-->| FUNCTION F
!| IELM           |-->| TYPE OF ELEMENT OF RESULT
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| RES            |<--| RESULTING VECTOR
!| T1             |<->| WORK ARRAY IN BIEF_OBJ STRUCTURE
!| T2             |<->| WORK ARRAY IN BIEF_OBJ STRUCTURE
!| U              |-->| X-COMPONENT OF ADVECTION FIELD
!| V              |-->| Y-COMPONENT OF ADVECTION FIELD
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VGFPSI => VGFPSI
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX
      IMPLICIT NONE
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER, INTENT(IN)           :: IELM
      LOGICAL, INTENT(IN)           :: MSK
!
      DOUBLE PRECISION, INTENT(IN)  :: DT,XMUL
      DOUBLE PRECISION, INTENT(OUT) :: CFLMAX
!
!     STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(IN)      :: U,V,F,MASKEL
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: RES,T1,T2
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER IT,NIT
      DOUBLE PRECISION DDT
!
      INTRINSIC INT
!
!-----------------------------------------------------------------------
!
!     NUMBER OF NECESSARY SUB-ITERATIONS
!
      CALL CFLPSI(T1,U,V,DT,IELM,MESH,MSK,MASKEL)
      CALL MAXI(CFLMAX,IT,T1%R,BIEF_NBPTS(IELM,MESH))
      IF(NCSIZE.GT.1) CFLMAX=P_MAX(CFLMAX)
!
      NIT = INT(CFLMAX) + 1
!     WRITE(LU,*) 'VGFPSI : NIT=',NIT,' CFLMAX=',CFLMAX
!
      IF(NIT.GT.100) THEN
        WRITE(LU,901) NIT
901     FORMAT(1X,'VGFPSI: ',1I6,' SUB-ITERATIONS REQUIRED FOR THE'
     &   ,/,1X,   '        PSI SCHEME. DECREASE THE TIME-STEP')
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DDT = DT/NIT
!
!     T1 WILL TAKE THE SUCCESSIVE VALUES OF F
      CALL OS( 'X=Y     ' , X=T1 , Y=F )
      IF(NIT.GT.1) THEN
        CALL VECTOR(MESH%T,'=','MASBAS          ',IELM,
     &              1.D0,F,F,F,F,F,F,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) CALL PARCOM(MESH%T,2,MESH)
        CALL OS('X=1/Y   ',X=MESH%T,Y=MESH%T,
     &          IOPT=2,INFINI=0.D0,ZERO=1.D-8)
      ENDIF
!
!     LOOP ON THE SUB-ITERATIONS
!
      CALL CPSTVC(F,RES)
!
      DO IT=1,NIT
!
        IF(NIT.GT.1) THEN
          IF(IT.EQ.1) CALL OS('X=0     ',X=RES)
          CALL VECTOR(T2,'=','VGRADF       PSI',IELM,
     &                XMUL,T1,T1,T1,U,V,V,MESH,MSK,MASKEL)
          CALL OS('X=X+CY  ',X=RES,Y=T2,C=1.D0/NIT)
          IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
          CALL OS('X=X+CYZ ',X=T1,Y=T2,Z=MESH%T,C=-DDT/XMUL)
        ELSE
          CALL VECTOR(RES,'=','VGRADF       PSI',IELM,
     &                XMUL,T1,T1,T1,U,V,V,MESH,MSK,MASKEL)
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
