!                   *****************
                    SUBROUTINE SMAGOR
!                   *****************
!
     &(VISC,U,V,MESH,T1,T2,T3,T4,MSK,MASKEL,PROPNU)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES VISCOSITY USING SMAGORINSKY'S MODEL.
!code
!+                                     (1/2)
!+    NU    =   CS2 * ( 2.0 * SIJ * SIJ )  * (MESH SIZE)**2
!+
!+                         2        2            2
!+                      DU       DV     DU   DV
!+     2*SIJ*SIJ = ( 2*(--) + 2*(--) + (-- + --)
!+                      DX       DY     DY   DX
!
!history  ADRIAN KLINGS (ENPC)
!+        06/10/1997
!+        V5P6
!+   First version
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
!history  C. Dorfman (user konsonaut)
!+        17/12/2015
!+        V7P1
!+   Missing CALL PARCOM have been added.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| PROPNU         |-->| MOLECULAR DIFFUSION
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| VISC           |-->| TURBULENT DIFFUSION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN) :: MSK
      DOUBLE PRECISION, INTENT(IN)   :: PROPNU
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: VISC,T1,T2,T3,T4
      TYPE(BIEF_OBJ), INTENT(IN)     :: MASKEL,U,V
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,NPOIN,IELMU
      DOUBLE PRECISION CS,CS2
!
!-----------------------------------------------------------------------
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
      CS = 0.1D0
      CS2 = CS**2
!
!-----------------------------------------------------------------------
!
      IELMU = U%ELM
!
!     COMPUTES GRADIENTS (IN FACT AVERAGED GRADIENT MULTIPLIED BY
!     A SURFACE WHICH IS THE INTEGRAL OF TEST FUNCTIONS ON THE DOMAIN,
!     THIS SURFACE IS CONSIDERED TO BE (MESH SIZE)**2)
!
      CALL VECTOR(T1,'=','GRADF          X',IELMU,
     &            1.D0,U,U,U,U,U,U,MESH,MSK,MASKEL)
      CALL VECTOR(T2,'=','GRADF          Y',IELMU,
     &            1.D0,U,U,U,U,U,U,MESH,MSK,MASKEL)
      CALL VECTOR(T3,'=','GRADF          X',IELMU,
     &            1.D0,V,V,V,V,V,V,MESH,MSK,MASKEL)
      CALL VECTOR(T4,'=','GRADF          Y',IELMU,
     &            1.D0,V,V,V,V,V,V,MESH,MSK,MASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(T1,2,MESH)
        CALL PARCOM(T2,2,MESH)
        CALL PARCOM(T3,2,MESH)
        CALL PARCOM(T4,2,MESH)
      ENDIF
!
      NPOIN = VISC%DIM1
!
      DO N = 1,NPOIN
        VISC%R(N)=PROPNU+SQRT((2*T1%R(N)**2+2*T4%R(N)**2
     &                                       +(T2%R(N)+T3%R(N))**2))*CS2
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
