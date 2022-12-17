!                   *******************
                    SUBROUTINE GRADNODT
!                   *******************
!
     &(NS,NT,NU,AIRT,AIRS,T,DPX,DPY,DJX,DJY,DX,DY,MESH)
!
!!***********************************************************************
! TELEMAC2D   V6P3                                   21/07/2013
!***********************************************************************
!
!brief    COMPUTES THE GRADIENTS BY TRIANGLES AND NODE
!+                AND THE DIFFUSION TERM FOR TRACER.
!
!history  INRIA
!+
!+        V5P4
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
!history  R. ATA (EDF R&D-LNHE)
!+        13/04/2013
!+        V6P3
!+   Optimization and parallel implementation
!+   More explicit english comments
!+   Adaptation for new data structure
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AIRS           |-->| CELL'S AREAS
!| AIRT           |-->| TRIANGLES' AREAS
!| CE             |<--| DIFFUSION TERM
!| CVIST          |-->| COEFFICIENT OF TRACER DIFFUSION
!| DIFT           |-->| LOGICAL TO SAY IF THERE IS TRACER DIFFUSION OR NO
!| DJX,DJY        |<--| GRADIENTS PER TRIANGLE
!| DT             |<->| TIME STEP
!| DX,DY          |<--| GRADIENTS AT NODES
!| MVIST          |-->| MODEL FOR TRACER DIFFUSION
!| NS             |-->| TOTAL NUMER OF NODES IN THE MESH
!| NT             |-->| TOTAL NUMBER OF ELEMENTS IN THE MESH
!| NU             |-->| NUMBERING OF NODES IN THE TRIANGLE
!| T              |-->| TRACERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE INTERFACE_TELEMAC2D, EX_GRADNODT => GRADNODT
      USE INTERFACE_PARALLEL, ONLY : P_MIN
      USE DECLARATIONS_SPECIAL
      USE BIEF, ONLY: OV
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NS,NT
      INTEGER, INTENT(IN)             :: NU(NT,3)
      DOUBLE PRECISION, INTENT(IN)    :: DPX(3,NT),DPY(3,NT)
      DOUBLE PRECISION, INTENT(IN)    :: AIRT(NT),AIRS(NS),T(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(NT),DJY(NT),DX(NS),DY(NS)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,JT,NUBO1,NUBO2,NUBO3,NSG
      DOUBLE PRECISION AIRJ,UA1,UA2,UA3,AIS,TEMPOR,TIERS
!
      TIERS = 1.0D0/3.0D0
!-----------------------------------------------------------------------
!

!     INITIALISES THE HERMITIAN NODAL GRADIENTS
!
      CALL OV('X=0     ',X=DX, DIM1=NS)
      CALL OV('X=0     ',X=DY, DIM1=NS)
!
!     LOOP ON GLOBAL LIST OF TRIANGLES
!
      DO JT=1,NT
!
        NUBO1 = NU(JT,1)
        NUBO2 = NU(JT,2)
        NUBO3 = NU(JT,3)
!
        AIRJ = AIRT(JT)
!
!       COMPUTES THE P1-GRADIENTS
!
        UA1=T(NUBO1)
        UA2=T(NUBO2)
        UA3=T(NUBO3)
!
!       GRADIENTS BY TRIANGLES
!
        DJX(JT) = UA1*DPX(1,JT) +
     &            UA2*DPX(2,JT) + UA3*DPX(3,JT)
        DJY(JT) = UA1*DPY(1,JT) +
     &            UA2*DPY(2,JT) + UA3*DPY(3,JT)
!
!       GRADIENTS BY NODES
!
        TEMPOR    = AIRJ*DJX(JT)
        DX(NUBO1) = DX(NUBO1) + TEMPOR
        DX(NUBO2) = DX(NUBO2) + TEMPOR
        DX(NUBO3) = DX(NUBO3) + TEMPOR
!
        TEMPOR    = AIRJ*DJY(JT)
        DY(NUBO1) = DY(NUBO1) + TEMPOR
        DY(NUBO2) = DY(NUBO2) + TEMPOR
        DY(NUBO3) = DY(NUBO3) + TEMPOR
!
      ENDDO ! RA: SEPARATION OF LOOPS TO EXECUTE PARCOM
!
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM2(DX,DY,DY,NS,1,2,2,MESH)
      ENDIF
!
!     COMPLETES THE COMPUTATION OF THE NODAL GRADIENTS
!
      DO IS=1,NS
        AIS     = TIERS/AIRS(IS)
        DX(IS)  = DX(IS)*AIS
        DY(IS)  = DY(IS)*AIS
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
