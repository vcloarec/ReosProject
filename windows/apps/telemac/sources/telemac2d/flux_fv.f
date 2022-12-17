!                       ******************
                        SUBROUTINE FLUX_FV
!                       ******************
!
     &(W,LIMPRO,NUBO,VNOIN,FLUX,FLUSORT,FLUENT,FLBOR,ELTSEG,IFABOR,
     & FLUHTEMP,NEISEG,UBOR,VBOR)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief    Compute fluxes for finite volume numerical schemes
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      ELTSEG     SEGMENT NUMBERS PER ELEMENT
!>@param  [in,out]  FLBOR      IN AND OUT WATER MASS FLUX
!>@param  [in,out]  FLUHBOR    BORD FLUX FOR TRACER
!>@param  [in,out]  FLUHTEMP   FLUX FOR TRACER
!>@param  [in,out]  FLUXT      FLUX FOR TRACER
!>@param  [in,out]  FLUENT     MASS FLUX INLET FROM TN TO TN+1
!>@param  [in,out]  FLUSORT    MASS FLUX OUTLET FROM TN TO TN+1
!>@param  [in,out]  FLUX       FLUX
!>@param  [in]      IFABOR     ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!>@param  [in]      LIMPRO     TYPES OF BOUNDARY CONDITION
!>@param  [in]      NEISEG     NEIGHBORS OF SEGMENT (FOR LIMITER)
!>@param  [in]      NUBO       GLOBAL INDICES OF EDGE EXTREMITIES
!>@param  [in]      VNOIN      NORMAL TO THE INTERFACE
!>@param  [in,out]  W          WORKING TABLE CONTAINING H,HU,HV
!>@param  [in,out]  UBOR       PRESCRIBED VALUES ON BOUNDARIES FOR U
!>@param  [in,out]  VBOR       PRESCRIBED VALUES ON BOUNDARIES FOR V
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY : BNDCIN,NPOIN,NPTFR,MESH,NELEM,
     &                                   NSEG,ICIN
      USE INTERFACE_TELEMAC2D, EX_FLUX_FV => FLUX_FV
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: LIMPRO(NPTFR,6)
      INTEGER, INTENT(IN) :: NUBO(2,*)
      INTEGER, INTENT(IN) :: ELTSEG(NELEM,3),IFABOR(NELEM,*)
      INTEGER, INTENT(IN) :: NEISEG(2,NSEG)
      DOUBLE PRECISION, INTENT(IN) :: VNOIN(3,*)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(NPOIN,3),FLUSORT,FLUENT
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FLBOR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FLUHTEMP
      TYPE(BIEF_OBJ), INTENT(INOUT) :: UBOR,VBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL HYD_FV(NUBO,W,VNOIN,ELTSEG,FLUX,IFABOR,FLUHTEMP,NEISEG)
!
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM2(FLUX(:,1),FLUX(:,2),FLUX(:,3),NPOIN,1,2,3,MESH)
      ENDIF
!
      IF((ICIN.EQ.1).AND.(BNDCIN.EQ.1)) THEN
        CALL CDL_CIN(LIMPRO,W,FLUX,FLUENT,FLUSORT,FLBOR,
     &               UBOR,VBOR)
      ELSE
        CALL CDL_FV(LIMPRO,W,FLUX,FLUENT,FLUSORT,FLBOR,UBOR,
     &              VBOR)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
