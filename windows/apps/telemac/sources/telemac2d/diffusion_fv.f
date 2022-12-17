!                   ***********************
                    SUBROUTINE DIFFUSION_FV
!                   ***********************
!
     &(CE,FLUXT,IVIS,FLBOR,
     & W,DJX,DJY,DX,DY,DJXT,DJYT,DXT,DYT)
!
!***********************************************************************
! TELEMAC2D   V8P3
!***********************************************************************
!
!brief    PARSER TO CALL THE DIFFUSION ROUTINE.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  CE         FLUX AT TIME N
!>@param  [in,out]  FLUXT      FLUX FOR TRACER AT TIME N
!>@param  [in]      IVIS       OPTION FOR THE DIFFUSION OF VELOCITIES
!>@param  [in]      FLBOR      BOUNDARY MASS FLUXES
!>@param  [in]      W          WORKING TABLE
!>@param  [in,out]  DJX        GRADIENT PER TRIANGLE
!>@param  [in,out]  DJY        GRADIENT PER TRIANGLE
!>@param  [in,out]  DX         GRADIENTS AT NODES
!>@param  [in,out]  DY         GRADIENTS AT NODES
!>@param  [in,out]  DJXT       TRACER GRADIENTS PER TRIANGLES
!>@param  [in,out]  DJYT       TRACER GRADIENTS PER TRIANGLES
!>@param  [in,out]  DXT        TRACER GRADIENTS AT THE NODES
!>@param  [in,out]  DYT        TRACER GRADIENTS AT THE NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY: KDIR,KNEU,KDDL,KLOG,KSORT,
     &                                KENT,KENTU
      USE DECLARATIONS_TELEMAC2D, ONLY:MESH,IKLE,U,V,TN,NPOIN,NPTFR,
     &                                 PROPNU,MVISUV,MVIST,NTRAC,DIFNU,
     &                                 DIFT,LIMTRA,LITBOR,LIUBOR,
     &                                 TBOR,UBOR,VBOR,VISC,ALRTPF,
     &                                 V2DPAR,NELMAX,OPTRTPF,NELEM
      USE INTERFACE_TELEMAC2D, EX_DIFF_FV => DIFFUSION_FV
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: IVIS
      DOUBLE PRECISION, INTENT(IN)    :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(3,NPOIN),DY(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(3,NELEM),DJY(3,NELEM)
      DOUBLE PRECISION, INTENT(INOUT) :: DXT(NPOIN),DYT(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DJXT(NELEM),DJYT(NELEM)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NPOIN,3)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLUXT
      TYPE(BIEF_OBJ), INTENT(IN)      :: FLBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ITRAC,K,IS
      INTEGER CLT(NPTFR),LIDU(NPTFR),LIDV(NPTFR)
      DOUBLE PRECISION FLUX(NPOIN)
      DOUBLE PRECISION TDBOR(NPTFR),DIFNUT(NPOIN)
      DOUBLE PRECISION DXUTEMP(NPOIN),DYUTEMP(NPOIN)
      DOUBLE PRECISION DXVTEMP(NPOIN),DYVTEMP(NPOIN)
!
!     ********************
!     DIFFUSION ON U AND V
!     ********************
      IF(IVIS.NE.0.AND.PROPNU.NE.0.D0) THEN
!
!       SET BOUNDARY CONDITION FOR THE DIFFUSION OF VELOCITY
        DO K=1,NPTFR
          LIDU(K) = KNEU
          LIDV(K) = KNEU
!
!         IMPOSED VELOCITY AND INLET
          IF((LIUBOR%I(K).EQ.KENT).OR.(LIUBOR%I(K).EQ.KENTU)) THEN
            IF(FLBOR%R(K).LT.0.D0) THEN
              LIDU(K) = KDIR
              LIDV(K) = KDIR
            ENDIF
          ENDIF
        ENDDO
!
!       FOR SECOND ORDER
        IF(OPTRTPF.EQ.2) THEN
          CALL GRADNOD(IKLE%I,W,MESH%DPX%R,MESH%DPY%R,DJX,DJY,DX,DY)
        ENDIF
        DO I=1,NPOIN
          DXUTEMP(I) = DX(2,I)
          DYUTEMP(I) = DY(2,I)
          DXVTEMP(I) = DX(3,I)
          DYVTEMP(I) = DY(3,I)
        ENDDO
!
!       COMPUTE DIFFUSION FLUX ON U
        CALL DIFFUSION_FLUX_FV(IKLE%I,U%R,MESH%DPX%R,MESH%DPY%R,FLUX,
     &                         MESH%VNOIN%R,MESH%ELTSEG%I,MESH%NUBO%I,
     &                         MESH%IFABOR%I,MVISUV,
     &                         VISC%R,MESH%COORDR%R,ALRTPF%R,
     &                         LIDU,UBOR%R,DXUTEMP,DYUTEMP)
        DO I=1,NPOIN
          CE(I,2) = CE(I,2) + FLUX(I)
        ENDDO
!       COMPUTE DIFFUSION FLUX ON V
        CALL DIFFUSION_FLUX_FV(IKLE%I,V%R,MESH%DPX%R,MESH%DPY%R,FLUX,
     &                         MESH%VNOIN%R,MESH%ELTSEG%I,MESH%NUBO%I,
     &                         MESH%IFABOR%I,MVISUV,
     &                         VISC%R,MESH%COORDR%R,ALRTPF%R,
     &                         LIDV,VBOR%R,DXVTEMP,DYVTEMP)
        DO I=1,NPOIN
          CE(I,3) = CE(I,3) + FLUX(I)
        ENDDO
      ENDIF
!
!     **************
!     DIFFUSION ON T
!     **************
      IF(NTRAC.GT.0.AND.DIFT) THEN
        DO ITRAC=1,NTRAC
!
          DO I=1,NPOIN
            DIFNUT(I) = DIFNU(ITRAC)
          ENDDO

!         SET BOUNDARY CONDITION FOR THE DIFFUSION OF TRACERS
          DO K=1,NPTFR
            IS = MESH%NBOR%I(K)
            CLT(K) = LITBOR%ADR(ITRAC)%P%I(K)
            TDBOR(K) = TBOR%ADR(ITRAC)%P%R(K)
!
!           IMPOSED TRACER VALUE
            IF(CLT(K).EQ.KENT) THEN
!             IF OUTLET, TRACER IMPOSED AT THE LAST VALUE
              IF(FLBOR%R(K).GT.0.D0) THEN
                TDBOR(K) = TN%ADR(ITRAC)%P%R(IS)
                CLT(K) = KENT
              ENDIF
!
!           FREE TRACER VALUE
            ELSEIF(CLT(K).EQ.KSORT) THEN
!             IF INLET, TRACER IMPOSED AT THE LAST VALUE
              IF(FLBOR%R(K).LT.0.D0) THEN
                TDBOR(K) = TN%ADR(ITRAC)%P%R(IS)
                CLT(K) = KENT
              ENDIF
            ENDIF
!
!           FROM PHYSICAL TO TECHNICAL CONDITIONS
            IF(CLT(K).EQ.KENT ) THEN
              LIMTRA%I(K) = KDIR
            ELSEIF(CLT(K).EQ.KSORT) THEN
              LIMTRA%I(K) = KDDL
            ELSEIF(CLT(K).EQ.KLOG ) THEN
              LIMTRA%I(K) = KNEU
            ELSE
!             ERROR, UNKNOWN VALUE OF LITBOR
              WRITE(LU,12) K, CLT(K)
12            FORMAT(1X,'DIFFIN: POINT ',1I6,' LITBOR= ',1I6,' ?????')
              CALL PLANTE(1)
              STOP
            ENDIF

          ENDDO
!
!         FOR SECOND ORDER
          IF(OPTRTPF.EQ.2) THEN
            CALL GRADNODT(NPOIN,NELMAX,IKLE%I,MESH%SURFAC%R,V2DPAR%R,
     &                    TN%ADR(ITRAC)%P%R,MESH%DPX%R,MESH%DPY%R,
     &                    DJXT,DJYT,DXT,DYT,MESH)
          ENDIF
!
!         COMPUTE DIFFUSION FLUX ON TRACER
          CALL DIFFUSION_FLUX_FV(IKLE%I,TN%ADR(ITRAC)%P%R,MESH%DPX%R,
     &                           MESH%DPY%R,FLUX,MESH%VNOIN%R,
     &                           MESH%ELTSEG%I,MESH%NUBO%I,
     &                           MESH%IFABOR%I,
     &                           MVIST(ITRAC),DIFNUT,
     &                           MESH%COORDR%R,ALRTPF%R,
     &                           LIMTRA%I,TDBOR,DXT,DYT)
          DO I=1,NPOIN
            FLUXT%ADR(ITRAC)%P%R(I) = FLUXT%ADR(ITRAC)%P%R(I) + FLUX(I)
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
