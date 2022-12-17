!                   ***************************
                    SUBROUTINE SPALART_ALLMARAS
!                   ***************************
!
     &(U,V,VISCSA,DT,NUN,NUTILD,PROPNU,IELMNU,SLVNU,
     & INFOSA, MSK, MASKEL, MASKPT, NPTFR, LIMSA, NUBOR, S, UCONV,
     & VCONV, ICONV,MAS,DIF,SM,CM2,T3,T1,T2,MESH,TB,T4,
     & WDIST,NUMIN,NUMAX,YAFLULIM,FLULIM,YAFLULIMEBE,FLULIMEBE,
     & TE1,TE2,YASMH)
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    DIFFUSION STEP FOR SOURCE TERMS (SPALART-ALLMARAS MODEL).
!
!history  A BOURGOIN (LNHE) & R. ATA(LNHE)
!+        31/08/2016
!+        V7P2
!+     First version.
!
!history  J-M HERVOUET (JUBILADO)
!+        06/11/2016
!+        V7P3
!+     Calling CVTRVF_NERD instead of CVTRVF_NERD_2 for the only
!+     variable to be advected. Dummy variables FLULIM, FLULIMEBE and
!+     YAFLULIMEBE added.
!
!history R.ATA & A. BOURGOIN
!      29/07/2016
!      V7P3
!      new adaptation of constants used for the model
!      they seem to give better results
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CM2            |<->| MATRIX
!| DIF            |<->| DIFFUSION MATRIX
!| DT             |-->| TIME STEP
!| FLULIM         |-->| FLUX LIMITATION, PER SEGMENT.
!| FLULIMEBE      |-->| FLUX LIMITATION, PER SEGMENT BUT EBE.
!| ICONV          |-->| TYPE OF ADVECTION
!|                |   |   1 : CHARACTERISTICS
!|                |   |   2 : SUPG, ...
!| IELMNU         |-->| TYPE OF ELEMENT FOR VISCSA
!| INFOSA         |-->| IF YES, INFORMATION ON LINEAR SYSTEMS
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| LIMSA          |-->| BOUNDARY CONDITIONS ON VISCSA
!| MAE            |<->| MATRIX FOR EPSILON EQUATION
!| MAS            |<->| MATRIX FOR VISCSA EQUATION
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUBOR          |-->| SPALART ALLAMRAS VISCOSITY ON BOUNDARIES
!| NUMIN,NUMAX    |-->| LIMITS FOR CLIPPING OF NU
!| NUN            |-->| SPALART ALLAMRAS VISCOSITY AT TIME T(N)
!| NUTILD         |-->| SPALART ALLAMRAS VISCOSITY AFTER ADVECTION
!| PROPNU         |-->| KINEMATIC VISCOSITY
!| S              |-->| VOID STRUCTURE
!| SLVNU          |-->| STRUCTURE WITH SOLVER OPTIONS FOR VISCSA
!| SM             |<--| RIGHT-HAND SIDE OF EQUATION
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T5             |<->| WORK BIEF_OBJ STRUCTURE
!| TB             |<->| BLOCK OF WORK ARRAYS
!| TE1            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TE2            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| UCONV          |-->| X-COMPONENT OF ADVECTION VELOCITY FIELD
!| VCONV          |-->| Y-COMPONENT OF ADVECTION VELOCITY FIELD
!| VISCSA         |<--| SPALART ALL. VISCOSITY (NU_TILDE) AT TIME T(N+1)
!| WDIST          |-->| DISTANCE TO THE SOLID BOUNDARIES
!| YAFLULIM       |-->| IF YES TAKE FLULIM INTO ACCOUNT
!| YAFLULIMEBE    |-->| IF YES TAKE FLULIMEBE INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D, ONLY : H,HN,HPROP,TB2,DM1,
     &   ZCONV,SOLSYS,SMH,UNSV2D,V2DPAR,VOLU2D,OPTSOU,FLBOR,
     &   FLBORTRA,MASKTR,OPTADV_SA,AM2,DEBUG,NCO_DIST,NSP_DIST,
     &   MAXADV
      USE INTERFACE_TELEMAC2D, EX_SPALART_ALLMARAS => SPALART_ALLMARAS
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: IELMNU,NPTFR,ICONV
      INTEGER         , INTENT(INOUT) :: LIMSA(NPTFR)
      LOGICAL         , INTENT(IN)    :: INFOSA, MSK,YAFLULIM,YASMH
      LOGICAL         , INTENT(IN)    :: YAFLULIMEBE
      DOUBLE PRECISION, INTENT(IN)    :: DT, PROPNU,NUMIN,NUMAX
      TYPE(SLVCFG)    , INTENT(INOUT) :: SLVNU
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: U,V,NUN,FLULIM,FLULIMEBE
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: MASKEL, MASKPT, S
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: UCONV, VCONV,WDIST
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: VISCSA,T4,NUTILD,NUBOR
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: SM,T3,T1,T2
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TB,TE1,TE2
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
!     MATRIX STRUCTURES
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: MAS,DIF,CM2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: I,NPOIN,DIMGLO,IOPT,N
      DOUBLE PRECISION :: CB1, CB2, KAP, CW1, CW2, CW3, CV1, FV2
      DOUBLE PRECISION :: FW, R, G, CHI, CHI3, D, SIG, C
      DOUBLE PRECISION :: AGGLOSA, SL1,FV1,BS,TSV,TS,FT2,KAP2
      DOUBLE PRECISION :: CT3,CT4,CB22,TETASA,MASSSA
      DOUBLE PRECISION :: CBK,CV13,CW6,PCW,UNSS,DISTB
!     WATER DINAMIC VISCOSITY
      DOUBLE PRECISION, PARAMETER :: PROPNU2=1.D-6
!
!-----------------------------------------------------------------------
!
      INTRINSIC MAX,MIN,EXP
!
!-----------------------------------------------------------------------
!
      DIMGLO=MESH%GLOSEG%DIM1
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE MASS MATRIX AND THE DIFFUSION MATRIX
!     ------------------------------------------------
!
!
        IF (DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-0'
!
! SETS THE CONSTANTS (TO BE MOVED TO COSASA.F
        KAP=0.41D0
        SIG=3.D0/2.D0 !1/SIG
        CB1= 0.1355D0 ! 0.105D0
        CB2= 0.622D0 ! 0.462D0
        CW1=CB1/KAP**2.D0+(1.D0+CB2)*SIG
        CW2=0.3D0  !0.1215D0
        CW3=2.D0
        CV1=7.1D0
        AGGLOSA=1.D0
        SL1=1.D0/DT
!        C2=0.7D0
!        C3=0.9D0
        CT3=1.2D0
        CT4=0.5D0
!        CR1=0.5D0
!        PROPNU2=1.D-6
        NPOIN=U%DIM1
!
!     -----------------------------
!     COMPUTES THE MASS MATRIX
!     -----------------------------
!
      CALL MATRIX(MAS,'M=N     ','MATMAS          ',IELMNU,IELMNU,
     &            SL1, S, S, S, S, S, S, MESH, MSK, MASKEL)
!
!     MASS-LUMPING TEST
!
!      AGGLOSA = 1.D0
!      IF(AGGLOSA.GT.0.001D0) THEN
!        CALL LUMP(T1,MAS,MESH,AGGLOSA)
!        CALL OM('M=CM    ', M=MAS, C=1.D0-AGGLOSA, MESH=MESH)
!        CALL OM('M=M+D   ', M=MAS, D=T1, MESH=MESH)
!      ENDIF
!
      IF (DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-1'
!     --------------------------------------------------
!     CONCATENATES THE MASS MATRIX: IN T3
!     --------------------------------------------------
!
      CALL LUMP(T3,MAS,MESH,DT)
!
!     ---------------------
!     DIFFUSION MATRIX
!     ---------------------
!
      CALL OS('X=Y+C   ', X=T1, Y=VISCSA, C=PROPNU)
      CALL MATRIX(DIF,'M=N     ','MATDIF          ', IELMNU, IELMNU,
     &            1.D0, S, S, S, T1, T1, T1, MESH, MSK, MASKEL)
      IF(DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-2'
!
!***********************************************************************
!
!     EXPLICIT SOURCE TERMS: T1 FOR VISCSA
!
!                                  N
!                              VISCA
!                              --   +    PROD + DEST + DIFFUSION
!                              DT
!
!
!      PROD      = Cb1(1-ft2)*S_TILDE*NU_TILDE
!      DEST      = (Cw1*Fw-Cb1*ft2/Kappa**2)*(NU_TILDE/D)**2
!                  HERE THERE IS AN IMPLICIT PART (SEE BELOW)
!      DIFFUSION = (1*SIGMA)*(d((NU+NU_TILDE)dNU_TILDE)) +
!                             ----           ---------
!                             dx_j            dx_j
!
!                  Cb2*dNU_TILDE * dNU_TILDE
!                      ---------   ---------
!                      dx_j         dX_j
!***********************************************************************
!
!     --------------------------------
!     TAKES ADVECTION INTO ACCOUNT
!     --------------------------------
!
      IF(ICONV.EQ.ADV_CAR) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-31'
        CALL MATVEC('X=AY    ',SM,MAS,NUTILD,C,MESH)
        IF(DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-31'
!
      ELSEIF(ICONV.EQ.ADV_SUP) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-32'
        CALL MATVEC('X=AY    ',SM,MAS,NUN,C,MESH)
!       CENTRED SEMI-IMPLICIT ADVECTION TERM : MATRIX
        CALL MATRIX(CM2,'M=N     ','MATVGR          ',IELMNU,IELMNU,
     &              1.D0,S,S,S,UCONV,VCONV,VCONV,MESH,MSK,MASKEL)
!       SUPG CONTRIBUTION
        IF(OPTADV_SA.EQ.1) THEN
!         CLASSICAL SUPG
          CALL KSUPG(TE1,TE2,1.D0,UCONV,VCONV,MESH)
          CALL MATRIX(CM2,'M=M+N   ','MASUPG          ',IELMNU,IELMNU,
     &                1.D0,TE1,TE2,S,UCONV,VCONV,VCONV,
     &                MESH,MSK,MASKEL)
        ELSEIF(OPTADV_SA.EQ.2) THEN
!         MODIFIED SUPG
          CALL MATRIX(CM2,'M=M+N   ','MAUGUG          ',IELMNU,IELMNU,
     &                0.5D0*DT,S,S,S,UCONV,VCONV,VCONV,
     &                MESH,MSK,MASKEL)
        ENDIF
!       END OF SUPG CONTRIBUTION
!       EXPLICIT RIGHT-HAND SIDES
        TETASA=0.6
        IF(DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-321'
        CALL MATVEC( 'X=X+CAY ',SM,CM2,NUN,TETASA-1.D0,MESH)
!       ADDS SUPG MATRIX TO MAS
        CALL OM('M=X(M)  ', M=MAS, MESH=MESH)
        CALL OM('M=M+CN  ', M=MAS, N=CM2, C=TETASA, MESH=MESH)
        IF(DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-322'
!
      ELSEIF(ICONV.EQ.ADV_LPO.OR.
     &       ICONV.EQ.ADV_NSC.OR.
     &       ICONV.EQ.ADV_PSI     ) THEN
!
        IF(ICONV.EQ.ADV_LPO) IOPT=2
        IF(ICONV.EQ.ADV_NSC) IOPT=2
        IF(ICONV.EQ.ADV_PSI) IOPT=3
        CALL OS('X=0     ',X=SM)

!       NO MASS BALANCE WILL BE DONE, SO DUMMY VALUE
        MASSSA=0.D0
!       PROVISIONAL: SM=0 GIVEN FOR FSCEXP (NO TURBULENCE AT SOURCES...)
!
!                              FSCEXP
        CALL CVTRVF(NUTILD,NUN,SM   ,H,HN,HPROP,
     &              UCONV,VCONV,DM1,ZCONV,SOLSYS,
!                                            SMI YASMI
     &              SM ,SMH,YASMH,S,.FALSE.,
     &              NUBOR,MASKTR,MESH,AGGLOSA,DT,INFOSA,
     &              MSK,MASKEL,S,MASSSA,OPTSOU,
!                                                          YAFLBOR
     &              LIMSA      ,KDIR,KDDL,MESH%NPTFR,FLBOR,.TRUE.,
     &              VOLU2D,V2DPAR,UNSV2D,IOPT,FLBORTRA,MASKPT,
!                     RAIN  PLUIE  TRAIN
     &              .FALSE.,  S ,  0.D0 ,OPTADV_SA,TB,12,AM2,TB2,
     &              NCO_DIST,NSP_DIST, YAFLULIM,FLULIM%R,
     &              YAFLULIMEBE,FLULIMEBE%R,SLVNU)
!
        CALL MATVEC('X=AY    ',SM,MAS,NUTILD,C,MESH)
      IF(DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-33'
!
      ELSEIF(ICONV.EQ.ADV_LPO_TF.OR.
     &       ICONV.EQ.ADV_NSC_TF.OR.
     &       ICONV.EQ.ADV_PSI_TF     ) THEN
!
        IF(ICONV.EQ.ADV_LPO_TF) IOPT=2
        IF(ICONV.EQ.ADV_NSC_TF) IOPT=2
        IF(ICONV.EQ.ADV_PSI_TF) IOPT=3
        CALL OS('X=0     ',X=SM)
!       NO MASS BALANCE WILL BE DONE, SO DUMMY VALUE
        MASSSA=0.D0
!       PROVISIONAL: SM =0 GIVEN FOR FSCEXP (NO TURBULENCE AT SOURCES...)
!
        IF(ICONV.EQ.ADV_LPO_TF.OR.ICONV.EQ.ADV_NSC_TF) THEN
          IF(TB%N.LT.20) THEN
            WRITE(LU,*) 'SIZE OF TB TOO SMALL IN CVDFTR'
            CALL PLANTE(1)
            STOP
          ENDIF
!
          CALL CVTRVF_NERD(NUTILD,NUN,SM,
     &                     H,HN,HPROP,UCONV,VCONV,S,S,1,SM,SM,
     &                     .FALSE.,S,.FALSE.,NUBOR,MASKTR,MESH,
     &                     TB%ADR(13)%P,TB%ADR(14)%P,TB%ADR(15)%P,
     &                     TB%ADR(16)%P,TB%ADR(17)%P,TB%ADR(18)%P,
     &                     TB%ADR(19)%P,TB%ADR(20)%P,
     &                     DT,INFOSA,MSK,MASKEL,1,LIMSA,
!                                                     YAFLBOR
     &                     KDIR,KDDL,MESH%NPTFR,FLBOR,.TRUE.,
     &                     UNSV2D,IOPT,TB%ADR(11)%P,
     &                     MESH%GLOSEG%I(       1:  DIMGLO),
     &                     MESH%GLOSEG%I(DIMGLO+1:2*DIMGLO),
     &                     MESH%NBOR%I,FLULIM%R,YAFLULIM,
!                           RAIN PLUIE TRAIN
     &                     .FALSE.,S  , 0.D0,S,.FALSE.,MAXADV)
!
        ELSE
!         SCHEME 15
!                                     FSCEXP (IF YASMH, HERE GIVEN FALSE)
          IF(TB%N.LT.20) THEN
            WRITE(LU,*) 'SIZE OF TB TOO SMALL IN KEPSIL'
            WRITE(LU,*) 'FOR CALLING CVTRVF_POS_2'
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL CVTRVF_ERIA(NUTILD,NUN,S,H,HN,HPROP,UCONV,VCONV,
     &                     DM1,ZCONV,SOLSYS,SM,S,.FALSE.,S,.FALSE.,
     &                     NUBOR,MASKTR,MESH,
     &                     TB%ADR(13)%P,TB%ADR(14)%P,TB%ADR(15)%P,
     &                     TB%ADR(16)%P,TB%ADR(17)%P,TB%ADR(18)%P,
     &                     TB%ADR(19)%P,TB%ADR(20)%P,
     &                     DT,INFOSA,MSK,MASKEL,1,
     &                     LIMSA   ,
!                                                     YAFLBOR
     &                     KDIR,KDDL,MESH%NPTFR,FLBOR,.TRUE.,
     &                     UNSV2D,IOPT,TB%ADR(11)%P,
!                                       RAIN   PLUIE
     &                     MESH%NBOR%I,.FALSE.,S,0.D0,
     &                     MAXADV,NCO_DIST,OPTADV_SA)
        ENDIF
        CALL MATVEC('X=AY    ',SM,MAS,NUTILD,C,MESH)

      ELSE
!
        WRITE(LU,101) ICONV
101     FORMAT(1X,'SPALART_ALLMARAS: UNKNOWN TYPE OF ADVECTION:',1I4)
        CALL PLANTE(1)
        STOP
!
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-5'
!
!     ------------------------------------------------
!     COMPUTE PRODUCTION
!     ------------------------------------------------
!
!     1- COMPUTE S=SQRT(2*VORTICITY:VORTICITY) (STOCKED IN T4)
!
      CALL CPSTVC(U,T4)
      CALL VECTOR(T4,'=','PRSAF           ',IELMNU,
     &            1.D0,S,S,S,U,V,S,MESH,MSK,MASKEL)
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM (T4, 1, MESH)
      ENDIF
      CALL OS('X=Y/Z   ' , X=T4 , Y=T4 , Z=T3)
      CALL OS('X=SQR(Y)' , X=T4 , Y=T4)
      IF (DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-6'
!
!     2- COMPUTE FIRST PRODUCTION TERM PROD1 (STOCKED IN T1)
!
!      DO I=1, NPOIN
!        CHI=VISCSA%R(I)/MAX(PROPNU2,1.D-10) !+CR1*KS/D
!        FT2=CT3*EXP(-CT4*CHI**2.D0)
!        T1%R(I)=CB1*T4%R(I)*VISCSA%R(I)*(1.D0-FT2)
!      ENDDO
!      IF (DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-7'
!
!     ADD IT TO SM
!
!      CALL OS('X=X+Y   ', X=SM, Y=T1)
!
!      CALL OS('X=YZ    ', X=T5, Y=VISCSA, Z=T3)
!
!     3- COMPUTE 2ND PRODUCTION TERM PROD2 (STOCKED IN T2)
!
      CV13 = CV1**3
      KAP2 = KAP**2
      UNSS = 1.D0/6.D0
      PCW  = (1.D0+CW3**6)**UNSS
      CW6  = CW3**6
      CBK  = CB1/KAP2
      DISTB=SUM(MESH%DISBOR%R)/REAL(NPTFR)
!
      DO I=1, NPOIN
!       D=MAX(WDIST%R(I),1.D-12)!+0.03*KS
        D=WDIST%R(I)+DISTB   !+0.03*KS
        CHI=VISCSA%R(I)/MAX(PROPNU2,1.D-12) !+CR1*KS/D
        FT2=CT3*EXP(-CT4*CHI**2)
        CHI3=CHI**3.D0
        FV1=CHI3/MAX((CHI3+CV13),1.D-12)
        FV2=1.D0-CHI/(1.D0+CHI*FV1)
        BS=VISCSA%R(I)*FV2/(KAP*D)**2
        TSV=(BS+T4%R(I))/UNSV2D%R(I)
        T1%R(I)=CB1*TSV*VISCSA%R(I)*(1.D0-FT2)
        ! DESTRUCTION (IMPLICIT)
!        BS=BS+VISCSA%R(I)*T4%R(I)/MAX(T5%R(I),1.D-12)
        TS=T4%R(I)+BS
        R=MIN(VISCSA%R(I)/(TS*KAP2*D**2),10.D0)
        G=R+CW2*(R**6-R)
        FW=G*PCW/(G**6+CW6)**UNSS
        T2%R(I)=MAX((CW1*FW-CBK*FT2)*VISCSA%R(I)/(MAX(D,1.D-12))**2,
     & 1.D-6)
      ENDDO
!
      IF (DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-8'
!
!     WHILE EVERYTHING IN P2 IS ASSUMED CONSTANT, P2 WILL GIVE
!
!      CALL OS('X=XY    ',X=T1 ,Y=T3)
!     ADD P2 TO SM
      CALL OS('X=X+Y   ',X=SM ,Y=T1)
!     SAME FOR DESTRUCTION
      CALL OS( 'X=XY    ' , X=T2 , Y=T3)
!     BUT IMPLICITLY ==> ADD TO THE DIAG OF MASS MATRIX
      CALL OM('M=M+D   ', M=MAS, D=T2, MESH=MESH)
      IF (DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-9'
!
!
!     4- COMPUTE DIFFUSION
!
!     FIRST PART OF DIFFUSION TERM IS TREATED IMPLICITLY
!        ==> COMBINE MASS AND DIFFUSION MATRICES
      CALL OM('M=M+CN  ', M=MAS, N=DIF, C=SIG, MESH=MESH)
!
!     COMPUTE THE SECOND PART OF DIFFUSION TERMS (EXPLICIT)
!
!AB    CB22=(1.D0+CB2)*SIG
      CB22=CB2*SIG
      CALL VECTOR(T4,'=','TRSAF           ',IELMNU,
     &    CB22,S,S,S,VISCSA,VISCSA,S,MESH,MSK,MASKEL)
!
      CALL OS('X=X+CY   ', X=SM, Y=T4, C=1.D0)
!
!    LIMITS TURBULENCE FOR SHALLOW DEPTHS ( < 1 CM )
!
      DO N=1,SM%DIM1
        SM%R(N) = SM%R(N) * (MIN(HN%R(N),0.01D0)/0.01D0)**2
      ENDDO
      IF (DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-10'
!
!     5- BOUNDARY CONDITIONS
!
      IF(NPTFR.GT.0) THEN
        CALL DIRICH(VISCSA, MAS, SM, NUBOR, LIMSA,
     &              TB, MESH, KDIR, MSK ,MASKPT)
      ENDIF
      IF (DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-11'
      IF (DEBUG.GT.0) WRITE(LU,*) 'IM'
!
!     6- LINEAR SYSTEM RESOLUTION
!
#if defined COMPAD
      CALL AD_SOLVE(VISCSA, MAS, SM, TB, SLVNU, INFOSA, MESH, DIF)
#else
      CALL SOLVE(VISCSA, MAS, SM, TB, SLVNU, INFOSA, MESH, DIF)
#endif
      IF (DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-12'
!
!     UP AND DOWN CLIPPING
!
      CALL CLIP(VISCSA,NUMIN,.TRUE.,NUMAX,.TRUE.,0)
      IF (DEBUG.GT.0) WRITE(LU,*) 'IM IN SPALART_ALLMARAS-13'
!
!***********************************************************************
!
      RETURN
      END
