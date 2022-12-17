!                     ****************
                      SUBROUTINE CALDT
!                     ****************
!
     &(DT,DTN,LEO)
!
!***********************************************************************
! TELEMAC-2D
!***********************************************************************
!
!>@brief  Computes the time step under CFL condition
!
!>@history  INRIA FOR KINETIC SCHEMES
!!
!!        V5P8
!!
!
!>@history  R. ATA (EDF-LNHE) DT FOR REMAINING SCHEMES
!!        15/03/2010
!!        V6P1
!!   Translation of French comments within the FORTRAN sources into
!!   English comments
!
!>@history  R. ATA (EDF-LNHE)
!!        15/01/2013
!!        V6P3
!!   introduce fixed time step
!!   handle very specific cases
!!   parallelization
!
!>@history  R. ATA (EDF-LNHE) INTRODUCE FIXED TIME STEP
!!        30/06/2013
!!        V6P3
!!  clean and remove unused variables
!
!>@history  R. ATA (EDF-LNHE) INTRODUCE FIXED TIME STEP
!!        11/01/2016
!!        V7P2
!!  adjust time step to graphical outputs
!
!>@history  J,RIEHME (ADJOINTWARE)
!!        November 2016
!!        V7P2
!!   Replaced EXTERNAL statements to parallel functions / subroutines
!!   by the INTERFACE_PARALLEL
!
!>@history  R. ATA (EDF-LNHE)
!!        18/10/ 2018
!!        V78P0
!!   fix a bug with graphical output when coupling with sisyphe
!!   COMPLEO is now incremented only by preres_telemac (like FE)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  DT      TIME STEP
!>@param  [in,out]  DTN     TIME STEP AT PREVIOUS ITERATION
!>@param  [in,out]  LEO     LOGICAL FOR GRAPHICAL OUTPUT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY:NCSIZE
      USE DECLARATIONS_TELEMAC2D,ONLY:DTINI,LEOPRD,PTINIG,GRAV,TORDER,
     &                                LEO_TRAC,NPOIN,ICIN,SORDER,LT,HN,
     &                                U,V,CFLWTD,AT,TMAX,MESH,DTVARI,
     &                                ENTET,DTCAS,LIMPRO,IKLE,MVISUV,
     &                                VISC,NTRAC,DIFNU,MVIST
      USE INTERFACE_TELEMAC2D, EX_CALDT => CALDT
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MIN
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: DT,DTN
      LOGICAL        ,  INTENT(INOUT) :: LEO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,ITRAC
      LOGICAL DEJA,THEEND
      DOUBLE PRECISION RA3,EPSL,SIGMAX,UA2,UA3,UNORM,DTT
      DOUBLE PRECISION RESTE,GPRDTIME,DT2
!
      INTRINSIC MIN,CEILING
!
!-----------------------------------------------------------------------
!
      THEEND=.FALSE.
      IF(LEO.OR.LT.EQ.1) LEO_TRAC = .FALSE.
      LEO   =.FALSE.
!   +++++++++++++++++++++++++
!     VARIABLE TIME STEP
      IF(DTVARI)THEN
!   +++++++++++++++++++++++++
      DEJA=.FALSE.
      DT = DTCAS
      EPSL = 0.01D0
!
!     KINETIC SCHEME
!     ==============
      IF(ICIN.EQ.1)THEN

        RA3 = SQRT(1.5D0*GRAV)
        DO IS=1,NPOIN
          IF(HN%R(IS).LT.0.D0.AND.ENTET.AND..NOT.DEJA)THEN
            WRITE(LU,*) 'CALDT WARNING : NEGATIVE WATER DEPTH'
            WRITE(LU,*) ' SEE NODE:',IS,' FOR EXAMPLE'
            DEJA = .TRUE.
          ELSE
            SIGMAX = ABS(HN%R(IS))
            UA2    = U%R(IS)
            UA3    = V%R(IS)
            UNORM=SQRT(UA2*UA2 + UA3*UA3)
            SIGMAX= MAX(EPSL, RA3*SQRT(SIGMAX) +UNORM )
            DT = MIN(DT, CFLWTD*MESH%DTHAUT%R(IS)/SIGMAX)
          ENDIF
        ENDDO
!
!     SCHEMES OF ROE, ZOKAGOA, TCHAMEN, HLLC AND WAF
!     ==============================================
      ELSEIF(ICIN.EQ.0.OR.ICIN.EQ.2.OR.ICIN.EQ.3.OR.
     &       ICIN.EQ.4.OR.ICIN.EQ.5) THEN
!
        DO IS=1,NPOIN
          IF(HN%R(IS).LT.0.D0.AND.ENTET.AND..NOT.DEJA)THEN
            WRITE(LU,*) 'CALDT WARNING : NEGATIVE WATER DEPTH'
            WRITE(LU,*) ' SEE NODE:',IS,' FOR EXAMPLE'
            DEJA = .TRUE.
          ELSE
            UA2    = U%R(IS)
            UA3    = V%R(IS)
            UNORM=SQRT(UA2*UA2 + UA3*UA3)
            SIGMAX= MAX(EPSL,SQRT(GRAV*ABS(HN%R(IS)))+UNORM)
!           DTHAUT=|Ci|/Sum(Lij)
            DT  = MIN(DT, CFLWTD*MESH%DTHAUT%R(IS)/SIGMAX)
          ENDIF
        ENDDO
!
      ELSE
        WRITE(LU,4020) ICIN
4020    FORMAT(1X,'CALDT: ERROR IN THE CHOICE OF OPTFV: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      CALL CDL_DT(DT,LIMPRO%I)
      CALL DIFF_DT(DT,IKLE%I,MESH%NUBO%I,MESH%ELTSEG%I,MESH%IFABOR%I,
     &             MESH%VNOIN%R,MESH%COORDR%R,MVISUV,VISC%R)
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL DIFF_DT(DT,IKLE%I,MESH%NUBO%I,MESH%ELTSEG%I,
     &                 MESH%IFABOR%I,
     &                 MESH%VNOIN%R,MESH%COORDR%R,MVIST(ITRAC),VISC%R,
     &                 DIFNU(ITRAC))
        ENDDO
      ENDIF
!
!     FOR NEWMARK SCHEME
!     ==================
      IF (TORDER.EQ.2) THEN
        IF (LT.EQ.1) THEN
          DTN = DT
        ELSE
          DT2 = 0.5D0*MIN(DT, DTN)
          DTN = DT
          DT = DT2
        ENDIF
      ENDIF
!
!   +++++++++++++++++++++++++
      ENDIF
!   +++++++++++++++++++++++++
!
!     FOR PARALLELISM
      IF(NCSIZE.GT.1) DT=P_MIN(DT)
!
      IF(DTVARI) THEN
!
        IF(TMAX.LT.DT)DT=TMAX !REALLY CRAZY CASES
        DTT=TMAX-AT
        IF(CFLWTD.GE.1.D0) DT=0.9D0*DT/CFLWTD
        IF(DTT.LE.DT.AND.DTT.GT.0.D0) THEN !LAST TIME STEP
          DT=DTT
!         END OF COMUTATION WILL BE DETECTED IN RESOLU FOR ORDER 2
          IF(SORDER.EQ.1)THEEND=.TRUE.
        ENDIF
        IF(AT.GT.TMAX) THEN
          WRITE(LU,*)'CALDT: BAD TIME PARAMETERS'
          WRITE(LU,*)'TIME AND TMAX',AT,TMAX
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSE
!
!       DT NOT VARIABLE
        WRITE(LU,*) 'ERROR: FIXED TIME-STEP AND CFL NOT GIVEN!...! '
        WRITE(LU,*) 'TIME-STEP MAY NOT SATISFY CFL CONDITION '
        WRITE(LU,*) 'THIS OPTION IS NOT ACCEPTED WITH FINITE VOLUMES '
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!*************************************************************************
!      GRAPHIC OUTPUTS LIKE ASKED BY USER: DT ADAPTATION
!*************************************************************************
        GPRDTIME=LEOPRD*DTINI
        IF(GPRDTIME.LT.1.D-12)THEN
          WRITE(LU,*) 'CALDT: PROBLEM WITH PARAMETERS: DTINI,LEOPRD',
     &                 DTINI,LEOPRD
          CALL PLANTE(1)
          STOP
!       CASE WHERE TIME STEP IS BIGGER THEN GRAPHIC OUTPUT (GPRDTIME)
        ELSEIF(GPRDTIME.LT.DT)THEN
          DT=DTINI
          IF(ENTET)THEN
            WRITE(LU,*) 'WARNING: GRAPHICAL OUTPUT NOT OPTIMIZED: '
            WRITE(LU,*) '   - INITIAL TIME STEP TOO SMALL '
            WRITE(LU,*) '   - AND/OR PERIOD OF GRAPHIC OUTPUT TOO SMALL'
            WRITE(LU,*) 'THIS COULD REDUCE COMPUTATION TIME-STEP'
            WRITE(LU,*) 'AND INCREASE CPU TIME'
          ENDIF
        ENDIF
!       ADAPT DT TO TAKE INTO ACCOUNT GRAPHIC OUTPUT
!       ONLY FOR ORDER 1, ORDER 2 WILL BE DONE IN RESOLU
        IF(SORDER.EQ.1)THEN
          IS=CEILING(AT/GPRDTIME)
          RESTE=IS*GPRDTIME-AT
!
          IF(THEEND.OR.
     &      (RESTE.LE.DT.AND.RESTE.GT.1.D-16.AND.LT.GT.PTINIG))THEN
!           HERE THERE IS GRAPHICAL OUTPUT
            LEO = .TRUE.
            DT=MIN(RESTE,DT)
          ENDIF
        ENDIF
!
!
!*************************************************************************
!     ERROR TREATMENT AND LISTING OUTPUTS
!*************************************************************************
!
!     CASE DT <=0
!
      IF(DT.LE.0.D0) THEN
        WRITE(LU,*) 'NEGATIVE (OR NIL) TIME-STEP: ',DT
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(CFLWTD.GE.1.D0) THEN
        IF(ENTET) THEN
          WRITE(LU,*) 'WARNING: CFL NOT GIVEN OR >1 !...! '
          WRITE(LU,*) 'TIME-STEP (WITH CFL = 0.9): ',DT
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
