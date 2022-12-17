!                   *********************
                    SUBROUTINE DEALL_BIEF
!
!***********************************************************************
! BIEF   V7P0
!***********************************************************************
!
!brief    CLEAN UP THE DATA FROM BIEF
!
!history Y AUDOUIN (LNHE)
!+       21/05/2015
!+       V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MESH           |-->| THE MESH TO BE DEALLOCATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF,EX_DEALL_BIEF=>DEALL_BIEF
      USE STREAMLINE, ONLY: DEALLOC_STREAMLINE
      USE ALGAE_TRANSP, ONLY: DEALLOC_ALGAE
      USE DECLARATIONS_TELEMAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I
!     Old saved variables
!
      ! solve
      IF(.NOT.FIRST_SOLVE) THEN
        CALL BIEF_DEALLOBJ(TBB)
        ! BB contains link to array that are deallocated by the module
        ! using solve
        DO I=1,BB%N
          NULLIFY(BB%ADR(I)%P)
        ENDDO
        CALL BIEF_DEALLOBJ(BB)
        ! Same with BX
        DO I=1,BX%N
          NULLIFY(BX%ADR(I)%P)
        ENDDO
        CALL BIEF_DEALLOBJ(BX)
        FIRST_SOLVE = .TRUE.
      ENDIF
      ! CVTRVF_POS_2
      IF(DEJA_CPOS2) THEN
        DEALLOCATE(INDIC_CPOS2)
        DEJA_CPOS2=.FALSE.
      ENDIF
      ! POSITIVE_DEPTHS
      IF(DEJA_PDEPT_NERD) THEN
        DEALLOCATE(INDIC_PDEPT_NERD)
        DEJA_PDEPT_NERD=.FALSE.
      ENDIF
      IF(DEJA_PDEPT_ERIA) THEN
        DEALLOCATE(INDIC_PDEPT_ERIA)
        DEJA_PDEPT_ERIA=.FALSE.
      ENDIF
      ! CVTRVF_POS
      IF(DEJA_CPOS) THEN
        DEALLOCATE(INDIC_CPOS)
        DEJA_CPOS=.FALSE.
      ENDIF
      ! SD_SOLVE_1
      IF(SIZE_IN.NE.0) THEN
        DEALLOCATE(IN_SS1)
        SIZE_IN = 0
      ENDIF
      IF(SIZE_IP.NE.0) THEN
        DEALLOCATE(IP_SS1)
        SIZE_IP = 0
      ENDIF
      IF(SIZE_ISEGIP.NE.0) THEN
        DEALLOCATE(ISEGIP_SS1)
        SIZE_ISEGIP = 0
      ENDIF
      IF(SIZE_IW1.NE.0) THEN
        DEALLOCATE(IW1_SS1)
        SIZE_IW1 = 0
      ENDIF
      IF(SIZE_INDTRI.NE.0) THEN
        DEALLOCATE(INDTRI_SS1)
        SIZE_INDTRI = 0
      ENDIF
      IF(SIZE_INX.NE.0) THEN
        DEALLOCATE(INX_SS1)
        SIZE_INX = 0
      ENDIF
      IF(SIZE_IPX.NE.0) THEN
        DEALLOCATE(IPX_SS1)
        SIZE_IPX = 0
      ENDIF
      IF(SIZE_AC.NE.0) THEN
        DEALLOCATE(AC_SS1)
        SIZE_AC = 0
      ENDIF
      IF(SIZE_ACTRI.NE.0) THEN
        DEALLOCATE(ACTRI_SS1)
        SIZE_ACTRI = 0
      ENDIF
      IF(SIZE_ISP.NE.0) THEN
        DEALLOCATE(ISP_SS1)
        SIZE_ISP = 0
      ENDIF
      IF(SIZE_RSP.NE.0) THEN
        DEALLOCATE(RSP_SS1)
        SIZE_RSP = 0
      ENDIF
      ! SD_SOLVE_4
      IF(SIZE_GLOSEG4.NE.0) THEN
        DEALLOCATE(GLOSEG4_SS4)
        SIZE_GLOSEG4 = 0
      ENDIF
      IF(SIZE_DA.NE.0) THEN
        DEALLOCATE(DA_SS4)
        SIZE_DA = 0
      ENDIF
      IF(SIZE_XA.NE.0) THEN
        DEALLOCATE(XA_SS4)
        SIZE_XA = 0
      ENDIF
      IF(SIZE_RHS.NE.0) THEN
        DEALLOCATE(RHS_SS4)
        SIZE_RHS = 0
      ENDIF
      IF(SIZE_XINC.NE.0) THEN
        DEALLOCATE(XINC_SS4)
        SIZE_XINC = 0
      ENDIF
      ! PRE4_MUMPS
      IF(SIZE_GLOSEG4_P4M.NE.0) THEN
        DEALLOCATE(GLOSEG4_P4M)
        SIZE_GLOSEG4_P4M = 0
      ENDIF
      IF(SIZE_DA_P4M.NE.0) THEN
        DEALLOCATE(DA_P4M)
        SIZE_DA_P4M = 0
      ENDIF
      IF(SIZE_XA_P4M.NE.0) THEN
        DEALLOCATE(XA_P4M)
        SIZE_XA_P4M = 0
      ENDIF
      IF(SIZE_RHS_P4M.NE.0) THEN
        DEALLOCATE(RHS_P4M)
        SIZE_RHS_P4M = 0
      ENDIF
      IF(SIZE_XINC_P4M.NE.0) THEN
        DEALLOCATE(XINC_P4M)
        SIZE_XINC_P4M = 0
      ENDIF
      ! CHARAC
      IF(DEJA_CHARAC) THEN
        CALL BIEF_DEALLOBJ(T1WEAK)
        CALL BIEF_DEALLOBJ(T2WEAK)
        CALL BIEF_DEALLOBJ(T3WEAK)
        CALL BIEF_DEALLOBJ(T4WEAK)
        CALL BIEF_DEALLOBJ(T5WEAK)
        CALL BIEF_DEALLOBJ(T6WEAK)
        CALL BIEF_DEALLOBJ(T7WEAK)
        CALL BIEF_DEALLOBJ(SHPWEA)
        CALL BIEF_DEALLOBJ(FTILD_WEAK)
        CALL BIEF_DEALLOBJ(SHPBUF)
        CALL BIEF_DEALLOBJ(SHZBUF)
        CALL BIEF_DEALLOBJ(SHZWEA)
        DEJA_CHARAC = .FALSE.
      ENDIF
      ! DERIVE
      IF(DEJA_DERIVE) THEN
        CALL BIEF_DEALLOBJ(SVOID_DERIVE)
        DEJA_DERIVE = .FALSE.
      ENDIF
      IF(.NOT.INIT_ALG) THEN
        DEALLOCATE(BUFF_1D_D)
        DEALLOCATE(BUFF_2D_D)
      ENDIF
!
!     Streamline
!
      CALL DEALLOC_STREAMLINE()

!
!     Algae
!
      CALL DEALLOC_ALGAE()
!
!     Gracjg
!
      GRACJG_CNT = 0

!
!-----------------------------------------------------------------------
!
      RETURN
      END
