!                   *******************************
                    SUBROUTINE T2D_WAC_CPL_UPDATE(PART)
!                   *******************************
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    Update data exhanged with tomawac
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        USE DECLARATIONS_TOMAWAC, ONLY : CPL_WAC_DATA
        USE DECLARATIONS_TELEMAC2D, ONLY : NIT, PERCOU_WAC, U, V, H,
     &         DIRMOY, HM0, TPR5, ORBVEL, FXWAVE, FYWAVE, T1, T2,
     &         DT, AT
        USE METEO_TELEMAC, ONLY : WINDX, WINDY
        IMPLICIT NONE
        INTEGER,           INTENT(IN)      :: PART
!
        CPL_WAC_DATA%NIT_TEL = NIT
        CPL_WAC_DATA%PERCOU_WAC = PERCOU_WAC
        CPL_WAC_DATA%U_TEL => U
        CPL_WAC_DATA%V_TEL => V
        CPL_WAC_DATA%H_TEL => H
        CPL_WAC_DATA%DIRMOY_TEL => DIRMOY
        CPL_WAC_DATA%HM0_TEL => HM0
        CPL_WAC_DATA%TPR5_TEL => TPR5
        CPL_WAC_DATA%ORBVEL_TEL => ORBVEL
        CPL_WAC_DATA%FX_WAC => FXWAVE
        CPL_WAC_DATA%FY_WAC => FYWAVE
        CPL_WAC_DATA%UV_TEL => WINDX
        CPL_WAC_DATA%VV_TEL => WINDY
        CPL_WAC_DATA%DT_TEL = DT
        IF (PART.EQ.0) THEN
          CPL_WAC_DATA%AT_TEL = AT 
        ELSE
          CPL_WAC_DATA%AT_TEL = AT -DT
        ENDIF
!
      END SUBROUTINE
