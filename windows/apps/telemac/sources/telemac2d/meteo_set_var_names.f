!                   **********************************
                    SUBROUTINE METEO_SET_VAR_NAMES
!                   **********************************
     &(U_NAME,V_NAME,P_NAME,T_NAME)
!
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    SET THE NAMES OF THE ATMOSPHERIC DATA
!
!warning  CAN BE ADAPTED BY USER TO USE DIFFERENT NAMES
!
!history  A. LEROY (LNHE)
!+        18/08/2016
!+        V7P2
!+
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| U_NAME         |-->| NAME OF THE WIND ALONG X
!| V_NAME         |-->| NAME OF THE WIND ALONG Y
!| P_NAME         |-->| NAME OF THE ATMOSPHERIC PRESSURE
!| T_NAME         |-->| NAME OF THE AIR TEMPERATURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=16), INTENT(INOUT) :: U_NAME,V_NAME,P_NAME,T_NAME
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      U_NAME ='WIND VELOCITY U '
      V_NAME ='WIND VELOCITY V '
      P_NAME ='SURFACE PRESSURE'
      T_NAME ='AIR TEMPERATURE '
!
!-----------------------------------------------------------------------
!
      RETURN
      END
