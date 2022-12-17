!                   **********************
                    SUBROUTINE USER_CORRXY
!                   **********************
!
     & (X,Y,NPOIN)
!
!***********************************************************************
! BIEF
!***********************************************************************
!
!brief    MODIFIES THE COORDINATES OF THE POINTS IN THE MESH.
!
!warning  DO NOT PERFORM ROTATIONS AS IT WILL CHANGE
!+            THE NUMBERING OF THE LIQUID BOUNDARIES
!
!history  EMILE RAZAFINDRAKOTO (LNHE)
!+        17/10/05
!+        V5P6
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| X              |<->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |<->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_USER_CORRXY => USER_CORRXY
!
!     OTHER DATA ARE AVAILABLE WITH THE DECLARATIONS OF EACH PROGRAM
!
!     USE DECLARATIONS_TELEMAC2D
!     USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN),Y(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION DEGTORAD
      INTRINSIC ACOS
!
!-----------------------------------------------------------------------
!
!     THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO
!     THE CALLING PROGRAM AND THE NEEDED MODIFICATION
!     BY ADDING USE DECLARATIONS_"NAME OF CALLING CODE"
!     ALL THE DATA STRUCTURE OF THIS CODE IS AVAILABLE
!
!-----------------------------------------------------------------------
!
!      EXAMPLE 1: MULTIPLIES BY A CONSTANT (SCALES THE MESH)
!                 AND CHANGES THE ORIGIN
!
!
      IF(.FALSE.) THEN
        DO I = 1 , NPOIN
          X(I) = 3.D0 * X(I) + 100.D0
          Y(I) = 5.D0 * Y(I) - 50.D0
        ENDDO
      ENDIF
!
!
!-----------------------------------------------------------------------
!
!      EXAMPLE 2: CHANGING LATITUDE-LONGITUDE IN DEGREES TO RADIANS
!
!
      IF(.FALSE.) THEN
        DEGTORAD=ACOS(-1.D0)/180.D0
        DO I = 1 , NPOIN
          X(I) = X(I) * DEGTORAD
          Y(I) = Y(I) * DEGTORAD
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     THIS SHOULD BE CHANGED IF MODIFICATIONS ARE DONE
!
      WRITE(LU,*)'USER_CORRXY (BIEF):NO MODIFICATION OF COORDINATES'
      WRITE(LU,*)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
