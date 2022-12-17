!                         ****************
                          MODULE LAMBERT93
!                         ****************
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    Module to deal with Lambert 93 projection
!+        The conversion formulae use a grid that needs to be stored
!+        but to be unchanged. It may be uploaded dynamically
!
!history  C.-T. PHAM (EDF, LNHE)
!+        31/07/2017
!+        Creation from some subroutines implemented indepently
!
!reference IGN
!+  HARMEL Alain, Tranformation de coordonnees NTF-RGF93. Format de
!+  grille NTV2. IGN, Service de Geodesie et Nivellement. Ref NT/G 111
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      PRIVATE
      PUBLIC :: T0X,T0Y,T0Z,READ_GR3DF97A,INDEX_GR3DF97A,
     &          DEALLOC_GR3DF97A
!
!-----------------------------------------------------------------------
!
      INTEGER, PARAMETER :: NGRID = 17316
!
      DOUBLE PRECISION, ALLOCATABLE :: T0X(:),T0Y(:),T0Z(:)
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!                     ************************
                      SUBROUTINE READ_GR3DF97A
!                     ************************
!
     &(NL93)
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    READS THE THE USEFUL DATA IN THE GR3DF93A GRID
!
!history  C-T PHAM (LNHE)
!+        03/10/2016
!+        V7P3
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NL93           |<--| NUMBER OF CANAL FOR LAMBERT 93 CONVERSION FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NL93
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IDUM,IERR
!
      DOUBLE PRECISION RDUM
!
      CHARACTER TDUM
!
!-----------------------------------------------------------------------
!
!      CALL GET_FREE_ID(NL93)
!      OPEN(NL93,FILE=NOM_93,FORM='FORMATTED',ACTION='READ')
      ALLOCATE(T0X(0:NGRID),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'T0X')
      ALLOCATE(T0Y(0:NGRID),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'T0Y')
      ALLOCATE(T0Z(0:NGRID),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'T0Z')
!
      REWIND(NL93)

      READ(NL93,*)
      READ(NL93,*)
      READ(NL93,*)
      READ(NL93,*)

      DO I=1,NGRID
        READ(NL93,*)IDUM,RDUM,RDUM,T0X(I),T0Y(I),T0Z(I),RDUM,TDUM
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!                   ***************************
                    SUBROUTINE DEALLOC_GR3DF97A
!                   ***************************
!
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    DEALLOCATE THE ARRAYS OF THE USEFUL DATA IN THE GR3DF93A GRID
!
!history  C-T PHAM (LNHE)
!+        03/10/2016
!+        V7P3
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DEALLOCATE(T0X)
      DEALLOCATE(T0Y)
      DEALLOCATE(T0Z)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!                     *************************
                      SUBROUTINE INDEX_GR3DF97A
!                     *************************
!
     &(LONW,LATN,IRSW,IRSE,IRNW,IRNE)
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    COMPUTATION OF THE INDICES TO INTERPOLATE VALUES IN THE
!+        gr3df97a GRID
!
!history  C-T PHAM (LNHE)
!+        03/10/2016
!+        V7P3
!+  Creation from pseudo-code available on the IGN website.
!+  HARMEL Alain, Tranformation de coordonnees NTF-RGF93. Format de
!+  grille NTV2. IGN, Service de Geodesie et Nivellement. Ref NT/G 111
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IRNE           |<--| NORTH-EAST INDEX
!| IRNW           |<--| NORTH-WEST INDEX
!| IRSE           |<--| SOUTH-EAST INDEX
!| IRSW           |<--| SOUTH-WEST INDEX
!| LATN           |-->| LATITUDE  NTF (DECIMAL DEGREES > 0: NORTH)
!| LONW           |-->| LONGITUDE GREENWICH NTF (DECIMAL DEGREES > 0:
!|                |   | EAST)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: LONW
      DOUBLE PRECISION, INTENT(IN) :: LATN
      INTEGER, INTENT(OUT) :: IRSW,IRSE,IRNW,IRNE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, PARAMETER :: S_LAT    = 41.D0
      DOUBLE PRECISION, PARAMETER :: N_LAT    = 52.D0
      DOUBLE PRECISION, PARAMETER :: E_LONG   = 10.D0
      DOUBLE PRECISION, PARAMETER :: W_LONG   = -5.5D0
      DOUBLE PRECISION, PARAMETER :: LAT_INC  = 0.1D0
      DOUBLE PRECISION, PARAMETER :: LONG_INC = 0.1D0
!
      INTEGER M,N,ISW,JSW
!
!-----------------------------------------------------------------------
!
      M = NINT((E_LONG-W_LONG) / LONG_INC) + 1 ! NUMBER OF MERIDIANS
      N = NINT((N_LAT -S_LAT ) / LAT_INC ) + 1 ! NUMBER OF PARALLELS

      ISW = INT((LONW-W_LONG) / LONG_INC) + 1 ! NUMBER OF THE MERIDIAN
                                              ! (SE CORNER) [1,M]
      JSW = INT((LATN-S_LAT ) / LAT_INC ) + 1 ! NUMBER OF THE PARALLEL
                                              ! (SE CORNER) [1,N]

      IRSW = (ISW-1)*N + JSW ! NUMBER OF RECORDING SW
      IRSE = IRSW + N        ! NUMBER OF RECORDING SE
      IRNW = IRSW + 1        ! NUMBER OF RECORDING NW
      IRNE = IRSE + 1        ! NUMBER OF RECORDING NE
!
!     POINT ON THE EASTERN BOUNDARY
      IF(ISW.EQ.M) THEN
        IRNE = IRNW
        IRSE = IRSW
      ENDIF
!     POINT ON THE NORTHERN BOUNDARY
      IF(JSW.EQ.N) THEN
        IRNE = IRSE
        IRNW = IRSW
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      END MODULE LAMBERT93
