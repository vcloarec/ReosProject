!                   *********************
                    SUBROUTINE BIEF_ANIMP
!                   *********************

     &(FORMAT_RES,VARNIM,N,NRES,AT,LT,LDIR,TEXTANIM,NPALE)
!
!***********************************************************************
! BIEF   V7P2                                   December 2016
!***********************************************************************
!
!brief   WRITES THE WAVE HEIGHT AND WAVE PHASE TO RESULT FILE.
!       (TO BE USED TO COMPUTE THE FREE SURFACE ELEVATION AT EVERY POINT)
!
!history  J.PARISI, N.TOZER
!+        08/01/2014
!+        V6P3
!+
!
!history  N.DURAND (HRW)
!+        19/03/2014
!+        V7P0
!+   Simplified. SLF file header with variables and mesh generated outside
!+   of BIEF_DESNIM in CREATE_DATANIM_SERAFIN and WRITE_MESH_SERAFIN
!
!history  N.DURAND (HRW)
!+        Dec 2016
!+        V7P2
!+   Ported to V7P2 with use of Hermes interface to write data
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FORMAT_RES     |-->| FILE FORMAT
!| VARNIM         |-->| BIEF_OBJ BLOCK WITH DATA VALUES
!| N              |-->| NUMBER OF POINTS IN THE MESH
!| NRES           |-->| LOGICAL UNIT OF THE RESULTS FILE
!| AT             |-->| "TIME STAMP" (PERIOD IN THIS CASE)
!| LT             |-->| ITERATION NUMBER
!| LDIR           |-->| DIRECTION NUMBER
!| TEXTANIM       |-->| NAMES AND UNITS OF VARIABLES
!| NPALE          |-->| NUMBER OF DISCRETISED PERIODS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_ANIMP => BIEF_ANIMP
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
! DECLARES TYPES AND DIMENSIONS
!-----------------------------------------------------------------------
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN)          :: FORMAT_RES
      CHARACTER(LEN=32),INTENT(IN)          :: TEXTANIM(*)
      INTEGER,          INTENT(IN)          :: N, NRES
      INTEGER,          INTENT(IN)          :: LT,LDIR
      DOUBLE PRECISION, INTENT(IN)          :: AT
      TYPE(BIEF_OBJ),   INTENT(IN)          :: VARNIM
      INTEGER,          INTENT(IN)          :: NPALE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL              :: FIRST_VAR
      CHARACTER(LEN=32)    :: VAR_NAME
      INTEGER              :: I,IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! IN THIS FILE, TIME STAMPS CORRESPOND TO DIFFERENT PERIOD COMPONENTS
! AND THERE ARE AS MANY (AMPLITUDE,PHASE) PAIRS AS THERE ARE DIRECTION
! COMPONENTS
!
!=======================================================================
!
      FIRST_VAR = .FALSE.
!     NEW "TIME STAMP" PRINTED TO FILE WITH EACH NEW PERIOD
!     SHOULD NOT WRITE AGAIN FOR THE NEXT DIRECTION COMPONENT
      IF (LT .LT. NPALE) THEN
        FIRST_VAR = .TRUE.
      ENDIF
!
!     AMPLITUDE AND PHASE FOR DIRECTION LDIR, PERIOD MOD(LT,NPALE)
!
      DO I = 1,2
        IF(ASSOCIATED(VARNIM%ADR(I)%P%R)) THEN
          VAR_NAME = TEXTANIM(2*(LDIR-1)+I)
          CALL ADD_DATA(FORMAT_RES,NRES,VAR_NAME,AT,MOD(LT,NPALE),
     &                  FIRST_VAR,VARNIM%ADR(I)%P%R,N,IERR)
          CALL CHECK_CALL(IERR,'WRITE_DATASET:ADD_DATA')
          FIRST_VAR = .FALSE.
        ELSE
          WRITE(LU,*) 'WRITE_DATA: VARIABLE NO: ',I
          WRITE(LU,*) '        NOT OR NOT WELL ALLOCATED'
          WRITE(LU,*) '        OR POINTER NOT ASSOCIATED '
        ENDIF
      ENDDO
!
!=======================================================================
!
      RETURN
      END
