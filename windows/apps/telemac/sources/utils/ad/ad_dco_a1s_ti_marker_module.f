!!!    module AD_DCO_A1S_TI_MARKER_MODULE
!!!     A module that allows to store tape indicess for
!!!       later extraction of intenrmediate adjoints
!!!
!!!       Assumption :  contigouous storage
!!!   JR 2016


#if defined COMPAD    /* do that only for active compilation */

      MODULE DCO_A1S_TI_MARKER_LIST_MODULE
        USE DCO_A1S_COMMON
        IMPLICIT NONE
        TYPE DCO_A1S_TI_MARKER_LIST_ELEM
        INTEGER(DCO_A1S_TAPE_IKND)     :: FRST, SIZE
!           INTEGER                        :: TSTP
        END TYPE DCO_A1S_TI_MARKER_LIST_ELEM
        TYPE DCO_A1S_TI_MARKER_LIST_TYPE
        TYPE(DCO_A1S_TI_MARKER_LIST_ELEM), ALLOCATABLE  :: DATA(:)
        END TYPE DCO_A1S_TI_MARKER_LIST_TYPE
      CONTAINS
        SUBROUTINE DCO_A1S_TI_MARKER_LIST_SETUP( TIM, NIT )
          TYPE(DCO_A1S_TI_MARKER_LIST_TYPE) :: TIM
          INTEGER, INTENT(IN)          :: NIT
          PRINT *,'DCO_A1S_TI_MARKER_LIST_SETUP( ', NIT,' )'
          IF ( NIT < 0 ) THEN
            PRINT *,'ERROR: DCO_A1S_TI_MARKER_LIST_SETUP :: NIT (',
     &            NIT,') < 0 !!'
            STOP
          END IF
          ALLOCATE( TIM%DATA(0:NIT))
          TIM%DATA = DCO_A1S_TI_MARKER_LIST_ELEM(0,0)
        END SUBROUTINE DCO_A1S_TI_MARKER_LIST_SETUP
        SUBROUTINE DCO_A1S_TI_MARKER_LIST_CLEAN( TIM )
          TYPE(DCO_A1S_TI_MARKER_LIST_TYPE) :: TIM
          DEALLOCATE( TIM%DATA )
        END SUBROUTINE DCO_A1S_TI_MARKER_LIST_CLEAN
        SUBROUTINE DCO_A1S_TI_MARKER_LIST_STORE( TIM, TSTP, FRST, SIZE )
          TYPE(DCO_A1S_TI_MARKER_LIST_TYPE) :: TIM
          INTEGER,                     INTENT(IN)  :: TSTP
          INTEGER(DCO_A1S_TAPE_IKND),  INTENT(IN)  :: FRST, SIZE
          TIM%DATA(TSTP) = DCO_A1S_TI_MARKER_LIST_ELEM( FRST, SIZE )
        END SUBROUTINE DCO_A1S_TI_MARKER_LIST_STORE
        SUBROUTINE DCO_A1S_TI_MARKER_LIST_GET( TIM, TSTP, FRST, SIZE )
          TYPE(DCO_A1S_TI_MARKER_LIST_TYPE),INTENT(IN) :: TIM
          INTEGER,                     INTENT(IN)  :: TSTP
          INTEGER(DCO_A1S_TAPE_IKND),  INTENT(OUT)  :: FRST, SIZE
          FRST  = TIM%DATA(TSTP)%FRST
          SIZE  = TIM%DATA(TSTP)%SIZE
        END SUBROUTINE DCO_A1S_TI_MARKER_LIST_GET
        SUBROUTINE DCO_A1S_TI_MARKER_LIST_PRINT( TIM )
          TYPE(DCO_A1S_TI_MARKER_LIST_TYPE),INTENT(IN)  :: TIM
          INTEGER                                  :: I
          PRINT *,'TIMDATA  : ',SIZE(TIM%DATA,1),' elements'
          DO I = 0, SIZE(TIM%DATA)-1
            PRINT *,'TIMDATA ',i,' : ', TIM%DATA(i)%FRST,
     &            ' -> ',TIM%DATA(i)%FRST+TIM%DATA(i)%SIZE,
     &            '  ( ',TIM%DATA(i)%SIZE,' )'
          END DO
        END SUBROUTINE DCO_A1S_TI_MARKER_LIST_PRINT
      END MODULE DCO_A1S_TI_MARKER_LIST_MODULE

      MODULE DCO_A1S_TI_MARKER_MODULE
        USE DCO_A1S_TI_MARKER_LIST_MODULE
        IMPLICIT NONE
        PRIVATE
        PUBLIC :: DCO_A1S_TI_MARKER
        PUBLIC :: DCO_A1S_TI_MARKER_SETUP, DCO_A1S_TI_MARKER_CLEAN
        PUBLIC :: DCO_A1S_TI_MARKER_STORE, DCO_A1S_TI_MARKER_GET
        PUBLIC :: DCO_A1S_TI_MARKER_GET_FIRST
        PUBLIC :: DCO_A1S_TI_MARKER_GET_SIZE
        PUBLIC :: DCO_A1S_TI_MARKER_PRINT
        TYPE(DCO_A1S_TI_MARKER_LIST_TYPE),
     &       DIMENSION(:), ALLOCATABLE :: DCO_A1S_TI_MARKER
      CONTAINS
        SUBROUTINE DCO_A1S_TI_MARKER_SETUP( NTIM, NIT )
          INTEGER, INTENT(IN)          :: NTIM
          INTEGER, INTENT(IN)          :: NIT
          INTEGER                      :: I
          PRINT *,'Called : DCO_A1S_TI_MARKER_SETUP(',NTIM,',',NIT,')'
          IF ( NTIM < 1 ) THEN
            PRINT *,'ERROR: DCO_A1S_TI_MARKER_SETUP :: NTIM (',
     &           NTIM,') < 1 !!'
            STOP
          END IF
          IF ( NIT < 0 ) THEN
            PRINT *,'ERROR: DCO_A1S_TI_MARKER_SETUP :: NIT (',
     &           NIT,') < 0 !!'
            STOP
          END IF
          ALLOCATE( DCO_A1S_TI_MARKER(NTIM) )
          DO I = 1, NTIM
            CALL DCO_A1S_TI_MARKER_LIST_SETUP( DCO_A1S_TI_MARKER(I),
     &                                         NIT )
          END DO
        END SUBROUTINE DCO_A1S_TI_MARKER_SETUP
        SUBROUTINE DCO_A1S_TI_MARKER_CLEAN( )
          INTEGER                      :: I
          PRINT *,'Called : DCO_A1S_TI_MARKER_CLEAN '
          DO I = 1, SIZE(DCO_A1S_TI_MARKER)
            CALL DCO_A1S_TI_MARKER_LIST_CLEAN( DCO_A1S_TI_MARKER(I) )
          END DO
          IF ( ALLOCATED( DCO_A1S_TI_MARKER ) ) THEN
            DEALLOCATE( DCO_A1S_TI_MARKER )
          ENDIF
        END SUBROUTINE DCO_A1S_TI_MARKER_CLEAN
        SUBROUTINE ARG_CHECK( TIM, TSTP, WHO )
          INTEGER,                     INTENT(IN)  :: TIM, TSTP
          CHARACTER(LEN=*),            INTENT(IN)  :: WHO
          IF ( TIM < 1 .OR. TIM > SIZE(DCO_A1S_TI_MARKER,1) ) THEN
            PRINT *,'ERROR: DCO_A1S_TI_MARKER_'//who
     &               //' :: INVALID TIM NUMBER ', TIM, ' !!'
            STOP
          END IF
          IF ( TSTP < 0 .OR.
     &         TSTP > SIZE(DCO_A1S_TI_MARKER(TIM)%DATA,1) ) THEN
            PRINT *,'ERROR: DCO_A1S_TI_MARKER_'//WHO
     &              //' :: INVALID TSTP NUMBER ',TSTP,
     &              '                    !!'
            STOP
          END IF
        END SUBROUTINE ARG_CHECK
        SUBROUTINE DCO_A1S_TI_MARKER_STORE( TIM, TSTP, FRST, SZE )
          INTEGER,                     INTENT(IN)  :: TIM, TSTP
          TYPE(DCO_A1S_TYPE),          INTENT(IN)  :: FRST
          INTEGER(DCO_A1S_TAPE_IKND),  INTENT(IN)  :: SZE
!          INTEGER(DCO_A1S_TAPE_IKND),  INTENT(IN)  :: FRST, sze
          PRINT *,'Called : DCO_A1S_TI_MARKER_STORE(',TIM,',',TSTP,
     &            ',',FRST,',',SZE,')'
          CALL ARG_CHECK( TIM, TSTP, 'STORE' )
          CALL DCO_A1S_TI_MARKER_LIST_STORE( DCO_A1S_TI_MARKER(TIM),
     &                                       TSTP, FRST%J, SZE )
        END SUBROUTINE DCO_A1S_TI_MARKER_STORE
        SUBROUTINE DCO_A1S_TI_MARKER_GET( TIM, TSTP, FRST, SZE )
          INTEGER,                     INTENT(IN)   :: TIM, TSTP
          INTEGER(DCO_A1S_TAPE_IKND),  INTENT(OUT)  :: FRST, SZE
          CALL ARG_CHECK( TIM, TSTP, 'GET' )
          CALL DCO_A1S_TI_MARKER_LIST_GET( DCO_A1S_TI_MARKER(TIM),
     &                                     TSTP, FRST, SZE )
          PRINT *,'Called : DCO_A1S_TI_MARKER_GET(',TIM,',',TSTP,
     &            ',->',FRST,',->',SIZE,')'
        END SUBROUTINE DCO_A1S_TI_MARKER_GET
        FUNCTION DCO_A1S_TI_MARKER_GET_FIRST( TIM, TSTP ) RESULT(RES)
          INTEGER,                     INTENT(IN)   :: TIM, TSTP
          INTEGER(DCO_A1S_TAPE_IKND)                :: FRST, SZE
          INTEGER(DCO_A1S_TAPE_IKND)                :: RES
          CALL ARG_CHECK( TIM, TSTP, 'GET_FIRST' )
          CALL DCO_A1S_TI_MARKER_LIST_GET( DCO_A1S_TI_MARKER(TIM),
     &                                     TSTP, FRST, SZE )
          RES = FRST
          !!PRINT *,'Called : DCO_A1S_TI_MARKER_GET_FIRST(',TIM,',',TSTP,') ->',RES
        END FUNCTION DCO_A1S_TI_MARKER_GET_FIRST
        FUNCTION DCO_A1S_TI_MARKER_GET_SIZE( TIM, TSTP ) RESULT(RES)
          INTEGER,                     INTENT(IN)   :: TIM, TSTP
          INTEGER(DCO_A1S_TAPE_IKND)                :: FRST, SZE
          INTEGER(DCO_A1S_TAPE_IKND)                :: RES
          CALL ARG_CHECK( TIM, TSTP, 'GET_SIZE' )
          CALL DCO_A1S_TI_MARKER_LIST_GET( DCO_A1S_TI_MARKER(TIM),
     &                                     TSTP, FRST, SZE )
          RES = SZE
          !!PRINT *,'Called : DCO_A1S_TI_MARKER_GET_FIRST(',TIM,',',TSTP,') ->',RES
        END FUNCTION DCO_A1S_TI_MARKER_GET_SIZE
        SUBROUTINE DCO_A1S_TI_MARKER_PRINT( TIM )
          INTEGER, INTENT(IN)                       :: TIM
          IF ( TIM < 1 .OR. TIM > SIZE(DCO_A1S_TI_MARKER,1) ) THEN
            PRINT *,
     &          'ERROR: DCO_A1S_TI_MARKER_PRINT :: INVALID TIM NUMBER ',
     &            TIM, ' !!'
            STOP
          END IF
          CALL DCO_A1S_TI_MARKER_LIST_PRINT( DCO_A1S_TI_MARKER(TIM) )
        END  SUBROUTINE DCO_A1S_TI_MARKER_PRINT
      END MODULE DCO_A1S_TI_MARKER_MODULE

#endif /* COMPAD    ! do that only for active compilation */
