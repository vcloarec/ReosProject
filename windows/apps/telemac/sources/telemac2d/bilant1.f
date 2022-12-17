!                   ******************
                    SUBROUTINE BILANT1
!                   ******************
!
     &(H,UCONV,VCONV,HPROP,WORK2,WORK3,WORK4,WORK5,LT,NIT,INFO,
     & MASKTR,T,TN,TETAT,MASSOU,MSK,MASKEL,MESH,FLUSOR,FLUENT,EQUA,
     & ITRAC)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CALCULATES THE BALANCE OF THE TRACER MASS.
!
!history  J-M HERVOUET (LNHE)
!+        05/11/2007
!+        V5P8
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EQUA           |-->| STRING DESCRIBING THE EQUATIONS SOLVED
!| FLUENT         |-->| ENTERING FLUX
!| FLUSOR         |-->| EXITING FLUX
!| H              |-->| DEPTH AT TIME N+1.
!| HPROP          |-->| PROPAGATION DEPTH.
!| INFO           |-->| IF YES, PRINTING INFORMATIONS
!| ITRAC          |-->| TRACER INDEX
!| LT             |-->| TIME STEP NUMBER
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKTR         |-->| MASKING OF TRACERS, PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MASSOU         |-->| MASS OF TRACER ADDED BY SOURCE TERM
!|                |   | SEE DIFSOU
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NIT            |-->| TOTAL NUMBER OF TIME STEPS
!| T              |-->| TRACER AT TIME T(N+1)
!| TETAT          |-->| SEMI-IMPLICITATION DU TRACEUR.
!| TN             |-->| TRACER AT TIME T(N)
!| UCONV          |-->| X-COMPONENT OF ADVECTION FIELD
!| VCONV          |-->| Y-COMPONENT OF ADVECTION FIELD
!| WORK2          |<->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| WORK3          |<->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| WORK4          |<->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| WORK5          |<->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC2D, ONLY : MASTR0_BT1,MASTR1_BT1,
     &                                   MASTR2_BT1,MASTEN_BT1,
     &                                   MASTOU_BT1,DIRTOT_BT1,
     &                                   NAMETRAC,DT
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: LT,NIT,ITRAC
      DOUBLE PRECISION, INTENT(IN)   :: TETAT,MASSOU,FLUSOR,FLUENT
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: WORK2,WORK3,WORK4,WORK5
      TYPE(BIEF_OBJ), INTENT(IN)     :: HPROP,UCONV,VCONV,H,T,TN,MASKEL
      TYPE(BIEF_OBJ), INTENT(IN)     :: MASKTR
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      LOGICAL, INTENT(IN)            :: MSK,INFO
      CHARACTER(LEN=20), INTENT(IN)  :: EQUA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER DIR,DDL,OND,IELMT,IELMH
!
      DOUBLE PRECISION PERDUE
      DOUBLE PRECISION FLUXT,MASBOR
      DOUBLE PRECISION FLTDIR,FLTDDL,FLTOND
      DOUBLE PRECISION RELATI,DENOM
!
!
      INTRINSIC ABS,MAX
!
!-----------------------------------------------------------------------
!
      IELMT = T%ELM
      IELMH = H%ELM
!
!-----------------------------------------------------------------------
!
! PROVISIONAL: H AND HPROP ARE REPLACED BY WORK4 AND WORK5 EVERYWHERE
!
      CALL OS ('X=Y     ' , X=WORK4 , Y=H    )
      CALL OS ('X=Y     ' , X=WORK5 , Y=HPROP)
!
      IF(IELMT.NE.IELMH) THEN
        CALL CHGDIS(WORK4,IELMH,IELMT,MESH)
        CALL CHGDIS(WORK5,IELMH,IELMT,MESH)
      ENDIF
!
! END OF PROVISIONAL, EXCEPT FOR REPLACEMENT OF H AND HPROP BELOW
!
!-----------------------------------------------------------------------
!
!  COMPATIBLE CALCULATION OF THE QUANTITY OF TRACER:
!
      IF(LT.NE.0) MASTR1_BT1(ITRAC) = MASTR2_BT1(ITRAC)
!     H IS PUT HERE AS A DUMMY STRUCTURE
!
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        CALL VECTOR(WORK2,'=','MASBAS          ',IELMT,
     &              1.D0,H,H,H,H,H,H,MESH,MSK,MASKEL)
        CALL OS( 'X=XY    ' , X=WORK2 , Y=T)
      ELSE
        CALL VECTOR(WORK2,'=','MASVEC          ',IELMT,
     &              1.D0,T,H,H,H,H,H,MESH,MSK,MASKEL)
      ENDIF
      MASTR2_BT1(ITRAC) = DOTS(WORK2,WORK4)
      IF(NCSIZE.GT.1) MASTR2_BT1(ITRAC)=P_SUM(MASTR2_BT1(ITRAC))
!
      IF(LT.EQ.0) THEN
        MASTR0_BT1(ITRAC) = MASTR2_BT1(ITRAC)
        MASTR1_BT1(ITRAC) = MASTR2_BT1(ITRAC)
        MASTEN_BT1(ITRAC) = 0.D0
        MASTOU_BT1(ITRAC) = 0.D0
        DIRTOT_BT1(ITRAC) = 0.D0
      ENDIF
!
!=======================================================================
!
!   CALCULATES FLUXES (IT MISSES DIFFUSIVE FLUX,...TO BE LOOKED AT)
!
!=======================================================================
!
      DIR=1
      DDL=2
      OND=4
!
!=======================================================================
!   CALCULATES IMPOSED FLUXES (DISCHARGE IMPOSED OR VELOCITY IMPOSED)
!
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        FLTDIR = DT * FLUENT
      ELSE
        CALL VECTOR(WORK2,'=','FLUBDF          ',IELBOR(IELMT,1),
     &              1.D0,WORK5,H,H,UCONV,VCONV,VCONV,
     &              MESH,.TRUE.,MASKTR%ADR(DIR)%P)
!
        CALL CPSTVC(WORK2,WORK3)
        CALL OSBD( 'X=CY    ' , WORK3 , T  ,  T , TETAT      , MESH )
        CALL OSBD( 'X=X+CY  ' , WORK3 , TN ,  T , 1.D0-TETAT , MESH )
        FLTDIR=DOTS(WORK2,WORK3)
        IF(NCSIZE.GT.1) FLTDIR=P_SUM(FLTDIR)
      ENDIF
!
!=======================================================================
!
!   CALCULATES FREE FLUXES
!
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        FLTDDL = DT * FLUSOR
      ELSE
        CALL VECTOR(WORK2,'=','FLUBDF          ',IELBOR(IELMT,1),
     &              1.D0,WORK5,H,H,UCONV,VCONV,VCONV,
     &              MESH,.TRUE.,MASKTR%ADR(DDL)%P)
        CALL CPSTVC(WORK2,WORK3)
        CALL OSBD( 'X=CY    ' , WORK3 , T  ,  T , TETAT      , MESH )
        CALL OSBD( 'X=X+CY  ' , WORK3 , TN ,  T , 1.D0-TETAT , MESH )
        FLTDDL=DOTS(WORK2,WORK3)
        IF(NCSIZE.GT.1) FLTDDL=P_SUM(FLTDDL)
      ENDIF
!
!=======================================================================
!
!   CALCULATES FLUXES BY INCIDENTAL WAVE
!
      CALL VECTOR(WORK2,'=','FLUBDF          ',IELBOR(IELMT,1),
     &            1.D0,WORK5,H,H,UCONV,VCONV,VCONV,
     &            MESH,.TRUE.,MASKTR%ADR(OND)%P)
      CALL CPSTVC(WORK2,WORK3)
      CALL OSBD( 'X=CY    ' , WORK3 , T  ,  T , TETAT      , MESH )
      CALL OSBD( 'X=X+CY  ' , WORK3 , TN ,  T , 1.D0-TETAT , MESH )
      FLTOND=DOTS(WORK2,WORK3)
      IF(NCSIZE.GT.1) FLTOND=P_SUM(FLTOND)
!
!=======================================================================
!
!   CALCULATES FLUXES AT THE LIQUID BOUNDARIES
!
      FLUXT = FLTDIR + FLTDDL + FLTOND
      MASTEN_BT1(ITRAC) = MASTEN_BT1(ITRAC) - FLUXT
      MASTOU_BT1(ITRAC) = MASTOU_BT1(ITRAC) + MASSOU
      DIRTOT_BT1(ITRAC) = DIRTOT_BT1(ITRAC) - FLTDIR
!
!=======================================================================
!
!   CALCULATES THE FLUX OF TRACER THROUGH THE WALLS, BY LAW OF FLUX
!
!     PROVISIONAL, TO BE PROGRAMMED
      MASBOR = 0.D0
!
!=======================================================================
!
!  PRINTS:
!
      IF(INFO) THEN
!
!-----------------------------------------------------------------------
!
!     PRINTS FOR THE TRACER
!
        WRITE(LU,501) NAMETRAC(ITRAC)
!
        IF(LT.EQ.0) THEN
!
          WRITE(LU,2090) MASTR0_BT1(ITRAC)
!
        ELSE
!
          WRITE(LU,2100) MASTR2_BT1(ITRAC)
          IF(ABS(FLTDIR).GT.1.D-8) WRITE(LU,2110) -FLTDIR
          IF(ABS(FLTDDL).GT.1.D-8) WRITE(LU,2111) -FLTDDL
          IF(ABS(FLTOND).GT.1.D-8) WRITE(LU,2112) -FLTOND
          IF(ABS(MASSOU).GT.1.D-8) WRITE(LU,2113) MASSOU
!
          PERDUE = MASTR0_BT1(ITRAC)+MASTEN_BT1(ITRAC)+
     &             MASBOR+MASTOU_BT1(ITRAC)-MASTR2_BT1(ITRAC)
          DENOM = MAX(ABS(MASTR0_BT1(ITRAC)),ABS(MASTR2_BT1(ITRAC)),
     &                ABS(DIRTOT_BT1(ITRAC)))
          IF(DENOM.GT.1.D-8) THEN
            RELATI = ABS(PERDUE) / DENOM
            WRITE(LU,2140) RELATI
          ELSE
            RELATI = PERDUE
            WRITE(LU,2150) RELATI
          ENDIF
!
          IF(ABS(MASTEN_BT1(ITRAC)).GT.1.D-8)
     &              WRITE(LU,2161) MASTEN_BT1(ITRAC)
          IF(ABS(MASTOU_BT1(ITRAC)).GT.1.D-8)
     &              WRITE(LU,2164) MASTOU_BT1(ITRAC)
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!  FINAL MASS BALANCE
!
      IF(LT.EQ.NIT) THEN
!
        WRITE(LU,601) ITRAC
!
        PERDUE = MASTR0_BT1(ITRAC)+MASTEN_BT1(ITRAC)+
     &           MASBOR+MASTOU_BT1(ITRAC)-MASTR2_BT1(ITRAC)
!
        WRITE(LU,2160) MASTR0_BT1(ITRAC),MASTR2_BT1(ITRAC)
        WRITE(LU,2165) PERDUE
!
      ENDIF
!
!  END OF PRINTS
!
!=======================================================================
!
!  PRINT FORMATS:
!
501   FORMAT(80(' '),/,21X,'BALANCE OF TRACER ',1A32)
601   FORMAT(80('-'),/,19X,'FINAL BALANCE OF TRACER ',1I2)
2090  FORMAT(5X,'INITIAL QUANTITY OF TRACER:',G16.7,' TRACER UNIT M3')
2100  FORMAT(/,5X,'QUANTITY OF TRACER:',G16.7,' TRACER UNIT M3')
2110  FORMAT(5X,'PRESCRIBED FLUX OF TRACER:         ' , G16.7 ,
     &          '  ( >0 : ENTERING  <0 : EXITING )')
2111  FORMAT(5X,'FREE FLUX OF TRACER:               ' , G16.7 ,
     &          '  ( >0 : ENTERING  <0 : EXITING )')
2112  FORMAT(5X,'INCIDENT FLUX OF TRACER:           ' , G16.7 ,
     &          '  ( >0 : ENTERING  <0 : EXITING )')
2113  FORMAT(5X,'QUANTITY CREATED BY SOURCE TERM:   ' , G16.7 )
2140  FORMAT(/,5X,'RELATIVE ERROR CUMULATED ON TRACER: ',G16.7)
2150  FORMAT(/,5X,'ABSOLUTE ERROR CUMULATED ON TRACER: ',G16.7)
2160  FORMAT(/,5X,'INITIAL QUANTITY OF TRACER        : ',G16.7,
     &       /,5X,'FINAL QUANTITY                    : ',G16.7)
2161  FORMAT(  5X,'QUANTITY ENTERED THROUGH LIQ. BND.: ',G16.7,
     &            '  ( IF <0 EXIT )')
2164  FORMAT(  5X,'QUANTITY CREATED BY SOURCE TERM   : ',G16.7)
2165  FORMAT(  5X,'TOTAL QUANTITY LOST               : ',G16.7)
!
!=======================================================================
!
      RETURN
      END
