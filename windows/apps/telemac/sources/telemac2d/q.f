!                   ***************************
                    DOUBLE PRECISION FUNCTION Q
!                   ***************************
!
     &(I)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   08/11/2011
!***********************************************************************
!
!brief    PRESCRIBES THE DISCHARGE FOR FLOW IMPOSED
!+                LIQUID BOUNDARIES.
!
!history  J-M HERVOUET (LNHE)
!+        09/01/2004
!+        V5P6
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
!history  C. COULET (ARTELIA GROUP)
!+        08/11/2011
!+        V6P2
!+   Modification of FCT size due to modification of TRACER numbering
!
!history  J-M HERVOUET (LNHE)
!+        21/05/2012
!+        V6P2
!+   Discharge taken at mid distance between AT-DT AND AT, except
!+   for finite volumes (DT unknown, and AT time of beginning of time
!+   step in this case)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| NUMBER OF THE LIQUID BOUNDARY.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_Q => Q
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN) :: I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=9) FCT
      DOUBLE PRECISION Q1,Q2
!
!-----------------------------------------------------------------------
!
!     IF LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OKQ REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OKQ IS SET  TO .FALSE.
!
      IF(OKQ(I).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE Q(1), Q(2), ETC, Q(99), DEPENDING ON I
        FCT='Q(       '
        IF(I.LT.10) THEN
          WRITE(FCT(3:3),FMT='(I1)') I
          FCT(4:4)=')'
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(3:4),FMT='(I2)') I
          FCT(5:5)=')'
        ELSE
          WRITE(LU,*) 'Q NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
          CALL READ_FIC_FRLIQ(Q1,FCT,AT-DT,
     &                        T2D_FILES(T2DIMP)%LU,ENTET,OKQ(I))
          CALL READ_FIC_FRLIQ(Q2,FCT,AT   ,
     &                        T2D_FILES(T2DIMP)%LU,ENTET,OKQ(I))
          Q=(Q1+Q2)*0.5D0
        ELSE
          CALL READ_FIC_FRLIQ(Q,FCT,AT   ,
     &                        T2D_FILES(T2DIMP)%LU,ENTET,OKQ(I))
        ENDIF
!
      ENDIF
!
      IF(.NOT.OKQ(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
!
!     Q IS TAKEN FROM THE STEERING FILE, BUT MAY BE CHANGED
!
        IF(NDEBIT.GE.I) THEN
          Q = DEBIT(I)
!
          ! USER UPDATE OF VALUE
          CALL USER_Q(I, Q)
        ELSE
          WRITE(LU,401) I
401       FORMAT(1X,/,1X,'Q : MORE PRESCRIBED FLOWRATES',/,
     &                1X,'    ARE REQUIRED IN THE PARAMETER FILE',/,
     &                1X,'    AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
