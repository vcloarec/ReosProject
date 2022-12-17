!                   ************************
                    SUBROUTINE COST_FUNCTION
!                   ************************
!
     &(JCOUT,OPTION,MODE)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PARTIALLY COMPUTES (ONE TIMESTEP) THE COST FUNCTION.
!
!history  E. BARROS
!+        25/06/1993
!+
!+
!
!history  A. LEOPARDI (UNINA)
!+        02/10/2000
!+        V5P2
!+   UPGRADE
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| JCOUT          |<->| COST FUNCTION
!| MODE           |-->| FCT: COST FUNCTION
!|                |   | GRD: GRADIENT COST FUNCTION
!|                |   | RHS: RIGHT HAND SIDE
!| OPTION         |-->| 1: COST FUNCTION COMPUTED WITH DEPTH
!|                |   | 2: COST FUNCTION COMPUTED WITH CELERITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_COST_FUNCTION => COST_FUNCTION
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION , INTENT(INOUT) :: JCOUT
      INTEGER , INTENT(IN)             :: OPTION
      CHARACTER(LEN=3) , INTENT(IN)    :: MODE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER I,J
!
!=======================================================================
!
      IF(MODE.EQ.'FCT') THEN
!
!       HERE U,V AND H ARE GIVEN BY A CALL TO LITENR
!
        IF(OPTION.EQ.1) THEN
!
          DO I=1,NPOIN
!
            JCOUT = JCOUT + ALPHA1%R(I) * (H%R(I)-HD%R(I))**2
     &                    + ALPHA2%R(I) * (U%R(I)-UD%R(I))**2
     &                    + ALPHA3%R(I) * (V%R(I)-VD%R(I))**2
!
          ENDDO
!
        ELSEIF(OPTION.EQ.2) THEN
!
          DO I=1,NPOIN
!
            JCOUT = JCOUT + ALPHA1%R(I) * GRAV *
     &              (SQRT(MAX(H%R(I),0.D0))-SQRT(MAX(HD%R(I),0.D0)))**2
     &                    + ALPHA2%R(I) * (U%R(I)-UD%R(I))**2
     &                    + ALPHA3%R(I) * (V%R(I)-VD%R(I))**2
!
          ENDDO
!
        ELSE
!
          WRITE(LU,*) 'COST_FUNCTION: UNEXPECTED OPTION : ',MODE
          CALL PLANTE(1)
          STOP
!
!       TEST ON OPTION
        ENDIF
!
!=======================================================================
!
      ELSEIF(MODE.EQ.'GRD') THEN
!
      IF(    INCLU2(ESTIME,'FROTTEMENT')
     &   .OR.INCLU2(ESTIME,'FRICTION'  )  ) THEN
!
!     IDENTIFICATION OF FRICTION
!
      IF (KFROT.EQ.3.OR.KFROT.EQ.2) THEN
!
      CALL SLOPES(TE3,ZF,MESH)
      CALL VECTOR(T1,'=','MASBAS          ',U%ELM,1.D0,
     &            T3,T3,T3,T3,T3,T3,MESH,.TRUE.,TE3)
!
      CALL FRICTI(T3,T4,T2,T2,UN,VN,HN,CF,MESH,T2,T5,VERTIC,UNSV2D,MSK,
     &            MASKEL,HFROT)
!
      CALL OS('X=XY    ', X=T3, Y=T1)
      CALL OS('X=XY    ', X=T4, Y=T1)
!
      CALL OS('X=CXYZ  ', X=T3, Y=QQ, Z=UU, C=-2.D0)
      CALL OS('X=CXYZ  ', X=T4, Y=RR, Z=VV, C=-2.D0)
!
      CALL OS('X=X+Y   ', X=T3, Y=T4)
      CALL OS('X=Y/Z   ', X=T3, Y=T3, Z=CHESTR)
!
      ELSE
!
        WRITE(LU,*) 'COST_FUNCTION: UNEXPECTED FRICTION LAW: ',KFROT
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
      ELSE
!
        WRITE(LU,*) 'COST_FUNCTION: UNEXPECTED PARAMETER :'
        WRITE(LU,*) ESTIME
        WRITE(LU,*) 'CHECK THE KEY-WORD: PARAMETER ESTIMATION'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
! COMPUTATION OF A GRADIENT FOR EVERY ZONE, AFTER BUILDING
! A GRADIENT VECTOR FOR EVERY POINT (IN T3)
!
      IF(NZONE.GT.0) THEN
!       IF ONE IDENTIFIES ONLY ONE PARAMETER S FOR A SET OF NODES
!       GRADIENT DJ/DS IS THE SUM OF THE GRADIENTS OF EACH
!       NODE OF THE SET
        DO J=1,NZONE
          DO I=1,NPOIN
            IF(ZONE%I(I).EQ.J) GRADJ%R(J)=GRADJ%R(J)+T3%R(I)
          ENDDO
        ENDDO
      ELSE
!       HERE IT IS SUPPOSED THAT NPARAM = NPOIN
        CALL OS('X=X+Y   ',X=GRADJ,Y=T3)
      ENDIF
!
!=======================================================================
!
      ELSEIF(MODE.EQ.'RHS') THEN
!
!           IT    IT    IT
!  TERMS 2 W   ( X   - M   ) OR EQUIVALENT DEPENDING ON THE OPTION
!           IP    IP    IP
!
      IF(OPTION.EQ.1) THEN
!
        CALL OS('X=Y-Z   ', X=CV1, Y=HH , Z=HD)
        CALL OS('X=Y-Z   ', X=CV2, Y=UU , Z=UD)
        CALL OS('X=Y-Z   ', X=CV3, Y=VV , Z=VD)
!
        CALL OS('X=CXY   ', X=CV1, Y=ALPHA1 , Z=ALPHA1 , C=2.D0 )
        CALL OS('X=CXY   ', X=CV2, Y=ALPHA2 , Z=ALPHA2 , C=2.D0 )
        CALL OS('X=CXY   ', X=CV3, Y=ALPHA3 , Z=ALPHA3 , C=2.D0 )
!
      ELSEIF(OPTION.EQ.2) THEN
!
!       HERE COST FUNCTION COMPUTED WITH CELERITY INSTEAD OF DEPTH
        CALL OS( 'X=SQR(Y)', X=T1  , Y=HH)
        CALL OS( 'X=SQR(Y)', X=T2  , Y=HD)
        CALL OS( 'X=Y-Z   ', X=T3  , Y=T1 , Z=T2)
        CALL OS( 'X=Y/Z   ', X=CV1 , Y=T3 , Z=T1)
        CALL OS( 'X=Y-Z   ', X=CV2 , Y=UU , Z=UD)
        CALL OS( 'X=Y-Z   ', X=CV3 , Y=VV , Z=VD)
!
        CALL OS( 'X=CXY   ', X=CV1 , Y=ALPHA1 , C=GRAV )
        CALL OS( 'X=CXY   ', X=CV2 , Y=ALPHA2 , C=2.D0 )
        CALL OS( 'X=CXY   ', X=CV3 , Y=ALPHA3 , C=2.D0 )
!
      ENDIF
!
!=======================================================================
!
      ELSE
!
!=======================================================================
!
        WRITE(LU,*) 'COST_FUNCTION: UNEXPECTED MODE : ', MODE
        CALL PLANTE(1)
        STOP
!
!=======================================================================
!
!     TEST ON MODE
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
