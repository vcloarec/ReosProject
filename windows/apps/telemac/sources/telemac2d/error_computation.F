!                   ****************************
                    SUBROUTINE ERROR_COMPUTATION
!                   ****************************
!
     &(F,MESH,FINEMESH,NELMAX,NPOIN,CORRESP,RLEVELS,NLEVEL,
     & IKLE,FINEIKLE)
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!Brief    Error computation
!+        Computes the error between the numerical solution for a field
!+        F and an analytical solution, which is defined here by the user
!+        The error is computed on the finest mesh FINEMESH
!
!history  A. LEROY (LNHE) & J-M HERVOUET (jubilado)
!+        26/09/2017
!+        V7P3
!+        The errors are now based on an integral on the domain.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |<--| FIELD FOR ERROR COMPUTATION
!| FINEMESH       |<--| FINE MESH ON WHICH ERRORS ARE COMPUTED
!| MESH           |<--| COARSER MESH CONTAINING THE SIMULATION RESULTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)       :: NPOIN
      DOUBLE PRECISION, INTENT(IN)       :: F(NPOIN)
      TYPE(BIEF_MESH) , INTENT(IN)       :: MESH
      TYPE(BIEF_MESH) , INTENT(INOUT)    :: FINEMESH
      INTEGER         , INTENT(IN)       :: NELMAX
      INTEGER         , INTENT(IN)       :: RLEVELS
      INTEGER         , INTENT(IN)       :: NLEVEL
      INTEGER         , INTENT(IN)       :: CORRESP(NELMAX,RLEVELS)
!     WARNING, THERE SHOULD BE NDP INSTEAD OF 3
      INTEGER         , INTENT(IN)       :: IKLE(NELMAX*3)
      INTEGER         , INTENT(IN)       :: FINEIKLE(NELMAX*3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,IERR,JPOIN1,JPOIN2,JPOIN3,IELEM,JELEM,I1,I2,I3
!
      DOUBLE PRECISION DETM,ERRL1,ERRL2,ERRLINF,SURDETM
      DOUBLE PRECISION X1,Y1,F1,X2,Y2,F2,X3,Y3,F3,A,B,C
      DOUBLE PRECISION XM1,XM2,XM3,YM1,YM2,YM3
!
      DOUBLE PRECISION, ALLOCATABLE :: ANALYTICAL(:)
      DOUBLE PRECISION, ALLOCATABLE :: NUMERICAL(:)
      INTEGER FID
!
      INTRINSIC MAX,SQRT
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
!     ALLOCATE ARRAYS TO STORE THE ANALYTICAL SOLUTION VALUES
!     AND THE NUMERICAL SOLUTION VALUES
!
      ALLOCATE(ANALYTICAL(FINEMESH%NPOIN),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,"ERROR_COMPUTATION: ANALYTICAL")
      ALLOCATE(NUMERICAL(FINEMESH%NPOIN),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,"ERROR_COMPUTATION: NUMERICAL")
!
!=======================================================================
! INTERPOLATE THE NUMERICAL SOLUTION ON THE FINE MESH
!=======================================================================
!
!     INITIALISING THE INTEGRAL OF BASES OF FINE MESH
!     IT WILL BE COMPUTED IN FINEMESH%M%D%R
!
      DO I=1,FINEMESH%NPOIN
        FINEMESH%M%D%R(I)=0.D0
      ENDDO
!
!     LOOP ON ALL THE FINE ELEMENTS
!
      DO IELEM=1,FINEMESH%NELEM
!       FIND THE ELEMENT OF THE COARSE MESH TO WHICH THE FINE
!       ELEMENT BELONGS
        JELEM = IELEM
        DO K=RLEVELS,NLEVEL,-1
          JELEM = CORRESP(JELEM,K)
        ENDDO
!       GET THE INDEX OF EACH POINT OF THE COARSE ELEMENT
        JPOIN1 = IKLE(              JELEM)
        JPOIN2 = IKLE(  MESH%NELMAX+JELEM)
        JPOIN3 = IKLE(2*MESH%NELMAX+JELEM)
!       GET THE COORDINATES OF EACH POINT OF THE COARSE ELEMENT
        X1 = MESH%X%R(JPOIN1)
        X2 = MESH%X%R(JPOIN2)
        X3 = MESH%X%R(JPOIN3)
        Y1 = MESH%Y%R(JPOIN1)
        Y2 = MESH%Y%R(JPOIN2)
        Y3 = MESH%Y%R(JPOIN3)
!       GET THE VALUE OF THE FUNCTION AT EACH POINT OF THE COARSE ELEMENT
        F1 = F(JPOIN1)
        F2 = F(JPOIN2)
        F3 = F(JPOIN3)
!
!       POINTS OF THE FINE ELEMENT (I1, I2 AND I3)
!       EACH POINT WILL BE TREATED SEVERAL TIMES
        I1 = FINEIKLE(                    IELEM)
        I2 = FINEIKLE(  FINEMESH%NELMAX + IELEM)
        I3 = FINEIKLE(2*FINEMESH%NELMAX + IELEM)
!       GET THE COORDINATES OF THE POINTS ON THE FINE MESH
        XM1 = FINEMESH%X%R(I1)
        YM1 = FINEMESH%Y%R(I1)
        XM2 = FINEMESH%X%R(I2)
        YM2 = FINEMESH%Y%R(I2)
        XM3 = FINEMESH%X%R(I3)
        YM3 = FINEMESH%Y%R(I3)
        SURDETM  = 1.D0/((X2 -X1)*(Y3-Y1)-(X3-X1)*(Y2 -Y1))
!       SOLVE A LINEAR SYSTEM TO COMPUTE COEFFICIENTS A, B AND C
        A = ((X2 -XM1)*(Y3-YM1)-(X3-XM1)*(Y2 -YM1))*SURDETM
        B = ((XM1 -X1)*(Y3- Y1)-(X3- X1)*(YM1 -Y1))*SURDETM
        C = ((X2  -X1)*(YM1-Y1)-(XM1-X1)*(Y2  -Y1))*SURDETM
        NUMERICAL(I1) = A*F1 + B*F2 + C*F3
!       SOLVE A LINEAR SYSTEM TO COMPUTE COEFFICIENTS A, B AND C
        A = ((X2 -XM2)*(Y3-YM2)-(X3-XM2)*(Y2 -YM2))*SURDETM
        B = ((XM2 -X1)*(Y3- Y1)-(X3- X1)*(YM2 -Y1))*SURDETM
        C = ((X2  -X1)*(YM2-Y1)-(XM2-X1)*(Y2  -Y1))*SURDETM
        NUMERICAL(I2) = A*F1 + B*F2 + C*F3
!       SOLVE A LINEAR SYSTEM TO COMPUTE COEFFICIENTS A, B AND C
        A = ((X2 -XM3)*(Y3-YM3)-(X3-XM3)*(Y2 -YM3))*SURDETM
        B = ((XM3 -X1)*(Y3- Y1)-(X3- X1)*(YM3 -Y1))*SURDETM
        C = ((X2  -X1)*(YM3-Y1)-(XM3-X1)*(Y2  -Y1))*SURDETM
        NUMERICAL(I3) = A*F1 + B*F2 + C*F3
!
        DETM  = ((XM2-XM1)*(YM3-YM1)-(XM3-XM1)*(YM2 -YM1))/3.D0
        FINEMESH%M%D%R(I1)=FINEMESH%M%D%R(I1)+DETM
        FINEMESH%M%D%R(I2)=FINEMESH%M%D%R(I2)+DETM
        FINEMESH%M%D%R(I3)=FINEMESH%M%D%R(I3)+DETM
      ENDDO
!
!=======================================================================
! COMPUTE THE ANALYTICAL SOLUTION ON THE FINE MESH
!=======================================================================
!
!     L IS USER DEFINED FOR THE ERROR COMPUTATION
!
      CALL USER_ANALYTICAL_SOLUTION
     &(F,MESH,FINEMESH,NELMAX,NPOIN,CORRESP,RLEVELS,NLEVEL,
     & IKLE,FINEIKLE,ANALYTICAL)
!
!=======================================================================
! COMPUTES THE ERROR COMPARED TO AN ANALYTICAL SOLUTION
!=======================================================================
!
      ERRL1=0.D0
      ERRL2=0.D0
      ERRLINF=0.D0
!     WILL BE THE AREA
      A=0.D0
!
!     DIFFERENT KINDS OF ERRORS
!
      DO I=1,FINEMESH%NPOIN
        ERRL1=ERRL1+ABS(ANALYTICAL(I)-NUMERICAL(I))*FINEMESH%M%D%R(I)
        ERRL2=ERRL2+(ANALYTICAL(I)-NUMERICAL(I))**2*FINEMESH%M%D%R(I)
        ERRLINF=MAX(ERRLINF,ABS(ANALYTICAL(I)-NUMERICAL(I)))
        A=A+FINEMESH%M%D%R(I)
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        ERRL1  =P_SUM(ERRL1)
        ERRL2  =P_SUM(ERRL2)
        A      =P_SUM(A)
        ERRLINF=P_MAX(ERRLINF)
      ENDIF
!
      WRITE(LU,*)'---------------------------------------------------'
      WRITE(LU,*)'ERROR AT REFINEMENT ',NLEVEL-1,' (0=ORIGINAL MESH)'
      WRITE(LU,*)'---------------------------------------------------'
      ERRL1=ERRL1/A
      ERRL2=SQRT(ERRL2/A)
      WRITE(LU,*)'ERROR NORM L1 IS',ERRL1
      WRITE(LU,*)'ERROR NORM L2 IS',ERRL2
      WRITE(LU,*)'ERROR NORM LINF IS',ERRLINF
      WRITE(LU,*)'NPOIN FOR NUMERICAL RESOLUTION IS',NPOIN
      WRITE(LU,*)'NPOIN FOR ERROR COMPUTATION IS',FINEMESH%NPOIN
!     WRITE THE ERRORS IN A FILE
      CALL GET_FREE_ID(FID)
#if defined NAGFOR
!     Manually going to the end of the file
      OPEN(UNIT=FID,FILE='ERRORS.DAT',FORM='FORMATTED',
     &     STATUS='UNKNOWN')
      REWIND(FID)
      IF(NLEVEL.GT.1) THEN
        READ(FID,*)
        DO I=1,NLEVEL-1
          READ(FID,*)
        ENDDO
      ENDIF
#else
      OPEN(UNIT=FID,FILE='ERRORS.DAT',FORM='FORMATTED',
     &     STATUS='UNKNOWN',POSITION='APPEND')
#endif
      IF(NLEVEL.EQ.1) THEN
        WRITE(FID, *) "#NPOIN ", "ERRL2 ", "ERRL1 ", "ERRLINF "
      ENDIF
      WRITE(FID, *) NPOIN, ERRL2, ERRL1, ERRLINF
      CLOSE(FID)
!
      IF(ERRL1.NE.0.D0) THEN
        ERRL1=LOG10(ERRL1)
        WRITE(LU,*) 'LOG_10(ERRL1) =',ERRL1
      ENDIF
      IF(ERRL2.NE.0.D0) THEN
        ERRL2=LOG10(ERRL2)
        WRITE(LU,*) 'LOG_10(ERRL2) =',ERRL2
      ENDIF
      IF(ERRLINF.NE.0.D0) THEN
        ERRLINF=LOG10(ERRLINF)
        WRITE(LU,*) 'LOG_10(ERRLINF) =',ERRLINF
      ENDIF
      WRITE(LU,*) '---------------------------------------------------'
!
      DEALLOCATE(ANALYTICAL)
      DEALLOCATE(NUMERICAL)
!
!=======================================================================
!
      RETURN
      END
