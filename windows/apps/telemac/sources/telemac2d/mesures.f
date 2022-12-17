!                   ******************
                    SUBROUTINE MESURES
!                   ******************
!
     &(ITER,TT)
!
!***********************************************************************
! TELEMAC2D   V6P3                                  21/08/2010
!***********************************************************************
!
!brief    READS MEASURED H, U AND V AT TIME AT.
!+                GIVES THE CORRESPONDING WEIGHTS ALPHA1, ALPHA2 AND ALPHA3.
!
!history  J-M HERVOUET (LNHE)
!+        17/08/2001
!+        V5P2
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
!history  R. KOPMANN (EDF R&D, LNHE)
!+        16/04/2013
!+        V6P3
!+   Adding the file format in the call to FIND_IN_SEL.
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ITER           |-->| ITERATION WHERE TO LOOK FOR THE MEASUREMENTS
!| TT             |-->| CORRESPONDING TIME (TO CHECK)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: ITER
      DOUBLE PRECISION, INTENT(IN) :: TT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8) :: FFORMAT
      INTEGER :: FILE_ID
      DOUBLE PRECISION TPS
      LOGICAL OKH,OKU,OKV
      INTEGER I,DISCLIN
      INTEGER IERR
!
!-----------------------------------------------------------------------
!
      IF(T2D_FILES(T2DREF)%NAME(1:1).NE.' ') THEN
!
!-----------------------------------------------------------------------
!
!       WHEN MEASUREMENTS ARE IN A SELAFIN FILE
!
        FFORMAT = T2D_FILES(T2DREF)%FMT
        FILE_ID = T2D_FILES(T2DREF)%LU
        CALL FIND_VARIABLE(FFORMAT, FILE_ID, TEXTE(4), HD%R, NPOIN,
     &                 IERR,RECORD=ITER,TIME_RECORD=TPS)
        OKH = IERR.EQ.0
        CALL FIND_VARIABLE(FFORMAT, FILE_ID, TEXTE(1), UD%R, NPOIN,
     &                 IERR,RECORD=ITER,TIME_RECORD=TPS)
        OKU = IERR.EQ.0
        CALL FIND_VARIABLE(FFORMAT, FILE_ID, TEXTE(2), VD%R, NPOIN,
     &                 IERR,RECORD=ITER,TIME_RECORD=TPS)
        OKV = IERR.EQ.0
!
        IF(.NOT.OKH.OR..NOT.OKU.OR..NOT.OKV) THEN
          WRITE(LU,*) 'MESURES : PROBLEM WHEN READIND HD, UD, OR VD'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(ABS(TT-TPS).GT.1.D-3) THEN
          WRITE(LU,*) 'MESURES : PROBLEM WHEN READIND TIME'
          CALL PLANTE(1)
          STOP
        ENDIF
!       UD AND VD MAY BE QUASI-BUBBLE
!       (BUT ALPHA2 AND ALPHA3 WILL BE SET TO 0)
        IF(UD%ELM.EQ.12) THEN
          DISCLIN=11
          CALL CHGDIS(UD,DISCLIN,12,MESH)
          CALL CHGDIS(VD,DISCLIN,12,MESH)
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
!      CASE TO BE IMPLEMENTED BY USER HERE (OTHER FILE FORMAT, ETC.)
!
        CALL USER_MESURES(ITER,TT)
!
!-----------------------------------------------------------------------
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     WEIGHT FUNCTIONS FOR ALL THE TIMESTEPS
!
      CALL VECTOR(T1,'=','MASBAS          ',
     &            HD%ELM,1.D0,T3,T3,T3,T3,T3,
     &            T3,MESH,MSK,MASKEL)
      CALL OS( 'X=Y     ' , X=ALPHA1 , Y=T1)
!
!     CASE OF QUASI-BUBBLE ELEMENT FOR UD
      IF(HD%ELM.NE.UD%ELM) THEN
        CALL VECTOR(T1,'=','MASBAS          ',
     &              UD%ELM,1.D0,T3,T3,T3,T3,T3,
     &              T3,MESH,MSK,MASKEL)
      ENDIF
!
      CALL OS( 'X=Y     ' , X=ALPHA2 , Y=T1)
      CALL OS( 'X=Y     ' , X=ALPHA3 , Y=T1)
!
!     CANCELS WEIGHTS FOR QUASI-BUBBLE POINTS
!
      IF(UD%ELM.EQ.12) THEN
        DO I=NPOIN+1,NPOIN+NELEM
          ALPHA2%R(I)=0.D0
          ALPHA3%R(I)=0.D0
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
