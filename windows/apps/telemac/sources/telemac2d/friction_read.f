!                   ************************
                    SUBROUTINE FRICTION_READ
!                   ************************
!
     &(NCOF,NZONMX,ITURB,LISRUG,VEGETATION,NOMCOF,NZONES,FRTAB,KFROTL,
     & SB) 
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    FRICTION FILE READ.
!
!history  F. HUVELIN
!+        20/04/2004
!+        V5P4
!+   First version
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
!history  J-M HERVOUET (LNHE)
!+        11/05/2015
!+        V7P1
!+   Data on lateral friction are now asked as soon as LISRUG=2
!+   and not only in case of k-epsilon model.
!
!history  J-M HERVOUET (LNHE)
!+        29/09/2015
!+        V7P1
!+   Bug corrected when the number of zones is the maximum.
!
!history R.KOPMANN (BAW)
!+        31/10/2019
!+        V8P2
!+   Lateral boundary roughness coefficient is not read from table
!+   but will set in the steering file or from the boundary file
!+   15 possible coefficients, read of vegetation law
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FRTAB          |-->| FRICTION_OBJ STRUCTURE WITH DATA ON FRICTION
!| ITURB          |-->| TURBULENCE MODEL
!| KFROTL         |-->| LAW OF FRICTION ON LATERAL BOUNDARIES
!| LISRUG         |-->| TURBULENCE REGIME (1: SMOOTH 2: ROUGH)
!| NCOF           |-->| LOGICAL UNIT OF FRICTION FILE
!| NOMCOF         |-->| NAME OF FRICTION FILE
!| NZONES         |<--| NUMBER OF FRICTION ZONES
!| NZONMX         |-->| MAXIMUM NUMBER OF FRICTION ZONES
!| SB             |-->| ROUGHNESS COEFFICIENT OF BOUNDARIES
!| VEGETATION     |-->| IF YES, THERE IS VEGETATION FRICTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE FRICTION_DEF
      USE INTERFACE_TELEMAC2D, EX_FRICTION_READ => FRICTION_READ
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,            INTENT(IN)    :: NCOF, NZONMX
      INTEGER,            INTENT(IN)    :: ITURB, LISRUG, KFROTL
      LOGICAL,            INTENT(IN)    :: VEGETATION
      DOUBLE PRECISION,   INTENT(IN)    :: SB
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: NOMCOF
      INTEGER,            INTENT(OUT)   :: NZONES
      TYPE(FRICTION_OBJ), INTENT(INOUT) :: FRTAB
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I, J, N, IZ1, IZ2, IZONE, TYP, LINE
      DOUBLE PRECISION  R1, R2, R3(15)
      CHARACTER(LEN=4)  LAW,LAWVEG
      CHARACTER(LEN=20) CHAINE(10)
!
!-----------------------------------------------------------------------
!
      ! CHECK THAT THERE IS A FRICTION FILE
      ! -----------------------------------
      IF(NOMCOF(1:1) .EQ. ' ') THEN
        WRITE(LU,2)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      REWIND NCOF
!
      DO I=1,NZONMX
        FRTAB%ADR(I)%P%GNUMB(1) = 0
        FRTAB%ADR(I)%P%GNUMB(2) = 0
        FRTAB%ADR(I)%P%RTYPE = 0
        FRTAB%ADR(I)%P%RCOEF = 0.D0
        FRTAB%ADR(I)%P%NDEF  = 0.D0
        FRTAB%ADR(I)%P%VTYPE = 0
        DO J = 1, 15
          FRTAB%ADR(I)%P%VCOEF(J) = 0.D0
        ENDDO
      ENDDO
!
!     LISTING
!
      WRITE(LU,3)
      WRITE(LU,4)
!
!     READ DATA FOR EACH ZONE
!
      IZONE = 0
      LINE = 0
1000  CONTINUE
!
!     FIND THE NEXT ZONE INDEX AND LAW
!
      CALL FRICTION_SCAN(NCOF,NOMCOF,TYP,LINE)
!
!     END OF FILE => EXIT
      IF(TYP .EQ. 3) THEN
        NZONES = IZONE
        GO TO 1001
      ELSE
        IZONE = IZONE + 1
        IF(IZONE.GT.NZONMX) THEN
          WRITE(LU,6)
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!     READ AND SAVE PARAMETERS OF THE ZONE
      IF(TYP .EQ. 1) N = 1
      IF(TYP .EQ. 2) N = 4
!
!     READ THE NAME OF THE BED LAW, VEGETATION LAW, AND NUMBERING OF THE ZONE
      CHAINE(1) = ' '
      IF(TYP.EQ.1) THEN
        READ(NCOF,*,END=999,ERR=998) IZ1,(CHAINE(J),J=2,N),LAW,
     &      (CHAINE(J),J=2,2),LAWVEG
        IF(LAW.EQ.'COWH') THEN
          BACKSPACE(NCOF)
          READ(NCOF,*,END=999,ERR=998) IZ1,(CHAINE(J),J=2,N),LAW,
     &        (CHAINE(J),J=2,2),(CHAINE(J),J=3,3),LAWVEG
        ENDIF
        IZ2 = IZ1
      ELSEIF(TYP.EQ.2) THEN
        READ(NCOF,*,END=999,ERR=998) CHAINE(1),IZ1,CHAINE(3),IZ2,
     &      (CHAINE(J),J=5,N),LAW,CHAINE(3),LAWVEG
      ENDIF
!
!     LOCAL-GLOBAL NUMBER OF THE ZONE
!
      FRTAB%ADR(IZONE)%P%GNUMB(1) = IZ1
      FRTAB%ADR(IZONE)%P%GNUMB(2) = IZ2
!
      BACKSPACE(NCOF)
      CALL MAJUS(LAW)
!
!     FIND THE LAW AND THE NUMBER OF PARAMETERS TO READ
!
      SELECT CASE (LAW)
!
      CASE('NOFR')
        READ(NCOF,*,END=999,ERR=900) (CHAINE(J),J=1,N),LAW
        FRTAB%ADR(IZONE)%P%RTYPE = 0
        FRTAB%ADR(IZONE)%P%RCOEF = 0.D0
        FRTAB%ADR(IZONE)%P%NDEF  = 0.D0
        N = N + 1
      CASE('HAAL')
        READ(NCOF,*,END=999,ERR=901) (CHAINE(J),J=1,N),LAW,R1
        FRTAB%ADR(IZONE)%P%RTYPE = 1
        FRTAB%ADR(IZONE)%P%RCOEF = R1
        FRTAB%ADR(IZONE)%P%NDEF  = 0.D0
!       THE BOUNDARY COEFFICIENT COEF MUST BE THE SAME AS THE BOTTOM
!
        IF(ABS(R1-SB).GT.1.D-4 .OR.KFROTL.NE.1) THEN
          WRITE(LU,16) NOMCOF,IZONE,
     &                 FRTAB%ADR(IZONE)%P%RCOEF
          CALL PLANTE(1)
          STOP
        ENDIF
        N = N + 2
      CASE('CHEZ')
        READ(NCOF,*,END=999,ERR=901) (CHAINE(J),J=1,N),LAW,R1
        FRTAB%ADR(IZONE)%P%RTYPE = 2
        FRTAB%ADR(IZONE)%P%RCOEF = R1
        FRTAB%ADR(IZONE)%P%NDEF  = 0.D0
        N = N + 2
      CASE('STRI')
        READ(NCOF,*,END=999,ERR=901) (CHAINE(J),J=1,N),LAW,R1
        FRTAB%ADR(IZONE)%P%RTYPE = 3
        FRTAB%ADR(IZONE)%P%RCOEF = R1
        FRTAB%ADR(IZONE)%P%NDEF  = 0.D0
        N = N + 2
      CASE('MANN')
        READ(NCOF,*,END=999,ERR=901) (CHAINE(J),J=1,N),LAW,R1
        FRTAB%ADR(IZONE)%P%RTYPE = 4
        FRTAB%ADR(IZONE)%P%RCOEF = R1
        FRTAB%ADR(IZONE)%P%NDEF  = 0.D0
        N = N + 2
      CASE('NIKU')
        READ(NCOF,*,END=999,ERR=901) (CHAINE(J),J=1,N),LAW,R1
        FRTAB%ADR(IZONE)%P%RTYPE = 5
        FRTAB%ADR(IZONE)%P%RCOEF = R1
        FRTAB%ADR(IZONE)%P%NDEF  = 0.D0
        N = N + 2
      CASE('LOGW')
        IF (((N .EQ. 1).AND.(TYP .EQ. 1))  .OR.
     &      ((N .EQ. 4).AND.(TYP .EQ. 2))) THEN
          WRITE(LU,8) NOMCOF, IZONE
          CALL PLANTE(1)
          STOP
        ENDIF
        READ(NCOF,*,END=999,ERR=901) (CHAINE(J),J=1,N),LAW,R1
        FRTAB%ADR(IZONE)%P%RTYPE = 6
        FRTAB%ADR(IZONE)%P%RCOEF = R1
        FRTAB%ADR(IZONE)%P%NDEF  = 0.D0
        N = N + 2
      CASE('COWH')
        READ(NCOF,*,END=999,ERR=907) (CHAINE(J),J=1,N),LAW,R1,R2
        FRTAB%ADR(IZONE)%P%RTYPE = 7
        FRTAB%ADR(IZONE)%P%RCOEF = R1
        FRTAB%ADR(IZONE)%P%NDEF  = R2
        N = N + 3
      CASE DEFAULT
        WRITE(LU,10) LINE, IZ1, IZ2, LAW
        CALL PLANTE(1)
        STOP
      END SELECT
!
!     READ NON-SUBMERGED COEFFICIENT IF NEEDED
!
      IF(VEGETATION) THEN
        BACKSPACE(NCOF)
        SELECT CASE (LAWVEG)

        CASE ('NULL')
          READ(NCOF,*,END=999,ERR=888)
          FRTAB%ADR(IZONE)%P%VTYPE = 0
          WRITE(LU,11) FRTAB%ADR(IZONE)%P%GNUMB(1),
     &                 FRTAB%ADR(IZONE)%P%GNUMB(2),
     &                 LAW,
     &                 FRTAB%ADR(IZONE)%P%RCOEF,
     &                 FRTAB%ADR(IZONE)%P%NDEF ,
     &                 LAWVEG
        CASE ('LIND')
          READ(NCOF,*,END=999,ERR=888)
     &        (CHAINE(J),J=1,N),LAWVEG,(R3(J),J=1,2)
          FRTAB%ADR(IZONE)%P%VTYPE = 1
          DO J=1,2
            FRTAB%ADR(IZONE)%P%VCOEF(J) = R3(J)
          ENDDO
          WRITE(LU,11) FRTAB%ADR(IZONE)%P%GNUMB(1),
     &                 FRTAB%ADR(IZONE)%P%GNUMB(2),
     &                 LAW,
     &                 FRTAB%ADR(IZONE)%P%RCOEF,
     &                 FRTAB%ADR(IZONE)%P%NDEF ,
     &                 LAWVEG,
     &                 (FRTAB%ADR(IZONE)%P%VCOEF(J),J=1,2)
        CASE ('JAER')
          READ(NCOF,*,END=999,ERR=888)
     &        (CHAINE(J),J=1,N),LAWVEG,(R3(J),J=1,5)
          FRTAB%ADR(IZONE)%P%VTYPE = 2
          DO J=1,5
            FRTAB%ADR(IZONE)%P%VCOEF(J) = R3(J)
          ENDDO
          WRITE(LU,11) FRTAB%ADR(IZONE)%P%GNUMB(1),
     &                 FRTAB%ADR(IZONE)%P%GNUMB(2),
     &                 LAW,
     &                 FRTAB%ADR(IZONE)%P%RCOEF,
     &                 FRTAB%ADR(IZONE)%P%NDEF ,
     &                 LAWVEG,
     &                 (FRTAB%ADR(IZONE)%P%VCOEF(J),J=1,5)
        CASE ('WHIT')
          READ(NCOF,*,END=999,ERR=888)
     &        (CHAINE(J),J=1,N),LAWVEG,(R3(J),J=1,6)
          FRTAB%ADR(IZONE)%P%VTYPE = 3
          DO J=1,6
            FRTAB%ADR(IZONE)%P%VCOEF(J) = R3(J)
          ENDDO
          WRITE(LU,11) FRTAB%ADR(IZONE)%P%GNUMB(1),
     &                 FRTAB%ADR(IZONE)%P%GNUMB(2),
     &                 LAW,
     &                 FRTAB%ADR(IZONE)%P%RCOEF,
     &                 FRTAB%ADR(IZONE)%P%NDEF ,
     &                 LAWVEG,
     &                 (FRTAB%ADR(IZONE)%P%VCOEF(J),J=1,6)
        CASE ('BAPT')
          READ(NCOF,*,END=999,ERR=888)
     &        (CHAINE(J),J=1,N),LAWVEG,(R3(J),J=1,3)
          FRTAB%ADR(IZONE)%P%VTYPE = 4
          DO J=1,3
            FRTAB%ADR(IZONE)%P%VCOEF(J) = R3(J)
          ENDDO
          WRITE(LU,11) FRTAB%ADR(IZONE)%P%GNUMB(1),
     &                 FRTAB%ADR(IZONE)%P%GNUMB(2),
     &                 LAW,
     &                 FRTAB%ADR(IZONE)%P%RCOEF,
     &                 FRTAB%ADR(IZONE)%P%NDEF ,
     &                 LAWVEG,
     &                 (FRTAB%ADR(IZONE)%P%VCOEF(J),J=1,3)
        CASE ('HUTH')
          READ(NCOF,*,END=999,ERR=888)
     &        (CHAINE(J),J=1,N),LAWVEG,(R3(J),J=1,4)
          FRTAB%ADR(IZONE)%P%VTYPE = 5
          DO J=1,4
            FRTAB%ADR(IZONE)%P%VCOEF(J) = R3(J)
          ENDDO
          WRITE(LU,11) FRTAB%ADR(IZONE)%P%GNUMB(1),
     &                 FRTAB%ADR(IZONE)%P%GNUMB(2),
     &                 LAW,
     &                 FRTAB%ADR(IZONE)%P%RCOEF,
     &                 FRTAB%ADR(IZONE)%P%NDEF ,
     &                 LAWVEG,
     &                 (FRTAB%ADR(IZONE)%P%VCOEF(J),J=1,4)
        CASE ('VANV')
          READ(NCOF,*,END=999,ERR=888)
     &        (CHAINE(J),J=1,N),LAWVEG,(R3(J),J=1,3)
          FRTAB%ADR(IZONE)%P%VTYPE = 6
          DO J=1,3
            FRTAB%ADR(IZONE)%P%VCOEF(J) = R3(J)
          ENDDO
          WRITE(LU,11) FRTAB%ADR(IZONE)%P%GNUMB(1),
     &                 FRTAB%ADR(IZONE)%P%GNUMB(2),
     &                 LAW,
     &                 FRTAB%ADR(IZONE)%P%RCOEF,
     &                 FRTAB%ADR(IZONE)%P%NDEF ,
     &                 LAWVEG,
     &                 (FRTAB%ADR(IZONE)%P%VCOEF(J),J=1,3)
        CASE ('LUNE')
          READ(NCOF,*,END=999,ERR=888)
     &        (CHAINE(J),J=1,N),LAWVEG,(R3(J),J=1,4)
          FRTAB%ADR(IZONE)%P%VTYPE = 7
          DO J=1,4
            FRTAB%ADR(IZONE)%P%VCOEF(J) = R3(J)
          ENDDO
          WRITE(LU,11) FRTAB%ADR(IZONE)%P%GNUMB(1),
     &                 FRTAB%ADR(IZONE)%P%GNUMB(2),
     &                 LAW,
     &                 FRTAB%ADR(IZONE)%P%RCOEF,
     &                 FRTAB%ADR(IZONE)%P%NDEF ,
     &                 LAWVEG,
     &                 (FRTAB%ADR(IZONE)%P%VCOEF(J),J=1,4)
        CASE ('VAST')
          READ(NCOF,*,END=999,ERR=888)
     &        (CHAINE(J),J=1,N),LAWVEG,(R3(J),J=1,9)
          FRTAB%ADR(IZONE)%P%VTYPE = 8
          DO J=1,9
            FRTAB%ADR(IZONE)%P%VCOEF(J) = R3(J)
          ENDDO
          WRITE(LU,11) FRTAB%ADR(IZONE)%P%GNUMB(1),
     &                 FRTAB%ADR(IZONE)%P%GNUMB(2),
     &                 LAW,
     &                 FRTAB%ADR(IZONE)%P%RCOEF,
     &                 FRTAB%ADR(IZONE)%P%NDEF ,
     &                 LAWVEG,
     &                 (FRTAB%ADR(IZONE)%P%VCOEF(J),J=1,9)
        CASE ('HYBR')
          READ(NCOF,*,END=999,ERR=888)
     &        (CHAINE(J),J=1,N),LAWVEG,(R3(J),J=1,5)
          FRTAB%ADR(IZONE)%P%VTYPE = 9
          DO J=1,5
            FRTAB%ADR(IZONE)%P%VCOEF(J) = R3(J)
          ENDDO
          WRITE(LU,11) FRTAB%ADR(IZONE)%P%GNUMB(1),
     &                 FRTAB%ADR(IZONE)%P%GNUMB(2),
     &                 LAW,
     &                 FRTAB%ADR(IZONE)%P%RCOEF,
     &                 FRTAB%ADR(IZONE)%P%NDEF ,
     &                 LAWVEG,
     &                 (FRTAB%ADR(IZONE)%P%VCOEF(J),J=1,5)
        CASE DEFAULT
          WRITE(LU,10) LINE, IZ1, IZ2, LAWVEG
          CALL PLANTE(1)
          STOP
        END SELECT
      ENDIF
!
      GO TO 1000
1001  CONTINUE
!
      WRITE(LU,3)
!
      WRITE(LU,14) NZONES
!
      GOTO 997
!
      ! ============ !
      ! ERROR FORMAT !
      ! ============ !
!
2     FORMAT('NO FRICTION DATA FILE')
3     FORMAT('-------------------------------------------------------'
     &     , '------------------------------------------------------')
4     FORMAT('                      BOTTOM                         '
     &     , '       VEGETATION'
     &     ,/'       NO    ZONE     LAW  RCOEF    NDEF   '
     &     , ' VEGLAW   VEGETATION PARAMETERS')
6     FORMAT('TOO MANY NUMBER OF FRICTION ZONES DEFINED'
     &     ,/'INCREASED THE NUMBER OF MAXIMAL ZONES WITH THE KEYWORD:'
     &     ,/'MAXIMUM NUMBER OF ZONES FOR THE FRICTION')
8     FORMAT('FRICTION DATA FILE: ',A144
     &      ,/'ZONE: ',I9
     &      ,/'LOG LAW CANNOT BE USED FOR THE BOTTOM')
10    FORMAT('FRICTION DATA FILE'
     &     ,/'READ ERROR LINE',I10
     &     ,/'ZONE FROM ',I10,' TO ',I10
     &     ,/'LAW ',A4)
11    FORMAT(' ',I8,I8,A8,2(F8.4),A8,15(F8.4))
!
14    FORMAT(I5,' ZONES TYPE SPECIFICATIONS')
16    FORMAT('FRICTION DATA FILE : ',A144
     &      ,/'ZONE : ',I9
     &      ,/'FRICTION COEFFICIENT OF HAALAND LAW FOR'
     &      ,/'BOUNDARY CONDITION MUST BE THE SAME AS THE BOTTOM :'
     &      , E12.4)
!
      ! END OF FILE
      ! -----------
999   CONTINUE
      WRITE(LU,*) 'FRICTION DATA FILE: ',NOMCOF
      WRITE(LU,*) 'ABNORMAL END OF FILE'
      WRITE(LU,*) 'CHECK ALL VALUE HAVE BEEN WRITTEN'
      CALL PLANTE(1)
      STOP
!
      ! INDEX AND FIRST LAW OF THE ZONE
      ! -------------------------------
998   CONTINUE
      WRITE(LU,*) 'FRICTION DATA FILE  ',NOMCOF
      WRITE(LU,*) 'READ ERROR ZONE: ',CHAINE(1)
      CALL PLANTE(0)
      STOP
      ! VEGETATION PARAMETER
      ! ----------------------------------
888   CONTINUE
      WRITE(LU,*) 'FRICTION DATA FILE: ',NOMCOF
      WRITE(LU,*) 'READ ERROR FOR VEG. COEF., ZONE: ',CHAINE(1)
      CALL PLANTE(0)
      STOP
!
      ! NO FRICTION LAW
      ! ---------------
900   CONTINUE
      WRITE(LU,*) 'FRICTION DATA FILE'
      WRITE(LU,*) 'READ ERROR ZONE: ',CHAINE(1)
      WRITE(LU,*) 'DEFINE ONLY THE NAME OF THE LAW: NOFR'
      CALL PLANTE(1)
      STOP
!
      ! HAALAND-CHEZY-STRICKLER-MANNING-NIKURADSE-LOG WALL LAWS
      ! -------------------------------------------------------
901   CONTINUE
      WRITE(LU,*) 'FRICTION DATA FILE: ',NOMCOF
      WRITE(LU,*) 'READ ERROR ZONE: ',CHAINE(1)
      WRITE(LU,*) 'DEFINE THE NAME OF THE LAW :',LAW//
     &              ' AND THE FRICTION COEFFICIENT'
      CALL PLANTE(1)
      STOP
!
      ! COLEBROOK WHITE LAW
      ! -------------------
907   CONTINUE
      WRITE(LU,*) 'FRICTION DATA FILE: ',NOMCOF
      WRITE(LU,*) 'READ ERROR ZONE: ',CHAINE(1)
      WRITE(LU,*) 'DEFINE THE NAME OF THE LAW: ',LAW//
     &              ' AND THE FRICTION COEFFICIENT'//
     &              ' AND DEFAULT MANNING'
      CALL PLANTE(1)
      STOP
!
997   CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
