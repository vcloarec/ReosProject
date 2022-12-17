!                   *****************
                    SUBROUTINE LECSNG
!                   *****************
!
     &(IOPTAN,IFIC)
!
!***********************************************************************
! TELEMAC2D   V7P2                                   21/08/2010
!***********************************************************************
!
!brief    READS THE DATA DEFINING SINGULARITIES
!+                FROM FORMATTED FILE 1.
!
!history  V. GUINOT (LHF)
!+        19/04/1996
!+
!+
!
!history  J.-M. HERVOUET (LNH)
!+        03/10/1996
!+        V5P2
!+   MODIFIED
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
!history  J.-M. HERVOUET (LNH)
!+        04/08/2011
!+        V6P2
!+   Numdig in file is now the global number of points
!+   it is read here as such, and then changed into the former
!+   definition, boundary points numbers. This allows a treatment
!+   in parallel.
!
!history  C.COULET (ARTELIA)
!+        30/03/2012
!+        V6P2
!+   Separation of weirs and culvert file
!
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        21/03/2013
!+        V6P3
!+   Modification for new treatment of weirs and dynamic allocation
!
!history  J.-M. HERVOUET (EDF LAB, LNHE)
!+        05/08/2013
!+        V6P3
!+   Setting QP0 to 0 must be protected by a test on TYPSEUIL
!
!history  J.-M. HERVOUET (EDF LAB, LNHE)
!+        01/04/2014
!+        V7P0
!+   ERR= statements added in READ commands.
!
!history  C.COULET (ARTELIA)
!+        01/09/2016
!+        V7P2
!+   Simplification of this subroutine (only called if typseuil=1)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IFIC           |-->| LOGICAL UNIT OF FORMATED DATA FILE 1
!| IOPTAN         |<--| OPTION FOR TANGENTIAL VELOCITIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: IFIC
      INTEGER, INTENT(INOUT) :: IOPTAN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,I,IPTFR,NNWEIRS
!
      CHARACTER(LEN=6) :: NOM
      CHARACTER(LEN=1), PARAMETER :: CHIFFRE(0:9) =
     &           (/'0','1','2','3','4','5','6','7','8','9'/)
!
!-----------------------------------------------------------------------
!
      MAXNPS=0
!     COMMENT LINE
      READ(IFIC,*,END=900,ERR=900)
!     NUMBER OF WEIRS, OPTION FOR TANGENTIAL VELOCITY
      READ(IFIC,*,END=900,ERR=998) NNWEIRS,IOPTAN
!
!     COHERENCE WITH THE STEERING FILE
!
      IF(NNWEIRS.NE.NWEIRS) THEN
        WRITE(LU,*) 'LECSNG : NUMBER OF WEIRS:',NNWEIRS
        WRITE(LU,*) '         DIFFERENT FROM THE ONE GIVEN IN THE'
        WRITE(LU,*) '         PARAMETER FILE: ',NWEIRS
        CALL PLANTE(1)
        STOP
      ENDIF
!
      WRITE(LU,*)'LECSNG : NUMBER OF WEIRS :',NWEIRS
!
!     ALLOCATIONS OF BLOCKS
!
      IF(NWEIRS.GT.0) THEN
        CALL BIEF_ALLVEC(2,NPSING,'NPSING',NWEIRS,1,0,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,NPSING,'NPSING',0,1,0,MESH)
      ENDIF
      CALL ALLBLO(NDGA1 ,'NDGA1 ')
      CALL ALLBLO(NDGB1 ,'NDGB1 ')
      CALL ALLBLO(ZDIG  ,'ZDIG  ')
      CALL ALLBLO(PHIDIG,'PHIDIG')
!
      DO N=1,NWEIRS
        READ(IFIC,*,ERR=900,END=900)
        READ(IFIC,*,ERR=900,END=900)
        READ(IFIC,*,END=900,ERR=997) NPSING%I(N)
        MAXNPS = MAX(MAXNPS,NPSING%I(N))
!
!     ALLOCATIONS IN EACH BLOCK
!
      IF(N.LE.NDGA1%MAXBLOCK) THEN
        NOM='      '
        IF(N.LT.10) THEN
          NOM(4:4) = CHIFFRE(N)
        ELSEIF(N.LT.100) THEN
          NOM(4:4) = CHIFFRE(N/10)
          NOM(5:5) = CHIFFRE(N-10*(N/10))
        ELSEIF(N.LT.1000) THEN
          NOM(4:4) = CHIFFRE(N/100)
          NOM(5:5) = CHIFFRE((N-100*(N/100))/10)
          NOM(6:6) = CHIFFRE((N-100*(N/100))-10*((N-100*(N/100))/10))
        ELSE
          WRITE(LU,*) 'MORE THAN 999 BREACHS ASKED IN LECSNG'
          CALL PLANTE(1)
          STOP
        ENDIF
!
        NOM(1:3) = 'NA1'
        CALL FIRST_ALL_BIEFOBJ(NDGA1%ADR(N)%P)
        NDGA1%N=NDGA1%N+1
        CALL BIEF_ALLVEC(2,NDGA1%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
        NDGA1%ADR(N)%P%FATHER = NDGA1%NAME
        NOM(1:3) = 'NB1'
        CALL FIRST_ALL_BIEFOBJ(NDGB1%ADR(N)%P)
        NDGB1%N=NDGB1%N+1
        CALL BIEF_ALLVEC(2,NDGB1%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
        NDGB1%ADR(N)%P%FATHER = NDGB1%NAME
        NOM(1:3) = 'ZDG'
        CALL FIRST_ALL_BIEFOBJ(ZDIG%ADR(N)%P)
        ZDIG%N=ZDIG%N+1
        CALL BIEF_ALLVEC(1,ZDIG%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
        ZDIG%ADR(N)%P%FATHER = ZDIG%NAME
!       SPECIFIC
        NOM(1:3) = 'PDG'
        PHIDIG%N=PHIDIG%N+1
        CALL FIRST_ALL_BIEFOBJ(PHIDIG%ADR(N)%P)
        CALL BIEF_ALLVEC(1,PHIDIG%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
        PHIDIG%ADR(N)%P%FATHER = PHIDIG%NAME
      ELSE
        WRITE(LU,*) 'LECSNG:'
        WRITE(LU,*) 'MORE THAN ',NDGA1%MAXBLOCK,'(',N,')'
        WRITE(LU,*) 'VECTORS TO BE ALLOCATED'
        WRITE(LU,*) 'CHANGE MAXBLOCK IN ALLBLO.'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      READ(IFIC,*,END=900)
      READ(IFIC,*,ERR=996) (NDGA1%ADR(N)%P%I(I),I=1,NPSING%I(N))
      READ(IFIC,*,END=900)
      READ(IFIC,*,ERR=994) (NDGB1%ADR(N)%P%I(I),I=1,NPSING%I(N))
      READ(IFIC,*,END=900)
      READ(IFIC,*,ERR=992) (ZDIG%ADR(N)%P%R(I),I=1,NPSING%I(N))
      READ(IFIC,*,END=900)
      READ(IFIC,*,ERR=991) (PHIDIG%ADR(N)%P%R(I),I=1,NPSING%I(N))
      ENDDO ! N
!
!     RETRIEVING BOUNDARY POINTS NUMBERS
!     WITH MINUS SIGN TO TRACE POINTS WHICH ARE NOT IN THE DOMAIN
!     SEE STEP 2.
!
!     1) SIDES 1 AND 2
!
      IF(NCSIZE.GT.1) THEN
!
        DO N=1,NWEIRS
          DO I=1,NPSING%I(N)
            DO IPTFR=1,NPTFR
              IF(NDGA1%ADR(N)%P%I(I).EQ.
     &           MESH%KNOLG%I(MESH%NBOR%I(IPTFR))) THEN
                NDGA1%ADR(N)%P%I(I)=-IPTFR
                EXIT
              ENDIF
            ENDDO
            DO IPTFR=1,NPTFR
              IF(NDGB1%ADR(N)%P%I(I).EQ.
     &           MESH%KNOLG%I(MESH%NBOR%I(IPTFR))) THEN
                NDGB1%ADR(N)%P%I(I)=-IPTFR
                EXIT
              ENDIF
            ENDDO
          ENDDO
        ENDDO
!
      ELSE
!
        DO N=1,NWEIRS
          DO I=1,NPSING%I(N)
            DO IPTFR=1,NPTFR
              IF(NDGA1%ADR(N)%P%I(I).EQ.MESH%NBOR%I(IPTFR)) THEN
                NDGA1%ADR(N)%P%I(I)=-IPTFR
                EXIT
              ENDIF
            ENDDO
            DO IPTFR=1,NPTFR
              IF(NDGB1%ADR(N)%P%I(I).EQ.MESH%NBOR%I(IPTFR)) THEN
                NDGB1%ADR(N)%P%I(I)=-IPTFR
                EXIT
              ENDIF
            ENDDO
          ENDDO
        ENDDO
!
      ENDIF
!
!     2) NOW PUTTING A POSITIVE NUMBER IF POINT IN DOMAIN
!                    AND ZERO IF POINT NOT IN DOMAIN (PARALLELISM)
!
      DO N=1,NWEIRS
        DO I=1,NPSING%I(N)
          NDGA1%ADR(N)%P%I(I)=MAX(-NDGA1%ADR(N)%P%I(I),0)
          NDGB1%ADR(N)%P%I(I)=MAX(-NDGB1%ADR(N)%P%I(I),0)
        ENDDO
      ENDDO
!
      GO TO 1000
!
!-----------------------------------------------------------------------
!     ERROR MESSAGES
!-----------------------------------------------------------------------
!
998   CONTINUE
      WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
      WRITE(LU,*) '         WEIRS DATA FILE'
      WRITE(LU,*) '         AT LINE 2'
      GO TO 2000
!
997   CONTINUE
      WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
      WRITE(LU,*) '         WEIRS DATA FILE'
      WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
      WRITE(LU,*) '         THE NUMBER OF POINTS CANNOT BE READ'
      GO TO 2000
!
996   CONTINUE
      WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
      WRITE(LU,*) '         WEIRS DATA FILE'
      WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
      WRITE(LU,*) '         THE NUMBER OF THE POINTS CANNOT BE READ'
      WRITE(LU,*) '         FOR SIDE NUMBER 1'
      GO TO 2000
!
994   CONTINUE
      WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
      WRITE(LU,*) '         WEIRS DATA FILE'
      WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
      WRITE(LU,*) '         THE NUMBER OF THE POINTS CANNOT BE READ'
      WRITE(LU,*) '         FOR SIDE NUMBER 2'
      GO TO 2000
!
992   CONTINUE
      WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
      WRITE(LU,*) '         WEIRS DATA FILE'
      WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
      WRITE(LU,*) '         ELEVATIONS ON THE WEIR CANNOT BE READ'
      GO TO 1000
!
991   CONTINUE
      WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
      WRITE(LU,*) '         WEIRS DATA FILE'
      WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
      WRITE(LU,*) '         DISCHARGE COEFFICIENTS CANNOT BE READ'
      GO TO 2000
!
900   CONTINUE
      WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
      WRITE(LU,*) '         WEIRS DATA FILE'
      WRITE(LU,*) '         UNEXPECTED END OF FILE'
!
2000  CONTINUE
!
      NWEIRS = 0
!
1000  CONTINUE
!
      IF(NWEIRS.EQ.0) THEN
        WRITE(LU,*)
        WRITE(LU,*)'LECSNG : READ ERROR'
        WRITE(LU,*)'         NO SINGULARITY WILL BE TAKEN'
        WRITE(LU,*)'         INTO ACCOUNT'
        WRITE(LU,*)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
