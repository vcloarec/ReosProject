!                       ************************
                        SUBROUTINE SEG_NEIGHBORS
!                       ************************
!
     &(XX,YY,IKLE,NPOIN,NVMAX,NELEM,NELMAX,NSEG,NEISEG)
!
!***********************************************************************
! TELEMAC 2D VERSION 6.2                                       18/06/11
!***********************************************************************
!
!brief
!
!  FUNCTION  : INSPIRED FROM INFCEL.F
!
!              SEG_NEIGHBOUR: TABLE (NSEG,2) THAT CONTAINS THE RIGTH AND
!                             LEFT NEIGHBOUR SEGMENTS OF A GIVEN SEGMENT
!                            (USEFUL FOR 2ND ORDRE RECONSTRUCTION OF FV)
!
!history  RIADH ATA (EDF R&D-LNHE)
!+        07/15/2012
!+        V6P2
!+   First version.
!
!
!history  SARA PAVAN (EDF R&D-LNHE)
!+        07/15/2014
!+        V7P0
!+   correction on the criterion of selection of the neighbour:
!+   GE is replaced by GT in 2 locations in order to overcome very
!+   specific problematic cases
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! |  NSEG          | -->|  NUMBER OF TOTAL MESH EDGES                  |
! |  NVMAX         | -->|  MAXIMUM OF NUMBER FOR A NODE (MXPTVS FOR FE)|
! |  NPOIN         | -->|  TOTAL NUMBER OF NODES                       |
! |  XX,YY         | -->|  COORDINATES OF NODES                        |
! |  NELEM         | -->|  TOTAL NUMBER OF ELEMENTS                    |
! |  IKLE          | -->|  CONNECTIVITY                                |
! !  SEG_NEGHBOUR  !<-- | LEFT & RIGHT NEIGHBOUR SEGMENTS OF A SEGMENT !
! !________________|____|______________________________________________|
!  MODE: -->( UNCHANGEABLE INPUT ),<--(OUTPUT),<-->(CHANGEABLE INPUT)
!-----------------------------------------------------------------------
!
!     - CALLING SUBROUTINE(S) : HYD_FV
!
!***********************************************************************
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG,NELMAX,NPOIN,NVMAX,NELEM
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3)
      INTEGER, INTENT(INOUT)          :: NEISEG(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: XX(NPOIN),YY(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NB1,NB2,ISEG,IS,KV
      INTEGER I1,I2,I3,IS1,IS2,KV1,KV2
      INTEGER IEL,JARET,ERR
!
      INTEGER NUBO1,NUBO2,NV1,J,NSG
!
      INTEGER NU(3),NEX(3),NUB1,NUBO(2,NSEG)
!
      DOUBLE PRECISION EPS
      DOUBLE PRECISION UNSIX
!
      DOUBLE PRECISION A,NORMNUB,XNUB12,YNUB12
      DOUBLE PRECISION XM,YM,NORM_MNUBO1,NORM_MNUBO2,NORM_JM
      DOUBLE PRECISION NORM_JNUBO1,NORM_JNUBO2
      DOUBLE PRECISION MINANGLE,SINANGLE
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: JVOIS
!
      INTRINSIC ABS,SQRT
!
!-----------------------------------------------------------------------
!
!  JVOIS: TABLE OF VERTICES (1) AND SEGMENTS (2) NEIGHBORS OF THE VERTEX.
!
      ALLOCATE(JVOIS(NPOIN,NVMAX,2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'JVOIS')
!
      EPS=1.D-5
!
!  NEX  : POINTER FOR THE FOLLOWING NODE IN AN ELEMENT
!
! 1. INITIALIZATION.
!
      DO IS = 1 , NPOIN
        DO KV = 1 , NVMAX
          JVOIS(IS,KV,1) = 0
          JVOIS(IS,KV,2) = 0
        ENDDO
      ENDDO
!
      ISEG = 0
      UNSIX = 1.D0/6.D0
!
! 2. CONSTRUCTION OF TABLE : JVOIS
!
!  -->  ORIENTATION.
!
      NEX(1) = 2
      NEX(2) = 3
      NEX(3) = 1
!
!      LOOP OVER ELEMENTS
!     ***********************
!
      DO IEL = 1 , NELEM
!
        I1 = IKLE(IEL,1)
        I2 = IKLE(IEL,2)
        I3 = IKLE(IEL,3)
!
        NU(1) = I1
        NU(2) = I2
        NU(3) = I3
!
!       LOOP OVER THE 3 NODES OF THE ELEMENT IEL
! ----  *******************************************
!
        DO IS = 1 , 3
          IS1 = NU(IS)
          IS2 = NU(NEX(IS))
!
!         IS2 IS IT A NEIGHBOR OF IS1? ---> LOOP OVER NEGHBORS OF IS1
!
          DO KV1 = 1 , NVMAX
            IF (JVOIS (IS1, KV1, 1) .EQ. 0) GO TO 43
            IF (JVOIS (IS1, KV1, 1) .EQ. IS2) GO TO 44
          ENDDO ! KV1
!
 43       CONTINUE
          JVOIS (IS1, KV1, 1) = IS2
!
!         IS1 IS IT NEIGHBOR OF IS2? --->LOOP OVER NEIGHBORS OF IS2
!
          DO KV2 = 1 , NVMAX
            IF (JVOIS (IS2, KV2, 1) .EQ. 0) GO TO 46
          ENDDO ! KV2
!
 46       CONTINUE
!
!  -->    CONSTRUCTION OF EDGES AND NEIGHBORS.
!
          ISEG = ISEG + 1
!
          JVOIS(IS2,KV2,1) = IS1
          JVOIS(IS1,KV1,2) = ISEG
          JVOIS(IS2,KV2,2) = ISEG
!
          JARET = ISEG
          NUBO (1, ISEG) = -IS1
          NUBO (2, ISEG) = IS2
!
          UNSIX = ABS(UNSIX)
!
          GO TO 48
!
 44       CONTINUE
!         EDGE ALREADY CONSIDERED
!
          JARET = JVOIS (IS1, KV1, 2)
          NUBO(1,JARET) = - NUBO(1,JARET)
          NB1 = NUBO(1,JARET)
          NB2 = NUBO(2,JARET)
          IF(NB1.EQ.IS1.AND.NB2.EQ.IS2) THEN
            UNSIX = ABS(UNSIX)
          ELSE
            IF(NB2.EQ.IS1.AND.NB1.EQ.IS2) THEN
              UNSIX = - ABS(UNSIX)
            ENDIF
          ENDIF
!
!           END OF THE LOOP OVER EDGES
! ----    *********************************
48    CONTINUE
        ENDDO ! IS
      ENDDO ! IEL
!
!   CONSTRUCTION OF EDGE OF THE BOUNDARY
!
      DO ISEG =1, NSEG
        NUB1 = NUBO(1,ISEG)
        IF(NUB1.LT.0) THEN
!
!        THIS IS A BOUNDARY EDGE
!
          NUBO(1,ISEG) = - NUBO(1,ISEG)
          IS1=NUBO(1,ISEG)
          IS2=NUBO(2,ISEG)
!
        ENDIF
      ENDDO ! ISEG
!
!---------------------------------------------------------------------
!
! ADDED FOR WAF SCHEMES : CONSTRUCTION OF SEG_NEIGHBOUR
! FIND THE RIGHT AND LEFT EDGES WHICH ARE THE CLOSEST TO THE EDGE DIRECTION
! WE EXPLOIT HERE THE MATRIX JVOIS: FOR A NODE I: THERE EXIST MAX NVMAX NEIGHBOURS
!    WHICH ARE STOCKED IN THE VECTOR JVOIS(I,NVMAX,1)
!    THESE NEIGNBOURS J ARE LINKED TO I VIA THE SEGMENTS STOCKED IN THE VECTOR
!    JVOIS(I,NVMAX,2)
!
!
!    INITIALISATION
      DO ISEG=1,NSEG
        NEISEG(1,ISEG)=0
        NEISEG(2,ISEG)=0
      ENDDO
!
      DO ISEG=1,NSEG
!       RECUPERATE NODES
        NUBO1 = NUBO (1,ISEG)
        NUBO2 = NUBO (2,ISEG)
!       VECTOR NUB12
        XNUB12 = XX(NUBO2)-XX(NUBO1)
        YNUB12 = YY(NUBO2)-YY(NUBO1)
!       ITS NORM
        NORMNUB =SQRT(XNUB12**2 + YNUB12**2)
        IF(NORMNUB.LE.EPS) THEN
          WRITE(LU,*)'PROBLEM: SEGMENT LENGTH =0'
          WRITE(LU,*)'WE ARE IN SEG_NEIGHBORS.F'
          WRITE(LU,*)'SEGMENT OF INTEREST  :',ISEG
          WRITE(LU,*)'NODES ARE  :',NUBO1,NUBO2
          CALL PLANTE(1)
          STOP
        ENDIF
!       INITALIALIZE MINANGLE
        MINANGLE = 1.E10
!
!       LET'S START WITH NUBO1
!
!       LET'S RETRIEVE ITS NEIGHBORS
!
        DO NV1= 1,NVMAX
          J   = JVOIS(NUBO1,NV1,1)
          NSG = JVOIS(NUBO1,NV1,2)
          IF(J.NE.0.AND.J.NE.NUBO2)THEN
!           ORTHOGONAL PROJECTION OF J OVER NUB12: M
!           XM= X1 + A*(X2-X1)
!           YM= Y1 + A*(Y2-Y1)
!           WHERE A = [(XJ-X1)(X2-X1)+(YJ-Y1)(Y2-Y1)]/NORMNUB**2
            A = ((XX(J)-XX(NUBO1))*XNUB12+(YY(J)-YY(NUBO1))*YNUB12)/
     &          NORMNUB**2
!           XM & YM
            XM = XX(NUBO1) + A*XNUB12
            YM = YY(NUBO1) + A*YNUB12
!           NORM OF MNUBO1 AND MNUBO2
            NORM_MNUBO1 = SQRT((XM-XX(NUBO1))**2 +
     &                          (YM-YY(NUBO1))**2)
            NORM_MNUBO2 = SQRT((XM-XX(NUBO2))**2 +
     &                          (YM-YY(NUBO2))**2)
!           MAKE THE DECISION: THE FOLLOWING "IF" ELIMINATES THE CASES:
!                            1- WHERE M IS BETWEEN NUB1 AND NUB2(N1--M--N2)
!                            2- WHERE M IS AT THE RIGHT OF NUB2 (N1--N2--M)
            IF((NORM_MNUBO1+NORM_MNUBO2).GT.NORMNUB
     &    .AND. NORM_MNUBO2.GE.NORM_MNUBO1) THEN
!             NORM OF JM
              NORM_JM= SQRT((XX(J)-XM)**2 + (YY(J)-YM)**2)
!             NORM OF JNUBO1
              NORM_JNUBO1 = SQRT((XX(J)-XX(NUBO1))**2 +
     &                            (YY(J)-YY(NUBO1))**2)
!             SIN OF THETA
              SINANGLE = NORM_JM/(NORM_JNUBO1+EPS)
!
              IF(SINANGLE.LT.MINANGLE)THEN
                NEISEG(1,ISEG) = NSG
                MINANGLE       = SINANGLE
              ENDIF
            ENDIF
          ENDIF
        ENDDO
!
!       DO THE SAME FOR NUBO2
!
!       INITALIALIZE MINANGLE
        MINANGLE = 1.E10
!       LET'S RECUPERATE ITS NEIGHBORS
        DO NV1= 1,NVMAX
          J   = JVOIS(NUBO2,NV1,1)
          NSG = JVOIS(NUBO2,NV1,2)
          IF(J.NE.0.AND.J.NE.NUBO1)THEN
!           ORTHOGONAL PROJECTION OF J OVER NUB12: M
!           XM= X1 + A*(X2-X1)
!           YM= Y1 + A*(Y2-Y1)
!           WHERE A = [(XJ-X1)(X2-X1)+(YJ-Y1)(Y2-Y1)]/NORMNUB**2
            A = ((XX(J)-XX(NUBO1))*XNUB12+(YY(J)-YY(NUBO1))*YNUB12)/
     &          NORMNUB**2
!           XM & YM
            XM = XX(NUBO1) + A*XNUB12
            YM = YY(NUBO1) + A*YNUB12
!           NORM OF MNUBO1 AND MNUBO2
            NORM_MNUBO1 = SQRT((XM-XX(NUBO1))**2 +
     &                          (YM-YY(NUBO1))**2)
            NORM_MNUBO2 = SQRT((XM-XX(NUBO2))**2 +
     &                          (YM-YY(NUBO2))**2)
!           MAKE THE DECISION: THE FOLLOWING IF ELIMINATES THE CASES:
!                            1- WHERE M IS BETWEEN NUB1 AND NUB2(N1--M--N2)
!                            2- WHERE M IS AT THE LEFT OF NUBO1 (M--N1--N2)
            IF((NORM_MNUBO1+NORM_MNUBO2).GT.NORMNUB
     &        .AND.NORM_MNUBO1.GE.NORM_MNUBO2)THEN
!             NORM OF JM
              NORM_JM= SQRT((XX(J)-XM)**2 + (YY(J)-YM)**2)
!             NORM OF JNUBO2
              NORM_JNUBO2 = SQRT((XX(J)-XX(NUBO2))**2 +
     &                            (YY(J)-YY(NUBO2))**2)
!             SIN OF THETA
              SINANGLE = NORM_JM/(NORM_JNUBO2+EPS)
!
              IF(SINANGLE.LT.MINANGLE)THEN
                NEISEG(2,ISEG) = NSG
                MINANGLE       = SINANGLE
              ENDIF
            ENDIF
          ENDIF
        ENDDO

! REFLEXION ABOUT HOW TO DEAL WITH BOUNDARY EDGES, ESPECIALLY FOR
! EDGE AT CORNER
! FOR THE MOMENT, THE EDGE IS ITS OWN NEIGHBOR
        IF(NEISEG(1,ISEG).EQ.0)THEN
!           WRITE(LU,*)'PROBLEM AT SEG_NEIGHBORS ....!'
!           WRITE(LU,*)'SEGMENT: ',ISEG,'HAS NO NEIGHBOR'
!           WRITE(LU,*)'ITS NODES ARE  :',NUBO1,NUBO2
!           WRITE(LU,*)'HE WILL BE HIS OWN NEIGHBOR'
          NEISEG(1,ISEG) = ISEG
        ENDIF
        IF(NEISEG(2,ISEG).EQ.0)THEN
!           WRITE(LU,*)'PROBLEM AT SEG_NEIGHBORS ....!'
!           WRITE(LU,*)'SEGMENT: ',ISEG,'HAS NO NEIGHBOR'
!           WRITE(LU,*)'ITS NODES ARE  :',NUBO1,NUBO2
!           WRITE(LU,*)'HE WILL BE HIS OWN NEIGHBOR'
          NEISEG(2,ISEG) = ISEG
        ENDIF
! VERIFICATION THAT THE ROUTINE WORKS WELL
        IF(NEISEG(1,ISEG).LE.0.OR.NEISEG(2,ISEG).LE.0)THEN
          WRITE(LU,*)'PROBLEM AT SEG_NEIGHBORS ....!'
          WRITE(LU,*)'SEGMENT: ',ISEG,'HAS NO NEIGHBOR'
          WRITE(LU,*)'ITS NODES ARE  :',NUBO1,NUBO2
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO ! ISEG
!
      DEALLOCATE(JVOIS)
!
!---------------------------------------------------------------------
!
      RETURN
      END
