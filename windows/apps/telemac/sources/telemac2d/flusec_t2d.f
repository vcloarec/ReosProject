!                   *********************
                    SUBROUTINE FLUSEC_T2D
!                   *********************
!
     &(GLOSEG,DIMGLO,DT,MESH,FLODEL,DOPLOT)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief  COMPUTES FLUXES OVER LINES (FLUXLINES/CONTROL SECTIONS) VIA
!+      FLODEL
!+
!+      THE FLUXES OF THE SEGMENTS ARE ALLREADY COMPUTED IN THE POSITIVE
!+      DEPTHS ROUTINE (BIEF)
!+
!+      IN A FIRST STEP WE SEARCH AND SAVE ALL NECESSARY SEGMENTS
!+      (ONE NODE IS ON THE LEFT SIDE , THE OTHER ON THE RIGHT SIDE
!+      OF THE FLUXLINE.
!+
!+      DURING LATER CALLS WE SUM UP THE FLUXES FOR EACH SEGMENT AND USE
!+      FLUXPR_TELEMAC2D TO WRITE OUT THE FLUXES
!
!history  L. STADLER (BAW)
!+        17/03/2016
!+        V7P2
!+   New way of computing discharges through control sections.
!+   First version.
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME_FLUSECT2D STEP
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| FLODEL         |<--| FLUXES BETWEEN POINTS (PER SEGMENT)
!| GLOSEG         |-->| GLOBAL NUMBERS OF APICES OF SEGMENTS
!| MESH           |-->| MESH STRUCTURE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY : T2D_FILES,T2DFLX,
     &                            FLUXLINEDATA_FLUSECT2D,
     &                            DEJA_FLUSECT2D,VOLFLUX_FLUSECT2D,
     &                            FLX_FLUSECT2D,
     &                            NUMBEROFLINES_FLUSECT2D,
     &                            TIME_FLUSECT2D
!
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: DIMGLO
      INTEGER, INTENT(IN)          :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(IN) :: DT
      TYPE(BIEF_MESH)              :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN) :: FLODEL
      LOGICAL, INTENT(IN)          :: DOPLOT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VOLFLUX_FLUSECT2D: CUMULATED VOLUME THROUGH SECTIONS
!     FLX_FLUSECT2D: FLUX THROUGH CONTROL SECTIONS
!
      INTEGER, PARAMETER :: MAXEDGES=500
!
      INTEGER I,INP,ISEC,MYPOS,IERR
!
      DOUBLE PRECISION, DIMENSION (2) :: SEG1,SEG2
      DOUBLE PRECISION :: SEGMENTFLUX,SIGN1,SIGN2
!
!
      DOUBLE PRECISION :: SEGXMIN,SEGXMAX
      DOUBLE PRECISION :: SEGYMIN,SEGYMAX
      DOUBLE PRECISION,ALLOCATABLE :: FLUXLINES(:,:)
!
      DOUBLE PRECISION :: SUMFLX, SUMVOLFLUX
!
!
!
!----------------------------------------------------------------------
!
!     PART I
!
!     SEARCH AND SAVE SEGMENTS (FIRST RUN ONLY)
!
!----------------------------------------------------------------------
!
      IF(.NOT.DEJA_FLUSECT2D) THEN
!
        INP=T2D_FILES(T2DFLX)%LU
        TIME_FLUSECT2D = 0.D0
!       READ FLUXLINE FILE
        READ(INP,*) NUMBEROFLINES_FLUSECT2D
!
!       ALLOCATE THE FLUXLINES
        IF(.NOT.ALLOCATED(FLUXLINES)) THEN
          ALLOCATE (FLUXLINES(NUMBEROFLINES_FLUSECT2D,9), STAT=IERR)
          CALL CHECK_ALLOCATE(IERR, 'FLUXLINE:FLUXLINES')
        ENDIF
!       READ NODES INTO FLUXLINE
        DO I = 1,NUMBEROFLINES_FLUSECT2D
          READ(INP,*) ( FLUXLINES(I,ISEC), ISEC=1, 9 )
        ENDDO
!
        WRITE(LU,*) 'FLUXLINES FOUND ',NUMBEROFLINES_FLUSECT2D
!
!------- DYNAMIC ALLOCATION OF FLX_FLUSECT2D, VOLFLUX_FLUSECT2D,...
!
        ALLOCATE(FLX_FLUSECT2D(NUMBEROFLINES_FLUSECT2D,1),STAT=IERR)
        ALLOCATE(VOLFLUX_FLUSECT2D(NUMBEROFLINES_FLUSECT2D,1),STAT=IERR)
        ALLOCATE(FLUXLINEDATA_FLUSECT2D(NUMBEROFLINES_FLUSECT2D),
     &           STAT=IERR)
        DO I = 1,NUMBEROFLINES_FLUSECT2D
          ALLOCATE(FLUXLINEDATA_FLUSECT2D(I)%SECTIONIDS(MAXEDGES),
     &           STAT=IERR)
          ALLOCATE(FLUXLINEDATA_FLUSECT2D(I)%DIRECTION(MAXEDGES),
     &           STAT=IERR)
        ENDDO
        CALL CHECK_ALLOCATE(IERR,"FLUXLINE:DATA")
!
!------ CLEANUP
!
        DO ISEC =1,NUMBEROFLINES_FLUSECT2D
          VOLFLUX_FLUSECT2D(ISEC,1) = 0.D0
          FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS = 0
        ENDDO
!
!-------LOOP OVER ALL MESH SEGMENTS TO STORE THEM FOR EACH FLUXLINE
!
        DO I = 1,MESH%NSEG
!
          SEG1(1) = MESH%X%R(GLOSEG(I,1))
          SEG1(2) = MESH%Y%R(GLOSEG(I,1))
          SEG2(1) = MESH%X%R(GLOSEG(I,2))
          SEG2(2) = MESH%Y%R(GLOSEG(I,2))
!
!         LOOP OVER ALL FLUXLINES
          DO ISEC =1,NUMBEROFLINES_FLUSECT2D
!
!----------------------------------------------------------
!
! SIGN IS USED TO LOOK ON WHICH SIDE OF THE LINE A NODE IS
!
!  - SIGN IS NEGATIVE IF WE ARE ON THE RIGHT SIDE
!  - SIGN IS POSITIVE IF WE ARE ON THE LEFT SIDE
!  - SIGN IS ZERO IF WE ARE ON A POINT
!
!---------------------------------------------------------
!
            SIGN1 = (SEG1(1) - FLUXLINES(ISEC,3))*
     &              (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4)) -
     &              (SEG1(2) - FLUXLINES(ISEC,4)) *
     &              (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))

            SIGN2 = (SEG2(1) - FLUXLINES(ISEC,3))*
     &              (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4)) -
     &              (SEG2(2) - FLUXLINES(ISEC,4)) *
     &              (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))
!
!---------------------------------------------------------
!
! THE FLUXLINE SHOULD NEVER CROSS A NODE (BE ZERO)
! IF THIS HAPPENS WE SHIFT THE NODE (RIGHT AND UPWARDS)
!
!---------------------------------------------------------
!
            IF(SIGN1.EQ.0.D0) THEN
              SIGN1 = (SEG1(1)+0.001D0 - FLUXLINES(ISEC,3)) *
     &                (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4))-
     &                (SEG1(2)+0.001D0 - FLUXLINES(ISEC,4)) *
     &                (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))
            ENDIF
            IF(SIGN2.EQ.0.D0) THEN
              SIGN2 = (SEG2(1)+0.001D0 - FLUXLINES(ISEC,3)) *
     &                (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4))-
     &                (SEG2(2)+0.001D0 - FLUXLINES(ISEC,4)) *
     &                (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))
            ENDIF
!
!           ADD THE SEGMENT ID TO THE NODES
!
            IF(SIGN1*SIGN2.LT.0.D0) THEN
!
              SEGXMIN = MIN(SEG1(1),SEG2(1))
              SEGXMAX = MAX(SEG1(1),SEG2(1))
              SEGYMIN = MIN(SEG1(2),SEG2(2))
              SEGYMAX = MAX(SEG1(2),SEG2(2))
!
              IF(SEGXMIN > FLUXLINES(ISEC,5).AND.
     &           SEGXMAX < FLUXLINES(ISEC,7).AND.
     &           SEGYMIN > FLUXLINES(ISEC,6).AND.
     &           SEGYMAX < FLUXLINES(ISEC,8)) THEN
!
                MYPOS = FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS + 1
                IF(MYPOS.EQ.MAXEDGES) THEN
                  WRITE(LU,*) 'FLUSEC_T2D:'
                  WRITE(LU,*) 'TOO MANY SEGMENTS IN A SECTION'
                  WRITE(LU,*) 'INCREASE MAXEDGES'
                  CALL PLANTE(1)
                  STOP
                ENDIF
                FLUXLINEDATA_FLUSECT2D(ISEC)%SECTIONIDS(MYPOS) = I
                IF(SIGN1.GT.0.D0) THEN
                  FLUXLINEDATA_FLUSECT2D(ISEC)%DIRECTION(MYPOS) = 1
                ELSE
                  FLUXLINEDATA_FLUSECT2D(ISEC)%DIRECTION(MYPOS) = -1
                ENDIF
                FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS = MYPOS
!
!               FOR DEBUGGING
!
!               WRITE(LU,*)'ADDED SEGMENTS ',
!    &                      I,GLOSEG(I,1),GLOSEG(I,2)
!               WRITE(LU,*)'AT COORDINATES ',SEG1,SEG2
!               WRITE(LU,*)'SECTIONS FOUND ',
!    &                      FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        DO ISEC=1,NUMBEROFLINES_FLUSECT2D
!       CASE WHERE NO SEGEMENT WAS ADDED
          IF(FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS.LE.2)THEN
            WRITE(LU,*)'FLUSEC_T2D: WARNING, SECTION (FLUXLINE)',ISEC
            WRITE(LU,*)'            CONTAINS LESS THAN 2 SEGMENTS.'
            WRITE(LU,*)'            POSSIBLE CAUSE: BOX CONTAINING'
            WRITE(LU,*)'            THE FLUXLINE IS TOO SMALL'
          ENDIF
        ENDDO
      ENDIF
!     END SEARCH SEGEMENT (DEJA_FLUSECT2D)
      DEJA_FLUSECT2D = .TRUE.
!
!----------------------------------------------------------------------
!
!     PART II
!
!     ADD THE FLUXES (FLODEL FROM POSITIVE DEPTHS) FOR SEGMENTS
!
!     TODO WE SHOULD THINK ABOUT HOW WE CAN HANDLE THIS IN THE PARALLEL
!          CASE! IF A SEGMENT IS SHARED WE NEED THE HALF FLUX?
!
!----------------------------------------------------------------------
!
      TIME_FLUSECT2D = TIME_FLUSECT2D + DT
!     LOOP OVER ALL FLUXLINES
      DO ISEC =1,NUMBEROFLINES_FLUSECT2D
        FLX_FLUSECT2D(ISEC,1) = 0.D0
!       LOOP OVER SEGMENT
        DO I = 1,FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS
          SEGMENTFLUX =  FLUXLINEDATA_FLUSECT2D(ISEC)%DIRECTION(I) *
     &             FLODEL%R(FLUXLINEDATA_FLUSECT2D(ISEC)%SECTIONIDS(I))
          FLX_FLUSECT2D(ISEC,1) = FLX_FLUSECT2D(ISEC,1) + SEGMENTFLUX
          VOLFLUX_FLUSECT2D(ISEC,1) = VOLFLUX_FLUSECT2D(ISEC,1) +
     &                                (SEGMENTFLUX * DT)
        ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!
!     PART III
!
!----------------------------------------------------------------------
!
      IF(DOPLOT) THEN
        WRITE(LU,*)''
        WRITE(LU,*) '   CONTROL SECTIONS (OPTION 2):'
!       PARALLEL CASE
        IF(NCSIZE.GT.1) THEN
!         PREPARE SINGLE DATA FOR SENDING
          DO I=1,NUMBEROFLINES_FLUSECT2D
            SUMFLX = P_SUM(FLX_FLUSECT2D(I,1))
            SUMVOLFLUX = P_SUM(VOLFLUX_FLUSECT2D(I,1))
            WRITE(LU,'(3X,A9,I3,A1)') 'SECTION ',I,':'
            WRITE(LU,1000) 'TIME FLUX WATER :',
     &      TIME_FLUSECT2D,SUMFLX,SUMVOLFLUX
          ENDDO
!       SERIAL CASE
        ELSE
          DO I=1,NUMBEROFLINES_FLUSECT2D
            WRITE(LU,'(3X,A9,I3,A1)') 'SECTION ',I,':'
            WRITE(LU,1000) 'TIME FLUX WATER :',
     &      TIME_FLUSECT2D, FLX_FLUSECT2D(I,1),VOLFLUX_FLUSECT2D(I,1)
          ENDDO
        ENDIF
      ENDIF
!
1000  FORMAT (5X,A17,G16.7,G16.7,G16.7)
!
!----------------------------------------------------------------------
!
      RETURN
      END
