!                   ************************
                    SUBROUTINE FLUX_EF_VF_3D
!                   ************************
!
     &(FLOW,W2D,W3D,NSEG2D,NELEM2,NELMAX2,MESH2D,INIFLO,
     & IOPT,SENS,IELM3,NPLAN,IKLE,NELMAX,KNOLG)
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    The 3D Element By Element fluxes leaving points are grouped
!+        on every plane (one contribution from below, one from above).
!+        Then every plane is considered like a 2D mesh, and the 2D
!+        EBE fluxes are transformed into horizontal fluxes between
!+        points (with a call to flux_ef_vf). This gives all the 3D
!+        horizontal fluxes between points.
!+
!+
!warning  For element 51 the result is summed on the vertical, and is
!+        a 2D result (see correction_depth_3d where flux_ef_vf_3d is
!+        called, otherwise this routine should not be called for
!+        tetrahedra).
!+
!
!history
!+        15/05/2009
!+        V6P0
!+   INSPIRED FROM LEO POSTMA (DELTARES)
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
!history  J.-M. HERVOUET (LNHE)
!+        26/08/2011
!+        V6P2
!+   Adaptation to element 51 (prisms cut into tetrahedrons)
!
!history  J.-M. HERVOUET (LNHE)
!+        24/08/2012
!+        V6P2
!+   Computation of fluxes and assembly swapped in intermediate planes.
!
!history  J.-M. HERVOUET (LNHE)
!+        11/09/2017
!+        V7P3
!+   Argument NELMAX2 added for dimensioning arrays.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FLOW           |<--| FLUX
!| IELM3          |-->| DISCRETISATION IN 3D
!|                |   | (41: PRISMS 51:PRISMS CUT INTO TETRAHEDRONS)
!| IKLE           |-->| CONNECTIVITY TABLE
!| INIFLO         |-->| IF(YES) FLOW WILL BE INITIALISED AT 0.
!| IOPT           |-->| CHOICE OF THE CONSTANT IN FLUX_EF_VF
!| KNOLG          |-->| GIVES THE ORIGINAL GLOBAL NUMBER OF POINTS
!|                |   | IN SCALAR MODE (SIZE NPOIN3 BUT FILLED ONLY
!|                |   | UP TO NPOIN2)
!| MESH2D         |-->| 2D MESH
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NELMAX2        |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS (SEE IKLE)
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH
!| NSEG2D         |-->| NUMBER OF SEGMENTS IN 2D
!| SENS           |-->| IF 1: HORIZONTAL FLUXES FROM BOTTOM TO TOP
!|                |   | IF 2: HORIZONTAL FLUXES FROM TOP TO BOTTOM
!| W2D            |<--| NON ASSEMBLED FLUXES LEAVING POINTS,PER TRIANGLE
!| W3D            |-->| NON ASSEMBLED FLUXES LEAVING POINTS,PER PRISM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FLUX_EF_VF_3D => FLUX_EF_VF_3D
      USE DECLARATIONS_TELEMAC, ONLY : TETRA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER, INTENT(IN)             :: NSEG2D,NELEM2
!                                             *=NSEG2D*NPLAN+NPOIN2*NETAGE
      INTEGER, INTENT(IN)             :: IOPT,SENS,IELM3,NPLAN,NELMAX
      INTEGER, INTENT(IN)             :: NELMAX2
!                                                    6 IF IELM3=41
!                                                    4 IF IELM3=51
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,*),KNOLG(*)
      DOUBLE PRECISION, INTENT(INOUT) :: FLOW(*)
!                                                   6 IF IELM3=41
!                                                   4 IF IELM3=51
      DOUBLE PRECISION, INTENT(IN)    :: W3D(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: W2D(NELMAX2,3)
      LOGICAL, INTENT(IN)             :: INIFLO
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2D
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER IPLAN,N1,N2,N3,N4,IELEM,I1,I2,I3,S1,S2,S3,IT,IELEM3D,K,L
!
!     TETRA : WILL GIVE THE LOCAL NUMBERS OF POINTS IN THE PRISM
!             THE 0 CORRESPOND TO SITUATIONS
!             THAT NEVER HAPPEN (TETRA(1,1,1,... OR TETRA(2,2,2,...)
!     INTEGER TETRA(2,2,2,3,4)
!     DATA TETRA / 0,1,1,1,1,1,1,0,0,4,4,4,4,4,4,0,0,6,4,5,5,4,6,0,
!    &             0,2,2,2,2,2,2,0,0,6,6,6,6,6,6,0,0,3,1,2,2,1,3,0,
!    &             0,3,3,3,3,3,3,0,0,5,5,5,5,5,5,0,0,2,3,4,1,6,5,0,
!    &             0,4,5,4,6,6,5,0,0,2,3,3,1,2,1,0,0,4,5,3,6,2,1,0 /
!
!-----------------------------------------------------------------------
!
      IF(SENS.NE.1.AND.SENS.NE.2) THEN
        WRITE(LU,*) 'FLUX_EF_VF_3D: UNKNOWN SENS = ',SENS
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(SENS.EQ.2.AND..NOT.INIFLO) THEN
        WRITE(LU,*) 'FLUX_EF_VF_3D: SENS = 2 AND INIFLO = .FALSE.'
        WRITE(LU,*) '               INCOMPATIBLE OPTIONS'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(IELM3.EQ.41) THEN
!
!     CASE OF PRISMS
!
!     ADDS FLUXES ON HORIZONTAL SEGMENTS (FOR SEGMENT NUMBERING IN 3D
!                                         SEE STOSEG41 IN BIEF)
!
!     FOR INTERMEDIATE PLANES
!     HERE WE HAVE THE CHOICE OF COMPUTING THE FLUXES BEFORE OR AFTER
!     ASSEMBLING ON VERTICAL. UP TO VERSION 6.1, ASSEMBLY WAS DONE FIRST
!     FROM VERSION 6.2 ON WE COMPUTE THE FLUXES FIRST AND ASSEMBLE AFTER
!     THIS IS MORE COMPATIBLE WITH DISTRIBUTIVE SCHEMES THAT COMPUTE
!     THEIR FLUXES AT ELEMENT LEVEL. THEN THE FLUX LIMITATION DONE
!     IN NA_FLUX3D_LIM WILL BE COMPATIBLE.
!
      DO IPLAN=1,NPLAN
!
        IF(SENS.EQ.1) THEN
          N1=(IPLAN-1    )*NSEG2D+1
          N2= IPLAN       *NSEG2D
        ELSE
          N1=(  NPLAN-IPLAN)*NSEG2D+1
          N2=(1+NPLAN-IPLAN)*NSEG2D
        ENDIF
!
!       POINTS 1, 2 AND 3 OF UPPER LEVEL
        IF(IPLAN.EQ.1) THEN
!         FIRST PLANE: ONLY POINTS 1, 2 AND 3 OF UPPER LEVEL
          DO IELEM=1,NELEM2
            W2D(IELEM,1)=W3D(IELEM,1)
            W2D(IELEM,2)=W3D(IELEM,2)
            W2D(IELEM,3)=W3D(IELEM,3)
          ENDDO
          CALL FLUX_EF_VF(FLOW(N1:N2),W2D,NSEG2D,NELEM2,MESH2D%NELMAX,
     &                    MESH2D%ELTSEG%I,MESH2D%ORISEG%I,
     &                    MESH2D%IKLE%I,INIFLO,IOPT)
        ELSEIF(IPLAN.EQ.NPLAN) THEN
!         LAST PLANE: ONLY POINTS 4, 5 AND 6 OF LOWER LEVEL
          N3=NELEM2*(IPLAN-2)
          DO IELEM=1,NELEM2
            W2D(IELEM,1)=W3D(N3+IELEM,4)
            W2D(IELEM,2)=W3D(N3+IELEM,5)
            W2D(IELEM,3)=W3D(N3+IELEM,6)
          ENDDO
          CALL FLUX_EF_VF(FLOW(N1:N2),W2D,NSEG2D,NELEM2,MESH2D%NELMAX,
     &                    MESH2D%ELTSEG%I,MESH2D%ORISEG%I,
     &                    MESH2D%IKLE%I,INIFLO,IOPT)
        ELSE
!       INTERMEDIATE PLANE
!         POINTS 4, 5 AND 6 OF LOWER LEVEL
          N3=NELEM2*(IPLAN-2)
          DO IELEM=1,NELEM2
            W2D(IELEM,1)=W3D(N3+IELEM,4)
            W2D(IELEM,2)=W3D(N3+IELEM,5)
            W2D(IELEM,3)=W3D(N3+IELEM,6)
          ENDDO
          CALL FLUX_EF_VF(FLOW(N1:N2),W2D,NSEG2D,NELEM2,MESH2D%NELMAX,
     &                    MESH2D%ELTSEG%I,MESH2D%ORISEG%I,
     &                    MESH2D%IKLE%I,INIFLO,IOPT)
!         POINTS 1, 2, 3 OF UPPER LEVEL
          N4=N3+NELEM2
          DO IELEM=1,NELEM2
            W2D(IELEM,1)=W3D(N4+IELEM,1)
            W2D(IELEM,2)=W3D(N4+IELEM,2)
            W2D(IELEM,3)=W3D(N4+IELEM,3)
          ENDDO
          CALL FLUX_EF_VF(FLOW(N1:N2),W2D,NSEG2D,NELEM2,MESH2D%NELMAX,
     &                    MESH2D%ELTSEG%I,MESH2D%ORISEG%I,
     &                    MESH2D%IKLE%I,.FALSE.,IOPT)
!                                       !!!!!!!
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELM3.EQ.51) THEN
!
!     CASE OF PRISMS CUT INTO TETRAHEDRONS
!
!     ADDS FLUXES ON HORIZONTAL SEGMENTS (FOR SEGMENT NUMBERING IN 3D
!                                         SEE STOSEG51 IN BIEF)
!
!
!     INITIALISING
!
      DO IELEM=1,NELEM2
        W2D(IELEM,1)=0.D0
        W2D(IELEM,2)=0.D0
        W2D(IELEM,3)=0.D0
      ENDDO
!
!     LOOP ON PLANES
!
      DO IPLAN=1,NPLAN
!
!       LOOP ON TRIANGLES
!
        DO IELEM=1,NELEM2
!
!         HERE LOWER LEVEL OF ELEMENTS ALWAYS TAKEN
!         TO FIND THE WAY THE PRISM HAS BEEN CUT BY LOOKING
!         AT GLOBAL NUMBERS OF POINTS
!         THIS PART IS THUS COMMON TO ALL PLANES
!         IKLE 3D IS TAKEN, COULD BE IKLE 2D AS WELL
!         SO NO DEPENDENCE ON LEVEL IPLAN
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
!         IN PARALLELISM BACK TO ORIGINAL GLOBAL NUMBERS
          IF(NCSIZE.GT.1) THEN
            I1=KNOLG(I1)
            I2=KNOLG(I2)
            I3=KNOLG(I3)
          ENDIF
!         THIS IS DONE LIKE IN CPIKLE3 TO USE ARRAY TETRA
          IF(I1.GT.I2) THEN
            S1=1
          ELSE
            S1=2
          ENDIF
          IF(I2.GT.I3) THEN
            S2=1
          ELSE
            S2=2
          ENDIF
          IF(I3.GT.I1) THEN
            S3=1
          ELSE
            S3=2
          ENDIF
!
!         NOW TAKING CONTRIBUTIONS OF TETRAHEDRON K= 1, 2 AND 3
!         OF LOWER AND UPPER LEVEL
!
!         LOWER LEVEL : POINTS 4,5,6 OF ORIGINAL PRISM CONTRIBUTE
!
          IF(IPLAN.GT.1) THEN
!           LOOP ON 3 TETRAHEDRONS
            DO K=1,3
              IELEM3D=3*(IPLAN-2)*NELEM2+IELEM+(K-1)*NELEM2
!             POINTS 1 TO 4
              DO L=1,4
!               ORIGINAL NUMBER IN THE PRISM
                IT=TETRA(S1,S2,S3,K,L)
                IF(IT.GE.4) THEN
                  W2D(IELEM,IT-3)=W2D(IELEM,IT-3)+W3D(IELEM3D,L)
                ENDIF
              ENDDO
            ENDDO
          ENDIF
!
!         UPPER LEVEL : POINTS 1,2,3 OF ORIGINAL PRISM CONTRIBUTE
!
          IF(IPLAN.LT.NPLAN) THEN
!           LOOP ON 3 TETRAHEDRONS
            DO K=1,3
              IELEM3D=3*(IPLAN-1)*NELEM2+IELEM+(K-1)*NELEM2
!             POINTS 1 TO 4
              DO L=1,4
!               ORIGINAL NUMBER IN THE PRISM
                IT=TETRA(S1,S2,S3,K,L)
                IF(IT.LE.3) THEN
                  W2D(IELEM,IT)=W2D(IELEM,IT)+W3D(IELEM3D,L)
                ENDIF
              ENDDO
            ENDDO
          ENDIF
!
        ENDDO
!
        IF(SENS.EQ.1) THEN
          N1=(IPLAN-1    )*NSEG2D+1
          N2= IPLAN       *NSEG2D
        ELSE
          N1=(  NPLAN-IPLAN)*NSEG2D+1
          N2=(1+NPLAN-IPLAN)*NSEG2D
        ENDIF
!
      ENDDO
!
!     HERE 2D FLUXES !!!!!!!!!!!!!
!
      CALL FLUX_EF_VF(FLOW,W2D,NSEG2D,NELEM2,MESH2D%NELMAX,
     &                MESH2D%ELTSEG%I,MESH2D%ORISEG%I,
     &                MESH2D%IKLE%I,INIFLO,IOPT)
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,*) 'UNEXPECTED ELEMENT TYPE IN FLUX_EF_VF_3D'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
