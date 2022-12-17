!                   *****************
                    SUBROUTINE INBIEF
!                   *****************
!
     &(LIHBOR,KLOG,IT1,IT2,IT3,LVMAC,IELMX,
     & LAMBD0,SPHERI,MESH,T1,T2,OPTASS,PRODUC,EQUA,MESH2D)
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    PREPARES THE DATA STRUCTURE FOR BIEF.
!+                THE INTEGER AND REAL ARRAYS DESCRIBING THE MESH
!+                ARE BUILT AND STORED IN MESH.
!
!history  J-M HERVOUET (LNHE) ; REGINA NEBAUER; LAM MINH PHUONG; EMILE RAZAFINDRAKOTO
!+        05/02/2010
!+        V6P0
!+   COMP_SEG UPDATED FOR QUADRATIC TRIANGLES
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
!history  J-M HERVOUET (LNHE) ; REGINA NEBAUER; LAM MINH PHUONG; EMILE RAZAFINDRAKOTO
!+        07/12/2011
!+        V6P2
!+   Checking elements for building GLOSEG. New optional argument MESH2D.
!+   for prisms split into tetrahedrons (call of STOSEG51)
!
!history  J-M HERVOUET (LNHE)
!+        10/09/2012
!+        V6P2
!+   Different call to VOISIN31, and call added for IELM=51
!
!history  J-M HERVOUET (LNHE)
!+        06/03/2013
!+        V6P3
!+   XEL, YEL now built in 3D. SURFAC based on XEL and YEL. Mercator
!+   projection treated in 3D.
!
!history R. ATA (EDF R&D - LNHE)
!+        21/05/2013
!+        V6P3
!+   add centre_mass_seg, new infcel, hloc.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        28/03/2014
!+        V7P0
!+   Now written to enable different numbering of boundary points and
!+   boundary segments.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        09/05/2014
!+        V7P0
!+   Allocation of new vectors of I8 integers MESH%WI8 and MESH%TI8
!+   for finite element assembly with integers.
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+        ALLOCATE BUF_SEND_ERR AND BUD_RECV_ERR
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        30/07/2015
!+        V7P1
!+   Call of make_eltcar modified to compute IFAC. FAC suppressed.
!
!history  M.S.TURNBULL (HRW)
!+        24/09/2015
!+        V7P1
!+   Correction to the computation of the both Y and XDEL in spherical
!+      coordinates, accounting for the fact that the Earth is round.
!
!history  J-M HERVOUET (Jubilado)
!+        10/09/2017
!+        V7P3
!+   MESH%IKLE%DIM1 replaced with MESH%IKLE%MAXDIM1 in calls to VOISIN,
!+   VOISIN31 and ELEBD. NELEM replaced with MESH%ELTSEG%DIM1 in the
!+   call to COMP_NH_COM_SEG.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EQUA           |-->| IDENTIFICATION OF PROGRAM OR EQUATIONS SOLVED
!| IELMX          |-->| THE MORE COMPLEX ELEMENT USED (FOR MEMORY)
!| IT1            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| IT2            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| IT3            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| LAMBD0         |-->| LATITUDE OF ORIGIN POINT (SPHERICAL COORDINATES)
!| LIHBOR         |-->| TYPES OF BOUNDARY CONDITIONS ON DEPTH
!| LVMAC          |-->| VECTOR LENGTH (IF VECTOR MACHINE)
!| MESH           |-->| MESH STRUCTURE
!| MESH2D         |-->| UNDERLYING 2D MESH (FOR PRISMS AND PRISMS SPLIT
!|                |   | INTO TETRAHEDRONS)
!| OPTASS         |-->| OPTION FOR MATRIX STORAGE.
!| PRODUC         |-->| OPTION FOR MATRIX x VECTOR PRODUCT.
!| SPHERI         |-->| LOGICAL, IF YES : SPHERICAL COORDINATES.
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_INBIEF => INBIEF
      USE DECLARATIONS_TELEMAC, ONLY : MODASS, CHECK_MESH
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: IELMX,OPTASS,PRODUC,KLOG,LVMAC
      INTEGER, INTENT(IN)            :: LIHBOR(*)
      DOUBLE PRECISION, INTENT(IN)   :: LAMBD0
      LOGICAL, INTENT(IN)            :: SPHERI
      CHARACTER(LEN=20)              :: EQUA
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: T1,T2,IT1,IT2,IT3
      TYPE(BIEF_MESH), INTENT(INOUT), OPTIONAL :: MESH2D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IELEM,NELEM,NELMAX,NPTFR,NPOIN,IELM,IPLAN,I3D
      INTEGER MXPTVS,NPLAN
      INTEGER LV,NDP,IDP,I1,I2,I3,NPOIN2
      INTEGER NPTFR2,NELEM2,NELMAX2,NELEB2,NELEB
      INTEGER NELCOU,IELEM3D
!
      DOUBLE PRECISION X2,X3,Y2,Y3
!
!-----------------------------------------------------------------------
!     FOR CALL TO VOISIN31
      INTEGER IKLESTR(1,3)
!
!     DEPLOYMENT OF THE DATA STRUCTURE
!
      NELEM = MESH%NELEM
      NELMAX= MESH%NELMAX
      NPOIN = MESH%NPOIN
      IELM  = MESH%X%ELM
      NDP   = BIEF_NBPEL(IELM,MESH)
      NPTFR = MESH%NPTFR
      NELEB = MESH%NELEB
!
!     WITH PRISMS, DIFFERENT FROM 2D VALUES, OTHERWISE
!
      IF(IELM.EQ.41.OR.IELM.EQ.51) THEN
        NPOIN2  =BIEF_NBPTS(11,MESH)
        NELEM2  =BIEF_NBPTS(10,MESH)
        NELMAX2 =BIEF_NBMPTS(10,MESH)
        NPTFR2  =BIEF_NBPTS(1,MESH)
        NPLAN   =NPOIN/NPOIN2
      ELSEIF(IELM.EQ.11.OR.IELM.EQ.31) THEN
        NPOIN2  =NPOIN
        NELEM2  =NELEM
        NELMAX2 =NELMAX
        NPTFR2  =NPTFR
        NELEB2  =NELEB
        NPLAN   =1
      ELSE
        WRITE(LU,*) 'UNEXPECTED ELEMENT IN INBIEF:',IELM
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     FINITE ELEMENT ASSEMBLY WITH I8 INTEGERS
!
      IF(MODASS.EQ.2) THEN
        ALLOCATE(MESH%WI8(NELMAX*NDP))
        ALLOCATE(MESH%TI8(NPOIN))
      ENDIF
!
!     PARALLEL MODE : INITIALISES THE ARRAYS NHP,NHM
!                        INDPU,FAC, ETC.
!
!
      IF(NCSIZE.GT.1) THEN
!
        CALL PARINI(MESH%NHP%I,MESH%NHM%I,MESH%INDPU%I,
     &              NPOIN2,MESH%NACHB%I,NPLAN,MESH,
     &              MESH%NB_NEIGHB,MESH%NB_NEIGHB_SEG,
     &              NELEM2,MESH%IFAPAR%I,MODASS)
!
      ELSE
!       THESE STUCTURES ARE ALLOCATED IN PARINI
        CALL BIEF_ALLVEC(2,MESH%NB_NEIGHB_PT,'NBNGPT',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,MESH%LIST_SEND   ,'LSSEND',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,MESH%NH_COM      ,'NH_COM',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,MESH%NB_NEIGHB_PT_SEG,'NBNGSG',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,MESH%LIST_SEND_SEG,'LSSESG',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,MESH%NH_COM_SEG  ,'NH_CSG',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,MESH%BUF_SEND    ,'BUSEND',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,MESH%BUF_RECV    ,'BURECV',0,1,0,MESH)
        IF (MODASS .EQ.3) THEN
          CALL BIEF_ALLVEC(1,MESH%BUF_SEND_ERR ,'BUSEND_ERR',0,1,0,MESH)
          CALL BIEF_ALLVEC(1,MESH%BUF_RECV_ERR ,'BURECV_ERR',0,1,0,MESH)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE NEIGHBOURS OF THE BOUNDARY FACES (TRIANGULAR MESH)
!
!     NOTE: SEE CPIKLE2 AND CPIKLE3 IN 3D. IKLE CAN HERE BE 3D BECAUSE
!           THE BEGINNING OF IKLE IN 3D IS THE SAME AS THAT IN 2D (THE
!           FIRST 3 POINTS OF THE PRISMS OR TETRAHEDRONS CORRESPOND
!           TO THE 3 POINTS OF THE BOTTOM TRIANGLES)
!
!
      IF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
        CALL VOISIN(MESH%IFABOR%I,NELEM2,NELMAX2,IELM,MESH%IKLE%I,
     &              MESH%IKLE%MAXDIM1,
     &              NPOIN2,MESH%NACHB%I,MESH%NBOR%I,NPTFR2,IT1%I,IT2%I)
!
      ELSEIF(IELM.NE.31) THEN
        WRITE(LU,*) 'UNEXPECTED ELEMENT IN INBIEF:',IELM
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
!
!       CASES WITH A FIRST CALL IN 2D
!
        MXPTVS = MESH%MXPTVS
!       HERE IFABOR FOR IELM=51 MUST STILL BE 2D
!       SO VOISIN31 CALLED LATER
!
!       NOTE: IN 3D IKLBOR BUILT HERE WITH NELEB 2D AND NELEBX 3D.
!
        CALL ELEBD(MESH%NELBOR%I,MESH%NULONE%I,MESH%KP1BOR%I,
     &             MESH%IFABOR%I,MESH%NBOR%I,MESH%IKLE%I,
     &             MESH%IKLE%MAXDIM1,
     &             MESH%IKLBOR%I,NELEM2,NELMAX2,NPOIN2,NPTFR2,IELM,
     &             LIHBOR,KLOG,MESH%ISEG%I,
     &             IT1%I,IT2%I,IT3%I,MESH%NELEBX,MESH%NELEB)
!
      ENDIF
!
!     3D CASES
!
      IF(IELM.EQ.31) THEN
!
!       BUILDING ARRAYS FOR TETRAHEDRONS
!
        CALL VOISIN31(MESH%IFABOR%I,NELEM,NELMAX,IELM,MESH%IKLE%I,
     &                MESH%IKLE%MAXDIM1,NPOIN,MESH%NBOR%I,NPTFR,
     &                LIHBOR,KLOG,MESH%INDPU%I,IKLESTR,NELEB2)
!
        CALL ELEBD31(MESH%NELBOR%I,MESH%NULONE%I,MESH%IKLBOR%I,
     &               MESH%IFABOR%I,MESH%NBOR%I,MESH%IKLE%I,
     &               NELEM,NELEB,NELMAX,NPOIN,NPTFR,IELM)
!
      ELSEIF(IELM.EQ.41) THEN
!
!       COMPLETES ARRAYS FOR PRISMS
!
        CALL ELEB3D(MESH%IKLE%I,MESH%NBOR%I,
     &              MESH%NELBOR%I,MESH%IKLBOR%I,
     &              MESH%NELEB,MESH%NELEBX,
     &              MESH%NULONE%I,NELMAX2,NPOIN2,NPLAN,NPLAN-1,NPTFR2)
!
      ELSEIF(IELM.EQ.51) THEN
!
!       COMPLETES ARRAYS FOR PRISMS SPLIT INTO TETRAHEDRONS
!
        IF(PRESENT(MESH2D)) THEN
!         NOTE THE USE OF MESH2D FOR NELBOR AND NULONE
!         THIS IS FOR CALLING STOSEG
          CALL ELEB3DT(MESH%IKLE%I,MESH%NBOR%I,MESH%NELBOR%I,
     &                 MESH2D%NELBOR%I,MESH%IKLBOR%I,
     &                 MESH%NELEB,MESH%NELEBX,MESH%NULONE%I,NELMAX2,
     &                 NPOIN2,NPLAN,NPLAN-1,NPTFR2,
     &                 MESH2D%IKLBOR%I,MESH2D%NELEB,MESH2D%NELEBX)
        ELSE
          WRITE(LU,*) 'ARGUMENT MESH2D SHOULD BE ADDED TO INBIEF'
          WRITE(LU,*) 'FOR A CALL WITH IELM=51'
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSEIF(IELM.NE.11) THEN
!
        WRITE(LU,*) 'INBIEF UNEXPECTED ELEMENT: ',IELM
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! LOOKS FOR VECTORISATION POSSIBILITIES
!
      IF(IELM.EQ.11) THEN
!
      IF(LVMAC.NE.1) THEN
        WRITE(LU,201) LVMAC
201     FORMAT(1X,'INBIEF (BIEF): VECTOR MACHINE',/,1X,
     &  'WITH VECTOR LENGTH :',1I6,
     &  ' (ACCORDING TO YOUR DATA OR IN THE DICTIONNARY OF KEY-WORDS)')
        CALL VECLEN(LV,NDP,MESH%IKLE%I,NELEM,NELMAX,NPOIN,T1%R)
        IF(LV.LT.LVMAC) THEN
          WRITE(LU,301) LV
301       FORMAT(1X,'THIS LENGTH IS REDUCED TO ',1I4,' BY THE NUMBERING
     &OF THE ELEMENTS (SEE STBTEL DOCUMENTATION)')
        ENDIF
      ELSE
        LV = 1
        WRITE(LU,401)
401     FORMAT(1X,'INBIEF (BIEF): NOT A VECTOR MACHINE',
     &                                      ' (ACCORDING TO YOUR DATA)')
      ENDIF
!
      MESH%LV = LV
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     MERCATOR PROJECTION (TRIANGLES AND PRISMS ONLY)
!
      IF(SPHERI.AND.IELM.NE.11.AND.IELM.NE.41) THEN
        WRITE(LU,399)
399     FORMAT(1X,'INBIEF (BIEF) : ELEMENT NOT IMPLEMENTED WITH',/,1X,
     &            'MERCATOR PROJECTION:',1I3)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(SPHERI) THEN
!
        CALL CPSTVC(MESH%X,T1)
        CALL CPSTVC(MESH%Y,T2)
        CALL LATITU(T2%R,MESH%COSLAT%R,MESH%SINLAT%R,
     &              LAMBD0,MESH%Y%R,NPOIN2)
        CALL CORLAT
!
        IF(IELM.EQ.11.OR.IELM.EQ.41) THEN
          DO I=1,NPOIN2
            T1%R(I)=MESH%X%R(I)
!            T2%R(I)=MESH%Y%R(I)*MESH%COSLAT%R(I)
          ENDDO
        ENDIF
!       COMPLETING UPPER LAYERS FOR 3D MESHES
        IF(IELM.EQ.41) THEN
          DO IPLAN=2,NPLAN
            DO I=1,NPOIN2
              I3D=(IPLAN-1)*NPOIN2+I
              T1%R(I3D)=MESH%X%R(I3D)
              T2%R(I3D)=T2%R(I)
            ENDDO
          ENDDO
        ENDIF
!
!       CONVERTS TO COORDINATES BY ELEMENTS (STARTING WITH X AND Y)
        CALL PTTOEL(MESH%XEL,T1,MESH)
        CALL PTTOEL(MESH%YEL,T2,MESH)
!
      ELSE
!
!       NOTE: IN 3D MESH%X AND MESH%Y FULLY BUILT IN ALMESH
!
!       CONVERTS TO COORDINATES BY ELEMENTS (STARTING WITH X AND Y)
!
        CALL PTTOEL(MESH%XEL,MESH%X,MESH)
        CALL PTTOEL(MESH%YEL,MESH%Y,MESH)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CONVERTS TO A LOCAL SYSTEM IN X AND Y, WITH POINT 1 AT ORIGIN
!
      DO IDP=2,NDP
        CALL OV_2('X=X-Y   ',MESH%XEL%R,IDP,
     &                       MESH%XEL%R,1  ,
     &                       MESH%XEL%R,1  , 0.D0 , NELMAX , NELEM )
        CALL OV_2('X=X-Y   ',MESH%YEL%R,IDP,
     &                       MESH%YEL%R,1  ,
     &                       MESH%YEL%R,1  , 0.D0 , NELMAX , NELEM )
      ENDDO
!
      CALL OV('X=C     ', X=MESH%XEL%R, C=0.D0, DIM1=NELEM)
      CALL OV('X=C     ', X=MESH%YEL%R, C=0.D0, DIM1=NELEM)
!
!
      IF(SPHERI) THEN
!
        IF(PRESENT(MESH2D)) THEN
          CALL LONGITU(MESH2D%XEL%R,MESH%COSLAT%R,MESH2D%IKLE%I,
     &                 NELMAX2,NELEM2)
          NELCOU = (NPLAN-1)*NELMAX2
          DO IPLAN=1,NPLAN-1
            DO IELEM=1,NELEM2
              IELEM3D=(IPLAN-1)*NELMAX2+IELEM
!             3 POINTS OF THE BOTTOM
              MESH%XEL%R(IELEM3D)         =MESH2D%XEL%R(IELEM)
              MESH%XEL%R(IELEM3D+NELCOU  )=MESH2D%XEL%R(IELEM+NELMAX2)
              MESH%XEL%R(IELEM3D+2*NELCOU)=MESH2D%XEL%R(IELEM+2*NELMAX2)
!             3 POINTS OF THE TOP
              MESH%XEL%R(IELEM3D+3*NELCOU)=MESH%XEL%R(IELEM3D)
              MESH%XEL%R(IELEM3D+4*NELCOU)=MESH%XEL%R(IELEM3D+NELCOU)
              MESH%XEL%R(IELEM3D+5*NELCOU)=MESH%XEL%R(IELEM3D+2*NELCOU)
            ENDDO
          ENDDO
        ELSE
          CALL LONGITU(MESH%XEL%R,MESH%COSLAT%R,MESH%IKLE%I,
     &                 NELMAX,NELEM)
        ENDIF
!
      ENDIF
!
!     IF DONE FOR Z (BUT IN MOVING MESHES SHOULD NOT BE USED !!!!)
!
!     IF(MESH%DIM1.EQ.3) THEN
!       CALL PTTOEL(MESH%ZEL,MESH%Z,MESH)
!       DO IDP=2,NDP
!         CALL OV_2('X=X-Y   ',MESH%ZEL%R,IDP,
!    &                         MESH%ZEL%R,1  ,
!    &                         MESH%ZEL%R,1  , 0.D0 , NELMAX , NELEM )
!       ENDDO
!       CALL OV('X=C     ', X=MESH%ZEL%R, C=0.D0, NELEM )
!     ENDIF
!
!-----------------------------------------------------------------------
!
! COMPUTES THE GEOMETRICAL COEFFICIENTS FOR EACH ELEMENT
!
      IF(IELM.EQ.11) THEN
!
        CALL GEOELT(MESH%SURDET%R,MESH%SURFAC%R,
     &              MESH%XEL%R   ,MESH%YEL%R   ,NELEM,NELMAX,IELM)
!
! FOR THE TIME BEING, SURDET IS ONLY USED BY CARACT, WHICH DOES NOT
! WORK ON THE MESH IN SPHERICAL COORDINATES.
! ERASES SURDET COMPUTED BY GEOELE FROM XEL AND YEL
!
        IF(SPHERI) THEN
!
          DO IELEM = 1 , NELEM
            I1 = MESH%IKLE%I(IELEM)
            I2 = MESH%IKLE%I(IELEM+NELMAX)
            I3 = MESH%IKLE%I(IELEM+2*NELMAX)
            X2 = - MESH%X%R(I1) + MESH%X%R(I2)
            X3 = - MESH%X%R(I1) + MESH%X%R(I3)
            Y2 = - MESH%Y%R(I1) + MESH%Y%R(I2)
            Y3 = - MESH%Y%R(I1) + MESH%Y%R(I3)
            MESH%SURDET%R(IELEM) = 1.D0 / (X2*Y3 - X3*Y2)
          ENDDO
!
        ENDIF
!
      ELSEIF(IELM.EQ.41.OR.IELM.EQ.51.OR.IELM.EQ.31) THEN
!
!       FOR PRISMS, SURFAC IS THE SURFACE OF THE TRIANGLES
!       FOR ELEMENTS 51 AND 31 ???????? SHOULD NOT BE USED...
!
        DO IELEM = 1 , NELEM
          X2 = MESH%XEL%R(IELEM+NELMAX)
          X3 = MESH%XEL%R(IELEM+2*NELMAX)
          Y2 = MESH%YEL%R(IELEM+NELMAX)
          Y3 = MESH%YEL%R(IELEM+2*NELMAX)
          MESH%SURFAC%R(IELEM) = 0.5D0 * (X2*Y3 - X3*Y2)
        ENDDO
!
      ELSE
        WRITE(LU,*) 'UNEXPECTED ELEMENT IN INBIEF:',IELM
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
! DEFINES THE OUTGOING NORMALS AT THE BOUNDARIES
!         AND THE DISTANCES TO THE BOUNDARY
!
      IF(IELM.EQ.11) THEN
!
      CALL NORMAB(MESH%XNEBOR%R,MESH%YNEBOR%R,
     &            MESH%XSGBOR%R,MESH%YSGBOR%R,
     &            MESH%DISBOR%R,MESH%SURFAC%R,NELMAX,MESH%NELBOR%I,
     &            MESH%NULONE%I,MESH%LGSEG%R,NPTFR,MESH,
     &            MESH%XEL%R,MESH%YEL%R,MESH%IKLBOR%I,
     &            MESH%NELEBX,MESH%NELEB)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  DATA STRUCTURE FOR EDGE-BASED STORAGE (FROM 5.9 ON ALWAYS DONE IN 2D)
!                                        (FROM 6.2 ON ALWAYS DONE IN 3D)
!  SEE CALL TO COMP_SEG BELOW TO COMPLETE THE STRUCTURE
!
      IF(IELM.EQ.11) THEN
!
      CALL STOSEG(MESH%IFABOR%I,NELEM,NELMAX,NELMAX,IELMX,MESH%IKLE%I,
     &            MESH%NBOR%I,NPTFR,MESH%GLOSEG%I,MESH%GLOSEG%MAXDIM1,
     &            MESH%ELTSEG%I,MESH%ORISEG%I,MESH%NSEG,
     &            MESH%NELBOR%I,MESH%NULONE%I,
     &            MESH%KNOLG%I,MESH%IKLBOR%I,MESH%NELEBX,MESH%NELEB)
!
      ELSEIF(IELM.EQ.41) THEN
!
      CALL STOSEG41(MESH%IFABOR%I,NELMAX,IELMX,MESH%IKLE%I,MESH%NBOR%I,
     &              MESH%GLOSEG%I,MESH%GLOSEG%MAXDIM1,
     &              MESH%ELTSEG%I,MESH%ORISEG%I,
     &              MESH%NELBOR%I,MESH%NULONE%I,
     &              NELMAX2,NELEM2,NPTFR2,NPOIN2,NPLAN,MESH%KNOLG%I,
     &              BIEF_NBSEG(11,MESH),
     &              MESH%IKLBOR%I,MESH%NELEBX,MESH%NELEB)
!
      ELSEIF(IELM.EQ.51) THEN
!
      IF(PRESENT(MESH2D)) THEN
!       NOTE THE USE OF MESH2D FOR NELBOR AND NULONE
!       THIS IS FOR CALLING STOSEG
        CALL STOSEG51(MESH%IFABOR%I,NELMAX,IELMX,
     &                MESH%IKLE%I,MESH%NBOR%I,
     &                MESH%GLOSEG%I,MESH%GLOSEG%MAXDIM1,
     &                MESH%ELTSEG%I,MESH%ORISEG%I,
     &                MESH2D%NELBOR%I,MESH2D%NULONE%I,
     &                NELMAX2,NELEM2,NPTFR2,NPOIN2,NPLAN,MESH%KNOLG%I,
     &                MESH2D%NSEG,MESH2D%IKLBOR%I,MESH2D%NELEB,
     &                MESH2D%NELEBX)
      ELSE
        WRITE(LU,*) 'ARGUMENT MESH2D SHOULD BE ADDED TO INBIEF'
        WRITE(LU,*) 'FOR A CALL WITH IELM=51'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      ELSE
!
        WRITE(LU,*) 'ELEMENT ',IELM,' NOT IMPLEMENTED FOR SEGMENTS'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!     NOW THE 3D VALUE OF IFABOR IS BUILT FOR PRISMS CUT INTO
!     TETRAHEDRA (UP TO STOSEG51 A 2D VALUE WAS USED)
!
      IF(IELM.EQ.51) THEN
        CALL VOISIN31(MESH%IFABOR%I,NELEM,NELMAX,IELM,MESH%IKLE%I,
     &                MESH%IKLE%MAXDIM1,NPOIN,MESH%NBOR%I,NPTFR,
     &                LIHBOR,KLOG,MESH%INDPU%I,IKLESTR,1)
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1.AND.IELM.EQ.11) THEN
!
!       COMPLETES NH_COM_SEG WITH SEGMENT NUMBERS ONCE ELTSEG IS KNOWN
!
        CALL COMP_NH_COM_SEG(MESH%ELTSEG%I,MESH%ELTSEG%DIM1,
     &                       MESH%NH_COM_SEG%I,
     &                       MESH%NH_COM_SEG%DIM1,MESH%NB_NEIGHB_SEG,
     &                       MESH%NB_NEIGHB_PT_SEG%I,
     &                       MESH%GLOSEG%I,MESH%GLOSEG%DIM1,
     &                       MESH%KNOLG%I,NPOIN)
!
!       COMPLETES FAC ONCE IFABOR AND ELTSEG ARE KNOWN
!
        IF(IELM.EQ.11.AND.IELMX.EQ.13) THEN
          CALL COMP_FAC(MESH%ELTSEG%I,MESH%ORISEG%I,MESH%IFABOR%I,NELEM,
     &                  NPOIN,MESH%IFAC)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  DATA STRUCTURE FOR EDGE-BASED STORAGE
!
      IF(IELM.EQ.11.AND.PRODUC.EQ.2) THEN
!
        CALL FROPRO(MESH%NBOR%I,MESH%IKLE%I,
     &              NELEM,NELMAX,NPOIN,MESH%NPMAX,NPTFR,IELM,
     &              MESH%IKLEM1%I,MESH%LIMVOI%I,OPTASS,PRODUC,MXPTVS,
     &              IT1%I,MESH%GLOSEG%I,MESH%GLOSEG%DIM1,MESH%NSEG)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  COMPLEMENTS IKLE AND NBOR BEYOND LINEAR ELEMENTS
!
      IF(IELM.EQ.11.AND.IELM.NE.IELMX) THEN
        IF(MESH%IKLE%DIM2.NE.BIEF_NBPEL(IELMX,MESH)) THEN
          WRITE(LU,101) IELMX
101       FORMAT(1X,'INBIEF (BIEF): WRONG DIMENSION OF IKLE',/,1X,
     &              'FOR AN ELEMENT WITH TYPE :',1I6)
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL COMP_IKLE(MESH%IKLE%I,MESH%IKLBOR%I,
     &                 MESH%ELTSEG%I,MESH%NBOR%I,MESH%NELBOR%I,
     &                 MESH%NULONE%I,IELMX,NELEM,NELMAX,NPOIN,NPTFR,
     &                 MESH%NELEB,MESH%NELEBX)
      ENDIF
!
!-----------------------------------------------------------------------
!
! COMPLEMENTS THE SEGMENT STRUCTURE BEYOND THE LINEAR ELEMENTS
!
      IF(IELM.NE.IELMX) THEN
        CALL COMP_SEG(NELEM,NELMAX,IELMX,MESH%IKLE%I,MESH%GLOSEG%I,
     &                MESH%GLOSEG%MAXDIM1,MESH%ELTSEG%I,MESH%ORISEG%I,
     &                MESH%NSEG)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     COMPLEMENTS THE DATA STRUCTURE FOR FINITE VOLUMES
!
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
!
!       COMPUTE THE COORDINATES OF CENTRE OF GRAVITY FOR ELEMENTS RIGHT AND
!       LEFT OF EDGES
!
        CALL CENTRE_MASS_SEG(MESH%X%R,MESH%Y%R,MESH%COORDG%R,
     &                       MESH%IKLE%I,NPOIN,MESH%ELTSEG%I,
     &                       MESH%ORISEG%I,NELEM,MESH%NSEG,
     &                       MESH%JMI%I,MESH%CMI%R,MESH%GLOSEG%I,
     &                       MESH%IFABOR%I,MESH%COORDS%R,MESH)
!
!       COMPUTES  CMI, AIRST AND NORMALS (VNOIN)
!
        CALL INFCEL(MESH%X%R,MESH%Y%R,
     &              MESH%NUBO%I,MESH%VNOIN%R,NPOIN,
     &              NELEM,MESH%NSEG,MESH%CMI%R,
     &              MESH%AIRST%R,MESH%GLOSEG%I,
     &              MESH%COORDG%R,MESH%ELTSEG%I,
     &              MESH%ORISEG%I,MESH%IFABOR%I)
!
!       COMPUTES THE SURFACE OF THE CELLS
!
        CALL VECTOR(T1,'=','MASBAS          ',11,
     &              1.D0,T2,T2,T2,T2,T2,T2,MESH,.FALSE.,T2)
        IF(NCSIZE.GT.1) CALL PARCOM(T1,2,MESH)
!
!       COMPUTES THE LOCAL SPACE STEP PER CELL
!
        CALL HLOC(NPOIN,MESH%NSEG,NELEM,MESH%NUBO%I,MESH%VNOIN%R,T1%R,
     &            MESH%DTHAUT%R,MESH,MESH%ELTSEG%I,MESH%IFABOR%I)
!
!       COMPUTES THE GRADIENTS OF THE BASE FUNCTIONS
!
        CALL GRADP(NPOIN,MESH%NELMAX,MESH%IKLE%I,MESH%SURFAC%R,
     &             MESH%X%R,MESH%Y%R,MESH%DPX%R,MESH%DPY%R)
!
!       INITIALISE COORDINATES OF POINTS TO USE FOR FIELD  
!       RECONSTRUCTION IN FINITE VOLUME DIFFUSION SOLVER
        CALL OV('X=C     ', X=MESH%COORDR%R, C=0.D0, DIM1=4*MESH%NSEG)

      ENDIF
!
!-----------------------------------------------------------------------
!
! COMPUTES THE STARTING ELEMENT FOR THE METHOD OF CHARACTERISTICS
!
      CALL MAKE_ELTCAR(MESH%ELTCAR%I,MESH%IFAC%I,
     &                 MESH%IKLE%I,NPOIN2,NELEM2,
     &                 NELMAX,MESH%KNOLG%I,IT1%I,MESH,NPLAN,IELMX)
!
!-----------------------------------------------------------------------
!
!     CHECKING THE MESH
!
      IF(CHECK_MESH) THEN
        CALL CHECKMESH(MESH,NPOIN2)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
