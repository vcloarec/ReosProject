!                   *************
                    SUBROUTINE OM
!                   *************
!
     &( OP , M , N , D , C , MESH )
!
!***********************************************************************
! BIEF   V7P0                                     21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON MATRICES.
!code
!+   D: DIAGONAL MATRIX
!+   C: CONSTANT
!+
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON MATRICES M AND N, D AND C.
!+
!+   THE RESULT IS MATRIX M.
!+
!+      OP = 'M=N     '  : COPIES N IN M
!+      OP = 'M=CN    '  : MULTIPLIES N BY C
!+      OP = 'M=CM    '  : MULTIPLIES M BY C
!+      OP = 'M=M+CN  '  : ADDS CN TO M
!+      OP = 'M=MD    '  : M X D
!+      OP = 'M=DM    '  : D X M
!+      OP = 'M=DMD   '  : D X M X D
!+      OP = 'M=0     '  : SETS M TO 0 (TO BE CHECKED)
!+      OP = 'M=X(M)  '  : NOT SYMMETRICAL FORM OF M
!+      OP = 'M=TN    '  : COPIES TRANSPOSE OF N IN M
!+      OP = 'M=MSK(M)'  : MASKS M EXTRADIAGONAL TERMS
!
!warning  BE CAREFUL IF A NEW OPERATION IS ADDED TO THE LIST
!warning  IF OP CONTAINS N, IT THEN MEANS THAT MATRIX N IS USED
!
!history  ALGIANE FROEHLY
!+        13/02/2008
!+
!+   ADDED OM1113 AND OM1311
!
!history  J-M HERVOUET (LNHE)     ; F  LEPEINTRE (LNH)
!+        05/02/2010
!+        V6P0
!+   CALL TO OMSEGBOR MODIFIED, OMSEGPAR SUPPRESSED
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
!history  F. DECUNG (LNHE)
!+        01/01/2013
!+        V6P3
!+   omborseg added : operations on matrices with an edge-based storage
!         where N is a boundary matrix
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/03/2014
!+        V7P0
!+   Now written to enable different numbering of boundary points and
!+   boundary segments.
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+      ADD MODASS=3
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| A GIVEN CONSTANT USED IN OPERATION OP
!| D              |-->| A DIAGONAL MATRIX
!| M              |<->| RESULTING MATRIX
!| MESH           |-->| MESH STRUCTURE
!| N              |-->| MATRIX USED IN FORMULA OP
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_OM => OM
      USE DECLARATIONS_TELEMAC, ONLY : MODASS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      TYPE(BIEF_OBJ)  , INTENT(INOUT), TARGET, OPTIONAL :: M
      TYPE(BIEF_OBJ)  , INTENT(IN), TARGET, OPTIONAL    :: N
      TYPE(BIEF_OBJ)  , INTENT(IN), TARGET, OPTIONAL    :: D
      DOUBLE PRECISION, INTENT(IN), OPTIONAL            :: C
      TYPE(BIEF_MESH) , INTENT(IN), OPTIONAL            :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELM1,IELM2,IELN1,IELN2,NELEM,NELMAX
      INTEGER NDIAGM,NDIAGN,NDIAGX,MSEG1,MSEG2,NSEG1,NSEG2
      INTEGER STOM,STON,NPTFR,MDIAGX
      INTEGER SIZXN,SZMXN,NETAGE
!
      LOGICAL YAN,YAD,YAC
      TYPE(BIEF_OBJ), POINTER :: NN
      TYPE(BIEF_OBJ), POINTER :: DD
      DOUBLE PRECISION CC
!
      CHARACTER(LEN=1) TYPDIM,TYPEXM,TYPDIN,TYPEXN
!
      INTEGER, DIMENSION(:), POINTER :: IKLE
!
      YAN=.FALSE.
      YAD=.FALSE.
      YAC=.FALSE.
      IF(INCLUS(OP,'N')) YAN=.TRUE.
      IF(INCLUS(OP,'D').OR.INCLUS(OP,'MSK')) YAD=.TRUE.
      IF(INCLUS(OP,'C')) YAC=.TRUE.
!
      IF(YAN) THEN
        IF(PRESENT(N)) THEN
          NN=>N
        ELSE
          WRITE(LU,*) "---------------------------------"
          WRITE(LU,2) OP
2         FORMAT(1X,'OM (BIEF) : N MISSING AND OPERATION ',A8,' ASKED')
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        NN=>M
      ENDIF
!
      IF(YAD) THEN
        IF(PRESENT(D)) THEN
          DD=>D
        ELSE
          WRITE(LU,*) "---------------------------------"
          WRITE(LU,4) OP
4         FORMAT(1X,'OM (BIEF) : D MISSING AND OPERATION ',A8,' ASKED')
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        DD=>M%D
      ENDIF
!
      IF(PRESENT(C)) THEN
        CC=C
      ELSE
        IF(YAC) THEN
          WRITE(LU,*) "---------------------------------"
          WRITE(LU,6) OP
6         FORMAT(1X,'OM (BIEF) : C MISSING AND OPERATION ',A8,' ASKED')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!  CASE WHERE THE STRUCTURE OF M BECOMES THAT OF N
!
      IF(OP(3:8).EQ.'N     '.OR.OP(3:8).EQ.'CN    ') THEN
        CALL CPSTMT(N,M)
      ELSEIF(OP(3:8).EQ.'TN    ') THEN
        CALL CPSTMT(N,M,TRANS=.TRUE.)
      ENDIF
!
!  EXTRACTS THE CHARACTERISTICS OF MATRIX M
!
      TYPDIM = M%TYPDIA
      TYPEXM = M%TYPEXT
      STOM = M%STO
      NDIAGM = M%D%DIM1
      MDIAGX = M%D%MAXDIM1
      IELM1 = M%ELMLIN
      IELM2 = M%ELMCOL
!
      IF(OP(3:8).EQ.'X(M)  ') THEN
        IF(M%ELMLIN.NE.M%ELMCOL) THEN
          WRITE(LU,901) M%NAME
901       FORMAT(1X,'OM (BIEF) : M (REAL NAME: ',A6,') NOT SQUARE')
          WRITE(LU,701)
701       FORMAT(1X,'            IS ALREADY NON SYMMETRICAL')
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(M%X%MAXDIM2*M%X%MAXDIM1.LT.
     &      BIEF_DIM1_EXT(IELM1,IELM2,STOM,'Q',MESH)*
     &      BIEF_DIM2_EXT(IELM1,IELM2,STOM,'Q',MESH)    ) THEN
            WRITE(LU,401) M%NAME
            WRITE(LU,801)
801         FORMAT(1X,'            TO BECOME NON SYMMETRICAL')
            CALL PLANTE(1)
            STOP
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EXTRACTS THE CHARACTERISTICS OF MATRIX N (OPTIONAL)
!
      IF(INCLUS(OP,'N')) THEN
        TYPDIN = NN%TYPDIA
        TYPEXN = NN%TYPEXT
        STON   = NN%STO
        NDIAGN = NN%D%DIM1
        NDIAGX = NN%D%MAXDIM1
!
        SIZXN = NN%X%DIM1
        SZMXN = NN%X%MAXDIM1
!
! 07/02/03 : DIVISION BY NDIAGN=0 AVOIDED (SUBDOMAIN WITHOUT BOUNDARY POINTS
!            IN PARALLEL). COURTESY OLIVER GOETHEL (HANNOVER UNIVERSITY)
!
        IF(NDIAGN.GT.0) THEN
          NETAGE = NDIAGM/NDIAGN - 1
        ELSE
          NETAGE = 0
        ENDIF
!
        IELN1 = NN%ELMLIN
        IELN2 = NN%ELMCOL
        IF(NDIAGN.GT.MDIAGX) THEN
          WRITE(LU,401) M%NAME
401       FORMAT(1X,'OM (BIEF) : M (REAL NAME: ',A6,') TOO SMALL')
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        IELN1 = IELM1
        IELN2 = IELM2
        STON = STOM
      ENDIF
!
!-----------------------------------------------------------------------
!
!  DEPLOYMENT OF THE MESH STRUCTURE
!
!     STANDARD MATRIX
      IF(DIMENS(IELM1).EQ.MESH%DIM1) THEN
        IKLE=>MESH%IKLE%I
        NELEM = MESH%NELEM
        NELMAX= MESH%NELMAX
      ELSE
!     BOUNDARY MATRIX
        IKLE=>MESH%IKLBOR%I
        NELEM  = MESH%NELEB
        NELMAX = MESH%NELEBX
      ENDIF
!
      NPTFR= MESH%NPTFR
!
!-----------------------------------------------------------------------
!
!  TRADITIONAL EBE STORAGE:
!
      IF(STOM.EQ.1.AND.STON.EQ.1) THEN
!
      IF(IELM1.EQ.1.AND.IELM2.EQ.1) THEN
!
!     ELEMENTS WITH 2 POINTS
!
      CALL OM0101(OP,M%D%R,TYPDIM,M%X%R,TYPEXM,
     &            NN%D%R,TYPDIN,NN%X%R,TYPEXN,DD%R,CC,
     &            IKLE,NELEM,NELMAX,NDIAGM)
!
!
!     ELEMENTS WITH 3 POINTS
!
      ELSEIF( (IELM1.EQ.2 .AND.IELM2.EQ.2 ) .OR.
     &        (IELM1.EQ.11.AND.IELM2.EQ.11) .OR.
     &        (IELM1.EQ.61.AND.IELM2.EQ.61) .OR.
     &        (IELM1.EQ.81.AND.IELM2.EQ.81)       ) THEN
!
        IF( (IELN1.EQ.2 .AND.IELN2.EQ.2 ) .OR.
     &      (IELN1.EQ.11.AND.IELN2.EQ.11) .OR.
     &      (IELN1.EQ.61.AND.IELN2.EQ.61) .OR.
     &      (IELN1.EQ.81.AND.IELN2.EQ.81)         ) THEN
!
          IF (MODASS .EQ. 3) THEN
            CALL OM1111(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                  NN%D%R,TYPDIN,NN%X%R,TYPEXN,DD%R,CC,
     &                  IKLE,NELEM,NELMAX,NDIAGM,
     &                  DM_ERR=M%D%E, DN_ERR=NN%D%E,
     &                  D_ERR=D%E)
          ELSE
            CALL OM1111(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                  NN%D%R,TYPDIN,NN%X%R,TYPEXN,DD%R,CC,
     &                  IKLE,NELEM,NELMAX,NDIAGM)
          ENDIF
!
        ELSEIF(IELN1.EQ.1.AND.IELN2.EQ.1) THEN
!
          CALL OM1101(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                     NN%D%R,TYPDIN,NN%X%R,TYPEXN,CC,
     &                     MESH%NULONE%I,MESH%NELBOR%I,
     &                     MESH%NBOR%I,
     &                     NELMAX,NDIAGM,NDIAGN,MESH%NELEBX,
     &                     MESH%NELEB)
!
        ELSE
          WRITE(LU,101) M%NAME
          WRITE(LU,201) IELM1,IELM2
          WRITE(LU,151) NN%NAME
          WRITE(LU,251) IELN1,IELN2
          WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!     ELEMENTS WITH 4 POINTS
!
      ELSEIF( (IELM1.EQ.21.AND.IELM2.EQ.21) .OR.
     &        (IELM1.EQ.71.AND.IELM2.EQ.71) .OR.
     &        (IELM1.EQ.31.AND.IELM2.EQ.31) .OR.
     &        (IELM1.EQ.51.AND.IELM2.EQ.51) .OR.
     &        (IELM1.EQ.12.AND.IELM2.EQ.12)      ) THEN
!
        IF(   (IELN1.EQ.21.AND.IELN2.EQ.21) .OR.
     &        (IELN1.EQ.71.AND.IELN2.EQ.71) .OR.
     &        (IELN1.EQ.31.AND.IELN2.EQ.31) .OR.
     &        (IELN1.EQ.51.AND.IELN2.EQ.51) .OR.
     &        (IELN1.EQ.12.AND.IELN2.EQ.12)      ) THEN
!
          CALL OM2121(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                NN%D%R,TYPDIN,NN%X%R,TYPEXN,DD%R,CC,
     &                IKLE,NELEM,NELMAX,NDIAGM)
        ELSEIF(  (IELM1.EQ.12.AND.IELM2.EQ.12) .AND.
     &           (IELN1.EQ.1 .AND.IELN2.EQ.1 )   ) THEN
!
          CALL OM1201(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                     NN%D%R,TYPDIN,NN%X%R,TYPEXN,CC,
     &                     MESH%NULONE%I,MESH%NELBOR%I,
     &                     MESH%NBOR%I,
     &                     NELMAX,NDIAGM,NDIAGN,MESH%NELEBX,
     &                     MESH%NELEB)
        ELSEIF(( (IELM1.EQ.51.AND.IELM2.EQ.51) .AND.
     &           (IELN1.EQ.61.AND.IELN2.EQ.61))   ) THEN
!         PRISMS SPLIT IN TETRAHEDRONS M INTERIOR MATRIX
!                                      N LATERAL BOUNDARY MATRIX
          CALL OM5161(OP,M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                NN%D%R,TYPDIN,NN%X%R,TYPEXN,CC,
     &                MESH%NULONE%I,MESH%NELBOR%I,MESH%NBOR%I,
     &                NELMAX,NDIAGN,SIZXN,SZMXN)
!
        ELSEIF(( (IELM1.EQ.31.AND.IELM2.EQ.31) .AND.
     &           (IELN1.EQ.81.AND.IELN2.EQ.81))   ) THEN
!         NOT STRUCTURED TETRAHEDRONS    M INTERIOR MATRIX
!                                        N LATERAL BOUNDARY MATRIX
          CALL OM3181(OP,M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                NN%D%R,TYPDIN,NN%X%R,TYPEXN,CC,
     &                MESH%NULONE%I,MESH%NELBOR%I,MESH%NBOR%I,
     &                NELMAX,NDIAGN,MESH%NELEB)
!
        ELSEIF(  (IELM1.EQ.51.AND.IELM2.EQ.51) .AND.
     &           (IELN1.EQ.11.AND.IELN2.EQ.11)   ) THEN
!         PRISMS SPLIT IN TETRAHEDRONS M INTERIOR MATRIX
!                                      N BOTTOM (NB) OR SURFACE (NS) BOUNDARY MATRIX
!                                      OPERATIONS M+NB AND M+NS
          CALL OM5111(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                NN%D%R,TYPDIN,NN%X%R,TYPEXN,
     &                NDIAGN,NDIAGX,SIZXN,NETAGE,NELMAX)
!
        ELSE
          WRITE(LU,101) M%NAME
          WRITE(LU,201) IELM1,IELM2
          WRITE(LU,151) NN%NAME
          WRITE(LU,251) IELN1,IELN2
          WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!     RECTANGULAR MATRICES WITH 3 AND 4 POINTS
!
      ELSEIF(IELM1.EQ.11.AND.IELM2.EQ.12) THEN
!
        IF((IELN1.EQ.11.AND.IELN2.EQ.12).OR.
     &     (IELN1.EQ.12.AND.IELN2.EQ.11.AND.OP(1:4).EQ.'M=TN')) THEN
          CALL OM1112(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                     NN%D%R,TYPDIN,NN%X%R,TYPEXN,DD%R,CC,
     &                     IKLE,NELEM,NELMAX,NDIAGM)
        ELSE
          WRITE(LU,101) M%NAME
          WRITE(LU,201) IELM1,IELM2
          WRITE(LU,151) NN%NAME
          WRITE(LU,251) IELN1,IELN2
          WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!     RECTANGULAR MATRICES WITH 4 AND 3 POINTS
!
      ELSEIF(IELM1.EQ.12.AND.IELM2.EQ.11) THEN
!
        IF((IELN1.EQ.12.AND.IELN2.EQ.11).OR.
     &     (IELN1.EQ.11.AND.IELN2.EQ.12.AND.OP(1:4).EQ.'M=TN')) THEN
          CALL OM1211(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                NN%D%R,TYPDIN,NN%X%R,TYPEXN,DD%R,CC,
     &                IKLE,NELEM,NELMAX,NDIAGM)
        ELSE
          WRITE(LU,101) M%NAME
          WRITE(LU,201) IELM1,IELM2
          WRITE(LU,151) NN%NAME
          WRITE(LU,251) IELN1,IELN2
          WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!     ELEMENTS WITH 6 POINTS
!
      ELSEIF( (IELM1.EQ.41.AND.IELM2.EQ.41).OR.
     &        (IELM1.EQ.13.AND.IELM2.EQ.13)      ) THEN
!
        IF( (IELN1.EQ.41.AND.IELN2.EQ.41).OR.
     &      (IELN1.EQ.13.AND.IELN2.EQ.13)        ) THEN
!
          CALL OM4141(OP,M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                NN%D%R,TYPDIN,NN%X%R,TYPEXN,DD%R,CC,
     &                IKLE,NELEM,NELMAX,NDIAGM,M%STOX)
!
        ELSEIF(IELN1.EQ.71.AND.IELN2.EQ.71) THEN
!
          CALL OM4121(OP,M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                   NN%D%R,TYPDIN,NN%X%R,TYPEXN,CC,
     &                   MESH%NULONE%I,MESH%NELBOR%I,MESH%NBOR%I,
     &                   NELMAX,NDIAGN,SIZXN,SZMXN)
!
        ELSEIF(IELN1.EQ.11.AND.IELN2.EQ.11) THEN
!
          CALL OM4111(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                     NN%D%R,TYPDIN,NN%X%R,TYPEXN,
     &                     NDIAGN,NDIAGX,SIZXN,NETAGE,NELMAX)
!
        ELSEIF(  (IELM1.EQ.13.AND.IELM2.EQ.13) .AND.
     &           (IELN1.EQ.2 .AND.IELN2.EQ.2 )   ) THEN
!
          CALL OM1302(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                     NN%D%R,TYPDIN,NN%X%R,TYPEXN,CC,
     &                     MESH%NULONE%I,MESH%NELBOR%I,
     &                     MESH%NBOR%I,
     &                     NELMAX,NDIAGM,NPTFR,MESH%NELEBX,
     &                     MESH%NELEB)
!
        ELSE
          WRITE(LU,101) M%NAME
          WRITE(LU,201) IELM1,IELM2
          WRITE(LU,151) NN%NAME
          WRITE(LU,251) IELN1,IELN2
          WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!     RECTANGULAR MATRICES WITH 3 AND 6 POINTS
!
      ELSEIF(IELM1.EQ.11.AND.IELM2.EQ.13) THEN
!
        IF((IELN1.EQ.11.AND.IELN2.EQ.13).OR.
     &     (IELN1.EQ.13.AND.IELN2.EQ.11.AND.OP(1:4).EQ.'M=TN')) THEN
          CALL OM1113(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                     NN%D%R,TYPDIN,NN%X%R,TYPEXN,DD%R,CC,
     &                     IKLE,NELEM,NELMAX,NDIAGM)
        ELSE
          WRITE(LU,101) M%NAME
          WRITE(LU,201) IELM1,IELM2
          WRITE(LU,151) NN%NAME
          WRITE(LU,251) IELN1,IELN2
          WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!     RECTANGULAR MATRICES WITH 6 AND 3 POINTS
!
      ELSEIF(IELM1.EQ.13.AND.IELM2.EQ.11) THEN
!
        IF((IELN1.EQ.13.AND.IELN2.EQ.11).OR.
     &     (IELN1.EQ.11.AND.IELN2.EQ.13.AND.OP(1:4).EQ.'M=TN')) THEN
          CALL OM1311(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                NN%D%R,TYPDIN,NN%X%R,TYPEXN,DD%R,CC,
     &                IKLE,NELEM,NELMAX,NDIAGM)
        ELSE
          WRITE(LU,101) M%NAME
          WRITE(LU,201) IELM1,IELM2
          WRITE(LU,151) NN%NAME
          WRITE(LU,251) IELN1,IELN2
          WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!  COMBINATION OF IELM1 AND IELM2 NOT IMPLEMENTED: ERROR
!
      ELSE
        WRITE(LU,101) M%NAME
        WRITE(LU,201) IELM1,IELM2
        WRITE(LU,411) STOM,STON
        WRITE(LU,301)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      ELSEIF(STOM.EQ.3.AND.STON.EQ.3) THEN
!
!  STORAGE BY SEGMENT
!
        IF(M%ELMCOL.NE.NN%ELMCOL.OR.M%ELMLIN.NE.NN%ELMLIN) THEN
!         WRITE(LU,*) 'M ET N DE STRUCTURES DIFFERENTES',M%ELMCOL
!         CALL PLANTE(1)
!         STOP
!
!     EDGE-BASED STORAGE FOR M AND N
!     THIS CAN HAPPEN ONLY WHEN N IS A BOUNDARY MATRIX (FD : REALLY?)
!     TESTED SO FAR FOR TETRA (81) AND TRIANGLES (31)
!     BORDER SEGMENTS ARE LINKED TO THE NSEGBOR FIRST SEGMENTS
!
          MSEG1 = BIEF_NBSEG(M%ELMLIN,MESH)
          MSEG2 = BIEF_NBSEG(M%ELMCOL,MESH)
          NSEG1 = BIEF_NBSEG(NN%ELMLIN,MESH)
          NSEG2 = BIEF_NBSEG(NN%ELMCOL,MESH)
!
          CALL OMBORSEG(OP,M%D%R,M%X%R,TYPEXM,
     &         NN%D%R,NN%X%R,TYPEXN,CC,
     &         NDIAGN,MSEG1,MSEG2,NSEG1,NSEG2,
     &         MESH%NBOR%I)
!
        ELSE
!
          NSEG1 = BIEF_NBSEG(M%ELMLIN,MESH)
          NSEG2 = BIEF_NBSEG(M%ELMCOL,MESH)
!
!         IN LINEAR-QUADRATIC RECTANGULAR MATRICES, PURELY QUADRATIC
!         SEGMENTS ARE NOT CONSIDERED (NUMBER 13,14 AND 15, SO 3 PER ELEMENT)
!
          IF(IELM1.EQ.11.AND.IELM2.EQ.13) THEN
            NSEG2=NSEG2-3*NELEM
          ELSEIF(IELM1.EQ.13.AND.IELM2.EQ.11) THEN
            NSEG1=NSEG1-3*NELEM
          ENDIF
!
!         IN LINEAR-QUADRATIC RECTANGULAR MATRICES, PURELY QUADRATIC
!         SEGMENTS ARE NOT CONSIDERED (NUMBER 13,14 AND 15, SO 3 PER ELEMENT)
!
          CALL OMSEG(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                    NN%D%R,TYPDIN,NN%X%R,TYPEXN,DD%R,CC,
     &                    NDIAGM,NSEG1,NSEG2,MESH%GLOSEG%I,
     &                    MESH%GLOSEG%MAXDIM1)
!
        ENDIF
!
      ELSEIF(STOM.EQ.3.AND.STON.EQ.1) THEN
!
!       EDGE-BASED STORAGE FOR M AND EBE FOR N
!       THIS CAN HAPPEN ONLY WHEN N IS A BOUNDARY MATRIX
!
        IF(  ( M%ELMLIN.EQ.11.AND. M%ELMCOL.EQ.11.AND.
     &        NN%ELMLIN.EQ.1 .AND.NN%ELMCOL.EQ.1) .OR.
     &       ( M%ELMLIN.EQ.12.AND. M%ELMCOL.EQ.12.AND.
     &        NN%ELMLIN.EQ.1 .AND.NN%ELMCOL.EQ.1) .OR.
     &       ( M%ELMLIN.EQ.13.AND. M%ELMCOL.EQ.13.AND.
     &        NN%ELMLIN.EQ.1 .AND.NN%ELMCOL.EQ.1) .OR.
     &       ( M%ELMLIN.EQ.13.AND. M%ELMCOL.EQ.13.AND.
     &        NN%ELMLIN.EQ.2 .AND.NN%ELMCOL.EQ.2)      ) THEN
!
          NSEG1 = BIEF_NBSEG(M%ELMLIN,MESH)
          NSEG2 = BIEF_NBSEG(M%ELMCOL,MESH)
!
          CALL OMSEGBOR(OP,M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                     NN%D%R,TYPDIN,NN%X%R,TYPEXN,CC,
     &                     NDIAGM,NSEG1,MESH%NBOR%I,
     &                     NPTFR,M%ELMLIN,NN%ELMLIN,
     &                     BIEF_NBSEG(11,MESH),
     &                     MESH%IKLBOR%I,MESH%NELEBX,MESH%NELEB)
!
        ELSE
          WRITE(LU,*) 'OM : UNEXPECTED CASE IN SEGMENT STORAGE'
          WRITE(LU,*) '     M%ELMLIN=',M%ELMLIN
          WRITE(LU,*) '     M%ELMCOL=',M%ELMCOL
          WRITE(LU,*) '     M%NAME=',M%NAME
          WRITE(LU,*) '     NN%ELMLIN=',NN%ELMLIN
          WRITE(LU,*) '     NN%ELMCOL=',NN%ELMCOL
          WRITE(LU,*) '     NN%NAME=',NN%NAME
          WRITE(LU,*) '     IMPLEMENTATION MISSING'
          CALL PLANTE(1)
          STOP
        ENDIF
!
!  COMBINATION OF IELM1 AND IELM2 NOT IMPLEMENTED: ERROR
!
!     ELSE
!        WRITE(LU,101) M%NAME
!        WRITE(LU,201) IELM1,IELM2
!        WRITE(LU,411) STOM,STON
!        WRITE(LU,301)
!        CALL PLANTE(1)
!        STOP
!     ENDIF
!
      ELSE
!
!  STORAGE COMBINATION NOT IMPLEMENTED
!
        WRITE(LU,101) M%NAME
        WRITE(LU,411) STOM,STON
        WRITE(LU,301)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  RE-ENCODES THE NEW TYPE
!
      M%TYPDIA = TYPDIM
      M%TYPEXT = TYPEXM
      IF(OP(3:8).EQ.'X(M)  ') THEN
        M%X%DIM1=BIEF_DIM1_EXT(IELM1,IELM2,STOM,'Q',MESH)
        M%X%DIM2=BIEF_DIM2_EXT(IELM1,IELM2,STOM,'Q',MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
101   FORMAT(1X,'OM (BIEF) : MATRIX  M (REAL NAME:',A6,')')
151   FORMAT(1X,'OM (BIEF) : MATRIX  N (REAL NAME:',A6,')')
201   FORMAT(1X,'            IELM1 = ',1I6,' IELM2 = ',1I6)
251   FORMAT(1X,'            IELN1 = ',1I6,' IELN2 = ',1I6)
301   FORMAT(1X,'            THIS CASE IS NOT IMPLEMENTED')
411   FORMAT(1X,'AND STORAGES   M  : ',1I6,' STON  : ',1I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
