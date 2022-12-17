!                   ****************
                    SUBROUTINE ELEBD
!                   ****************
!
     &(NELBOR,NULONE,KP1BOR,IFABOR,NBOR,IKLE,SIZIKL,IKLBOR,NELEM,NELMAX,
     & NPOIN,NPTFR,IELM,LIHBOR,KLOG,ISEG,T1,T2,T3,NELEBX,
     & NELEB)
!
!***********************************************************************
! BIEF   V7P2
!***********************************************************************
!
!brief    BUILDING DATA STRUCTURES TO NAVIGATE IN A 2D MESH.
!+
!+            1) ARRAYS NELBOR AND NULONE,
!+
!+            2) ARRAY KP1BOR,
!+
!+            3) DISTINGUISHES IN THE ARRAY IFABOR FOR
!+                   SOLID BOUNDARY FACES OR LIQUID FACES,
!+
!+            4) IKLBOR, CONNECTIVITY TABLE OF BOUNDARY ELEMENTS
!
!history  J-M HERVOUET (LNHE)
!+        23/06/2008
!+        V5P9
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
!history  J-M HERVOUET (LNHE)
!+        18/03/2013
!+        V6P3
!+   Error messages more accurate.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/03/2014
!+        V7P0
!+   Now written to enable different numbering of boundary points and
!+   boundary segments.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        16/06/2014
!+        V7P0
!+   New possible errors in the mesh now stopped.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        25/04/2016
!+        V7P2
!+   Checking wrong meshes where a point appears on several different
!+   boundaries.
!
!history  S.E.BOURBAN (HRW)
!+        21/03/2017
!+        V7P3
!+   Replacement of the DATA declarations by the PARAMETER associates
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IELM           |-->| TYPE D'ELEMENT.
!|                |   | 11 : TRIANGLES.
!|                |   | 21 : QUADRILATERES.
!| IFABOR         |-->| TABLEAU DES VOISINS DES FACES.
!| IKLBOR         |<--| CONNECTIVITY TABLE OF BOUNDARY ELEMENTS
!| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
!| ISEG           |---|
!| KLOG           |-->| CONVENTION POUR LA CONDITION LIMITE DE PAROI
!| KP1BOR         |<--| NUMERO DU POINT SUIVANT LE POINT DE BORD K.
!| LIHBOR         |-->| TYPES DE CONDITIONS AUX LIMITES SUR H
!| NBOR           |-->| NUMERO GLOBAL DU POINT DE BORD K.
!| NELBOR         |<--| NUMERO DE L'ELEMENT ADJACENT AU KIEME SEGMENT
!| NELEM          |-->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE.
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NOMBRE TOTAL DE POINTS DU DOMAINE.
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
!| NULONE         |<--| NUMERO LOCAL D'UN POINT DE BORD DANS
!|                |   | L'ELEMENT ADJACENT DONNE PAR NELBOR
!| SIZIKL         |-->| FIRST DIMENSION OF IKLE
!| T2             |---|
!| T3             |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ELEBD => ELEBD
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: KLOG,NELMAX,NELEM,SIZIKL,NELEBX
      INTEGER, INTENT(INOUT) :: NELEB
      INTEGER, INTENT(IN)    :: NPOIN,NPTFR,IELM
      INTEGER, INTENT(INOUT) :: NELBOR(NELEBX),NULONE(NELEBX,2)
      INTEGER, INTENT(INOUT) :: KP1BOR(NPTFR,2)
      INTEGER, INTENT(INOUT) :: NBOR(*)
      INTEGER, INTENT(INOUT) :: IFABOR(NELMAX,*)
      INTEGER, INTENT(IN)    :: IKLE(SIZIKL,*)
      INTEGER, INTENT(IN)    :: LIHBOR(NPTFR)
      INTEGER, INTENT(INOUT) :: IKLBOR(NELEBX,2)
      INTEGER, INTENT(IN)    :: ISEG(*)
      INTEGER, INTENT(INOUT) :: T1(NPOIN),T2(NPOIN),T3(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,NFACE,NPT,KEL,IPOIN
      INTEGER K,IFACE,I1,I2,N1,N2,IPT,IEL,I,K1,K2
!
      INTEGER :: SOMFAC(2,4,2)
      PARAMETER ( SOMFAC = RESHAPE( (/
     &      1,2 , 2,3 , 3,1 , 0,0   ,
     &      1,2 , 2,3 , 3,4 , 4,1 /), SHAPE=(/ 2,4,2 /) ) )
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
!       TRIANGLES
        NFACE = 3
        NPT = 3
        KEL = 1
      ELSE
        WRITE(LU,901) IELM
901     FORMAT(1X,'ELEBD: IELM=',1I6,' UNKNOWN TYPE OF ELEMENT')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!  INITIALISES T1,2,3 TO 0
!
      DO IPOIN=1,NPOIN
        T1(IPOIN) = 0
        T2(IPOIN) = 0
        T3(IPOIN) = 0
      ENDDO
!
!  STORES K IN TRAV(*,3), ADDRESS NBOR(K)
!  GIVES CORRESPONDENCE GLOBAL --> BOUNDARY NUMBER
!  FINDS POINTS WITH MORE THAN ONE BOUNDARY
!
      DO K = 1, NPTFR
        T3(NBOR(K)) = K
        T1(NBOR(K)) = T1(NBOR(K)) +1
      ENDDO
      DO I = 1, NPOIN
        IF(T1(I).GT.1) THEN
          WRITE(LU,*) 'ELEBD: POINT ',I
          WRITE(LU,*) 'APPEARS ON ',T1(I)
          WRITE(LU,*) 'DIFFERENT BOUNDARIES'
          WRITE(LU,*) 'WRONG MESH'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!  REINITIALISES T1 TO 0 (IS THIS USEFUL ??)
!
      DO IPOIN=1,NPOIN
        T1(IPOIN) = 0
      ENDDO
!
!  LOOP ON ALL THE FACES OF ALL THE ELEMENTS:
!
      DO IFACE = 1 , NFACE
        DO IELEM = 1 , NELEM
          IF(IFABOR(IELEM,IFACE).EQ.-1) THEN
!           THIS IS A TRUE BOUNDARY FACE
!           INTERNAL FACES ARE MARKED WITH -2 IN PARALLELE MODE
!           GLOBAL NUMBERS OF THE FACE POINTS :
            I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
            I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
!           STORES IN T1 AND T2 (ADDRESS I1) : I2 AND IELEM
            T1(I1) = I2
            T2(I1) = IELEM
!           A LIQUID FACE IS RECOGNIZED BY THE BOUNDARY CONDITION ON H
            IF(NPTFR.GT.0) THEN
              IF(T3(I1).NE.0.AND.T3(I2).NE.0) THEN
                IF(LIHBOR(T3(I1)).NE.KLOG.AND.
     &             LIHBOR(T3(I2)).NE.KLOG) THEN
!                 LIQUID FACE : IFABOR=0  SOLID FACE : IFABOR=-1
                  IFABOR(IELEM,IFACE)=0
                ENDIF
              ELSE
                IF(T3(I1).EQ.0) THEN
                  WRITE(LU,*) 'POINT ',I1,' IS ON A BOUNDARY'
                ENDIF
                IF(T3(I2).EQ.0) THEN
                  WRITE(LU,*) 'POINT ',I2,' IS ON A BOUNDARY'
                ENDIF
                WRITE(LU,*) 'BUT NOT IN THE LIST OF BOUNDARY POINTS'
                WRITE(LU,*) 'OR THE NUMBER OF LINES IN THE BOUNDARY'
                WRITE(LU,*) 'CONDITIONS FILE IS GREATER THAN:',NPTFR
                WRITE(LU,*) 'NUMBER TAKEN IN THE GEOMETRY FILE'
                CALL PLANTE(1)
                STOP
              ENDIF
            ENDIF
!
          ENDIF
        ENDDO ! IELEM
      ENDDO ! IFACE
!
!     LOOP ON ALL THE BOUNDARY POINTS TO BUILD KP1BOR:
!
      IF(NPTFR.GT.0) THEN
        DO I = 1 , NPOIN
          IF(T1(I).NE.0) THEN
!           FOLLOWING POINT
            KP1BOR(T3(I),1)=T3(T1(I))
!           PRECEDING POINT
            KP1BOR(T3(T1(I)),2)=T3(I)
            NELBOR(T3(I))=T2(I)
          ENDIF
        ENDDO
      ENDIF
!
!     DUMMY VALUES IN KP1BOR WHEN THE FOLLOWING POINT IS IN ANOTHER SUB-DOMAIN
!     NELBOR AND NULONE SET TO 0
!
      IF(NCSIZE.GT.1) THEN
        DO K1=1,NPTFR
          IF(ISEG(K1).GT.0) THEN
            KP1BOR(K1,1)=K1
            NELBOR(K1)=0
            NULONE(K1,1)=0
            NULONE(K1,2)=0
          ELSEIF(ISEG(K1).EQ.-9999) THEN
            KP1BOR(K1,1)=K1
            KP1BOR(K1,2)=K1
            NELBOR(K1)  =0
            NULONE(K1,1)=0
            NULONE(K1,2)=0
          ELSEIF(ISEG(K1).LT.0) THEN
            KP1BOR(K1,2)=K1
          ENDIF
        ENDDO
      ENDIF
!
! COMPUTES ARRAY NULONE
!
      DO K1=1,NPTFR
!
      IF(NCSIZE.GT.1) THEN
        IF(ISEG(K1).GT.0.OR.ISEG(K1).EQ.-9999) CYCLE
      ENDIF
!
      K2=KP1BOR(K1,1)
      IEL = NELBOR(K1)
      N1  = NBOR(K1)
      IF(K2.LT.0.OR.K2.GT.NPTFR) PRINT*,'K2=',K2,' K1=',K1
      N2  = NBOR(K2)
!
      I1 = 0
      I2 = 0
!
      DO IPT=1,NPT
        IF(IKLE(IEL,IPT).EQ.N1) THEN
          NULONE(K1,1) = IPT
          I1 = 1
        ENDIF
        IF(IKLE(IEL,IPT).EQ.N2) THEN
          NULONE(K1,2) = IPT
          I2 = 1
        ENDIF
      ENDDO
!
      IF(I1.EQ.0.OR.I2.EQ.0) THEN
        WRITE(LU,811) IPT,K1,IEL
811     FORMAT(1X,'ELEBD: ERROR AT POINT:',    I10                   ,/,
     &         1X,'       THE BOUNDARY POINT:',I10                   ,/,
     &         1X,'       DOES NOT BELONG TO ELEMENT : ',I10         ,/,
     &         1X,'       POSSIBLE REASONS:'                         ,/,
     &         1X,'       THE BOUNDARY CONDITION FILE IS NOT      '  ,/,
     &         1X,'       RELEVANT TO THE GEOMETRY FILE           '  ,/,
     &         1X,'       OR THE MESH HAS A WRONG TOPOLOGY')
        CALL PLANTE(1)
        STOP
      ENDIF
!
      ENDDO ! K1
!
!     COMPUTES IKLBOR : LIKE IKLE FOR BOUNDARY POINTS, WITH BOUNDARY
!                       POINTS NUMBERING
!
!                       IN 3D WILL BE REDONE IN OTHER ELEB.. ROUTINES
!
!     IF(IELM.NE.41.AND.IELM.NE.51) THEN
      DO K=1,NPTFR
        IKLBOR(K,1) = K
        IKLBOR(K,2) = KP1BOR(K,1)
      ENDDO
!     ENDIF
!
!-----------------------------------------------------------------------
!
!     VERSION 7.0: NOW RENUMBERING IN PARALLEL TO AVOID HOLES IN SEGMENT
!                  NUMBERING. NELEB BECOMES THE REAL NUMBER OF BOUNDARY
!                  ELEMENTS
!
      IF(NCSIZE.GT.1) THEN
        NELEB=0
        DO K=1,NPTFR
          IF(KP1BOR(K,1).NE.K) THEN
            NELEB=NELEB+1
            NELBOR(NELEB)  =NELBOR(K)
            NULONE(NELEB,1)=NULONE(K,1)
            NULONE(NELEB,2)=NULONE(K,2)
            IKLBOR(NELEB,1)=IKLBOR(K,1)
            IKLBOR(NELEB,2)=IKLBOR(K,2)
          ENDIF
        ENDDO
      ELSE
        NELEB=NPTFR
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

