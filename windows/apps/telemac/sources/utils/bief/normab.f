!                   *****************
                    SUBROUTINE NORMAB
!                   *****************
!
     &(XNEBOR,YNEBOR,XSGBOR,YSGBOR,DISBOR,SURFAC,NELMAX,
     & NELBOR,NULONE,LGSEG,NPTFR,MESH,XEL,YEL,IKLBOR,NELEBX,NELEB)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!warning  XSGBOR(*,2) and XSGBOR(*,4) should not be used anymore
!+        could be removed (with care...).
!
!brief    1) COMPUTES THE COMPONENTS OF THE OUTGOING NORMAL VECTOR
!+
!+               - FOR THE BOUNDARY POINTS      (XNEBOR,YNEBOR)
!+
!+               - FOR THE BOUNDARY SEGMENTS    (XSGBOR,YSGBOR)
!+
!+            2) DISTANCE TO THE BOUNDARY OF THE FIRST ELEMENT POINTS
!+
!+            3) LENGTH OF THE BOUNDARY SEGMENTS
!+
!+            4) DISTANCE TO THE BOUNDARY OF THE FIRST INTERNAL POINTS
!code
!+  BEWARE:  XSGBOR AND YSGBOR DIMENSION IS (NPTFR,4):
!+
!+           (K,1) : NORMALISED    , SEGMENT FOLLOWING K
!+           (K,2) : NORMALISED    , SEGMENT PRECEDING K
!+           (K,3) : NOT NORMALISED, SEGMENT FOLLOWING K
!+           (K,4) : NOT NORMALISED, SEGMENT PRECEDING K
!+
!+           XSGBOR(K,1) AND YSGBOR(K,1) ARE THE COMPONENTS
!+           FOR THE SEGMENT FOLLOWING POINT K.
!+
!+           XSGBOR(K,2) AND YSGBOR(K,2) ARE THE COMPONENTS
!+           FOR THE SEGMENT PRECEDING POINT K.
!
!history  J-M HERVOUET (LNHE)
!+        26/06/2008
!+        V5P9
!+   Modifications for parallelism.
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        10/01/2013
!+        V6P3
!+   LGSEG now computed with coordinates per element (XEL and YEL)
!+   This is important in spherical coordinates.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/03/2014
!+        V7P0
!+   Now written to enable different numbering of boundary points and
!+   boundary segments.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DISBOR         |<--| DISTANCE FROM BOUNDARY POINT TO CLOSER
!|                |   | INNER POINT
!| IKLBOR         |-->| CONNECTIVITY TABLE FOR BOUNDARY ELEMENTS
!| LGSEG          |<--| LENGTH OF BOUNDARY SEGMENTS
!| MESH           |-->| MESH STRUCTURE
!| NELBOR         |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT.
!| NULONE         |-->| NUMBER OF FIRST POINT OF SEGMENT IN ADJACENT ELEMENT
!| NELMAX         |-->| NUMBER OF ELEMENTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| SURFAC         |-->| AREA OF TRIANGLES
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XNEBOR         |<--| COMPONANT ALONG X OF VECTOR NORMAL TO POINT
!| XSGBOR         |<--| COMPONANT ALONG X OF VECTOR NORMAL TO SEGMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XNEBOR         |<--| COMPONANT ALONG Y OF VECTOR NORMAL TO POINT
!| YSGBOR         |<--| COMPONANT ALONG Y OF VECTOR NORMAL TO SEGMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_NORMAB => NORMAB
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR,NELMAX,NELEBX,NELEB
      INTEGER, INTENT(IN) :: NELBOR(NELEBX)
      INTEGER, INTENT(IN) :: NULONE(NELEBX,2),IKLBOR(NELEBX,2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: XNEBOR(NPTFR,2),YNEBOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(INOUT) :: XSGBOR(NELEBX,4)
      DOUBLE PRECISION, INTENT(INOUT) :: YSGBOR(NELEBX,4)
      DOUBLE PRECISION, INTENT(INOUT) :: DISBOR(NPTFR),LGSEG(NELEBX)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,3),YEL(NELMAX,3)
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K1,K2,IELEM,I1,I2,IELEB
      DOUBLE PRECISION X12,Y12,XNORM,X1,X2,Y1,Y2,Z(1)
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE NORMAL VECTORS AND SEGMENT LENGTHS
!
!     0) INITIALISES LGSEG, XSGBOR AND YSGBOR TO 0
!
      IF(NPTFR.GT.0) THEN
!
!       TO START WITH XSGBOR AND YSGBOR IN BOUNDARY NODES NUMBERING
!
        DO K1=1,NPTFR
          XSGBOR(K1,1) = 0.D0
          YSGBOR(K1,1) = 0.D0
          XSGBOR(K1,2) = 0.D0
          YSGBOR(K1,2) = 0.D0
        ENDDO
!
!       1) NORMALS TO SEGMENTS AND LENGTH OF THE BOUNDARY SEGMENT
!          COMMON VERSION FOR SCALAR/PARALLEL MODES
!          STILL WITH BOUNDARY NODES NUMBERING
!
        DO IELEB=1,NELEB
!
          K1=IKLBOR(IELEB,1)
          K2=IKLBOR(IELEB,2)
!
          IELEM=NELBOR(IELEB)
          I1=NULONE(IELEB,1)
          I2=NULONE(IELEB,2)
          X1=XEL(IELEM,I1)
          Y1=YEL(IELEM,I1)
          X2=XEL(IELEM,I2)
          Y2=YEL(IELEM,I2)
          X12 = X2 - X1
          Y12 = Y2 - Y1
!         LENGTH OF THE BOUNDARY SEGMENT
          LGSEG(IELEB) = SQRT( X12**2 + Y12**2 )
!         NORMAL TO THE SEGMENT FOLLOWING K1:
          XSGBOR(K1,1) =  Y12
          YSGBOR(K1,1) = -X12
!         NORMAL TO THE SEGMENT PRECEDING THE ONE FOLLOWING K1:
          XSGBOR(K2,2) =  Y12
          YSGBOR(K2,2) = -X12
!
        ENDDO
!
      ENDIF
!
!     2) COMPLEMENT IN PARALLEL MODE, WITH PARCOM OPTION 1
!        (VALUE OF GREATER ABSOLUTE VALUE)
!
      IF(NCSIZE.GT.1) THEN
        IF(NPTFR.GT.0) THEN
!         CALL PARCOM_BORD(LGSEG            ,1,MESH)
          CALL PARCOM_BORD(XSGBOR(1:NPTFR,1),1,MESH)
          CALL PARCOM_BORD(XSGBOR(1:NPTFR,2),1,MESH)
          CALL PARCOM_BORD(YSGBOR(1:NPTFR,1),1,MESH)
          CALL PARCOM_BORD(YSGBOR(1:NPTFR,2),1,MESH)
        ELSE
!         THIS DOES NOTHING FOR THE SUB-DOMAIN, BUT IS
!         NECESSARY TO THE PARALLEL COMMUNICATION
!         IN CASE OF CALL PARCOM_BORD, ALL PROCESSORS MUST CALL IT
!         CALL PARCOM_BORD(Z,1,MESH)
          CALL PARCOM_BORD(Z,1,MESH)
          CALL PARCOM_BORD(Z,1,MESH)
          CALL PARCOM_BORD(Z,1,MESH)
          CALL PARCOM_BORD(Z,1,MESH)
        ENDIF
      ENDIF
!
!     3) NORMALS BY NODES, APPROXIMATE DISTANCE FROM THE BOUNDARY
!        THE VECTORS ARE THEN NORMALISED
!
      IF(NPTFR.GT.0) THEN
!
      DO K1=1,NPTFR
!
!       NORMAL AT THE POINT: AVERAGE OF 2 NOT NORMALISED NORMALS
!       ASSOCIATED WITH THE 2 ADJACENT SEGMENTS
!
!       NOT NORMALISED VERSION XNEBOR(*,2) AND YNEBOR(*,2)
        XNEBOR(K1,2)=(XSGBOR(K1,1)+XSGBOR(K1,2))*0.5D0
        YNEBOR(K1,2)=(YSGBOR(K1,1)+YSGBOR(K1,2))*0.5D0
!
!       NOT NORMALISED VERSION XSGBOR(*,3) AND XSGBOR(*,4)
!                              YSGBOR(*,3) AND YSGBOR(*,4)
        XSGBOR(K1,3)=XSGBOR(K1,1)
        XSGBOR(K1,4)=XSGBOR(K1,2)
        YSGBOR(K1,3)=YSGBOR(K1,1)
        YSGBOR(K1,4)=YSGBOR(K1,2)
!
!       NORMALISED VERSION XNEBOR(*,1) AND YNEBOR(*,1)
        XNORM=SQRT(XNEBOR(K1,2)**2+YNEBOR(K1,2)**2)
        XNEBOR(K1,1)=XNEBOR(K1,2)/XNORM
        YNEBOR(K1,1)=YNEBOR(K1,2)/XNORM
!
!       NORMALISED VERSION OF XSGBOR AND YSGBOR FOR FOLLOWING SEGMENT
        XNORM=SQRT(XSGBOR(K1,1)**2+YSGBOR(K1,1)**2)
        XSGBOR(K1,1)=XSGBOR(K1,1)/XNORM
        YSGBOR(K1,1)=YSGBOR(K1,1)/XNORM
!
!       NORMALISED VERSION OF XSGBOR AND YSGBOR FOR PRECEDING SEGMENT
        XNORM=SQRT(XSGBOR(K1,2)**2+YSGBOR(K1,2)**2)
        XSGBOR(K1,2)=XSGBOR(K1,2)/XNORM
        YSGBOR(K1,2)=YSGBOR(K1,2)/XNORM
!
!       INITIALISATION FOR NEXT LOOP
!
        DISBOR(K1)=0.D0
!
      ENDDO
!
!     DISTANCE TO BOUNDARY
!
      DO IELEB=1,NELEB
!
        K1=IKLBOR(IELEB,1)
!
!       THIS CAN BE APPROXIMATION OF THE MESH SIZE AT THE BOUNDARY
!       AND IS USED FOR LOG LAW AT THE BOUNDARIES
        DISBOR(K1) = 2.D0*SURFAC(NELBOR(IELEB))/LGSEG(IELEB)
!
      ENDDO
!
      ENDIF
!
!     DISBOR IS POSITIVE, CAN TAKE THE MAX
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM_BORD(DISBOR,3,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     NOW, XSGBOR, YSGBOR AND DISBOR ARE GIVEN THEIR SEGMENT NUMBERING
!
!     IN THIS LOOP IELEB ALWAYS SMALLER OR EQUAL TO K1
!     SO NOTHING IS ERASED PREMATURELY
!
!     IN SCALAR MODE THE NUMBERING IS THE SAME SO NOTHING TO DO
!
      IF(NCSIZE.GT.1) THEN
!
        DO IELEB=1,NELEB
!
          K1=IKLBOR(IELEB,1)
!
          XSGBOR(IELEB,1)= XSGBOR(K1,1)
          XSGBOR(IELEB,2)= XSGBOR(K1,2)
          XSGBOR(IELEB,3)= XSGBOR(K1,3)
          XSGBOR(IELEB,4)= XSGBOR(K1,4)
          YSGBOR(IELEB,1)= YSGBOR(K1,1)
          YSGBOR(IELEB,2)= YSGBOR(K1,2)
          YSGBOR(IELEB,3)= YSGBOR(K1,3)
          YSGBOR(IELEB,4)= YSGBOR(K1,4)
!         DISBOR(IELEB)  = DISBOR(K1)
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
