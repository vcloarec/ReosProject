!                   *****************
                    SUBROUTINE MT14TT
!                   *****************
!
     &(T,XM,LEGO,XMUL,SW,W,H,
     & X,Y,IKLE,NELEM,NELMAX,NPLAN,NPOIN2)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS COEFFICIENTS LAMBDA(I,J) FOR N-TYPE MURD SCHEME.
!+
!+            THE ELEMENT IS THE P1 TETRAHEDRON.
!
!warning  THE JACOBIAN MUST BE POSITIVE
!+        Only for prisms cut into tetrahedra so far.
!
!reference  J-M HERVOUET THESIS OR BOOK: MURD SCHEME IN 3 DIMENSIONS
!+             CHAPTER 6.
!+
!+          Computational Fluid dynamics '92 volume 1. Multidimensional
!+          upwind schemes for scalar advection on tetrahedral meshes.
!+          G. Bourgois, H. Deconinck, P.L. Roe and R. Struijs
!
!history  J-M HERVOUET (LNHE)
!+        18/09/2011
!+        V6P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| H              |-->| FUNCTION USED IN THE FORMULA
!| IKLE           |-->| CONNECTIVITY TABLE
!| LEGO           |-->| LOGICAL. IF YES RESULT ASSEMBLED.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELEM2         |-->| NUMBER OF TRIANGLES IN THE UNDERLYING 2D MESH
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPLAN          |-->| NUMBER OF PLANES IN THE MESH
!| NPOIN2         |-->| NUMBER OF POINTS IN THE UNDERLYING 2D MESH
!| SW             |-->| BIEF_OBJ STRUCTURE OF W
!| T              |<->| WORK ARRAY FOR ELEMENT BY ELEMENT DIAGONAL
!| W              |-->| FUNCTION USED IN THE FORMULA
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| XM             |<->| OFF-DIAGONAL TERMS
!| XMUL           |-->| COEFFICIENT FOR MULTIPLICATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF           !, EX_MT14TT => MT14TT
      USE DECLARATIONS_TELEMAC, ONLY : ISEGT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPLAN,NPOIN2
      INTEGER, INTENT(IN) :: IKLE(NELMAX,4)
!
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,4),XM(12,NELMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: W(*)
      DOUBLE PRECISION, INTENT(IN) :: H(NELMAX,4)
!
      LOGICAL, INTENT(IN) :: LEGO
!
!     STRUCTURES DE U,V,W
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SW
!
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELEM,IPLAN,J1,J2,ISEG,II1,II2,II3,IELEM3D,NELEM2
!
      DOUBLE PRECISION X2,X3,Y2,Y3
      DOUBLE PRECISION L12,L13,L14,L23,L24,L34,L21,L31,L41,L32,L42,L43
      DOUBLE PRECISION ALFA(4),SURFSUR3,SUMMAXK
!
      INTEGER IL1,IL2,I1,I2,I3,I4,I5,I6,I
!
!-----------------------------------------------------------------------
!
!     THE SIX SEGMENTS IN A TETRAHEDRON
!     ISEGT(ISEG,1 OR 2) : FIRST OR SECOND POINT OF SEGMENT ISEG
!     INTEGER ISEGT(6,2)
!     DATA ISEGT/1,2,3,1,2,3,2,3,1,4,4,4/
!
!=======================================================================
!
!     RETRIEVING THE NUMBER OF ELEMENTS THE 2D MESH
!
      NELEM2=NELMAX/3/(NPLAN-1)
!                                                         *   *
!     COMPUTES THE COEFFICIENTS A(I) (INTEGRAL OF H U.GRAD(PSI), ONLY
!     CONSIDERING THE HORIZONTAL PART OF U). SEE JMH THESIS
!
!     NOTE: THIS VC04TT CALL IS ALREADY DONE BY FLUX3D IN TELEMAC-3D
!           THROUGH A CALL TO VECTOR. THE EQUIVALENT OF T HAS BEEN
!           SAVED IN WHAT IS HERE H
!
!     FORMUL='             HOR'
!     CALL VC04TT(XMUL,SU,SV,SW,U,V,W,SF,SG,SH,F,G,H,X,Y,Z,
!    * IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
!    * NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),
!    * SPECAD,FORMUL,NPLAN)
!
!     NOW T IS RETRIEVED BY STORAGE DONE INTO FLUX3D
!     FUNCTION H MUST CONTAIN THE EQUIVALENT OF T ABOVE, WHICH IS
!     THE NON ASSEMBLED VALUE OF FLUINT
!
!     DO IELEM=1,NELEM
!       T(IELEM,1)=H(         IELEM)
!       T(IELEM,2)=H(  NELMAX+IELEM)
!       T(IELEM,3)=H(2*NELMAX+IELEM)
!       T(IELEM,4)=H(3*NELMAX+IELEM)
!     ENDDO
!
!     FORMUL NORMALLY STARTS WITH VGRADP OR VGRADP2, OR VGRADP22 TO KNOW
!     SIGMAG AND SPECAD, USELESS HERE.
!
!-----------------------------------------------------------------------
!
      IF(SW%ELM.NE.51) THEN
        WRITE(LU,*) 'MT14TT DISCRETISATION NON PREVUE'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO IPLAN=1,NPLAN-1
        DO IELEM=1,NELEM2
!
!         THE SIX GLOBAL NUMBERS OF POINTS IN THE PRISM
!         IKLE HAS BEEN BUILT SO THAT IT COINCIDES WITH IKLE2D ON THE
!         FIRST LAYER (SEE CPIKLE3).
!
          II1=IKLE(IELEM,1)
          II2=IKLE(IELEM,2)
          II3=IKLE(IELEM,3)
          I1=II1+(IPLAN-1)*NPOIN2
          I2=II2+(IPLAN-1)*NPOIN2
          I3=II3+(IPLAN-1)*NPOIN2
          I4=I1+NPOIN2
          I5=I2+NPOIN2
          I6=I3+NPOIN2
!
          X2=X(II2)-X(II1)
          X3=X(II3)-X(II1)
          Y2=Y(II2)-Y(II1)
          Y3=Y(II3)-Y(II1)
          SURFSUR3=XMUL*(X2*Y3-X3*Y2)/6.D0
!
!         EVERY TETRAHEDRON IN THE PRISM
!
          DO I=1,3
!
            IELEM3D=3*NELEM2*(IPLAN-1)+(I-1)*NELEM2+IELEM
!
!           CONTRIBUTION OF HORIZONTAL GRADIENTS
!
            ALFA(1)=XMUL*H(IELEM3D,1)
            ALFA(2)=XMUL*H(IELEM3D,2)
            ALFA(3)=XMUL*H(IELEM3D,3)
            ALFA(4)=XMUL*H(IELEM3D,4)
!
!           CONTRIBUTION OF VERTICAL GRADIENTS
!           ONLY FOR VERTICAL SEGMENTS
!
!           LOOP ON EVERY SEGMENT IN THE PRISM
!
            DO ISEG=1,6
!             SEE NUMBERING OF TETRAHEDRONS IN CPIKLE3
              IL1=ISEGT(ISEG,1)
              IL2=ISEGT(ISEG,2)
              J1=IKLE(IELEM3D,IL1)
              J2=IKLE(IELEM3D,IL2)
!             VERTICAL SEGMENTS
              IF(J1.EQ.I1.AND.J2.EQ.I4) THEN
                ALFA(IL1)=ALFA(IL1)-W(I1)*SURFSUR3
                ALFA(IL2)=ALFA(IL2)+W(I1)*SURFSUR3
              ELSEIF(J1.EQ.I4.AND.J2.EQ.I1) THEN
                ALFA(IL1)=ALFA(IL1)+W(I1)*SURFSUR3
                ALFA(IL2)=ALFA(IL2)-W(I1)*SURFSUR3
              ELSEIF(J1.EQ.I2.AND.J2.EQ.I5) THEN
                ALFA(IL1)=ALFA(IL1)-W(I2)*SURFSUR3
                ALFA(IL2)=ALFA(IL2)+W(I2)*SURFSUR3
              ELSEIF(J1.EQ.I5.AND.J2.EQ.I2) THEN
                ALFA(IL1)=ALFA(IL1)+W(I2)*SURFSUR3
                ALFA(IL2)=ALFA(IL2)-W(I2)*SURFSUR3
              ELSEIF(J1.EQ.I3.AND.J2.EQ.I6) THEN
                ALFA(IL1)=ALFA(IL1)-W(I3)*SURFSUR3
                ALFA(IL2)=ALFA(IL2)+W(I3)*SURFSUR3
              ELSEIF(J1.EQ.I6.AND.J2.EQ.I3) THEN
                ALFA(IL1)=ALFA(IL1)+W(I3)*SURFSUR3
                ALFA(IL2)=ALFA(IL2)-W(I3)*SURFSUR3
              ENDIF
            ENDDO
!
!           N-SCHEME (SEE REFERENCE 2 PAGE 3, BOTTOM RIGHT)
!
            SUMMAXK=MAX(0.D0,ALFA(1))
     &             +MAX(0.D0,ALFA(2))
     &             +MAX(0.D0,ALFA(3))
     &             +MAX(0.D0,ALFA(4))
            IF(SUMMAXK.GT.1.D-10) THEN
              SUMMAXK=-1.D0/SUMMAXK
              L12=SUMMAXK*MAX(0.D0,ALFA(1))*MIN(0.D0,ALFA(2))
              L13=SUMMAXK*MAX(0.D0,ALFA(1))*MIN(0.D0,ALFA(3))
              L14=SUMMAXK*MAX(0.D0,ALFA(1))*MIN(0.D0,ALFA(4))
              L23=SUMMAXK*MAX(0.D0,ALFA(2))*MIN(0.D0,ALFA(3))
              L24=SUMMAXK*MAX(0.D0,ALFA(2))*MIN(0.D0,ALFA(4))
              L34=SUMMAXK*MAX(0.D0,ALFA(3))*MIN(0.D0,ALFA(4))
              L21=SUMMAXK*MAX(0.D0,ALFA(2))*MIN(0.D0,ALFA(1))
              L31=SUMMAXK*MAX(0.D0,ALFA(3))*MIN(0.D0,ALFA(1))
              L41=SUMMAXK*MAX(0.D0,ALFA(4))*MIN(0.D0,ALFA(1))
              L32=SUMMAXK*MAX(0.D0,ALFA(3))*MIN(0.D0,ALFA(2))
              L42=SUMMAXK*MAX(0.D0,ALFA(4))*MIN(0.D0,ALFA(2))
              L43=SUMMAXK*MAX(0.D0,ALFA(4))*MIN(0.D0,ALFA(3))
            ELSE
!             A SIMPLE SOLUTION, WITHOUT DIVISION
!             PRINCIPLE : POINTS 1, 2, 3 GIVE THEIR CONTRIBUTION
!             TO POINT 4, IT WORKS BECAUSE ALFA(4)=-ALFA(1)-ALFA(2)-ALFA(3)
!             BUT IT IS NOT THE N-SCHEME
!             BETTER THAN PUTTING 0 FOR MASS ERROR ?
!             COULD BE USELESS DEPENDING ON FLUX CLIPPING AFTER
              L12 = 0.D0
              L13 = 0.D0
              L14 = MAX(ALFA(1),0.D0)
              L23 = 0.D0
              L24 = MAX(ALFA(2),0.D0)
              L34 = MAX(ALFA(3),0.D0)
              L21 = 0.D0
              L31 = 0.D0
              L41 = - MIN(ALFA(1),0.D0)
              L32 = 0.D0
              L42 = - MIN(ALFA(2),0.D0)
              L43 = - MIN(ALFA(3),0.D0)
            ENDIF
!
            XM(01,IELEM3D) = L12
            XM(02,IELEM3D) = L13
            XM(03,IELEM3D) = L14
            XM(04,IELEM3D) = L23
            XM(05,IELEM3D) = L24
            XM(06,IELEM3D) = L34
            XM(07,IELEM3D) = L21
            XM(08,IELEM3D) = L31
            XM(09,IELEM3D) = L41
            XM(10,IELEM3D) = L32
            XM(11,IELEM3D) = L42
            XM(12,IELEM3D) = L43
!
          ENDDO
!
!         END OF LOOP ON THE 3 TETRAHEDRONS
!
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE SUM OF EACH ROW (WITH A - SIGN)
!     THE DIAGONAL TERMS ARE 0
!
      IF(LEGO) THEN
!
        DO IELEM = 1,NELEM
          T(IELEM,1) = -XM(01,IELEM)-XM(02,IELEM)-XM(03,IELEM)
          T(IELEM,2) = -XM(04,IELEM)-XM(05,IELEM)-XM(07,IELEM)
          T(IELEM,3) = -XM(06,IELEM)-XM(08,IELEM)-XM(10,IELEM)
          T(IELEM,4) = -XM(09,IELEM)-XM(11,IELEM)-XM(12,IELEM)
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
