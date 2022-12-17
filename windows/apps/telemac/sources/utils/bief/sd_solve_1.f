!                   *********************
                    SUBROUTINE SD_SOLVE_1
!                   *********************
!
     &(NPOIN,NSEGB,GLOSEG,MAXSEG,DA,XA,XINC,RHS,INFOGR,TYPEXT)
!
!***********************************************************************
! BIEF   V7P0                                    21/07/2011
!***********************************************************************
!
!brief    DIRECT RESOLUTION OF A SYMMETRICAL LINEAR SYSTEM WITH
!+                MINIMUM DEGREE PERMUTATION AND LDLT DECOMPOSITION.
!+
!+            FROM SEGMENT STORAGE TO COMPACT STORAGE (MORSE).
!code
!+ IMPORTANT NOTE: INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
!+
!+
!+               YALE SPARSE MATRIX PACKAGE - NONSYMMETRIC CODES
!+                    SOLVING THE SYSTEM OF EQUATIONS MX = B
!+                        (UNCOMPRESSED POINTER STORAGE)
!+
!+    I.   CALLING SEQUENCES
!+         THE COEFFICIENT MATRIX CAN BE PROCESSED BY AN ORDERING ROUTINE
!+    (E.G., TO REDUCE FILLIN OR ENSURE NUMERICAL STABILITY) BEFORE USING
!+    THE REMAINING SUBROUTINES.  IF NO REORDERING IS DONE, THEN SET
!+    R(I) = C(I) = IC(I) = I  FOR I=1,...,N.  THE CALLING SEQUENCE IS --
!+        (      (MATRIX ORDERING))
!+         NSF   (SYMBOLIC FACTORIZATION TO DETERMINE WHERE FILLIN WILL
!+                 OCCUR DURING NUMERIC FACTORIZATION)
!+         NNF   (NUMERIC FACTORIZATION INTO PRODUCT LDU OF UNIT LOWER
!+                 TRIANGULAR MATRIX L, DIAGONAL MATRIX D, AND UNIT UPPER
!+                 TRIANGULAR MATRIX U, AND SOLUTION OF LINEAR SYSTEM)
!+         NNS   (SOLUTION OF LINEAR SYSTEM FOR ADDITIONAL RIGHT-HAND
!+     OR          SIDE USING LDU FACTORIZATION FROM NNF)
!+         NNT   (SOLUTION OF TRANSPOSED LINEAR SYSTEM FOR ADDITIONAL
!+                 RIGHT-HAND SIDE USING LDU FACTORIZATION FROM NNF)
!+
!+    II.  STORAGE OF SPARSE MATRICES
!+         THE NONZERO ENTRIES OF THE COEFFICIENT MATRIX M ARE STORED
!+    ROW-BY-ROW IN_SS1 THE ARRAY A.  TO IDENTIFY THE INDIVIDUAL NONZERO
!+    ENTRIES IN_SS1 EACH ROW, WE NEED TO KNOW IN_SS1 WHICH COLUMN EACH ENTRY
!+    LIES.  THE COLUMN INDICES WHICH CORRESPOND TO THE NONZERO ENTRIES
!+    OF M ARE STORED IN_SS1 THE ARRAY JA;  I.E., IF  A(K) = M(I,J),  THEN
!+    JA(K) = J.  IN_SS1 ADDITION, WE NEED TO KNOW WHERE EACH ROW STARTS AND
!+    HOW LONG IT IS.  THE INDEX POSITIONS IN_SS1 JA AND A WHERE THE ROWS OF
!+    M BEGIN ARE STORED IN_SS1 THE ARRAY IA;  I.E., IF M(I,J) IS THE FIRST
!+    NONZERO ENTRY (STORED) IN_SS1 THE I-TH ROW AND A(K) = M(I,J),  THEN
!+    IA(I) = K.  MOREOVER, THE INDEX IN_SS1 JA AND A OF THE FIRST LOCATION
!+    FOLLOWING THE LAST ELEMENT IN_SS1 THE LAST ROW IS STORED IN_SS1 IA(N+1).
!+    THUS, THE NUMBER OF ENTRIES IN_SS1 THE I-TH ROW IS GIVEN BY
!+    IA(I+1) - IA(I),  THE NONZERO ENTRIES OF THE I-TH ROW ARE STORED
!+    CONSECUTIVELY IN_SS1
!+            A(IA(I)),  A(IA(I)+1),  ..., A(IA(I+1)-1),
!+    AND THE CORRESPONDING COLUMN INDICES ARE STORED CONSECUTIVELY IN_SS1
!+            JA(IA(I)), JA(IA(I)+1), ..., JA(IA(I+1)-1).
!+    FOR EXAMPLE, THE 5 BY 5 MATRIX
!+                ( 1. 0. 2. 0. 0.)
!+                ( 0. 3. 0. 0. 0.)
!+            M = ( 0. 4. 5. 6. 0.)
!+                ( 0. 0. 0. 7. 0.)
!+                ( 0. 0. 0. 8. 9.)
!+    WOULD BE STORED AS
!+               \ 1  2  3  4  5  6  7  8  9
!+            ---+--------------------------
!+            IA \ 1  3  4  7  8 10
!+            JA \ 1  3  2  2  3  4  4  4  5
!+             A \ 1. 2. 3. 4. 5. 6. 7. 8. 9.         .
!+
!+         THE STRICT TRIANGULAR PORTIONS OF THE MATRICES L AND U ARE
!+    STORED IN_SS1 THE SAME FASHION USING THE ARRAYS  IL, JL, L  AND
!+    IU, JU, U  RESPECTIVELY.  THE DIAGONAL ENTRIES OF L AND U ARE
!+    ASSUMED TO BE EQUAL TO ONE AND ARE NOT STORED.  THE ARRAY D
!+    CONTAINS THE RECIPROCALS OF THE DIAGONAL ENTRIES OF THE MATRIX D.
!+
!+    III. ADDITIONAL STORAGE SAVINGS
!+         IN_SS1 NSF, R AND IC CAN BE THE SAME ARRAY IN_SS1 THE CALLING
!+    SEQUENCE IF NO REORDERING OF THE COEFFICIENT MATRIX HAS BEEN DONE.
!+         IN_SS1 NNF, R, C AND IC CAN ALL BE THE SAME ARRAY IF NO REORDERING
!+    HAS BEEN DONE.  IF ONLY THE ROWS HAVE BEEN REORDERED, THEN C AND IC
!+    CAN BE THE SAME ARRAY.  IF THE ROW AND COLUMN ORDERINGS ARE THE
!+    SAME, THEN R AND C CAN BE THE SAME ARRAY.  Z AND ROW CAN BE THE
!+    SAME ARRAY.
!+         IN_SS1 NNS OR NNT, R AND C CAN BE THE SAME ARRAY IF NO REORDERING
!+    HAS BEEN DONE OR IF THE ROW AND COLUMN ORDERINGS ARE THE SAME.  Z
!+    AND B CAN BE THE SAME ARRAY;  HOWEVER, THEN B WILL BE DESTROYED.
!+
!+    IV.  PARAMETERS
!+         FOLLOWING IS A LIST OF PARAMETERS TO THE PROGRAMS.  NAMES ARE
!+    UNIFORM AMONG THE VARIOUS SUBROUTINES.  CLASS ABBREVIATIONS ARE --
!+       N - INTEGER VARIABLE
!+       F - REAL VARIABLE
!+       V - SUPPLIES A VALUE TO A SUBROUTINE
!+       R - RETURNS A RESULT FROM A SUBROUTINE
!+       I - USED INTERNALLY BY A SUBROUTINE
!+       A - ARRAY
!+
!+ CLASS \ PARAMETER
!+ ------+----------
!+ FVA   \ A     - NONZERO ENTRIES OF THE COEFFICIENT MATRIX M, STORED
!+       \           BY ROWS.
!+       \           SIZE = NUMBER OF NONZERO ENTRIES IN_SS1 M.
!+ FVA   \ B     - RIGHT-HAND SIDE B.
!+       \           SIZE = N.
!+ NVA   \ C     - ORDERING OF THE COLUMNS OF M.
!+       \           SIZE = N.
!+ FVRA  \ D     - RECIPROCALS OF THE DIAGONAL ENTRIES OF THE MATRIX D.
!+       \           SIZE = N.
!+ NR    \ FLAG  - ERROR FLAG;  VALUES AND THEIR MEANINGS ARE --
!+       \            0     NO ERRORS DETECTED
!+       \            N+K   NULL ROW IN_SS1 A  --  ROW = K
!+       \           2N+K   DUPLICATE ENTRY IN_SS1 A  --  ROW = K
!+       \           3N+K   INSUFFICIENT STORAGE FOR JL  --  ROW = K
!+       \           4N+1   INSUFFICIENT STORAGE FOR L
!+       \           5N+K   NULL PIVOT  --  ROW = K
!+       \           6N+K   INSUFFICIENT STORAGE FOR JU  --  ROW = K
!+       \           7N+1   INSUFFICIENT STORAGE FOR U
!+       \           8N+K   ZERO PIVOT  --  ROW = K
!+ NVA   \ IA    - POINTERS TO DELIMIT THE ROWS IN_SS1 A.
!+       \           SIZE = N+1.
!+ NVA   \ IC    - INVERSE OF THE ORDERING OF THE COLUMNS OF M;  I.E.,
!+       \           IC(C(I) = I  FOR I=1,...N.
!+       \           SIZE = N.
!+ NVRA  \ IL    - POINTERS TO DELIMIT THE ROWS IN_SS1 L.
!+       \           SIZE = N+1.
!+ NVRA  \ IU    - POINTERS TO DELIMIT THE ROWS IN_SS1 U.
!+       \           SIZE = N+1.
!+ NVA   \ JA    - COLUMN NUMBERS CORRESPONDING TO THE ELEMENTS OF A.
!+       \           SIZE = SIZE OF A.
!+ NVRA  \ JL    - COLUMN NUMBERS CORRESPONDING TO THE ELEMENTS OF L.
!+       \           SIZE = JLMAX.
!+ NV    \ JLMAX - DECLARED DIMENSION OF JL;  JLMAX MUST BE LARGER THAN
!+       \           THE NUMBER OF NONZERO ENTRIES IN_SS1 THE STRICT LOWER
!+       \           TRIANGLE OF M PLUS FILLIN  (IL(N+1)-1 AFTER NSF).
!+ NVRA  \ JU    - COLUMN NUMBERS CORRESPONDING TO THE ELEMENTS OF U.
!+       \           SIZE = JUMAX.
!+ NV    \ JUMAX - DECLARED DIMENSION OF JU;  JUMAX MUST BE LARGER THAN
!+       \           THE NUMBER OF NONZERO ENTRIES IN_SS1 THE STRICT UPPER
!+       \           TRIANGLE OF M PLUS FILLIN  (IU(N+1)-1 AFTER NSF).
!+ FVRA  \ L     - NONZERO ENTRIES IN_SS1 THE STRICT LOWER TRIANGULAR PORTION
!+       \           OF THE MATRIX L, STORED BY ROWS.
!+       \           SIZE = LMAX
!+ NV    \ LMAX  - DECLARED DIMENSION OF L;  LMAX MUST BE LARGER THAN
!+       \           THE NUMBER OF NONZERO ENTRIES IN_SS1 THE STRICT LOWER
!+       \           TRIANGLE OF M PLUS FILLIN  (IL(N+1)-1 AFTER NSF).
!+ NV    \ N     - NUMBER OF VARIABLES/EQUATIONS.
!+ NVA   \ R     - ORDERING OF THE ROWS OF M.
!+       \           SIZE = N.
!+ FVRA  \ U     - NONZERO ENTRIES IN_SS1 THE STRICT UPPER TRIANGULAR PORTION
!+       \           OF THE MATRIX U, STORED BY ROWS.
!+       \           SIZE = UMAX.
!+ NV    \ UMAX  - DECLARED DIMENSION OF U;  UMAX MUST BE LARGER THAN
!+       \           THE NUMBER OF NONZERO ENTRIES IN_SS1 THE STRICT UPPER
!+       \           TRIANGLE OF M PLUS FILLIN  (IU(N+1)-1 AFTER NSF).
!+ FRA   \ Z     - SOLUTION X.
!+       \           SIZE = N.
!
!history  E. RAZAFINDRAKOTO (LNH)
!+        20/11/06
!+        V5P7
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
!+        23/08/2012
!+        V6P2
!+   Size of ISP_SS1 doubled IN_SS1 non symmetric cases.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        18/04/2014
!+        V7P0
!+   Checking that 2*NSP is less than HUGE(1). Meshes with about
!+   2 millions of points will trigger memory allocations of numbers
!+   greater than the largest I4 integer.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DA             |-->| MATRIX DIAGONAL COEFFICIENTS
!| XA             |-->| OFF-DIAGONAL TERM OF MATRIX
!| GLOSEG         |-->| GLOBAL NUMBER OF SEGMENTS OF THE MATRIX
!| INFOGR         |-->| IF, YES INFORMATIONS ON LISTING
!| MAXSEG         |-->| MAXIMUM NUMBER OF SEGMENTS
!| NPOIN          |-->| NUMBER OF UNKNOWN
!| NSEGB          |-->| NUMBER OF SEGMENTS
!| RHS            |-->| SECOND MEMBER OF LINEAR EQUATION
!| TYPEXT         |---| = 'S' : SYMETRIC MATRIX
!| XINC           |<--| SOLUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_SOLVE_1 => SD_SOLVE_1
      USE DECLARATIONS_TELEMAC, ONLY : IN_SS1,IP_SS1,ISEGIP_SS1,IW1_SS1,
     &                                 INDTRI_SS1,INX_SS1,AC_SS1,
     &                                 ACTRI_SS1,ISP_SS1,RSP_SS1,
     &                                 INDTRI_SS1,SIZE_IN,SIZE_IP,
     &                                 SIZE_ISEGIP,SIZE_IW1,SIZE_INDTRI,
     &                                 SIZE_INX,SIZE_AC,SIZE_ACTRI,
     &                                 SIZE_ISP,SIZE_RSP,SIZE_IPX,
     &                                 IPX_SS1
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NSEGB,MAXSEG
      INTEGER, INTENT(IN)             :: GLOSEG(MAXSEG,2)
      LOGICAL, INTENT(IN)             :: INFOGR
      DOUBLE PRECISION, INTENT(IN)    :: XA(*),RHS(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: XINC(NPOIN),DA(NPOIN)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER MEMFACTOR,IERR,NPBLK,NSEGBLK,NSP,ESP,INDIC,FLAG,I,IERRK
!
!
!
!-----------------------------------------------------------------------
!
!     CORRECTS DIAGONALS (TIDAL FLATS WITH MASKING)
!
      DO I=1,NPOIN
        IF(ABS(DA(I)).LT.1.D-15) DA(I)=1.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(INFOGR) THEN
        WRITE(LU,*) '                      DIRECT SYSTEM SOLVER'
      ENDIF
!
      NPBLK=NPOIN
      NSEGBLK=NSEGB
!
!     1. MEMFACTOR: MEMORY FACTOR FOR SIZE ISP_SS1 AND RSP_SS1 IN_SS1 ODRV AND SDRV
!     =======================================================================
!
      IF(TYPEXT.EQ.'S') THEN
        MEMFACTOR = 5
      ELSE
        MEMFACTOR = 15
      ENDIF
      NSP=MEMFACTOR*(NPBLK+4*NSEGBLK)
      ESP=MEMFACTOR*(NPBLK+4*NSEGBLK)
!
!     2. ALLOCATES ARRAYS (OR REALLOCATES IF TOO SMALL)
!     =======================================================================
!
      IF(SIZE_IN.EQ.0) THEN
        ALLOCATE(IN_SS1(NPBLK+1))
        SIZE_IN=    NPBLK+1
      ELSEIF(       NPBLK+1.GT.SIZE_IN) THEN
        DEALLOCATE(IN_SS1)
        ALLOCATE(IN_SS1(NPBLK+1))
        SIZE_IN=    NPBLK+1
      ENDIF
!
      IF(SIZE_IP.EQ.0) THEN
        ALLOCATE(IP_SS1(NSEGBLK*2))
        SIZE_IP=    NSEGBLK*2
      ELSEIF(       NSEGBLK*2.GT.SIZE_IP) THEN
        DEALLOCATE(IP_SS1)
        ALLOCATE(IP_SS1(NSEGBLK*2))
        SIZE_IP=    NSEGBLK*2
      ENDIF
!
      IF(SIZE_ISEGIP.EQ.0) THEN
        ALLOCATE(ISEGIP_SS1(NSEGBLK*2+1))
        SIZE_ISEGIP=    NSEGBLK*2+1
      ELSEIF(           NSEGBLK*2+1.GT.SIZE_ISEGIP) THEN
        DEALLOCATE(ISEGIP_SS1)
        ALLOCATE(ISEGIP_SS1(NSEGBLK*2+1))
        SIZE_ISEGIP=    NSEGBLK*2+1
      ENDIF
!
      IF(SIZE_IW1.EQ.0) THEN
        ALLOCATE(IW1_SS1(NPBLK))
        SIZE_IW1=    NPBLK
      ELSEIF(        NPBLK.GT.SIZE_IW1) THEN
        DEALLOCATE(IW1_SS1)
        ALLOCATE(IW1_SS1(NPBLK))
        SIZE_IW1=    NPBLK
      ENDIF
!
      IF(SIZE_INDTRI.EQ.0) THEN
        ALLOCATE(INDTRI_SS1(NPBLK))
        SIZE_INDTRI=    NPBLK
      ELSEIF(           NPBLK.GT.SIZE_INDTRI) THEN
        DEALLOCATE(INDTRI_SS1)
        ALLOCATE(INDTRI_SS1(NPBLK))
        SIZE_INDTRI=    NPBLK
      ENDIF
!
      IF(SIZE_INX.EQ.0) THEN
        ALLOCATE(INX_SS1(NPBLK+1))
        SIZE_INX=    NPBLK+1
      ELSEIF(        NPBLK+1.GT.SIZE_INX) THEN
        DEALLOCATE(INX_SS1)
        ALLOCATE(INX_SS1(NPBLK+1))
        SIZE_INX=    NPBLK+1
      ENDIF
!
      IF(SIZE_IPX.EQ.0) THEN
        ALLOCATE(IPX_SS1(NSEGBLK*2+NPBLK+1))
        SIZE_IPX=    NSEGBLK*2+NPBLK+1
      ELSEIF(        NSEGBLK*2+NPBLK+1.GT.SIZE_IPX) THEN
        DEALLOCATE(IPX_SS1)
        ALLOCATE(IPX_SS1(NSEGBLK*2+NPBLK+1))
        SIZE_IPX=    NSEGBLK*2+NPBLK+1
      ENDIF
!
      IF(SIZE_AC.EQ.0) THEN
        ALLOCATE(AC_SS1(NSEGBLK*2+NPBLK+1))
        SIZE_AC=    NSEGBLK*2+NPBLK+1
      ELSEIF(       NSEGBLK*2+NPBLK+1.GT.SIZE_AC) THEN
        DEALLOCATE(AC_SS1)
        ALLOCATE(AC_SS1(NSEGBLK*2+NPBLK+1))
        SIZE_AC=    NSEGBLK*2+NPBLK+1
      ENDIF
!
      IF(SIZE_ACTRI.EQ.0) THEN
        ALLOCATE(ACTRI_SS1(NPBLK))
        SIZE_ACTRI=    NPBLK
      ELSEIF(          NPBLK.GT.SIZE_ACTRI) THEN
        DEALLOCATE(ACTRI_SS1)
        ALLOCATE(ACTRI_SS1(NPBLK))
        SIZE_ACTRI=    NPBLK
      ENDIF
!
      IF(TYPEXT.EQ.'S') THEN
!
        IF(SIZE_ISP.EQ.0) THEN
          ALLOCATE(ISP_SS1(NSP))
        ELSEIF(        NSP.GT.SIZE_ISP) THEN
          DEALLOCATE(ISP_SS1)
          ALLOCATE(ISP_SS1(NSP))
        ENDIF
        SIZE_ISP=    NSP
!
      ELSE
!
        IF(2*NSP.LT.NSP) THEN
          WRITE(LU,*) 'SIZE OF LARGEST INTEGER  ',HUGE(1)
          WRITE(LU,*) 'TRESPASSED BY 2*NSP, NSP=',NSP
          CALL PLANTE(1)
          STOP
        ENDIF
!
        IF(SIZE_ISP.EQ.0) THEN
          ALLOCATE(ISP_SS1(2*NSP))
        ELSEIF(      2*NSP.GT.SIZE_ISP) THEN
          DEALLOCATE(ISP_SS1)
          ALLOCATE(ISP_SS1(2*NSP))
        ENDIF
        SIZE_ISP=    2*NSP
!
      ENDIF
!
      IF(SIZE_RSP.EQ.0) THEN
        ALLOCATE(RSP_SS1(ESP))
        SIZE_RSP=    ESP
      ELSEIF(        ESP.GT.SIZE_RSP) THEN
        DEALLOCATE(RSP_SS1)
        ALLOCATE(RSP_SS1(ESP))
        SIZE_RSP=    ESP
      ENDIF
!
!     3. BUILDS NONSYMMETRICAL COMPACT STORAGE (IN_SS1,IP_SS1)
!     WITHOUT THE DIAGONAL AND (INX_SS1,IPX_SS1) WITH THE DIAGONAL
!     =======================================================================
!
      CALL SD_STRSSD(NPBLK,NSEGBLK,GLOSEG(1,1),GLOSEG(1,2),
     &               IN_SS1,IP_SS1,ISEGIP_SS1,IW1_SS1)
!
      IF(TYPEXT.EQ.'S') THEN
!       XA IS THE OFF-DIAGONAL TERMS AND MAY COME DIRECTLY FROM TELEMAC
!       HENCE THE LOWER TRIANGULAR PART MAY NOT BE BUILT, WE GIVE TWICE XA
!       INSTEAD OF XA,XA(NSEGBLK+1)
        CALL SD_FABCAD(NPBLK,NSEGBLK,IN_SS1,IP_SS1,ISEGIP_SS1,
     &                 INDTRI_SS1,IW1_SS1,INX_SS1,IPX_SS1,ACTRI_SS1,XA,
     &                 XA,DA,AC_SS1)
!                             ISTRI                !!
      ELSE
        CALL SD_FABCAD(NPBLK,NSEGBLK,IN_SS1,IP_SS1,ISEGIP_SS1,
     &                 INDTRI_SS1,IW1_SS1,INX_SS1,IPX_SS1,ACTRI_SS1,XA,
     &                 XA(NSEGBLK+1),DA,AC_SS1)
      ENDIF
!
!     4. MINIMUM DEGREE PERMUTATION (YSMP PACKAGE)
!     =======================================================================
!
      INDIC=1
!
      CALL SD_ODRV(NPBLK,INX_SS1,IPX_SS1,AC_SS1,IN_SS1,IW1_SS1,NSP,
     &             ISP_SS1,INDIC,FLAG)
!                                   PERM,INVP
!
      IF(FLAG.NE.0) THEN
        WRITE(LU,*) 'INCREASE THE MEMORY FACTOR (MEMFACTOR)',
     &              ' IN_SS1 THE ROUTINE SD_SOLVE_1'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!---> SECOND MEMBER OF THE SYSTEM
!
!     5. LDLT DECOMPOSITION AND RESOLUTION (YSMP PACKAGE)
!
      IF(TYPEXT.EQ.'S') THEN
!                          PERM,INVP
        CALL SD_SDRV(NPBLK,IN_SS1,IW1_SS1,INX_SS1,IPX_SS1,AC_SS1,RHS,
     &               XINC,NSP,ISP_SS1,RSP_SS1,ESP,INDIC,FLAG)
      ELSE
        CALL SD_CDRV(NPBLK,IN_SS1,IN_SS1,IW1_SS1,INX_SS1,IPX_SS1,
     &               AC_SS1,RHS,XINC,
     &               NSP,ISP_SS1,RSP_SS1,ESP,INDIC,FLAG)
      ENDIF
!
      IF(TYPEXT.EQ.'S') THEN
      IF(FLAG.NE.0) THEN
        IERR=FLAG-8*NPBLK
        IF(IERR.GT.0) THEN
          WRITE(LU,*) 'MATRIX WITH ZERO PIVOT AT ROW'
          WRITE(LU,*) IERR
        ELSE
          WRITE(LU,*) 'ADD 1 TO THE MEMORY FACTOR (MEMFACTOR)',
     &                ' IN_SS1 SUBROUTINE SD_SOLVE_1'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
      ELSE
!
!---> COMMENTS THE ERROR: FLAG_SD_NDRV:
!
! FLAG  - ERROR FLAG;  VALUES AND THEIR MEANINGS ARE --
!             0     NO ERRORS DETECTED
!             N+K   NULL ROW IN_SS1 A  --  ROW = K
!            2N+K   DUPLICATE ENTRY IN_SS1 A  --  ROW = K
!            3N+K   INSUFFICIENT STORAGE IN_SS1 NSF  --  ROW = K
!            4N+1   INSUFFICIENT STORAGE IN_SS1 NNF
!            5N+K   NULL PIVOT  --  ROW = K
!            6N+K   INSUFFICIENT STORAGE IN_SS1 NSF  --  ROW = K
!            7N+1   INSUFFICIENT STORAGE IN_SS1 NNF
!            8N+K   ZERO PIVOT  --  ROW = K
!           10N+1   INSUFFICIENT STORAGE IN_SS1 NDRV
!           11N+1   ILLEGAL PATH SPECIFICATION (INDIC)
      IF(FLAG.NE.0) THEN
        IERR=INT(FLAG/NPBLK)
        IF(IERR.EQ.3.OR.IERR.EQ.5.OR.IERR.EQ.8) THEN
          IERRK=FLAG-IERR*NPBLK
          WRITE(LU,*) 'MATRIX WITH ZERO PIVOT AT ROW'
          WRITE(LU,*) IERRK
        ELSE
          WRITE(LU,*) 'INCREASE THE MEMORY FACTOR (MEMFACTOR)',
     &                ' IN_SS1 SUBROUTINE SD_SOLVE_1'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
