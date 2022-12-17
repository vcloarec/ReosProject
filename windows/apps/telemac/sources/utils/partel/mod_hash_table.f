!                       *********************
                        MODULE MOD_HASH_TABLE
!                       *********************
!
!***********************************************************************
! PARTEL
!***********************************************************************
!
!BRIEF    Functions to handle hash_table for partel
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      !THIS HASH TABLE IS SPECIFICALY MADE FOR PARTEL AND IS ABSOLUTELY
      !NOT GENERIC.
      !THIS HASH TABLE PROVIDE INSERTION AND SEARCH BUT NO DELETION
      !
      !THE IMPLEMENTATION USE OPEN ADDRESSING WITH LINEAR PROBING FOR
      !COLLISION RESOLUTION
!
      INTEGER, PARAMETER :: UNUSED = -1
!
      !THIS TYPE IS THE ELEMENT CONTAINED BY THE HASH TABLE.
      !THE X AND Y REPRESENTS THE INDICES OF THE ARRAYS PARTEL USED
      !PREVIOUSLY. SO FROM THE USER POINT OF VIEW, X AND Y ARE ALWAYS
      !POSITIVE BUT INTERNALY THE HASH TABLE USE NEGATIVE VALUE TO
      !REPRESENTS AN UNUSED ELEMENT, WHICH MEANS THAHT THE HASH TABLE
      !IS FREE TO INSERT AT THIS ELEMENT
      !
      !V CAN BE POSITIVE OR NEGATIVE
      TYPE HASH_TABLE_EL
        INTEGER :: X=UNUSED
        INTEGER :: Y, V
      END TYPE
!
      TYPE HASH_TABLE
        TYPE(HASH_TABLE_EL), ALLOCATABLE :: TABLE(:)
!
        !THE NUMBER OF INSERTED ELEMENTS IN THE HASH TABLE
        INTEGER :: NELTS = 0
!
        !THE DEFAULT VALUE RETURNED WHEN THE USER SEARCH FOR A
        !VALUE WHICH IS NOT CONTAINED BY THE HASH TABLE
        INTEGER :: DEFAULT_VALUE = 0
!
        !THE MAXIMUM NUMBER OF CONSECUTIVE ELEMENTS WE HAVE TO
        !BROWSE WHEN SEARCHING FOR A VALUE
        INTEGER :: LONGUEST_PROBE = 0
!
        !WHEN THE RATIO 'NO. OF ELTS / SIZE OF THE HASH TABLE' IS
        !SUPERIOR TO MAX_LOAD_FACTOR, THE TABLE WILL GROW
        REAL :: MAX_LOAD_FACTOR = 0.75
      END TYPE HASH_TABLE
!
      CONTAINS
!***********************************************************************
      SUBROUTINE HASH_TABLE_CREATE
!***********************************************************************
!
     &(HT, TABLE_SIZE)
!
!***********************************************************************
! PARTEL                                                      27/02/2018
!***********************************************************************
!
!brief    The newly created hash table will have at least 1M elements.
!         If the user ask for a size which is not a power of two, the
!         size will be rounded to the nearest superior power of two
!
!history  Judicaël Grasset (Daresbury Lab & EDF)
!+        27/02/2018
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HT         |<->| Hash table
!| TABLE_SIZE |-->| The required size
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        TYPE(HASH_TABLE), INTENT(INOUT) :: HT
        INTEGER, INTENT(IN) :: TABLE_SIZE
        INTEGER :: IERR, P2
!
        P2 = NEAREST_SUPERIOR_POWER_2(TABLE_SIZE)
        ALLOCATE(HT%TABLE(P2), STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'HASH TABLE CREATION')
      END SUBROUTINE
!
!***********************************************************************
      SUBROUTINE HASH_TABLE_DESTROY
!***********************************************************************
!
     &(HT)
!
!***********************************************************************
! PARTEL                                                      27/02/2018
!***********************************************************************
!
!brief    Destroy a hash table and free his memory.
!
!history  Judicaël Grasset (Daresbury Lab & EDF)
!+        27/02/2018
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HT         |<->| Hash table to destroy
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        TYPE(HASH_TABLE), INTENT(INOUT) :: HT
!
        HT%NELTS=-1
        HT%DEFAULT_VALUE=-1
        HT%LONGUEST_PROBE=-1
        IF(ALLOCATED(HT%TABLE)) DEALLOCATE(HT%TABLE)
      END SUBROUTINE
!***********************************************************************
      PURE FUNCTION NEAREST_SUPERIOR_POWER_2
!***********************************************************************
!
     &(X) RESULT(P2)
!
!***********************************************************************
! PARTEL                                                      27/02/2018
!***********************************************************************
!
!brief    Compute the nearest power of two of X which is at least 2**20
!
!history  Judicaël Grasset (Daresbury Lab & EDF)
!+        27/02/2018
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| X     |-->| Minimum required by user
!| P2    |<--| Nearest power of two found (at least 2**20)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        INTEGER, INTENT(IN) :: X
        INTEGER :: P2
        !WILL SET THE DEFAULT SIZE OF THE HASHTABLE TO
        !1MIB OF ELEMENTS
        P2 = 2**20
!
        DO WHILE(P2 < X)
          P2 = P2*2
        END DO
      END FUNCTION
!***********************************************************************
      PURE FUNCTION ELEGANT_PAIRING
!***********************************************************************
!
     &(X,Y) RESULT(PAIRING)
!
!***********************************************************************
! PARTEL                                                      27/02/2018
!***********************************************************************
!
!brief    Elegant pairing,algorithm by matthew szudzik(wolfram research)
!
!history  Judicaël Grasset (Daresbury Lab & EDF)
!+        27/02/2018
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| X       |-->| First number to paired
!| Y       |-->| Second Number to paired
!| PAIRING |<--| Pairing of X and Y
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        INTEGER, INTENT(IN) :: X,Y
        INTEGER(KIND=K8) :: PAIRING

        IF (X >= Y) THEN
          PAIRING = INT(X, KIND=K8) * X + X + Y
        ELSE
          PAIRING = Y * Y + X
        END IF
      END FUNCTION ELEGANT_PAIRING
!
!***********************************************************************
      PURE FUNCTION HASH
!***********************************************************************
!
     &(K, TABLE_SIZE) RESULT(H)
!
!***********************************************************************
! PARTEL                                                      27/02/2018
!***********************************************************************
!
!brief    Hide the real hash function from the user
!
!history  Judicaël Grasset (Daresbury Lab & EDF)
!+        27/02/2018
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| K          |-->| Key to hash
!| TABLE_SIZE |-->| Size of the hash table
!| H          |<--| Computed hash
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        INTEGER(KIND=K8), INTENT(IN) :: K
        INTEGER, INTENT(IN) :: TABLE_SIZE
        INTEGER :: H
!
        !ADD ONE TO BE IN THE RANGE OF FORTRAN ARRAY (1:N)
        H=FINGERPRINT(K,INT(TABLE_SIZE, K8))+1
      END FUNCTION HASH
!
!***********************************************************************
      PURE FUNCTION FINGERPRINT
!***********************************************************************
!
     &(K, S) RESULT(H)
!
!***********************************************************************
! PARTEL                                                      27/02/2018
!***********************************************************************
!
!brief    Fingerprint is a sightly modified version of the fingerprint
!         from the farmhash(v1.1) framework of google (mit licence)
!         https://github.com/google/farmhash/blob/master/src/farmhash.h
!
!history  Judicaël Grasset (Daresbury Lab & EDF)
!+        27/02/2018
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| K   |-->| Key to hash
!| S   |-->| Size of the hash table
!| H   |<--| Computed hash
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        INTEGER(KIND=K8), INTENT(IN) :: K
        INTEGER(KIND=K8), INTENT(IN) :: S
        !MAGIC NUMBER FROM MURMURHASH 3
        INTEGER(KIND=K8)::KMUL
        INTEGER(KIND=K8) :: B
        INTEGER :: H
#ifdef NAGFOR
        !THIS CONSTANT COMES FROM KNUTH,
        !THE ART OF COMPUTER PROGRAMMING,
        !VOL.3, SORTING AND SEARCHING, 1973
        DOUBLE PRECISION, PARAMETER :: A=(2.2360679774D0-1D0)/2D0
!
        H=FLOOR(DBLE(S) * DMOD(DBLE(K)*A,1D0))
        H=ABS(H)
#else
!
        KMUL=INT(Z'CC9E2D51', KIND=K8)
        B = K*KMUL
        B = IEOR(B, ISHFT(B,44))
        B = B*KMUL
        B = IEOR(B, ISHFT(B,41))
        B = B*KMUL
!
        !IN THE ORIGINAL FINGERPRINT FROM HASHFARM, ONLY UNSIGNED
        !INTEGER WERE USED, SO WHEN THE INT OVERFLOW IT WAS
        !POSITIVE, BUT WE DO NOT HAVE UNSIGNED IN FORTRAN
        B = ABS(B)
!
        !WE CAN DO THE MODULO BY BITSHIFT BECAUSE WE KNOW THE SIZE
        !IS A POWER OF TWO. DOING THIS WAY SHOULD BE REALLY FASTER
        !THAN USING THE MODULO
        !B = MOD(B,SK16)
        B = IAND(B, S-1)
!
        !B CAN BE CASTED TO A SMALLER INTEGER SINCE IT HAVE
        !BEEN REDUCED BY A MODULO OF THE HASHTABLE SIZE, WHICH IS
        !ALWAYS CONTAINED INTO AN INT
        H = INT(B)
#endif
      END FUNCTION
!***********************************************************************
      RECURSIVE SUBROUTINE HASH_TABLE_INSERT
!***********************************************************************
!
     &(HT, X, Y, V)
!
!***********************************************************************
! PARTEL                                                      27/02/2018
!***********************************************************************
!
!brief    Insert an element into the hash table. Will make the hash to
!         table grow if necessary (See max_load_factor).
!
!history  Judicaël Grasset (Daresbury Lab & EDF)
!+        27/02/2018
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HT  |<->| Hash table to insert into
!| X   |-->| X coordinate of the element
!| Y   |-->| Y coordinate of the element
!| V   |-->| Value to insert
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        TYPE(HASH_TABLE), INTENT(INOUT) :: HT
        INTEGER, INTENT(IN) :: X,Y,V
        INTEGER :: I, PROBE_SIZE
        INTEGER(KIND=K8) :: K

        IF(X < 0 .OR. Y < 0) THEN
          WRITE(LU,*)'X AND Y MUST BE POSITIVE'
          CALL PLANTE(1,'MOD_HASH_TABLE')
        END IF
!
        !CHECK IF IT'S TIME TO GROW
        IF(REAL(HT%NELTS)/SIZE(HT%TABLE)>HT%MAX_LOAD_FACTOR) THEN
          CALL HASH_TABLE_GROW(HT)
        END IF
!
        !TRANSFORM THE COORDINATES INTO A SINGLE INTEGER
        K = ELEGANT_PAIRING(X,Y)
!
        I = HASH(K, SIZE(HT%TABLE))
!
        PROBE_SIZE = 0
!
        DO WHILE (HT%TABLE(I)%X /= UNUSED)
!
          !TEST IF IT'S A MODIFICATION OF AN ALREADY EXISTING ELT
          IF(HT%TABLE(I)%X == X .AND. HT%TABLE(I)%Y == Y)EXIT
!
          I = I+1
          PROBE_SIZE = PROBE_SIZE+1
!
          !IF WE ARE AT THE END OF THE TABLE, CONTINUE TO SEARCH
          !FROM THE BEGINING
          IF(I>SIZE(HT%TABLE)) I=1
        END DO
!
        HT%TABLE(I)%X = X
        HT%TABLE(I)%Y = Y
        HT%TABLE(I)%V = V
!
        IF(HT%LONGUEST_PROBE < PROBE_SIZE) THEN
          HT%LONGUEST_PROBE=PROBE_SIZE
        END IF
!
        HT%NELTS = HT%NELTS+1
      END SUBROUTINE
!***********************************************************************
      FUNCTION HASH_TABLE_GET
!***********************************************************************
!
     &(HT, X, Y) RESULT(V)
!
!***********************************************************************
! PARTEL                                                      27/02/2018
!***********************************************************************
!
!brief    Return the value contained in the couple (x,y), will return
!         default_value if the couple is not found
!
!history  Judicaël Grasset (Daresbury Lab & EDF)
!+        27/02/2018
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HT  |-->| Hash table to insert into
!| X   |-->| X coordinate of the element
!| Y   |-->| Y coordinate of the element
!| V   |<--| Retrieved value
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        TYPE(HASH_TABLE), INTENT(IN) :: HT
        INTEGER, INTENT(IN) :: X, Y
        INTEGER :: I, V
        INTEGER(KIND=K8) :: K

        IF(X < 0 .OR. Y < 0) THEN
          WRITE(LU,*)'X AND Y MUST BE POSITIVE'
          CALL PLANTE(1,'MOD_HASH_TABLE')
        END IF
!
        V = HT%DEFAULT_VALUE
!
        K = ELEGANT_PAIRING(X,Y)
        I = HASH(K, SIZE(HT%TABLE))
!
        DO WHILE(HT%TABLE(I)%X /= UNUSED)
          IF(HT%TABLE(I)%X == X .AND. HT%TABLE(I)%Y == Y) THEN
            V = HT%TABLE(I)%V
            EXIT
          END IF
          I = I+1
          IF(I>SIZE(HT%TABLE)) I = 1
        END DO
      END FUNCTION HASH_TABLE_GET
!***********************************************************************
      SUBROUTINE HASH_TABLE_GROW
!***********************************************************************
!
     &(HT)
!
!***********************************************************************
! PARTEL                                                      27/02/2018
!***********************************************************************
!
!brief    Increase the size of the hash table, currently by doubling it
!
!history  Judicaël Grasset (Daresbury Lab & EDF)
!+        27/02/2018
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HT  |-->| Hash table to grow
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        TYPE(HASH_TABLE), INTENT(INOUT) :: HT
        TYPE(HASH_TABLE) :: NEW_HT
        INTEGER :: I
!
        CALL HASH_TABLE_CREATE(NEW_HT, SIZE(HT%TABLE)*2)
!
        DO I=1,SIZE(HT%TABLE)
          IF(HT%TABLE(I)%X /= UNUSED) THEN
            CALL HASH_TABLE_INSERT(NEW_HT,
     &        HT%TABLE(I)%X, HT%TABLE(I)%Y, HT%TABLE(I)%V)
          END IF
        END DO
!
#ifdef NAGFOR
        ALLOCATE(HT%TABLE(SIZE(NEW_HT%TABLE)))
        HT%TABLE = NEW_HT%TABLE
        DEALLOCATE(NEW_HT%TABLE)
#else
        CALL MOVE_ALLOC(NEW_HT%TABLE, HT%TABLE)
#endif
        HT%LONGUEST_PROBE = NEW_HT%LONGUEST_PROBE
        CALL HASH_TABLE_DESTROY(NEW_HT)
      END SUBROUTINE HASH_TABLE_GROW
!
!***********************************************************************
      SUBROUTINE HASH_TABLE_STAT
!***********************************************************************
!
     &(HT)
!
!***********************************************************************
! PARTEL                                                      27/02/2018
!***********************************************************************
!
!brief    Print some statistics about an hash table
!
!history  Judicaël Grasset (Daresbury Lab & EDF)
!+        27/02/2018
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HT  |-->| Hash table
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        TYPE(HASH_TABLE), INTENT(IN) :: HT
        WRITE(LU,*)'LARGEST NO. OF COLLISIONS',HT%LONGUEST_PROBE
        WRITE(LU,*)'NUMBER OF BUCKETS:',SIZE(HT%TABLE)
        WRITE(LU,*)'NUMBER OF ELEMENTS:',HT%NELTS
      END SUBROUTINE
!
!***********************************************************************
      SUBROUTINE HASH_TABLE_TEST
!***********************************************************************
!
     &()
!
!***********************************************************************
! PARTEL                                                      27/02/2018
!***********************************************************************
!
!brief    Some tests to check that the hash table implementations is
!         working. Only usefull for debugging.
!
!history  Judicaël Grasset (Daresbury Lab & EDF)
!+        27/02/2018
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HT  |-->| Hash table
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        TYPE(HASH_TABLE) :: HT
        INTEGER :: V,I
!
        CALL HASH_TABLE_CREATE(HT, 1024)
!
        !SIMPLE INSERT
        CALL HASH_TABLE_INSERT(HT, 10,10,10)
        V=HASH_TABLE_GET(HT,10,10)
        IF (V/=10) THEN
          WRITE(LU,*)'FOUND',V,'SHOULD HAVE BEEN 10'
          CALL PLANTE(1,'MOD_HASH_TABLE')
        END IF
!
        !SIMPLE INSERT WITH NEGATIVE VALUE
        CALL HASH_TABLE_INSERT(HT, 10,20,-10)
        V=HASH_TABLE_GET(HT,10,20)
        IF (V/=-10) THEN
          WRITE(LU,*)'FOUND',V,'SHOULD HAVE BEEN -10'
          CALL PLANTE(1,'MOD_HASH_TABLE')
        END IF
!
        !INSERT WITH GIANTIC X Y
        CALL HASH_TABLE_INSERT(HT, 1234567890,1234567890,42)
        V=HASH_TABLE_GET(HT,1234567890,1234567890)
        IF (V/=42) THEN
          WRITE(LU,*)'FOUND',V,'SHOULD HAVE BEEN 42'
          CALL PLANTE(1,'MOD_HASH_TABLE')
        END IF
!
        !INSERT LOT OF DATA
        DO I=100,4298500
          CALL HASH_TABLE_INSERT(HT,I,I,I)
        END DO
        DO I=100,4298500
          V=HASH_TABLE_GET(HT,I,I)
          IF (V /= I) THEN
            WRITE(LU,*)'FOUND',V,'SHOULD HAVE BEEN',I
            CALL PLANTE(1,'MOD_HASH_TABLE')
          END IF
        END DO
!
        !CHANGE THE VALUE OF AN ALREADY EXISTING ELEMENT
        CALL HASH_TABLE_INSERT(HT,10,10,2017)
        V=HASH_TABLE_GET(HT,10,10)
        IF (V /= 2017) THEN
          WRITE(LU,*)'FOUND',V,'SHOULD HAVE BEEN 2017'
          CALL PLANTE(1,'MOD_HASH_TABLE')
        END IF
!
        CALL HASH_TABLE_STAT(HT)
        CALL HASH_TABLE_DESTROY(HT)
        WRITE(LU,*)'HASH_TABLE: ALL TEST PASSED'
      END SUBROUTINE HASH_TABLE_TEST
!
      END MODULE MOD_HASH_TABLE
