!                   ****************************************
                    CHARACTER(LEN=11) FUNCTION EXTENS
!                   ****************************************
!
     &(N,I)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    GIVES THE EXTENSION FOR NAMING FILES IN PARALLEL
!+
!
!history  J-M HERVOUET (LNHE)
!+        11/07/2008
!+        V5P9
!+
!
!history  J-M HERVOUET (LNHE)
!+        22/11/2012
!+        V6P3
!+   USE BIEF removed, IIPID and IPID changed into I.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| N              |-->| TOTAL NUMBER OF PROCESSORS
!| I              |-->| RANK OF THE PROCESSOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: I,N
!
!-----------------------------------------------------------------------
!
      IF(N.GT.0) THEN
!
        EXTENS='00000-00000'
!
        IF(N.LT.10) THEN
          WRITE(EXTENS(05:05),'(I1)') N
        ELSEIF(N.LT.100) THEN
          WRITE(EXTENS(04:05),'(I2)') N
        ELSEIF(N.LT.1000) THEN
          WRITE(EXTENS(03:05),'(I3)') N
        ELSEIF(N.LT.10000) THEN
          WRITE(EXTENS(02:05),'(I4)') N
        ELSE
          WRITE(EXTENS(01:05),'(I5)') N
        ENDIF
!
        IF(I.LT.10) THEN
          WRITE(EXTENS(11:11),'(I1)') I
        ELSEIF(I.LT.100) THEN
          WRITE(EXTENS(10:11),'(I2)') I
        ELSEIF(I.LT.1000) THEN
          WRITE(EXTENS(09:11),'(I3)') I
        ELSEIF(I.LT.10000) THEN
          WRITE(EXTENS(08:11),'(I4)') I
        ELSE
          WRITE(EXTENS(07:11),'(I5)') I
        ENDIF
!
      ELSE
!
        EXTENS='           '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION EXTENS

