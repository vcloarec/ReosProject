!                   *****************
                    SUBROUTINE PLANTE
!                   *****************
!
     &(IVAL)
!
!***********************************************************************
! SPECIAL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CAUSES A DIVIDE CHECK IF IVAL = 0 SUCH THAT THE CALL TREE
!+              IS GIVEN WHEN THE PROGRAM STOPS FOLLOWING DETECTION OF
!+              AN ERROR.
!+
!+              USE INSTEAD OF "STOP"
!
!warning  ALSO EXISTS IN THE BIEF LIBRARY
!warning  CALL TO PLANTE MUST BE FOLLOWED BY A "STOP" SO THAT
!+            THE COMPILER UNDERSTANDS THAT'S THE END
!
!bug      IN THE EVENT OF A COMPILATION ERROR WITH THIS SUBROUTINE
!+            ERASE THE TWO LINES MARKED CJAJ
!
!history  J-M HERVOUET (LNH) ; F  LEPEINTRE (LNH)
!+        17/08/1994
!+        V5P5
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
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Now crashing properly in parallel
!
!history  S.E.BOURBAN (HRW)
!+        16/09/2016
!+        V7P2
!+   Compatible flushing depending on compiler used
!+   (necessary for Algorithmic Differentiation)
!+   TODO: Create a common function with NESTOR and other flushes.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IVAL           |-->| INTEGER VALUE, OPTION, SEE CODE BELOW
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
#if defined NO_STD_FLUSH || NAGFOR
      USE F90_UNIX_IO, ONLY: FLUSH
#endif /* NO_STD_FLUSH || NAGFOR */
!
#if defined HAVE_MPI
#  if defined HAVE_MPI_MOD
      USE MPI
      IMPLICIT NONE
#  else
      IMPLICIT NONE
      INCLUDE 'mpif.h'
#  endif
#endif
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IVAL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ICODE, IERR
      LOGICAL MPI_IS_INIT
!
!-----------------------------------------------------------------------
!
      WRITE(LU,20)
20    FORMAT(1X,///,1X,'PLANTE: PROGRAM STOPPED AFTER AN ERROR')
!
!-----------------------------------------------------------------------
! PARALLEL MODE
!
!JAJ SETTING EXIT VALUES ACCORDING TO THE IVAL VALUE
!    IN CODE IVAL=0 OR IVAL=1 ARE USED NON-CONSEQUENTLY
!
!     STANDARD F90 :  STOP [n] WHERE N IS A STRING OF NOT MORE
!     THAN FIVE DIGITS OR IS A CHARACTER CONSTANT.
!     HOWEVER, CODE IS NOT ALWAYS SENT TO STDERR
!     (COMPILER DEPENDENT, NAG DOESN'T FOR INSTANCE)
!     ICODE MIGHT BE USED IN A POSSIBLE SYSTEM DEPENDENT EXIT PROCEDURE
#if defined NO_STD_FLUSH
      CALL FLUSH(LU)
#else
      FLUSH(LU)
#endif
#if defined HAVE_MPI
      CALL MPI_INITIALIZED(MPI_IS_INIT,ICODE)
#endif
      IF(IVAL.LT.0) THEN
        ICODE = 0      ! JUST ASSUMED FOR NON-ERROR STOP
      ELSEIF(IVAL.EQ.0.OR.IVAL.EQ.1) THEN
        ICODE = 2      ! EXIT IVAL 0 OR 1 INDICATING A "CONTROLLED" ERROR
      ELSE
        ICODE = 1     ! SOMETHING ELSE? BUT AN ERROR!
      ENDIF
      WRITE(LU,*) 'RETURNING EXIT CODE: ', ICODE
!
#if defined HAVE_MPI
      IF(MPI_IS_INIT) CALL MPI_ABORT(MPI_COMM_WORLD,ICODE,IERR)
#endif
      STOP 1   ! WHICH IS USUALLY EQUIVALENT TO CALL EXIT(1)
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE PLANTE
