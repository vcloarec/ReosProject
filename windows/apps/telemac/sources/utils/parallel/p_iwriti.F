!                   *******************
                    SUBROUTINE P_IWRITI
!                   *******************
!
     &(BUFFER,NVAL,DEST,ITAG,IREQ)
!
!***********************************************************************
! PARALLEL   V8P0
!***********************************************************************
!
!brief    EXCHANGES VALUES BETWEEN PROCESSORS.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        19/11/2013
!+        V7P0
!+     A mere copy of P_IWRIT by Pascal Vezolles for integers. However
!+     NBYTES changed into NVAL to avoid *4 in the calls.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BUFFER         |-->| BUFFER / PUFFERFELD
!| DEST           |-->| DESTINATION DES DONNEES
!|                |   | TID OF THE DEST.  / KNOTEN-ID DES EMPFAENGERS
!| IREQ           |<->| NUMERO DE REQUEST POUR MPI_ISEND
!| ITAG           |-->| MESSAGE TAG
!| NVAL           |-->| NUMBER OF VALUES TO BE TRANSMITTED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NVAL,DEST,ITAG
      INTEGER, INTENT(INOUT) :: IREQ
      INTEGER, INTENT(IN) :: BUFFER(NVAL)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_MPI
      INTEGER IER
!
!-----------------------------------------------------------------------
!
!                                4: TO BE PARAMETERISED ????
      CALL MPI_ISEND(BUFFER,NVAL*4,MPI_BYTE,DEST,ITAG,
     &               COMM,IREQ,IER)
!
      IF(IER.NE.0) THEN
        WRITE(LU,*) 'P_IWRITI: ERROR IN MPI_ISEND'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      WRITE(LU,*) 'CALL OF P_IWRITI IN ITS VOID VERSION'
#endif
!
!----------------------------------------------------------------------
!
      RETURN
      END
