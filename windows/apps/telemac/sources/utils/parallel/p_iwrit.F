!                   ******************
                    SUBROUTINE P_IWRIT
!                   ******************
!
     &(BUFFER,N,BYTES,DEST,ITAG,IREQ)
!
!***********************************************************************
! PARALLEL   V8P0
!***********************************************************************
!
!brief    EXCHANGES VALUES BETWEEN PROCESSORS.
!
!history  PASCAL VEZOLLES (IBM)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BUFFER         |-->| ZONE TAMPON POUR LES DONNEES
!|                |   | BUFFER / PUFFERFELD
!| DEST           |-->| DESTINATION DES DONNEES
!|                |   | TID OF THE DEST.  / KNOTEN-ID DES EMPFAENGERS
!| IREQ           |<->| NUMERO DE REQUEST POUR MPI_ISEND
!| ITAG           |-->| MESSAGE TAG
!|  N             |-->| NOMBRE DE ELEMENTS A TRANSMETTRE
!|                |   | NUMBER OF ELEMENTS / ANZAHL DER ELEMENTE
!|  BYTES         |-->| NOMBRE DE BYTES A ELEMTENT
!|                |   | SIZE OF ELEMENTS IN BYTES / GR"OSSE DER ELEMENTE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DISTINGUISHING THE NUMBER OF ELEMENTS AND THEIR SIZE
      INTEGER, INTENT(IN)          :: N, BYTES,DEST,ITAG
      INTEGER, INTENT(INOUT)       :: IREQ
      DOUBLE PRECISION, INTENT(IN) :: BUFFER(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_MPI
      INTEGER IER
!
!-----------------------------------------------------------------------
!

!      DISTINGUISHING THE NUMBER OF ELEMENTS AND THEIR SIZE
!      NBYTES = N * MPI_SIZEOF_COMPAD_TYPE( ier )
#  ifdef COMPAD
      CALL AMPI_ISEND(BUFFER,N,AMPI_TYPE,DEST,ITAG,
     &                COMM,IREQ,IER)
#  else
      CALL MPI_ISEND(BUFFER,N*BYTES,MPI_BYTE,DEST,ITAG,
     &                COMM,IREQ,IER)
#  endif
!
      IF(IER.NE.0) THEN
        WRITE(LU,*) 'P_IWRIT: ERROR IN MPI_ISEND'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      WRITE(LU,*) 'CALL OF P_IWRIT IN ITS VOID VERSION'
#endif
!
!----------------------------------------------------------------------
!
      RETURN
      END
