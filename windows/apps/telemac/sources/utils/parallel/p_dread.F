!                   ******************
                    SUBROUTINE P_DREAD
!                   ******************
!
     &(BUFFER,N,BYTES,SOURCE,DATATYPE)
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    RECEIVES DATA.
!
!history  REINHARD HINKELMANN (HANOVER)
!+        08/06/1996
!+
!+   MODIFIED
!
!history  J-M MERVOUET (LNH)
!+        17/12/1996
!+
!+   MODIFIED
!
!history  HANS HERRMANN (HANOVRE)
!+        08/01/1997
!+
!+
!
!history  RAINER JOHANNI (SGI MUNICH)
!+        **/10/1999
!+
!+   ADAPTED FOR MPI
!
!history  J.A. JANKOWSKI (BAW KARLSRUHE)
!+        28/12/1999
!+
!+   RELEASE 5.0 MODIFIED
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
!| BUFFER         |<--| ZONE TAMPON POUR LES DONNEES
!|                |   | BUFFER / PUFFERFELD
!|  N             |-->| NOMBRE DE ELEMENTS A TRANSMETTRE
!|                |   | NUMBER OF ELEMENTS / ANZAHL DER ELEMENTE
!|  BYTES         |-->| NOMBRE DE BYTES A ELEMTENT
!|                |   | SIZE OF ELEMENTS IN BYTES / GR"OSSE DER ELEMENTE
!| SOURCE         |-->| ORIGINE DES DONNEES
!|                |   | TID OF THE SENDER / KNOTEN-ID DES SENDER
!| DATATYPE           |-->| TYPE DES DONNEES (MSGTAG DE PVM)
!|                |   | 0 - STRING
!|                |   | 1 - BYTE1
!|                |   | 2 - INTEGER2
!|                |   | 3 - INTEGER4
!|                |   | 4 - REAL4
!|                |   | 5 - COMPLEX8
!|                |   | 6 - REAL8
!|                |   | 7 - COMPLEX16
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DISTINGUISHING THE NUMBER OF ELEMENTS AND THEIR SIZE
      INTEGER, INTENT(IN) :: N, BYTES,SOURCE,DATATYPE
!     NO DEFAULT INITIALISATION FOR USER TYPE COMPONENTS ALLOWED
      DOUBLE PRECISION, INTENT(INOUT) :: BUFFER(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_MPI
      INTEGER STATUS(MPI_STATUS_SIZE)
      INTEGER IER
!
!-----------------------------------------------------------------------
!
!     RECEIVES DATA
!
!     DISTINGUISHING THE NUMBER OF ELEMENTS AND THEIR SIZE
!     NBYTES = N * MPI_SIZEOF_COMPAD_TYPE( ier )
#  ifdef COMPAD
      CALL AMPI_RECV(BUFFER,N,AMPI_TYPE,SOURCE,4711,
     &              COMM,STATUS,IER)
#  else
      CALL MPI_RECV(BUFFER,N*BYTES,MPI_BYTE,SOURCE,4711,
     &              COMM,STATUS,IER)
#  endif
!
      IF(IER.NE.0) THEN
        WRITE(LU,*) 'P_DREAD: ERROR IN MPI_RECV'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      BUFFER(1)=0.D0
      WRITE(LU,*) 'CALL OF P_DREAD IN ITS VOID VERSION'
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
