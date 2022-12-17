!                   ******************
                    SUBROUTINE P_DWRIT
!                   ******************
!
     &(BUFFER,N,BYTES,DEST,DATATYPE)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    EXCHANGES VALUES BETWEEN PROCESSORS.
!
!history  REINHARD HINKELMANN (HANOVER)
!+        08/06/1996
!+
!+   MODIFIED
!
!history  J-M HERVOUET (LNH)
!+        17/12/1996
!+
!+   MODIFIED
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
!history  HANS HERRMANN (HANOVRE)
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
!|  N             |-->| NOMBRE DE ELEMENTS A TRANSMETTRE
!|                |   | NUMBER OF ELEMENTS / ANZAHL DER ELEMENTE
!|  BYTES         |-->| NOMBRE DE BYTES A ELEMTENT
!|                |   | SIZE OF ELEMENTS IN BYTES / GR"OSSE DER ELEMENTE
!| DATATYPE       |-->| TYPE DES DONNEES (MSGTAG DE PVM)
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
      INTEGER, INTENT(IN)          :: N,BYTES,DEST,DATATYPE
!     NO DEFAULT INITIALISATION FOR USER TYPE COMPONENTS ALLOWED
      DOUBLE PRECISION, INTENT(INOUT) :: BUFFER(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_MPI
      INTEGER IER
!
!-----------------------------------------------------------------------
!
!     DISTINGUISHING THE NUMBER OF ELEMENTS AND THEIR SIZE
!     NBYTES = N * MPI_SIZEOF_COMPAD_TYPE( ier )
#  ifdef COMPAD
      CALL AMPI_SEND(BUFFER,N,AMPI_TYPE,DEST,4711,COMM,IER)
#  else
      CALL MPI_SEND(BUFFER,N*BYTES,MPI_BYTE,DEST,4711,COMM,IER)
#  endif
!
      IF (IER.NE.0) THEN
        WRITE(LU,*) 'P_DWRITE: ERROR IN MPI_SEND'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
      RETURN
#else
      WRITE(LU,*) 'CALL OF P_DWRIT IN ITS VOID VERSION'
#endif
!
!----------------------------------------------------------------------
!
      END
