!                   *****************************
                    SUBROUTINE MULT_INTERFACE_SEG
!                   *****************************
!
     &(FSEG,NH_COM_SEG,DIM1NHCOM,NB_NEIGHB_SEG,
     & NB_NEIGHB_PT_SEG,LIST_SEND,NSEG)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    Shares a function defined on segments between sub-domains.
!
!history  J-M HERVOUET (LNHE)
!+        27/02/2009
!+        V5P9
!+   First version.
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        12/06/2015
!+        V7P1
!+   Now one sub-domain receives all, the others nothing.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIM1NHCOM       |-->| FIRST DIMENSION OF NH_COM_SEG
!| FSEG            |<->| THE FUNCTION DEFINED ON SEGMENTS
!| LIST_SEND       |-->| RANKS OF NEIGHBOURING SUB-DOMAINS
!| NB_NEIGHB_PT_SEG|-->| NUMBER OF SEGMENTS SHARED WITH A NEIGHBOUR
!|                 |   | PROCESSOR
!| NB_NEIGHB_SEG   |-->| NUMBER OF NEIGHBOUR PROCESSORS (FOR SEGMENTS)
!| NH_COM_SEG      |-->| ADDRESSES OF INTERFACE SEGMENTS
!| NSEG            |-->| NUMBER OF SEGMENTS
!| XMUL            |-->| THE CONSTANT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: DIM1NHCOM,NB_NEIGHB_SEG,NSEG
      INTEGER, INTENT(INOUT) :: NH_COM_SEG(DIM1NHCOM,NB_NEIGHB_SEG)
      INTEGER, INTENT(IN)    :: NB_NEIGHB_PT_SEG(NB_NEIGHB_SEG)
      INTEGER, INTENT(IN)             :: LIST_SEND(NB_NEIGHB_SEG)
      DOUBLE PRECISION, INTENT(INOUT) :: FSEG(NSEG)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISEG,IPROC,IADSEG
!
!-----------------------------------------------------------------------
!
!     DONE ONLY IF THERE IS AT LEAST ONE OTHER SUB-DOMAIN SHARING
!     A SEGMENT WITH THIS ONE
!
      IF(NB_NEIGHB_SEG.GT.0) THEN
!
!       LOOP ON ALL NEIGHBOURING SUB-DOMAINS
!
        DO IPROC=1,NB_NEIGHB_SEG
!
!         ONLY THE PROCESSOR WITH HIGHEST RANK WILL KEEP ITS VALUES
!         OTHERS ARE CANCELLED
          IF(IPID.LT.LIST_SEND(IPROC)) THEN
!           LOOP ON ALL SEGMENTS SHARED WITH THIS SUB-DOMAIN
!           WHICH CANNOT BE SHARED WITH ANOTHER SUB-DOMAIN (UNLIKE POINTS)
            DO ISEG=1,NB_NEIGHB_PT_SEG(IPROC)
!             ADDRESS IN SEGMENT NUMBERING
              IADSEG=NH_COM_SEG(ISEG,IPROC)
              FSEG(IADSEG)=0.D0
            ENDDO
          ENDIF
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

