!                   ******************************
                    SUBROUTINE CORRECTION_DEPTH_2D
!                   ******************************
!
     &(GLOSEG,DIMGLO,YASMH)
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    APPLIES VARIOUS TECHNIQUES TO TREAT NEGATIVE DEPTHS.
!
!history  J-M HERVOUET (LNHE)
!+        25/08/2009
!+        V6P0
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
!+        24/02/2012
!+        V6P2
!+   Rain and evaporation added
!
!history  J-M HERVOUET (LNHE)
!+        16/07/2012
!+        V6P2
!+   Call to receding added.
!
!history  J-M HERVOUET (LNHE)
!+        22/02/2016
!+        V7P2
!+   Adapted to treatment number 3 of negative depths, with a newly
!+   active option for positive_depths. Checking OPT_HNEG.
!
!history  L. STADLER (BAW)
!+        15/03/2016
!+        V7P2
!+   Call to flusec_t2d added at the end, for computing the discharge
!+   control sections.
!
!history  J-M HERVOUET (LNHE)
!+        09/09/2016
!+        V7P2
!+   Call to positive_depths changed (sources and rain now split).
!
!history  J-M HERVOUET (LNHE)
!+        06/11/2016
!+        V7P3
!+   Call to positive_depths changed (FLULIMEBE, etc.). Now, depending
!+   on OPT_HNEG, either FLULIM or FLULIMEBE will be produced.
!
!history  R. ATA (LNHE)
!+        10/03/2018
!+        V7P3
!+   fixing a bug with fluxline. flodel was not computed in some cases
!+   even though DOFLUX is YES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| GLOSEG         |-->| GLOBAL NUMBERS OF APICES OF SEGMENTS
!| YASMH          |-->| THE RIGHT-HAND SIDE SMH HAS TO BE TAKEN
!|                |   | INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D,
     &                     EX_CORRECTION_DEPTH_2D => CORRECTION_DEPTH_2D
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: DIMGLO
      INTEGER, INTENT(IN)    :: GLOSEG(DIMGLO,2)
      LOGICAL, INTENT(IN)    :: YASMH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IOPT1,OPTPOS
      CHARACTER(LEN=16) :: FORMUL
!
!-----------------------------------------------------------------------
!
      IF(OPT_HNEG.LT.0.OR.OPT_HNEG.GT.3) THEN
        WRITE(LU,*) 'TREATMENT OF NEGATIVE DEPTHS'
        WRITE(LU,*) 'MUST BE BETWEEN 0 AND 3'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     OPTION FOR POSITIVE DEPTHS
!     DEPENDING ON TREATMENT OF NEGATIVE DEPTHS
!
      IF(OPT_HNEG.EQ.2) THEN
        OPTPOS=2
      ELSEIF(OPT_HNEG.EQ.3) THEN
        OPTPOS=1
      ELSE
!       NOT USED
        OPTPOS=0
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CASES OF COMPUTATION OF FLODEL
!
      IF(  INCLUS(COUPLING,'DELWAQ')  .OR.DOFLUX.OR.
     &   ((OPTBAN.EQ.1.OR.OPTBAN.EQ.3).AND.OPT_HNEG.GE.2) ) THEN
        FORMUL='HUGRADP         '
        IF(SOLSYS.EQ.2) FORMUL(8:8)='2'
!
!       HERE ASSEMBLING T1 IS NOT USEFUL, HENCE LEGO=.FALSE.
!
        CALL VECTOR(T1,'=',FORMUL,H%ELM,-1.D0,
     &              HPROP,DM1,ZCONV,UDEL,VDEL,VDEL,MESH,MSK,MASKEL,
     &              LEGO=.FALSE.)
!                   T1 AS HUGRADP IS NOT USED AS AN ASSEMBLED VECTOR
!                   BUT TO GET THE NON ASSEMBLED FORM MESH%W
!
!       COMPUTING FLODEL, SO FAR IOPT1 HARDCODED OPTION
        IOPT1=2
        CALL FLUX_EF_VF(FLODEL%R,MESH%W%R,MESH%NSEG,MESH%NELEM,
     &                  MESH%NELMAX,MESH%ELTSEG%I,MESH%ORISEG%I,
     &                  MESH%IKLE%I,.TRUE.,IOPT1)
        YAFLODEL=.TRUE.
      ENDIF
!
      IF(OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
      IF(DEBUG.GT.0) WRITE(LU,*) 'TRAITEMENT BANCS DECOUVRANTS'
!
      IF(OPT_HNEG.GE.2) THEN
!
!       LIMITS FLUXES TO HAVE POSITIVE DEPTHS
!       BEWARE, WILL BE ONLY VALID FOR ADVECTION WITH UDEL,VDEL
!       HENCE FOR TRACERS AND IF SOLSYS=2
!       FLULIM OR FLULIMEBE WILL BE CORRECT AT THE EXIT OF POSITIVE_DEPTHS
        IF(OPT_HNEG.EQ.2) YAFLULIM=.TRUE.
        IF(OPT_HNEG.EQ.3) YAFLULIMEBE=.TRUE.
!
        IF(YAFLULIMEBE) THEN
          CALL POSITIVE_DEPTHS(T1,T2,T3,T4,H,HN,MESH,FLODEL,.FALSE.,
     &                         FLBOR,DT,UNSV2D,
     &                         NPOIN,GLOSEG(1:DIMGLO,1),
     &                         GLOSEG(1:DIMGLO,2),
     &                         MESH%NBOR%I,MESH%NPTFR,
     &                         SMH,YASMH,PLUIE,RAIN,
     &                         OPTSOU,FLULIM%R,LIMPRO%I,HBOR%R,KDIR,
     &                         ENTET,
     &                         MESH%W%R,NAMECODE,OPTPOS,MAXADV,
     &                         DOFLULIM=YAFLULIM,FLULIMEBE=FLULIMEBE%R,
     &                         DOFLULIMEBE=YAFLULIMEBE)
        ELSE
          CALL POSITIVE_DEPTHS(T1,T2,T3,T4,H,HN,MESH,FLODEL,.FALSE.,
     &                         FLBOR,DT,UNSV2D,
     &                         NPOIN,GLOSEG(1:DIMGLO,1),
     &                         GLOSEG(1:DIMGLO,2),
     &                         MESH%NBOR%I,MESH%NPTFR,
     &                         SMH,YASMH,PLUIE,RAIN,
     &                         OPTSOU,FLULIM%R,LIMPRO%I,HBOR%R,KDIR,
     &                         ENTET,
     &                         MESH%W%R,NAMECODE,OPTPOS,MAXADV,
     &                         DOFLULIM=YAFLULIM)
        ENDIF
!
      ELSEIF(OPT_HNEG.EQ.1) THEN
!
!     CONSERVATIVE SMOOTHING OF THE NEGATIVE DEPTHS
!
!     1) OPTIONAL THRESHOLD (HNEG IS NEGATIVE)
!
      IF(HNEG.LT.-1.D-6) CALL OS('X=X+C   ',X=H,C=-HNEG)
!
!     2) NEGATIVE DEPTHS IN T1 AND TAKEN OUT OF H
!
      CALL OS( 'X=-(Y,C)' , X=T1 , Y=H  , C=0.D0 )
      CALL OS( 'X=X-Y   ' , X=H  , Y=T1 )
!
!     3) NEGATIVE DEPTHS ARE SMOOTHED (TWICE HERE)
!        AND MASKED TO AVOID SPREAD OVER THE WETTING/DRYING AREAS
!
      IF(OPTBAN.EQ.1) THEN
        CALL FILTER_H(T1,T2,MESH,MSK,MASKEL,2,FLODEL,
     &                YAFLODEL,DT,W1,UNSV2D)
      ELSEIF(OPTBAN.EQ.3) THEN
!            FILTER_H DOES NOT WORK WITH POROSITY
!       CALL FILTER_H(T1,T2,MESH,.TRUE.,TE5,2,FLODEL,
!    *                YAFLODEL,DT,W1,UNSV2D)
!
!       THIS WILL BE SLIGHTLY WRONG WITH DELWAQ
        CALL FILTER(T1,.TRUE.,T2,T3,
     &              AM1,'MATMAS          ',
     &              1.D0,S,S,S,S,S,S,MESH,.TRUE.,TE5,2)
      ENDIF
!
!     4) SMOOTHED NEGATIVE DEPTHS TRANSFERRED BACK TO H
!
      CALL OS( 'X=X+Y   ' , X=H , Y=T1 )
!
!     5) OPTIONAL THRESHOLD IS SUBTRACTED
!
      IF(HNEG.LT.-1.D-6) CALL OS('X=X+C   ',X=H,C=HNEG)
!
      ENDIF
!
!     HREC IS THE THRESHOLD DEPTH FOR RECEDING PROCEDURE
!     RECEDING PROCEDURE, TO PREVENT SPURIOUS OVERWHELMING OF DYKES
!     WHEN MESH TOO COARSE...
!
      IF((HREC.GT.0.D0.AND.DOFLUX).OR.
     &    (HREC.GT.0.D0.AND.OPT_HNEG.GE.2.AND.
     &    (OPTBAN.EQ.1.OR.OPTBAN.EQ.3))) THEN
!       HERE FLODEL HAS ALREADY BEEN BUILT, IT WILL BE MODIFIED
        CALL CPSTVC(H,T1)
        CALL CPSTVC(H,T2)
        CALL RECEDING(H%R,ZF%R,HREC,V2DPAR%R,MESH%IKLE%I,
     &                NPOIN,MESH%NELEM,MESH%NELMAX,T1,T2,MESH,
     &                W1%R,YAFLODEL,FLODEL,DT)
      ENDIF
!
!     OPTIONAL CLIPPING OF NEGATIVE VALUES
      IF(CLIPH) CALL CLIP(H,HMIN,.TRUE.,1.D6,.FALSE.,0)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'FIN DU TRAITEMENT BANCS DECOUVRANTS'
      ENDIF
!
!-----------------------------------------------------------------------
!
!     OPTIONAL FLUXLINE
!
      IF(DOFLUX) THEN
        CALL FLUSEC_T2D(GLOSEG,DIMGLO,DT,MESH,FLODEL,ENTET)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
