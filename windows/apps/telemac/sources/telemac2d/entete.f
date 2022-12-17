!                   *****************
                    SUBROUTINE ENTETE
!                   *****************
!
     &(IETAPE,AT,LT,ASCHEME,AORDRE)
!
!***********************************************************************
! TELEMAC2D   V8P0                                   21/09/2018
!***********************************************************************
!
!brief    WRITES ON THE LISTING HEADINGS FOR THE VARIOUS STAGES
!+               OF THE PROGRAM.
!
!history  J-M HERVOUET (LNH)
!+        05/09/2007
!+        V5P8
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
!| AT             |-->| TIME IN SECONDS
!| AORDRE         |-->| ORDRE OF FV SCHEME
!| IETAPE         |-->| FRACTIONAL STEP IN THE ALGORITHM
!| LT             |-->| TIME STEP
!| IETAPE         |-->| FRACTIONAL STEP IN THE ALGORITHM
!| ASCHEME        |-->| FV SCHEME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TELEMAC2D, EX_ENTETE => ENTETE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: AT
      INTEGER, INTENT(IN)          :: LT,IETAPE
      INTEGER, INTENT(IN),OPTIONAL :: ASCHEME,AORDRE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION S
!
      INTEGER J,H,M,ORDRE,SCHEME
!
      CHARACTER(LEN=32) :: FR(14),GB(14)
      CHARACTER(LEN=32) :: SCHEMEFR(9),SCHEMEGB(9)
      CHARACTER(LEN=32) :: ORDERFR(6),ORDERGB(6)
!
      INTRINSIC INT
!
!-----------------------------------------------------------------------
!
      PARAMETER ( FR = (/
     &     '                                ' ,
     &     '                                ' ,
     &     '     ETAPE DE CONVECTION        ' ,
     &     '       MODELE K-EPSILON         ' ,
     &     'ETAPE DE DIFFUSION DES TRACEURS ' ,
     &     ' ETAPE DE DIFFUSION-PROPAGATION ' ,
     &     '      BILAN DE VOLUME D''EAU     ' ,
     &     ' BILAN FINAL DE VOLUME D''EAU    ' ,
     &     '  TEMPS :                       ' ,
     &     ' SECONDES                       ' ,
     &     'ITERATION                       ' ,
     &     '     DERIVE DE FLOTTEUR(S)      ' ,
     &     '   DERIVE(S) LAGRANGIENNE(S)    ' ,
     &     '   MODELE DE SPALART-ALLMARAS   '  /) )
      PARAMETER ( GB = (/
     &     '                                ' ,
     &     '                                ' ,
     &     '        ADVECTION STEP          ' ,
     &     '        K-EPSILON MODEL         ' ,
     &     '   DIFFUSION OF TRACERS STEP    ' ,
     &     '  DIFFUSION-PROPAGATION STEP    ' ,
     &     '     BALANCE OF WATER VOLUME    ' ,
     &     ' FINAL BALANCE OF WATER VOLUME  ' ,
     &     '    TIME:                       ' ,
     &     ' SECONDS                        ' ,
     &     'ITERATION                       ' ,
     &     '       DRIFT OF DROGUE(S)       ' ,
     &     '      LAGRANGIAN DRIFT(S)       ' ,
     &     '  SPALART-ALLMARAS TURB. MODEL  '  /) )
!
      PARAMETER ( SCHEMEFR = (/
     &     '*                              *' ,
     &     '*                              *' ,
     &     '*        SCHEMA DE ROE         *' ,
     &     '*       SCHEMA CINETIQUE       *' ,
     &     '*   SCHEMA DE ZOKAGOA-TCHAMEN  *' ,
     &     '*   SCHEMA DE ZOKAGOA-TCHAMEN  *' ,
     &     '*        SCHEMA HLLC           *' ,
     &     '*        SCHEMA WAF            *' ,
     &     '********************************'  /) )
      PARAMETER ( SCHEMEGB = (/
     &     '*                              *' ,
     &     '*                              *' ,
     &     '*         ROE SCHEME           *' ,
     &     '*       KINETIC SCHEME         *' ,
     &     '*   ZOKAGOA-TCHAMEN SCHEME     *' ,
     &     '*   ZOKAGOA-TCHAMEN SCHEME     *' ,
     &     '*        HLLC SCHEME           *' ,
     &     '*         WAF SCHEME           *' ,
     &     '********************************'  /) )
      PARAMETER ( ORDERFR = (/
     &     '*                              *' ,
     &     '*                              *' ,
     &     '*   PREMIER ORDRE EN ESPACE    *' ,
     &     '*   DEUXIEME ORRE EN ESPACE    *' ,
     &     '*   TROISIEME ORDRE EN ESPACE  *' ,
     &     '********************************'  /) )
      PARAMETER ( ORDERGB = (/
     &     '*                              *' ,
     &     '*                              *' ,
     &     '*     FIRST ORDRE IN SPACE     *' ,
     &     '*     SECOND ORDRE IN SPACE    *' ,
     &     '*     THIRD ORDRE IN SPACE     *' ,
     &     '********************************'  /) )
!
!-----------------------------------------------------------------------
!
      ORDRE=1
      SCHEME=1
      IF(PRESENT(AORDRE ))ORDRE = AORDRE
      IF(PRESENT(ASCHEME))SCHEME= ASCHEME
!
!-----------------------------------------------------------------------
!
      IF(.NOT.PRESENT(ASCHEME))THEN
!
!       DECOMPOSITION OF TIME IN DAYS, HOURS, MINUTES AND SECONDS
!
        S = AT
        J = INT(AT/86400.D0)
        S = S - 86400.D0 * J
        H = INT(S/3600.D0)
        S = S - 3600.D0 * H
        M = INT(S/60.D0)
        S = S - 60.D0 * M
!
!-----------------------------------------------------------------------
!
!       PRINTS TIME AND ITERATIONS
!
        IF (IETAPE.EQ.1.OR.IETAPE.EQ.2) THEN
!
          IF(J.NE.0) THEN
            IF(LNG.EQ.LNG_FR) WRITE(LU,10) FR(11),LT,FR(9),J,H,M,S,AT
            IF(LNG.EQ.LNG_EN) WRITE(LU,11) GB(11),LT,GB(9),J,H,M,S,AT
          ELSEIF(H.NE.0) THEN
            IF(LNG.EQ.LNG_FR) WRITE(LU,20) FR(11),LT,FR(9),H,M,S,AT
            IF(LNG.EQ.LNG_EN) WRITE(LU,20) GB(11),LT,GB(9),H,M,S,AT
          ELSEIF(M.NE.0) THEN
            IF(LNG.EQ.LNG_FR) WRITE(LU,30) FR(11),LT,FR(9),M,S,AT
            IF(LNG.EQ.LNG_EN) WRITE(LU,30) GB(11),LT,GB(9),M,S,AT
          ELSE
            IF(LNG.EQ.LNG_FR) WRITE(LU,40) FR(11),LT,FR(9),S
            IF(LNG.EQ.LNG_EN) WRITE(LU,40) GB(11),LT,GB(9),S
          ENDIF
!
!       PRINTS TITLES FOR EACH STAGES
!
        ELSE
!
          IF(LNG.EQ.LNG_FR) WRITE(LU,200) FR(IETAPE)
          IF(LNG.EQ.LNG_EN) WRITE(LU,200) GB(IETAPE)
!
        ENDIF
!
      ELSE
        IF(LNG.EQ.LNG_FR)WRITE(LU,400)SCHEMEFR(1),SCHEMEFR(SCHEME+3),
     &                           ORDERFR (ORDRE +2),SCHEMEFR(1)
        IF(LNG.EQ.LNG_EN)WRITE(LU,400)SCHEMEGB(1),SCHEMEGB(SCHEME+3),
     &                           ORDERGB(ORDRE+2),SCHEMEGB(1)
      ENDIF
!
!-----------------------------------------------------------------------
!
10     FORMAT(/,80('='),/,1X,A10,I8,A10,
     &     1I4,' J ',1I2,' H ',1I2,' MIN ',F8.4,' S',3X,'(',F14.4,' S)')
11     FORMAT(/,80('='),/,1X,A10,I8,A10,
     &     1I4,' D ',1I2,' H ',1I2,' MN ',F8.4,' S',3X,'(',F14.4,' S)')
20     FORMAT(/,80('='),/,1X,A10,I8,A10,1I2,' H ',1I2,' MIN ',F8.4,' S',
     &                                               3X,'(',F14.4,' S)')
30     FORMAT(/,80('='),/,1X,A10,I8,A10,1I2,' MN ',F8.4,' S',
     &                                               3X,'(',F14.4,' S)')
40     FORMAT(/,80('='),/,1X,A10,I8,A10,F8.4,' S')
200    FORMAT(80('-'),/,18X,A32)
400    FORMAT(/,18X,32('*'),/,18X,A32,/,18X,A32,/,18X,A32,/,18X,A32,/,
     &          18X,32('*'))
!
!-----------------------------------------------------------------------
!
      RETURN
      END
