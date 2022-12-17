!                   *****************
                    SUBROUTINE WRIHYD
!                   *****************
!
     &(TITRE , ITSTRT , ITSTOP , NPOIN2 , MBND   ,
     & NSEG  , NOLAY  , NOMGEO , NOMLIM ,
     & F     , NSTEPA , NOMSOU , NOMMAB , NOMCOU ,
     & NOMINI, NOMVEB , NOMMAF , NOMSAL , NOMTEM , NOMVEL , NOMVIS ,
     & NHYD  , SALI_DEL,TEMP_DEL,VELO_DEL,DIFF_DEL,MARDAT,MARTIM)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    WRITES OUT THE HYDRODYNAMIC FILE FOR DELWAQ (.HYD).
!
!history  CHARLES MOULINEC
!+        20/03/2007
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
!history  C.-T. PHAM (LNHE), Y. AUDOUIN (LNHE)
!+        12/03/2013
!+        V6P2
!+   Correction of bugs when LEN_TRIM(FILENAME) = 0 + cosmetics
!+   Definition of the size of strings. In particular, TITRE has
!+   a length lower or equal 72 => useless tests have been deleted
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIFF_DEL       |-->| IF YES, WRITES DIFFUSION FILE FOR DELWAQ
!| F              |-->| ARRAY TO STORE FRACTION OF DEPTH PER LAYER
!| ITSTOP         |-->| STOP TIME
!| ITSTRT         |-->| START TIME
!| MARDAT         |-->| DATE (YEAR, MONTH,DAY)
!| MARTIM         |-->| TIME (HOUR, MINUTE,SECOND)
!| MBND           |-->| SEQUENTIAL COUNTER OPEN BOUNDARIES
!| NHYD           |-->| DELWAQ STEERING FILE CANAL
!| NOLAY          |-->| NUMBER OF PLANES
!| NOMCOU         |-->| FLUX FILE
!| NOMGEO         |-->| RESULT FILE OF THE SIMULATION
!| NOMINI         |-->| HORIZONTAL SURFACE FILE
!| NOMLIM         |-->| BOUNDARY FILE OF THE SIMULATION
!| NOMMAB         |-->| AREA FILE
!| NOMMAF         |-->| NODE DISTANCE FILE
!| NOMSAL         |-->| SALINITY FOR DELWAQ FILE
!| NOMSOU         |-->| VOLUME FILE
!| NOMTEM         |-->| TEMPERATURE FOR DELWAQ FILE
!| NOMVEB         |-->| NODE EXCHANGE FILE
!| NOMVEL         |-->| VELOCITY FILE
!| NOMVIS         |-->| DIFFUSION FILE
!| NPOIN2         |-->| NUMBER OF 2D POINTS IN THE MESH
!| NSEG           |-->| NUMBER OF 2D SEGMENTS IN THE MESH
!| NSTEPA         |-->| NUMBER OF TIME-STEPS FOR TIME AGGREGATION
!| SALI_DEL       |-->| IF YES, THERE IS SALINITY
!| TEMP_DEL       |-->| IF YES, THERE IS TEMPERATURE
!| TITRE          |-->| TITLE OF STUDY
!| VELO_DEL       |-->| IF YES, WRITES VELOCITY FILE FOR DELWAQ
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN) :: NHYD,ITSTRT,ITSTOP,NPOIN2
      INTEGER,          INTENT(IN) :: NSEG,NOLAY,NSTEPA,MBND
      INTEGER,          INTENT(IN) :: MARDAT(3),MARTIM(3)
      CHARACTER(LEN=72),  INTENT(IN) :: TITRE
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: NOMSOU,NOMMAB,NOMCOU,NOMSAL
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: NOMINI,NOMVEB,NOMMAF,NOMVEL
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: NOMGEO,NOMLIM,NOMTEM,NOMVIS
      DOUBLE PRECISION, INTENT(IN) :: F(NPOIN2,NOLAY)
      LOGICAL,          INTENT(IN) :: SALI_DEL,TEMP_DEL
      LOGICAL,          INTENT(IN) :: VELO_DEL,DIFF_DEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ILAY,IWAQ,I
      INTEGER IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEC
      DOUBLE PRECISION REFER_DAY,JULIAN_DAY
!
      DOUBLE PRECISION JULTIM
      EXTERNAL         JULTIM
!
!-----------------------------------------------------------------------
!
      IF( MARDAT(1) .EQ. 1900 ) THEN
        WRITE(LU,*) ' Severe warning:'
        WRITE(LU,*) ' YOU USE THE DEFAULT REFERENCE YEAR OF 1900!!!'
        WRITE(LU,*) ' SINCE DELWAQ TIMES ARE COUNTED FROM THIS YEAR,'
        WRITE(LU,*) ' AND SPAN AT MOST SOME 68 YEARS, YOU MAY NOT BE'
        WRITE(LU,*) ' ABLE TO RUN BEYOND 1968!!!'
        WRITE(LU,*) ' Remedy:'
        WRITE(LU,*) ' MANUALLY EDIT THE ~~~.hyd FILE TO ADJUST ALL'
        WRITE(LU,*) ' MENTIONED YEARS TO 50 OR 100 YEARS LATER!!!'
        WRITE(LU,*) ' YOU NEED not RERUN THE HYDRODYNAMICS!!!'
      ENDIF
!
      WRITE ( NHYD, '(A)' )
     &    "task      full-coupling                              "
      WRITE ( NHYD, '(A)' )
     &    "                                                     "
      WRITE ( NHYD, '(A)' )
     &    "#                                                    "
      WRITE ( NHYD, '(A)' )
     &    "# telemac data                                       "
      WRITE ( NHYD, '(A)' )
     &    "#                                                    "
      WRITE ( NHYD, '(A)' )
     &    "                                                     "
      WRITE ( NHYD, '(A)' )
     &    "geometry  finite-elements                            "
      WRITE ( NHYD, '(A)' )
     &    "                                                     "
      WRITE ( NHYD, '(A)' )
     &    "horizontal-aggregation       no                      "
      WRITE ( NHYD, '(A)' )
     &    "minimum-vert-diffusion-used  no                      "
      WRITE ( NHYD, '(A)' )
     &    "vertical-diffusion           calculated              "
      WRITE ( NHYD, '(A)' )
     &    "description                                          "
!  CHARACTER(LEN=72),  INTENT(IN) :: TITRE
!  PREVIOUS TESTS FOR LEN GREATER THAN 80 ARE USELESS
      IWAQ = LEN_TRIM(TITRE)
      IF ( IWAQ .EQ. 0 ) THEN
        WRITE ( NHYD, '(A)' )
     &    "   '                                    '            "
        WRITE ( NHYD, '(A)' )
     &    "   '                                    '            "
        WRITE ( NHYD, '(A)' )
     &    "   '                                    '            "
      ELSEIF ( IWAQ .LE. 40 ) THEN
        WRITE ( NHYD, '(A,A,A)' ) "   '",TITRE(1:IWAQ),"'"
        WRITE ( NHYD, '(A)' )
     &    "   '                                    '            "
        WRITE ( NHYD, '(A)' )
     &    "   '                                    '            "
      ELSE
        WRITE ( NHYD, '(A,A,A)' ) "   '",TITRE(1:40),"'"
        WRITE ( NHYD, '(A,A,A)' ) "   '",TITRE(41:IWAQ),"'"
        WRITE ( NHYD, '(A)' )
     &    "   '                                    '            "
      ENDIF
      WRITE ( NHYD, '(A)' )
     &    "end-description                                      "
      WRITE ( NHYD, '(A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)' )
     &    "reference-time           '",MARDAT(1),MARDAT(2),MARDAT(3),
     &                                 MARTIM(1),MARTIM(2),MARTIM(3),"'"
      REFER_DAY  = JULTIM(MARDAT(1),MARDAT(2),MARDAT(3),
     &                    MARTIM(1),MARTIM(2),MARTIM(3),0.D0)
      JULIAN_DAY = REFER_DAY + DBLE(ITSTRT)/(86400.D0*36525.D0)
      CALL GREGTIM( JULIAN_DAY, IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC )
      WRITE ( NHYD, '(A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)' )
     &    "hydrodynamic-start-time  '",IYEAR,IMONTH,IDAY,
     &                                 IHOUR,IMIN  ,ISEC, "'"
      JULIAN_DAY = REFER_DAY + DBLE(ITSTOP)/(86400.D0*36525.D0)
      CALL GREGTIM( JULIAN_DAY, IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC )
      WRITE ( NHYD, '(A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)' )
     &    "hydrodynamic-stop-time   '",IYEAR,IMONTH,IDAY,
     &                                 IHOUR,IMIN  ,ISEC, "'"
!      IDAY  = NSTEPA/86400
!      IHOUR = (NSTEPA-IDAY*86400)/3600
!      IMIN  = (NSTEPA-IDAY*86400-IHOUR*3600)/60
!      ISEC  =  NSTEPA-IDAY*86400-IHOUR*3600-IMIN*60
!      WRITE ( NHYD, '(A,I2.2,I2.2,I2.2,A)' )
!     &    "hydrodynamic-timestep    '00000000",IHOUR,IMIN,ISEC,"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "hydrodynamic-timestep    '",NSTEPA,"'"
      WRITE ( NHYD, '(A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)' )
     &    "conversion-ref-time      '",MARDAT(1),MARDAT(2),MARDAT(3),
     &                                 MARTIM(1),MARTIM(2),MARTIM(3),"'"
      JULIAN_DAY = REFER_DAY + DBLE(ITSTRT)/(86400.D0*36525.D0)
      CALL GREGTIM( JULIAN_DAY, IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC )
      WRITE ( NHYD, '(A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)' )
     &    "conversion-start-time    '",IYEAR,IMONTH,IDAY,
     &                                 IHOUR,IMIN  ,ISEC, "'"
      JULIAN_DAY = REFER_DAY + DBLE(ITSTOP)/(86400.D0*36525.D0)
      CALL GREGTIM( JULIAN_DAY, IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC )
      WRITE ( NHYD, '(A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)' )
     &    "conversion-stop-time     '",IYEAR,IMONTH,IDAY,
     &                                 IHOUR,IMIN  ,ISEC, "'"
!      IDAY  = NSTEPA/86400
!      IHOUR = (NSTEPA-IDAY*86400)/3600
!      IMIN  = (NSTEPA-IDAY*86400-IHOUR*3600)/60
!      ISEC  =  NSTEPA-IDAY*86400-IHOUR*3600-IMIN*60
!      WRITE ( NHYD, '(A,I2.2,I2.2,I2.2,A)' )
!     &    "conversion-timestep      '00000000",IHOUR,IMIN,ISEC,"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "conversion-timestep      '",NSTEPA,"'"
      WRITE ( NHYD, '(A,I7)'  )
     &    "grid-cells-first-direction ",NPOIN2
      WRITE ( NHYD, '(A,I7,A)')
     &    "grid-cells-second-direction",NSEG+MBND," # nr of exchanges!"
      WRITE ( NHYD, '(A,I6)' )
     &    "number-hydrodynamic-layers ",NOLAY
      WRITE ( NHYD, '(A,I6)' )
     &    "number-water-quality-layers",NOLAY
      IWAQ = LEN_TRIM(NOMGEO)
      I    = IWAQ
      DO WHILE(I.GE.1)
        IF((NOMGEO(I:I).NE.'/').AND.(NOMGEO(I:I).NE.'\')) THEN
          I = I-1
        ELSE
          EXIT
        ENDIF
      ENDDO
      IF(IWAQ.NE.0) THEN
        WRITE ( NHYD, '(A,A,A)' )
     &    "hydrodynamic-file        '",NOMGEO(I+1:IWAQ),"'"
      ELSE
        WRITE ( NHYD, '(A)' ) "hydrodynamic-file        ''"
      ENDIF
      WRITE ( NHYD, '(A)' )
     &    "aggregation-file         none                        "
      IF(IWAQ.NE.0) THEN
        WRITE ( NHYD, '(A,A,A)' )
     &    "grid-indices-file        '",NOMGEO(I+1:IWAQ),"'"
      ELSE
        WRITE ( NHYD, '(A)' ) "grid-indices-file        ''"
      ENDIF
      IWAQ = LEN_TRIM(NOMLIM)
      I    = IWAQ
      DO WHILE(I.GE.1)
        IF((NOMLIM(I:I).NE.'/').AND.(NOMLIM(I:I).NE.'\')) THEN
          I = I-1
        ELSE
          EXIT
        ENDIF
      ENDDO
      IF(IWAQ.NE.0) THEN
        WRITE ( NHYD, '(A,A,A)' )
     &    "grid-coordinates-file    '",NOMLIM(I+1:IWAQ),"'"
      ELSE
        WRITE ( NHYD, '(A)' ) "grid-coordinates-file    ''"
      ENDIF
      IWAQ = LEN_TRIM(NOMSOU)
      I    = IWAQ
      DO WHILE(I.GE.1)
        IF((NOMSOU(I:I).NE.'/').AND.(NOMSOU(I:I).NE.'\')) THEN
          I = I-1
        ELSE
          EXIT
        ENDIF
      ENDDO
      IF(IWAQ.NE.0) THEN
        WRITE ( NHYD, '(A,A,A)' )
     &    "volumes-file             '",NOMSOU(I+1:IWAQ),"'"
      ELSE
        WRITE ( NHYD, '(A)' ) "volumes-file             ''"
      ENDIF
      IWAQ = LEN_TRIM(NOMMAB)
      I    = IWAQ
      DO WHILE(I.GE.1)
        IF((NOMMAB(I:I).NE.'/').AND.(NOMMAB(I:I).NE.'\')) THEN
          I = I-1
        ELSE
          EXIT
        ENDIF
      ENDDO
      IF(IWAQ.NE.0) THEN
        WRITE ( NHYD, '(A,A,A)' )
     &    "areas-file               '",NOMMAB(I+1:IWAQ),"'"
      ELSE
        WRITE ( NHYD, '(A)' ) "areas-file               ''"
      ENDIF
      IWAQ = LEN_TRIM(NOMCOU)
      I    = IWAQ
      DO WHILE(I.GE.1)
        IF((NOMCOU(I:I).NE.'/').AND.(NOMCOU(I:I).NE.'\')) THEN
          I = I-1
        ELSE
          EXIT
        ENDIF
      ENDDO
      IF(IWAQ.NE.0) THEN
        WRITE ( NHYD, '(A,A,A)' )
     &    "flows-file               '",NOMCOU(I+1:IWAQ),"'"
      ELSE
        WRITE ( NHYD, '(A)' ) "flows-file               ''"
      ENDIF
      IWAQ = LEN_TRIM(NOMVEB)
      I    = IWAQ
      DO WHILE(I.GE.1)
        IF((NOMVEB(I:I).NE.'/').AND.(NOMVEB(I:I).NE.'\')) THEN
          I = I-1
        ELSE
          EXIT
        ENDIF
      ENDDO
      IF(IWAQ.NE.0) THEN
        WRITE ( NHYD, '(A,A,A)' )
     &    "pointers-file            '",NOMVEB(I+1:IWAQ),"'"
      ELSE
        WRITE ( NHYD, '(A)' ) "pointers-file            ''"
      ENDIF
      IWAQ = LEN_TRIM(NOMMAF)
      I    = IWAQ
      DO WHILE(I.GE.1)
        IF((NOMMAF(I:I).NE.'/').AND.(NOMMAF(I:I).NE.'\')) THEN
          I = I-1
        ELSE
          EXIT
        ENDIF
      ENDDO
      IF(IWAQ.NE.0) THEN
        WRITE ( NHYD, '(A,A,A)' )
     &    "lengths-file             '",NOMMAF(I+1:IWAQ),"'"
      ELSE
        WRITE ( NHYD, '(A)' ) "lengths-file             ''"
      ENDIF
      IF(SALI_DEL) THEN
        IWAQ = LEN_TRIM(NOMSAL)
        I    = IWAQ
        DO WHILE(I.GE.1)
          IF((NOMSAL(I:I).NE.'/').AND.(NOMSAL(I:I).NE.'\')) THEN
            I = I-1
          ELSE
            EXIT
          ENDIF
        ENDDO
        IF(IWAQ.NE.0) THEN
          WRITE ( NHYD, '(A,A,A)' )
     &    "salinity-file            '",NOMSAL(I+1:IWAQ),"'"
        ELSE
          WRITE ( NHYD, '(A)' ) "salinity-file            ''"
        ENDIF
      ELSE
        WRITE ( NHYD, '(A)' )
     &    "salinity-file            none                        "
      ENDIF
      IF(TEMP_DEL) THEN
        IWAQ = LEN_TRIM(NOMTEM)
        I    = IWAQ
        DO WHILE(I.GE.1)
          IF((NOMTEM(I:I).NE.'/').AND.(NOMTEM(I:I).NE.'\')) THEN
            I = I-1
          ELSE
            EXIT
          ENDIF
        ENDDO
        IF(IWAQ.NE.0) THEN
          WRITE ( NHYD, '(A,A,A)' )
     &    "temperature-file         '",NOMTEM(I+1:IWAQ),"'"
        ELSE
          WRITE ( NHYD, '(A)' ) "temperature-file         ''"
        ENDIF
      ELSE
        WRITE ( NHYD, '(A)' )
     &    "temperature-file         none                        "
      ENDIF
      IF(DIFF_DEL) THEN
        IWAQ = LEN_TRIM(NOMVIS)
        I    = IWAQ
        DO WHILE(I.GE.1)
          IF((NOMVIS(I:I).NE.'/').AND.(NOMVIS(I:I).NE.'\')) THEN
            I = I-1
          ELSE
            EXIT
          ENDIF
        ENDDO
        IF(IWAQ.NE.0) THEN
          WRITE ( NHYD, '(A,A,A)' )
     &    "vert-diffusion-file      '",NOMVIS(I+1:IWAQ),"'"
        ELSE
          WRITE ( NHYD, '(A)' ) "vert-diffusion-file      ''"
        ENDIF
      ELSE
        WRITE ( NHYD, '(A)' )
     &    "vert-diffusion-file      none                        "
      ENDIF
      IF(VELO_DEL) THEN
        IWAQ = LEN_TRIM(NOMVEL)
        I    = IWAQ
        DO WHILE(I.GE.1)
          IF((NOMVEL(I:I).NE.'/').AND.(NOMVEL(I:I).NE.'\')) THEN
            I = I-1
          ELSE
            EXIT
          ENDIF
        ENDDO
        IF(IWAQ.NE.0) THEN
          WRITE ( NHYD, '(A,A,A)' )
     &    "velocity-file            '",NOMVEL(I+1:IWAQ),"'"
        ELSE
          WRITE ( NHYD, '(A)' ) "velocity-file            ''"
        ENDIF
      ELSE
        WRITE ( NHYD, '(A)' )
     &    "velocity-file            none                        "
      ENDIF
      IWAQ = LEN_TRIM(NOMINI)
      I    = IWAQ
      DO WHILE(I.GE.1)
        IF((NOMINI(I:I).NE.'/').AND.(NOMINI(I:I).NE.'\')) THEN
          I = I-1
        ELSE
          EXIT
        ENDIF
      ENDDO
      IF(IWAQ.NE.0) THEN
        WRITE ( NHYD, '(A,A,A)' )
     &    "surfaces-file            '",NOMINI(I+1:IWAQ),"'"
      ELSE
        WRITE ( NHYD, '(A)' ) "surfaces-file            ''"
      ENDIF
      WRITE ( NHYD, '(A)' )
     &    "total-grid-file          none                        "
      WRITE ( NHYD, '(A)' )
     &    "discharges-file          none                        "
      WRITE ( NHYD, '(A)' )
     &    "chezy-coefficients-file  none                        "
      WRITE ( NHYD, '(A)' )
     &    "shear-stresses-file      none                        "
      WRITE ( NHYD, '(A)' )
     &    "walking-discharges-file  none                        "
      IF ( NOLAY .GT. 1 ) THEN
        WRITE ( NHYD, '(A)' )
     &       "minimum-vert-diffusion                            "
        WRITE ( NHYD, '(A)' )
     &       "   upper-layer       0.0000E+00                   "
        WRITE ( NHYD, '(A)' )
     &       "   lower-layer       0.0000E+00                   "
        WRITE ( NHYD, '(A)' )
     &       "   interface-depth   0.0000E+00                   "
        WRITE ( NHYD, '(A)' )
     &       "end-minimum-vert-diffusion                        "
      ENDIF
      WRITE ( NHYD, '(A)' )
     &    "constant-dispersion                                  "
      WRITE ( NHYD, '(A)' )
     &    "   first-direction    0.0000                         "
      WRITE ( NHYD, '(A)' )
     &    "   second-direction   0.0000                         "
      WRITE ( NHYD, '(A)' )
     &    "   third-direction    0.0000                         "
      WRITE ( NHYD, '(A)' )
     &    "end-constant-dispersion                              "
      WRITE ( NHYD, '(A)' )
     &    "hydrodynamic-layers                               "
!      DO ILAY=1,NOLAY
!     FROM TOP TO BOTTOM IN DELWAQ
      DO ILAY=NOLAY,1,-1
        WRITE ( NHYD, '(F10.4)' ) F(1,ILAY)
      ENDDO
      WRITE ( NHYD, '(A)' )
     &    "end-hydrodynamic-layers                           "
      WRITE ( NHYD, '(A)' )
     &    "water-quality-layers                              "
      DO ILAY=1,NOLAY
        WRITE ( NHYD, '(F10.4)' ) 1.0
      ENDDO
      WRITE ( NHYD, '(A)' )
     &    "end-water-quality-layers                          "
      WRITE ( NHYD, '(A)' )
     &    "discharges                                           "
      WRITE ( NHYD, '(A)' )
     &    "end-discharges                                       "
!
!-----------------------------------------------------------------------
!
      RETURN
      END
