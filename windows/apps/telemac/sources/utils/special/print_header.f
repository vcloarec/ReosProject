!                   ***********************
                    SUBROUTINE PRINT_HEADER
!                   ***********************
!
     &(CODE,CODE1)
!
!***********************************************************************
! SPECIAL   V7P2
!***********************************************************************
!
!brief    Print the head for a given code name
!
!
!history  Y. AUDOUIN (EDF)
!+        11/03/2016
!+        V7P2
!+        Creation of the file
!
!history  F. HUANG (CLARKSON U.) AND S.E. BOURBAN (HRW)
!+        11/11/2016
!+        V7P3
!+        Coupling TELEMAC-2D with KHIONE (ice modelling component)
!
!reference  www.chris.com/ascii/
!+  SOME OF THE DRAWINGS WERE INSPIRED BY DISPLAYS ON THE ABOVE WEBSITE.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CODE           |-->| NAME OF THE CODE FOR WHICH TO PRINT THE HEADER
!| CODE1          |-->| NAME OF THE CODE IT IS COUPLED WITH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=24), INTENT(IN) :: CODE
      CHARACTER(LEN=24), INTENT(IN) :: CODE1
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      WRITE(LU,*) 'LISTING OF ',TRIM(CODE), REPEAT('-',78)
!
      IF(CODE(1:7).EQ."ARTEMIS") THEN
        WRITE(LU,1) VERSION
        WRITE(LU,2)
1       FORMAT(/////,
     &  16X,' AAA  RRRR  TTTTT EEEEE M   M IIIII  SSSS',/,
     &  16X,'A   A R   R   T   E     MM MM   I   S    ',/,
     &  16X,'AAAAA RRRR    T   EEEEE M M M   I    SSS ',/,
     &  16X,'A   A R   R   T   E     M   M   I       S',/,
     &  16X,'A   A R   R   T   EEEEE M   M IIIII SSSS ',/,
     &  16X,'                                         ',/,
     &  16X,'      VERSION ',A,' FORTRAN 2003 ',/,
     &  16X,/////)
2       FORMAT(/////,
     &  16X,'                         .               ',/,
     &  16X,'                     ____/_              ',/,
     &  16X,"          \==      .'__===_|             ",/,
     &  16X,'           \_ _ _ /',"---'  \/           ",/,
     &  16X,'           |_|_|_|_\_______\______       ',/,
     &  16X,'           `.==-------------------|_     ',/,
     &  16X,'             `.___________________/      ',/,
     &  16X,/////)
      ELSE IF(CODE(1:7).EQ."TOMAWAC") THEN
        WRITE(LU,3) VERSION
        WRITE(LU,4)
3       FORMAT(/////,
     &  16X,'TTTTT  OOOOO  M   M  AAAAA  W   W  AAAAA  CCCCC ',/,
     &  16X,'  T    O   O  MM MM  A   A  W   W  A   A  C     ',/,
     &  16X,'  T    O   O  M W M  AAAAA  W W W  AAAAA  C     ',/,
     &  16X,'  T    O   O  M   M  A   A  WW WW  A   A  C     ',/,
     &  16X,'  T    OOOOO  M   M  A   A  W   W  A   A  CCCCC ',/,
     &  16X,'                                                ',/,
     &  16X,'         VERSION ',A,' FORTRAN 2003              ',/,
     &  16X,/////)
4       FORMAT(/,
     &  15X,'               |    |    |                  ',/,
     &  15X,'              )_)  )_)  )_) _               ',/,
     &  15X,'             )___))___))___)\               ',/,
     &  15X,'             )____)____)_____)\\            ',/,
     &  15X,'           _____|____|____|____\\\__        ',/,
     &  15X,'  ---------\                    /---------  ',/,
     &  15X,'    ^^^^^^^^^^^^^^^^^^^^^^^^^^^             ',/,
     &  15X,'         ^^^^      ^^^^     ^^^    ^^       ',/,
     &  15X,'             ^^^^      ^^^                  ',/,
     &  15X,/////)
      ELSE IF(CODE(1:9).EQ."TELEMAC2D") THEN
      WRITE(LU,5) VERSION
      WRITE(LU,6)
5     FORMAT(/,
     &16X,'TTTTT  EEEEE  L      EEEEE  M   M  AAAAA  CCCCC',/,
     &16X,'  T    E      L      E      MM MM  A   A  C    ',/,
     &16X,'  T    EEE    L      EEE    M M M  AAAAA  C    ',/,
     &16X,'  T    E      L      E      M   M  A   A  C    ',/,
     &16X,'  T    EEEEE  LLLLL  EEEEE  M   M  A   A  CCCCC',/,
     &16X,'                                               ',/,
     &16X,'        2D    VERSION ',A,' FORTRAN 2003       ',/,
     &16X,/////)
6     FORMAT(/,
     &16X,'        ~^~^~^~^~^~^~^~^~^~^~^^~^~^~^~^~^~     ',/,
     &16X,'          ~                            ~       ',/,
     &16X,"               \   '    o      '               ",/,
     &16X,'               /\ o       \  o                 ',/,
     &16X,"             >=)'>    '   /\ '                 ",/,
     &16X,"               \/   \   >=)'>        ~         ",/,
     &16X,'               /    /\    \/                   ',/,
     &16X,"        ~         >=)'>   /     .              ",/,
     &16X,'                    \/                   )     ',/,
     &16X,'                    /                   (      ',/,
     &16X,'                          ~          )   )     ',/,
     &16X,'          }     ~              (    (   (      ',/,
     &16X,'         {                      )    )   )     ',/,
     &16X,'          }  }         .       (    (   (      ',/,
     &16X,'         {  {               /^^^^^^^^^^^^      ',/,
     &16X,'        ^^^^^^^^^\         /                   ',/,
     &16X,'                  ^^^^^^^^^                    ',/,
     &16X,/////)
      ELSE IF(CODE(1:9).EQ."TELEMAC3D") THEN
      WRITE(LU,7) VERSION
      WRITE(LU,8)
7     FORMAT(/,
     &16X,'TTTTT  EEEEE  L      EEEEE  M   M  AAAAA  CCCCC',/,
     &16X,'  T    E      L      E      MM MM  A   A  C    ',/,
     &16X,'  T    EEE    L      EEE    M M M  AAAAA  C    ',/,
     &16X,'  T    E      L      E      M   M  A   A  C    ',/,
     &16X,'  T    EEEEE  LLLLL  EEEEE  M   M  A   A  CCCCC',/,
     &16X,'                                               ',/,
     &16X,'        3D   VERSION ',A,' FORTRAN 2003    ',/,
     &16X,/////)
8     FORMAT(/////,
     &16X,'        ~^~^~^~^~^~^~^~^~^~^~^^~^~^~^~^~^~     ',/,
     &16X,'          ~                            ~       ',/,
     &16X,"               \   '    o      '               ",/,
     &16X,'               /\ o       \  o                 ',/,
     &16X,"             >=)'>    '   /\ '                 ",/,
     &16X,"               \/   \   >=)'>        ~         ",/,
     &16X,'               /    /\    \/                   ',/,
     &16X,"        ~         >=)'>   /     .              ",/,
     &16X,'                    \/                   )     ',/,
     &16X,'                    /                   (      ',/,
     &16X,'                          ~          )   )     ',/,
     &16X,'          }     ~              (    (   (      ',/,
     &16X,'         {                      )    )   )     ',/,
     &16X,'          }  }         .       (    (   (      ',/,
     &16X,'         {  {               /^^^^^^^^^^^^      ',/,
     &16X,'        ^^^^^^^^^\         /                   ',/,
     &16X,'                  ^^^^^^^^^                    ',/,
     &16X,/////)
      ELSE IF(CODE(1:7).EQ."SISYPHE") THEN
      WRITE(LU,9) VERSION
      WRITE(LU,10)
9     FORMAT(/////,
     &16X,'   SSSS I   SSSS Y   Y PPPP  H   H EEEEE  ',/,
     &16X,'  S     I  S      Y Y  P   P H   H E      ',/,
     &16X,'   SSS  I   SSS    Y   PPPP  HHHHH EEEE   ',/,
     &16X,'      S I      S   Y   P     H   H E      ',/,
     &16X,'  SSSS  I  SSSS    Y   P     H   H EEEEE  ',/,
     &16X,'                                          ',/,
     &16X,'       VERSION ',A,' FORTRAN 2003  ',/,
     &16X,/////)
10    FORMAT(/,
     &16X,'                            ____             ',/,
     &16X,'                     /^\   / -- )            ',/,
     &16X,'                    / | \ (____/             ',/,
     &16X,'                   / | | \ / /               ',/,
     &16X,'                  /_|_|_|_/ /                ',/,
     &16X,'                   |     / /                 ',/,
     &16X,'    __    __    __ |    / /__    __    __    ',/,
     &16X,'   [  ]__[  ]__[  ].   / /[  ]__[  ]__[  ]   ',/,
     &16X,'   |__            ____/ /___           __|   ',/,
     &16X,'      |          / .------  )         |      ',/,
     &16X,'      |         / /        /          |      ',/,
     &16X,'      |        / /        /           |      ',/,
     &16X,'   ~~~~~~~~~~~~-----------~~~~~~~~~~~~~~~~~~ ',/,
     &16X,/////)
      ELSE IF(CODE(1:6).EQ."WAQTEL") THEN
        WRITE(LU,11) VERSION
        WRITE(LU,12)
11      FORMAT(/////,
     &16X,'W   W  AAAAA  QQQQQ  TTTTT  EEEEE  L     ',/,
     &16X,'W   W  A   A  Q   Q    T    E      L     ',/,
     &16X,'W W W  AAAAA  Q Q Q    T    EEE    L     ',/,
     &16X,'WW WW  A   A  Q  QQ    T    E      L     ',/,
     &16X,'W   W  A   A  QQQQQ    T    EEEEE  LLLLL ',/,
     &16X,'                                       ',/,
     &16X,'     VERSION ',A,' FORTRAN 2003 ',/,
     &16X,/////)
12    FORMAT(/,
     &15X,"                                           ",/,
     &15X,"                                           ",/,
     &15X,"         ,      ,      ,      ,            ",/,
     &15X,"         )\     )\     )\     )\           ",/,
     &15X,"        /  \   /  \   /  \   /  \          ",/,
     &15X,"       '    ' '    ' '    ' '    '         ",/,
     &15X,"       ',  ,' ',  ,' ',  ,' ',  ,'         ",/,
     &15X,"         `'     `'     `'     `'           ",/,
     &15X,/////)
      ELSE IF(CODE(1:6).EQ."KHIONE") THEN
      WRITE(LU,13) VERSION
      WRITE(LU,14)
13     FORMAT(/////,
     &16X,'   K  K  H  H  III   OOO   N   N  EEEE   ',/,
     &16X,'   K K   H  H   I   O   O  NN  N  E      ',/,
     &16X,'   KK    HHHH   I   O   O  N N N  EEE    ',/,
     &16X,'   K K   H  H   I   O   O  N  NN  E      ',/,
     &16X,'   K  K  H  H  III   OOO   N   N  EEEE   ',/,
     &16X,'                                         ',/,
     &16X,'       VERSION ',A,' FORTRAN 2003  ',/,
     &16X,/////)
14    FORMAT(/,
     &9X," _ \        '        '           '    .-~~'\       :::::: ",/,
     &9X," _\/_/   '     _ \     '    '        /      \_     |::::| ",/,
     &9X,"/ /\_      '   _\/_/            '    ~X   .-~__)   l~~~~l ",/,
     &9X,"  \     '     / /\_    '               \x/.-~__/~-. \  /  ",/,
     &9X,"          '     \         '    '    _  `-/         \ ||   ",/,
     &9X,"     .__    __,      '              ||  ( o  o      )||   ",/,
     &9X,"     /_/ /\ \_\        '    '     =#:l  |   <       )||   ",/,
     &9X,"   .__ \ \/ / __.                    \\  \  ._/    / (3   ",/,
     &9X,"    \_\_\/\/_/_/         \ /    '     \\,/^-,___.-'r/||   ",/,
     &9X," _/\___\_\/_/___/\_      ~V~          }^-\.(o).__.-//||   ",/,
     &9X,"  \/ __/_/\_\__ \/        )          /     \X/    // ||\  ",/,
     &9X,"    /_/ /\/\ \_\     >-~-<          /      |\ \   X  || \ ",/,
     &9X,"   ' __/ /\ \__ ,,,,, \ /\,,,,,,,,  |      ||\ \     || | ",/,
     &9X,"     \_\ \/ /_/                     |      \| |/     || | ",/,
     &9X,"     '        `                     \                || | ",/,
     &9X,"                                     \               ||/  ",/,
     &9X,"                                .--.--\              ||--.",/,
     &9X,"                                       `-._________,~`'   ",/,
     &9X,/////)
      ELSE IF(CODE(1:6).EQ."STBTEL") THEN
      WRITE(LU,15) VERSION
      WRITE(LU,16)
15    FORMAT(/////,
     &14X,'   SSSSS  TTTTT  BBBB   TTTTT  EEEEE  L    ',/,
     &14X,'   S        T    B   B    T    E      L    ',/,
     &14X,'   SSSSS    T    BBBB     T    EEEE   L    ',/,
     &14X,'       S    T    B   B    T    E      L    ',/,
     &14X,'   SSSSS    T    BBBB     T    EEEEE  LLLLL',/,
     &14X,'                                           ',/,
     &14X,'         VERSION ',A,' FORTRAN 2003        ',/,
     &14X,/////)
16    FORMAT(/,
     &14X,'    *     *    *     /\__/\  *    ---    * ',/,
     &14X,'       *            /      \    /     \    ',/,
     &14X,'            *   *  |  -  -  |  |       |*  ',/,
     &14X,'     *   __________| \     /|  |       |   ',/,
     &14X,'       /              \ T / |   \     /    ',/,
     &14X,'     /                      |  *  ---      ',/,
     &14X,'    |  ||     |    |       /             * ',/,
     &14X,'    |  ||    /______\     / |*     *       ',/,
     &14X,'    |  | \  |  /     \   /  |              ',/,
     &14X,'     \/   | |\ \      | | \ \              ',/,
     &14X,'          | | \ \     | |  \ \             ',/,
     &14X,'          | |  \ \    | |   \ \            ',/,
     &14X,"          '''   '''   '''    '''           ",/,
     &14X,/////)
      ELSE IF(CODE(1:8).EQ."POSTEL3D") THEN
      WRITE(LU,17) VERSION
      WRITE(LU,18)
17    FORMAT(/////,
     &12X,'PPPP    OOO    SSSS  TTTTT  EEEEE  L         3333   DDDD ',/,
     &12X,'P   P  O   O  S        T    E      L             3  D   D',/,
     &12X,'PPPP   O   O   SSS     T    EEEE   L     ---  333   D   D',/,
     &12X,'P      O   O      S    T    E      L             3  D   D',/,
     &12X,'P       OOO   SSSS     T    EEEEE  LLLLL     3333   DDDD ',/,
     &16X,'                                       ',/,
     &16X,'          VERSION ',A,' FORTRAN 2003 ',/,
     &12X,/////)
18    FORMAT(/,
     &15X,'                  __                 ',/,
     &15X,'                 /  \     ,    ,     ',/,
     &15X,'       _._     _ |oo| _  / \__/ \    ',/,
     &15X,'      _||||   ((/ () \))   /  \      ',/,
     &15X,'      |||||/|  ( ==== )    |oo|      ',/,
     &15X,"       \____/  _`\  /'_    /  \      ",/,
     &15X,"       /   /.-' /\<>/\ `\.( () )_._  ",/,
     &15X,"       |    `  /  \/  \  /`'--'////) ",/,
     &15X,"        \__,-'",'`|  |.  |\/ |/\/\|"\"` ',/,
     &15X,'               |  |.  | \___/\___/   ',/,
     &15X,'               |  |.  |              ',///)

      ELSE
      ENDIF
      ! Coupling
      IF(CODE1(1:1).NE.' ') THEN
        WRITE(LU,*) REPEAT(' ',14),
     &              '      COUPLED WITH '//TRIM(CODE1)//' INTERNALLY  '
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE PRINT_HEADER
