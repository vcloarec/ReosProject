!                 *****************
                  MODULE LIB_VTK_IO
!                 *****************
!
!***********************************************************************
! STBTEL   V6P3                                     06/2013
!***********************************************************************
!
!brief
!
!history  Y.AUDOUIN
!+        30/06/2013
!+        V6P3
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|              |<-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      PRIVATE
#if !defined DISABLE_VTK
      ! FUNCTIONS FOR VTK LEGACY
      PUBLIC:: VTK_INI
      PUBLIC:: VTK_GEO
      PUBLIC:: VTK_CON
      PUBLIC:: VTK_DAT
      PUBLIC:: VTK_VAR
      PUBLIC:: VTK_END
      ! PORTABLE KIND-PRECISION
      PUBLIC:: R16P, FR16P
      PUBLIC:: R8P,  FR8P
      PUBLIC:: R4P,  FR4P
      PUBLIC:: R_P,  FR_P
      PUBLIC:: I8P,  FI8P
      PUBLIC:: I4P,  FI4P
      PUBLIC:: I2P,  FI2P
      PUBLIC:: I1P,  FI1P
      PUBLIC:: I_P,  FI_P
      !-----------------------------------------------

      !----------------------------------------------
      ! overloading of VTK_GEO
      INTERFACE VTK_GEO
        MODULE PROCEDURE VTK_GEO_UNST_R8,   ! REAL(R8P) UNSTRUCTURED_GRID
     &                   VTK_GEO_UNST_R4,   ! REAL(R4P) UNSTRUCTURED_GRID
     &                   VTK_GEO_STRP_R8,   ! REAL(R8P) STRUCTURED_POINTS
     &                   VTK_GEO_STRP_R4,   ! REAL(R4P) STRUCTURED_POINTS
     &                   VTK_GEO_STRG_R8,   ! REAL(R8P) STRUCTURED_GRID
     &                   VTK_GEO_STRG_R4,   ! REAL(R4P) STRUCTURED_GRID
     &                   VTK_GEO_RECT_R8,   ! REAL(R8P) RECTILINEAR_GRID
     &                   VTK_GEO_RECT_R4    ! REAL(R4P) RECTILINEAR_GRID
      END INTERFACE
      ! overloading of VTK_VAR
      INTERFACE VTK_VAR
        MODULE PROCEDURE VTK_VAR_SCAL_R8,   ! REAL(R8P)    SCALAR
     &                   VTK_VAR_SCAL_R4,   ! REAL(R4P)    SCALAR
     &                   VTK_VAR_SCAL_I4,   ! INTEGER(I4P) SCALAR
     &                   VTK_VAR_VECT_R8,   ! REAL(R8P)    VECTORIAL
     &                   VTK_VAR_VECT_R4,   ! REAL(R4P)    VECTORIAL
     &                   VTK_VAR_VECT_I4,   ! INTEGER(I4P) VECTORIAL
     &                   VTK_VAR_TEXT_R8,   ! REAL(R8P)    VECTORIAL (TEXTURE)
     &                   VTK_VAR_TEXT_R4    ! REAL(R4P)    VECTORIAL (TEXTURE)
      END INTERFACE
      !----------------------------------------------------------------------------------------------------------------------------------

      !----------------------------------------------------------------------------------------------------------------------------------
      !!\LIBVTKIO has a small set of internal variables and parameters some of which have public visibility.
      !!
      !!The \LIBVTKIO uses a partable kind parameters for real and integer variables. The following are the kind parameters used: these
      !!parameters are public and their use is strong encouraged.
      !!
      !!Real precision definitions:
      !!
      INTEGER, PARAMETER:: R16P = SELECTED_REAL_KIND(33,4931) ! 33  DIGITS, RANGE $[\PM 10^{-4931}  ,\PM 10^{+4931}   -1]$
      INTEGER, PARAMETER:: R8P  = SELECTED_REAL_KIND(15,307)  ! 15  DIGITS, RANGE $[\PM 10^{-307}~~ ,\PM 10^{+307}~~  -1]$
      INTEGER, PARAMETER:: R4P  = SELECTED_REAL_KIND(6,37)    ! 6~~~DIGITS, RANGE $[\PM 10^{-37}~~~~,\PM 10^{+37}~~~~ -1]$
      INTEGER, PARAMETER:: R_P  = R8P                         ! DEFAULT REAL PRECISION
      !!Integer precision definitions:
      !!
      INTEGER, PARAMETER:: I8P  = SELECTED_INT_KIND(18)       ! RANGE $[-2^{63} ,+2^{63}  -1]$
      INTEGER, PARAMETER:: I4P  = SELECTED_INT_KIND(9)        ! RANGE $[-2^{31} ,+2^{31}  -1]$
      INTEGER, PARAMETER:: I2P  = SELECTED_INT_KIND(4)        ! RANGE $[-2^{15} ,+2^{15}  -1]$
      INTEGER, PARAMETER:: I1P  = SELECTED_INT_KIND(2)        ! RANGE $[-2^{7}~~,+2^{7}~~ -1]$
      INTEGER, PARAMETER:: I_P  = I4P                         ! DEFAULT INTEGER PRECISION
      !!
      !!Besides the kind parameters there are also the format parameters useful for writing in a well-ascii-format numeric variables.
      !!Also these parameters are public.
      !!
      !! Real output formats:
      !!
      CHARACTER(10), PARAMETER:: FR16P = '(E41.33E4)'         ! R16P  OUTPUT FORMAT
      CHARACTER(10), PARAMETER:: FR8P  = '(E23.15E3)'         ! R8P   OUTPUT FORMAT
      CHARACTER(9),  PARAMETER:: FR4P  = '(E14.6E2)'          ! R4P   OUTPUT FORMAT
      CHARACTER(10), PARAMETER:: FR_P  = '(E23.15E3)'         ! R\_P  OUTPUT FORMAT
      !! Integer output formats:
      !!
      CHARACTER(5), PARAMETER:: FI8P  = '(I21)'               ! I8P  OUTPUT FORMAT
      CHARACTER(5), PARAMETER:: FI4P  = '(I12)'               ! I4P  OUTPUT FORMAT
      CHARACTER(4), PARAMETER:: FI2P  = '(I7)'                ! I2P  OUTPUT FORMAT
      CHARACTER(4), PARAMETER:: FI1P  = '(I5)'                ! I1P  OUTPUT FORMAT
      CHARACTER(5), PARAMETER:: FI_P  = '(I12)'               ! I\_P OUTPUT FORMAT
      !!
      !!\LIBVTKIO uses a small set of internal variables that are private (not accessible from the outside). The following are
      !! private variables:
      !!
      INTEGER(I4P), PARAMETER:: MAXLEN       = 500         ! MAX NUMBER OF CHARACTERS OS STATIC STRING
      CHARACTER(1), PARAMETER:: END_REC      = CHAR(10)    ! END-CHARACTER FOR BINARY-RECORD FINALIZE
      INTEGER(I4P), PARAMETER:: F_OUT_ASCII  = 0           ! ASCII-OUTPUT-FORMAT PARAMETER IDENTIFIER
      INTEGER(I4P), PARAMETER:: F_OUT_BINARY = 1           ! BINARY-OUTPUT-FORMAT PARAMETER IDENTIFIER
      INTEGER(I4P)::            F_OUT        = F_OUT_ASCII ! CURRENT OUTPUT-FORMAT (INITIALIZED TO ASCII FORMAT)
      CHARACTER(LEN=MAXLEN)::   TOPOLOGY                   ! MESH TOPOLOGY
      INTEGER(I4P)::            UNIT_VTK                   ! INTERNAL LOGICAL UNIT
      !----------------------------------------------------------------------------------------------------------------------------------

      !!IN THE FOLLOWING CHAPTERS THERE IS THE API REFERENCE OF ALL FUNCTIONS OF \LIBVTKIO.
      CONTAINS
        !!\chapter{Auxiliary functions}
        !!\minitoc
        !!\vspace*{8mm}
        !!
        !!\LIBVTKIO uses two auxiliary functions that are not connected with the VTK standard. These functions are private and so they
        !!cannot be called outside the library.
        FUNCTION GETUNIT() RESULT(FREE_UNIT)
        !--------------------------------------------------------------------------------------------------------------------------------
        !!The GetUnit function is used for getting a free logic unit. The users of \LIBVTKIO does not know which is
        !!the logical unit: \LIBVTKIO handels this information without boring the users. The logical unit used is safe-free: if the
        !!program calling \LIBVTKIO has others logical units used \LIBVTKIO will never use these units, but will choice one that is free.
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P):: FREE_UNIT ! FREE LOGIC UNIT
        INTEGER(I4P):: N1        ! COUNTER
        INTEGER(I4P):: IOS       ! INQUIRING FLAG
        LOGICAL(4)::   LOPEN     ! INQUIRING FLAG
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        !!The following is the code snippet of GetUnit function: the units 0, 5, 6, 9 and all non-free units are discarded.
        !!
        !(\doc)codesnippet
        FREE_UNIT = -1_I4P                                      ! INITIALIZING FREE LOGIC UNIT
        N1=1_I4P                                                ! INITIALIZING COUNTER
        DO
          IF ((N1/=5_I4P).AND.(N1/=6_I4P).AND.(N1/=9_I4P)) THEN
            INQUIRE (UNIT=N1,OPENED=LOPEN,IOSTAT=IOS)           ! VERIFY LOGIC UNITS
            IF (IOS==0_I4P) THEN
              IF (.NOT.LOPEN) THEN
                FREE_UNIT = N1                                  ! ASSIGNMENT OF FREE LOGIC
                RETURN
              ENDIF
            ENDIF
          ENDIF
          N1=N1+1_I4P                                           ! UPDATING COUNTER
        ENDDO
        RETURN
        !(doc/)codesnippet
        !!GetUnit function is private and cannot be called outside \LIBVTKIO. If you are interested to use it change its scope to public.
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION GETUNIT

        FUNCTION UPPER_CASE(STRING)
        !--------------------------------------------------------------------------------------------------------------------------------
        !!The Upper\_Case function converts the lower case characters of a string to upper case one. \LIBVTKIO uses this function in
        !!order to achieve case-insensitive: all character variables used within \LIBVTKIO functions are pre-processed by
        !!Uppper\_Case function before these variables are used. So the users can call \LIBVTKIO functions whitout pay attention of the
        !!case of the kwywords passed to the functions: calling the function VTK\_INI with the string \code{E_IO = VTK_INI('Ascii',...)}
        !!or with the string  \code{E_IO = VTK_INI('AscII',...)} is equivalent.
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        CHARACTER(LEN=*), INTENT(IN):: STRING     ! STRING TO BE CONVERTED
        CHARACTER(LEN=LEN(STRING))::   UPPER_CASE ! CONVERTED STRING
        INTEGER::                      N1         ! CHARACTERS COUNTER
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        !!The following is the code snippet of Upper\_Case function.
        !!
        !(\doc)codesnippet
        UPPER_CASE = STRING
        DO N1=1,LEN(STRING)
          SELECT CASE(ICHAR(STRING(N1:N1)))
          CASE(97:122)
            UPPER_CASE(N1:N1)=CHAR(ICHAR(STRING(N1:N1))-32) ! UPPER CASE CONVERSION
          ENDSELECT
        ENDDO
        RETURN
        !(doc/)codesnippet
        !!Upper\_Case function is private and cannot be called outside \LIBVTKIO. If you are interested to use it change its scope
        !!to public.
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION UPPER_CASE

        !!\chapter{VTK LEGACY functions}
        !!\minitoc
        !!\vspace*{8mm}
        !!
        FUNCTION VTK_INI(OUTPUT_FORMAT,FILENAME,TITLE,MESH_TOPOLOGY)
     &          RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !!The VTK\_INI function is used for initializing file. This function must be the first to be called.
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        CHARACTER(*), INTENT(IN):: OUTPUT_FORMAT ! OUTPUT FORMAT: ASCII OR BINARY
        CHARACTER(*), INTENT(IN):: FILENAME      ! NAME OF FILE
        CHARACTER(*), INTENT(IN):: TITLE         ! TITLE
        CHARACTER(*), INTENT(IN):: MESH_TOPOLOGY ! MESH TOPOLOGY
        INTEGER(I4P)::             E_IO          ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        !!The VTK\_INI variables have the following meaning:
        !!
        !!\begin{description}
        !! \item[{\color{RoyalBlue}output\_format}] indicates the \virgo{format} of output file. It can assume the following values:
        !! \begin{enumerateABlu}
        !!  \item \emph{ascii} (it is case insensitive) $\rightarrow$ creating an ascii output file.
        !!  \item \emph{binary} (it is case insensitive) $\rightarrow$ creating a binary (big\_endian encoding) output file.
        !! \end{enumerateABlu}
        !! \item[{\color{RoyalBlue}filename}] contains the name (with its path) of the output file.
        !! \item[{\color{RoyalBlue}title}] contains the title of the VTK dataset.
        !! \item[{\color{RoyalBlue}topology}] indicates the topology of the mesh and can assume the following values:
        !! \begin{enumerateABlu}
        !!  \item \emph{STRUCTURED\_POINTS}.
        !!  \item \emph{STRUCTURED\_GRID}.
        !!  \item \emph{UNSTRUCTURED\_GRID}.
        !!  \item \emph{RECTILINEAR\_GRID}.
        !! \end{enumerateABlu}
        !! \item[{\color{RoyalBlue}E\_IO}] contains the inquiring integer flag for error handling.
        !!\end{description}
        !!
        !!The following is an example of VTK\_INI calling:
        !!
        !!\begin{boxred}{VTK\_INI Calling}
        !!\begin{verbatim}
        !!...
        !!E_IO = VTK_INI('Binary','example.vtk','VTK legacy file','UNSTRUCTURED_GRID')
        !!...
        !!\end{verbatim}
        !!\end{boxred}
        !!\noindent Note that the \virgo{.vtk} extension is necessary in the file name.
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        TOPOLOGY = TRIM(MESH_TOPOLOGY)
        UNIT_VTK=GETUNIT()
        SELECT CASE(TRIM(UPPER_CASE(OUTPUT_FORMAT)))
        CASE('ASCII')
          F_OUT = F_OUT_ASCII
          OPEN(UNIT     = UNIT_VTK,
     &         FILE     = TRIM(FILENAME),
     &         FORM     = 'FORMATTED',
     &         ACCESS   = 'SEQUENTIAL',
     &         ACTION   = 'WRITE',
!     &         BUFFERED = 'YES',
     &         IOSTAT   = E_IO)
          ! WRITING HEADER OF FILE
          WRITE(UNIT=UNIT_VTK,FMT='(A)',IOSTAT=E_IO)
     &                  '# vtk DataFile Version 3.0'
          WRITE(UNIT=UNIT_VTK,FMT='(A)',IOSTAT=E_IO)TRIM(TITLE)
          WRITE(UNIT=UNIT_VTK,FMT='(A)',IOSTAT=E_IO)
     &                  TRIM(UPPER_CASE(OUTPUT_FORMAT))
          WRITE(UNIT=UNIT_VTK,FMT='(A)',IOSTAT=E_IO)
     &                  'DATASET '//TRIM(TOPOLOGY)
        CASE('BINARY')
          F_OUT = F_OUT_BINARY
          OPEN(UNIT       = UNIT_VTK,
     &         FILE       = TRIM(FILENAME),
     &         FORM       = 'UNFORMATTED',
     &         ACCESS     = 'SEQUENTIAL',
     &         ACTION     = 'WRITE',
!     &         RECORDTYPE = 'STREAM',
!     &         BUFFERED   = 'YES',
     &         IOSTAT     = E_IO)
          ! WRITING HEADER OF FILE
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)'# vtk DataFile Version 3.0'//
     &                                    END_REC
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)TRIM(TITLE)//END_REC
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)
     &               TRIM(UPPER_CASE(OUTPUT_FORMAT))//END_REC
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)'DATASET '//TRIM(TOPOLOGY)//
     &               END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_INI

        !!\section{VTK\_GEO}
        !!
        !!VTK\_GEO is an interface to 8 different functions; there are 2 functions for each 4 different topologies actually supported:
        !!one function for mesh coordinates with R8P precision and one for mesh coordinates with R4P precision.
        !!This function must be called after VTK\_INI. It saves the mesh geometry. The inputs that must be passed change depending on
        !!the topologies choiced. Not all VTK topologies have been implemented (\virgo{polydata} topologies are absent). The signatures
        !!for all implemented topologies are now reported.
        !!
        !!\subsection{VTK\_GEO STRUCTURED POINTS}
        !!
        !!\begin{boxred}{}
        !!\begin{lstlisting}[style=signature,title=\color{Maroon}\MaiuscolettoBS{VTK\_GEO Structured Points Signature}]
        !! function VTK_GEO(NX,NY,NZ,X0,Y0,Z0,Dx,Dy,Dz) result(E_IO)
        !!\end{lstlisting}
        !!\end{boxred}
        !!
        !!The topology \virgo{structured points} is useful for structured grid with uniform discretization steps.
        !!
        !!\begin{boxred}{}
        !!\begin{lstlisting}[style=variables,title=\color{Maroon}\MaiuscolettoBS{VTK\_GEO Structured Points Variables}]
        !!integer(I4P),     intent(IN):: NX   ! number of nodes in x direction
        !!integer(I4P),     intent(IN):: NY   ! number of nodes in y direction
        !!integer(I4P),     intent(IN):: NZ   ! number of nodes in z direction
        !!real(R8P or R4P), intent(IN):: X0   ! x coordinate of origin
        !!real(R8P or R4P), intent(IN):: Y0   ! y coordinate of origin
        !!real(R8P or R4P), intent(IN):: Z0   ! z coordinate of origin
        !!real(R8P or R4P), intent(IN):: Dx   ! space step in x
        !!real(R8P or R4P), intent(IN):: Dy   ! space step in y
        !!real(R8P or R4P), intent(IN):: Dz   ! space step in z
        !!integer(I4P)::                 E_IO ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
        !!\end{lstlisting}
        !!\end{boxred}
        !!
        !!Note that the variables \texttt{X0,Y0,Z0,Dx,Dy,Dz} can be passed both as 8-byte real kind and 4-byte real kind; the dynamic
        !!displacement interface will call the correct function. Mixing 8-byte real kind and 4-byte real kind is not allowed: be sure
        !!that all variables are 8-byte real kind or all are 4-byte real kind.
        !!
        !!The VTK\_GEO structured point variables have the following meaning:
        !!
        !!\begin{description}
        !! \item[{\color{RoyalBlue}NX}] indicates the number of nodes in $X$ direction.
        !! \item[{\color{RoyalBlue}NY}] indicates the number of nodes in $Y$ direction.
        !! \item[{\color{RoyalBlue}NZ}] indicates the number of nodes in $Z$ direction.
        !! \item[{\color{RoyalBlue}X0}] indicates the $X$ value of coordinates system origin. It is a scalar.
        !! \item[{\color{RoyalBlue}Y0}] indicates the $Y$ value of coordinates system origin. It is a scalar.
        !! \item[{\color{RoyalBlue}Z0}] indicates the $Z$ value of coordinates system origin. It is a scalar.
        !! \item[{\color{RoyalBlue}Dx}] indicates the uniform grid step discretization in $X$ direction. It is a scalar.
        !! \item[{\color{RoyalBlue}Dy}] indicates the uniform grid step discretization in $Y$ direction. It is a scalar.
        !! \item[{\color{RoyalBlue}DZ}] indicates the uniform grid step discretization in $Z$ direction. It is a scalar.
        !! \item[{\color{RoyalBlue}E\_IO}] contains the inquiring integer flag for error handling.
        !!\end{description}
        !!
        !!The following is an example of VTK\_GEO structured point calling:
        !!
        !!\begin{boxred}{VTK\_GEO Structured Points Calling}
        !!\begin{verbatim}
        !!...
        !!integer(4):: NX,NY,NZ
        !!real(8):: X0,Y0,Z0
        !!real(8):: Dx,Dy,Dz
        !!...
        !!E_IO = VTK_GEO(NX,NY,NZ, &
        !!               X0,Y0,Z0,Dx,Dy,Dz)
        !!...
        !!\end{verbatim}
        !!\end{boxred}
        !!
        !!\subsection{VTK\_GEO STRUCTURED GRID}
        !!
        !!\begin{boxred}{}
        !!\begin{lstlisting}[style=signature,title=\color{Maroon}\MaiuscolettoBS{VTK\_GEO Structured Grid Signature}]
        !!function VTK_GEO(NX,NY,NZ,NN,X,Y,Z) result(E_IO)
        !!\end{lstlisting}
        !!\end{boxred}
        !!
        !!The topology \virgo{structured grid} is useful for structured grid with non-uniform discretization steps.
        !!
        !!\begin{boxred}{}
        !!\begin{lstlisting}[style=variables,title=\color{Maroon}\MaiuscolettoBS{VTK\_GEO Structured Grid Variables}]
        !!integer(I4P),     intent(IN):: NX      ! number of nodes in x direction
        !!integer(I4P),     intent(IN):: NY      ! number of nodes in y direction
        !!integer(I4P),     intent(IN):: NZ      ! number of nodes in z direction
        !!integer(I4P),     intent(IN):: NN      ! number of all nodes
        !!real(R8P or R4P), intent(IN):: X(1:NN) ! x coordinates
        !!real(R8P or R4P), intent(IN):: Y(1:NN) ! y coordinates
        !!real(R8P or R4P), intent(IN):: Z(1:NN) ! z coordinates
        !!integer(I4P)::                 E_IO    ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
        !!\end{lstlisting}
        !!\end{boxred}
        !!
        !!Note that the variables \texttt{X,Y,Z} can be passed both as 8-byte real kind and 4-byte real kind; the dynamic
        !!displacement interface will call the correct function. Mixing 8-byte real kind and 4-byte real kind is not allowed: be
        !!sure that all variables are 8-byte real kind or all are 4-byte real kind.
        !!
        !!The VTK\_GEO structured grid variables have the following meaning:
        !!
        !!\begin{description}
        !!  \item[{\color{RoyalBlue}NX}] indicates the number of nodes in $X$ direction.
        !!  \item[{\color{RoyalBlue}NY}] indicates the number of nodes in $Y$ direction.
        !!  \item[{\color{RoyalBlue}NZ}] indicates the number of nodes in $Z$ direction.
        !!  \item[{\color{RoyalBlue}NN}] indicates the number of all nodes, $NN= NX\cdot NY\cdot NZ$.
        !!  \item[{\color{RoyalBlue}X}] contains the $X$ coordinates values of all nodes. It is a vector of $[1:NN]$.
        !!  \item[{\color{RoyalBlue}Y}] contains the $Y$ coordinates values of all nodes. It is a vector of $[1:NN]$.
        !!  \item[{\color{RoyalBlue}Z}] contains the $Z$ coordinates values of all nodes. It is a vector of $[1:NN]$.
        !!  \item[{\color{RoyalBlue}E\_IO}] contains the inquiring integer flag for error handling.
        !!\end{description}
        !!
        !!The following is an example of VTK\_GEO structured grid calling:
        !!
        !!\begin{boxred}{VTK\_GEO Structured Grid Calling}
        !!\begin{verbatim}
        !!...
        !!integer(4), parameter:: NX=10,NY=10,NZ=10
        !!integer(4), parameter:: Nnodi=NX*NY*NZ
        !!real(8):: X(1:Nnodi),Y(1:Nnodi),Z(1:Nnodi)
        !!...
        !!E_IO = VTK_GEO(NX,NY,NZ,Nnodi,X,Y,Z)
        !!...
        !!\end{verbatim}
        !!\end{boxred}
        !!
        !!\subsection{VTK\_GEO RECTILINEAR GRID}
        !!
        !!\begin{boxred}{}
        !!\begin{lstlisting}[style=signature,title=\color{Maroon}\MaiuscolettoBS{VTK\_GEO Rectilinear Grid Signature}]
        !!function VTK_GEO(NX,NY,NZ,X,Y,Z) result(E_IO)
        !!\end{lstlisting}
        !!\end{boxred}
        !!
        !!The topology \virgo{rectilinear grid} is useful for structured grid with non-uniform discretization steps even
        !!in generalized coordinates.
        !!
        !!\begin{boxred}{}
        !!\begin{lstlisting}[style=variables,title=\color{Maroon}\MaiuscolettoBS{VTK\_GEO Rectilinear Grid Signature}]
        !!integer(I4P),     intent(IN):: NX      ! number of nodes in x direction
        !!integer(I4P),     intent(IN):: NY      ! number of nodes in y direction
        !!integer(I4P),     intent(IN):: NZ      ! number of nodes in z direction
        !!real(R8P or R4P), intent(IN):: X(1:NX) ! x coordinates
        !!real(R8P or R4P), intent(IN):: Y(1:NY) ! y coordinates
        !!real(R8P or R4P), intent(IN):: Z(1:NZ) ! z coordinates
        !!integer(I4P)::                 E_IO    ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
        !!\end{lstlisting}
        !!\end{boxred}
        !!
        !!Note that the variables \texttt{X,Y,Z} can be passed both as 8-byte real kind and 4-byte real kind; the dynamic
        !!displacement interface will call the correct function. Mixing 8-byte real kind and 4-byte real kind is not allowed: be
        !!sure that all variables are 8-byte real kind or all are 4-byte real kind.
        !!
        !!The VTK\_GEO rectilinear grid variables have the following meaning:
        !!
        !!\begin{description}
        !! \item[{\color{RoyalBlue}NX}] indicates the number of nodes in $X$ direction.
        !! \item[{\color{RoyalBlue}NY}] indicates the number of nodes in $Y$ direction.
        !! \item[{\color{RoyalBlue}NZ}] indicates the number of nodes in $Z$ direction.
        !! \item[{\color{RoyalBlue}X}] contains the $X$ coordinates values of nodes. It is a vector of $[1:NX]$.
        !! \item[{\color{RoyalBlue}Y}] contains the $Y$ coordinates values of nodes. It is a vector of $[1:NY]$.
        !! \item[{\color{RoyalBlue}Z}] contains the $Z$ coordinates values of nodes. It is a vector of $[1:NZ]$.
        !! \item[{\color{RoyalBlue}E\_IO}] contains the inquiring integer flag for error handling.
        !!\end{description}
        !!
        !!The following is an example of VTK\_GEO rectilinear grid calling:
        !!
        !!\begin{boxred}{VTK\_GEO Rectilinear Grid Calling}
        !!\begin{verbatim}
        !!...
        !!integer(4), parameter:: NX=10,NY=20,NZ=30
        !!real(4):: X(1:NX),Y(1:NY),Z(1:NZ)
        !!...
        !!E_IO = VTK_GEO(NX,NY,NZ,X,Y,Z)
        !!...
        !!\end{verbatim}
        !!\end{boxred}
        !!
        !!\subsection{VTK\_GEO UNSTRUCTURED GRID}
        !!
        !!\begin{boxred}{}
        !!\begin{lstlisting}[style=signature,title=\color{Maroon}\MaiuscolettoBS{VTK\_GEO Unstructured Grid Signature}]
        !!function VTK_GEO(Nnodi,X,Y,Z) result(E_IO)
        !!\end{lstlisting}
        !!\end{boxred}
        !!
        !!The topology \virgo{unstructured grid} is necessary for unstructured grid, the most general mesh format. This
        !!topology is also useful for scructured mesh in order to save only a non-structured clip of mesh.
        !!
        !!\begin{boxred}{}
        !!\begin{lstlisting}[style=variables,title=\color{Maroon}\MaiuscolettoBS{VTK\_GEO Unstructured Grid Variables}]
        !!integer(I4P),     intent(IN):: NN      ! number of nodes
        !!real(R8P or R4P), intent(IN):: X(1:NN) ! x coordinates of all nodes
        !!real(R8P or R4P), intent(IN):: Y(1:NN) ! y coordinates of all nodes
        !!real(R8P or R4P), intent(IN):: Z(1:NN) ! z coordinates of all nodes
        !!integer(I4P)::                 E_IO    ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
        !!\end{lstlisting}
        !!\end{boxred}
        !!
        !!Note that the variables \texttt{X,Y,Z} can be passed both as 8-byte real kind and 4-byte real kind; the dynamic
        !!displacement interface will call the correct function. Mixing 8-byte real kind and 4-byte real kind is not allowed: be
        !!sure that all variables are 8-byte real kind or all are 4-byte real kind.
        !!
        !!The VTK\_GEO unstructured grid variables have the following meaning:
        !!
        !!\begin{description}
        !! \item[{\color{RoyalBlue}NN}] indicates the number of all nodes.
        !! \item[{\color{RoyalBlue}X}] contains the $X$ coordinates values of nodes. It is a vector of $[1:NN]$.
        !! \item[{\color{RoyalBlue}Y}] contains the $Y$ coordinates values of nodes. It is a vector of $[1:NN]$.
        !! \item[{\color{RoyalBlue}Z}] contains the $Z$ coordinates values of nodes. It is a vector of $[1:NN]$.
        !! \item[{\color{RoyalBlue}E\_IO}] contains the inquiring integer flag for error handling.
        !!\end{description}
        !!
        !!The following is an example of VTK\_GEO unstructured grid calling:
        !!
        !!\begin{boxred}{VTK\_GEO Unstructured Grid Calling}
        !!\begin{verbatim}
        !!...
        !!integer(4), parameter:: NN=100
        !!real(4):: X(1:NN),Y(1:NN),Z(1:NN)
        !!...
        !!E_IO = VTK_GEO(NN,X,Y,Z)
        !!...
        !!\end{verbatim}
        !!\end{boxred}
        !!
        !!In order to use the \virgo{unstructured grid} it is necessary to save also the \virgo{connectivity} of the grid. The
        !!connectivity must be saved with the function \MaiuscolettoBS{VTK\_CON}.
        !!
        !(\doc)skippedblock
        FUNCTION VTK_GEO_STRP_R8(NX,NY,NZ,X0,Y0,Z0,DX,DY,DZ)
     &  RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! FUNCTION FOR SAVING MESH; TOPOLOGY = STRUCTURED\_POINTS (R8P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NX        ! NUMBER OF NODES IN X DIRECTION
        INTEGER(I4P), INTENT(IN):: NY        ! NUMBER OF NODES IN Y DIRECTION
        INTEGER(I4P), INTENT(IN):: NZ        ! NUMBER OF NODES IN Z DIRECTION
        REAL(R8P),    INTENT(IN):: X0        ! X COORDINATE OF ORIGIN
        REAL(R8P),    INTENT(IN):: Y0        ! Y COORDINATE OF ORIGIN
        REAL(R8P),    INTENT(IN):: Z0        ! Z COORDINATE OF ORIGIN
        REAL(R8P),    INTENT(IN):: DX        ! SPACE STEP IN X DIRECTION
        REAL(R8P),    INTENT(IN):: DY        ! SPACE STEP IN Y DIRECTION
        REAL(R8P),    INTENT(IN):: DZ        ! SPACE STEP IN Z DIRECTION
        INTEGER(I4P)::             E_IO      ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        CHARACTER(LEN=MAXLEN)::    S_BUFFER  ! BUFFER STRING
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          WRITE(UNIT=UNIT_VTK,FMT='(A,3'//FI4P//')', IOSTAT=E_IO)
     &          'DIMENSIONS ',NX,NY,NZ
          WRITE(UNIT=UNIT_VTK,FMT='(A,3'//FR8P//')', IOSTAT=E_IO)
     &          'ORIGIN ',X0,Y0,Z0
          WRITE(UNIT=UNIT_VTK,FMT='(A,3'//FR8P//')', IOSTAT=E_IO)
     &          'SPACING ',DX,DY,DZ
        CASE(F_OUT_BINARY)
          WRITE(S_BUFFER,     FMT='(A,3'//FI4P//')', IOSTAT=E_IO)
     &          'DIMENSIONS ',NX,NY,NZ
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &           TRIM(S_BUFFER)//END_REC
          WRITE(S_BUFFER,     FMT='(A,3'//FR8P//')', IOSTAT=E_IO)
     &           'ORIGIN ',X0,Y0,Z0
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &           TRIM(S_BUFFER)//END_REC
          WRITE(S_BUFFER,     FMT='(A,3'//FR8P//')', IOSTAT=E_IO)
     &           'SPACING ',DX,DY,DZ
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &           TRIM(S_BUFFER)//END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_GEO_STRP_R8

        FUNCTION VTK_GEO_STRP_R4(NX,NY,NZ,X0,Y0,Z0,DX,DY,DZ)
     &  RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! FUNCTION FOR SAVING MESH; TOPOLOGY = STRUCTURED\_POINTS (R4P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NX        ! NUMBER OF NODES IN X DIRECTION
        INTEGER(I4P), INTENT(IN):: NY        ! NUMBER OF NODES IN Y DIRECTION
        INTEGER(I4P), INTENT(IN):: NZ        ! NUMBER OF NODES IN Z DIRECTION
        REAL(R4P),    INTENT(IN):: X0        ! X COORDINATE OF ORIGIN
        REAL(R4P),    INTENT(IN):: Y0        ! Y COORDINATE OF ORIGIN
        REAL(R4P),    INTENT(IN):: Z0        ! Z COORDINATE OF ORIGIN
        REAL(R4P),    INTENT(IN):: DX        ! SPACE STEP IN X DIRECTION
        REAL(R4P),    INTENT(IN):: DY        ! SPACE STEP IN Y DIRECTION
        REAL(R4P),    INTENT(IN):: DZ        ! SPACE STEP IN Z DIRECTION
        INTEGER(I4P)::             E_IO      ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        CHARACTER(LEN=MAXLEN)::    S_BUFFER  ! BUFFER STRING
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          WRITE(UNIT=UNIT_VTK,FMT='(A,3'//FI4P//')', IOSTAT=E_IO)
     &          'DIMENSIONS ',NX,NY,NZ
          WRITE(UNIT=UNIT_VTK,FMT='(A,3'//FR4P//')', IOSTAT=E_IO)
     &          'ORIGIN ',X0,Y0,Z0
          WRITE(UNIT=UNIT_VTK,FMT='(A,3'//FR4P//')', IOSTAT=E_IO)
     &          'SPACING ',DX,DY,DZ
        CASE(F_OUT_BINARY)
          WRITE(S_BUFFER,     FMT='(A,3'//FI4P//')', IOSTAT=E_IO)
     &          'DIMENSIONS ',NX,NY,NZ
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(S_BUFFER,     FMT='(A,3'//FR4P//')', IOSTAT=E_IO)
     &          'ORIGIN ',X0,Y0,Z0
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(S_BUFFER,     FMT='(A,3'//FR4P//')', IOSTAT=E_IO)
     &          'SPACING ',DX,DY,DZ
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_GEO_STRP_R4

        FUNCTION VTK_GEO_STRG_R8(NX,NY,NZ,NN,X,Y,Z) RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! FUNCTION FOR SAVING MESH; TOPOLOGY = STRUCTURED\_GRID (R8P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NX       ! NUMBER OF NODES IN X DIRECTION
        INTEGER(I4P), INTENT(IN):: NY       ! NUMBER OF NODES IN Y DIRECTION
        INTEGER(I4P), INTENT(IN):: NZ       ! NUMBER OF NODES IN Z DIRECTION
        INTEGER(I4P), INTENT(IN):: NN       ! NUMBER OF ALL NODES
        REAL(R8P),    INTENT(IN):: X(1:NN)  ! X COORDINATES
        REAL(R8P),    INTENT(IN):: Y(1:NN)  ! Y COORDINATES
        REAL(R8P),    INTENT(IN):: Z(1:NN)  ! Z COORDINATES
        INTEGER(I4P)::             E_IO     ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        CHARACTER(LEN=MAXLEN)::    S_BUFFER ! BUFFER STRING
        INTEGER(I4P)::             N1       ! COUNTER
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          WRITE(UNIT=UNIT_VTK,FMT='(A,3'//FI4P//')', IOSTAT=E_IO)
     &          'DIMENSIONS ',NX,NY,NZ
          WRITE(UNIT=UNIT_VTK,FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'POINTS ',NN,' double'
          WRITE(UNIT=UNIT_VTK,FMT='(3'//FR8P//')',   IOSTAT=E_IO)
     &          (X(N1),Y(N1),Z(N1),N1=1,NN)
        CASE(F_OUT_BINARY)
          WRITE(S_BUFFER,     FMT='(A,3'//FI4P//')', IOSTAT=E_IO)
     &          'DIMENSIONS ',NX,NY,NZ
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(S_BUFFER,     FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'POINTS ',NN,' double'
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          (X(N1),Y(N1),Z(N1),N1=1,NN)
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_GEO_STRG_R8

        FUNCTION VTK_GEO_STRG_R4(NX,NY,NZ,NN,X,Y,Z) RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! FUNCTION FOR SAVING MESH; TOPOLOGY = STRUCTURED\_GRID (R4P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NX       ! NUMBER OF NODES IN X DIRECTION
        INTEGER(I4P), INTENT(IN):: NY       ! NUMBER OF NODES IN Y DIRECTION
        INTEGER(I4P), INTENT(IN):: NZ       ! NUMBER OF NODES IN Z DIRECTION
        INTEGER(I4P), INTENT(IN):: NN       ! NUMBER OF ALL NODES
        REAL(R4P),    INTENT(IN):: X(1:NN)  ! X COORDINATES
        REAL(R4P),    INTENT(IN):: Y(1:NN)  ! Y COORDINATES
        REAL(R4P),    INTENT(IN):: Z(1:NN)  ! Z COORDINATES
        INTEGER(I4P)::             E_IO     ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        CHARACTER(LEN=MAXLEN)::    S_BUFFER ! BUFFER STRING
        INTEGER(I4P)::             N1       ! COUNTER
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          WRITE(UNIT=UNIT_VTK,FMT='(A,3'//FI4P//')', IOSTAT=E_IO)
     &          'DIMENSIONS ',NX,NY,NZ
          WRITE(UNIT=UNIT_VTK,FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'POINTS ',NN,' float'
          WRITE(UNIT=UNIT_VTK,FMT='(3'//FR4P//')',   IOSTAT=E_IO)
     &          (X(N1),Y(N1),Z(N1),N1=1,NN)
        CASE(F_OUT_BINARY)
          WRITE(S_BUFFER,     FMT='(A,3'//FI4P//')', IOSTAT=E_IO)
     &          'DIMENSIONS ',NX,NY,NZ
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(S_BUFFER,     FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'POINTS ',NN,' float'
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          (X(N1),Y(N1),Z(N1),N1=1,NN)
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_GEO_STRG_R4

        FUNCTION VTK_GEO_RECT_R8(NX,NY,NZ,X,Y,Z) RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! FUNCTION FOR SAVING MESH; TOPOLOGY = RECTILINEAR\_GRID (R8P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NX        ! NUMBER OF NODES IN X DIRECTION
        INTEGER(I4P), INTENT(IN):: NY        ! NUMBER OF NODES IN Y DIRECTION
        INTEGER(I4P), INTENT(IN):: NZ        ! NUMBER OF NODES IN Z DIRECTION
        REAL(R8P),    INTENT(IN):: X(1:NX)   ! X COORDINATES
        REAL(R8P),    INTENT(IN):: Y(1:NY)   ! Y COORDINATES
        REAL(R8P),    INTENT(IN):: Z(1:NZ)   ! Z COORDINATES
        INTEGER(I4P)::             E_IO      ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        CHARACTER(LEN=MAXLEN)::    S_BUFFER  ! BUFFER STRING
        INTEGER(I4P)::             N1        ! COUNTER
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          WRITE(UNIT=UNIT_VTK,FMT='(A,3'//FI4P//')', IOSTAT=E_IO)
     &          'DIMENSIONS ',NX,NY,NZ
          WRITE(UNIT=UNIT_VTK,FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'X_COORDINATES ',NX,' double'
          WRITE(UNIT=UNIT_VTK,FMT=FR8P,              IOSTAT=E_IO)
     &          (X(N1),N1=1,NX)
          WRITE(UNIT=UNIT_VTK,FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'Y_COORDINATES ',NY,' double'
          WRITE(UNIT=UNIT_VTK,FMT=FR8P,              IOSTAT=E_IO)
     &          (Y(N1),N1=1,NY)
          WRITE(UNIT=UNIT_VTK,FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'Z_COORDINATES ',NZ,' double'
          WRITE(UNIT=UNIT_VTK,FMT=FR8P,              IOSTAT=E_IO)
     &          (Z(N1),N1=1,NZ)
        CASE(F_OUT_BINARY)
          WRITE(S_BUFFER,     FMT='(A,3'//FI4P//')', IOSTAT=E_IO)
     &          'DIMENSIONS ',NX,NY,NZ
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(S_BUFFER,     FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'X_COORDINATES ',NX,' double'
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          (X(N1),N1=1,NX)
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)END_REC
          WRITE(S_BUFFER,     FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'Y_COORDINATES ',NY,' double'
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          (Y(N1),N1=1,NY)
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)END_REC
          WRITE(S_BUFFER,     FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'Z_COORDINATES ',NZ,' double'
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          (Z(N1),N1=1,NZ)
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_GEO_RECT_R8

        FUNCTION VTK_GEO_RECT_R4(NX,NY,NZ,X,Y,Z) RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! FUNCTION FOR SAVING MESH; TOPOLOGY = RECTILINEAR\_GRID (R4P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NX        ! NUMBER OF NODES IN X DIRECTION
        INTEGER(I4P), INTENT(IN):: NY        ! NUMBER OF NODES IN Y DIRECTION
        INTEGER(I4P), INTENT(IN):: NZ        ! NUMBER OF NODES IN Z DIRECTION
        REAL(R4P),    INTENT(IN):: X(1:NX)   ! X COORDINATES
        REAL(R4P),    INTENT(IN):: Y(1:NY)   ! Y COORDINATES
        REAL(R4P),    INTENT(IN):: Z(1:NZ)   ! Z COORDINATES
        INTEGER(I4P)::             E_IO      ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        CHARACTER(LEN=MAXLEN)::    S_BUFFER  ! BUFFER STRING
        INTEGER(I4P)::             N1        ! COUNTER
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          WRITE(UNIT=UNIT_VTK,FMT='(A,3'//FI4P//')', IOSTAT=E_IO)
     &          'DIMENSIONS ',NX,NY,NZ
          WRITE(UNIT=UNIT_VTK,FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'X_COORDINATES ',NX,' float'
          WRITE(UNIT=UNIT_VTK,FMT=FR4P,              IOSTAT=E_IO)
     &          (X(N1),N1=1,NX)
          WRITE(UNIT=UNIT_VTK,FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'Y_COORDINATES ',NY,' float'
          WRITE(UNIT=UNIT_VTK,FMT=FR4P,              IOSTAT=E_IO)
     &          (Y(N1),N1=1,NY)
          WRITE(UNIT=UNIT_VTK,FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'Z_COORDINATES ',NZ,' float'
          WRITE(UNIT=UNIT_VTK,FMT=FR4P,              IOSTAT=E_IO)
     &          (Z(N1),N1=1,NZ)
        CASE(F_OUT_BINARY)
          WRITE(S_BUFFER,     FMT='(A,3'//FI4P//')', IOSTAT=E_IO)
     &          'DIMENSIONS ',NX,NY,NZ
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(S_BUFFER,     FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'X_COORDINATES ',NX,' float'
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          (X(N1),N1=1,NX)
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)END_REC
          WRITE(S_BUFFER,     FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'Y_COORDINATES ',NY,' float'
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          (Y(N1),N1=1,NY)
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)END_REC
          WRITE(S_BUFFER,     FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'Z_COORDINATES ',NZ,' float'
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          (Z(N1),N1=1,NZ)
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_GEO_RECT_R4

        FUNCTION VTK_GEO_UNST_R8(NN,X,Y,Z) RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! FUNCTION FOR SAVING MESH; TOPOLOGY = UNSTRUCTURED\_GRID (R8P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NN        ! NUMBER OF NODES
        REAL(R8P),    INTENT(IN):: X(1:NN)   ! X COORDINATES OF ALL NODES
        REAL(R8P),    INTENT(IN):: Y(1:NN)   ! Y COORDINATES OF ALL NODES
        REAL(R8P),    INTENT(IN):: Z(1:NN)   ! Z COORDINATES OF ALL NODES
        INTEGER(I4P)::             E_IO      ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        CHARACTER(LEN=MAXLEN)::    S_BUFFER  ! BUFFER STRING
        INTEGER(I4P)::             N1        ! COUNTER
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          WRITE(UNIT=UNIT_VTK,FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'POINTS ',NN,' double'
          WRITE(UNIT=UNIT_VTK,FMT='(3'//FR8P//')',   IOSTAT=E_IO)
     &          (X(N1),Y(N1),Z(N1),N1=1,NN)
        CASE(F_OUT_BINARY)
          WRITE(S_BUFFER,     FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'POINTS ',NN,' double'
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          (X(N1),Y(N1),Z(N1),N1=1,NN)
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_GEO_UNST_R8

        FUNCTION VTK_GEO_UNST_R4(NN,X,Y,Z) RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! FUNCTION FOR SAVING MESH; TOPOLOGY = UNSTRUCTURED\_GRID (R4P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NN        ! NUMBER OF NODES
        REAL(R4P),    INTENT(IN):: X(1:NN)   ! X COORDINATES OF ALL NODES
        REAL(R4P),    INTENT(IN):: Y(1:NN)   ! Y COORDINATES OF ALL NODES
        REAL(R4P),    INTENT(IN):: Z(1:NN)   ! Z COORDINATES OF ALL NODES
        INTEGER(I4P)::             E_IO      ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        CHARACTER(LEN=MAXLEN)::    S_BUFFER  ! BUFFER STRING
        INTEGER(I4P)::             N1        ! COUNTER
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          WRITE(UNIT=UNIT_VTK,FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'POINTS ',NN,' float'
          WRITE(UNIT=UNIT_VTK,FMT='(3'//FR4P//')',   IOSTAT=E_IO)
     &          (X(N1),Y(N1),Z(N1),N1=1,NN)
        CASE(F_OUT_BINARY)
          WRITE(S_BUFFER,     FMT='(A,'//FI4P//',A)',IOSTAT=E_IO)
     &          'POINTS ',NN,' float'
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)
     &          (X(N1),Y(N1),Z(N1),N1=1,NN)
          WRITE(UNIT=UNIT_VTK,                       IOSTAT=E_IO)END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_GEO_UNST_R4
        !(DOC/)SKIPPEDBLOCK

        FUNCTION VTK_CON(NC,CONNECT,CELL_TYPE) RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !!THIS FUNCTION \MAIUSCOLETTOBS{MUST} BE USED WHEN UNSTRUCTURED GRID IS USED. IT SAVES THE CONNECTIVITY OF THE UNSTRUCTURED
        !!MESH.
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NC              ! NUMBER OF CELLS
        INTEGER(I4P), INTENT(IN):: CONNECT(:)      ! MESH CONNECTIVITY
        INTEGER(I4P), INTENT(IN):: CELL_TYPE(1:NC) ! VTK CELL TYPE
        INTEGER(I4P)::             E_IO            ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        CHARACTER(LEN=MAXLEN)::    S_BUFFER        ! BUFFER STRING
        INTEGER(I4P)::             NCON            ! DIMENSION OF CONNECTIVITY VECTOR 
        !!THE VTK\_CON VARIABLES HAVE THE FOLLOWING MEANING:
        !!
        !!\BEGIN{DESCRIPTION}
        !! \ITEM[{\COLOR{ROYALBLUE}NC}] INDICATES THE NUMBER OF ALL CELLS.
        !! \ITEM[{\COLOR{ROYALBLUE}CONNECT}] CONTAINS THE CONNECTIVITY OF THE MESH. IT IS A VECTOR.
        !! \ITEM[{\COLOR{ROYALBLUE}CELL\_TYPE}] CONTAINS THE TYPE OF EVERY CELLS. IT IS A VECTOR OF $[1:NC]$.
        !! \ITEM[{\COLOR{ROYALBLUE}E\_IO}] CONTAINS THE INQUIRING INTEGER FLAG FOR ERROR HANDLING.
        !!\END{DESCRIPTION}
        !!
        !!THE VECTOR \MAIUSCOLETTOBS{CONNECT} MUST FOLLOW THE VTK LEGACY STANDARD. IT IS PASSED AS \MAIUSCOLETTOBS{ASSUMED-SHAPE} ARRAY
        !!BECAUSE ITS DIMENSIONS IS RELATED TO THE MESH DIMENSIONS IN A COMPLEX WAY. ITS DIMENSIONS CAN BE CALCULATED BY THE FOLLOWING
        !!EQUATION:
        !!
        !!\BEGIN{EQUATION}
        !!DC = NC + \SUM\LIMITS_{I = 1}^{NC} {NVERTEX_I }
        !!\LABEL{EQ:CONNECTIVITY DIMENSIONS}
        !!\END{EQUATION}
        !!
        !!\NOINDENT WHERE $DC$ IS CONNECTIVITY VECTOR DIMENSION AND $NVERTEX_I$ IS THE NUMBER OF VERTICES OF $I^{TH}$ CELL. THE VTK
        !!LEGACY STANDARD FOR THE MESH CONNECTIVITY IS QUITE OBSCURE AT LEAST AT FIRST SIGHT. IT IS MORE SIMPLE ANALIZING AN EXAMPLE.
        !!SUPPOSE WE HAVE A MESH COMPOSED BY 2 CELLS, ONE HEXAHEDRON (8 VERTICES) AND ONE PYRAMID WITH SQUARE BASIS (5 VERTICES); SUPPOSE
        !!THAT THE BASIS OF PYRAMID IS CONSTITUTE BY A FACE OF THE HEXAHEDRON AND SO THE TWO CELLS SHARE 4 VERTICES. THE EQUATION
        !!\REF{EQ:CONNECTIVITY DIMENSIONS} GIVES $DC=2+8+5=15$; THE CONNECTIVITY VECTOR FOR THIS MESH CAN BE:
        !!
        !!\BEGIN{BOXRED}{CONNECTIVITY VECTOR EXAMPLE FOR VTK LEGACY STANDARD}
        !!\BEGIN{VERBATIM}
        !!! FIRST CELL
        !!CONNECT(1)  = 8  => NUMBER OF VERTICES OF 1° CELL
        !!CONNECT(2)  = 0  => IDENTIFICATION FLAG OF 1° VERTEX OF 1° CELL
        !!CONNECT(3)  = 1  => IDENTIFICATION FLAG OF 2° VERTEX OF 1° CELL
        !!CONNECT(4)  = 2  => IDENTIFICATION FLAG OF 3° VERTEX OF 1° CELL
        !!CONNECT(5)  = 3  => IDENTIFICATION FLAG OF 4° VERTEX OF 1° CELL
        !!CONNECT(6)  = 4  => IDENTIFICATION FLAG OF 5° VERTEX OF 1° CELL
        !!CONNECT(7)  = 5  => IDENTIFICATION FLAG OF 6° VERTEX OF 1° CELL
        !!CONNECT(8)  = 6  => IDENTIFICATION FLAG OF 7° VERTEX OF 1° CELL
        !!CONNECT(9)  = 7  => IDENTIFICATION FLAG OF 8° VERTEX OF 1° CELL
        !!! SECOND CELL
        !!CONNECT(10) = 5  => NUMBER OF VERTICES OF 2° CELL
        !!CONNECT(11) = 0  => IDENTIFICATION FLAG OF 1° VERTEX OF 2° CELL
        !!CONNECT(12) = 1  => IDENTIFICATION FLAG OF 2° VERTEX OF 2° CELL
        !!CONNECT(13) = 2  => IDENTIFICATION FLAG OF 3° VERTEX OF 2° CELL
        !!CONNECT(14) = 3  => IDENTIFICATION FLAG OF 4° VERTEX OF 2° CELL
        !!CONNECT(15) = 8  => IDENTIFICATION FLAG OF 5° VERTEX OF 2° CELL
        !!\END{VERBATIM}
        !!\END{BOXRED}
        !!
        !!\NOINDENT NOTE THAT THE FIRST 4 IDENTIFICATION FLAGS OF PYRAMID VERTICES AS THE SAME OF THE FIRST 4 IDENTIFICATION FLAGS OF
        !!THE HEXAHEDRON BECAUSE THE TWO CELLS SHARE THIS FACE. IT IS ALSO IMPORTANT TO NOTE THAT THE IDENTIFICATION FLAGS START
        !!FORM $0$ VALUE: THIS IS IMPOSE TO THE VTK STANDARD. THE FUNCTION VTK\_CON DOES NOT CALCULATE THE CONNECTIVITY VECTOR: IT
        !!WRITES THE CONNECTIVITY VECTOR CONFORMING THE VTK STANDARD, BUT DOES NOT CALCULATE IT. IN THE FUTURE RELEASE OF \LIBVTKIO WILL
        !!BE INCLUDED A FUNCTION TO CALCULATE THE CONNECTIVITY VECTOR.
        !!
        !!THE VECTOR VARIABLE \MAIUSCOLETTOBS{TIPO} MUST CONFORM THE VTK STANDARD \FOOTNOTE{SEE THE FILE VTK-STANDARD AT THE KITWARE
        !!HOMEPAGE.}. IT CONTAINS THE \EMPH{TYPE} OF EACH CELLS. FOR THE ABOVE EXAMPLE THIS VECTOR IS:
        !!
        !!\BEGIN{BOXRED}{CELL-TYPE VECTOR EXAMPLE FOR VTK LEGACY STANDARD}
        !!\BEGIN{VERBATIM}
        !!TIPO(1) = 12  => VTK HEXAHEDRON TYPE OF 1° CELL
        !!TIPO(2) = 14  => VTK PYRAMID TYPE OF 2° CELL
        !!\END{VERBATIM}
        !!\END{BOXRED}
        !!
        !!THE FOLLOWING IS AN EXAMPLE OF VTK\_CON CALLING:
        !!
        !!\BEGIN{BOXRED}{VTK\_CON CALLING}
        !!\BEGIN{VERBATIM}
        !!...
        !!INTEGER(4), PARAMETER:: NC=2
        !!INTEGER(4), PARAMETER:: NVERTEX1=8
        !!INTEGER(4), PARAMETER:: NVERTEX2=5
        !!INTEGER(4), PARAMETER:: DC=NC+NVERTEX1+NVERTEX2
        !!INTEGER(4)::            CONNECT(1:DC)
        !!INTEGER(4)::            CELL_TYPE(1:NC)
        !!...
        !!E_IO = VTK_CON(NC,CONNECT,CELL_TYPE)
        !!...
        !!\END{VERBATIM}
        !!\END{BOXRED}
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        NCON = SIZE(CONNECT,1)
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          WRITE(UNIT=UNIT_VTK,FMT='(A,2'//FI4P//')',IOSTAT=E_IO)
     &          'CELLS ',NC,NCON
          WRITE(UNIT=UNIT_VTK,FMT=FI4P,             IOSTAT=E_IO)CONNECT
          WRITE(UNIT=UNIT_VTK,FMT='(A,'//FI4P//')', IOSTAT=E_IO)
     &          'CELL_TYPES ',NC
          WRITE(UNIT=UNIT_VTK,FMT=FI4P,             IOSTAT=E_IO)
     &          CELL_TYPE
        CASE(F_OUT_BINARY)
          WRITE(S_BUFFER,     FMT='(A,2'//FI4P//')',IOSTAT=E_IO)
     &          'CELLS ',NC,NCON
          WRITE(UNIT=UNIT_VTK,                      IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(UNIT=UNIT_VTK,                      IOSTAT=E_IO)CONNECT
          WRITE(UNIT=UNIT_VTK,                      IOSTAT=E_IO)END_REC
          WRITE(S_BUFFER,     FMT='(A,'//FI4P//')', IOSTAT=E_IO)
     &          'CELL_TYPES ',NC
          WRITE(UNIT=UNIT_VTK,                      IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(UNIT=UNIT_VTK,                      IOSTAT=E_IO)
     &          CELL_TYPE
          WRITE(UNIT=UNIT_VTK,                      IOSTAT=E_IO)END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_CON

        FUNCTION VTK_DAT(NC_NN,VAR_LOCATION) RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !!THIS FUNCTION \MAIUSCOLETTOBS{MUST} BE CALLED BEFORE SAVING THE DATA RELATED TO GEOMETRIC MESH. THIS FUNCTION INITIALIZES THE
        !!SAVING OF DATA VARIABLES INDICATING THE \EMPH{TYPE} OF VARIABLES THAT WILL BE SAVED.
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NC_NN        ! NUMBER OF CELLS OR NODES OF FIELD
        CHARACTER(*), INTENT(IN):: VAR_LOCATION ! LOCATION OF SAVING VARIABLES: CELL FOR CELL-CENTERED, NODE FOR NODE-CENTERED
        INTEGER(I4P)::             E_IO         ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        CHARACTER(LEN=MAXLEN)::    S_BUFFER     ! BUFFER STRING
        !!THE VTK\_DAT VARIABLES HAVE THE FOLLOWING MEANING:
        !!
        !!\BEGIN{DESCRIPTION}
        !! \ITEM[{\COLOR{ROYALBLUE}NC\_NN}] INDICATES THE NUMBER OF ALL CELLS OR ALL NODES ACCORDING TO THE VALUE OF {\COLOR{ROYALBLUE}TIPO}.
        !! \ITEM[{\COLOR{ROYALBLUE}VAR\_LOCATION}] CONTAINS THE LOCATION-TYPE OF VARIABLES THAT WILL BE SAVED AFTER VTK\_DAT. IT IS A SCALAR AND CAB ASSUME THE FOLLOWING VALUES:
        !! \BEGIN{ENUMERATEABLU}
        !!  \ITEM \EMPH{CELL} (IT IS CASE INSENSITIVE) $\RIGHTARROW$ VARIABLES WILL BE CELL-CENTERED.
        !!  \ITEM \EMPH{NODE} (IT IS CASE INSENSITIVE) $\RIGHTARROW$ VARIABLES WILL BE NODE-CENTERED.
        !! \END{ENUMERATEABLU}
        !! \ITEM[{\COLOR{ROYALBLUE}E\_IO}] CONTAINS THE INQUIRING INTEGER FLAG FOR ERROR HANDLING.
        !!\END{DESCRIPTION}
        !!
        !!OF COURSE A SINGLE FILE CAN CONTAIN BOTH CELL AND NODE CENTERED VARIABLES; IN THIS CASE THE VTK\_DAT FUNCTION MUST BE CALLED TWO TIMES, BEFORE SAVING CELL-CENTERED VARIABLES AND BEFORE SAVING NODE-CENTERED VARIABLES.
        !!
        !!THE FOLLOWING IS AN EXAMPLE OF VTK\_DAT CALLING:
        !!
        !!\BEGIN{BOXRED}{VTK\_DAT CALLING}
        !!\BEGIN{VERBATIM}
        !!...
        !!E_IO = VTK_DAT(50,'NODE')
        !!...
        !!\END{VERBATIM}
        !!\END{BOXRED}
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          SELECT CASE(TRIM(UPPER_CASE(VAR_LOCATION)))
          CASE('CELL')
            WRITE(UNIT=UNIT_VTK,FMT='(A,'//FI4P//')',IOSTAT=E_IO)
     &            'CELL_DATA ',NC_NN
          CASE('NODE')
            WRITE(UNIT=UNIT_VTK,FMT='(A,'//FI4P//')',IOSTAT=E_IO)
     &            'POINT_DATA ',NC_NN
          ENDSELECT
        CASE(F_OUT_BINARY)
          SELECT CASE(TRIM(UPPER_CASE(VAR_LOCATION)))
          CASE('CELL')
            WRITE(S_BUFFER,     FMT='(A,'//FI4P//')',IOSTAT=E_IO)
     &          'CELL_DATA ',NC_NN
            WRITE(UNIT=UNIT_VTK,                     IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          CASE('NODE')
            WRITE(S_BUFFER,     FMT='(A,'//FI4P//')',IOSTAT=E_IO)
     &          'POINT_DATA ',NC_NN
            WRITE(UNIT=UNIT_VTK,                     IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          ENDSELECT
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_DAT

        !!\SECTION{VTK\_VAR}
        !!
        !!VTK\_VAR IS AN INTERFACE TO 8 DIFFERENT FUNCTIONS; THERE ARE 3 FUNCTIONS FOR SCALAR VARIABLES, 3 FUNCTIONS FOR VECTORIAL
        !!VARIABLES AND 2 FUNCTION TEXTURE VARIABLES.
        !!THIS FUNCTION SAVES THE DATA VARIABLES RELATED TO GEOMETRIC MESH. THE INPUTS THAT MUST BE PASSED CHANGE DEPENDING ON THE DATA
        !!VARIABLES TYPE.
        !!
        !!\SUBSECTION{VTK\_VAR SCALAR DATA}
        !!
        !!\BEGIN{BOXRED}{}
        !!\BEGIN{LSTLISTING}[STYLE=SIGNATURE,TITLE=\COLOR{MAROON}\MAIUSCOLETTOBS{VTK\_VAR SCALAR DATA SIGNATURE}]
        !!FUNCTION VTK_VAR(FORMATO,NC_NN,VARNAME,VAR) RESULT(E_IO)
        !!\END{LSTLISTING}
        !!\END{BOXRED}
        !!
        !!THIS KIND OF CALL IS USED TO SAVE SCALAR DATA.
        !!
        !!\BEGIN{BOXRED}{}
        !!\BEGIN{LSTLISTING}[STYLE=VARIABLES,TITLE=\COLOR{MAROON}\MAIUSCOLETTOBS{VTK\_VAR SCALAR DATA VARIABLES}]
        !!INTEGER(I4P),                     INTENT(IN):: NC_NN        ! NUMBER OF NODES OR CELLS
        !!CHARACTER(*),                     INTENT(IN):: VARNAME      ! VARIABLE NAME
        !!REAL(R8P OR R4P) OR INTEGER(I4P), INTENT(IN):: VAR(1:NC_NN) ! VARIABLE TO BE SAVED
        !!INTEGER(I4P)::                                 E_IO         ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        !!\END{LSTLISTING}
        !!\END{BOXRED}
        !!
        !!THE VTK\_VAR VARIABLES HAVE THE FOLLOWING MEANING:
        !!
        !!\BEGIN{DESCRIPTION}
        !! \ITEM[{\COLOR{ROYALBLUE}NC\_NN}] INDICATES THE NUMBER OF ALL CELLS OR ALL NODES ACCORDING TO THE VALUE OF
        !!                                  {\COLOR{ROYALBLUE}TIPO} PASSED TO VTK\_DAT.
        !! \ITEM[{\COLOR{ROYALBLUE}VARNAME}] CONTAINS THE NAME ATTRIBUITED THE VARIABLE SAVED.
        !! \ITEM[{\COLOR{ROYALBLUE}VAR}] CONTAINS THE VALUES OF VARIABLES IN EACH NODES OR CELLS. IT IS A VECTOR OF $[1:NC\_NN]$.
        !! \ITEM[{\COLOR{ROYALBLUE}E\_IO}] CONTAINS THE INQUIRING INTEGER FLAG FOR ERROR HANDLING.
        !!\END{DESCRIPTION}
        !!
        !!NOTE THAT THE VARIABLES \TEXTTT{VAR} CAN BE PASSED BOTH AS 8-BYTE REAL KIND, 4-BYTE REAL KIND AND 4-BYTE INTEGER; THE
        !!DYNAMIC DISPLACEMENT INTERFACE WILL CALL THE CORRECT FUNCTION.
        !!
        !!THE FOLLOWING IS AN EXAMPLE OF VTK\_VAR SCALAR DATA CALLING:
        !!
        !!\BEGIN{BOXRED}{VTK\_VAR SCALAR DATA CALLING}
        !!\BEGIN{VERBATIM}
        !!...
        !!INTEGER(4), PARAMETER:: NC_NN=100
        !!REAL(4)::               VAR(1:NC_NN)
        !!...
        !!E_IO = VTK_VAR(NC_NN,'SCALAR DATA',VAR)
        !!...
        !!\END{VERBATIM}
        !!\END{BOXRED}
        !!
        !!\SUBSECTION{VTK\_VAR REAL VECTORIAL DATA}
        !!
        !!\BEGIN{BOXRED}{}
        !!\BEGIN{LSTLISTING}[STYLE=SIGNATURE,TITLE=\COLOR{MAROON}\MAIUSCOLETTOBS{VTK\_VAR REAL VECTORIAL DATA SIGNATURE}]
        !!FUNCTION VTK_VAR(TIPO,NC_NN,VARNAME,VARX,VARY,VARZ) RESULT(E_IO)
        !!\END{LSTLISTING}
        !!\END{BOXRED}
        !!
        !!THIS KIND OF CALL IS USED TO SAVE REAL VECTORIAL DATA.
        !!
        !!\BEGIN{BOXRED}{}
        !!\BEGIN{LSTLISTING}[STYLE=VARIABLES,TITLE=\COLOR{MAROON}\MAIUSCOLETTOBS{VTK\_VAR REAL VECTORIAL DATA VARIABLES}]
        !!CHARACTER(*),     INTENT(IN):: VEC_TYPE      ! VECTOR TYPE: VECT = GENERIC VECTOR , NORM = NORMAL VECTOR
        !!INTEGER(I4P),     INTENT(IN):: NC_NN         ! NUMBER OF NODES OR CELLS
        !!CHARACTER(*),     INTENT(IN):: VARNAME       ! VARIABLE NAME
        !!REAL(R8P OR R4P), INTENT(IN):: VARX(1:NC_NN) ! X COMPONENT OF VECTOR
        !!REAL(R8P OR R4P), INTENT(IN):: VARY(1:NC_NN) ! Y COMPONENT OF VECTOR
        !!REAL(R8P OR R4P), INTENT(IN):: VARZ(1:NC_NN) ! Z COMPONENT OF VECTOR
        !!INTEGER(I4P)::                 E_IO          ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        !!\END{LSTLISTING}
        !!\END{BOXRED}
        !!
        !!THE VTK\_VAR VARIABLES HAVE THE FOLLOWING MEANING:
        !!
        !!\BEGIN{DESCRIPTION}
        !! \ITEM [{\COLOR{ROYALBLUE}TIPO}] INDICATES THE TYPE OF VECTOR. IT CAN ASSUME THE FOLLOWING VALUE:
        !! \BEGIN{ENUMERATEABLU}
        !!  \ITEM \EMPH{VECT} $\RIGHTARROW$ GENERIC VECTOR.
        !!  \ITEM \EMPH{NORM} $\RIGHTARROW$ NORMAL VECTOR OF FACE.
        !! \END{ENUMERATEABLU}
        !! \ITEM[{\COLOR{ROYALBLUE}NC\_NN}] INDICATES THE NUMBER OF ALL CELLS OR ALL NODES ACCORDING TO THE VALUE OF
        !!                                  {\COLOR{ROYALBLUE}TIPO} PASSED TO VTK\_DAT.
        !! \ITEM[{\COLOR{ROYALBLUE}VARNAME}] CONTAINS THE NAME ATTRIBUITED THE VARIABLE SAVED.
        !! \ITEM[{\COLOR{ROYALBLUE}VARX}] CONTAINS THE VALUES OF $X$ COMPONENT IN EACH NODES OR CELLS. IT IS A VECTOR OF $[1:NC\_NN]$.
        !! \ITEM[{\COLOR{ROYALBLUE}VARY}] CONTAINS THE VALUES OF $Y$ COMPONENT IN EACH NODES OR CELLS. IT IS A VECTOR OF $[1:NC\_NN]$.
        !! \ITEM[{\COLOR{ROYALBLUE}VARZ}] CONTAINS THE VALUES OF $Z$ COMPONENT IN EACH NODES OR CELLS. IT IS A VECTOR OF $[1:NC\_NN]$.
        !! \ITEM[{\COLOR{ROYALBLUE}E\_IO}] CONTAINS THE INQUIRING INTEGER FLAG FOR ERROR HANDLING.
        !!\END{DESCRIPTION}
        !!
        !!NOTE THAT THE VARIABLES \TEXTTT{VARX,VARY,VARZ} CAN BE PASSED BOTH AS 8-BYTE REAL KIND AND 4-BYTE REAL KIND; THE DYNAMIC
        !!DISPLACEMENT INTERFACE WILL CALL THE CORRECT FUNCTION.
        !!
        !!THE FOLLOWING IS AN EXAMPLE OF VTK\_VAR REAL VECTORIAL DATA CALLING:
        !!
        !!\BEGIN{BOXRED}{VTK\_VAR REAL VECTORIAL DATA CALLING}
        !!\BEGIN{VERBATIM}
        !!...
        !!INTEGER(4), PARAMETER:: NC_NN=100
        !!REAL(4)::               VARX(1:NC_NN)
        !!REAL(4)::               VARZ(1:NC_NN)
        !!REAL(4)::               VARZ(1:NC_NN)
        !!...
        !!E_IO = VTK_VAR('VECT',NC_NN,'REAL VECTORIAL DATA',...
        !!            ...VARX,VARY,VARZ)
        !!...
        !!\END{VERBATIM}
        !!\END{BOXRED}
        !!
        !!\SUBSECTION{VTK\_VAR INTEGER VECTORIAL DATA}
        !!
        !!\BEGIN{BOXRED}{}
        !!\BEGIN{LSTLISTING}[STYLE=SIGNATURE,TITLE=\COLOR{MAROON}\MAIUSCOLETTOBS{VTK\_VAR INTEGER VECTORIAL DATA SIGNATURE}]
        !!FUNCTION VTK_VAR(NC_NN,VARNAME,VARX,VARY,VARZ) RESULT(E_IO)
        !!\END{LSTLISTING}
        !!\END{BOXRED}
        !!
        !!THIS KIND OF CALL IS USED TO SAVE INTEGER VECTORIAL DATA.
        !!
        !!\BEGIN{BOXRED}{}
        !!\BEGIN{LSTLISTING}[STYLE=VARIABLES,TITLE=\COLOR{MAROON}\MAIUSCOLETTOBS{VTK\_VAR INTEGER VECTORIAL DATA VARIABLES}]
        !!INTEGER(R4P),   INTENT(IN):: NC_NN         ! NUMBER OF NODES OR CELLS
        !!CHARACTER(*),   INTENT(IN):: VARNAME       ! VARIABLE NAME
        !!INTEGER(R4P),   INTENT(IN):: VARX(1:NC_NN) ! X COMPONENT OF VECTOR
        !!INTEGER(R4P),   INTENT(IN):: VARY(1:NC_NN) ! Y COMPONENT OF VECTOR
        !!INTEGER(R4P),   INTENT(IN):: VARZ(1:NC_NN) ! Z COMPONENT OF VECTOR
        !!INTEGER(R4P)::               E_IO          ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        !!\END{LSTLISTING}
        !!\END{BOXRED}
        !!
        !!THE VTK\_VAR VARIABLES HAVE THE FOLLOWING MEANING:
        !!
        !!\BEGIN{DESCRIPTION}
        !! \ITEM[{\COLOR{ROYALBLUE}NC\_NN}] INDICATES THE NUMBER OF ALL CELLS OR ALL NODES ACCORDING TO THE VALUE OF
        !!                                  {\COLOR{ROYALBLUE}TIPO} PASSED TO VTK\_DAT.
        !! \ITEM[{\COLOR{ROYALBLUE}VARNAME}] CONTAINS THE NAME ATTRIBUITED THE VARIABLE SAVED.
        !! \ITEM[{\COLOR{ROYALBLUE}VARX}] CONTAINS THE VALUES OF $X$ COMPONENT IN EACH NODES OR CELLS. IT IS A VECTOR OF $[1:NC\_NN]$.
        !! \ITEM[{\COLOR{ROYALBLUE}VARY}] CONTAINS THE VALUES OF $Y$ COMPONENT IN EACH NODES OR CELLS. IT IS A VECTOR OF $[1:NC\_NN]$.
        !! \ITEM[{\COLOR{ROYALBLUE}VARZ}] CONTAINS THE VALUES OF $Z$ COMPONENT IN EACH NODES OR CELLS. IT IS A VECTOR OF $[1:NC\_NN]$.
        !! \ITEM[{\COLOR{ROYALBLUE}E\_IO}] CONTAINS THE INQUIRING INTEGER FLAG FOR ERROR HANDLING.
        !!\END{DESCRIPTION}
        !!
        !!THE FOLLOWING IS AN EXAMPLE OF VTK\_VAR REAL VECTORIAL DATA CALLING:
        !!
        !!\BEGIN{BOXRED}{VTK\_VAR INTEGER VECTORIAL DATA CALLING}
        !!\BEGIN{VERBATIM}
        !!...
        !!INTEGER(4), PARAMETER:: NC_NN=100
        !!INTEGER(4)::            VARX(1:NC_NN)
        !!INTEGER(4)::            VARZ(1:NC_NN)
        !!INTEGER(4)::            VARZ(1:NC_NN)
        !!...
        !!E_IO = VTK_VAR(NC_NN,'INTEGER VECTORIAL DATA', &
        !!               VARX,VARY,VARZ)
        !!...
        !!\END{VERBATIM}
        !!\END{BOXRED}
        !!
        !!\SUBSECTION{VTK\_VAR TEXTURE DATA}
        !!
        !!\BEGIN{BOXRED}{}
        !!\BEGIN{LSTLISTING}[STYLE=SIGNATURE,TITLE=\COLOR{MAROON}\MAIUSCOLETTOBS{VTK\_VAR TEXTURE DATA SIGNATURE}]
        !!FUNCTION VTK_VAR(NC_NN,,DIMM,VARNAME,TEXTCOO) RESULT(E_IO)
        !!\END{LSTLISTING}
        !!\END{BOXRED}
        !!
        !!THIS KIND OF CALL IS USED TO SAVE TEXTURE DATA.
        !!
        !!\BEGIN{BOXRED}{}
        !!\BEGIN{LSTLISTING}[STYLE=VARIABLES,TITLE=\COLOR{MAROON}\MAIUSCOLETTOBS{VTK\_VAR TEXTURE DATA VARIABLES}]
        !!INTEGER(R4P),     INTENT(IN):: NC_NN                   ! NUMBER OF NODES OR CELLS
        !!INTEGER(R4P),     INTENT(IN):: DIMM                    ! TEXTURE DIMENSIONS
        !!CHARACTER(*),     INTENT(IN):: VARNAME                 ! VARIABLE NAME
        !!REAL(R8P OR R4P), INTENT(IN):: TEXTCOO(1:NC_NN,1:DIMM) ! TEXTURE
        !!INTEGER(R4P)::                 E_IO                    ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        !!\END{LSTLISTING}
        !!\END{BOXRED}
        !!
        !!THE VTK\_VAR VARIABLES HAVE THE FOLLOWING MEANING:
        !!
        !!\BEGIN{DESCRIPTION}
        !! \ITEM[{\COLOR{ROYALBLUE}NC\_NN}] INDICATES THE NUMBER OF ALL CELLS OR ALL NODES ACCORDING TO THE VALUE OF
        !!                                  {\COLOR{ROYALBLUE}TIPO} PASSED TO VTK\_DAT.
        !! \ITEM[{\COLOR{ROYALBLUE}DIMM}] INDICATES THE DIMENSIONS OF THE TEXTURE COORDINATES. IT CAN ASSUME THE VALUE:
        !! \BEGIN{ENUMERATEABLU}
        !!  \ITEM \EMPH{1} $\RIGHTARROW$ SCALAR TEXTURE.
        !!  \ITEM \EMPH{2} $\RIGHTARROW$ TWODIMENSIONAL TEXTURE.
        !!  \ITEM \EMPH{3} $\RIGHTARROW$ THREEDIMENSIONAL TEXTURE.
        !! \END{ENUMERATEABLU}
        !! \ITEM[{\COLOR{ROYALBLUE}VARNAME}] CONTAINS THE NAME ATTRIBUITED THE VARIABLE SAVED.
        !! \ITEM[{\COLOR{ROYALBLUE}TEXTCOO}] CONTAINS THE COORDINATES OF TEXTURE IN EACH NODES OR CELLS. IT IS A VECTOR OF
        !!                                   $[1:NC\_NN,1:DIMM]$.
        !! \ITEM[{\COLOR{ROYALBLUE}E\_IO}] CONTAINS THE INQUIRING INTEGER FLAG FOR ERROR HANDLING.
        !!\END{DESCRIPTION}
        !!
        !!NOTE THAT THE VARIABLE \TEXTTT{TEXTCOO} CAN BE PASSED BOTH AS 8-BYTE REAL KIND AND 4-BYTE REAL KIND; THE DYNAMIC
        !!DISPLACEMENT INTERFACE WILL CALL THE CORRECT FUNCTION.
        !!
        !!THE FOLLOWING IS AN EXAMPLE OF VTK\_VAR TEXTURE DATA CALLING:
        !!
        !!\BEGIN{BOXRED}{VTK\_VAR TEXTURE DATA CALLING}
        !!\BEGIN{VERBATIM}
        !!...
        !!INTEGER(4), PARAMETER:: NC_NN=100
        !!INTEGER(4), PARAMETER:: DIMM=2
        !!REAL(4)::               TEXTCOO(1:NC_NN,1:DIMM)
        !!...
        !!E_IO = VTK_VAR(NC_NN,DIMM,'TEXTURE DATA',TEXTCOO)
        !!...
        !!\END{VERBATIM}
        !!\END{BOXRED}
        !!
        !(\DOC)SKIPPEDBLOCK
        FUNCTION VTK_VAR_SCAL_R8(NC_NN,VARNAME,VAR) RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! FUNCTION FOR SAVING FIELD OF SCALAR VARIABLE (R8P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NC_NN        ! NUMBER OF NODES OR CELLS
        CHARACTER(*), INTENT(IN):: VARNAME      ! VARIABLE NAME
        REAL(R8P),    INTENT(IN):: VAR(1:NC_NN) ! VARIABLE TO BE SAVED
        INTEGER(I4P)::             E_IO         ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          WRITE(UNIT=UNIT_VTK,FMT='(A)',IOSTAT=E_IO)'SCALARS '//
     &          TRIM(VARNAME)//' double 1'
          WRITE(UNIT=UNIT_VTK,FMT='(A)',IOSTAT=E_IO)
     &          'LOOKUP_TABLE default'
          WRITE(UNIT=UNIT_VTK,FMT=FR8P, IOSTAT=E_IO)VAR
        CASE(F_OUT_BINARY)
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)'SCALARS '//TRIM(VARNAME)//
     &          ' double 1'//END_REC
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)'LOOKUP_TABLE default'//
     &          END_REC
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)VAR
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_VAR_SCAL_R8

        FUNCTION VTK_VAR_SCAL_R4(NC_NN,VARNAME,VAR) RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! FUNCTION FOR SAVING FIELD OF SCALAR VARIABLE (R4P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NC_NN        ! NUMBER OF NODES OR CELLS
        CHARACTER(*), INTENT(IN):: VARNAME      ! VARIABLE NAME
        REAL(R4P),    INTENT(IN):: VAR(1:NC_NN) ! VARIABLE TO BE SAVED
        INTEGER(I4P)::             E_IO         ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          WRITE(UNIT=UNIT_VTK,FMT='(A)',IOSTAT=E_IO)'SCALARS '//
     &          TRIM(VARNAME)//' float 1'
          WRITE(UNIT=UNIT_VTK,FMT='(A)',IOSTAT=E_IO)
     &          'LOOKUP_TABLE default'
          WRITE(UNIT=UNIT_VTK,FMT=FR4P, IOSTAT=E_IO)VAR
        CASE(F_OUT_BINARY)
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)'SCALARS '//TRIM(VARNAME)//
     &          ' float 1'//END_REC
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)'LOOKUP_TABLE default'//
     &          END_REC
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)VAR
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_VAR_SCAL_R4

        FUNCTION VTK_VAR_SCAL_I4(NC_NN,VARNAME,VAR) RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! FUNCTION FOR SAVING FIELD OF SCALAR VARIABLE (I4P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NC_NN        ! NUMBER OF NODES OR CELLS
        CHARACTER(*), INTENT(IN):: VARNAME      ! VARIABLE NAME
        INTEGER(I4P), INTENT(IN):: VAR(1:NC_NN) ! VARIABLE TO BE SAVED
        INTEGER(I4P)::             E_IO         ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          WRITE(UNIT=UNIT_VTK,FMT='(A)',IOSTAT=E_IO)'SCALARS '//
     &          TRIM(VARNAME)//' int 1'
          WRITE(UNIT=UNIT_VTK,FMT='(A)',IOSTAT=E_IO)
     &          'LOOKUP_TABLE default'
          WRITE(UNIT=UNIT_VTK,FMT=FI4P, IOSTAT=E_IO)VAR
        CASE(F_OUT_BINARY)
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)'SCALARS '//TRIM(VARNAME)//
     &          ' int 1'//END_REC
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)'LOOKUP_TABLE default'//
     &          END_REC
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)VAR
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_VAR_SCAL_I4

        FUNCTION VTK_VAR_VECT_R8(VEC_TYPE,NC_NN,VARNAME,VARX,VARY,VARZ)
     &           RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! FUNCTION FOR SAVING FIELD OF VECTORIAL VARIABLE (R8P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        CHARACTER(*), INTENT(IN):: VEC_TYPE      ! VECTOR TYPE: VECT = GENERIC VECTOR , NORM = NORMAL VECTOR
        INTEGER(I4P), INTENT(IN):: NC_NN         ! NUMBER OF NODES OR CELLS
        CHARACTER(*), INTENT(IN):: VARNAME       ! VARIABLE NAME
        REAL(R8P),    INTENT(IN):: VARX(1:NC_NN) ! X COMPONENT OF VECTOR
        REAL(R8P),    INTENT(IN):: VARY(1:NC_NN) ! Y COMPONENT OF VECTOR
        REAL(R8P),    INTENT(IN):: VARZ(1:NC_NN) ! Z COMPONENT OF VECTOR
        INTEGER(I4P)::             E_IO          ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        INTEGER(I8P)::             N1            ! COUNTER
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          SELECT CASE(UPPER_CASE(TRIM(VEC_TYPE)))
          CASE('VECT')
            WRITE(UNIT=UNIT_VTK,FMT='(A)',          IOSTAT=E_IO)
     &          'VECTORS '//TRIM(VARNAME)//' double'
          CASE('NORM')
            WRITE(UNIT=UNIT_VTK,FMT='(A)',          IOSTAT=E_IO)
     &          'NORMALS '//TRIM(VARNAME)//' double'
          ENDSELECT
          WRITE(UNIT=UNIT_VTK,FMT='(3'//FR8P//')',IOSTAT=E_IO)
     &          (VARX(N1),VARY(N1),VARZ(N1),N1=1,NC_NN)
        CASE(F_OUT_BINARY)
          SELECT CASE(UPPER_CASE(TRIM(VEC_TYPE)))
          CASE('VECT')
            WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)'VECTORS '//TRIM(VARNAME)//
     &          ' double'//END_REC
          CASE('NORM')
            WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)'NORMALS '//TRIM(VARNAME)//
     &          ' double'//END_REC
          ENDSELECT
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)
     &          (VARX(N1),VARY(N1),VARZ(N1),N1=1,NC_NN)
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_VAR_VECT_R8

        FUNCTION VTK_VAR_VECT_R4(VEC_TYPE,NC_NN,VARNAME,VARX,VARY,VARZ)
     &          RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! FUNCTION FOR SAVING FIELD OF VECTORIAL VARIABLE (R4P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        CHARACTER(*), INTENT(IN):: VEC_TYPE      ! VECTOR TYPE: VECT = GENERIC VECTOR , NORM = NORMAL VECTOR
        INTEGER(I4P), INTENT(IN):: NC_NN         ! NUMBER OF NODES OR CELLS
        CHARACTER(*), INTENT(IN):: VARNAME       ! VARIABLE NAME
        REAL(R4P),    INTENT(IN):: VARX(1:NC_NN) ! X COMPONENT OF VECTOR
        REAL(R4P),    INTENT(IN):: VARY(1:NC_NN) ! Y COMPONENT OF VECTOR
        REAL(R4P),    INTENT(IN):: VARZ(1:NC_NN) ! Z COMPONENT OF VECTOR
        INTEGER(I4P)::             E_IO          ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        INTEGER(I8P)::             N1            ! COUNTER
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          SELECT CASE(UPPER_CASE(TRIM(VEC_TYPE)))
          CASE('VECT')
            WRITE(UNIT=UNIT_VTK,FMT='(A)',          IOSTAT=E_IO)
     &          'VECTORS '//TRIM(VARNAME)//' float'
          CASE('NORM')
            WRITE(UNIT=UNIT_VTK,FMT='(A)',          IOSTAT=E_IO)
     &          'NORMALS '//TRIM(VARNAME)//' float'
          ENDSELECT
          WRITE(UNIT=UNIT_VTK,FMT='(3'//FR4P//')',IOSTAT=E_IO)
     &          (VARX(N1),VARY(N1),VARZ(N1),N1=1,NC_NN)
        CASE(F_OUT_BINARY)
          SELECT CASE(UPPER_CASE(TRIM(VEC_TYPE)))
          CASE('VECT')
            WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)'VECTORS '//TRIM(VARNAME)//
     &          ' float'//END_REC
          CASE('NORM')
            WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)'NORMALS '//TRIM(VARNAME)//
     &          ' float'//END_REC
          ENDSELECT
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)
     &          (VARX(N1),VARY(N1),VARZ(N1),N1=1,NC_NN)
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_VAR_VECT_R4

        FUNCTION VTK_VAR_VECT_I4(NC_NN,VARNAME,VARX,VARY,VARZ)
     &  RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! FUNCTION FOR SAVING FIELD OF VECTORIAL VARIABLE (I4P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NC_NN         ! NUMBER OF NODES OR CELLS
        CHARACTER(*), INTENT(IN):: VARNAME       ! VARIABLE NAME
        INTEGER(I4P), INTENT(IN):: VARX(1:NC_NN) ! X COMPONENT OF VECTOR
        INTEGER(I4P), INTENT(IN):: VARY(1:NC_NN) ! Y COMPONENT OF VECTOR
        INTEGER(I4P), INTENT(IN):: VARZ(1:NC_NN) ! Z COMPONENT OF VECTOR
        INTEGER(I4P)::             E_IO          ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        INTEGER(I8P)::             N1            ! COUNTER
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          WRITE(UNIT=UNIT_VTK,FMT='(A)',          IOSTAT=E_IO)
     &          'VECTORS '//TRIM(VARNAME)//' int'
          WRITE(UNIT=UNIT_VTK,FMT='(3'//FI4P//')',IOSTAT=E_IO)
     &          (VARX(N1),VARY(N1),VARZ(N1),N1=1,NC_NN)
        CASE(F_OUT_BINARY)
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)'VECTORS '//TRIM(VARNAME)//
     &          ' int'//END_REC
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)
     &          (VARX(N1),VARY(N1),VARZ(N1),N1=1,NC_NN)
          WRITE(UNIT=UNIT_VTK,IOSTAT=E_IO)END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_VAR_VECT_I4

        FUNCTION VTK_VAR_TEXT_R8(NC_NN,DIMM,VARNAME,TEXTCOO)
     &  RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! Function for saving texture variable (R8P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NC_NN                   ! NUMBER OF NODES OR CELLS
        INTEGER(I4P), INTENT(IN):: DIMM                    ! TEXTURE DIMENSIONS
        CHARACTER(*), INTENT(IN):: VARNAME                 ! VARIABLE NAME
        REAL(R8P),    INTENT(IN):: TEXTCOO(1:NC_NN,1:DIMM) ! TEXTURE
        INTEGER(I4P)::             E_IO                    ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        CHARACTER(LEN=MAXLEN)::    S_BUFFER                ! BUFFER STRING
        CHARACTER(LEN=MAXLEN)::    S_BUFFER2               ! BUFFER STRING
        INTEGER(I8P)::             N1,N2                   ! COUNTERS
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          S_BUFFER2 = '(A,1X,'//FI4P//'1X,A)'
          WRITE(UNIT=UNIT_VTK,FMT=TRIM(S_BUFFER2),        IOSTAT=E_IO)
     &          'TEXTURE_COORDINATES '//TRIM(VARNAME),DIMM,' double'
          WRITE(S_BUFFER,     FMT='(I1)',                 IOSTAT=E_IO)
     &          DIMM
          S_BUFFER='('//TRIM(S_BUFFER)//FR4P//')'
          WRITE(UNIT=UNIT_VTK,FMT=TRIM(S_BUFFER),         IOSTAT=E_IO)
     &          ((TEXTCOO(N1,N2),N2=1,DIMM),N1=1,NC_NN)
        CASE(F_OUT_BINARY)
          S_BUFFER2 = '(A,1X,'//FI4P//'1X,A)'
          WRITE(S_BUFFER,     FMT=TRIM(S_BUFFER),         IOSTAT=E_IO)
     &          'TEXTURE_COORDINATES '//TRIM(VARNAME),DIMM,' double'
          WRITE(UNIT=UNIT_VTK,                            IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(UNIT=UNIT_VTK,                            IOSTAT=E_IO)
     &          ((TEXTCOO(N1,N2),N2=1,DIMM),N1=1,NC_NN)
          WRITE(UNIT=UNIT_VTK,                            IOSTAT=E_IO)
     &          END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_VAR_TEXT_R8

        FUNCTION VTK_VAR_TEXT_R4(NC_NN,DIMM,VARNAME,TEXTCOO)
     &  RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !! FUNCTION FOR SAVING TEXTURE VARIABLE (R4P).
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P), INTENT(IN):: NC_NN                   ! NUMBER OF NODES OR CELLS
        INTEGER(I4P), INTENT(IN):: DIMM                    ! TEXTURE DIMENSIONS
        CHARACTER(*), INTENT(IN):: VARNAME                 ! VARIABLE NAME
        REAL(R4P),    INTENT(IN):: TEXTCOO(1:NC_NN,1:DIMM) ! TEXTURE
        INTEGER(I4P)::             E_IO                    ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        CHARACTER(LEN=MAXLEN)::    S_BUFFER                ! BUFFER STRING
        CHARACTER(LEN=MAXLEN)::    S_BUFFER2               ! BUFFER STRING
        INTEGER(I8P)::             N1,N2                   ! COUNTERS
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        SELECT CASE(F_OUT)
        CASE(F_OUT_ASCII)
          S_BUFFER2 = '(A,1X,'//FI4P//'1X,A)'
          WRITE(UNIT=UNIT_VTK,FMT=TRIM(S_BUFFER2),        IOSTAT=E_IO)
     &          'TEXTURE_COORDINATES '//TRIM(VARNAME),DIMM,' float'
          WRITE(S_BUFFER,     FMT='(I1)',                 IOSTAT=E_IO)
     &          DIMM
          S_BUFFER='('//TRIM(S_BUFFER)//FR4P//')'
          WRITE(UNIT=UNIT_VTK,FMT=TRIM(S_BUFFER),         IOSTAT=E_IO)
     &          ((TEXTCOO(N1,N2),N2=1,DIMM),N1=1,NC_NN)
        CASE(F_OUT_BINARY)
          S_BUFFER2 = '(A,1X,'//FI4P//'1X,A)'
          WRITE(S_BUFFER,     FMT=TRIM(S_BUFFER2),        IOSTAT=E_IO)
     &          'TEXTURE_COORDINATES '//TRIM(VARNAME),DIMM,' float'
          WRITE(UNIT=UNIT_VTK,                            IOSTAT=E_IO)
     &          TRIM(S_BUFFER)//END_REC
          WRITE(UNIT=UNIT_VTK,                            IOSTAT=E_IO)
     &          ((TEXTCOO(N1,N2),N2=1,DIMM),N1=1,NC_NN)
          WRITE(UNIT=UNIT_VTK,                            IOSTAT=E_IO)
     &          END_REC
        ENDSELECT
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_VAR_TEXT_R4
        !(DOC/)SKIPPEDBLOCK

        FUNCTION VTK_END() RESULT(E_IO)
        !--------------------------------------------------------------------------------------------------------------------------------
        !!THIS FUNCTION IS USED TO FINALIZE THE FILE OPENED AND IT HAS NOT INPUTS. THE \LIBVTKIO MANAGES THE FILE UNIT WITHOUT THE
        !!USER'S ACTION.
        !--------------------------------------------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !--------------------------------------------------------------------------------------------------------------------------------
        INTEGER(I4P):: E_IO ! INPUT/OUTPUT INQUIRING FLAG: $0$ IF IO IS DONE, $> 0$ IF IO IS NOT DONE
        !!THE VTK\_END VARIABLES HAVE THE FOLLOWING MEANING:
        !!
        !!\BEGIN{DESCRIPTION}
        !! \ITEM[{\COLOR{ROYALBLUE}E\_IO}] CONTAINS THE INQUIRING INTEGER FLAG FOR ERROR HANDLING.
        !!\END{DESCRIPTION}
        !!
        !!THE FOLLOWING IS AN EXAMPLE OF VTK\_END CALLING:
        !!
        !!\BEGIN{BOXRED}{VTK\_END CALLING}
        !!\BEGIN{VERBATIM}
        !!...
        !!E_IO = VTK_END()
        !!...
        !!\END{VERBATIM}
        !!\END{BOXRED}
        !--------------------------------------------------------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------------------------------------
        CLOSE(UNIT=UNIT_VTK,IOSTAT=E_IO)
        RETURN
        !--------------------------------------------------------------------------------------------------------------------------------
        END FUNCTION VTK_END

        !!\chapter{VTK XML functions}
        !!\minitoc
        !!\vspace*{8mm}
        !!
        !!\lettrine[lines=2,loversize=-.1,lraise=0.2]{{\bf T}}{he} XML standard is more powerful than legacy one. It is more flexible
        !!and free but on the other hand is more (but not so more using a library like \LIBVTKIO...) complex than legacy standard. The
        !!output of XML functions is a well-formated XML file at least for the ascii format (in the binary format \LIBVTKIO use
        !!raw-data format that does not produce a well formated XML file).
        !!
        !!The XML functions follow the same calling-convention of the legacy functions; all the \LIBVTKIO XML functions are
        !!\MaiuscolettoBS{4-byte integer function}: the output of these functions is an integer that is $0$ if the function calling
        !!has been done right while it is $> 0$  if some errors occur. The functions calling is the same as legacy functions:
        !!
        !!\begin{boxred}{Functions Calling}
        !!\begin{verbatim}
        !!...
        !!integer(4):: E_IO
        !!...
        !!E_IO = VTK_INI_XML(....
        !!...
        !!\end{verbatim}
        !!\end{boxred}
        !!
        !!\noindent Note that the XML functions have the same name of legacy functions with the suffix \virgo{\_XML}.
        !!
#endif
      END MODULE LIB_VTK_IO
      !!
      !!\appendix
      !!
      !!\chapter{LIB\_VTK\_IO Usage Example}
      !!\label{cap:example}
      !!\minitoc
      !!
      !!\vspace*{8mm}
      !!
      !!\lettrine[lines=2,loversize=-.1,lraise=0.2]{{\bf T}}{he} usage of \LIBVTKIO is quite simple. In this chapter there are some
      !!example of \LIBVTKIO usage. Some of the following examples are present also in the file \MaiuscolettoBS{Test\_LIB\_VTK\_IO.f90}
      !!distributed within the \LIBVTKIO.
      !!
      !!\section{Legacy Rectilinear Grid}
      !!\label{sec:example LRECTG}
      !!
      !!\begin{boxred}{Legacy Rectilinear Grid}
      !!\begin{verbatim}
      !!...
      !!integer(4), intent(IN)::   NX
      !!real(8),    intent(IN)::           p(1:NX)
      !!real(8),    intent(IN)::         rho(1:NX)
      !!real(8),    intent(IN)::           u(1:NX)
      !!real(8),    intent(IN)::       gamma(1:NX)
      !!character(*), intent(IN):: filename
      !!real(8)::                  x(1:NX)
      !!integer(4)::               i
      !!...
      !!x=(/(i, i=1, NX, 1)/)
      !!E_IO = VTK_INI(output_format = 'ascii',                &
      !!               filene        = trim(filename)//'.vtk', &
      !!               title         = 'Field',                &
      !!               mesh_topology = 'RECTILINEAR_GRID')
      !!E_IO = VTK_GEO(NX        = NX,        &
      !!               NY        = 1,         &
      !!               NZ        = 1,         &
      !!               X         = x,         &
      !!               Y         = (/0.0_8/), &
      !!               Z         = (/0.0_8/))
      !!E_IO = VTK_DAT(NC_NN   = NX,      &
      !!               tipo    = 'node')
      !!E_IO = VTK_VAR(NC_NN   = NX,      &
      !!               varname = 'p',     &
      !!               var     = p)
      !!E_IO = VTK_VAR(NC_NN   = NX,      &
      !!               varname = 'rho',   &
      !!               var     = rho)
      !!E_IO = VTK_VAR(NC_NN   = NX,      &
      !!               varname = 'u',     &
      !!               var     = u)
      !!E_IO = VTK_VAR(NC_NN   = NX,      &
      !!               varname = 'gamma', &
      !!               var     = gamma)
      !!E_IO = VTK_VAR(NC_NN   = NX,      &
      !!               varname = 'a',     &
      !!               var     = sqrt(gamma*p/rho))
      !!E_IO = VTK_END()
      !!...
      !!\end{verbatim}
      !!\end{boxred}
      !!
      !!\section{XML Rectilinear Grid}
      !!\label{sec:example XRECTG}
      !!
      !!\begin{boxred}{XML Rectilinear Grid}
      !!\begin{verbatim}
      !!...
      !!integer(4),   intent(IN):: n
      !!integer(4),   intent(IN):: NX
      !!real(8),      intent(IN)::     p(1:NX)
      !!real(8),      intent(IN)::   rho(1:NX)
      !!real(8),      intent(IN)::     u(1:NX)
      !!real(8),      intent(IN):: gamma(1:NX)
      !!character(*), intent(IN):: filename
      !!real(8)::                  x(1:NX)
      !!integer(4)::               i
      !!...
      !!x=(/(i, i=1, NX, 1)/)
      !!E_IO = VTK_INI_XML(output_format = 'ascii',                &
      !!                   filename      = trim(filename)//'.vtr', &
      !!                   mesh_topology = 'RectilinearGrid',      &
      !!                   nx1=1,nx2=NX,ny1=1,ny2=1,nz1=1,nz2=1)
      !!E_IO = VTK_GEO_XML(nx1=1,nx2=NX,ny1=1,ny2=1,nz1=1,nz2=1, &
      !!                   X=x,Y=(/0.0_8/),Z=(/0.0_8/))
      !!E_IO = VTK_DAT_XML(tipo    = 'node',   &
      !!                   azione  = 'OPEN')
      !!E_IO = VTK_VAR_XML(NC_NN   = NX,      &
      !!                   varname = 'p',     &
      !!                   var     = p)
      !!E_IO = VTK_VAR_XML(NC_NN   = NX,      &
      !!                   varname = 'rho',   &
      !!                   var     = rho)
      !!E_IO = VTK_VAR_XML(NC_NN   = NX,      &
      !!                   varname = 'u',     &
      !!                   var     = u)
      !!E_IO = VTK_VAR_XML(NC_NN   = NX,      &
      !!                   varname = 'gamma', &
      !!                   var     = gamma)
      !!E_IO = VTK_VAR_XML(NC_NN   = NX,      &
      !!                   varname = 'a',     &
      !!                   var     = sqrt(gamma*p/rho))
      !!E_IO = VTK_DAT_XML(tipo    = 'node',  &
      !!                   azione  = 'CLOSE')
      !!E_IO = VTK_GEO_XML()
      !!E_IO = VTK_END_XML()
      !!...
      !!\end{verbatim}
      !!\end{boxred}
      !!
      !!\section{Legacy Unstructured Grid}
      !!\label{sec:example LUNSTG}
      !!
      !!\begin{boxred}{Legacy Unstructured Grid}
      !!\begin{verbatim}
      !!...
      !!integer(4), parameter::       Nn   = 27
      !!integer(4), parameter::       Ne   = 11
      !!real(4),    dimension(1:Nn):: x_uns
      !!real(4),    dimension(1:Nn):: y_uns
      !!real(4),    dimension(1:Nn):: z_uns
      !!integer(4), dimension(1:Ne):: tipo
      !!integer(4), dimension(1:60):: connect
      !!real(8),    dimension(1:Nn):: var_uns_grid
      !!integer(4), dimension(1:Nn):: var_uns_grid_X
      !!integer(4), dimension(1:Nn):: var_uns_grid_Y
      !!integer(4), dimension(1:Nn):: var_uns_grid_Z
      !!...
      !!E_IO = VTK_INI(output_format  = 'BINARY',                   &
      !!               filename       = 'UNST_GRID_BIN.vtk',        &
      !!               title          = 'Unstructured Grid Example' &
      !!               mesh_topology  = 'UNSTRUCTURED_GRID')
      !!
      !!x_uns=(/0,1,2,0,1,2, &
      !!        0,1,2,0,1,2, &
      !!        0,1,2,0,1,2, &
      !!        0,1,2,0,1,2, &
      !!        0,1,2/)
      !!y_uns=(/0,0,0,1,1,1, &
      !!        0,0,0,1,1,1, &
      !!        1,1,1,1,1,1, &
      !!        1,1,1,1,1,1, &
      !!        1,1,1/)
      !!z_uns=(/0,0,0,0,0,0, &
      !!        1,1,1,1,1,1, &
      !!        2,2,2,3,3,3, &
      !!        4,4,4,5,5,5, &
      !!        6,6,6/)
      !!
      !!E_IO = VTK_GEO(Nnodi = Nn, &
      !!               X=x_uns,Y=y_uns,Z=z_uns)
      !!
      !!connect = (/ 8, 0, 1, 4, 3, 6, 7,10, 9, &
      !!             8, 1, 2, 5, 4, 7, 8,11,10, &
      !!             4, 6,10, 9,12,             &
      !!             4, 5,11,10,14,             &
      !!             6,15,16,17,14,13,12,       &
      !!             6,18,15,19,16,20,17,       &
      !!             4,22,23,20,19,             &
      !!             3,21,22,18,                &
      !!             3,22,19,18,                &
      !!             2,26,25,                   &
      !!             1,24/)
      !!tipo = (/12, &
      !!         12, &
      !!         10, &
      !!         10, &
      !!          7, &
      !!          6, &
      !!          9, &
      !!          5, &
      !!          5, &
      !!          3, &
      !!          1/)
      !!E_IO = VTK_CON(NCelle  = Ne,       &
      !!               connect = connect,  &
      !!               tipo    = tipo)
      !!E_IO = VTK_DAT(NC_NN   = Nn,       &
      !!               tipo    = 'node')
      !!
      !!var_uns_grid =(/ 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, &
      !!                 6.0, 7.0, 8.0, 9.0,10.0,11.0, &
      !!                12.0,13.0,14.0,15.0,16.0,17.0, &
      !!                18.0,19.0,20.0,21.0,22.0,23.0, &
      !!                24.0,25.0,26.0/)
      !!
      !!E_IO = VTK_VAR(NC_NN   = Nn,        &
      !!               varname = 'scalars', &
      !!               var     = var_uns_grid)
      !!
      !!var_uns_grid_X=(/1,1,0,1,1,0, &
      !!                 1,1,0,1,1,0, &
      !!                 0,0,0,0,0,0, &
      !!                 0,0,0,0,0,0, &
      !!                 0,0,0/)
      !!var_uns_grid_Y=(/0,1,2,0,1,2, &
      !!                 0,1,2,0,1,2, &
      !!                 0,0,0,0,0,0, &
      !!                 0,0,0,0,0,0, &
      !!                 0,0,0/)
      !!var_uns_grid_Z=(/0,0,0,0,0,0, &
      !!                 0,0,0,0,0,0, &
      !!                 1,1,1,1,1,1, &
      !!                 1,1,1,1,1,1, &
      !!                 1,1,1/)
      !!E_IO = VTK_VAR(NC_NN   = Nn,             &
      !!               varname = 'vectors',      &
      !!               varX    = var_uns_grid_X, &
      !!               varY    = var_uns_grid_Y, &
      !!               varZ    = var_uns_grid_Z)
      !!E_IO = VTK_END()
      !!...
      !!\end{verbatim}
      !!\end{boxred}
      !!
      !!\section{XML Unstructured Grid}
      !!\label{sec:example XUNSTG}
      !!
      !!\begin{boxred}{XML Unstructured Grid}
      !!\begin{verbatim}
      !!...
      !!integer(4), parameter::       Nn   = 27
      !!integer(4), parameter::       Ne   = 11
      !!real(4),    dimension(1:Nn):: x_uns
      !!real(4),    dimension(1:Nn):: y_uns
      !!real(4),    dimension(1:Nn):: z_uns
      !!integer(4), dimension(1:Ne):: tipo
      !!integer(4), dimension(1:49):: connect_xml
      !!integer(4), dimension(1:Ne):: offset_xml
      !!real(8),    dimension(1:Nn):: var_uns_grid
      !!integer(4), dimension(1:Nn):: var_uns_grid_X
      !!integer(4), dimension(1:Nn):: var_uns_grid_Y
      !!integer(4), dimension(1:Nn):: var_uns_grid_Z
      !!...
      !!E_IO = VTK_INI_XML(output_format = 'BINARY',              &
      !!                   filename      = 'XML_UNST_BINARY.vtu', &
      !!                   mesh_topology = 'UnstructuredGrid')
      !!
      !!x_uns=(/0,1,2,0,1,2, &
      !!        0,1,2,0,1,2, &
      !!        0,1,2,0,1,2, &
      !!        0,1,2,0,1,2, &
      !!        0,1,2/)
      !!y_uns=(/0,0,0,1,1,1, &
      !!        0,0,0,1,1,1, &
      !!        1,1,1,1,1,1, &
      !!        1,1,1,1,1,1, &
      !!        1,1,1/)
      !!z_uns=(/0,0,0,0,0,0, &
      !!        1,1,1,1,1,1, &
      !!        2,2,2,3,3,3, &
      !!        4,4,4,5,5,5, &
      !!        6,6,6/)
      !!
      !!E_IO = VTK_GEO_XML(Nnodi     = Nn, &
      !!                   NCelle    = Ne, &
      !!                   X=x_uns,Y=y_uns,Z=z_uns)
      !!
      !!connect_xml = (/ 0, 1, 4, 3, 6, 7,10, 9, &
      !!                 1, 2, 5, 4, 7, 8,11,10, &
      !!                 6,10, 9,12,             &
      !!                 5,11,10,14,             &
      !!                15,16,17,14,13,12,       &
      !!                18,15,19,16,20,17,       &
      !!                22,23,20,19,             &
      !!                21,22,18,                &
      !!                22,19,18,                &
      !!                26,25,                   &
      !!                24/)
      !!offset_xml = (/ 8, &
      !!               16, &
      !!               20, &
      !!               24, &
      !!               30, &
      !!               36, &
      !!               40, &
      !!               43, &
      !!               46, &
      !!               48, &
      !!               49/)
      !!
      !!E_IO = VTK_CON_XML(NCelle  = Ne,          &
      !!                   connect = connect_xml, &
      !!                   offset  = offset_xml,  &
      !!                   tipo    = (/12_1, &
      !!                               12_1, &
      !!                               10_1, &
      !!                               10_1, &
      !!                                7_1, &
      !!                                6_1, &
      !!                                9_1, &
      !!                                5_1, &
      !!                                5_1, &
      !!                                3_1, &
      !!                                1_1/))
      !!
      !!E_IO = VTK_DAT_XML(tipo    = 'node', &
      !!                   azione  = 'OPEN')
      !!
      !!var_uns_grid =(/ 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, &
      !!                 6.0, 7.0, 8.0, 9.0,10.0,11.0, &
      !!                12.0,13.0,14.0,15.0,16.0,17.0, &
      !!                18.0,19.0,20.0,21.0,22.0,23.0, &
      !!                24.0,25.0,26.0/)
      !!
      !!E_IO = VTK_VAR_XML(NC_NN   = Nn,        &
      !!                   varname = 'scalars', &
      !!                   var     = var_uns_grid)
      !!
      !!var_uns_grid_X=(/1,1,0,1,1,0, &
      !!                 1,1,0,1,1,0, &
      !!                 0,0,0,0,0,0, &
      !!                 0,0,0,0,0,0, &
      !!                 0,0,0/)
      !!var_uns_grid_Y=(/0,1,2,0,1,2, &
      !!                 0,1,2,0,1,2, &
      !!                 0,0,0,0,0,0, &
      !!                 0,0,0,0,0,0, &
      !!                 0,0,0/)
      !!var_uns_grid_Z=(/0,0,0,0,0,0, &
      !!                 0,0,0,0,0,0, &
      !!                 1,1,1,1,1,1, &
      !!                 1,1,1,1,1,1, &
      !!                 1,1,1/)
      !!
      !!E_IO = VTK_VAR_XML(NC_NN   = Nn,             &
      !!                   varname = 'vector',       &
      !!                   varX    = var_uns_grid_X, &
      !!                   varY    = var_uns_grid_Y, &
      !!                   varZ    = var_uns_grid_Z)
      !!E_IO = VTK_DAT_XML(tipo    = 'node',   &
      !!                   azione  = 'CLOSE')
      !!E_IO = VTK_GEO_XML()
      !!E_IO = VTK_END_XML()
      !!...
      !!\end{verbatim}
      !!\end{boxred}
      !!
      !!\chapter{Fortran \& Portable-Kind-Precision Selection}
      !!\label{cap:kind precision}
      !!
      !!\lettrine[lines=2,loversize=-.1,lraise=0.2]{{\bf F}}{ortran} is the most popular programming language for scientific computing.
      !!With fortran it is quite simple obtain fast code and manage large multidimensional array. Because fortran permits the achivment
      !!of high performance it is also used on great range of different computer-architettures, and often on the fastest supercomputer
      !!in the world. Therefore fortran programs must be \MaiuscolettoBS{portable}: portability means that the code will give the same
      !!results on every different computer-architettures. One of the most important goal of the numeric code is to control the
      !!\MaiuscolettoBS{the numeric error} due to finite precision of numerical operations. Fortran uses the \MaiuscolettoBS{IEEE
      !!rappresentations}; integers and reals (floating point) are represented with a finite precision. So when the code computes an
      !!operation it has a \MaiuscolettoBS{trunction error} due to the truncation of the numerical finite rappresentaions. For numerical
      !!and more in general scientific applications this source of errors must be controlled. The programmer must know which is the
      !!precision associated to the code variables. Before the standard fortran 90/95 there are not any way to select the precision of
      !!the numerical variables in a portable fashion. With the possibility to specify a kind parameter for variables, the standard
      !!fortran 90/95 makes avaible two useful functions to select the kind precision of integers and reals:
      !!
      !!\begin{boxred}{selected\_real\_kind \& selected\_int\_kind}
      !!\begin{verbatim}
      !!function selected_real_kind(p,r) result(kind_id)
      !!integer, intent(IN), optional:: p
      !!integer, intent(IN), optional:: r
      !!integer::                       kind_id
      !!
      !!The result, kind_id, is a scalar of type default integer.
      !!If both arguments are absent, the result is zero.
      !!Otherwise, the result has a value equal to a value of
      !!the kind parameter of a real data type with decimal
      !!precision, as returned by the function PRECISION, of at
      !!least p digits and a decimal exponent range, as returned
      !!by the function RANGE, of at least r.
      !!
      !!function selected_int_kind(p) result(kind_id)
      !!integer, intent(IN), optional:: p
      !!integer::                       kind_id
      !!
      !!The result, kind_id, is a scalar of type default integer.
      !!The result has a value equal to the value of the kind
      !!parameter of the integer data type that represents all
      !!values n in the range of about values n with
      !!-10^p < n < 10^p.
      !!\end{verbatim}
      !!\end{boxred}
      !!
      !!Using these two functions the programmer can accurately control the precision of its own variables in a portable manner.
      !!Note that specifing the kind precision without using these two functions is not portable: $real(8)$ means different
      !!precisions on different architettures. Parametrizing the kind of all numerical variables using these two functions makes
      !!the portable. The \LIBVTKIO uses this principle to achive portable-kind-precision selection; in the library are defined
      !!some parameters by which all variables kind-precisions are parametrized:
      !!
      !!\begin{boxblu}{\LIBVTKIO Kind-Precision Parameters}
      !!{\color{RoyalBlue}\MaiuscolettoS{Real Precision Definitions}}
      !!\begin{description}
      !! \item [{\color{RoyalBlue}R16P}] real with $33$ digits, range $[+-10^{-4931},+-10^{+4931}-1]$
      !! \item [{\color{RoyalBlue}R8P}]  real with $15$ digits, range $[+-10^{-307} ,+-10^{+307}-1 ]$
      !! \item [{\color{RoyalBlue}R4P}]  real with $6$  digits, range $[+-10^{-37}  ,+-10^+{37}-1  ]$
      !!\end{description}
      !!{\color{RoyalBlue}\MaiuscolettoS{Integer Precision Definitions}}
      !!\begin{description}
      !! \item [{\color{RoyalBlue}I8P}] range $[-2^{63},+2^{63}-1]$
      !! \item [{\color{RoyalBlue}I4P}] range $[-2^{31},+2^{31}-1]$
      !! \item [{\color{RoyalBlue}I2P}] range $[-2^{15},+2^{15}-1]$
      !! \item [{\color{RoyalBlue}I1P}] range $[-2^{7} ,+2^{7} -1]$
      !!\end{description}
      !!\end{boxblu}
      !!
      !!In order to avoid strange results porting your code the use of parametrized-kind-precision is very useful. The \LIBVTKIO
      !!makes avaible to the external its own kind-parameters that can be used to parametrize the code.
      !!
      !!\chapter{Dynamic Dispatching}
      !!\label{cap:Dynamic Dispatching}
      !!
      !!\lettrine[lines=2,loversize=-.1,lraise=0.2]{{\bf F}}{ortran} is not an \MaiuscolettoBS{object oriented} (OOp) programming
      !!language. It is a procedural language with some of the the goals (ineritance, user-definited data type, polimorphism...)
      !!of OOp. Fortran most important aim is to ensure the performance of the code not its \virgo{friendliness}... Despite its
      !!nature, fortran 90/95 makes avaible some interesting features: it permits the dynamic dispatching of functions and
      !!subroutine ensuring the best performance. This goal is achived with use of $interface$ construct. In the \LIBVTKIO there are,
      !!at today, 4 interface blocks:
      !!
      !!\begin{boxred}{\LIBVTKIO Interface Blocks}
      !!\begin{verbatim}
      !!interface VTK_GEO
      !!  module procedure VTK_GEO_UNST_R8, &
      !!                   VTK_GEO_UNST_R4, &
      !!                   VTK_GEO_STRP_R8, &
      !!                   VTK_GEO_STRP_R4, &
      !!                   VTK_GEO_STRG_R8, &
      !!                   VTK_GEO_STRG_R4, &
      !!                   VTK_GEO_RECT_R8, &
      !!                   VTK_GEO_RECT_R4
      !!endinterface
      !!
      !!interface VTK_VAR
      !!  module procedure VTK_VAR_SCAL_R8, &
      !!                   VTK_VAR_SCAL_R4, &
      !!                   VTK_VAR_SCAL_I4, &
      !!                   VTK_VAR_VECT_R8, &
      !!                   VTK_VAR_VECT_R4, &
      !!                   VTK_VAR_VECT_I4, &
      !!                   VTK_VAR_TEXT_R8, &
      !!                   VTK_VAR_TEXT_R4
      !!endinterface
      !!
      !!interface VTK_GEO_XML
      !!  module procedure VTK_GEO_XML_STRG_R4, &
      !!                   VTK_GEO_XML_STRG_R8, &
      !!                   VTK_GEO_XML_RECT_R8, &
      !!                   VTK_GEO_XML_RECT_R4, &
      !!                   VTK_GEO_XML_UNST_R8, &
      !!                   VTK_GEO_XML_UNST_R4, &
      !!                   VTK_GEO_XML_CLOSEP
      !!endinterface
      !!
      !!interface VTK_VAR_XML
      !!  module procedure VTK_VAR_XML_SCAL_R8, &
      !!                   VTK_VAR_XML_SCAL_R4, &
      !!                   VTK_VAR_XML_SCAL_I8, &
      !!                   VTK_VAR_XML_SCAL_I4, &
      !!                   VTK_VAR_XML_SCAL_I2, &
      !!                   VTK_VAR_XML_SCAL_I1, &
      !!                   VTK_VAR_XML_VECT_R8, &
      !!                   VTK_VAR_XML_VECT_R4, &
      !!                   VTK_VAR_XML_VECT_I8, &
      !!                   VTK_VAR_XML_VECT_I4, &
      !!                   VTK_VAR_XML_VECT_I2, &
      !!                   VTK_VAR_XML_VECT_I1
      !!endinterface
      !!\end{verbatim}
      !!\end{boxred}
      !!
      !!By the interface construct \LIBVTKIO has a more simple API. The user deals with a few functions without non-sense-long-name...
      !!Dynamic dispatching is not the magic wand to solve all problems but it is an useful tool to simplify the code API. It is
      !!not powerful as the C++ template, but it is a \MaiuscolettoBS{quantum-leap} for fortran programmers.
      !!
      !!\chapter{Known Bugs}
      !!\label{cap:BUG}
      !!
      !!\lettrine[lines=2,loversize=-.1,lraise=0.2]{{\bf T}}{he} \LIBVTKIO is a very young project and it is a good example of wrong
      !!programming style... It is unstable and not tested. It is used by only one user (... me of course!) and there are a lot of
      !!bugs that are still hidden. At the moment several features are missing (the input functions and the poly-data topology...),
      !!but it is useful to export fortran data to VTK standard, and this goal was the most important for me.
      !!
      !!At today only one main bug was found. Fortran allows the automatic reshape of arrays: as an example 2D array can be
      !!automatically (in the function calling) transformed  to a 1D array with the same number of element of 2D array. The use of
      !!dynamic dispatching had disable this feature: dynamic dispatching use the array-shape information to dectet, at compile-time,
      !!the correct function to be called. So reshape arrays at calling phase is not allowed. In the next release I will fix this bug
      !!introducing the function to reshape arrays between 1D, 2D and 3D arrays.
      !!
      !!A possible, not already found, bug is the non correct kind detection. It is possible that a code uses kind-precision parameter
      !!that does not match the \LIBVTKIO parameters. I never observe this bug but it is possible. To avoid it the simple way is to use
      !!always the \LIBVTKIO kind-precision parameters; if the parameters actually present do not match your necessities, define new
      !!parameters in \LIBVTKIO and redistribuite \LIBVTKIO with your pacth!
      !!
      !!Finally there is a strong inefficiency when saving XML binary file. To write XML binary \LIBVTKIO uses a temporary scratch file
      !!to save binary data while saving all formatting data to the final XML file; only when all XML formatting data have been written
      !!the scratch file is rewinded and the binary data is saved in the final tag of XML file as \MaiuscolettoBS{raw} data. This
      !!algorithm is obviously inefficient. Any tip is welcome!
      !!
      !!\chapter{GNU GENERAL PUBLIC LICENSE}
      !!\label{cap:GPL}
      !!
      !!\begin{center}
      !!\MaiuscolettoS{Version 3, 29 June 2007}
      !!
      !!{\parindent 0in
      !!
      !!Copyright \copyright\ 2007 Free Software Foundation, Inc. \texttt{http://fsf.org/}
      !!
      !!\bigskip
      !!Everyone is permitted to copy and distribute verbatim copies of this
      !!
      !!license document, but changing it is not allowed.}
      !!
      !!\end{center}
      !!
      !!
      !!\section*{Preamble}
      !!The GNU General Public License is a free, copyleft license for
      !!software and other kinds of works.
      !!
      !!The licenses for most software and other practical works are designed
      !!to take away your freedom to share and change the works.  By contrast,
      !!the GNU General Public License is intended to guarantee your freedom to
      !!share and change all versions of a program--to make sure it remains free
      !!software for all its users.  We, the Free Software Foundation, use the
      !!GNU General Public License for most of our software; it applies also to
      !!any other work released this way by its authors.  You can apply it to
      !!your programs, too.
      !!
      !!When we speak of free software, we are referring to freedom, not
      !!price.  Our General Public Licenses are designed to make sure that you
      !!have the freedom to distribute copies of free software (and charge for
      !!them if you wish), that you receive source code or can get it if you
      !!want it, that you can change the software or use pieces of it in new
      !!free programs, and that you know you can do these things.
      !!
      !!To protect your rights, we need to prevent others from denying you
      !!these rights or asking you to surrender the rights.  Therefore, you have
      !!certain responsibilities if you distribute copies of the software, or if
      !!you modify it: responsibilities to respect the freedom of others.
      !!
      !!For example, if you distribute copies of such a program, whether
      !!gratis or for a fee, you must pass on to the recipients the same
      !!freedoms that you received.  You must make sure that they, too, receive
      !!or can get the source code.  And you must show them these terms so they
      !!know their rights.
      !!
      !!Developers that use the GNU GPL protect your rights with two steps:
      !!(1) assert copyright on the software, and (2) offer you this License
      !!giving you legal permission to copy, distribute and/or modify it.
      !!
      !!For the developers' and authors' protection, the GPL clearly explains
      !!that there is no warranty for this free software.  For both users' and
      !!authors' sake, the GPL requires that modified versions be marked as
      !!changed, so that their problems will not be attributed erroneously to
      !!authors of previous versions.
      !!
      !!Some devices are designed to deny users access to install or run
      !!modified versions of the software inside them, although the manufacturer
      !!can do so.  This is fundamentally incompatible with the aim of
      !!protecting users' freedom to change the software.  The systematic
      !!pattern of such abuse occurs in the area of products for individuals to
      !!use, which is precisely where it is most unacceptable.  Therefore, we
      !!have designed this version of the GPL to prohibit the practice for those
      !!products.  If such problems arise substantially in other domains, we
      !!stand ready to extend this provision to those domains in future versions
      !!of the GPL, as needed to protect the freedom of users.
      !!
      !!Finally, every program is threatened constantly by software patents.
      !!States should not allow patents to restrict development and use of
      !!software on general-purpose computers, but in those that do, we wish to
      !!avoid the special danger that patents applied to a free program could
      !!make it effectively proprietary.  To prevent this, the GPL assures that
      !!patents cannot be used to render the program non-free.
      !!
      !!The precise terms and conditions for copying, distribution and
      !!modification follow.
      !!
      !!
      !!\begin{center}
      !!{\Large \sc Terms and Conditions}
      !!\end{center}
      !!
      !!\begin{enumerate}
      !!
      !!\addtocounter{enumi}{-1}
      !!
      !!\item Definitions.
      !!
      !!``This License'' refers to version 3 of the GNU General Public License.
      !!
      !!``Copyright'' also means copyright-like laws that apply to other kinds of
      !!works, such as semiconductor masks.
      !!
      !!``The Program'' refers to any copyrightable work licensed under this
      !!License.  Each licensee is addressed as ``you''.  ``Licensees'' and
      !!``recipients'' may be individuals or organizations.
      !!
      !!To ``modify'' a work means to copy from or adapt all or part of the work
      !!in a fashion requiring copyright permission, other than the making of an
      !!exact copy.  The resulting work is called a ``modified version'' of the
      !!earlier work or a work ``based on'' the earlier work.
      !!
      !!A ``covered work'' means either the unmodified Program or a work based
      !!on the Program.
      !!
      !!To ``propagate'' a work means to do anything with it that, without
      !!permission, would make you directly or secondarily liable for
      !!infringement under applicable copyright law, except executing it on a
      !!computer or modifying a private copy.  Propagation includes copying,
      !!distribution (with or without modification), making available to the
      !!public, and in some countries other activities as well.
      !!
      !!To ``convey'' a work means any kind of propagation that enables other
      !!parties to make or receive copies.  Mere interaction with a user through
      !!a computer network, with no transfer of a copy, is not conveying.
      !!
      !!An interactive user interface displays ``Appropriate Legal Notices''
      !!to the extent that it includes a convenient and prominently visible
      !!feature that (1) displays an appropriate copyright notice, and (2)
      !!tells the user that there is no warranty for the work (except to the
      !!extent that warranties are provided), that licensees may convey the
      !!work under this License, and how to view a copy of this License.  If
      !!the interface presents a list of user commands or options, such as a
      !!menu, a prominent item in the list meets this criterion.
      !!
      !!\item Source Code.
      !!
      !!The ``source code'' for a work means the preferred form of the work
      !!for making modifications to it.  ``Object code'' means any non-source
      !!form of a work.
      !!
      !!A ``Standard Interface'' means an interface that either is an official
      !!standard defined by a recognized standards body, or, in the case of
      !!interfaces specified for a particular programming language, one that
      !!is widely used among developers working in that language.
      !!
      !!The ``System Libraries'' of an executable work include anything, other
      !!than the work as a whole, that (a) is included in the normal form of
      !!packaging a Major Component, but which is not part of that Major
      !!Component, and (b) serves only to enable use of the work with that
      !!Major Component, or to implement a Standard Interface for which an
      !!implementation is available to the public in source code form.  A
      !!``Major Component'', in this context, means a major essential component
      !!(kernel, window system, and so on) of the specific operating system
      !!(if any) on which the executable work runs, or a compiler used to
      !!produce the work, or an object code interpreter used to run it.
      !!
      !!The ``Corresponding Source'' for a work in object code form means all
      !!the source code needed to generate, install, and (for an executable
      !!work) run the object code and to modify the work, including scripts to
      !!control those activities.  However, it does not include the work's
      !!System Libraries, or general-purpose tools or generally available free
      !!programs which are used unmodified in performing those activities but
      !!which are not part of the work.  For example, Corresponding Source
      !!includes interface definition files associated with source files for
      !!the work, and the source code for shared libraries and dynamically
      !!linked subprograms that the work is specifically designed to require,
      !!such as by intimate data communication or control flow between those
      !!subprograms and other parts of the work.
      !!
      !!The Corresponding Source need not include anything that users
      !!can regenerate automatically from other parts of the Corresponding
      !!Source.
      !!
      !!The Corresponding Source for a work in source code form is that
      !!same work.
      !!
      !!\item Basic Permissions.
      !!
      !!All rights granted under this License are granted for the term of
      !!copyright on the Program, and are irrevocable provided the stated
      !!conditions are met.  This License explicitly affirms your unlimited
      !!permission to run the unmodified Program.  The output from running a
      !!covered work is covered by this License only if the output, given its
      !!content, constitutes a covered work.  This License acknowledges your
      !!rights of fair use or other equivalent, as provided by copyright law.
      !!
      !!You may make, run and propagate covered works that you do not
      !!convey, without conditions so long as your license otherwise remains
      !!in force.  You may convey covered works to others for the sole purpose
      !!of having them make modifications exclusively for you, or provide you
      !!with facilities for running those works, provided that you comply with
      !!the terms of this License in conveying all material for which you do
      !!not control copyright.  Those thus making or running the covered works
      !!for you must do so exclusively on your behalf, under your direction
      !!and control, on terms that prohibit them from making any copies of
      !!your copyrighted material outside their relationship with you.
      !!
      !!Conveying under any other circumstances is permitted solely under
      !!the conditions stated below.  Sublicensing is not allowed; section 10
      !!makes it unnecessary.
      !!
      !!\item Protecting Users' Legal Rights From Anti-Circumvention Law.
      !!
      !!No covered work shall be deemed part of an effective technological
      !!measure under any applicable law fulfilling obligations under article
      !!11 of the WIPO copyright treaty adopted on 20 December 1996, or
      !!similar laws prohibiting or restricting circumvention of such
      !!measures.
      !!
      !!When you convey a covered work, you waive any legal power to forbid
      !!circumvention of technological measures to the extent such circumvention
      !!is effected by exercising rights under this License with respect to
      !!the covered work, and you disclaim any intention to limit operation or
      !!modification of the work as a means of enforcing, against the work's
      !!users, your or third parties' legal rights to forbid circumvention of
      !!technological measures.
      !!
      !!\item Conveying Verbatim Copies.
      !!
      !!You may convey verbatim copies of the Program's source code as you
      !!receive it, in any medium, provided that you conspicuously and
      !!appropriately publish on each copy an appropriate copyright notice;
      !!keep intact all notices stating that this License and any
      !!non-permissive terms added in accord with section 7 apply to the code;
      !!keep intact all notices of the absence of any warranty; and give all
      !!recipients a copy of this License along with the Program.
      !!
      !!You may charge any price or no price for each copy that you convey,
      !!and you may offer support or warranty protection for a fee.
      !!
      !!\item Conveying Modified Source Versions.
      !!
      !!You may convey a work based on the Program, or the modifications to
      !!produce it from the Program, in the form of source code under the
      !!terms of section 4, provided that you also meet all of these conditions:
      !!  \begin{enumerate}
      !!  \item The work must carry prominent notices stating that you modified
      !!  it, and giving a relevant date.
      !!
      !!  \item The work must carry prominent notices stating that it is
      !!  released under this License and any conditions added under section
      !!  7.  This requirement modifies the requirement in section 4 to
      !!  ``keep intact all notices''.
      !!
      !!  \item You must license the entire work, as a whole, under this
      !!  License to anyone who comes into possession of a copy.  This
      !!  License will therefore apply, along with any applicable section 7
      !!  additional terms, to the whole of the work, and all its parts,
      !!  regardless of how they are packaged.  This License gives no
      !!  permission to license the work in any other way, but it does not
      !!  invalidate such permission if you have separately received it.
      !!
      !!  \item If the work has interactive user interfaces, each must display
      !!  Appropriate Legal Notices; however, if the Program has interactive
      !!  interfaces that do not display Appropriate Legal Notices, your
      !!  work need not make them do so.
      !!\end{enumerate}
      !!A compilation of a covered work with other separate and independent
      !!works, which are not by their nature extensions of the covered work,
      !!and which are not combined with it such as to form a larger program,
      !!in or on a volume of a storage or distribution medium, is called an
      !!``aggregate'' if the compilation and its resulting copyright are not
      !!used to limit the access or legal rights of the compilation's users
      !!beyond what the individual works permit.  Inclusion of a covered work
      !!in an aggregate does not cause this License to apply to the other
      !!parts of the aggregate.
      !!
      !!\item Conveying Non-Source Forms.
      !!
      !!You may convey a covered work in object code form under the terms
      !!of sections 4 and 5, provided that you also convey the
      !!machine-readable Corresponding Source under the terms of this License,
      !!in one of these ways:
      !!  \begin{enumerate}
      !!  \item Convey the object code in, or embodied in, a physical product
      !!  (including a physical distribution medium), accompanied by the
      !!  Corresponding Source fixed on a durable physical medium
      !!  customarily used for software interchange.
      !!
      !!  \item Convey the object code in, or embodied in, a physical product
      !!  (including a physical distribution medium), accompanied by a
      !!  written offer, valid for at least three years and valid for as
      !!  long as you offer spare parts or customer support for that product
      !!  model, to give anyone who possesses the object code either (1) a
      !!  copy of the Corresponding Source for all the software in the
      !!  product that is covered by this License, on a durable physical
      !!  medium customarily used for software interchange, for a price no
      !!  more than your reasonable cost of physically performing this
      !!  conveying of source, or (2) access to copy the
      !!  Corresponding Source from a network server at no charge.
      !!
      !!  \item Convey individual copies of the object code with a copy of the
      !!  written offer to provide the Corresponding Source.  This
      !!  alternative is allowed only occasionally and noncommercially, and
      !!  only if you received the object code with such an offer, in accord
      !!  with subsection 6b.
      !!
      !!  \item Convey the object code by offering access from a designated
      !!  place (gratis or for a charge), and offer equivalent access to the
      !!  Corresponding Source in the same way through the same place at no
      !!  further charge.  You need not require recipients to copy the
      !!  Corresponding Source along with the object code.  If the place to
      !!  copy the object code is a network server, the Corresponding Source
      !!  may be on a different server (operated by you or a third party)
      !!  that supports equivalent copying facilities, provided you maintain
      !!  clear directions next to the object code saying where to find the
      !!  Corresponding Source.  Regardless of what server hosts the
      !!  Corresponding Source, you remain obligated to ensure that it is
      !!  available for as long as needed to satisfy these requirements.
      !!
      !!  \item Convey the object code using peer-to-peer transmission, provided
      !!  you inform other peers where the object code and Corresponding
      !!  Source of the work are being offered to the general public at no
      !!  charge under subsection 6d.
      !!  \end{enumerate}
      !!
      !!A separable portion of the object code, whose source code is excluded
      !!from the Corresponding Source as a System Library, need not be
      !!included in conveying the object code work.
      !!
      !!A ``User Product'' is either (1) a ``consumer product'', which means any
      !!tangible personal property which is normally used for personal, family,
      !!or household purposes, or (2) anything designed or sold for incorporation
      !!into a dwelling.  In determining whether a product is a consumer product,
      !!doubtful cases shall be resolved in favor of coverage.  For a particular
      !!product received by a particular user, ``normally used'' refers to a
      !!typical or common use of that class of product, regardless of the status
      !!of the particular user or of the way in which the particular user
      !!actually uses, or expects or is expected to use, the product.  A product
      !!is a consumer product regardless of whether the product has substantial
      !!commercial, industrial or non-consumer uses, unless such uses represent
      !!the only significant mode of use of the product.
      !!
      !!``Installation Information'' for a User Product means any methods,
      !!procedures, authorization keys, or other information required to install
      !!and execute modified versions of a covered work in that User Product from
      !!a modified version of its Corresponding Source.  The information must
      !!suffice to ensure that the continued functioning of the modified object
      !!code is in no case prevented or interfered with solely because
      !!modification has been made.
      !!
      !!If you convey an object code work under this section in, or with, or
      !!specifically for use in, a User Product, and the conveying occurs as
      !!part of a transaction in which the right of possession and use of the
      !!User Product is transferred to the recipient in perpetuity or for a
      !!fixed term (regardless of how the transaction is characterized), the
      !!Corresponding Source conveyed under this section must be accompanied
      !!by the Installation Information.  But this requirement does not apply
      !!if neither you nor any third party retains the ability to install
      !!modified object code on the User Product (for example, the work has
      !!been installed in ROM).
      !!
      !!The requirement to provide Installation Information does not include a
      !!requirement to continue to provide support service, warranty, or updates
      !!for a work that has been modified or installed by the recipient, or for
      !!the User Product in which it has been modified or installed.  Access to a
      !!network may be denied when the modification itself materially and
      !!adversely affects the operation of the network or violates the rules and
      !!protocols for communication across the network.
      !!
      !!Corresponding Source conveyed, and Installation Information provided,
      !!in accord with this section must be in a format that is publicly
      !!documented (and with an implementation available to the public in
      !!source code form), and must require no special password or key for
      !!unpacking, reading or copying.
      !!
      !!\item Additional Terms.
      !!
      !!``Additional permissions'' are terms that supplement the terms of this
      !!License by making exceptions from one or more of its conditions.
      !!Additional permissions that are applicable to the entire Program shall
      !!be treated as though they were included in this License, to the extent
      !!that they are valid under applicable law.  If additional permissions
      !!apply only to part of the Program, that part may be used separately
      !!under those permissions, but the entire Program remains governed by
      !!this License without regard to the additional permissions.
      !!
      !!When you convey a copy of a covered work, you may at your option
      !!remove any additional permissions from that copy, or from any part of
      !!it.  (Additional permissions may be written to require their own
      !!removal in certain cases when you modify the work.)  You may place
      !!additional permissions on material, added by you to a covered work,
      !!for which you have or can give appropriate copyright permission.
      !!
      !!Notwithstanding any other provision of this License, for material you
      !!add to a covered work, you may (if authorized by the copyright holders of
      !!that material) supplement the terms of this License with terms:
      !!  \begin{enumerate}
      !!  \item Disclaiming warranty or limiting liability differently from the
      !!  terms of sections 15 and 16 of this License; or
      !!
      !!  \item Requiring preservation of specified reasonable legal notices or
      !!  author attributions in that material or in the Appropriate Legal
      !!  Notices displayed by works containing it; or
      !!
      !!  \item Prohibiting misrepresentation of the origin of that material, or
      !!  requiring that modified versions of such material be marked in
      !!  reasonable ways as different from the original version; or
      !!
      !!  \item Limiting the use for publicity purposes of names of licensors or
      !!  authors of the material; or
      !!
      !!  \item Declining to grant rights under trademark law for use of some
      !!  trade names, trademarks, or service marks; or
      !!
      !!  \item Requiring indemnification of licensors and authors of that
      !!  material by anyone who conveys the material (or modified versions of
      !!  it) with contractual assumptions of liability to the recipient, for
      !!  any liability that these contractual assumptions directly impose on
      !!  those licensors and authors.
      !!  \end{enumerate}
      !!
      !!All other non-permissive additional terms are considered ``further
      !!restrictions'' within the meaning of section 10.  If the Program as you
      !!received it, or any part of it, contains a notice stating that it is
      !!governed by this License along with a term that is a further
      !!restriction, you may remove that term.  If a license document contains
      !!a further restriction but permits relicensing or conveying under this
      !!License, you may add to a covered work material governed by the terms
      !!of that license document, provided that the further restriction does
      !!not survive such relicensing or conveying.
      !!
      !!If you add terms to a covered work in accord with this section, you
      !!must place, in the relevant source files, a statement of the
      !!additional terms that apply to those files, or a notice indicating
      !!where to find the applicable terms.
      !!
      !!Additional terms, permissive or non-permissive, may be stated in the
      !!form of a separately written license, or stated as exceptions;
      !!the above requirements apply either way.
      !!
      !!\item Termination.
      !!
      !!You may not propagate or modify a covered work except as expressly
      !!provided under this License.  Any attempt otherwise to propagate or
      !!modify it is void, and will automatically terminate your rights under
      !!this License (including any patent licenses granted under the third
      !!paragraph of section 11).
      !!
      !!However, if you cease all violation of this License, then your
      !!license from a particular copyright holder is reinstated (a)
      !!provisionally, unless and until the copyright holder explicitly and
      !!finally terminates your license, and (b) permanently, if the copyright
      !!holder fails to notify you of the violation by some reasonable means
      !!prior to 60 days after the cessation.
      !!
      !!Moreover, your license from a particular copyright holder is
      !!reinstated permanently if the copyright holder notifies you of the
      !!violation by some reasonable means, this is the first time you have
      !!received notice of violation of this License (for any work) from that
      !!copyright holder, and you cure the violation prior to 30 days after
      !!your receipt of the notice.
      !!
      !!Termination of your rights under this section does not terminate the
      !!licenses of parties who have received copies or rights from you under
      !!this License.  If your rights have been terminated and not permanently
      !!reinstated, you do not qualify to receive new licenses for the same
      !!material under section 10.
      !!
      !!\item Acceptance Not Required for Having Copies.
      !!
      !!You are not required to accept this License in order to receive or
      !!run a copy of the Program.  Ancillary propagation of a covered work
      !!occurring solely as a consequence of using peer-to-peer transmission
      !!to receive a copy likewise does not require acceptance.  However,
      !!nothing other than this License grants you permission to propagate or
      !!modify any covered work.  These actions infringe copyright if you do
      !!not accept this License.  Therefore, by modifying or propagating a
      !!covered work, you indicate your acceptance of this License to do so.
      !!
      !!\item Automatic Licensing of Downstream Recipients.
      !!
      !!Each time you convey a covered work, the recipient automatically
      !!receives a license from the original licensors, to run, modify and
      !!propagate that work, subject to this License.  You are not responsible
      !!for enforcing compliance by third parties with this License.
      !!
      !!An ``entity transaction'' is a transaction transferring control of an
      !!organization, or substantially all assets of one, or subdividing an
      !!organization, or merging organizations.  If propagation of a covered
      !!work results from an entity transaction, each party to that
      !!transaction who receives a copy of the work also receives whatever
      !!licenses to the work the party's predecessor in interest had or could
      !!give under the previous paragraph, plus a right to possession of the
      !!Corresponding Source of the work from the predecessor in interest, if
      !!the predecessor has it or can get it with reasonable efforts.
      !!
      !!You may not impose any further restrictions on the exercise of the
      !!rights granted or affirmed under this License.  For example, you may
      !!not impose a license fee, royalty, or other charge for exercise of
      !!rights granted under this License, and you may not initiate litigation
      !!(including a cross-claim or counterclaim in a lawsuit) alleging that
      !!any patent claim is infringed by making, using, selling, offering for
      !!sale, or importing the Program or any portion of it.
      !!
      !!\item Patents.
      !!
      !!A ``contributor'' is a copyright holder who authorizes use under this
      !!License of the Program or a work on which the Program is based.  The
      !!work thus licensed is called the contributor's ``contributor version''.
      !!
      !!A contributor's ``essential patent claims'' are all patent claims
      !!owned or controlled by the contributor, whether already acquired or
      !!hereafter acquired, that would be infringed by some manner, permitted
      !!by this License, of making, using, or selling its contributor version,
      !!but do not include claims that would be infringed only as a
      !!consequence of further modification of the contributor version.  For
      !!purposes of this definition, ``control'' includes the right to grant
      !!patent sublicenses in a manner consistent with the requirements of
      !!this License.
      !!
      !!Each contributor grants you a non-exclusive, worldwide, royalty-free
      !!patent license under the contributor's essential patent claims, to
      !!make, use, sell, offer for sale, import and otherwise run, modify and
      !!propagate the contents of its contributor version.
      !!
      !!In the following three paragraphs, a ``patent license'' is any express
      !!agreement or commitment, however denominated, not to enforce a patent
      !!(such as an express permission to practice a patent or covenant not to
      !!sue for patent infringement).  To ``grant'' such a patent license to a
      !!party means to make such an agreement or commitment not to enforce a
      !!patent against the party.
      !!
      !!If you convey a covered work, knowingly relying on a patent license,
      !!and the Corresponding Source of the work is not available for anyone
      !!to copy, free of charge and under the terms of this License, through a
      !!publicly available network server or other readily accessible means,
      !!then you must either (1) cause the Corresponding Source to be so
      !!available, or (2) arrange to deprive yourself of the benefit of the
      !!patent license for this particular work, or (3) arrange, in a manner
      !!consistent with the requirements of this License, to extend the patent
      !!license to downstream recipients.  ``Knowingly relying'' means you have
      !!actual knowledge that, but for the patent license, your conveying the
      !!covered work in a country, or your recipient's use of the covered work
      !!in a country, would infringe one or more identifiable patents in that
      !!country that you have reason to believe are valid.
      !!
      !!If, pursuant to or in connection with a single transaction or
      !!arrangement, you convey, or propagate by procuring conveyance of, a
      !!covered work, and grant a patent license to some of the parties
      !!receiving the covered work authorizing them to use, propagate, modify
      !!or convey a specific copy of the covered work, then the patent license
      !!you grant is automatically extended to all recipients of the covered
      !!work and works based on it.
      !!
      !!A patent license is ``discriminatory'' if it does not include within
      !!the scope of its coverage, prohibits the exercise of, or is
      !!conditioned on the non-exercise of one or more of the rights that are
      !!specifically granted under this License.  You may not convey a covered
      !!work if you are a party to an arrangement with a third party that is
      !!in the business of distributing software, under which you make payment
      !!to the third party based on the extent of your activity of conveying
      !!the work, and under which the third party grants, to any of the
      !!parties who would receive the covered work from you, a discriminatory
      !!patent license (a) in connection with copies of the covered work
      !!conveyed by you (or copies made from those copies), or (b) primarily
      !!for and in connection with specific products or compilations that
      !!contain the covered work, unless you entered into that arrangement,
      !!or that patent license was granted, prior to 28 March 2007.
      !!
      !!Nothing in this License shall be construed as excluding or limiting
      !!any implied license or other defenses to infringement that may
      !!otherwise be available to you under applicable patent law.
      !!
      !!\item No Surrender of Others' Freedom.
      !!
      !!If conditions are imposed on you (whether by court order, agreement or
      !!otherwise) that contradict the conditions of this License, they do not
      !!excuse you from the conditions of this License.  If you cannot convey a
      !!covered work so as to satisfy simultaneously your obligations under this
      !!License and any other pertinent obligations, then as a consequence you may
      !!not convey it at all.  For example, if you agree to terms that obligate you
      !!to collect a royalty for further conveying from those to whom you convey
      !!the Program, the only way you could satisfy both those terms and this
      !!License would be to refrain entirely from conveying the Program.
      !!
      !!\item Use with the GNU Affero General Public License.
      !!
      !!Notwithstanding any other provision of this License, you have
      !!permission to link or combine any covered work with a work licensed
      !!under version 3 of the GNU Affero General Public License into a single
      !!combined work, and to convey the resulting work.  The terms of this
      !!License will continue to apply to the part which is the covered work,
      !!but the special requirements of the GNU Affero General Public License,
      !!section 13, concerning interaction through a network will apply to the
      !!combination as such.
      !!
      !!\item Revised Versions of this License.
      !!
      !!The Free Software Foundation may publish revised and/or new versions of
      !!the GNU General Public License from time to time.  Such new versions will
      !!be similar in spirit to the present version, but may differ in detail to
      !!address new problems or concerns.
      !!
      !!Each version is given a distinguishing version number.  If the
      !!Program specifies that a certain numbered version of the GNU General
      !!Public License ``or any later version'' applies to it, you have the
      !!option of following the terms and conditions either of that numbered
      !!version or of any later version published by the Free Software
      !!Foundation.  If the Program does not specify a version number of the
      !!GNU General Public License, you may choose any version ever published
      !!by the Free Software Foundation.
      !!
      !!If the Program specifies that a proxy can decide which future
      !!versions of the GNU General Public License can be used, that proxy's
      !!public statement of acceptance of a version permanently authorizes you
      !!to choose that version for the Program.
      !!
      !!Later license versions may give you additional or different
      !!permissions.  However, no additional obligations are imposed on any
      !!author or copyright holder as a result of your choosing to follow a
      !!later version.
      !!
      !!\item Disclaimer of Warranty.
      !!
      !!\begin{sloppypar}
      !! THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
      !! APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE
      !! COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM ``AS IS''
      !! WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED,
      !! INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
      !! MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE
      !! RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU.
      !! SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL
      !! NECESSARY SERVICING, REPAIR OR CORRECTION.
      !!\end{sloppypar}
      !!
      !!\item Limitation of Liability.
      !!
      !! IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN
      !! WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES
      !! AND/OR CONVEYS THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR
      !! DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL
      !! DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM
      !! (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED
      !! INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE
      !! OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH
      !! HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
      !! DAMAGES.
      !!
      !!\item Interpretation of Sections 15 and 16.
      !!
      !!If the disclaimer of warranty and limitation of liability provided
      !!above cannot be given local legal effect according to their terms,
      !!reviewing courts shall apply local law that most closely approximates
      !!an absolute waiver of all civil liability in connection with the
      !!Program, unless a warranty or assumption of liability accompanies a
      !!copy of the Program in return for a fee.
      !!
      !!\begin{center}
      !!{\Large\sc End of Terms and Conditions}
      !!
      !!\bigskip
      !!How to Apply These Terms to Your New Programs
      !!\end{center}
      !!
      !!If you develop a new program, and you want it to be of the greatest
      !!possible use to the public, the best way to achieve this is to make it
      !!free software which everyone can redistribute and change under these terms.
      !!
      !!To do so, attach the following notices to the program.  It is safest
      !!to attach them to the start of each source file to most effectively
      !!state the exclusion of warranty; and each file should have at least
      !!the ``copyright'' line and a pointer to where the full notice is found.
      !!
      !!{\footnotesize
      !!\begin{verbatim}
      !!<one line to give the program's name and a brief idea of what it does.>
      !!
      !!Copyright (C) <textyear>  <name of author>
      !!
      !!This program is free software: you can redistribute it and/or modify
      !!it under the terms of the GNU General Public License as published by
      !!the Free Software Foundation, either version 3 of the License, or
      !!(at your option) any later version.
      !!
      !!This program is distributed in the hope that it will be useful,
      !!but WITHOUT ANY WARRANTY; without even the implied warranty of
      !!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      !!GNU General Public License for more details.
      !!
      !!You should have received a copy of the GNU General Public License
      !!along with this program.  If not, see <http://www.gnu.org/licenses/>.
      !!\end{verbatim}
      !!}
      !!
      !!Also add information on how to contact you by electronic and paper mail.
      !!
      !!If the program does terminal interaction, make it output a short
      !!notice like this when it starts in an interactive mode:
      !!
      !!{\footnotesize
      !!\begin{verbatim}
      !!<program>  Copyright (C) <year>  <name of author>
      !!
      !!This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
      !!This is free software, and you are welcome to redistribute it
      !!under certain conditions; type `show c' for details.
      !!\end{verbatim}
      !!}
      !!
      !!The hypothetical commands {\tt show w} and {\tt show c} should show
      !!the appropriate
      !!parts of the General Public License.  Of course, your program's commands
      !!might be different; for a GUI interface, you would use an ``about box''.
      !!
      !!You should also get your employer (if you work as a programmer) or
      !!school, if any, to sign a ``copyright disclaimer'' for the program, if
      !!necessary.  For more information on this, and how to apply and follow
      !!the GNU GPL, see \texttt{http://www.gnu.org/licenses/}.
      !!
      !!The GNU General Public License does not permit incorporating your
      !!program into proprietary programs.  If your program is a subroutine
      !!library, you may consider it more useful to permit linking proprietary
      !!applications with the library.  If this is what you want to do, use
      !!the GNU Lesser General Public License instead of this License.  But
      !!first, please read \newline\texttt{http://www.gnu.org/philosophy/why-not-lgpl.html}.
      !!
      !!\end{enumerate}

      !(doc)footer
