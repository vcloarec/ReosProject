/***************************************************************************
                      reosapplication.h
                     --------------------------------------
Date                 : 20-09-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSAPPLICATION_H
#define REOSAPPLICATION_H


#include <QApplication>
#include <QPointer>

#include "reoscore.h"

class ReosCoreModule;

class REOSCORE_EXPORT ReosApplication: public QApplication
{

#ifdef SIP_RUN
    % TypeCode
    // Convert a Python argv list to a conventional C argc count and argv array.
    static char **qtgui_ArgvToC( PyObject *argvlist, int &argc )
    {
      char **argv;

      argc = PyList_GET_SIZE( argvlist );

      // Allocate space for two copies of the argument pointers, plus the
      // terminating NULL.
      if ( ( argv = ( char ** )sipMalloc( 2 * ( argc + 1 ) * sizeof( char * ) ) ) == NULL )
        return NULL;

      // Convert the list.
      for ( int a = 0; a < argc; ++a )
      {
        char *arg;
        // Get the argument and allocate memory for it.
        if ( ( arg = PyBytes_AsString( PyList_GET_ITEM( argvlist, a ) ) ) == NULL ||
             ( argv[a] = ( char * )sipMalloc( strlen( arg ) + 1 ) ) == NULL )
          return NULL;
        // Copy the argument and save a pointer to it.
        strcpy( argv[a], arg );
        argv[a + argc + 1] = argv[a];
      }

      argv[argc + argc + 1] = argv[argc] = NULL;

      return argv;
    }

    // Remove arguments from the Python argv list that have been removed from the
    // C argv array.
    static void qtgui_UpdatePyArgv( PyObject *argvlist, int argc, char **argv )
    {
      for ( int a = 0, na = 0; a < argc; ++a )
      {
        // See if it was removed.
        if ( argv[na] == argv[a + argc + 1] )
          ++na;
        else
          PyList_SetSlice( argvlist, na, na + 1, NULL );
      }
    }
    % End
#endif

    Q_OBJECT
  public:

    ~ReosApplication();
    bool notify( QObject *receiver, QEvent *event ) override;

#ifndef SIP_RUN
    static ReosApplication *initializationReos( int &argc, char **argv, const QString &appName = QStringLiteral( "Reos" ) );
#else

    /**
     * Initialization of a ReosApplication. Returns a pointer to the ReosApplication singleton, caller take ownership
     *
     * \param argv command line arguments
     */
    static ReosApplication *initializationReos( SIP_PYLIST argv );
    % MethodCode
    // The Python interface is a list of argument strings that is modified.

    int argc;
    char **argv;

    if ( ( argv = qtgui_ArgvToC( a0, argc ) ) == NULL )
      sipIsErr = 1;
    else
    {
      // Create it now the arguments are right.
      static int nargc = argc;

      // Now modify the original list.
      qtgui_UpdatePyArgv( a0, argc, argv );

      return  sipConvertFromType( ReosApplication::initializationReos( argc, argv ), sipType_ReosApplication, NULL );
    }

    % End
#endif

    //! Returns a pointer to the core module that contains all other modules
    ReosCoreModule *coreModule() const;

    //! Returns the path of engine library files
    static QString enginesPath();

    //! Returns the path of data provider library files
    static QString dataProviderpath();

    //! Returns the path of gis provider library files
    static QString gisProviderPath();

#ifndef SIP_RUN

    static QString i18nPath();

    static QString styleSheet();

#endif // no SIP_RUN

  private:

    ReosApplication( int &argc, char **argv, const QString &appName = QStringLiteral( "Reos" ) );

    QPointer<ReosCoreModule> mCoreModule;
    static QString sReosPrefix;
    static QString resolvePath( const QString &subDir );
};


#endif // REOSAPPLICATION_H
