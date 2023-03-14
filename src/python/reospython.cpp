/***************************************************************************
  reospython.cpp - ReosPython

 ---------------------
 begin                : 11.3.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

// Python.h lust be included first to avoid conflict with "slots" key word used as a variable in python files
#include <Python.h>

#include "reospython.h"

#include <QApplication>
#include <QDebug>

ReosPython::ReosPython()
{
  QString pythonPathEnv = qgetenv( "PYTHONPATH" );
#ifdef _WIN32
  pythonPathEnv += ';';
#else
  pythonPathEnv += ':';
#endif
  pythonPathEnv += QApplication::applicationDirPath() + QStringLiteral( "/python" );
  qputenv( "PYTHONPATH", pythonPathEnv.toUtf8() );
}

ReosPython::~ReosPython()
{
#if SIP_VERSION >= 0x40e06
  exitPython();
#endif
}

void ReosPython::initialize()
{
  initPython();
}

bool ReosPython::evalString( const QString &command, QString &result )
{
  // acquire global interpreter lock to ensure we are in a consistent state
  PyGILState_STATE gstate;
  gstate = PyGILState_Ensure();

  PyObject *res = PyRun_String( command.toUtf8().constData(), Py_eval_input, mMainDict, mMainDict );
  const bool success = nullptr != res;

  if ( success )
    result = PyObjectToQString( res );

  Py_XDECREF( res );

  // we are done calling python API, release global interpreter lock
  PyGILState_Release( gstate );

  return success;
}

bool ReosPython::runString( const QString &command, QString &error, bool single )
{
  error = runStringUnsafe( command, single );
  return error.isEmpty();
}

QString ReosPython::runStringUnsafe( const QString &command, bool single )
{
  // acquire global interpreter lock to ensure we are in a consistent state
  PyGILState_STATE gstate;
  gstate = PyGILState_Ensure();
  QString ret;

  // TODO: convert special characters from unicode strings u"…" to \uXXXX
  // so that they're not mangled to utf-8
  // (non-unicode strings can be mangled)
  PyObject *obj = PyRun_String( command.toUtf8().constData(), single ? Py_single_input : Py_file_input, mMainDict, mMainDict );
  PyObject *errobj = PyErr_Occurred();
  if ( nullptr != errobj )
  {
    ret = getTraceback();
  }
  Py_XDECREF( obj );

  // we are done calling python API, release global interpreter lock
  PyGILState_Release( gstate );

  return ret;
}

QString ReosPython::getTraceback()
{
#define TRACEBACK_FETCH_ERROR(what) {errMsg = what; goto done;}

  QString errMsg;
  QString result;

  PyObject *modStringIO = nullptr;
  PyObject *modTB = nullptr;
  PyObject *obStringIO = nullptr;
  PyObject *obResult = nullptr;

  PyObject *type = nullptr, *value = nullptr, *traceback = nullptr;

  PyErr_Fetch( &type, &value, &traceback );
  PyErr_NormalizeException( &type, &value, &traceback );

  const char *iomod = "io";

  modStringIO = PyImport_ImportModule( iomod );
  if ( !modStringIO )
    TRACEBACK_FETCH_ERROR( QStringLiteral( "can't import %1" ).arg( iomod ) );

  obStringIO = PyObject_CallMethod( modStringIO, reinterpret_cast< const char * >( "StringIO" ), nullptr );

  /* Construct a cStringIO object */
  if ( !obStringIO )
    TRACEBACK_FETCH_ERROR( QStringLiteral( "cStringIO.StringIO() failed" ) );

  modTB = PyImport_ImportModule( "traceback" );
  if ( !modTB )
    TRACEBACK_FETCH_ERROR( QStringLiteral( "can't import traceback" ) );

  obResult = PyObject_CallMethod( modTB,  reinterpret_cast< const char * >( "print_exception" ),
                                  reinterpret_cast< const char * >( "OOOOO" ),
                                  type, value ? value : Py_None,
                                  traceback ? traceback : Py_None,
                                  Py_None,
                                  obStringIO );

  if ( !obResult )
    TRACEBACK_FETCH_ERROR( QStringLiteral( "traceback.print_exception() failed" ) );

  Py_DECREF( obResult );

  obResult = PyObject_CallMethod( obStringIO,  reinterpret_cast< const char * >( "getvalue" ), nullptr );
  if ( !obResult )
    TRACEBACK_FETCH_ERROR( QStringLiteral( "getvalue() failed." ) );

  /* And it should be a string all ready to go - duplicate it. */
  if ( !PyUnicode_Check( obResult ) )
    TRACEBACK_FETCH_ERROR( QStringLiteral( "getvalue() did not return a string" ) );

  result = QString::fromUtf8( PyUnicode_AsUTF8( obResult ) );

done:

  // All finished - first see if we encountered an error
  if ( result.isEmpty() && !errMsg.isEmpty() )
  {
    result = errMsg;
  }

  Py_XDECREF( modStringIO );
  Py_XDECREF( modTB );
  Py_XDECREF( obStringIO );
  Py_XDECREF( obResult );
  Py_XDECREF( value );
  Py_XDECREF( traceback );
  Py_XDECREF( type );

  return result;
}

void ReosPython::initPython()
{
#if defined(PY_MAJOR_VERSION) && defined(PY_MINOR_VERSION) && ((PY_MAJOR_VERSION == 3 && PY_MINOR_VERSION >= 8) || PY_MAJOR_VERSION > 3)
  PyStatus status;
  PyPreConfig preconfig;
  PyPreConfig_InitPythonConfig( &preconfig );

  preconfig.utf8_mode = 1;

  status = Py_PreInitialize( &preconfig );
  if ( PyStatus_Exception( status ) )
  {
    Py_ExitStatusException( status );
  }
#endif

  // initialize python
  Py_Initialize();

  mPythonEnabled = true;

  mMainModule = PyImport_AddModule( "__main__" ); // borrowed reference
  mMainDict = PyModule_GetDict( mMainModule ); // borrowed reference
}

void ReosPython::exitPython()
{
  mMainModule = nullptr;
  mMainDict = nullptr;
  mPythonEnabled = false;
}

QString ReosPython::PyObjectToQString( PyObject *obj )
{
  QString result;

  // is it None?
  if ( obj == Py_None )
  {
    return QString();
  }

  // check whether the object is already a unicode string
  if ( PyUnicode_Check( obj ) )
  {
    result = QString::fromUtf8( PyUnicode_AsUTF8( obj ) );
    return result;
  }

  // if conversion to Unicode failed, try to convert it to classic string, i.e. str(obj)
  PyObject *obj_str = PyObject_Str( obj ); // new reference
  if ( obj_str )
  {
    result = QString::fromUtf8( PyUnicode_AsUTF8( obj_str ) );
    Py_XDECREF( obj_str );
    return result;
  }

  // some problem with conversion to Unicode string
  qDebug() << ( QStringLiteral( "unable to convert PyObject to a QString!" ) );
  return QStringLiteral( "(qgis error)" );
}
