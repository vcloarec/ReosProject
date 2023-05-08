/***************************************************************************
  reospython.h - ReosPython

  from QGIS, qgspythonutils.h  / qgspythonutils.cpp
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
#ifndef REOSPYTHON_H
#define REOSPYTHON_H

#include "reos_python.h"
#include "reosmodule.h"

// forward declaration for PyObject
#ifndef PyObject_HEAD
struct _object;
typedef _object PyObject;
#endif

class REOS_PYTHON_EXPORT ReosPython : public ReosModule
{
  public:
    ReosPython();
    ~ReosPython();

    void initialize();

    bool evalString( const QString &command, QString &result );

    bool runString( const QString &command, QString &error, bool single = true );
    QString runStringUnsafe( const QString &command, bool single );
    QString getTraceback();

  private:
    void initPython();
    void exitPython();

    //! convert Python object to QString. If the object isn't unicode/str, it will be converted
    QString PyObjectToQString( PyObject *obj );

    //! reference to module __main__
    PyObject *mMainModule = nullptr;

    //! dictionary of module __main__
    PyObject *mMainDict = nullptr;

    //! flag determining that Python support is enabled
    bool mPythonEnabled = false;
};

#endif // REOSPYTHON_H
