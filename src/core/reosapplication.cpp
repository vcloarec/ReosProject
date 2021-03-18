/***************************************************************************
                      reosapplication.cpp
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

#include "reosapplication.h"

#include <QThread>
#include <QMessageBox>

ReosApplication::ReosApplication( int &argc, char **argv, int flag ): QApplication( argc, argv, flag )
{}

bool ReosApplication::notify( QObject *receiver, QEvent *event )
{
  bool done = true;
  try
  {
    done = QApplication::notify( receiver, event );
  }
  catch ( std::exception &e )
  {
    if ( qApp->thread() == QThread::currentThread() )
      QMessageBox::critical( activeWindow(), tr( "Exception" ), e.what() );
  }
  catch ( ... )
  {
    if ( qApp->thread() == QThread::currentThread() )
      QMessageBox::critical( activeWindow(), tr( "Exception" ), tr( "unknown exception" ) );
  }

  return done;
}

QString ReosApplication::i18nPath()
{
  return QApplication::applicationDirPath() + QStringLiteral( "/../i18n" );
}
