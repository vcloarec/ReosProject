/***************************************************************************
                      main.cpp
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com projetreos@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include <QTimer>
#include <QApplication>
#include <QCoreApplication>
#include <QTranslator>
#include <QDesktopWidget>
#include <QMainWindow>
#include <QStandardPaths>

#include "reosstartingwidget.h"

#include "lekanmainwindow.h"
#include "../Reos/reossettings.h"


#include <QDir>
#include <QDebug>

#include "reosapplication.h"

int main( int argc, char *argv[] )
{
  //Q_INIT_RESOURCE( images );

#ifdef Q_OS_WIN
  _putenv( QString( "%1=%2" ).arg( "GDAL_DATA" ).arg( QDir::currentPath().append( "/../gdal" ) ).toLatin1().data() );
  _putenv( QString( "%1=%2" ).arg( "GDAL_DRIVER_PATH" ).arg( QDir::currentPath().append( "./gdalplugins/" ) ).toLatin1().data() );
#else
  setenv( "GDAL_DATA", QDir::currentPath().append( "/../gdal" ).toLatin1().data(), 0 );
#endif

  ReosApplication a( argc, argv );
  QCoreApplication::setOrganizationName( QStringLiteral( "ReosProject" ) );
  QCoreApplication::setApplicationName( QStringLiteral( "Lekan" ) );

  QSettings::setDefaultFormat( QSettings::IniFormat );

  ReosSettings settings;
  QLocale locale;
//  if ( settings.contains( QStringLiteral( "Locale" ) ) )
//    locale = settings.value( QStringLiteral( "Locale" ) ).toLocale();
//  else
  {
    locale = QLocale::system();
    settings.setValue( QStringLiteral( "Locale" ), locale );
  }

  //TODO ; need totale care of the translation...
//  QTranslator translatorReos;
//  QTranslator translatorQt;
//  QTranslator translatorQGis;
//  qDebug() << QDir::currentPath();
//  qDebug() << "load reos translation" << translatorReos.load( locale, QStringLiteral( "i18n/hydro" ), "_" );
//  translatorQt.load( locale, QStringLiteral( "../i18n/qtbase" ), "_" );
//  translatorQGis.load( locale, QStringLiteral( "../i18n/qgis" ), "_" );

//  a.installTranslator( &translatorReos );
//  a.installTranslator( &translatorQt );
//  a.installTranslator( &translatorQGis );

  std::unique_ptr<LekanMainWindow> w = std::make_unique<LekanMainWindow>();

  ReosVersionMessageBox *versionBox = new ReosVersionMessageBox( w.get(), lekanVersion );
  versionBox->setDefaultWebSite( webSite );

  if ( settings.contains( QStringLiteral( "Windows/MainWindow/geometry" ) ) )
  {
    w->restoreGeometry( settings.value( QStringLiteral( "Windows/MainWindow/geometry" ) ).toByteArray() );
    w->showMaximized();
  }
  else
  {
    w->showMaximized();
  }

  ReosStartingWidget *starting = new ReosStartingWidget( w.get() );
  starting->move( QApplication::desktop()->screenGeometry().center() - starting->rect().center() );
  starting->setBan( QPixmap( ":/images/lekan.svg" ) );

  QTimer::singleShot( 1, starting, [starting]
  {
    starting->exec();
    starting->deleteLater();
  } );

  int ret = a.exec();

  return ret;

}
