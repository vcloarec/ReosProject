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


#include <QApplication>
#include <QCoreApplication>
#include <QTranslator>
#include <QDesktopWidget>

#include "lekanmainwindow.h"
#include "../Reos/reossettings.h"


#include <QDir>
#include <QDebug>

int main( int argc, char *argv[] )
{

  qDebug() << QDir::currentPath().append( "/../gdal/" ).toLatin1().data();
#ifdef Q_OS_WIN
  _putenv( QString( "%1=%2" ).arg( "GDAL_DATA" ).arg( QDir::currentPath().append( "/../gdal" ) ).toLatin1().data() );
  _putenv( QString( "%1=%2" ).arg( "GDAL_DRIVER_PATH" ).arg( QDir::currentPath().append( "./gdalplugins/" ) ).toLatin1().data() );
#else
  setenv( "GDAL_DATA", QDir::currentPath().append( "/../gdal" ).toLatin1().data(), 0 );
#endif

  QApplication a( argc, argv );
  QCoreApplication::setOrganizationName( QStringLiteral( "ReosProject" ) );
  QCoreApplication::setApplicationName( QStringLiteral( "Lekan" ) );
  QSettings::setDefaultFormat( QSettings::IniFormat );

  ReosSettings settings;
  QLocale locale;
  if ( settings.contains( QStringLiteral( "Locale" ) ) )
    locale = settings.value( QStringLiteral( "Locale" ) ).toLocale();
  else
  {
    locale = QLocale::system();
    settings.setValue( QStringLiteral( "Locale" ), locale );
  }

  QTranslator translatorHydro;
  QTranslator translatorQt;
  QTranslator translatorQGis;
  translatorHydro.load( locale, "i18n/hydro", "_" );
  translatorQt.load( locale, "i18n/qtbase", "_" );
  translatorQGis.load( locale, "i18n/qgis", "_" );

  a.installTranslator( &translatorHydro );
  a.installTranslator( &translatorQt );
  a.installTranslator( &translatorQGis );

  LekanMainWindow w;


  if ( settings.contains( QStringLiteral( "Windows/MainWindow/geometry" ) ) )
  {
    w.restoreGeometry( settings.value( QStringLiteral( "Windows/MainWindow/geometry" ) ).toByteArray() );
    w.showMaximized();
  }
  else
  {
    w.showMaximized();
  }


  ReosVersionMessageBox *versionBox = new ReosVersionMessageBox( &w, lekanVersion );
  versionBox->setDefaultWebSite( webSite );


  Demarrage *demarrage = new Demarrage( &w );
  demarrage->move( QApplication::desktop()->screenGeometry().center() - demarrage->rect().center() );
  demarrage->setBan( QPixmap( "://titre_Lekan.png" ) );
  if ( demarrage->exec() )
  {
    if ( settings.contains( QStringLiteral( "Windows/MainWindow/state" ) ) )
    {
      w.addDock();
      w.restoreState( settings.value( QStringLiteral( "Windows/MainWindow/state" ) ).toByteArray() );
    }
    else
    {
      w.addDock();
    }

    if ( demarrage->choixOuvrirProjet() )
      w.open();

  }
  else
  {
    return 0;
  }

  return a.exec();


}
