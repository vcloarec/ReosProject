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
#include <qstylefactory.h>
#include <QCoreApplication>
#include <QTranslator>
#include <QDesktopWidget>
#include <QMainWindow>
#include <QStandardPaths>
#include <QScreen>

#ifdef _MSC_VER
#include <Windows.h>
#include <errhandlingapi.h>
#undef max
#undef min
#endif

#include "reosstartingwidget.h"

#include "lekanmainwindow.h"
#include "reossettings.h"
#include "reosremoteinformation.h"


#include <QDir>

#include "reosapplication.h"

#ifdef _MSC_VER
LONG WINAPI handleCrash(LPEXCEPTION_POINTERS exception)
{
    QMessageBox messBox(QMessageBox::Critical, QObject::tr("Lekan Crashes"), "", QMessageBox::Close);
    messBox.setTextFormat(Qt::RichText);
    messBox.setText( QObject::tr(  "Lekan just crahes....<br>"
                                   "Hopping you don't loose your work...<br><br>"
                                   "If you are not too angry, and if you manage to reproduce this crash,<br>"
                                   "you are cordially invited to report it (see <a href = \"https://www.reos.site/en/how-to-support/\"> here </a> how to).<br><br>"
                                   "By this, you participate to improve this software and also your future uses."));
    messBox.exec();
    return TRUE;
}
#endif

int main( int argc, char *argv[] )
{
#ifdef _MSC_VER
  qputenv( "PATH", "C:\\WINDOWS\\system32;C:\\WINDOWS;C:\\WINDOWS\\system32\\WBem" );

  QString arg(argv[1]);
  if (arg!="test")
    SetUnhandledExceptionFilter(handleCrash);
#endif

  ReosApplication a( argc, argv );

#ifdef _MSC_VER
  qputenv( "PATH", "C:\\WINDOWS\\system32;C:\\WINDOWS;C:\\WINDOWS\\system32\\WBem" );
  if (qgetenv("GDAL_DATA").isEmpty())
  {
    QString gdalData = QCoreApplication::applicationDirPath();
    gdalData.append( "\\..\\share\\gdal" );
    qputenv( "GDAL_DATA", gdalData.toUtf8().constData() );
  }

#endif

  QCoreApplication::setOrganizationName( QStringLiteral( "ReosProject" ) );
  QCoreApplication::setApplicationName( QStringLiteral( "Lekan" ) );

  QApplication::setStyle( QStyleFactory::create( "fusion" ) );

  QSettings::setDefaultFormat( QSettings::IniFormat );

  ReosSettings settings;
  QLocale localeGlobal;
  if ( settings.contains( QStringLiteral( "Locale-global" ) ) )
    localeGlobal = settings.value( QStringLiteral( "Locale-global" ) ).toLocale();
  else
    localeGlobal = QLocale::system();

  QLocale::setDefault( localeGlobal );

  QLocale localeLanguage;
  if ( settings.contains( QStringLiteral( "Locale-language" ) ) )
    localeLanguage = settings.value( QStringLiteral( "Locale-language" ) ).toLocale();
  else
    localeLanguage = QLocale::system();

  QTranslator ReosTranslator;
  QTranslator QtTranslator;
  QTranslator QgisTranslator;

  QString i18nPath = ReosApplication::i18nPath();

  if ( QtTranslator.load( localeLanguage, i18nPath + QStringLiteral( "/qtbase" ), "_" ) )
    a.installTranslator( &QtTranslator );
  if ( QgisTranslator.load( localeLanguage, i18nPath + QStringLiteral( "/qgis" ), "_" ) )
    a.installTranslator( &QgisTranslator );
  if ( ReosTranslator.load( localeLanguage, i18nPath + QStringLiteral( "/reos" ), "_" ) )
    a.installTranslator( &ReosTranslator );

  std::unique_ptr<LekanMainWindow> w = std::make_unique<LekanMainWindow>();
  new ReosVersionMessageBox( w.get(), ReosVersion::currentApplicationVersion() );

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
  starting->move( w->screen()->geometry().center() - starting->rect().center() );
  starting->setBan( QPixmap( ":/images/lekan.svg" ) );

  QTimer::singleShot( 1, starting, [starting]
  {
    starting->exec();
    starting->deleteLater();
  } );

  int ret = a.exec();

  return ret;

}
