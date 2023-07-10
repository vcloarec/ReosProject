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
#include <QTranslator>
#include <QStyleFactory>
#include <QDebug>

#include "reosmapextent.h"
#include "reossettings.h"
#include "reoscoremodule.h"
#include "reoswatershedmodule.h"

QString ReosApplication::sReosPrefix = qgetenv( "REOS_PREFIX_PATH" );

ReosApplication::ReosApplication( int &argc, char **argv, bool guiEnabled, const QString &appName )
  : QApplication( argc, argv, guiEnabled )
{
  QCoreApplication::setOrganizationName( QStringLiteral( "ReosProject" ) );
  QCoreApplication::setApplicationName( appName );

  sReosPrefix = qgetenv( "REOS_PREFIX_PATH" );
  if ( sReosPrefix.isEmpty() )
  {
    sReosPrefix = QApplication::applicationDirPath();
    if ( sReosPrefix.endsWith( "/bin" ) )
    {
      sReosPrefix.chop( 4 );
    }
  }

  qDebug() << QStringLiteral( "Reos prefix path is: %1" ).arg( sReosPrefix );

  QApplication::setStyle( QStyleFactory::create( "fusion" ) );

  qRegisterMetaType<ReosSpatialPosition>( "ReosSpatialPosition" );

#ifdef _MSC_VER
  if ( qgetenv( "GDAL_DATA" ).isEmpty() )
  {
    QString gdalData = QCoreApplication::applicationDirPath();
    gdalData.append( "\\..\\share\\gdal" );
    qputenv( "GDAL_DATA", gdalData.toUtf8().constData() );
  }

#endif

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
    installTranslator( &QtTranslator );
  if ( QgisTranslator.load( localeLanguage, i18nPath + QStringLiteral( "/qgis" ), "_" ) )
    installTranslator( &QgisTranslator );
  if ( ReosTranslator.load( localeLanguage, i18nPath + QStringLiteral( "/reos" ), "_" ) )
    installTranslator( &ReosTranslator );

  mCoreModule = new ReosCoreModule( this );
}

ReosApplication::~ReosApplication()
{
  if ( mCoreModule )
    delete mCoreModule;
}

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
      QMessageBox::critical( activeWindow(), tr( "Error:" ), e.what() );
  }
  catch ( ... )
  {
    if ( qApp->thread() == QThread::currentThread() )
      QMessageBox::critical( activeWindow(), tr( "Exception" ), tr( "Unknown error" ) );
  }

  return done;
}

QString ReosApplication::i18nPath()
{
  return QApplication::applicationDirPath() + QStringLiteral( "/../i18n" );
}


QString ReosApplication::styleSheet()
{
  QString myStyle = QStringLiteral( ".overview{"
                                    "  font: 1.82em;"
                                    "  font-weight: bold;"
                                    "}"
                                    "body{"
                                    "  background: white;"
                                    "  color: black;"
                                    "  font-family: 'Lato', 'Open Sans', 'Lucida Grande', 'Segoe UI', 'Arial', sans-serif;"
                                    "  width: 100%;"
                                    "}"
                                    "h1{  background-color: #F6F6F6;"
                                    "  color: #0996e6; "
                                    "  font-size: x-large;  "
                                    "  font-weight: normal;"
                                    "  background: none;"
                                    "  padding: 0.75em 0 0;"
                                    "  margin: 0;"
                                    "  line-height: 3em;"
                                    "}"
                                    "h2{  background-color: #F6F6F6;"
                                    "  color: #0978b8; "
                                    "  font-size: medium;  "
                                    "  font-weight: normal;"
                                    "  background: none;"
                                    "  padding: 0.75em 0 0;"
                                    "  margin: 0;"
                                    "  line-height: 1.1em;"
                                    "}"
                                    "h3{  background-color: #F6F6F6;"
                                    "  color: #515151;"
                                    "  font-weight: bold;"
                                    "  font-size: large;"
                                    "  text-align: left;"
                                    "  border-bottom: 5px solid #DCEB5C;"
                                    "}"
                                    "h4{  background-color: #F6F6F6;"
                                    "  color: #93b023;"
                                    "  font-weight: bold;"
                                    "  font-size: medium;"
                                    "  text-align: left;"
                                    "}"
                                    "h5{    background-color: #F6F6F6;"
                                    "   color: #93b023;"
                                    "   font-weight: bold;"
                                    "   font-size: small;"
                                    "   text-align: left;"
                                    "}"
                                    "a{  color: #729FCF;"
                                    "  font-family: arial,sans-serif;"
                                    "}"
                                    "label{  background-color: #FFFFCC;"
                                    "  border: 1px solid black;"
                                    "  margin: 1px;"
                                    "  padding: 0px 3px; "
                                    "  font-size: small;"
                                    "}"
                                    "th .strong {"
                                    "  font-weight: bold;"
                                    "}"
                                    "hr {"
                                    "  border: 0;"
                                    "  height: 0;"
                                    "  border-top: 1px solid black;"
                                    "}"
                                    ".list-view .highlight {"
                                    "  text-align: left;"
                                    "  border: 0px;"
                                    "  width: 20%;"
                                    "  padding-right: 15px;"
                                    "  padding-left: 20px;"
                                    "  font-weight: bold;"
                                    "}"
                                    ".tabular-view .odd-row {"
                                    "  background-color: #f9f9f9;"
                                    "}"
                                    ".section {"
                                    "  font-weight: bold;"
                                    "  padding-top:25px;"
                                    "}" );


  myStyle += QStringLiteral(
               ".tabular-view{ "
               "  border-collapse: collapse;"
               "  width: 95%;"
               "}"
               ".tabular-view th, .tabular-view td { "
               "  border:1px solid black;"
               "}" );

  myStyle.append( QStringLiteral( "body { margin: 10px; }\n " ) );

  return myStyle;
}

QString ReosApplication::resolvePath( const QString &subDir )
{
  QDir targetDir( sReosPrefix + QStringLiteral( "/bin" ) );
  if ( targetDir.cd( subDir ) )
    return targetDir.absolutePath();
  else
  {
#ifndef _NDEBUG
    qDebug() << QStringLiteral( "Default path not found (%1) for %2. Try with building directory." ).arg( targetDir.absolutePath(), subDir );
#endif
    QString targetPath = REOS_BUILDING_OUTPUT;
    targetDir = QDir( targetPath );
    if ( targetDir.cd( subDir ) )
      return targetPath = targetDir.absolutePath();
    else
      return subDir;
  }
}

ReosCoreModule *ReosApplication::coreModule() const
{
  return mCoreModule;
}

QString ReosApplication::enginesPath()
{
  return resolvePath( QStringLiteral( REOS_SIMULATION_ENGINES ) );
}

QString ReosApplication::dataProviderpath()
{
  return resolvePath( QStringLiteral( REOS_PROVIDERS ) );
}

QString ReosApplication::gisProviderPath()
{
  return resolvePath( QStringLiteral( "qgisProvider" ) );
}
