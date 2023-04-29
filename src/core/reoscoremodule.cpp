/***************************************************************************
  reoscoremodule.cpp - ReosCoreModule

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
#include "reoscoremodule.h"

#include "reoswatershedmodule.h"
#include "reosgisengine.h"
#include "reoshydraulicnetwork.h"
#include "reosstyleregistery.h"
#include "reosrainfallregistery.h"
#include "reosrunoffmodel.h"
#include "reosversion.h"

#define PROJECT_FILE_MAGIC_NUMBER 19092014


ReosCoreModule::ReosCoreModule( QObject *parent )
  : ReosModule( "core-module", parent )
{
  new ReosGisEngine( this );
  new ReosWatershedModule( this, gisEngine() );
  new ReosHydraulicNetwork( this, gisEngine(), watershedModule() );

  ReosStyleRegistery::instantiate( this );
  ReosRainfallRegistery::instantiate( this );
  ReosRunoffModelRegistery::instantiate( this );
}

ReosGisEngine *ReosCoreModule::gisEngine() const
{
  return module<ReosGisEngine *>();
}

bool ReosCoreModule::openProject( const QString &filePath )
{
  QFile file( filePath );
  QFileInfo fileInfo( filePath );
  if ( !file.open( QIODevice::ReadOnly ) )
    return false;

  clearProject();

  const QDir projectDir( fileInfo.dir() );
  ReosEncodeContext encodeContext;
  encodeContext.setBaseDir( projectDir );

  QDataStream stream( &file );

  //*** read header
  qint32 magicNumber;
  QByteArray bytesVersion;
  stream >> magicNumber;

  if ( magicNumber == PROJECT_FILE_MAGIC_NUMBER )
  {
    // since Lekan 2.2
    qint32 serialisationVersion;
    stream >> serialisationVersion;
    stream >> bytesVersion;
    QDataStream::Version v = static_cast<QDataStream::Version>( serialisationVersion );
    ReosEncodedElement::setSerialisationVersion( v );
  }
  else
  {
    //old version don't have header
    ReosEncodedElement::setSerialisationVersion( QDataStream::Qt_5_12 ); /// TODO : check the Qt version of Lekan 2.0 / 2.1
    stream.device()->reset();
  }

  QByteArray byteArray;
  stream >> byteArray;

  ReosEncodedElement lekanProject( byteArray );
  if ( lekanProject.description() != QStringLiteral( "Lekan-project" ) &&
       lekanProject.description() != QStringLiteral( "reos-project" ) )
    return false;

  QByteArray gisEngineData;
  if ( !lekanProject.getData( QStringLiteral( "GIS-engine" ), gisEngineData ) )
    return false;
  ReosEncodedElement encodedGisEngine( gisEngineData );
  if ( !gisEngine()->decode( lekanProject.getEncodedData( QStringLiteral( "GIS-engine" ) ), projectDir.path(), fileInfo.baseName() ) )
    return false;

  watershedModule()->decode( lekanProject.getEncodedData( QStringLiteral( "watershed-module" ) ), encodeContext );
  hydraulicNetwork()->decode( lekanProject.getEncodedData( QStringLiteral( "hydaulic-network" ) ), projectDir.path(), fileInfo.baseName() );

  return true;
}

bool ReosCoreModule::saveProject( const QString &filePath )
{
  QFileInfo fileInfo( filePath );
  QDir projectDir = fileInfo.dir();

  ReosEncodeContext context;
  context.setBaseDir( projectDir );
  ReosEncodedElement lekanProject( QStringLiteral( "Lekan-project" ) );

  if ( gisEngine() )
    lekanProject.addEncodedData( QStringLiteral( "GIS-engine" ), gisEngine()->encode( projectDir.path(), fileInfo.baseName() ) );
  if ( watershedModule() )
    lekanProject.addEncodedData( QStringLiteral( "watershed-module" ), watershedModule()->encode( context ) );
  if ( hydraulicNetwork() )
    lekanProject.addEncodedData( QStringLiteral( "hydaulic-network" ), hydraulicNetwork()->encode( projectDir.path(), fileInfo.baseName() ) );

  QString completeFilePath = filePath;
  if ( fileInfo.suffix().isEmpty() )
    completeFilePath.append( QStringLiteral( ".lkn" ) );

  QFile file( completeFilePath );
  if ( !file.open( QIODevice::WriteOnly ) )
    return false;

  QDataStream stream( &file );

  //**** header
  qint32 magicNumber = PROJECT_FILE_MAGIC_NUMBER;
  qint32 serialisationVersion = stream.version();

  QByteArray versionBytes = ReosVersion::currentApplicationVersion().bytesVersion();

  Q_ASSERT( versionBytes.size() == 21 );

  stream << magicNumber;
  stream << serialisationVersion;
  stream << versionBytes;
  //*****

  stream << lekanProject.bytes();

  const QFileInfoList filesToRemove = uselessFiles( true );
  for ( const QFileInfo &fi : filesToRemove )
  {
    if ( !fi.exists() )
      continue;
    if ( fi.isDir() )
    {
      QDir dir( fi.filePath() );
      dir.removeRecursively();
    }
    else if ( fi.isFile() )
    {
      QFile fileToRemove( fi.path() );
      fileToRemove.remove();
    }
  }

  return true;
}

ReosWatershedModule *ReosCoreModule::watershedModule() const
{
  return module<ReosWatershedModule *>();
}

ReosHydraulicNetwork *ReosCoreModule::hydraulicNetwork() const
{
  return module<ReosHydraulicNetwork *>();
}

void ReosCoreModule::clearProject()
{
  if ( gisEngine() )
    gisEngine()->clearProject();

  if ( watershedModule() )
    watershedModule()->reset();

  if ( hydraulicNetwork() )
    hydraulicNetwork()->reset();
}
