/***************************************************************************
  reosdataprovider.cpp - ReosDataProvider

 ---------------------
 begin                : 6.11.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosdataprovider.h"

#include <QCoreApplication>
#include <QDir>
#include <QLibrary>
#include <QDebug>

#include "reostimeseriesprovider.h"


ReosDataProviderRegistery *ReosDataProviderRegistery::sInstance = nullptr;

ReosDataProviderRegistery::ReosDataProviderRegistery()
{
  registerProviderFactory( new ReosTimeSerieConstantTimeStepMemoryProviderFactory );
  registerProviderFactory( new ReosTimeSerieVariableTimeStepMemoryProviderFactory );
}

void ReosDataProviderRegistery::registerProviderFactory( ReosDataProviderFactory *factory )
{
  mFactories[factory->key()] = std::unique_ptr<ReosDataProviderFactory>( factory );
  mListedFactories.append( mFactories.at( factory->key() ).get() );
}

ReosDataProviderRegistery *ReosDataProviderRegistery::instance()
{
  if ( !sInstance )
  {
    sInstance = new ReosDataProviderRegistery();
    sInstance->loadDynamicProvider();
  }

  return sInstance;
}

ReosDataProvider *ReosDataProviderRegistery::createProvider( const QString &key )
{
  QString dataType;
  ReosDataProviderFactory *fact = extractFactory( key, dataType );

  if ( fact )
    return fact->createProvider( dataType );
  else
    return nullptr;
}

ReosDataProvider *ReosDataProviderRegistery::createProvider( const QString &key, const QString &dataType )
{
  auto it = mFactories.find( key );
  if ( it != mFactories.end() )
    return it->second->createProvider( dataType );
  else
    return nullptr;
}

ReosDataProvider *ReosDataProviderRegistery::createCompatibleProvider( const QString &uri, const QString &dataType ) const
{
  for ( ReosDataProviderFactory *fact : mListedFactories )
  {
    std::unique_ptr<ReosDataProvider> provider( fact->createProvider( dataType ) );
    if ( provider && provider->canReadUri( uri ) )
    {
      return provider.release();
    }
  }

  return nullptr;
}

QStringList ReosDataProviderRegistery::withCapabilities( const QString &dataType, ReosDataProvider::Capabilities capabilities )
{
  QStringList ret;
  for ( auto it = mFactories.cbegin(); it != mFactories.cend(); ++it )
    if ( it->second->hasCapabilities( dataType, capabilities ) )
      ret.append( it->first );

  return ret;
}

void ReosDataProviderRegistery::loadDynamicProvider()
{
  QString providerPath = QCoreApplication::applicationDirPath();
  QDir providerDir( providerPath );
  if ( providerDir.cd( QStringLiteral( REOS_PROVIDERS ) ) )
    providerPath = providerDir.absolutePath();
  else
  {
    providerPath = REOS_BUILDING_OUTPUT;
    providerDir = QDir( providerPath );
    if ( providerDir.cd( QStringLiteral( REOS_PROVIDERS ) ) )
      providerPath = providerDir.absolutePath();
    else
    {
      providerPath = REOS_PROVIDERS;
      providerDir = QDir( providerPath );
    }
  }

  providerDir.setSorting( QDir::Name | QDir::IgnoreCase );
  providerDir.setFilter( QDir::Files | QDir::NoSymLinks );

#if defined(Q_OS_WIN) || defined(__CYGWIN__)
  providerDir.setNameFilters( QStringList( "*.dll" ) );
#else
  providerDir.setNameFilters( QStringList( QStringLiteral( "*.so" ) ) );
#endif

  typedef ReosDataProviderFactory *factory_function( );

  const QFileInfoList files = providerDir.entryInfoList();
  for ( const QFileInfo &file : files )
  {
    QLibrary library( file.filePath() );
    if ( library.load() )
    {
      QFunctionPointer fcp = library.resolve( "providerFactory" );
      factory_function *func = reinterpret_cast<factory_function *>( fcp );

      if ( func )
      {
        ReosDataProviderFactory *providerFactory = func();
        registerProviderFactory( providerFactory );
        qDebug() << QString( "Library %1 loaded and conform" ).arg( file.baseName() );
      }
      else
      {
        qDebug() << QString( "Library %1 loaded and not conform" ).arg( file.baseName() );
      }
    }
    else
    {
      qDebug() << QString( "Library %1 not loaded for following reason: %2" ).arg( file.baseName(), library.errorString() );
    }
  }
}

ReosDataProviderFactory *ReosDataProviderRegistery::extractFactory( const QString &key, QString &dataType ) const
{
  QString providerKey;
  if ( key.contains( QStringLiteral( "::" ) ) )
  {
    providerKey = key.split( QStringLiteral( "::" ) ).at( 0 );
    dataType = key.split( QStringLiteral( "::" ) ).at( 1 );
  }
  else
  {
    providerKey = key;
    dataType = QString();
  }

  auto it = mFactories.find( providerKey );
  if ( it != mFactories.end() )
    return it->second.get();
  else
    return nullptr;
}

bool ReosDataProvider::canReadUri( const QString & ) const {return false;}

bool ReosDataProviderFactory::hasCapabilities( const QString &dataType, ReosDataProvider::Capabilities capabilities ) const {return false;}
