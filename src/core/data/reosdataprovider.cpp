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

#include "reostimeserieprovider.h"


ReosDataProviderRegistery *ReosDataProviderRegistery::sInstance = nullptr;

ReosDataProviderRegistery::ReosDataProviderRegistery()
{
  registerProviderFactory( new ReosTimeSerieConstantTimeStepMemoryProviderFactory );
  registerProviderFactory( new ReosTimeSerieVariableTimeStepMemoryProviderFactory );
}

void ReosDataProviderRegistery::registerProviderFactory( ReosDataProviderFactory *factory )
{
  mFactories[factory->key()] = std::unique_ptr<ReosDataProviderFactory>( factory );
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
  QString providerKey;
  if ( key.contains( QStringLiteral( "::" ) ) )
  {
    providerKey = key.split( QStringLiteral( "::" ) ).at( 0 );
    dataType = key.split( QStringLiteral( "::" ) ).at( 1 );
  }
  else
    providerKey = key;

  auto it = mFactories.find( providerKey );
  if ( it != mFactories.end() )
    return it->second->createProvider( dataType );
  else
    return nullptr;
}

void ReosDataProviderRegistery::loadDynamicProvider()
{
  QString providerPath = QCoreApplication::applicationDirPath();
  QDir providerDir( providerPath );
  if ( providerDir.cd( QStringLiteral( REOS_PROVIDERS ) ) )
    providerPath = providerDir.absolutePath();
  else
  {
    providerPath = REOS_PROVIDERS;
    providerDir = QDir( providerPath );
  }

  providerDir.setSorting( QDir::Name | QDir::IgnoreCase );
  providerDir.setFilter( QDir::Files | QDir::NoSymLinks );

#if defined(Q_OS_WIN) || defined(__CYGWIN__)
  providerDir.setNameFilters( QStringList( "*.dll" ) );
#elif defined(ANDROID)
  providerDir.setNameFilters( QStringList( "*provider.so" ) );
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
      }
    }
  }
}
