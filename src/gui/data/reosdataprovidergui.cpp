/***************************************************************************
  reosdataproviderselectorwidget.cpp - ReosDataProviderSelectorWidget

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
#include "reosdataprovidergui.h"
#include "qicon.h"
#include "reosdataprovider.h"

#include <QVariantMap>
#include <QCoreApplication>
#include <QDir>
#include <QLibrary>
#include <QDebug>

ReosDataProviderSelectorWidget *ReosDataProviderGuiFactory::createProviderSelectorWidget( ReosMap *, const QString &, QWidget * ) const
{
  return nullptr;
}

ReosDataProviderSettingsWidget *ReosDataProviderGuiFactory::createProviderSettingsWidget( ReosDataProvider *, QWidget * ) const
{
  return nullptr;
}

QIcon ReosDataProviderGuiFactory::icon() const
{
  return QIcon();
}

ReosDataProviderGuiRegistery *ReosDataProviderGuiRegistery::sInstance = nullptr;

ReosDataProviderGuiRegistery::ReosDataProviderGuiRegistery()
{

}

void ReosDataProviderGuiRegistery::registerProviderGuiFactory( ReosDataProviderGuiFactory *factory )
{
  mFactories[factory->key()] = std::unique_ptr<ReosDataProviderGuiFactory>( factory );
}

QStringList ReosDataProviderGuiRegistery::providers( const QString &dataType, ReosDataProviderGuiFactory::GuiCapability capability ) const
{
  QStringList ret;

  for ( auto const &fact : std::as_const( mFactories ) )
    if ( ( fact.second->capabilities() & capability ) && fact.second->dataType().contains( dataType ) )
      ret.append( fact.second->key() );

  return ret;
}

ReosDataProviderSelectorWidget *ReosDataProviderGuiRegistery::createProviderSelectorWidget( const QString &key, const QString &dataType, ReosMap *map, QWidget *parent )
{
  ReosDataProviderGuiFactory *fact = guiFactory( key );
  if ( fact )
    return fact->createProviderSelectorWidget( map, dataType, parent );

  return nullptr;
}

ReosDataProviderSettingsWidget *ReosDataProviderGuiRegistery::createProviderSettingsWidget( ReosDataProvider *dataProvider, QWidget *parent )
{
  QString providerKey = dataProvider->key();
  if ( providerKey.contains( ':' ) )
    providerKey = providerKey.split( ':' ).at( 0 );

  ReosDataProviderGuiFactory *fact = guiFactory( providerKey );
  if ( fact )
    return fact->createProviderSettingsWidget( dataProvider, parent );

  return nullptr;
}

bool ReosDataProviderGuiRegistery::hasCapability( QString providerKey, ReosDataProviderGuiFactory::GuiCapability capability ) const
{
  ReosDataProviderGuiFactory *fact = guiFactory( providerKey );
  if ( !fact )
    return false;

  return fact->capabilities().testFlag( capability );
}

ReosDataProviderGuiFactory *ReosDataProviderGuiRegistery::guiFactory( const QString &key ) const
{
  auto itFact = mFactories.find( key );
  if ( itFact != mFactories.end() )
    return itFact->second.get();

  return nullptr;
}

QIcon ReosDataProviderGuiRegistery::providerIcon( const QString &key ) const
{
  ReosDataProviderGuiFactory *fact = guiFactory( key );
  if ( fact )
    return fact->icon();
  else
    return QPixmap();
}

QString ReosDataProviderGuiRegistery::providerDisplayText( const QString &key ) const
{
  ReosDataProviderGuiFactory *fact = guiFactory( key );
  if ( fact )
    return fact->displayText();

  return QString();
}

ReosDataProviderGuiRegistery *ReosDataProviderGuiRegistery::instance()
{
  if ( !sInstance )
  {
    sInstance = new ReosDataProviderGuiRegistery();
    sInstance->loadDynamicProvider();
  }

  return sInstance;
}

void ReosDataProviderGuiRegistery::loadDynamicProvider()
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

  typedef ReosDataProviderGuiFactory *factory_function( );

  const QFileInfoList files = providerDir.entryInfoList();
  for ( const QFileInfo &file : files )
  {
    QLibrary library( file.filePath() );
    if ( library.load() )
    {
      QFunctionPointer fcp = library.resolve( "providerGuiFactory" );
      factory_function *func = reinterpret_cast<factory_function *>( fcp );

      if ( func )
      {
        ReosDataProviderGuiFactory *providerFactory = func();
        registerProviderGuiFactory( providerFactory );
      }
    }
    else
      qDebug() << library.errorString();
  }
}

ReosDataObject *ReosDataProviderSelectorWidget::createData( QObject * ) const {return nullptr;}

ReosDataObject *ReosDataProviderSelectorWidget::selectedData() const {return nullptr;}

QVariantMap ReosDataProviderSelectorWidget::selectedMetadata() const
{
  return QVariantMap();
}

ReosDataProviderSettingsWidget::ReosDataProviderSettingsWidget( QWidget *parent ): QWidget( parent ) {}
