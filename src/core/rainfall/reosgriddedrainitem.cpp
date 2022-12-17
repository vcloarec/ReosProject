/***************************************************************************
  reosgriddedrainitem.cpp - ReosGriddedRainItem

 ---------------------
 begin                : 11.11.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosgriddedrainitem.h"

#include "reosgriddedrainfallprovider.h"
#include "reosgriddedrainfallrenderer_p.h"
#include "reosmemoryraster.h"
#include "reosgisengine.h"
#include "reosrenderersettings.h"


ReosGriddedRainfall::ReosGriddedRainfall( QObject *parent )
  : ReosRenderedObject( parent )
  , mProvider( new ReosGriddedRainfallMemoryProvider )
{
  mRendererFactory.reset( new ReosGriddedRainfallRendererFactory_p( this ) );
}


ReosGriddedRainfall::ReosGriddedRainfall( const QString &dataSource, const QString &providerKey, QObject *parent )
  : ReosRenderedObject( parent )
  , mProvider( qobject_cast<ReosGriddedRainfallProvider*>( ReosDataProviderRegistery::instance()->createProvider( providerKey ) ) )
{
  if ( mProvider )
    mProvider->setDataSource( dataSource );

  // renderer factory must be created after set the datasource because, the factory needs the extent of the provider on creation
  mRendererFactory.reset( new ReosGriddedRainfallRendererFactory_p( this ) );
}

ReosGriddedRainfall *ReosGriddedRainfall::decode( const ReosEncodedElement &element,  const ReosEncodeContext &context, QObject *parent )
{
  if ( element.description() != QStringLiteral( "gridded-precipitaton" ) )
    return nullptr;

  return new ReosGriddedRainfall( element, context, parent );
}

ReosGriddedRainfall::ReosGriddedRainfall( const ReosEncodedElement &element, const ReosEncodeContext &context, QObject *parent )
  : ReosRenderedObject( parent )
{
  ReosDataObject::decode( element );
  element.getData( QStringLiteral( "overriden-crs" ), mOverridenCrs );

  QString providerKey;
  element.getData( QStringLiteral( "provider-key" ), providerKey );
  mProvider.reset( qobject_cast<ReosGriddedRainfallProvider *>( ReosDataProviderRegistery::instance()->createProvider( providerKey ) ) );
  if ( mProvider )
    mProvider->decode( element.getEncodedData( QStringLiteral( "provider" ) ), context );

  // renderer factory must be created after set the datasource because, the factory needs the extent of the provider on creation
  mRendererFactory.reset( new ReosGriddedRainfallRendererFactory_p( element.getEncodedData( QStringLiteral( "renderer" ) ), this ) );
}

ReosEncodedElement ReosGriddedRainfall::encode( const ReosEncodeContext &context ) const
{
  ReosEncodedElement element( QStringLiteral( "gridded-precipitaton" ) );
  ReosDataObject::encode( element );
  element.addData( QStringLiteral( "overriden-crs" ), mOverridenCrs );
  element.addEncodedData( QStringLiteral( "renderer" ), mRendererFactory->encode() );

  element.addData( QStringLiteral( "provider-key" ), mProvider->key() );
  element.addEncodedData( QStringLiteral( "provider" ), mProvider->encode( context ) );

  return element;
}

ReosGriddedRainfall::~ReosGriddedRainfall()
{
}

QString ReosGriddedRainfall::type() const {return staticType();}

ReosObjectRenderer *ReosGriddedRainfall::createRenderer( ReosRendererSettings *settings )
{
  if ( mRendererFactory )
    return mRendererFactory->createRasterRenderer( settings );

  return nullptr;
}

ReosRendererObjectMapTimeStamp *ReosGriddedRainfall::createMapTimeStamp( ReosRendererSettings *settings ) const
{
  return new ReosRendererGriddedRainfallMapTimeStamp_p( dataIndex( settings->mapTime() ) );
}

ReosGriddedRainfallProvider *ReosGriddedRainfall::dataProvider() const
{
  return mProvider.get();
}

QString ReosGriddedRainfall::staticType() {return QStringLiteral( "gridded-precipitation" );}

int ReosGriddedRainfall::gridCount() const
{
  if ( mProvider )
    return mProvider->count();
  else
    return 0;
}

const QDateTime ReosGriddedRainfall::startTime( int index ) const
{
  if ( mProvider )
    return mProvider->startTime( index );
  else
    return QDateTime();
}

const QDateTime ReosGriddedRainfall::endTime( int index ) const
{
  if ( mProvider )
    return mProvider->endTime( index );
  else
    return QDateTime();
}

QPair<QDateTime, QDateTime> ReosGriddedRainfall::timeExtent() const
{
  if ( !mProvider )
    return QPair<QDateTime, QDateTime>();

  int count = mProvider->count();

  if ( count == 0 )
    return QPair<QDateTime, QDateTime>();

  return {mProvider->startTime( 0 ), mProvider->endTime( count - 1 )};
}

ReosDuration ReosGriddedRainfall::minimumTimeStep() const
{
  if ( !mProvider )
    return ReosDuration();

  int count = mProvider->count();

  if ( count == 0 )
    return ReosDuration();

  ReosDuration ret = ReosDuration( mProvider->startTime( 0 ), mProvider->endTime( 0 ) );

  for ( int i = 1; i < count; ++i )
  {
    ReosDuration dt( mProvider->startTime( i ), mProvider->endTime( i ) );
    if ( dt < ret )
      ret = dt;
  }

  return ret;
}

const QVector<double> ReosGriddedRainfall::intensityValues( int index ) const
{
  if ( mProvider )
    return mProvider->data( index );
  else
    return QVector<double>();
}

ReosRasterMemory<double> ReosGriddedRainfall::intensityRaster( int index ) const
{
  ReosRasterExtent extent = mProvider->extent();
  ReosRasterMemory<double> ret( extent.yCellCount(), extent.xCellCount() );
  ret.setValues( mProvider->data( index ) );

  return ret;
}

int ReosGriddedRainfall::dataIndex( const QDateTime &time ) const
{
  if ( !mProvider )
    return  -1;

  return mProvider->dataIndex( time );
}

ReosRasterExtent ReosGriddedRainfall::extent() const
{
  if ( mProvider )
  {
    if ( mOverridenCrs.isEmpty() )
      return mProvider->extent();
    ReosRasterExtent extent = mProvider->extent();
    extent.setCrs( mOverridenCrs );
    return extent;
  }
  else
    return ReosRasterExtent();
}

bool ReosGriddedRainfall::isValid() const
{
  return mProvider && mProvider->isValid();
}

void ReosGriddedRainfall::overrideCrs( const QString &crs )
{
  mOverridenCrs = crs;
}

ReosGriddedRainfall *ReosGriddedRainfall::transform( const ReosMapExtent &destination, double resolX, double resolY, QObject *parent ) const
{
  std::unique_ptr<ReosGriddedRainfall> projectedRainfall( new ReosGriddedRainfall( parent ) );

  ReosRasterExtent destinationExtent;
  bool success;
  const ReosRasterExtent sourceExtent = extent();
  auto convertValues = ReosGisEngine::transformRasterExtent( sourceExtent, destination, resolX, resolY, destinationExtent, success );
  int sourceXCount = sourceExtent.xCellCount();

  std::unique_ptr<ReosGriddedRainfallMemoryProvider> newProvider( new ReosGriddedRainfallMemoryProvider );

  newProvider->setExtent( destinationExtent );

  int xCount = destinationExtent.xCellCount();
  int yCount = destinationExtent.yCellCount();

  for ( int i = 0; i < mProvider->count(); ++i )
  {
    const QVector<double> sourceValue = mProvider->data( i );
    ReosRasterMemory<double> raster( yCount, xCount );
    raster.reserveMemory();

    for ( int y = 0; y < yCount; ++y )
    {
      for ( int x = 0; x < xCount; ++x )
      {
        const QList<QPair<double, QPoint>> &pairs = convertValues.value( y, x );
        double value = 0;
        for ( const QPair<double, QPoint> &pair : pairs )
        {
          const QPoint &pt = pair.second;
          double coef = pair.first;
          int valIndex = pt.x() + pt.y() * sourceXCount;
          value += sourceValue.at( valIndex ) * coef;
        }
        raster.setValue( y, x, value );
      }
    }

    newProvider->addFrame( raster, mProvider->startTime( i ), mProvider->endTime( i ) );
  }

  projectedRainfall->mProvider.reset( newProvider.release() );
  return projectedRainfall.release();
}

void ReosGriddedRainfall::copyFrom( ReosGriddedRainfall *other )
{
  mProvider->copyFrom( other->mProvider.get() );

  //! We need to reset the renderer factor tp take account of the new extent
  mRendererFactory.reset( new ReosGriddedRainfallRendererFactory_p( this ) );
}

ReosColorShaderSettings *ReosGriddedRainfall::colorSetting() const
{
  return mRendererFactory->colorRampShaderSettings();
}

QList<ReosColorShaderSettings *> ReosGriddedRainfall::colorShaderSettings() const
{
  QList<ReosColorShaderSettings *> ret;
  ret << mRendererFactory->colorRampShaderSettings();
  return ret;
}

ReosGriddedRainItem::ReosGriddedRainItem( const QString &name, const QString &description, ReosGriddedRainfall *data )
  : ReosRainfallDataItem( name, description )
  , mGriddedRainfall( data )
{
  if ( data )
    data->setParent( this );
}

ReosGriddedRainItem::ReosGriddedRainItem( const ReosEncodedElement &element, const ReosEncodeContext &context )
  : ReosRainfallDataItem( element )
{
  if ( element.description() != QStringLiteral( "gridded-rainfall-item" ) )
    return;

  mGriddedRainfall = ReosGriddedRainfall::decode( element.getEncodedData( QStringLiteral( "gridded-rainfall" ) ), context );
}

ReosGriddedRainfall *ReosGriddedRainItem::data() const
{
  return mGriddedRainfall;
}

QIcon ReosGriddedRainItem::icone() const {return QIcon( QStringLiteral( ":/images/griddedRainfall.svg" ) );}

bool ReosGriddedRainItem::accept( ReosRainfallItem *, bool ) const {return false;}

bool ReosGriddedRainItem::canBeSubItem( const ReosRainfallItem *item, bool ) const
{
  return item && item->type() == ReosRainfallItem::Zone;
}

ReosEncodedElement ReosGriddedRainItem::encode( const ReosEncodeContext &context ) const
{
  ReosEncodedElement element( QStringLiteral( "gridded-rainfall-item" ) );
  ReosRainfallDataItem::encodeBase( element, context );

  element.addEncodedData( QStringLiteral( "gridded-rainfall" ), mGriddedRainfall->encode( context ) );

  return element;
}
