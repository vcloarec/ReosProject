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
  makeConnection();
  mRendererFactory.reset( new ReosGriddedRainfallRendererFactory_p( this ) );
}

ReosGriddedRainfall::ReosGriddedRainfall( const QString &dataSource, const QString &providerKey, QObject *parent )
  : ReosRenderedObject( parent )
  , mProvider( qobject_cast<ReosGriddedRainfallProvider*>( ReosDataProviderRegistery::instance()->createProvider( formatKey( providerKey ) ) ) )
{
  if ( mProvider )
  {
    makeConnection();
    mProvider->setDataSource( dataSource );
  }
  // renderer factory must be created after set the datasource because, the factory needs the extent of the provider on creation
  mRendererFactory.reset( new ReosGriddedRainfallRendererFactory_p( this ) );
}

ReosGriddedRainfall *ReosGriddedRainfall::decode( const ReosEncodedElement &element,  const ReosEncodeContext &context, QObject *parent )
{
  if ( element.description() != ReosGriddedRainfall::staticType() )
    return nullptr;

  return new ReosGriddedRainfall( element, context, parent );
}

void ReosGriddedRainfall::updateData() const
{
  if ( dataProvider() )
    dataProvider()->load();
}

ReosGriddedRainfall::ReosGriddedRainfall( const ReosEncodedElement &element, const ReosEncodeContext &context, QObject *parent )
  : ReosRenderedObject( parent )
{
  ReosDataObject::decode( element );
  element.getData( QStringLiteral( "overriden-crs" ), mOverridenCrs );

  QString providerKey;
  element.getData( QStringLiteral( "provider-key" ), providerKey );
  mProvider.reset( qobject_cast<ReosGriddedRainfallProvider *>( ReosDataProviderRegistery::instance()->createProvider( formatKey( providerKey ) ) ) );
  if ( mProvider )
    mProvider->decode( element.getEncodedData( QStringLiteral( "provider" ) ), context );

  makeConnection();

  // renderer factory must be created after set the datasource because, the factory needs the extent of the provider on creation
  mRendererFactory.reset( new ReosGriddedRainfallRendererFactory_p( element.getEncodedData( QStringLiteral( "renderer" ) ), this ) );
}

QString ReosGriddedRainfall::formatKey( const QString &rawKey ) const
{
  if ( rawKey.contains( QStringLiteral( "::" ) ) )
    return rawKey;

  return rawKey + QStringLiteral( "::" ) + ReosGriddedRainfall::staticType();
}

void ReosGriddedRainfall::makeConnection()
{
  if ( mProvider )
  {
    connect( mProvider.get(), &ReosDataProvider::dataChanged, this, &ReosDataObject::dataChanged );
    connect( mProvider.get(), &ReosDataProvider::dataReset, this, &ReosDataObject::dataReset );
    connect( mProvider.get(), &ReosDataProvider::loadingFinished, this, &ReosGriddedRainfall::loadingFinished );
  }

  connect( this, &ReosDataObject::dataChanged, this, &ReosRenderedObject::repaintRequested );
}

ReosEncodedElement ReosGriddedRainfall::encode( const ReosEncodeContext &context ) const
{
  ReosEncodedElement element( ReosGriddedRainfall::staticType() );
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

ReosMapExtent ReosGriddedRainfall::extent() const
{
  return rasterExtent();
}

ReosGriddedRainfall *ReosGriddedRainfall::loadGriddedRainfall( const QString &dataSource, const QString &providerKey, QObject *parent )
{
  std::unique_ptr<ReosGriddedRainfall> ret = std::make_unique<ReosGriddedRainfall>( dataSource, providerKey, parent );
  if ( ret->dataProvider()->isLoading() )
  {
    QEventLoop loop;
    connect( ret->dataProvider(), &ReosDataProvider::loadingFinished, &loop, &QEventLoop::quit );
    loop.exec();
  }

  return ret.release();
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

const QVector<double> ReosGriddedRainfall::intensityValuesInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax ) const
{
  if ( !mProvider )
    return QVector<double>();

  if ( index < 0 )
    return QVector<double>();

  if ( mProvider->hasPrecipitationCapability( ReosGriddedRainfallProvider::SubGridExtract ) )
    return mProvider->dataInGridExtent( index, rowMin, rowMax, colMin, colMax );

  QVector<double> data = mProvider->data( index );

  ReosRasterExtent extent = mProvider->extent();
  int colRawCount = extent.xCellCount();

  int rowCount = rowMax - rowMin + 1;
  int colCount = colMax - colMin + 1;
  QVector<double> ret;
  ret.resize( rowCount * colCount );
  for ( int r = 0; r < rowCount; ++r )
    for ( int c = 0; c < colCount; ++c )
    {
      int rawIndex = c + colMin + ( r + rowMin ) * colRawCount;
      int locIndex = c + r * colCount;
      ret[locIndex] = data.at( rawIndex );
    }

  return ret;
}

const QVector<double> ReosGriddedRainfall::qualificationData( int index ) const
{
  if ( mProvider && mProvider->hasPrecipitationCapability( ReosGriddedRainfallProvider::SubGridExtract ) )
    return mProvider->qualifData( index );

  return QVector<double>();
}

double ReosGriddedRainfall::nullCoverage( int index ) const
{
  const QVector<double> data = intensityValues( index );
  int nullCount = 0;
  for ( double val : data )
    if ( std::isnan( val ) )
      nullCount++;

  return static_cast<double>( nullCount ) / data.size();
}

double ReosGriddedRainfall::qualifCoverage( int index, double qualif ) const
{
  if ( mProvider && mProvider->hasPrecipitationCapability( ReosGriddedRainfallProvider::QualificationValue ) )
  {
    const QVector<double> qualifData = mProvider->qualifData( index );
    int supCount = 0;
    for ( double val : qualifData )
      if ( val >= qualif )
        supCount++;

    return static_cast<double>( supCount ) / qualifData.size();
  }

  return 1.0;
}

bool ReosGriddedRainfall::supportExtractSubGrid() const
{
  if ( mProvider )
    return mProvider->hasPrecipitationCapability( ReosGriddedRainfallProvider::SubGridExtract );

  return false;
}

ReosRasterMemory<double> ReosGriddedRainfall::intensityRaster( int index ) const
{
  ReosRasterExtent extent = mProvider->extent();
  ReosRasterMemory<double> ret( extent.yCellCount(), extent.xCellCount() );
  ret.setValues( mProvider->data( index ) );

  return ret;
}

ReosFloat64GridBlock ReosGriddedRainfall::intensityGridBlock( int index ) const
{
  ReosRasterExtent extent = mProvider->extent();
  ReosFloat64GridBlock ret( extent.yCellCount(), extent.xCellCount() );
  ret.setValues( mProvider->data( index ) );

  return ret;
}

ReosFloat64GridBlock ReosGriddedRainfall::qualificationGridBloc( int index ) const
{
  ReosRasterExtent extent = mProvider->extent();
  ReosFloat64GridBlock ret( extent.yCellCount(), extent.xCellCount() );
  ret.setValues( qualificationData( index ) );

  return ret;
}

int ReosGriddedRainfall::dataIndex( const QDateTime &time ) const
{
  if ( !mProvider )
    return  -1;

  return mProvider->dataIndex( time );
}

ReosRasterExtent ReosGriddedRainfall::rasterExtent() const
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

ReosGriddedRainfall *ReosGriddedRainfall::transform( const ReosMapExtent &destination, double resolX, double resolY, const ReosTimeWindow &timeWindow, QObject *parent ) const
{
  std::unique_ptr<ReosGriddedRainfall> projectedRainfall( new ReosGriddedRainfall( parent ) );

  ReosRasterExtent destinationExtent;
  bool success;
  const ReosRasterExtent sourceExtent = rasterExtent();
  auto convertValues = ReosGisEngine::transformRasterExtent( sourceExtent, destination, resolX, resolY, destinationExtent, success );
  int sourceXCount = sourceExtent.xCellCount();

  std::unique_ptr<ReosGriddedRainfallMemoryProvider> newProvider( new ReosGriddedRainfallMemoryProvider );

  newProvider->setExtent( destinationExtent );

  int xCount = destinationExtent.xCellCount();
  int yCount = destinationExtent.yCellCount();

  bool filterTime = timeWindow.isValid();

  for ( int i = 0; i < mProvider->count(); ++i )
  {
    if ( filterTime )
    {
      const QDateTime &start = mProvider->startTime( i );
      const QDateTime &end = mProvider->endTime( i );
      const ReosTimeWindow step_tw( start, end );

      if ( !timeWindow.intersect( step_tw ) )
        continue;
    }

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
  copyFrom( other->mProvider.get() );
}

void ReosGriddedRainfall::copyFrom( ReosGriddedRainfallProvider *provider )
{
  mProvider->copyFrom( provider );

  //! We need to reset the renderer factor tp take account of the new extent
  mRendererFactory.reset( new ReosGriddedRainfallRendererFactory_p( this ) );
}

ReosColorShaderSettings *ReosGriddedRainfall::colorSetting() const
{
  return mRendererFactory->colorRampShaderSettings();
}

void ReosGriddedRainfall::setColorSetting( ReosColorShaderSettings *colorRampShader )
{
  mRendererFactory->setColorRampShaderSettings( colorRampShader );
  emit repaintRequested();
}

QList<ReosColorShaderSettings *> ReosGriddedRainfall::colorShaderSettings() const
{
  QList<ReosColorShaderSettings *> ret;
  ret << mRendererFactory->colorRampShaderSettings();
  return ret;
}

bool ReosGriddedRainfall::getDirectMinMaxValue( double &min, double &max ) const
{
  if ( mProvider )
    return mProvider->getDirectMinMax( min, max );
  return false;
}

void ReosGriddedRainfall::calculateMinMaxValue( double &min, double &max ) const
{
  if ( mProvider )
    mProvider->calculateMinMax( min, max );
}

ReosGriddedRainItem::ReosGriddedRainItem( const QString &name, const QString &description, ReosGriddedRainfall *data )
  : ReosRainfallDataItem( name, description )
  , mGriddedRainfall( data )
{
  if ( data )
  {
    data->setParent( this );
    connect( this, &ReosRainfallItem::changed, mGriddedRainfall, [this]
    {
      mGriddedRainfall->setName( ReosRainfallItem::name() );
      emit mGriddedRainfall->repaintRequested();
    } );
  }

}

ReosGriddedRainItem::ReosGriddedRainItem( const ReosEncodedElement &element, const ReosEncodeContext &context )
  : ReosRainfallDataItem( element )
{
  if ( element.description() != QStringLiteral( "gridded-rainfall-item" ) )
    return;

  mGriddedRainfall = ReosGriddedRainfall::decode( element.getEncodedData( QStringLiteral( "gridded-rainfall" ) ), context, this );

  if ( !mGriddedRainfall )
    mGriddedRainfall = new ReosGriddedRainfall( this );

  if ( mGriddedRainfall )
    connect( this, &ReosRainfallItem::changed, mGriddedRainfall, [this]
  {
    mGriddedRainfall->setName( ReosRainfallItem::name() );
    emit mGriddedRainfall->repaintRequested();
  } );
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

  if ( mGriddedRainfall )
    element.addEncodedData( QStringLiteral( "gridded-rainfall" ), mGriddedRainfall->encode( context ) );

  return element;
}

ReosGriddedRainfall *ReosGriddedRainfallRendererFactory::rainfall() const
{
  return mRainfall;
}
