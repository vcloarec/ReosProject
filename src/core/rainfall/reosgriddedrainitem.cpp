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

#include <QElapsedTimer>

ReosGriddedRainfall *ReosGriddedRainfall::decode( const ReosEncodedElement &element,  const ReosEncodeContext &context, QObject *parent )
{
  if ( element.description() != ReosGriddedRainfall::staticType() )
    return nullptr;

  return new ReosGriddedRainfall( element, context, parent );
}

ReosGriddedRainfall::ReosGriddedRainfall( QObject *parent )
  : ReosGriddedData( parent )
{
  setProvider( new ReosGriddedRainfallMemoryProvider );
  makeConnection();
}

ReosGriddedRainfall::ReosGriddedRainfall( const QString &dataSource, const QString &providerKey, QObject *parent )
  : ReosGriddedData( dataSource, formatKey( providerKey ), parent )
{
}

ReosGriddedRainfall::ReosGriddedRainfall( const ReosEncodedElement &element, const ReosEncodeContext &context, QObject *parent )
  : ReosGriddedData( parent )
{
  ReosDataObject::decode( element );
  element.getData( QStringLiteral( "overriden-crs" ), mOverridenCrs );

  decodeProvider( element, context );
  makeConnection();

  setRenderer( new ReosGriddedRainfallRendererFactory_p( element.getEncodedData( QStringLiteral( "renderer" ) ), this ) );
}

QString ReosGriddedRainfall::formatKey( const QString &rawKey ) const
{
  if ( rawKey.contains( QStringLiteral( "::" ) ) )
    return rawKey;

  return rawKey + QStringLiteral( "::" ) + ReosGriddedRainfall::staticType();
}

ReosEncodedElement ReosGriddedRainfall::encode( const ReosEncodeContext &context ) const
{
  ReosEncodedElement element( ReosGriddedRainfall::staticType() );
  ReosDataObject::encode( element );
  element.addData( QStringLiteral( "overriden-crs" ), mOverridenCrs );
  element.addEncodedData( QStringLiteral( "renderer" ), renderer()->encode() );

  element.addData( QStringLiteral( "provider-key" ), dataProvider()->key() );
  element.addEncodedData( QStringLiteral( "provider" ), dataProvider()->encode( context ) );

  return element;
}

ReosGriddedRainfall::~ReosGriddedRainfall()
{
}

QString ReosGriddedRainfall::type() const
{
  return staticType();
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
  return qobject_cast<ReosGriddedRainfallProvider *>( ReosGriddedData::dataProvider() );
}

QString ReosGriddedRainfall::staticType() {return QStringLiteral( "gridded-precipitation" );}



const QVector<double> ReosGriddedRainfall::intensityValues( int index ) const
{
  return values( index );
}

const QVector<double> ReosGriddedRainfall::intensityValuesInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax ) const
{
  return valuesInGridExtent( index, rowMin, rowMax, colMin, colMax );
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
  if ( dataProvider() && dataProvider()->hasCapability( ReosGriddedRainfallProvider::QualificationValue ) )
  {
    const QVector<double> qualifData = dataProvider()->qualifData( index );
    int supCount = 0;
    for ( double val : qualifData )
      if ( val >= qualif )
        supCount++;

    return static_cast<double>( supCount ) / qualifData.size();
  }

  return 1.0;
}

const QVector<double> ReosGriddedRainfall::qualificationData( int index ) const
{
  if ( dataProvider() && dataProvider()->hasCapability( ReosGriddedRainfallProvider::QualificationValue ) )
    return dataProvider()->qualifData( index );

  return QVector<double>();
}


ReosRasterMemory<double> ReosGriddedRainfall::intensityRaster( int index ) const
{
  ReosRasterExtent extent = dataProvider()->extent();
  ReosRasterMemory<double> ret( extent.yCellCount(), extent.xCellCount() );
  ret.setValues( dataProvider()->data( index ) );

  return ret;
}

ReosFloat64GridBlock ReosGriddedRainfall::intensityGridBlock( int index ) const
{
  ReosRasterExtent extent = dataProvider()->extent();
  ReosFloat64GridBlock ret( extent.yCellCount(), extent.xCellCount() );
  ret.setValues( dataProvider()->data( index ) );

  return ret;
}

ReosFloat64GridBlock ReosGriddedRainfall::qualificationGridBloc( int index ) const
{
  ReosRasterExtent extent = dataProvider()->extent();
  ReosFloat64GridBlock ret( extent.yCellCount(), extent.xCellCount() );
  ret.setValues( qualificationData( index ) );

  return ret;
}

ReosGriddedData *ReosGriddedRainfallRendererFactory::griddedData() const
{
  return mGriddedData;
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

  for ( int i = 0; i < dataProvider()->count(); ++i )
  {
    if ( filterTime )
    {
      const QDateTime &start = dataProvider()->startTime( i );
      const QDateTime &end = dataProvider()->endTime( i );
      const ReosTimeWindow step_tw( start, end );

      if ( !timeWindow.intersect( step_tw ) )
        continue;
    }

    const QVector<double> sourceValue = dataProvider()->data( i );
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

    newProvider->addFrame( raster, dataProvider()->startTime( i ), dataProvider()->endTime( i ) );
  }

  projectedRainfall->setProvider( newProvider.release() );
  return projectedRainfall.release();
}

ReosColorShaderSettings *ReosGriddedRainfall::colorSetting() const
{
  return renderer()->colorRampShaderSettings();
}

void ReosGriddedRainfall::setColorSetting( ReosColorShaderSettings *colorRampShader )
{
  renderer()->setColorRampShaderSettings( colorRampShader );
  emit repaintRequested();
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
