/***************************************************************************
  reosseriesrainfall.cpp - ReosSeriesRainfall

 ---------------------
 begin                : 22.11.2022
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
#include "reosseriesrainfall.h"

#include <QEventLoop>

#include "reosgriddedrainitem.h"
#include "reosmemoryraster.h"
#include "reoswatershed.h"
#include "reosgisengine.h"
#include "reosgriddedrainfallprovider.h"
#include "reosgriddeddata.h"


ReosSeriesRainfall::ReosSeriesRainfall( QObject *parent, const QString &providerKey, const QString &dataSource ):
  ReosTimeSeriesConstantInterval( parent, providerKey, dataSource )
{
  setupData();
}

QString ReosSeriesRainfall::staticType() {return ReosTimeSeriesConstantInterval::staticType() + ':' + QStringLiteral( "hyetograph" );}

ReosEncodedElement ReosSeriesRainfall::encode( const ReosEncodeContext &context ) const
{
  return ReosTimeSeriesConstantInterval::encode( context, QStringLiteral( "serie-rainfall-data" ) );
}

ReosSeriesRainfall *ReosSeriesRainfall::decode( const ReosEncodedElement &element, const ReosEncodeContext &context, QObject *parent )
{
  if ( element.description() != QStringLiteral( "serie-rainfall-data" ) )
    return nullptr;

  return new ReosSeriesRainfall( element, context, parent );
}

ReosSeriesRainfall::ReosSeriesRainfall( const ReosEncodedElement &element, const ReosEncodeContext &context, QObject *parent ):
  ReosTimeSeriesConstantInterval( element, context, parent )
{
  setupData();
}

void ReosSeriesRainfall::setupData()
{
  setValueUnit( tr( "mm" ) );
  setValueModeName( ReosTimeSeriesConstantInterval::Value, tr( "Height per time step" ) );
  setValueModeName( ReosTimeSeriesConstantInterval::Cumulative, tr( "Total height" ) );
  setValueModeName( ReosTimeSeriesConstantInterval::Intensity, tr( "Rainfall intensity" ) );
  setValueModeColor( ReosTimeSeriesConstantInterval::Value, QColor( 0, 0, 200, 200 ) );
  setValueModeColor( ReosTimeSeriesConstantInterval::Intensity, QColor( 50, 100, 255, 200 ) );
  setValueModeColor( ReosTimeSeriesConstantInterval::Cumulative, QColor( 255, 50, 0 ) );
}

ReosSeriesRainfallFromGriddedOnWatershed::ReosSeriesRainfallFromGriddedOnWatershed(
  ReosWatershed *watershed,
  ReosGriddedRainfall *griddedRainfall,
  QObject *parent )
  : ReosSeriesRainfall( parent )
  , ReosDataGriddedOnWatershed( watershed, griddedRainfall )
{
  connect( watershed, &ReosWatershed::geometryChanged, this, &ReosSeriesRainfallFromGriddedOnWatershed::onWatershedGeometryChanged );
  registerUpstreamData( griddedRainfall );

  setReferenceTime( griddedRainfall->startTime( 0 ) );
  setTimeStep( griddedRainfall->minimumTimeStep() );
  QDateTime endTime = griddedRainfall->endTime( griddedRainfall->gridCount() - 1 );
  ReosDuration rainDuration( qint64( referenceTime().msecsTo( endTime ) ) );
  int valueCount = static_cast<int>( rainDuration.numberOfFullyContainedIntervals( timeStep() ) );
  QVector<double> values( valueCount, std::numeric_limits<double>::quiet_NaN() );
  setValues( values );

  launchCalculation();
}

ReosSeriesRainfallFromGriddedOnWatershed::~ReosSeriesRainfallFromGriddedOnWatershed()
{
}

ReosSeriesRainfallFromGriddedOnWatershed *ReosSeriesRainfallFromGriddedOnWatershed::create( ReosWatershed *watershed, ReosGriddedRainfall *griddedRainfall )
{
  QEventLoop loop;
  std::unique_ptr<ReosSeriesRainfallFromGriddedOnWatershed> ret = std::make_unique<ReosSeriesRainfallFromGriddedOnWatershed>( watershed, griddedRainfall );
  connect( ret.get(), &ReosSeriesRainfallFromGriddedOnWatershed::calculationFinished, &loop, &QEventLoop::quit );
  loop.exec();

  return ret.release();
}

double ReosSeriesRainfallFromGriddedOnWatershed::valueAt( int i ) const
{
  double val = ReosSeriesRainfall::valueAt( i );
  if ( !std::isnan( val ) )
    return val;

  val = calculateValueAt( i );

  constantTimeStepDataProvider()->setValue( i, val );

  return val;
}

void ReosSeriesRainfallFromGriddedOnWatershed::preCalculate() const
{
  int count = mGriddedData->gridCount();
  for ( int i = 0; i < count; ++i )
    valueAt( i );
}

void ReosSeriesRainfallFromGriddedOnWatershed::updateData() const
{
  if ( isObsolete() )
  {
    const_cast<ReosSeriesRainfallFromGriddedOnWatershed *>( this )->launchCalculation();
  }
}

void ReosSeriesRainfallFromGriddedOnWatershed::onCalculationFinished()
{
  emit calculationFinished();
}

void ReosSeriesRainfallFromGriddedOnWatershed::onDataChanged() const
{
  emit dataChanged();
}

QDateTime ReosSeriesRainfallFromGriddedOnWatershed::timeAtIndex( int i ) const
{
  return ReosSeriesRainfall::timeAt( i );
}

void ReosSeriesRainfallFromGriddedOnWatershed::setDataActualized() const
{
  setActualized();
}


void ReosSeriesRainfallFromGriddedOnWatershed::onWatershedGeometryChanged()
{
  for ( int i = 0; i < valueCount(); ++i )
    setValueAt( i, std::numeric_limits<double>::quiet_NaN() );
  setObsolete();
}
