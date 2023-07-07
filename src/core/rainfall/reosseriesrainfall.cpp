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
  , mWatershed( watershed )
  , mGriddedRainfall( griddedRainfall )
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

  ReosRasterExtent rainExtent = mGriddedRainfall->rasterExtent();

  int rasterizedXCount = mRasterizedExtent.xCellCount();
  int rasterizedYCount = mRasterizedExtent.yCellCount();

  ReosRasterMemory<double> rainValues;
  int griddedIndex = mGriddedRainfall->dataIndex( timeAt( i ) );

  int effXori;
  int effYOri;

  if ( mGriddedRainfall->supportExtractSubGrid() )
  {
    effXori = 0;
    effYOri = 0;
    rainValues = ReosRasterMemory<double>( rasterizedYCount, rasterizedXCount );
    rainValues.setValues( mGriddedRainfall->intensityValuesInGridExtent(
                            griddedIndex, mYOri, mYOri + rasterizedYCount - 1, mXOri, mXOri + rasterizedXCount - 1 ) );
  }
  else
  {
    effXori = mXOri;
    effYOri = mYOri;
    rainValues = ReosRasterMemory<double>( rainExtent.yCellCount(), rainExtent.xCellCount() );
    rainValues.setValues( mGriddedRainfall->intensityValues( griddedIndex ) );
  }

  double averageIntensity = 0;
  double totalSurf = 0;
  for ( int xi = 0; xi < rasterizedXCount; ++xi )
  {
    for ( int yi = 0; yi < rasterizedYCount; ++yi )
    {
      double surf = mRasterizedWatershed.value( yi, xi );
      double rv = rainValues.value( yi + effYOri, xi + effXori );
      if ( !std::isnan( rv ) )
        averageIntensity += surf * rv;
      totalSurf += mRasterizedWatershed.value( yi, xi );
    }
  }
  averageIntensity = averageIntensity / totalSurf;

  constantTimeStepDataProvider()->setValue( i, averageIntensity * timeStep().valueHour() );

  return averageIntensity;

}

void ReosSeriesRainfallFromGriddedOnWatershed::updateData() const
{
  if ( isObsolete() )
  {
    const_cast<ReosSeriesRainfallFromGriddedOnWatershed *>( this )->launchCalculation();
  }
}


void ReosSeriesRainfallFromGriddedOnWatershed::onWatershedGeometryChanged()
{
  for ( int i = 0; i < valueCount(); ++i )
    setValueAt( i, std::numeric_limits<double>::quiet_NaN() );
  setObsolete();
}

void ReosSeriesRainfallFromGriddedOnWatershed::launchCalculation()
{
  AverageCalculation *newCalc = getCalculationProcess();

  connect( newCalc, &ReosProcess::finished, newCalc, [newCalc, this]
  {
    if ( mCurrentCalculation == newCalc )
    {
      if ( newCalc->isSuccessful() )
      {
        mRasterizedExtent = mCurrentCalculation->rasterizedExtent;
        mRasterizedWatershed = mCurrentCalculation->rasterizedWatershed;
        mXOri = mCurrentCalculation->xOri;
        mYOri = mCurrentCalculation->yOri;
        mCurrentCalculation = nullptr;
        emit calculationFinished();
      }
    }
    newCalc->deleteLater();
  } );
  setActualized();
  mCurrentCalculation = newCalc;

  newCalc->startOnOtherThread();
}

ReosSeriesRainfallFromGriddedOnWatershed::AverageCalculation *ReosSeriesRainfallFromGriddedOnWatershed::getCalculationProcess() const
{
  if ( mCurrentCalculation )
    mCurrentCalculation->stop( true );

  if ( mWatershed.isNull()
       || mGriddedRainfall.isNull()
       || mGriddedRainfall->gridCount() == 0 )
  {
    setActualized();
    emit dataChanged();
    return nullptr;
  }

  ReosArea watershedArea = mWatershed->areaParameter()->value();
  ReosRasterExtent rainExtent = mGriddedRainfall->rasterExtent();
  QRectF cellRect( rainExtent.xMapOrigin(),
                   rainExtent.yMapOrigin(),
                   std::fabs( rainExtent.xCellSize() ),
                   std::fabs( rainExtent.yCellSize() ) );
  ReosArea cellArea = ReosGisEngine::polygonAreaWithCrs( cellRect, rainExtent.crs() );

  std::unique_ptr<AverageCalculation> newCalc( new AverageCalculation );

  newCalc->griddedRainfallProvider.reset( mGriddedRainfall->dataProvider()->clone() );
  newCalc->timeStep = mGriddedRainfall->minimumTimeStep();
  newCalc->usePrecision = watershedArea < cellArea * 30;
  newCalc->watershedPolygon = ReosGisEngine::transformToCoordinates( mWatershed->crs(), mWatershed->delineating(), rainExtent.crs() );

  return newCalc.release();
}

void ReosSeriesRainfallFromGriddedOnWatershed::AverageCalculation::start()
{
  mIsSuccessful = false;
  QElapsedTimer timer;
  timer.start();

  ReosRasterExtent rainExtent = griddedRainfallProvider->extent();

  rasterizedWatershed = ReosGeometryUtils::rasterizePolygon(
                          watershedPolygon, rainExtent, rasterizedExtent, xOri, yOri, usePrecision, this );

  mIsSuccessful = true;

  qDebug() << QString( "average gridded precipitation %1 on watershed:" ).arg( usePrecision ? "with precision" : "without precition" ) << timer.elapsed();
}
