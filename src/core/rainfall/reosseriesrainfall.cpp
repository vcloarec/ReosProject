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

#include "reosgriddedrainitem.h"
#include "reosmemoryraster.h"
#include "reoswatershed.h"
#include "reosgisengine.h"
#include "reosgriddedrainfallprovider.h"

ReosSeriesRainfall::ReosSeriesRainfall( QObject *parent, const QString &providerKey, const QString &dataSource ):
  ReosTimeSerieConstantInterval( parent, providerKey, dataSource )
{
  setupData();
}

QString ReosSeriesRainfall::staticType() {return ReosTimeSerieConstantInterval::staticType() + ':' + QStringLiteral( "hyetograph" );}

ReosEncodedElement ReosSeriesRainfall::encode() const
{
  return ReosTimeSerieConstantInterval::encode( QStringLiteral( "serie-rainfall-data" ) );
}

ReosSeriesRainfall *ReosSeriesRainfall::decode( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "serie-rainfall-data" ) )
    return nullptr;

  return new ReosSeriesRainfall( element, parent );
}

ReosSeriesRainfall::ReosSeriesRainfall( const ReosEncodedElement &element, QObject *parent ):
  ReosTimeSerieConstantInterval( element, parent )
{
  setupData();
}

void ReosSeriesRainfall::setupData()
{
  setValueUnit( tr( "mm" ) );
  setValueModeName( ReosTimeSerieConstantInterval::Value, tr( "Height per time step" ) );
  setValueModeName( ReosTimeSerieConstantInterval::Cumulative, tr( "Total height" ) );
  setValueModeName( ReosTimeSerieConstantInterval::Intensity, tr( "Rainfall intensity" ) );
  setValueModeColor( ReosTimeSerieConstantInterval::Value, QColor( 0, 0, 200, 200 ) );
  setValueModeColor( ReosTimeSerieConstantInterval::Intensity, QColor( 50, 100, 255, 200 ) );
  setValueModeColor( ReosTimeSerieConstantInterval::Cumulative, QColor( 255, 50, 0 ) );
}

ReosSeriesRainfallFromGriddedOnWatershed::ReosSeriesRainfallFromGriddedOnWatershed(
  ReosWatershed *watershed,
  ReosGriddedRainfall *griddedRainfall,
  QObject *parent )
  : ReosSeriesRainfall( parent )
  , mWatershed( watershed )
  , mGriddedRainfall( griddedRainfall )
{
  registerUpstreamData( watershed );
  registerUpstreamData( griddedRainfall );
  launchCalculation();
}

void ReosSeriesRainfallFromGriddedOnWatershed::updateData() const
{
  if ( isObsolete() )
  {
    const_cast<ReosSeriesRainfallFromGriddedOnWatershed *>( this )->launchCalculation();
  }
}

void ReosSeriesRainfallFromGriddedOnWatershed::launchCalculation()
{
  AverageCalculation *newCalc = getCalculationProcess();

  connect( newCalc, &ReosProcess::finished, this, [newCalc, this]
  {
    if ( mCurrentCalculation == newCalc )
    {
      if ( newCalc->isSuccessful() )
      {
        constantTimeStepDataProvider()->copy( newCalc->result.constantTimeStepDataProvider() );
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

  constantTimeStepDataProvider()->clear();

  if ( mWatershed.isNull()
       || mGriddedRainfall.isNull()
       || mGriddedRainfall->gridCount() == 0 )
  {
    setActualized();
    emit dataChanged();
    return nullptr;
  }

  ReosArea watershedArea = mWatershed->area()->value();
  ReosRasterExtent rainExtent = mGriddedRainfall->extent();
  QRectF cellRect( rainExtent.xMapOrigin(),
                   rainExtent.yMapOrigin(),
                   std::fabs( rainExtent.xCellSize() ),
                   std::fabs( rainExtent.yCellSize() ) );
  ReosArea cellArea = ReosGisEngine::polygonAreaWithCrs( cellRect, rainExtent.crs() );

  AverageCalculation *newCalc = new AverageCalculation;

  newCalc->griddedRainfallProvider.reset( mGriddedRainfall->dataProvider()->clone() );
  newCalc->timeStep = mGriddedRainfall->minimumTimeStep();
  newCalc->usePrecision = watershedArea < cellArea * 30;
  newCalc->watershedPolygon = ReosGisEngine::transformToCoordinates( mWatershed->crs(), mWatershed->delineating(), rainExtent.crs() );

  return newCalc;
}

void ReosSeriesRainfallFromGriddedOnWatershed::AverageCalculation::start()
{
  mIsSuccessful = false;
  QElapsedTimer timer;
  timer.start();

  result.clear();

  ReosRasterExtent rainExtent = griddedRainfallProvider->extent();

  ReosRasterExtent rasterizedExtent;
  int xOri = -1;
  int yOri = -1;
  ReosRasterMemory<double> rasterizedWatershed = ReosGeometryUtils::rasterizePolygon(
        watershedPolygon, rainExtent, rasterizedExtent, xOri, yOri, usePrecision, this );

  int rasterizedXCount = rasterizedExtent.xCellCount();
  int rasterizedYCount = rasterizedExtent.yCellCount();

  int gridCount = griddedRainfallProvider->count();

  QDateTime referenceTime = griddedRainfallProvider->startTime( 0 );
  QDateTime endTime = griddedRainfallProvider->endTime( gridCount - 1 );
  QDateTime currentTime = referenceTime;

  result.setReferenceTime( referenceTime );
  result.setTimeStep( timeStep );

  int currentGriddedIndex = 0;
  int prevGriddedIndex = -1;
  double averageIntensity = 0;

  while ( currentTime < endTime && currentGriddedIndex < gridCount )
  {
    if ( prevGriddedIndex != currentGriddedIndex )
    {
      ReosRasterMemory<double> rainValues( rainExtent.yCellCount(), rainExtent.xCellCount() );
      rainValues.setValues( griddedRainfallProvider->data( currentGriddedIndex ) );
      averageIntensity = 0;
      double totalSurf = 0;
      for ( int xi = 0; xi < rasterizedXCount; ++xi )
      {
        for ( int yi = 0; yi < rasterizedYCount; ++yi )
        {
          if ( isStop() )
          {
            qDebug() << "Average gridded rainfall on watershed stop after: " << timer.elapsed();
            return;
          }

          double surf = rasterizedWatershed.value( yi, xi );
          averageIntensity += surf * rainValues.value( yi + yOri, xi + xOri );
          totalSurf += rasterizedWatershed.value( yi, xi );
        }
      }
      averageIntensity = averageIntensity / totalSurf;
    }

    ReosDuration interval( griddedRainfallProvider->startTime( currentGriddedIndex ), griddedRainfallProvider->endTime( currentGriddedIndex ) );
    result.appendValue( averageIntensity * interval.valueHour() );
    currentTime = currentTime.addMSecs( timeStep.valueMilliSecond() );
    prevGriddedIndex = currentGriddedIndex;
    currentGriddedIndex = griddedRainfallProvider->dataIndex( currentTime );
  }

  mIsSuccessful = true;

  qDebug() << QString( "average gridded precipitation %1 on watershed:" ).arg( usePrecision ? "with precision" : "without precition" ) << timer.elapsed();
}
