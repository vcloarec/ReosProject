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
  setObsolete();
}

void ReosSeriesRainfallFromGriddedOnWatershed::updateData() const
{
  if ( isObsolete() )
    updateAverageRainfallCalculation();
}

void ReosSeriesRainfallFromGriddedOnWatershed::updateAverageRainfallCalculation() const
{
  QElapsedTimer timer;
  timer.start();
  ReosTimeSerieConstantTimeStepProvider *data = constantTimeStepDataProvider();
  if ( !data )
    return;

  data->clear();

  if ( mWatershed.isNull()
       || mGriddedRainfall.isNull()
       || mGriddedRainfall->gridCount() == 0 )
  {
    setActualized();
    emit dataChanged();
    return;
  }

  ReosRasterExtent rainExtent = mGriddedRainfall->extent();
  ReosArea watershedArea = mWatershed->area()->value();
  QRectF cellRect( rainExtent.xMapOrigin(),
                   rainExtent.yMapOrigin(),
                   std::fabs( rainExtent.xCellSize() ),
                   std::fabs( rainExtent.yCellSize() ) );

  ReosArea cellArea = ReosGisEngine::polygonAreaWithCrs( cellRect, rainExtent.crs() );

  bool usePrecision = watershedArea < cellArea * 100;

  QPolygonF watershedPolygon = ReosGisEngine::transformToCoordinates( mWatershed->crs(), mWatershed->delineating(), rainExtent.crs() );

  ReosRasterExtent rasterizedExtent;
  int xOri = -1;
  int yOri = -1;
  ReosRasterMemory<double> rasterizedWatershed = ReosGeometryUtils::rasterizePolygon(
        watershedPolygon, rainExtent, rasterizedExtent, xOri, yOri, usePrecision );

  int rasterizedXCount = rasterizedExtent.xCellCount();
  int rasterizedYCount = rasterizedExtent.yCellCount();

  int gridCount = mGriddedRainfall->gridCount();
  ReosDuration timeStep = mGriddedRainfall->minimumTimeStep();
  QDateTime referenceTime = mGriddedRainfall->startTime( 0 );
  QDateTime endTime = mGriddedRainfall->endTime( gridCount - 1 );
  QDateTime currentTime = referenceTime;

  data->setReferenceTime( referenceTime );
  data->setTimeStep( timeStep );

  int currentGriddedIndex = 0;
  int prevGriddedIndex = -1;
  double averageIntensity = 0;
  while ( currentTime < endTime && currentGriddedIndex < gridCount )
  {
    if ( prevGriddedIndex != currentGriddedIndex )
    {
      const ReosRasterMemory<double> rainValues = mGriddedRainfall->intensityRaster( currentGriddedIndex );
      averageIntensity = 0;
      double totalSurf = 0;
      for ( int xi = 0; xi < rasterizedXCount; ++xi )
      {
        for ( int yi = 0; yi < rasterizedYCount; ++yi )
        {
          double surf = rasterizedWatershed.value( yi, xi );
          averageIntensity += surf * rainValues.value( yi + yOri, xi + xOri );
          totalSurf += rasterizedWatershed.value( yi, xi );
        }
      }
      averageIntensity = averageIntensity / totalSurf;
    }

    ReosDuration interval( mGriddedRainfall->startTime( currentGriddedIndex ), mGriddedRainfall->endTime( currentGriddedIndex ) );
    data->appendValue( averageIntensity * interval.valueHour() );
    currentTime = currentTime.addMSecs( timeStep.valueMilliSecond() );
    prevGriddedIndex = currentGriddedIndex;
    currentGriddedIndex = mGriddedRainfall->dataIndex( currentTime );
  }

  qDebug() << QString( "average gridded precipitation %1 on watershed %2 :" ).arg( usePrecision ? "with precision" : "without precition", mWatershed->name() ) << timer.elapsed();
  setActualized();
  emit dataChanged();
}
