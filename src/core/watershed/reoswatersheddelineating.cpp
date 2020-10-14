/***************************************************************************
                      reoswatersheddelineating.cpp
                     --------------------------------------
Date                 : 04-10-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reoswatersheddelineating.h"
#include "reosdigitalelevationmodel.h"
#include "reosrasterfilling.h"
#include "reosrasterwatershed.h"

ReosWatershedDelineating::ReosWatershedDelineating( ReosModule *parent, ReosGisEngine *gisEngine ):
  ReosModule( parent ),
  mGisEngine( gisEngine )
{}

bool ReosWatershedDelineating::hasValidDigitalElevationModel() const
{
  if ( !mGisEngine )
    return false;

  return mGisEngine->isDigitalElevationModel( mDEMLayerId ) && mGisEngine->layerType( mDEMLayerId ) == ReosGisEngine::RasterLayer;

}

bool ReosWatershedDelineating::setDigitalElevationModelDEM( const QString &layerId )
{
  if ( mGisEngine->layerType( layerId ) == ReosGisEngine::RasterLayer && mGisEngine->isDigitalElevationModel( layerId ) )
  {
    QString oldLayer = layerId;
    mDEMLayerId = layerId;
    mCurrentState = WaitingForDownstream;
    return true;
  }

  return false;
}


ReosWatershedDelineating::State ReosWatershedDelineating::currentState() const
{
  return mCurrentState;
}

bool ReosWatershedDelineating::setDownstreamLine( const QPolygonF &downstreamLine )
{
  if ( mCurrentState == WaitingForDownstream && downstreamLine.count() > 1 )
  {
    mDownstreamLine = downstreamLine;
    mCurrentState = WaitingForExtent;
    return true;
  }

  return false;
}

bool ReosWatershedDelineating::setPreDefinedExtent( const ReosMapExtent &extent )
{
  if ( extent.containsPartialy( mDownstreamLine ) )
  {
    mExtent = extent;
    mCurrentState = WaitingforProceed;
    return true;
  }

  return false;
}

ReosProcess *ReosWatershedDelineating::delineatingProcess()
{
  if ( mCurrentState != WaitingforProceed )
  {
    return nullptr;
  }

  std::unique_ptr<ReosDigitalElevationModel> dem;
  dem.reset( mGisEngine->getTopDigitalElevationModel() );
  return new ReosWatershedDelineatingProcess( dem.release(), mExtent, mDownstreamLine );
}


ReosWatershedDelineatingProcess::ReosWatershedDelineatingProcess( ReosDigitalElevationModel *dem, const ReosMapExtent &mapExtent, const QPolygonF &downtreamLine ):
  mExtent( mapExtent ),
  mEntryDem( dem ),
  mDownstreamLine( downtreamLine )
{

}

void ReosWatershedDelineatingProcess::start()
{
  mIsSuccessful = false;
  if ( !mEntryDem )
    return;

  ReosRasterExtent rasterExtent;
  ReosRasterMemory<float> dem( mEntryDem->extractMemoryRasterSimplePrecision( mExtent, rasterExtent ) );
  std::unique_ptr<ReosRasterFillingWangLiu> fillDemProcess( new ReosRasterFillingWangLiu( dem, fabs( rasterExtent.xCellSize() ), fabs( rasterExtent.yCellSize() ) ) );

  fillDemProcess->start();

  if ( !fillDemProcess->isSuccessful() )
    return;

  ReosRasterWatershed::Directions direction = fillDemProcess->directionRaster();
  fillDemProcess.release();

  //rasterize the dowstreamline
  ReosRasterLine rasterDownstreamLine;
  for ( const QPointF &point : mDownstreamLine )
  {
    ReosRasterCellPos cell = rasterExtent.mapToCellPos( point );
    rasterDownstreamLine.addPoint( cell.row(), cell.column() );
  }

  std::unique_ptr<ReosRasterWatershedFromDirectionAndDownStreamLine> rasterWatershedFromDirection(
    new ReosRasterWatershedFromDirectionAndDownStreamLine( direction, rasterDownstreamLine ) );

  rasterWatershedFromDirection->start();

  if ( !rasterWatershedFromDirection->isSuccessful() )
    return;

  ReosRasterWatershed::Watershed watershed = rasterWatershedFromDirection->watershed();
  ReosRasterCellPos downStreamPoint = rasterWatershedFromDirection->firstCell();
  ReosRasterCellPos endOfLongerPath = rasterWatershedFromDirection->endOfLongerPath();
  rasterWatershedFromDirection.release();

  std::unique_ptr<ReosRasterWatershedToVector> watershedToPolygon( new ReosRasterWatershedToVector( watershed, rasterExtent, downStreamPoint ) );

  watershedToPolygon->start();

  if ( !watershedToPolygon->isSuccessful() )
    return;

  QPolygonF polygonWatershed = watershedToPolygon->watershed();

  mIsSuccessful = true;
}
