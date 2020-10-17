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
#include "reoswatershedstore.h"

ReosWatershedDelineating::ReosWatershedDelineating( ReosModule *parent, ReosGisEngine *gisEngine ):
  ReosModule( parent ),
  mGisEngine( gisEngine )
{
  connect( this, &ReosModule::processFinished, this, &ReosWatershedDelineating::onDelineatingFinished );
}

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

bool ReosWatershedDelineating::setDownstreamLine( const QPolygonF &downstreamLine, const ReosWatershedStore &store )
{
  if ( mCurrentState == WaitingForDownstream && downstreamLine.count() > 1 )
  {
    bool ok;
    ReosWatershed *downstreamWatershed = store.downstreamWatershed( downstreamLine, ok );
    if ( !ok )
      return false;

    mDownstreamLine = downstreamLine;
    if ( downstreamWatershed )
    {
      mDirection = downstreamWatershed->directions();
      if ( mDirection.isValid() )
      {
        mDirectionExtent = downstreamWatershed->directionExtent();
        mCurrentState = WaitingforProceed;
        return true;
      }
    }

    mCurrentState = WaitingForExtent;
    return true;
  }

  return false;
}

bool ReosWatershedDelineating::setPreDefinedExtent( const ReosMapExtent &extent )
{
  if ( extent.containsPartialy( mDownstreamLine ) &&
       ( mCurrentState == WaitingForExtent || mCurrentState == WaitingWithBroughtBackExtent ) )
  {
    mExtent = extent;
    mCurrentState = WaitingforProceed;
    return true;
  }

  return false;
}

bool ReosWatershedDelineating::startDelineating()
{
  if ( mCurrentState != WaitingforProceed )
  {
    return false;
  }

  if ( mDirection.isValid() )
    mProcess = std::make_unique<ReosWatershedDelineatingProcess>( mDirection, mDirectionExtent, mDownstreamLine );
  else
  {
    std::unique_ptr<ReosDigitalElevationModel> dem;
    dem.reset( mGisEngine->getTopDigitalElevationModel() );
    mProcess = std::make_unique<ReosWatershedDelineatingProcess>( dem.release(), mExtent, mDownstreamLine, mBurningLines );
  }

  startProcessOnOtherThread( mProcess.get() );
  return true;
}

bool ReosWatershedDelineating::isDelineatingFinished() const
{
  return ( mProcess && mProcess->isSuccessful() );
}

QPolygonF ReosWatershedDelineating::lastWatershedDelineated() const
{
  if ( isDelineatingFinished() && mProcess && mProcess->isSuccessful() )
    return mProcess->watershedPolygon();
  else
    return QPolygonF();
}

QPolygonF ReosWatershedDelineating::lastStreamLine() const
{
  if ( isDelineatingFinished() && mProcess && mProcess->isSuccessful() )
    return mProcess->streamLine();
  else
    return QPolygonF();
}

ReosWatershed *ReosWatershedDelineating::validateWatershed( ReosWatershedStore &store )
{
  if ( mCurrentState == WaitingForValidate && isDelineatingFinished() && mProcess && mProcess->isSuccessful() )
  {
    mCurrentState = WaitingForDownstream;
    return store.addWatershed( new ReosWatershed(
                                 mProcess->watershedPolygon(),
                                 mDownstreamLine,
                                 mProcess->directions(),
                                 mProcess->outputRasterExtent() ) );
  }
  else
    return nullptr;
}

void ReosWatershedDelineating::onDelineatingFinished()
{
  testPredefinedExtentValidity();
}

void ReosWatershedDelineating::testPredefinedExtentValidity()
{
  if ( ! isDelineatingFinished() || !mProcess || !mProcess->isSuccessful() )
  {
    mCurrentState = WaitingWithBroughtBackExtent;
    return;
  }

  QPolygonF watershedPolygon = mProcess->watershedPolygon();
  ReosRasterExtent predefinedRasterExtent = mProcess->outputRasterExtent();

  ReosMapExtent watershedExtent( mProcess->watershedPolygon() );

  //! As the dem/direction raster have the border cells unrealistic, if the watershed extent have an extent closer than
  //! one cell of the dem/direction extent, that means the watershed extent must exceed the source extent
  double xCellSize = fabs( predefinedRasterExtent.xCellSize() );
  double yCellSize = fabs( predefinedRasterExtent.yCellSize() );
  if ( watershedExtent.xMapMin() <= predefinedRasterExtent.xMapMin() + xCellSize ||
       watershedExtent.xMapMax() >= predefinedRasterExtent.xMapMax() - xCellSize ||
       watershedExtent.yMapMin() <= predefinedRasterExtent.yMapMin() + yCellSize ||
       watershedExtent.yMapMax() >= predefinedRasterExtent.yMapMax() - yCellSize )
  {
    mCurrentState = WaitingWithBroughtBackExtent;
    return;
  }

  mCurrentState = WaitingForValidate;
}

void ReosWatershedDelineating::addBurningLines( const QPolygonF &burningLine )
{
  mBurningLines.append( burningLine );
  mIsBurningLineUpToDate = false;
}


ReosWatershedDelineatingProcess::ReosWatershedDelineatingProcess( ReosDigitalElevationModel *dem,
    const ReosMapExtent &mapExtent,
    const QPolygonF &downtreamLine,
    const QList<QPolygonF> &burningLines ):
  mExtent( mapExtent ),
  mEntryDem( dem ),
  mDownstreamLine( downtreamLine ),
  mBurningLines( burningLines )
{}

ReosWatershedDelineatingProcess::ReosWatershedDelineatingProcess( ReosRasterWatershed::Directions direction,
    const ReosRasterExtent &rasterExtent,
    const QPolygonF &downtreamLine ):

  mDownstreamLine( downtreamLine ),
  mDirections( direction ),
  mOutputRasterExtent( rasterExtent )
{

}

void ReosWatershedDelineatingProcess::start()
{
  mIsSuccessful = false;

  if ( !mDirections.isValid() )
  {
    if ( !mEntryDem )
      return;
    //--------------------------
    //Extract dem and fill it (burn it if needed)
    ReosRasterMemory<float> dem( mEntryDem->extractMemoryRasterSimplePrecision( mExtent, mOutputRasterExtent ) );
    burnRasterDem( dem, mBurningLines, mOutputRasterExtent );
    std::unique_ptr<ReosRasterFillingWangLiu> fillDemProcess( new ReosRasterFillingWangLiu( dem, fabs( mOutputRasterExtent.xCellSize() ), fabs( mOutputRasterExtent.yCellSize() ) ) );

    fillDemProcess->start();

    if ( !fillDemProcess->isSuccessful() )
      return;

    mDirections = fillDemProcess->directionRaster();
    fillDemProcess.release();
  }

  //--------------------------
  //rasterize the dowstreamline
  ReosRasterLine rasterDownstreamLine;
  for ( const QPointF &point : mDownstreamLine )
  {
    ReosRasterCellPos cell = mOutputRasterExtent.mapToCellPos( point );
    rasterDownstreamLine.addPoint( cell.row(), cell.column() );
  }

  //--------------------------
  // Calculate raster watershed
  std::unique_ptr<ReosRasterWatershedFromDirectionAndDownStreamLine> rasterWatershedFromDirection(
    new ReosRasterWatershedFromDirectionAndDownStreamLine( mDirections, rasterDownstreamLine ) );

  rasterWatershedFromDirection->start();

  if ( !rasterWatershedFromDirection->isSuccessful() )
    return;

  ReosRasterWatershed::Watershed watershed = rasterWatershedFromDirection->watershed();
  ReosRasterCellPos downStreamPoint = rasterWatershedFromDirection->firstCell();
  ReosRasterCellPos endOfLongerPath = rasterWatershedFromDirection->endOfLongerPath();
  rasterWatershedFromDirection.release();

  //--------------------------
  // Polygonize the raster watershed
  std::unique_ptr<ReosRasterWatershedToVector> watershedToPolygon( new ReosRasterWatershedToVector( watershed, mOutputRasterExtent, downStreamPoint ) );
  watershedToPolygon->start();

  if ( !watershedToPolygon->isSuccessful() )
    return;

  mOutputWatershed = watershedToPolygon->watershed();
  watershedToPolygon.release();

  //--------------------------
  // Get the stream line
  std::unique_ptr<ReosRasterWatershedTraceDownstream> traceDownstream( new ReosRasterWatershedTraceDownstream( mDirections, rasterDownstreamLine, mOutputRasterExtent, endOfLongerPath ) );
  traceDownstream->start();

  if ( !traceDownstream->isSuccessful() )
    return;

  mOutputStreamline = traceDownstream->resultPolyline();

  mIsSuccessful = true;
}

QPolygonF ReosWatershedDelineatingProcess::watershedPolygon() const
{
  return mOutputWatershed;
}

QPolygonF ReosWatershedDelineatingProcess::streamLine() const
{
  return mOutputStreamline;
}

ReosRasterWatershed::Directions ReosWatershedDelineatingProcess::directions() const
{
  return mDirections;
}

ReosRasterExtent ReosWatershedDelineatingProcess::outputRasterExtent() const
{
  return mOutputRasterExtent;
}

void ReosWatershedDelineatingProcess::burnRasterDem( ReosRasterMemory<float> &rasterDem, const QList<QPolygonF> &burningLines, const ReosRasterExtent &rasterExtent )
{
  for ( const QPolygonF &burningLine : burningLines )
  {
    if ( !burningLine.isEmpty() )
    {
      ReosRasterLine rasterBurningLine( false );
      const ReosRasterCellPos &px0 = rasterExtent.mapToCellPos( burningLine.at( 0 ) );
      rasterBurningLine.addPoint( px0 );
      double length = 0;
      for ( int i = 0; i < burningLine.count() - 1; ++i )
      {
        const QPointF ptMap_i = burningLine.at( i );
        const QPointF ptMap_ip1 = burningLine.at( i + 1 );
        const QPointF vectorDistance = ptMap_ip1 - ptMap_i;
        length += sqrt( pow( vectorDistance.x(), 2 ) + pow( vectorDistance.y(), 2 ) );
        const ReosRasterCellPos px_ip1 = rasterExtent.mapToCellPos( ptMap_ip1 );
        rasterBurningLine.addPoint( px_ip1 );
      }
      const ReosRasterCellPos firstPixel = rasterBurningLine.cellPosition( 0 );
      const ReosRasterCellPos lastPixel = rasterBurningLine.lastCellPosition();
      float firstAlt = rasterDem.value( firstPixel.row(), firstPixel.column() );
      float dz = ( rasterDem.value( lastPixel.row(), lastPixel.column() ) - firstAlt ) / ( rasterBurningLine.cellCount() - 1 );

      for ( unsigned i = 1; i < rasterBurningLine.cellCount() - 1; ++i )
      {
        const ReosRasterCellPos currentPixel = rasterBurningLine.cellPosition( i );
        float newAlt = firstAlt + dz * i;
        rasterDem.setValue( currentPixel, newAlt );
      }
    }
  }
}
