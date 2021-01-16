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
#include "reoswatershedtree.h"

ReosWatershedDelineating::ReosWatershedDelineating( ReosModule *parent, ReosWatershedTree *watershedtree, ReosGisEngine *gisEngine ):
  ReosModule( parent ),
  mWatershedTree( watershedtree ),
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

  mDEMLayerId = QString();
  mCurrentState = NoDigitalElevationModel;

  return false;
}


ReosWatershedDelineating::State ReosWatershedDelineating::currentState() const
{
  return mCurrentState;
}

bool ReosWatershedDelineating::setDownstreamLine( const QPolygonF &downstreamLine )
{
  if ( downstreamLine.count() > 1 )
  {
    bool ok;
    mDownstreamWatershed = mWatershedTree->downstreamWatershed( downstreamLine, ok );
    if ( !ok )
      return false;

    mDownstreamLine = downstreamLine;
    if ( mDownstreamWatershed &&
         mDownstreamWatershed->hasDirectiondata() &&
         mIsBurningLineUpToDate )
    {
      mCurrentState = WaitingforProceed;
      sendMessage( tr( "Waiting for proceeding to delineating" ), ReosModule::Message );
      return true;
    }
    else
      mDownstreamWatershed = nullptr;

    mCurrentState = WaitingForExtent;
    sendMessage( tr( "Waiting for predefined extent" ), ReosModule::Message );
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
    sendMessage( tr( "Waiting for proceeding to delineating" ), ReosModule::Message );
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

  if ( mDownstreamWatershed && mDownstreamWatershed->hasDirectiondata() && mIsBurningLineUpToDate )
    mProcess = std::make_unique<ReosWatershedDelineatingProcess>( mDownstreamWatershed, mDownstreamLine );
  else
  {
    std::unique_ptr<ReosDigitalElevationModel> dem;
    dem.reset( mGisEngine->getDigitalElevationModel( mDEMLayerId ) );
    mProcess = std::make_unique<ReosWatershedDelineatingProcess>( dem.release(), mExtent, mDownstreamLine, mBurningLines );
  }

  startProcessOnOtherThread( mProcess.get() );
  sendMessage( tr( "Start delineating" ), ReosModule::Message );
  return true;
}

ReosProcess *ReosWatershedDelineating::delineatingProcess()
{
  return mProcess.get();
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

bool ReosWatershedDelineating::validateWatershed( bool &needAdjusting )
{
  if ( mCurrentState == WaitingForValidate && isDelineatingFinished() && mProcess && mProcess->isSuccessful() )
  {
    if ( mDownstreamWatershed && mDownstreamWatershed->hasDirectiondata() )
      mCurrentWatershed.reset( new ReosWatershed(
                                 mProcess->watershedPolygon(),
                                 mProcess->streamLine().last(),
                                 ReosWatershed::Automatic,
                                 mDownstreamLine,
                                 mProcess->streamLine() ) );
    else
    {
      // reduce the direction raster extent and create new watershed with it
      ReosRasterExtent originalRasterExtent = mProcess->outputRasterExtent();
      QPointF cornerMin = QPointF( mExtent.xMapMin() + fabs( originalRasterExtent.xCellSize() ) / 2,
                                   mExtent.yMapMin() + fabs( originalRasterExtent.yCellSize() ) / 2 );
      QPointF cornerMax = QPointF( mExtent.xMapMax() - fabs( originalRasterExtent.xCellSize() ) / 2,
                                   mExtent.yMapMax() - fabs( originalRasterExtent.yCellSize() ) / 2 );

      ReosRasterCellPos corner1 = originalRasterExtent.mapToCellPos( cornerMin );
      ReosRasterCellPos corner2 = originalRasterExtent.mapToCellPos( cornerMax );
      int rowMin = std::min( corner1.row(), corner2.row() );
      int rowMax = std::max( corner1.row(), corner2.row() );
      int colMin = std::min( corner1.column(), corner2.column() );
      int colMax = std::max( corner1.column(), corner2.column() );
      ReosRasterExtent reducedRasterExtent( mExtent, colMax - colMin + 1, rowMax - rowMin + 1 );

      ReosRasterWatershed::Directions reducedDirection = mProcess->directions().reduceRaster( rowMin, rowMax, colMin, colMax );

      mCurrentWatershed.reset( new ReosWatershed( mProcess->watershedPolygon(),
                               mProcess->streamLine().last(),
                               ReosWatershed::Automatic,
                               mDownstreamLine,
                               mProcess->streamLine(),
                               reducedDirection,
                               reducedRasterExtent ) ) ;
    }

    needAdjusting = mWatershedTree->isWatershedIntersectExisting( mCurrentWatershed.get() );
    mIsBurningLineUpToDate = true;
    mCurrentState = WaitingToRecord;
    return true;
  }

  return false;

}

ReosWatershed *ReosWatershedDelineating::storeWatershed( bool adjustIfNeeded )
{
  ReosWatershed *newWatershed = nullptr;

  if ( mCurrentState == WaitingToRecord && mCurrentWatershed )
  {
    newWatershed = mWatershedTree->addWatershed( mCurrentWatershed.release(), adjustIfNeeded );
    ReosWatershed *dsws = newWatershed->downstreamWatershed();
    sendMessage( tr( "%1 validated%2" ).arg( newWatershed->name() )
                 .arg( dsws ? ( tr( " and added to %1" ).arg( newWatershed->downstreamWatershed()->name() ) ) : QString() ), ReosModule::Message );
    mCurrentState = WaitingForDownstream;
  }

  return newWatershed;
}

bool ReosWatershedDelineating::hasDirectionData() const
{
  return ( mDownstreamWatershed && mDownstreamWatershed->hasDirectiondata() );
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
    sendMessage( tr( "Predifined extent intersect watershed delineating, please redefined extent" ), ReosModule::Warning );
    return;
  }

  if ( !mDownstreamWatershed )
  {
    //! Here the original extent is not too small, so reduce it to fit just to the new watershed (with extraborder);
    mExtent = ReosMapExtent( watershedExtent.xMapMin() - xCellSize * 2,
                             watershedExtent.yMapMin() - yCellSize * 2,
                             watershedExtent.xMapMax() + xCellSize * 2,
                             watershedExtent.yMapMax() + yCellSize * 2 );

  }

  mCurrentState = WaitingForValidate;
  sendMessage( tr( "Watershed ready for validation" ), ReosModule::Message );
}

void ReosWatershedDelineating::setBurningLines( const QList<QPolygonF> &burningLines )
{
  mBurningLines = burningLines;
  mIsBurningLineUpToDate = false;
  mWatershedTree->removeDirectionData();
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

ReosWatershedDelineatingProcess::ReosWatershedDelineatingProcess( ReosWatershed *downstreamWatershed,
    const QPolygonF &downtreamLine ):

  mDownstreamLine( downtreamLine ),
  mDirections( downstreamWatershed->directions() ),
  mOutputRasterExtent( downstreamWatershed->directionExtent() )
{

}

void ReosWatershedDelineatingProcess::start()
{
  mIsSuccessful = false;

  if ( !mDirections.isValid() )
  {
    if ( !mEntryDem )
    {
      finish();
      return;
    }

    //--------------------------
    //Extract dem and fill it (burn it if needed)
    setCurrentProgression( 0 );
    setInformation( tr( "Extract digital elevation model" ) );
    ReosRasterMemory<float> dem( mEntryDem->extractMemoryRasterSimplePrecision( mExtent, mOutputRasterExtent, QString(), this ) );
    if ( isStopAsked() )
    {
      finish();
      return;
    }

    burnRasterDem( dem, mBurningLines, mOutputRasterExtent );
    setCurrentProgression( 0 );
    std::unique_ptr<ReosRasterFillingWangLiu> fillDemProcess( new ReosRasterFillingWangLiu( dem, fabs( mOutputRasterExtent.xCellSize() ), fabs( mOutputRasterExtent.yCellSize() ) ) );
    setSubProcess( fillDemProcess.get() );

    setInformation( tr( "Filling digital elevation model and calculating direction" ) );
    fillDemProcess->start();

    if ( isStopAsked() || !fillDemProcess->isSuccessful() )
    {
      finish();
      return;
    }

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
  setCurrentProgression( 0 );
  setSubProcess( rasterWatershedFromDirection.get() );

  setInformation( tr( "Delineate watershed" ) );

  rasterWatershedFromDirection->start();

  if ( isStopAsked() || !rasterWatershedFromDirection->isSuccessful() )
  {
    finish();
    return;
  }

  ReosRasterWatershed::Watershed watershed = rasterWatershedFromDirection->watershed();
  ReosRasterCellPos downStreamPoint = rasterWatershedFromDirection->firstCell();
  ReosRasterCellPos endOfLongerPath = rasterWatershedFromDirection->endOfLongerPath();
  rasterWatershedFromDirection.release();

  //--------------------------
  // Polygonize the raster watershed
  std::unique_ptr<ReosRasterWatershedToVector> watershedToPolygon( new ReosRasterWatershedToVector( watershed, mOutputRasterExtent, downStreamPoint ) );
  setSubProcess( watershedToPolygon.get() );
  setCurrentProgression( 0 );
  setInformation( tr( "Polygonize watershed" ) );
  watershedToPolygon->start();

  if ( isStopAsked() || !watershedToPolygon->isSuccessful() )
  {
    finish();
    return;
  }
  mOutputWatershed = watershedToPolygon->watershed();
  watershedToPolygon.release();

  //--------------------------
  // Get the stream line
  std::unique_ptr<ReosRasterWatershedTraceDownstream> traceDownstream( new ReosRasterWatershedTraceDownstream( mDirections, rasterDownstreamLine, mOutputRasterExtent, endOfLongerPath ) );
  setInformation( tr( "Trace stream line" ) );
  traceDownstream->start();

  if ( isStopAsked() || !traceDownstream->isSuccessful() )
  {
    finish();
    return;
  }

  mOutputStreamline = traceDownstream->resultPolyline();

  mIsSuccessful = true;
  emit finished();
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
