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
         mDownstreamWatershed->hasDirectiondata( mDEMLayerId ) &&
         mIsBurningLineUpToDate )
    {
      mCurrentState = WaitingforProceed;
      sendMessage( tr( "Waiting for proceeding to delineating" ), ReosModule::Simple );
      return true;
    }
    else
      mDownstreamWatershed = nullptr;

    mCurrentState = WaitingForExtent;
    sendMessage( tr( "Waiting for predefined extent" ), ReosModule::Simple );
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
    sendMessage( tr( "Waiting for proceeding to delineating" ), ReosModule::Simple );
    return true;
  }

  return false;
}

bool ReosWatershedDelineating::prepareDelineating()
{
  if ( mCurrentState != WaitingforProceed )
  {
    return false;
  }

  if ( mDownstreamWatershed && mDownstreamWatershed->hasDirectiondata( mDEMLayerId ) && mIsBurningLineUpToDate )
    mProcess = std::make_unique<ReosWatershedDelineatingProcess>( mDownstreamWatershed, mDownstreamLine, mDEMLayerId );
  else
  {
    std::unique_ptr<ReosDigitalElevationModel> dem;
    dem.reset( mGisEngine->getDigitalElevationModel( mDEMLayerId ) );
    mProcess = std::make_unique<ReosWatershedDelineatingProcess>( dem.release(), mExtent, mDownstreamLine, mBurningLines, mCalculateAverageElevation );
  }

  connect( mProcess.get(), &ReosProcess::finished, this, &ReosWatershedDelineating::onDelineatingFinished );
  sendMessage( tr( "Start delineating" ), ReosModule::Simple );
  mCurrentState = Delineating;
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
    if ( mDownstreamWatershed && mDownstreamWatershed->hasDirectiondata( mDEMLayerId ) )
      mCurrentWatershed.reset( new ReosWatershed(
                                 mProcess->watershedPolygon(),
                                 mProcess->streamLine().last(),
                                 ReosWatershed::Automatic,
                                 mDownstreamLine,
                                 mProcess->streamLine(),
                                 mProcess->rasterizedWatershed(),
                                 mProcess->outputRasterExtent(),
                                 mDEMLayerId ) );
    else
    {
      mCurrentWatershed.reset( new ReosWatershed( mProcess->watershedPolygon(),
                               mProcess->streamLine().last(),
                               ReosWatershed::Automatic,
                               mDownstreamLine,
                               mProcess->streamLine(),
                               mProcess->directions(),
                               mProcess->rasterizedWatershed(),
                               mProcess->outputRasterExtent(),
                               mDEMLayerId ) ) ;
    }

    if ( mProcess->calculateAverageElevation() && mCalculateAverageElevation )
      mCurrentWatershed->averageElevationParameter()->setDerivedValue( mProcess->averageElevation() );
    else if ( mCalculateAverageElevation )
    {
      std::unique_ptr<ReosDigitalElevationModel> dem;
      dem.reset( mGisEngine->getDigitalElevationModel( mDEMLayerId ) );
      if ( dem )
        mCurrentWatershed->averageElevationParameter()->setDerivedValue( dem->averageElevationOnGrid( mProcess->rasterizedWatershed(), mProcess->outputRasterExtent() ) );
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
    sendMessage( tr( "%1 validated%2" ).arg( newWatershed->watershedName()->value() )
                 .arg( dsws ? ( tr( " and added to %1" ).arg( newWatershed->downstreamWatershed()->watershedName()->value() ) ) : QString() ), ReosModule::Simple );
    mCurrentState = WaitingForDownstream;
  }

  return newWatershed;
}

ReosEncodedElement ReosWatershedDelineating::encode() const
{
  ReosEncodedElement element( QStringLiteral( "watershed-delineating" ) );

  element.addData( QStringLiteral( "dem-layer" ), mDEMLayerId );
  element.addData( QStringLiteral( "burning-lines" ), mBurningLines );
  element.addData( QStringLiteral( "burning-lines-uptodate" ), mIsBurningLineUpToDate );

  return element;
}

void ReosWatershedDelineating::decode( const ReosEncodedElement &element )
{
  mDEMLayerId.clear();
  mCurrentState = NoDigitalElevationModel;
  mDownstreamLine.clear();
  mExtent = ReosMapExtent();
  mDownstreamWatershed = nullptr;
  mBurningLines.clear();
  mIsBurningLineUpToDate = false;

  if ( element.description() != QStringLiteral( "watershed-delineating" ) )
    return;

  if ( !element.getData( QStringLiteral( "dem-layer" ), mDEMLayerId ) )
    return;

  if ( mGisEngine->isDigitalElevationModel( mDEMLayerId ) )
    mCurrentState = WaitingForDownstream;
  else
    mDEMLayerId.clear();

  if ( !element.getData( QStringLiteral( "burning-lines" ), mBurningLines ) )
    return;

  if ( !element.getData( QStringLiteral( "burning-lines-uptodate" ), mIsBurningLineUpToDate ) )
    return;

  emit hasBeenReset();

}

bool ReosWatershedDelineating::hasDirectionData() const
{
  return ( mDownstreamWatershed && mDownstreamWatershed->hasDirectiondata( mDEMLayerId ) );
}

ReosMapExtent ReosWatershedDelineating::currentExtent() const
{
  return mExtent;
}

void ReosWatershedDelineating::onDelineatingFinished()
{
  if ( mCurrentState == Delineating )
  {
    testPredefinedExtentValidity();
  }

}

void ReosWatershedDelineating::testPredefinedExtentValidity()
{
  if ( ! isDelineatingFinished() || !mProcess || !mProcess->isSuccessful() )
  {
    mCurrentState = WaitingWithBroughtBackExtent;
    return;
  }

  QPolygonF watershedPolygon = mProcess->watershedPolygon();
  ReosRasterExtent predefinedRasterExtent = mProcess->predefinedRasterExtent();

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
  sendMessage( tr( "Watershed ready for validation" ), ReosModule::Simple );
}

void ReosWatershedDelineating::reset()
{
  if ( mCurrentState != NoDigitalElevationModel )
    mCurrentState = WaitingForDownstream;
  mDownstreamLine.clear();
  mExtent = ReosMapExtent();
  mDownstreamWatershed = nullptr;
}

void ReosWatershedDelineating::clear()
{
  reset();
  mDEMLayerId.clear();
  mCurrentState = NoDigitalElevationModel;
  mBurningLines.clear();
  mIsBurningLineUpToDate = false;
  emit hasBeenReset();
}

void ReosWatershedDelineating::setBurningLines( const QList<QPolygonF> &burningLines )
{
  mBurningLines = burningLines;
  mIsBurningLineUpToDate = false;
  mWatershedTree->removeDirectionData();
}

QList<QPolygonF> ReosWatershedDelineating::burninglines() const
{
  return mBurningLines;
}

void ReosWatershedDelineating::setCalculateAverageElevation( bool calculate )
{
  mCalculateAverageElevation = calculate;
}


ReosWatershedDelineatingProcess::ReosWatershedDelineatingProcess( ReosDigitalElevationModel *dem,
    const ReosMapExtent &mapExtent,
    const QPolygonF &downtreamLine,
    const QList<QPolygonF> &burningLines,
    bool calculateAverageElevation ):
  mExtent( mapExtent ),
  mEntryDem( dem ),
  mDownstreamLine( downtreamLine ),
  mBurningLines( burningLines ),
  mCalculateAverageElevation( calculateAverageElevation )
{}

ReosWatershedDelineatingProcess::ReosWatershedDelineatingProcess( ReosWatershed *downstreamWatershed,
    const QPolygonF &downtreamLine,
    const QString layerId ):
  mDownstreamLine( downtreamLine ),
  mDirections( downstreamWatershed->directions( layerId ) ),
  mPredefinedRasterExtent( downstreamWatershed->directionExtent( layerId ) )
{

}

void ReosWatershedDelineatingProcess::start()
{
  mIsSuccessful = false;

  bool needNewDirection = !mDirections.isValid();
  if ( needNewDirection )
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
    float maxValue;
    ReosRasterMemory<float> dem( mEntryDem->extractMemoryRasterSimplePrecision( mExtent, mPredefinedRasterExtent, maxValue, QString(), this ) );
    if ( isStop() )
    {
      finish();
      return;
    }

    burnRasterDem( dem, mBurningLines, mPredefinedRasterExtent );
    setCurrentProgression( 0 );
    std::unique_ptr<ReosRasterFillingWangLiu> fillDemProcess( new ReosRasterFillingWangLiu( dem, fabs( mPredefinedRasterExtent.xCellSize() ), fabs( mPredefinedRasterExtent.yCellSize() ), maxValue ) );
    setSubProcess( fillDemProcess.get() );

    setInformation( tr( "Filling digital elevation model" ) );
    fillDemProcess->start();

    if ( isStop() || !fillDemProcess->isSuccessful() )
    {
      finish();
      return;
    }

    ReosRasterMemory<float> filledDem = fillDemProcess->filledDEM();
    filledDem.createTiffFile( "/home/vincent/filledDem.tif", GDT_Float32, ReosRasterExtent( mExtent, filledDem.columnCount(), filledDem.rowCount() ) );

    setCurrentProgression( 0 );
    std::unique_ptr<ReosRasterWatershedDirectionCalculation> directionProcess( new ReosRasterWatershedDirectionCalculation( fillDemProcess->filledDEM() ) );
    setSubProcess( directionProcess.get() );

    setInformation( tr( "Calculating direction" ) );
    directionProcess->start();

    if ( isStop() || !directionProcess->isSuccessful() )
    {
      finish();
      return;
    }

    mDirections = directionProcess->directions();

    mDirections.createTiffFile( "/home/vincent/dir.tif", GDT_Byte, ReosRasterExtent( mExtent, mDirections.columnCount(), mDirections.rowCount() ) );

    setSubProcess( nullptr );

    fillDemProcess.reset();
    directionProcess.reset();
  }

  //--------------------------
  //rasterize the dowstreamline
  ReosRasterLine rasterDownstreamLine;
  for ( const QPointF &point : mDownstreamLine )
  {
    ReosRasterCellPos cell = mPredefinedRasterExtent.mapToCellPos( point );
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

  if ( isStop() || !rasterWatershedFromDirection->isSuccessful() )
  {
    finish();
    return;
  }

  mRasterizedWatershed = rasterWatershedFromDirection->watershed();
  ReosRasterCellPos downStreamPoint = rasterWatershedFromDirection->firstCell();
  ReosRasterCellPos endOfLongerPath = rasterWatershedFromDirection->endOfLongerPath();
  setSubProcess( nullptr );
  rasterWatershedFromDirection.reset();

  //--------------------------
  // Polygonize the raster watershed
  std::unique_ptr<ReosRasterWatershedToVector> watershedToPolygon( new ReosRasterWatershedToVector( mRasterizedWatershed, mPredefinedRasterExtent, downStreamPoint ) );
  setSubProcess( watershedToPolygon.get() );
  setCurrentProgression( 0 );
  setInformation( tr( "Polygonize watershed" ) );
  watershedToPolygon->start();

  if ( isStop() || !watershedToPolygon->isSuccessful() )
  {
    finish();
    return;
  }
  mOutputWatershed = watershedToPolygon->watershed();
  setSubProcess( nullptr );
  watershedToPolygon.reset();

  //--------------------------
  // Get the stream line
  std::unique_ptr<ReosRasterWatershedTraceDownstream> traceDownstream( new ReosRasterWatershedTraceDownstream( mDirections, rasterDownstreamLine, mPredefinedRasterExtent, endOfLongerPath ) );
  setInformation( tr( "Trace stream line" ) );
  traceDownstream->start();

  if ( isStop() || !traceDownstream->isSuccessful() )
  {
    finish();
    return;
  }

  mOutputStreamline = traceDownstream->resultPolyline();

  //--------------------------
  // Reduce the result raster extent to the real extent of the raster watershed
  int r = 0;

  do
  {
    bool stop = false;
    for ( int c = 0; c < mRasterizedWatershed.columnCount(); ++c )
      if ( mRasterizedWatershed.value( r, c ) != 0 )
      {
        stop = true;
        break;
      }
    if ( stop )
      break;
    r++;
  }
  while ( r < mRasterizedWatershed.rowCount() - 1 );

  int rowMin = r;

  r = mRasterizedWatershed.rowCount() - 1;

  do
  {
    bool stop = false;
    for ( int c = 0; c < mRasterizedWatershed.columnCount(); ++c )
      if ( mRasterizedWatershed.value( r, c ) != 0 )
      {
        stop = true;
        break;
      }
    if ( stop )
      break;
    r--;
  }
  while ( r > 0 );


  int rowMax = r;

  int c = 0;
  do
  {
    bool stop = false;
    for ( int r = 0; r < mRasterizedWatershed.columnCount(); ++r )
      if ( mRasterizedWatershed.value( r, c ) != 0 )
      {
        stop = true;
        break;
      }
    if ( stop )
      break;
    c++;
  }
  while ( c < mRasterizedWatershed.columnCount() - 1 );

  int colMin = c;

  c = mRasterizedWatershed.columnCount() - 1;
  do
  {
    bool stop = false;
    for ( int r = 0; r < mRasterizedWatershed.columnCount(); ++r )
      if ( mRasterizedWatershed.value( r, c ) != 0 )
      {
        stop = true;
        break;
      }
    if ( stop )
      break;
    c--;
  }
  while ( c > 0 );

  int colMax = c;

  //get a 2 cells marge
  rowMin = std::max( 0, rowMin - 2 );
  rowMax = std::min( mRasterizedWatershed.rowCount() - 1, rowMax + 2 );
  colMin = std::max( 0, colMin - 2 );
  colMax = std::min( mRasterizedWatershed.columnCount() - 1, colMax + 2 );

  if ( needNewDirection )
    mDirections = mDirections.reduceRaster( rowMin, rowMax, colMin, colMax );

  mRasterizedWatershed = mRasterizedWatershed.reduceRaster( rowMin, rowMax, colMin, colMax );

  double newXOrigin = mPredefinedRasterExtent.xMapOrigin() + colMin * mPredefinedRasterExtent.xCellSize();
  double newYOrigin = mPredefinedRasterExtent.yMapOrigin() + rowMin * mPredefinedRasterExtent.yCellSize();

  mOutputRasterExtent = ReosRasterExtent( newXOrigin, newYOrigin, colMax - colMin + 1, rowMax - rowMin + 1, mPredefinedRasterExtent.xCellSize(), mPredefinedRasterExtent.yCellSize() );
  mOutputRasterExtent.setCrs( mExtent.crs() );

  // Calculate average elevation
  if ( mCalculateAverageElevation && mEntryDem )
    mAverageElevation = mEntryDem->averageElevationOnGrid( mRasterizedWatershed, mOutputRasterExtent, this );

  mEntryDem.reset();

  mIsSuccessful = true;
  finish();
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

ReosRasterWatershed::Watershed ReosWatershedDelineatingProcess::rasterizedWatershed() const
{
  return mRasterizedWatershed;
}

ReosRasterExtent ReosWatershedDelineatingProcess::predefinedRasterExtent() const
{
  return mPredefinedRasterExtent;
}

ReosRasterExtent ReosWatershedDelineatingProcess::outputRasterExtent() const
{
  return mOutputRasterExtent;
}

double ReosWatershedDelineatingProcess::averageElevation() const
{
  return mAverageElevation;
}

bool ReosWatershedDelineatingProcess::calculateAverageElevation() const
{
  return mCalculateAverageElevation;
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
