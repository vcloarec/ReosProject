/***************************************************************************
  reosgriddeddata.cpp - ReosGriddedData

 ---------------------
 begin                : 25.6.2024
 copyright            : (C) 2024 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosgriddeddata.h"

#include <QElapsedTimer>

#include "reosgriddedrainfallprovider.h"
#include "reosgriddedrainfallrenderer_p.h"
#include "reosgeometryutils.h"
#include "reosgisengine.h"
#include "reoswatershed.h"
#include "reosgdalutils.h"


ReosGriddedData::ReosGriddedData( QObject *parent )
  : ReosRenderedObject( parent )
{
  mRendererFactory.reset( new ReosGriddedRainfallRendererFactory_p( this ) );
}

ReosGriddedData::ReosGriddedData( const QString &dataSource, const QString &providerKey, QObject *parent )
  : ReosRenderedObject( parent )
  , mProvider( qobject_cast<ReosGriddedDataProvider*>( ReosDataProviderRegistery::instance()->createProvider( formatKey( providerKey ) ) ) )
{
  if ( mProvider )
  {
    makeConnection();
    mProvider->setDataSource( dataSource );
  }
  // renderer factory must be created after set the datasource because, the factory needs the extent of the provider on creation
  mRendererFactory.reset( new ReosGriddedRainfallRendererFactory_p( this ) );
}

void ReosGriddedData::makeConnection()
{
  if ( mProvider )
  {
    connect( mProvider.get(), &ReosDataProvider::dataChanged, this, &ReosDataObject::dataChanged );
    connect( mProvider.get(), &ReosDataProvider::dataReset, this, &ReosDataObject::dataReset );
    connect( mProvider.get(), &ReosDataProvider::loadingFinished, this, &ReosGriddedData::loadingFinished );
  }

  connect( this, &ReosDataObject::dataChanged, this, &ReosRenderedObject::repaintRequested );
}

void ReosGriddedData::decodeProvider( const ReosEncodedElement &element, const ReosEncodeContext &context )
{
  ReosDataObject::decode( element );
  QString providerKey;
  element.getData( QStringLiteral( "provider-key" ), providerKey );
  mProvider.reset( qobject_cast<ReosGriddedDataProvider *>( ReosDataProviderRegistery::instance()->createProvider( formatKey( providerKey ) ) ) );
  if ( mProvider )
    mProvider->decode( element.getEncodedData( QStringLiteral( "provider" ) ), context );
}

void ReosGriddedData::setProvider( ReosGriddedDataProvider *provider )
{
  mProvider.reset( provider );
}

void ReosGriddedData::setRenderer( ReosGriddedRainfallRendererFactory *rendererFactory )
{
  mRendererFactory.reset( rendererFactory );
}

ReosGriddedRainfallRendererFactory *ReosGriddedData::renderer() const
{
  return mRendererFactory.get();
}

QString ReosGriddedData::formatKey( const QString &rawKey ) const
{
  if ( rawKey.contains( QStringLiteral( "::" ) ) )
    return rawKey;

  return rawKey + QStringLiteral( "::" ) + ReosGriddedData::staticType();
}

void ReosGriddedData::updateData() const
{
  if ( mProvider )
    mProvider->load();
}


QString ReosGriddedData::type() const {return staticType();}

ReosObjectRenderer *ReosGriddedData::createRenderer( ReosRendererSettings *settings )
{
  if ( mRendererFactory )
    return mRendererFactory->createRasterRenderer( settings );

  return nullptr;
}

ReosRendererObjectMapTimeStamp *ReosGriddedData::createMapTimeStamp( ReosRendererSettings *settings ) const
{
  return new ReosRendererGriddedRainfallMapTimeStamp_p( dataIndex( settings->mapTime() ) );
}

ReosMapExtent ReosGriddedData::extent() const
{
  return rasterExtent();
}

QString ReosGriddedData::staticType() {return QStringLiteral( "gridded-data" );}

bool ReosGriddedData::isValid() const
{
  return mProvider && mProvider->isValid();
}

int ReosGriddedData::gridCount() const
{
  if ( mProvider )
    return mProvider->count();
  else
    return 0;
}

const QVector<double> ReosGriddedData::values( int index ) const
{
  if ( mProvider )
    return mProvider->data( index );
  else
    return QVector<double>();
}

const QVector<double> ReosGriddedData::valuesInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax ) const
{
  if ( !mProvider )
    return QVector<double>();

  if ( index < 0 )
    return QVector<double>();

  if ( mProvider->hasCapability( ReosGriddedRainfallProvider::SubGridExtract ) )
  {
    return mProvider->dataInGridExtent( index, rowMin, rowMax, colMin, colMax );
  }

  const QVector<double> &data = mProvider->data( index );

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

const QDateTime ReosGriddedData::startTime( int index ) const
{
  if ( mProvider )
    return mProvider->startTime( index );
  else
    return QDateTime();
}

const QDateTime ReosGriddedData::endTime( int index ) const
{
  if ( mProvider )
    return mProvider->endTime( index );
  else
    return QDateTime();
}

QPair<QDateTime, QDateTime> ReosGriddedData::timeExtent() const
{
  if ( !mProvider )
    return QPair<QDateTime, QDateTime>();

  int count = mProvider->count();

  if ( count == 0 )
    return QPair<QDateTime, QDateTime>();

  return {mProvider->startTime( 0 ), mProvider->endTime( count - 1 )};
}

ReosDuration ReosGriddedData::minimumTimeStep() const
{
  if ( !mProvider )
    return ReosDuration();

  return mProvider->minimumTimeStep();
}

double ReosGriddedData::timeStepRatio( int index, const ReosDuration &timeStep ) const
{
  if ( !mProvider )
    return 1.0;

  return mProvider->timeStepRatio( index, timeStep );
}

bool ReosGriddedData::supportExtractSubGrid() const
{
  if ( mProvider )
    return mProvider->hasCapability( ReosGriddedRainfallProvider::SubGridExtract );

  return false;
}


int ReosGriddedData::dataIndex( const QDateTime &time ) const
{
  if ( !mProvider )
    return  -1;

  return mProvider->dataIndex( time );
}

ReosRasterExtent ReosGriddedData::rasterExtent() const
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

void ReosGriddedData::copyFrom( ReosGriddedData *other )
{
  copyFrom( other->mProvider.get() );
}

void ReosGriddedData::copyFrom( ReosGriddedDataProvider *provider )
{
  mProvider->copyFrom( provider );

  //! We need to reset the renderer factor tp take account of the new extent
  mRendererFactory.reset( new ReosGriddedRainfallRendererFactory_p( this ) );
}

void ReosGriddedData::exportToTiff( int index, const QString &fileName ) const
{
  mProvider->exportToTiff( index, fileName );
}

QList<ReosColorShaderSettings *> ReosGriddedData::colorShaderSettings() const
{
  QList<ReosColorShaderSettings *> ret;
  ret << mRendererFactory->colorRampShaderSettings();
  return ret;
}

bool ReosGriddedData::getDirectMinMaxValue( double &min, double &max ) const
{
  if ( mProvider )
    return mProvider->getDirectMinMax( min, max );
  return false;
}

void ReosGriddedData::calculateMinMaxValue( double &min, double &max ) const
{
  if ( mProvider )
    mProvider->calculateMinMax( min, max );
}


ReosGriddedDataProvider *ReosGriddedData::dataProvider() const
{
  return mProvider.get();
}

AverageCalculation *ReosDataGriddedOnWatershed::getCalculationProcess() const
{
  if ( mCurrentCalculation )
    mCurrentCalculation->stop( true );

  if ( mWatershed.isNull()
       || mGriddedData.isNull()
       || mGriddedData->gridCount() == 0 )
  {
    setDataActualized();
    onDataChanged();
    return nullptr;
  }

  ReosArea watershedArea = mWatershed->areaParameter()->value();
  ReosRasterExtent rainExtent = mGriddedData->rasterExtent();
  QRectF cellRect( ( rainExtent.xMapMax() + rainExtent.xMapMin() ) / 2,
                   ( rainExtent.yMapMax() + rainExtent.yMapMin() ) / 2,
                   std::fabs( rainExtent.xCellSize() ),
                   std::fabs( rainExtent.yCellSize() ) );
  ReosArea cellArea = ReosGisEngine::polygonAreaWithCrs( cellRect, rainExtent.crs() );
  qDebug() << QString( "Cell area is: %1" ).arg( cellArea.toString() );

  std::unique_ptr<AverageCalculation> newCalc( new AverageCalculation );

  newCalc->gridExtent = mGriddedData->rasterExtent();
  newCalc->timeStep = mGriddedData->minimumTimeStep();
  qDebug() << QString( "Watershed area is: %1" ).arg( watershedArea.toString() );
  newCalc->usePrecision = watershedArea < cellArea * 30;
  newCalc->watershedPolygon = ReosGisEngine::transformToCoordinates( mWatershed->crs(), mWatershed->delineating(), rainExtent.crs() );

  return newCalc.release();
}

void AverageCalculation::start()
{
  mIsSuccessful = false;

  rasterizedWatershed = ReosGeometryUtils::rasterizePolygon(
                          watershedPolygon, gridExtent, rasterizedExtent, xOri, yOri, usePrecision, this );

  mIsSuccessful = true;

  qDebug() << QString( "average gridded precipitation %1 on watershed:" ).arg( usePrecision ? "with precision" : "without precision" );
}


void ReosDataGriddedOnWatershed::launchCalculation()
{
  AverageCalculation *newCalc = getCalculationProcess();

  if ( !newCalc )
    return;

  QObject::connect( newCalc, &ReosProcess::finished, newCalc, [newCalc, this]
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

        if ( mDistributePerArea )
        {
          ReosGdalDataset areaDistributionDS( mAreaDistributionFilePath );
          if ( areaDistributionDS.isValid() )
          {
            areaDistributionDS.resample( mRasterizedExtent, "mode" );
            mAreaDistributionGrid = areaDistributionDS.valuesBytes( 1 );

            QVector<unsigned char> distrValues = mAreaDistributionGrid.values();

            int div = static_cast<int>( std::round( 256 / mAreaCount ) );

            for ( int i = 0; i < distrValues.count(); ++i )
              if ( distrValues.at( i ) == 0 )
                distrValues[i] = mAreaCount; //we don't want values when area is classified as 0 (no data value)
              else
                distrValues[i] = static_cast<unsigned char>( std::floor( distrValues.at( i ) / div ) );

            mAreaDistributionGrid.setValues( distrValues );

            for ( int i = 0; i < mAreaCount; ++i )
              mValuesPerAreas.append( std::make_shared < QVector<double>>() );
          }
          else
          {
            mDistributePerArea = false;
          }
        }
        onCalculationFinished();
      }
    }
    newCalc->deleteLater();
  } );
  setDataActualized();
  mCurrentCalculation = newCalc;

  newCalc->startOnOtherThread();
}

ReosDataGriddedOnWatershed::ReosDataGriddedOnWatershed( ReosWatershed *watershed,
    ReosGriddedData *griddeddata,
    const ReosDuration &outputTimeStep,
    const QString areaDistributionFilePath,
    int areaCount )
  : mWatershed( watershed )
  , mGriddedData( griddeddata )
  , mOutputTimeStep( outputTimeStep )
  , mAreaDistributionFilePath( areaDistributionFilePath )
  , mAreaCount( areaCount )
{
  if ( mAreaDistributionFilePath.isEmpty() || mAreaCount < +0 )
    return;

  mDistributePerArea = true;
}

double ReosDataGriddedOnWatershed::calculateValueAt( int i ) const
{
  ReosRasterExtent extent = mGriddedData->rasterExtent();

  int rasterizedXCount = mRasterizedExtent.xCellCount();
  int rasterizedYCount = mRasterizedExtent.yCellCount();

  ReosRasterMemory<double> dataValues;
  int griddedIndex = mGriddedData->dataIndex( timeAtIndex( i ) );

  if ( griddedIndex < 0 )
    return std::numeric_limits<double>::quiet_NaN();

  int effXori;
  int effYOri;

  if ( mGriddedData->supportExtractSubGrid() )
  {
    effXori = 0;
    effYOri = 0;
    dataValues = ReosRasterMemory<double>( rasterizedYCount, rasterizedXCount );
    dataValues.setValues( mGriddedData->valuesInGridExtent(
                            griddedIndex, mYOri, mYOri + rasterizedYCount - 1, mXOri, mXOri + rasterizedXCount - 1 ) );
  }
  else
  {
    effXori = mXOri;
    effYOri = mYOri;
    dataValues = ReosRasterMemory<double>( extent.yCellCount(), extent.xCellCount() );
    dataValues.setValues( mGriddedData->values( griddedIndex ) );
  }

  double timeStepRatio = mGriddedData->timeStepRatio( griddedIndex, mOutputTimeStep );

  double averageValue = 0;
  double totalSurf = 0;

  QVector<double> distributeAverageValue;
  QVector<double> distribureTotalSurf;

  if ( mDistributePerArea )
  {
    distributeAverageValue.resize( mAreaCount );
    distributeAverageValue.fill( 0 );
    distribureTotalSurf.resize( mAreaCount );
    distribureTotalSurf.fill( 0 );
  }

  for ( int xi = 0; xi < rasterizedXCount; ++xi )
  {
    for ( int yi = 0; yi < rasterizedYCount; ++yi )
    {
      double surf = mRasterizedWatershed.value( yi, xi );
      double rv = dataValues.value( yi + effYOri, xi + effXori );
      if ( !std::isnan( rv ) )
      {
        averageValue += surf * rv;
      }
      totalSurf += surf;

      if ( mDistributePerArea && surf > 0 )
      {
        int distIndex = mAreaDistributionGrid.value( yi, xi );
        if ( distIndex < 0 || distIndex >= mAreaCount )
        {
          for ( int ni = -1; ni <= 1; ++ni )
          {
            bool stop = false;
            for ( int nj = -1; nj <= 1; ++nj )
            {
              if ( ni == 0 && nj == 0 )
                continue;

              if ( yi + ni < 0 || yi + ni >= mAreaDistributionGrid.rowCount() )
                continue;

              if ( xi + nj < 0 || xi + nj >= mAreaDistributionGrid.columnCount() )
                continue;

              if ( mAreaDistributionGrid.value( yi + ni, xi + nj ) != mAreaCount )
              {
                distIndex = mAreaDistributionGrid.value( yi + ni, xi + nj );
                stop = true;
                break;
              }
            }
            if ( stop )
              break;
          }
        }

        if ( distIndex >= 0 && distIndex < mAreaCount )
        {
          if ( !std::isnan( rv ) )
            distributeAverageValue[distIndex] = distributeAverageValue.at( distIndex ) + surf * rv;
          distribureTotalSurf[distIndex] = distribureTotalSurf.at( distIndex ) + surf;
        }
        else
        {
          for ( int di = 0; di < mAreaCount; ++di )
          {
            if ( !std::isnan( rv ) )
              distributeAverageValue[di] = distributeAverageValue.at( di ) + surf * rv / mAreaCount;
            distribureTotalSurf[di] = distribureTotalSurf.at( di ) + surf;
          }
        }
      }
    }
  }


  if ( mDistributePerArea )
  {
    for ( int di = 0; di < mValuesPerAreas.count(); ++di )
    {
      if ( distribureTotalSurf.at( di ) > 0 )
        ( *mValuesPerAreas.at( di ).get() )[i] = distributeAverageValue.at( di ) / distribureTotalSurf.at( di ) * timeStepRatio;
      else
        ( *mValuesPerAreas.at( di ).get() )[i] = 0;
    }
  }

  return averageValue / totalSurf * timeStepRatio;
}

QVector<double> ReosDataGriddedOnWatershed::valuesForArea( int areaIndex ) const
{
  if ( areaIndex >= 0 && areaIndex < mAreaCount )
    return ( *mValuesPerAreas[areaIndex].get() );

  return QVector<double>();
}


ReosSeriesFromGriddedDataOnWatershed::ReosSeriesFromGriddedDataOnWatershed( ReosWatershed *watershed, ReosGriddedData *griddedData, QObject *parent )
  : ReosSeriesFromGriddedDataOnWatershed( watershed, griddedData, griddedData->minimumTimeStep(), QString(), 0, parent )
{
}

ReosSeriesFromGriddedDataOnWatershed::ReosSeriesFromGriddedDataOnWatershed( ReosWatershed *watershed,
    ReosGriddedData *griddedData,
    const ReosDuration &outputTimeStep,
    const QString areaDistributionFilePath,
    int areaCount,
    QObject *parent )
  : ReosTimeSeriesConstantInterval( parent )
  , ReosDataGriddedOnWatershed( watershed, griddedData, outputTimeStep, areaDistributionFilePath, areaCount )
{
  connect( watershed, &ReosWatershed::geometryChanged, this, &ReosSeriesFromGriddedDataOnWatershed::onWatershedGeometryChanged );
  registerUpstreamData( griddedData );
  if ( griddedData->gridCount() > 0 )
  {
    setReferenceTime( griddedData->startTime( 0 ) );
    setTimeStep( outputTimeStep );
    QDateTime endTime = griddedData->endTime( griddedData->gridCount() - 1 );
    ReosDuration dataDuration( qint64( referenceTime().msecsTo( endTime ) ) );
    int valueCount = static_cast<int>( dataDuration.numberOfFullyContainedIntervals( timeStep() ) );
    QVector<double> values( valueCount, std::numeric_limits<double>::quiet_NaN() );
    setValues( values );
  }

  launchCalculation();

}

ReosSeriesFromGriddedDataOnWatershed::~ReosSeriesFromGriddedDataOnWatershed()
{}

ReosSeriesFromGriddedDataOnWatershed *ReosSeriesFromGriddedDataOnWatershed::create( ReosWatershed *watershed, ReosGriddedData *griddedData )
{
  std::unique_ptr<ReosSeriesFromGriddedDataOnWatershed> ret = std::make_unique<ReosSeriesFromGriddedDataOnWatershed>( watershed, griddedData );
  if ( griddedData->gridCount() > 0 )
  {
    QEventLoop loop;
    connect( ret.get(), &ReosSeriesFromGriddedDataOnWatershed::calculationFinished, &loop, &QEventLoop::quit );
    loop.exec();
  }
  return ret.release();
}

ReosSeriesFromGriddedDataOnWatershed *ReosSeriesFromGriddedDataOnWatershed::createWithTimeStep( ReosWatershed *watershed, ReosGriddedData *griddedData, const ReosDuration &outputTimeStep, const QString areaDistributionFilePath, int areaCount )
{
  QEventLoop loop;
  std::unique_ptr<ReosSeriesFromGriddedDataOnWatershed> ret =
    std::make_unique<ReosSeriesFromGriddedDataOnWatershed>(
      watershed,
      griddedData,
      outputTimeStep,
      areaDistributionFilePath,
      areaCount );
  connect( ret.get(), &ReosSeriesFromGriddedDataOnWatershed::calculationFinished, &loop, &QEventLoop::quit );
  loop.exec();

  return ret.release();
}

double ReosSeriesFromGriddedDataOnWatershed::valueAt( int i ) const
{
  double val = ReosTimeSeriesConstantInterval::valueAt( i );
  if ( !std::isnan( val ) )
    return val;

  val = calculateValueAt( i );

  constantTimeStepDataProvider()->setValue( i, val );

  return val;
}

void ReosSeriesFromGriddedDataOnWatershed::preCalculate() const
{
  int count = valueCount();
  for ( int i = 0; i < count; ++i )
  {
    valueAt( i );
    if ( i % 10 == 0 )
      std::cout << "Timestep: " << i << " / " << count << std::endl;
  }
}

void ReosSeriesFromGriddedDataOnWatershed::updateData() const
{
  if ( isObsolete() )
  {
    const_cast<ReosSeriesFromGriddedDataOnWatershed *>( this )->launchCalculation();
  }
}

void ReosSeriesFromGriddedDataOnWatershed::onCalculationFinished()
{
  initValuesPerArea( valueCount() );
  emit calculationFinished();
}

void ReosSeriesFromGriddedDataOnWatershed::onDataChanged() const
{
  emit dataChanged();
}

QDateTime ReosSeriesFromGriddedDataOnWatershed::timeAtIndex( int i ) const
{
  return ReosTimeSeriesConstantInterval::timeAt( i );
}

void ReosSeriesFromGriddedDataOnWatershed::setDataActualized() const
{
  setActualized();
}


void ReosSeriesFromGriddedDataOnWatershed::onWatershedGeometryChanged()
{
  for ( int i = 0; i < valueCount(); ++i )
    setValueAt( i, std::numeric_limits<double>::quiet_NaN() );
  setObsolete();
}


void ReosDataGriddedOnWatershed::initValuesPerArea( int count )
{
  for ( int i = 0; i < mValuesPerAreas.count(); ++i )
  {
    mValuesPerAreas.at( i )->resize( count );
    mValuesPerAreas.at( i )->fill( std::numeric_limits<double>::quiet_NaN() );
  }
}
