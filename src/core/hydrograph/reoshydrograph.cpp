/***************************************************************************
  reoshydrograph.cpp
 ---------------------
 begin                : 19.5.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reoshydrograph.h"
#include "reosmeteorologicmodel.h"
#include "reosrunoffmodel.h"
#include "reostransferfunction.h"

#include <QTimer>


ReosHydrograph::ReosHydrograph( QObject *parent, const QString &providerKey, const QString &dataSource )
  : ReosTimeSerieVariableTimeStep( parent,
                                   providerKey.isEmpty() ? QStringLiteral( "variable-time-step-memory" ) : providerKey,
                                   dataSource ) {}

void ReosHydrograph::updateData() const
{

}


ReosEncodedElement ReosHydrograph::encode() const
{
  ReosEncodedElement element( QStringLiteral( "hydrograph" ) );
  ReosTimeSerieVariableTimeStep::baseEncode( element );
  return element;
}

ReosHydrograph *ReosHydrograph::decode( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "hydrograph" ) )
    return nullptr;

  std::unique_ptr<ReosHydrograph> ret = std::make_unique<ReosHydrograph>( parent );
  ret->decodeBase( element );

  return ret.release();
}

bool ReosHydrograph::hydrographIsObsolete() const
{
  return isObsolete();
}

void ReosHydrograph::setHydrographObsolete()
{
  setObsolete();
}

ReosHydrographsStore::ReosHydrographsStore( QObject *parent ): ReosHydrographGroup( parent ) {}

void ReosHydrographsStore::addHydrograph( ReosHydrograph *hydrograph )
{
  hydrograph->setParent( this );
  mHydrographs.append( hydrograph );

  connect( hydrograph, &ReosDataObject::dataChanged, this, &ReosHydrographsStore::hydrographChanged );

  emit dataChanged();
}

void ReosHydrographsStore::removeHydrograph( int index )
{
  mHydrographs.takeAt( index )->deleteLater();
  emit hydrographRemoved( index );
  emit dataChanged();
}

int ReosHydrographsStore::hydrographCount() const
{
  return mHydrographs.count();
}

QStringList ReosHydrographsStore::hydrographNames() const
{
  QStringList ret;
  for ( const ReosHydrograph *hyd : mHydrographs )
    ret.append( hyd->name() );

  return ret;
}

QList<ReosHydrograph *> ReosHydrographsStore::hydrographsForTimeRange( const QDateTime &startTime, const QDateTime &endTime ) const
{
  QList<ReosHydrograph *> ret;

  for ( ReosHydrograph *hyd : std::as_const( mHydrographs ) )
  {
    QPair<QDateTime, QDateTime> timeExtent = hyd->timeExtent();
    if ( timeExtent.second > startTime && timeExtent.first < endTime )
      ret.append( hyd );
  }

  return ret;
}

QList<ReosHydrograph *> ReosHydrographsStore::allHydrographs() const
{
  return mHydrographs;
}

ReosHydrograph *ReosHydrographsStore::hydrograph( int index ) const
{
  if ( index >= 0 && index < mHydrographs.count() )
    return mHydrographs.at( index );

  return nullptr;
}

ReosEncodedElement ReosHydrographsStore::encode() const
{
  ReosEncodedElement element( QStringLiteral( "hydrograph-store" ) );

  QList<QByteArray> encodedHydrographs;
  encodedHydrographs.reserve( mHydrographs.count() );
  for ( const ReosHydrograph *hyd : mHydrographs )
    encodedHydrographs.append( hyd->encode().bytes() );

  element.addData( "hydrographs", encodedHydrographs );

  return element;
}

void ReosHydrographsStore::decode( const ReosEncodedElement &element )
{
  qDeleteAll( mHydrographs );
  mHydrographs.clear();

  if ( element.description() != QStringLiteral( "hydrograph-store" ) )
    return;

  QList<QByteArray> encodedHydrographs;
  if ( !element.getData( "hydrographs", encodedHydrographs ) )
    return;

  mHydrographs.reserve( encodedHydrographs.count() );
  for ( const QByteArray &bytes : std::as_const( encodedHydrographs ) )
  {
    mHydrographs.append( ReosHydrograph::decode( ReosEncodedElement( bytes ), this ) );

    connect( mHydrographs.last(), &ReosDataObject::dataChanged, this, &ReosHydrographsStore::hydrographChanged );

    if ( mHydrographs.last()->dataProvider()->key() == QStringLiteral( "variable-time-step-memory" ) )
    {
      // with Lekan 2.2, one extra millisecond is present
      // Leanding to wrong behavior in certain cases
      // So, we nne to check end remove this millisecond
      QDateTime dt = mHydrographs.last()->referenceTime();
      QTime time = dt.time();
      time = QTime( time.hour(), time.minute(), time.second(), 0 );
      dt.setTime( time );
      mHydrographs.last()->setReferenceTime( dt );
    }
  }

}

ReosRunoffHydrographsStore::ReosRunoffHydrographsStore( ReosMeteorologicModelsCollection *meteoModelsCollection,
    QObject *parent )
  : ReosHydrographGroup( parent )
  , mMeteoModelsCollection( meteoModelsCollection )
{
  connect( mMeteoModelsCollection, &ReosMeteorologicModelsCollection::changed, this, &ReosRunoffHydrographsStore::updateStore );
}

void ReosRunoffHydrographsStore::setWatershed( ReosWatershed *watershed )
{
  if ( mWatershed != watershed )
    mMeteoModelToHydrographCalculationData.clear();
  mWatershed = watershed;
  updateStore();
}

int ReosRunoffHydrographsStore::hydrographCount() const
{
  return mMeteoModelToHydrographCalculationData.count();
}

QPointer<ReosHydrograph> ReosRunoffHydrographsStore::hydrograph( ReosMeteorologicModel *meteoModel )
{
  if ( mMeteoModelToHydrographCalculationData.contains( meteoModel ) )
  {
    mMeteoModelToHydrographCalculationData[meteoModel].hasBeenAsked = true;
    updateHydrograph( mMeteoModelToHydrographCalculationData.value( meteoModel ).hydrograph.get() );
    return mMeteoModelToHydrographCalculationData.value( meteoModel ).hydrograph.get();
  }

  return nullptr;
}

QPointer<ReosRunoff> ReosRunoffHydrographsStore::runoff( ReosMeteorologicModel *meteoModel )
{
  if ( mMeteoModelToHydrographCalculationData.contains( meteoModel ) )
  {
    return mMeteoModelToHydrographCalculationData.value( meteoModel ).runoff.get();
  }

  return nullptr;
}

void ReosRunoffHydrographsStore::updateStore()
{
  if ( mWatershed.isNull() || mMeteoModelsCollection.isNull() )
    mMeteoModelToHydrographCalculationData.clear();

  QList<ReosMeteorologicModel *> allModels;
  QList<ReosMeteorologicModel *> modelToRemove;
  for ( int i = 0; i < mMeteoModelsCollection->modelCount(); ++i )
  {
    //For each model in the collection
    ReosMeteorologicModel *model = mMeteoModelsCollection->meteorologicModel( i );
    allModels.append( model );
    if ( model->hasRainfall( mWatershed ) )
    {
      // if this model has a rainfall associated with this watershed
      if ( !mMeteoModelToHydrographCalculationData.contains( model ) )
      {
        // if no data exist for this model, create some
        HydrographCalculationData hydData;
        hydData.rainfall = model->associatedRainfall( mWatershed );
        hydData.hydrograph.reset( new ReosHydrograph( this ) );
        if ( model->color().isValid() )
        {
          hydData.hydrograph->setColor( model->color() );
        }
        ReosHydrograph *hyd = hydData.hydrograph.get();
        connect( hyd, &ReosHydrograph::colorChanged, model, &ReosMeteorologicModel::setColor );
        connect( model, &ReosMeteorologicModel::colorChanged, hyd, &ReosTimeSerieVariableTimeStep::setCommonColor );

        hydData.hydrograph->setName( tr( "%1 hydrograph" ).arg( model->name()->value() ) );
        hydData.runoff.reset( new ReosRunoff( mWatershed->runoffModels(), hydData.rainfall ) );
        mMeteoModelToHydrographCalculationData.insert( model, hydData );
        registerInputdata( model, hydData.hydrograph.get() );
        registerInputdata( hydData.rainfall, hydData.hydrograph.get() );
        registerInputdata( mWatershed, hydData.hydrograph.get() );
        emit hydrographAdded( model );
      }
      else
      {
        // if there is already some data related to this meteo model
        HydrographCalculationData hydData = mMeteoModelToHydrographCalculationData.value( model );
        ReosSeriesRainfall *modelRainfall = model->associatedRainfall( mWatershed );
        if ( modelRainfall != hydData.rainfall ) // first, we check if the rainfall is the same
        {
          //if not, deregister the old one and replace it by the new one
          deregisterInputData( hydData.rainfall, hydData.hydrograph.get() );
          hydData.rainfall = modelRainfall;
          registerInputdata( hydData.rainfall, hydData.hydrograph.get() );
          // do not forget to replace the rainfall in the runoff object, setting it obsolete
          hydData.runoff->setRainfall( hydData.rainfall );

          // Then, update the map
          mMeteoModelToHydrographCalculationData[model] = hydData;
        }
      }

    }
    else if ( mMeteoModelToHydrographCalculationData.contains( model ) )
    {
      modelToRemove.append( model );
    }
  }

  const QList<ReosMeteorologicModel *> models = mMeteoModelToHydrographCalculationData.keys();
  for ( ReosMeteorologicModel *model : models )
    if ( !allModels.contains( model ) && !modelToRemove.contains( model ) )
      modelToRemove.append( model );

  for ( ReosMeteorologicModel *model : modelToRemove )
  {
    mMeteoModelToHydrographCalculationData.remove( model );
    emit hydrographRemoved( model );
  }
}

void ReosHydrographGroup::registerInputdata( ReosDataObject *input, ReosHydrograph *hydrograph )
{
  hydrograph->setObsolete();
  hydrograph->registerUpstreamData( input );

  QList<QPointer<ReosHydrograph>> hydsPtr;

  if ( mMapInputToHydrographs.contains( input ) )
    hydsPtr = mMapInputToHydrographs.value( input );
  else
    connect( input, &ReosDataObject::dataChanged, this, &ReosHydrographGroup::updateHydrographFromSignal );

  if ( !hydsPtr.contains( hydrograph ) )
    hydsPtr.append( hydrograph );
  mMapInputToHydrographs[input] = hydsPtr;
}

void ReosHydrographGroup::deregisterInputData( ReosDataObject *input, ReosHydrograph *hydrograph )
{
  hydrograph->deregisterUpstreamData( input );

  if ( mMapInputToHydrographs.contains( input ) )
  {
    QList<QPointer<ReosHydrograph>> hydsPtr = mMapInputToHydrographs.value( input );
    if ( hydsPtr.contains( hydrograph ) )
      hydsPtr.removeOne( hydrograph );

    if ( hydsPtr.isEmpty() )
    {
      disconnect( input, &ReosDataObject::dataChanged, this, &ReosHydrographGroup::updateHydrographFromSignal );
      mMapInputToHydrographs.remove( input );
    }
    else
      mMapInputToHydrographs[input] = hydsPtr;
  }

}

void ReosHydrographGroup::updateHydrographFromSignal()
{
  ReosDataObject *senderData = qobject_cast<ReosDataObject *>( sender() );
  if ( senderData )
  {
    if ( mMapInputToHydrographs.contains( senderData ) )
    {
      const QList<QPointer<ReosHydrograph>> hydsPtr = mMapInputToHydrographs.value( senderData );
      QList<QPointer<ReosHydrograph>> hydsToUpdate;

      for ( QPointer<ReosHydrograph> hydPtr : hydsPtr )
      {
        if ( !hydPtr.isNull() )
        {
          updateHydrograph( hydPtr );
        }
      }
    }
  }
}

void ReosHydrographGroup::updateHydrograph( ReosHydrograph * ) {}

void ReosHydrographGroup::onInputDataDestroy()
{
  ReosDataObject *senderData = qobject_cast<ReosDataObject *>( sender() );
  if ( senderData )
  {
    mMapInputToHydrographs.remove( senderData );
  }
}

void ReosRunoffHydrographsStore::updateHydrograph( ReosHydrograph *hyd )
{
  const QList<ReosMeteorologicModel *> keys = mMeteoModelToHydrographCalculationData.keys();
  for ( ReosMeteorologicModel *meteoModel : keys )
  {
    HydrographCalculationData hydData = mMeteoModelToHydrographCalculationData.value( meteoModel );
    if ( hyd == hydData.hydrograph.get() )
    {
      //check if the rainfall is still the same
      ReosSeriesRainfall *modelRainfall = meteoModel->associatedRainfall( mWatershed );
      if ( hydData.rainfall != modelRainfall )
      {
        hydData.runoff->setRainfall( modelRainfall );
        hydData.rainfall = modelRainfall;
        mMeteoModelToHydrographCalculationData[meteoModel] = hydData;
      }

      if ( ! hydData.hasBeenAsked )
        continue;
      else
        mModelMeteoToUpdate.insert( meteoModel );
    }
  }

#ifndef _NDEBUG
  qDebug() << "update hydrograph " << mCalculationCanBeLaunch << mModelMeteoToUpdate.count();
#endif

  if ( mCalculationCanBeLaunch && !mModelMeteoToUpdate.isEmpty() )
  {
#ifndef _NDEBUG
    qDebug() << "prepare hydrograhs calculation ";
#endif

    // first, we invoke a lambda function that prepare the calculation once we come back to the event loop
    // this is because we are sure that all data objects related to the calculation will be set obsolete only when we are back in the event loop
    // this is due to the propogation of signals/slots and the fact that we don't know the order of the propagation
    QMetaObject::invokeMethod( this, [this]
    {
#ifndef _NDEBUG
      qDebug() << "launch hydrograhs calculation " << mModelMeteoToUpdate.count();
#endif
      ReosTransferFunction *function = nullptr;
      if ( mWatershed )
        function = mWatershed->currentTransferFunction();
      if ( !function )
        return;

      for ( ReosMeteorologicModel *model : std::as_const( mModelMeteoToUpdate ) )
      {
        if ( mHydrographCalculation.contains( model ) )
        {
#ifndef _NDEBUG
          qDebug() << "stop hydrograh calculation " << mHydrographCalculation.value( model );
#endif
          mHydrographCalculation.value( model )->stop( true );
          mHydrographCalculation.remove( model );
        }

        HydrographCalculationData hydData;
        if ( mMeteoModelToHydrographCalculationData.contains( model ) )
          hydData = mMeteoModelToHydrographCalculationData.value( model );

        ReosHydrograph *hydro = hydData.hydrograph.get();

        if ( !hydro || !hydro->hydrographIsObsolete() )
        {
          //emit hydrographReady( hydro );
          continue;
        }

        hydro->clear();

        ReosHydrographCalculation *hydrographCalculation = function->calculationProcess( hydData.runoff.get() );
        if ( !hydrographCalculation )
          continue;
        mHydrographCalculation.insert( model, hydrographCalculation );

        connect( hydrographCalculation, &ReosHydrographCalculation::finished, this, [this, model, hydrographCalculation]()
        {
          if ( mMeteoModelToHydrographCalculationData.contains( model ) && hydrographCalculation->isSuccessful() )
          {
            mMeteoModelToHydrographCalculationData.value( model ).hydrograph->copyFrom( hydrographCalculation->hydrograph() );
            emit hydrographReady( mMeteoModelToHydrographCalculationData.value( model ).hydrograph.get() );
          }
#ifndef _NDEBUG
          qDebug() << "finish hydrograh calculation " << hydrographCalculation;
#endif
          hydrographCalculation->deleteLater();
          if ( mHydrographCalculation.value( model ) == hydrographCalculation )
            mHydrographCalculation.remove( model );
        } );
      }

      mModelMeteoToUpdate.clear();
    }, Qt::QueuedConnection );

    // second, we invoke a lambda function that will be execute the calculation once we come back again in the event loop
    // this is because we are sure all data object related to the calculation will be set updated only when we are back in the event loop
    // this is due to the propogation of signals
    QMetaObject::invokeMethod( this, [this]
    {

      if ( !mHydrographCalculation.isEmpty() )
      {
        for ( ReosHydrographCalculation *calculation : std::as_const( mHydrographCalculation ) )
        {
#ifndef _NDEBUG
          qDebug() << "start hydrograh calculation " << calculation;
#endif
          calculation->startOnOtherThread();
        }
        updateCount++;
        mModelMeteoToUpdate.clear();
      }

      mCalculationCanBeLaunch = true;

    }, Qt::QueuedConnection );

    mCalculationCanBeLaunch = false;
  }
}

ReosHydrograph *ReosHydrographCalculation::getHydrograph( QObject *parent )
{
  if ( mHydrograph )
    mHydrograph->setParent( parent );
  return mHydrograph.release();
}

ReosHydrograph *ReosHydrographCalculation::hydrograph()
{
  return mHydrograph.get();
}
