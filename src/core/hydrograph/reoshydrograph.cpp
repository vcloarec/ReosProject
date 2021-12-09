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

void ReosHydrograph::setInputData( ReosDataObject *dataObject )
{
  registerUpstreamData( dataObject );
}

bool ReosHydrograph::hydrographIsObsolete() const
{
  return isObsolete();
}

ReosHydrographsStore::ReosHydrographsStore( QObject *parent ): ReosHydrographGroup( parent ) {}

void ReosHydrographsStore::addHydrograph( ReosHydrograph *hydrograph )
{
  hydrograph->setParent( this );
  mHydrographs.append( hydrograph );

  emit dataChanged();
}

void ReosHydrographsStore::removeHydrograph( int index )
{
  delete mHydrographs.takeAt( index );
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

QList<ReosHydrograph *> ReosHydrographsStore::hydrographsForTimeRange( const QDateTime &startTime, const QDateTime &endTime )
{
  QList<ReosHydrograph *> ret;

  for ( ReosHydrograph *hyd : std::as_const( mHydrographs ) )
  {
    QPair<QDateTime, QDateTime> timeExtent = hyd->timeExtent();
    if ( ( timeExtent.first >= startTime && timeExtent.first <= endTime ) ||
         ( timeExtent.second >= startTime && timeExtent.second <= endTime ) )
      ret.append( hyd );
  }

  return ret;
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
    mHydrographs.append( ReosHydrograph::decode( ReosEncodedElement( bytes ), this ) );

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
  mWatershed = watershed;
  updateStore();
}

int ReosRunoffHydrographsStore::hydrographCount() const
{
  return mMeteoModelToHydrograph.count();
}

QPointer<ReosHydrograph> ReosRunoffHydrographsStore::hydrograph( ReosMeteorologicModel *meteoModel )
{
  if ( mMeteoModelToHydrograph.contains( meteoModel ) )
  {
    mModelMeteoToUpdate.insert( meteoModel );
    updateHydrographs( mMeteoModelToHydrograph.value( meteoModel ).hydrograph );
    return mMeteoModelToHydrograph.value( meteoModel ).hydrograph;
  }

  return nullptr;
}

QPointer<ReosRunoff> ReosRunoffHydrographsStore::runoff( ReosMeteorologicModel *meteoModel )
{
  if ( mMeteoModelToHydrograph.contains( meteoModel ) )
  {
    return mMeteoModelToHydrograph.value( meteoModel ).runoff;
  }

  return nullptr;
}

void ReosRunoffHydrographsStore::updateStore()
{
  if ( mWatershed.isNull() || mMeteoModelsCollection.isNull() )
    mMeteoModelToHydrograph.clear();

  QList<ReosMeteorologicModel *> allModels;
  QList<ReosMeteorologicModel *> modelToRemove;
  for ( int i = 0; i < mMeteoModelsCollection->modelCount(); ++i )
  {
    ReosMeteorologicModel *model = mMeteoModelsCollection->meteorologicModel( i );
    allModels.append( model );
    if ( model->hasRainfall( mWatershed ) )
    {
      if ( mMeteoModelToHydrograph.contains( model ) )
      {
        HydrographData hydData = mMeteoModelToHydrograph.value( model );
        ReosSerieRainfall *modelRainfall = model->associatedRainfall( mWatershed );
        if ( modelRainfall != hydData.rainfall )
        {
          deregisterInputData( hydData.rainfall, hydData.hydrograph );
          hydData.rainfall = modelRainfall;
          registerInputdata( hydData.rainfall, hydData.hydrograph );
          hydData.runoff->setRainfall( hydData.rainfall );
        }
        mMeteoModelToHydrograph[model] = hydData;
      }
      else
      {
        HydrographData hydData;
        hydData.rainfall = model->associatedRainfall( mWatershed );
        hydData.hydrograph =  new ReosHydrograph( this );
        if ( model->color().isValid() )
        {
          hydData.hydrograph->setColor( model->color() );
        }
        connect( hydData.hydrograph, &ReosHydrograph::colorChanged, model, &ReosMeteorologicModel::setColor );
        hydData.hydrograph->setName( tr( "%1 hydrograph" ).arg( model->name()->value() ) );
        hydData.runoff = new ReosRunoff( mWatershed->runoffModels(), hydData.rainfall );
        mMeteoModelToHydrograph.insert( model, hydData );
        registerInputdata( model, hydData.hydrograph );
        registerInputdata( hydData.rainfall, hydData.hydrograph );
        registerInputdata( mWatershed, hydData.hydrograph );
      }
    }
    else if ( mMeteoModelToHydrograph.contains( model ) )
    {
      modelToRemove.append( model );
    }
  }

  for ( ReosMeteorologicModel *model : mMeteoModelToHydrograph.keys() )
    if ( !allModels.contains( model ) && !modelToRemove.contains( model ) )
      modelToRemove.append( model );

  for ( ReosMeteorologicModel *model : modelToRemove )
  {
    delete mMeteoModelToHydrograph.value( model ).hydrograph;
    delete mMeteoModelToHydrograph.value( model ).runoff;
    mMeteoModelToHydrograph.remove( model );
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
          updateHydrographs( hydPtr );
        }
      }
    }
  }
}

void ReosHydrographGroup::updateHydrographs( ReosHydrograph * ) {}

void ReosHydrographGroup::onInputDataDestroy()
{
  ReosDataObject *senderData = qobject_cast<ReosDataObject *>( sender() );
  if ( senderData )
  {
    mMapInputToHydrographs.remove( senderData );
  }
}

void ReosRunoffHydrographsStore::updateHydrographs( ReosHydrograph *hyd )
{
  const QList<ReosMeteorologicModel *> keys = mMeteoModelToHydrograph.keys();
  for ( ReosMeteorologicModel *meteoModel : keys )
  {
    HydrographData hydData = mMeteoModelToHydrograph.value( meteoModel );
    if ( hyd == hydData.hydrograph )
    {
      //check if the rainfall is still the same
      ReosSerieRainfall *modelRainfall = meteoModel->associatedRainfall( mWatershed );
      if ( hydData.rainfall != modelRainfall )
      {
        hydData.runoff->setRainfall( modelRainfall );
        hydData.rainfall = modelRainfall;
        mMeteoModelToHydrograph[meteoModel] = hydData;
      }

      mModelMeteoToUpdate.insert( meteoModel );

      if ( mHydrographCalculation.contains( meteoModel ) )
        continue;
      else
        mModelMeteoToUpdate.insert( meteoModel );
    }
  }

  if ( mCalculationCanBeLaunch && !mModelMeteoToUpdate.isEmpty() )
  {
    // first, we invoke a lambda function that prepare the calculation once we come back to the event loop
    // this is because we are sure that all data objects related to the calculation will be set obsolete only when we are back in the event loop
    // this is due to the propogation of signals
    QMetaObject::invokeMethod( this, [this]
    {
      ReosTransferFunction *function = nullptr;
      if ( mWatershed )
        function = mWatershed->currentTransferFunction();
      if ( !function )
        return;

      for ( ReosMeteorologicModel *model : mModelMeteoToUpdate )
      {
        HydrographData hydData;
        if ( mMeteoModelToHydrograph.contains( model ) )
          hydData = mMeteoModelToHydrograph.value( model );

        ReosHydrograph *hydro = hydData.hydrograph;

        if ( !hydro || !hydro->hydrographIsObsolete() )
        {
          emit hydrographReady( hydro );
          continue;
        }

        hydro->clear();

        ReosTransferFunctionCalculation *hydrographCalculation = function->calculationProcess( hydData.runoff );
        mHydrographCalculation.insert( model, hydrographCalculation );

        connect( hydrographCalculation, &ReosTransferFunctionCalculation::hydrographReady, this, [this, model, hydrographCalculation]( ReosHydrograph * result )
        {
          if ( mMeteoModelToHydrograph.contains( model ) )
          {
            mMeteoModelToHydrograph.value( model ).hydrograph->copyFrom( result );
            emit hydrographReady( mMeteoModelToHydrograph.value( model ).hydrograph );
          }
          hydrographCalculation->deleteLater();

        } );
      }

      mModelMeteoToUpdate.clear();
    }, Qt::QueuedConnection );

    // second, we invoke a lambda function that will be execute the calculation once we come back to the event loop
    // this is because we are sure all data object related to the calculation will be set updated only when we are back in the event loop
    // this is due to the propogation of signals
    QMetaObject::invokeMethod( this, [this]
    {
      if ( !mHydrographCalculation.isEmpty() )
      {
        for ( ReosTransferFunctionCalculation *calculation : mHydrographCalculation )
          calculation->startOnOtherThread();
        updateCount++;
        mHydrographCalculation.clear();
      }

      mCalculationCanBeLaunch = true;

    }, Qt::QueuedConnection );

    mCalculationCanBeLaunch = false;
  }
}


