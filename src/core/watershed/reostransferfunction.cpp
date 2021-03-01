/***************************************************************************
  reostransferfunction.cpp - ReosTransferFunction

 ---------------------
 begin                : 19.2.2021
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
#include "reostransferfunction.h"

#include "reoswatershed.h"
#include "reosrunoffmodel.h"

ReosTransferFunction::ReosTransferFunction( ReosWatershed *watershed ):
  ReosDataObject( watershed )
  , mWatershed( watershed )
{
  if ( watershed )
  {
    mConcentrationTime = watershed->concentrationTime();
    mArea = watershed->area();
    connect( watershed, &ReosWatershed::changed, this, &ReosDataObject::dataChanged );
  }
  else
  {
    mConcentrationTime = new ReosParameterDuration( tr( "Concentration time" ), false, this );
    mArea = new ReosParameterArea( tr( "Area" ), false, this );
    connect( mConcentrationTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
    connect( mArea, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  }
}

ReosParameterDuration *ReosTransferFunction::concentrationTime() const
{
  return mConcentrationTime;
}

ReosParameterArea *ReosTransferFunction::area() const
{
  return mArea;
}

ReosTransferFunctionLinearReservoir::ReosTransferFunctionLinearReservoir( ReosWatershed *parent ):
  ReosTransferFunction( parent )
  , mLagTime( new ReosParameterDuration( tr( "Lag time" ), false, this ) )
  , mUseConcentrationTime( new ReosParameterBoolean( tr( "Use concentration time" ), false, this ) )
  , mFactorToLagTime( new ReosParameterDouble( tr( "Factor" ), false, this ) )
{
  mUseConcentrationTime->setValue( true );

  mFactorToLagTime->setValue( 0.6 );
  mLagTime->setValue( concentrationTime()->value() * mFactorToLagTime->value() );

  connect( mLagTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mUseConcentrationTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mFactorToLagTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
}

ReosHydrograph *ReosTransferFunctionLinearReservoir::applyFunction( ReosRunoff *runoff, QObject *hydrographParent ) const
{
  if ( !runoff )
    return nullptr;

  ReosDuration _lagTime;

  if ( mUseConcentrationTime->value() )
    _lagTime = concentrationTime()->value() * mFactorToLagTime->value();
  else
    _lagTime = mLagTime->value();

  if ( _lagTime < ReosDuration( qint64( 0 ) ) )
    return nullptr;

  std::unique_ptr<ReosHydrograph> hydrograph = std::make_unique<ReosHydrograph>( hydrographParent );

  double adt = runoff->timeStep() / _lagTime;
  double wsArea = area()->value().valueM2();
  double dt = runoff->timeStep().valueSecond();
  double qPrev = 0;

  hydrograph->referenceTime()->setValue( runoff->data()->referenceTime()->value() );
  hydrograph->setValue( ReosDuration(), 0 );

  for ( int i = 0; i < runoff->valueCount(); ++i )
  {
    double intensity = runoff->incrementalValue( i ) / dt;
    double q = qPrev * exp( -adt ) + intensity * ( 1 - exp( -adt ) ) / 1000 * wsArea;
    hydrograph->setValue( runoff->timeStep() * ( i + 1 ), q );
    qPrev = q;
  }

  double lastQ = qPrev;
  int i = runoff->valueCount() + 1;
  while ( qPrev != 0 && qPrev > lastQ / 100 )
  {
    hydrograph->setValue( runoff->timeStep() * ( i ),  qPrev * exp( -adt ) );
    qPrev = qPrev * exp( -adt );
    ++i;
  }

  return hydrograph.release();
}

ReosParameterDouble *ReosTransferFunctionLinearReservoir::factorToLagTime() const
{
  return mFactorToLagTime;
}

ReosParameterBoolean *ReosTransferFunctionLinearReservoir::useConcentrationTime() const
{
  return mUseConcentrationTime;
}

ReosParameterDuration *ReosTransferFunctionLinearReservoir::lagTime() const
{
  return mLagTime;
}

QColor ReosHydrograph::color() const
{
  return Qt::red;
}

ReosHydrograph *ReosTransferFunctionGeneralizedRationalMethod::applyFunction( ReosRunoff *runoff, QObject *hydrographParent ) const
{
  return nullptr;
}

ReosTransferFunctionFactories *ReosTransferFunctionFactories::sInstance = nullptr;

void ReosTransferFunctionFactories::instantiate( ReosModule *parent )
{
  if ( !sInstance )
    sInstance = new ReosTransferFunctionFactories( parent );
}

bool ReosTransferFunctionFactories::isInstantiate()
{
  return sInstance != nullptr;
}

ReosTransferFunctionFactories *ReosTransferFunctionFactories::instance()
{
  if ( !sInstance )
    sInstance = new ReosTransferFunctionFactories;

  return sInstance;
}


ReosTransferFunction *ReosTransferFunctionFactories::createTransferFunction( const QString &type, ReosWatershed *watershed )
{
  for ( const Factory &fact : mFactories )
    if ( fact->type() == type )
      return fact->createTransferFunction( watershed );

  return nullptr;
}

void ReosTransferFunctionFactories::addFactory( ReosTransferFunctionFactory *factory )
{
  for ( const Factory &fact : mFactories )
    if ( fact->type() == factory->type() )
      return;

  mFactories.emplace_back( factory );
}

QString ReosTransferFunctionFactories::type( int i )
{
  return mFactories.at( i )->type();
}

int ReosTransferFunctionFactories::factoryCount() const
{
  return mFactories.size();
}

int ReosTransferFunctionFactories::index( const QString &type ) const
{
  for ( size_t i = 0; i < mFactories.size(); ++i )
    if ( mFactories.at( i )->type() == type )
      return i;

  return -1;
}

QAbstractListModel *ReosTransferFunctionFactories::listModel() const
{
  return mModel;
}

ReosTransferFunctionFactories::ReosTransferFunctionFactories( ReosModule *parent ):
  ReosModule( parent ),
  mModel( new ReosTransferFunctionFactoriesModel( mFactories, this ) )
{}

ReosTransferFunctionFactoriesModel::ReosTransferFunctionFactoriesModel( std::vector<std::unique_ptr<ReosTransferFunctionFactory> > &factories, QObject *parent ):
  QAbstractListModel( parent ),
  mFactories( factories )
{

}

QModelIndex ReosTransferFunctionFactoriesModel::index( int row, int column, const QModelIndex & ) const
{
  if ( row < 0 )
    return QModelIndex();

  if ( row < static_cast<int>( mFactories.size() ) )
    return createIndex( row, column, mFactories.at( row ).get() );
  else
    return QModelIndex();
}

QModelIndex ReosTransferFunctionFactoriesModel::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosTransferFunctionFactoriesModel::rowCount( const QModelIndex & ) const
{
  return mFactories.size();
}

int ReosTransferFunctionFactoriesModel::columnCount( const QModelIndex & ) const
{
  return 1;
}

QVariant ReosTransferFunctionFactoriesModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QModelIndex();

  if ( index.row() >= static_cast<int>( mFactories.size() ) )
    return QModelIndex();

  switch ( role )
  {
    case Qt::DisplayRole:
      return mFactories.at( index.row() )->displayText();
      break;
    default:
      break;
  }

  return QModelIndex();
}
