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

void ReosTransferFunction::encodeBase( ReosEncodedElement &element ) const
{
  if ( !mWatershed )
  {
    element.addEncodedData( QStringLiteral( "concentration-time" ), mConcentrationTime->encode() );
    element.addEncodedData( QStringLiteral( "area" ), mArea->encode() );
  }
}

ReosTransferFunction::ReosTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed ):
  ReosDataObject( watershed )
{
  if ( watershed )
  {
    mConcentrationTime = watershed->concentrationTime();
    mArea = watershed->area();
    connect( watershed, &ReosWatershed::changed, this, &ReosDataObject::dataChanged );
  }
  else
  {
    mConcentrationTime = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "concentration-time" ) ), false, this );
    mArea = ReosParameterArea::decode( element.getEncodedData( QStringLiteral( "area" ) ), false, this );
    connect( mConcentrationTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
    connect( mArea, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  }
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

ReosEncodedElement ReosTransferFunctionLinearReservoir::encode() const
{
  ReosEncodedElement element( type() );
  encodeBase( element );

  element.addEncodedData( QStringLiteral( "lag-time" ), mLagTime->encode() );
  element.addEncodedData( QStringLiteral( "use-concentration-time" ), mUseConcentrationTime->encode() );
  element.addEncodedData( QStringLiteral( "factor-to-lag-time" ), mFactorToLagTime->encode() );

  return element;
}

ReosTransferFunctionLinearReservoir *ReosTransferFunctionLinearReservoir::decode( const ReosEncodedElement &element, ReosWatershed *watershed )
{
  if ( element.description() != QStringLiteral( "transfer-function-linear-reservoir" ) )
    return nullptr;

  return new ReosTransferFunctionLinearReservoir( element, watershed );
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

ReosTransferFunctionLinearReservoir::ReosTransferFunctionLinearReservoir( const ReosEncodedElement &element, ReosWatershed *watershed ):
  ReosTransferFunction( element, watershed )
{
  mLagTime = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "lag-time" ) ), false, this );
  mUseConcentrationTime = ReosParameterBoolean::decode( element.getEncodedData( QStringLiteral( "use-concentration-time" ) ), false, this );
  mFactorToLagTime = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "factor-to-lag-time" ) ), false, this );

  connect( mLagTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mUseConcentrationTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mFactorToLagTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
}

QColor ReosHydrograph::color() const
{
  return Qt::red;
}

ReosTransferFunctionGeneralizedRationalMethod::ReosTransferFunctionGeneralizedRationalMethod( ReosWatershed *watershed ):
  ReosTransferFunction( watershed )
{}

ReosHydrograph *ReosTransferFunctionGeneralizedRationalMethod::applyFunction( ReosRunoff *runoff, QObject *hydrographParent ) const
{
  if ( !runoff || !runoff->data() )
    return nullptr;

  ReosDuration::Unit timeUnit = ReosDuration::second;

  std::unique_ptr<ReosHydrograph> hydrograph = std::make_unique<ReosHydrograph>( hydrographParent );
  ReosTimeSerieConstantInterval *runoffTimeSerie = runoff->data();

  ReosDuration timeStep = runoffTimeSerie->timeStep()->value();
  ReosDuration concTime = concentrationTime()->value();
  //! Construct the unit hydrograph
  double peakRatio = ( area()->value().valueM2() / ( 1000 * timeStep.valueUnit( timeUnit ) ) ) * std::min( 1.0, timeStep / concTime );
  ReosHydrograph unitHydrograph;
  ReosDuration d1 = timeStep;
  ReosDuration d2 = concTime;
  ReosDuration d3 = timeStep + concTime;
  unitHydrograph.setValue( ReosDuration(), 0 );
  unitHydrograph.setValue( d1, peakRatio );
  unitHydrograph.setValue( d2, peakRatio );
  unitHydrograph.setValue( d3, 0 );

  double *runoffData = runoffTimeSerie->data();
  unitHydrograph.referenceTime()->setValue( runoffTimeSerie->referenceTime()->value() );
  hydrograph->referenceTime()->setValue( runoffTimeSerie->referenceTime()->value() );

  for ( int i = 0; i < runoffTimeSerie->valueCount(); ++i )
  {
    unitHydrograph.referenceTime()->setValue( runoffTimeSerie->referenceTime()->value().addMSecs( timeStep.valueMilliSecond()*i ) );
    hydrograph->addOther( unitHydrograph, runoffData[i] );
  }

  return hydrograph.release();
}

ReosEncodedElement ReosTransferFunctionGeneralizedRationalMethod::encode() const
{
  ReosEncodedElement element( type() );
  encodeBase( element );

  return element;
}

ReosTransferFunction *ReosTransferFunctionGeneralizedRationalMethod::decode( const ReosEncodedElement &element, ReosWatershed *watershed )
{
  if ( element.description() != QStringLiteral( "transfer-function-generalized-rational-method" ) )
    return nullptr;

  return new ReosTransferFunctionGeneralizedRationalMethod( element, watershed );
}

ReosTransferFunctionGeneralizedRationalMethod::ReosTransferFunctionGeneralizedRationalMethod( const ReosEncodedElement &element, ReosWatershed *watershed ):
  ReosTransferFunction( element, watershed )
{}

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

ReosTransferFunction *ReosTransferFunctionFactories::createTransferFunction( const ReosEncodedElement &elem, ReosWatershed *watershed )
{
  for ( const Factory &fac : mFactories )
    if ( fac->type() == elem.description() )
      return fac->createTransferFunction( elem, watershed );

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
    return QVariant();

  if ( index.row() >= static_cast<int>( mFactories.size() ) )
    return QVariant();

  switch ( role )
  {
    case Qt::DisplayRole:
      return mFactories.at( index.row() )->displayText();
      break;
    default:
      break;
  }

  return QVariant();
}

ReosTransferFunction *ReosTransferFunctionGeneralizedRationalMethodFactory::createTransferFunction( ReosWatershed *watershed ) const
{
  return new ReosTransferFunctionGeneralizedRationalMethod( watershed );
}

ReosTransferFunction *ReosTransferFunctionGeneralizedRationalMethodFactory::createTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed ) const
{
  return ReosTransferFunctionGeneralizedRationalMethod::decode( element, watershed );
}

ReosTransferFunction *ReosTransferFunctionLinearReservoirFactory::createTransferFunction( ReosWatershed *watershed ) const
{
  return new ReosTransferFunctionLinearReservoir( watershed );
}

ReosTransferFunction *ReosTransferFunctionLinearReservoirFactory::createTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed ) const
{
  return ReosTransferFunctionLinearReservoir::decode( element, watershed );
}
