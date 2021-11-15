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
#include "reoshydrograph.h"

#include <cmath>

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
    mConcentrationTime = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "concentration-time" ) ), false, tr( "Concentration time" ), this );
    mArea = ReosParameterArea::decode( element.getEncodedData( QStringLiteral( "area" ) ), false, tr( "Area" ), this );
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

ReosTransferFunctionLinearReservoir::~ReosTransferFunctionLinearReservoir() = default;

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

ReosTransferFunctionCalculation *ReosTransferFunctionLinearReservoir::calculationProcess( ReosRunoff *runoff )
{
  if ( !runoff )
    return nullptr;

  ReosDuration lagTime;

  if ( mUseConcentrationTime->value() )
    lagTime = concentrationTime()->value() * mFactorToLagTime->value();
  else
    lagTime = mLagTime->value();

  if ( lagTime < ReosDuration( qint64( 0 ) ) )
    return nullptr;

  return new Calculation( runoff->data()->constData(), lagTime, area()->value(), runoff->timeStep(), runoff->data()->referenceTime()->value() );
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
  mLagTime = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "lag-time" ) ), false, tr( "Lag time" ), this );
  mUseConcentrationTime = ReosParameterBoolean::decode( element.getEncodedData( QStringLiteral( "use-concentration-time" ) ), false, tr( "Use concentration time" ), this );
  mFactorToLagTime = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "factor-to-lag-time" ) ), false, tr( "Factor" ), this );

  connect( mLagTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mUseConcentrationTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mFactorToLagTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
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
    hydrograph->addOther( &unitHydrograph, runoffData[i] );
  }

  return hydrograph.release();
}

ReosEncodedElement ReosTransferFunctionGeneralizedRationalMethod::encode() const
{
  ReosEncodedElement element( type() );
  encodeBase( element );

  return element;
}

ReosTransferFunctionCalculation *ReosTransferFunctionGeneralizedRationalMethod::calculationProcess( ReosRunoff *runoff )
{
  if ( !runoff || !runoff->data() )
    return nullptr;
  return new Calculation( runoff->data()->constData(), concentrationTime()->value(), area()->value(), runoff->timeStep(), runoff->data()->referenceTime()->value() );
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
  return static_cast<int>( mFactories.size() );
}

int ReosTransferFunctionFactories::index( const QString &type ) const
{
  for ( size_t i = 0; i < mFactories.size(); ++i )
    if ( mFactories.at( i )->type() == type )
      return static_cast<int>( i );

  return -1;
}

QAbstractListModel *ReosTransferFunctionFactories::listModel() const
{
  return mModel;
}

QString ReosTransferFunctionFactories::presentationText( const QString &type ) const
{
  for ( const Factory &fact : mFactories )
    if ( fact->type() == type )
      return fact->presentationText();

  return QString();
}

QPixmap ReosTransferFunctionFactories::formulation( const QString &type ) const
{
  for ( const Factory &fact : mFactories )
    if ( fact->type() == type )
      return fact->formulation();

  return QPixmap();
}

QString ReosTransferFunctionFactories::variablesDescription( const QString &type ) const
{
  for ( const Factory &fact : mFactories )
    if ( fact->type() == type )
      return fact->variablesDescription();

  return QString();
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
  return static_cast<int>( mFactories.size() );
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

QString ReosTransferFunctionGeneralizedRationalMethodFactory::presentationText() const
{
  return QObject::tr( "The Generalized Rational Method is a Unit Hydrograph transfer function.<br>"
                      "This Unit Hydrograph is defined as below:" );
}

QPixmap ReosTransferFunctionGeneralizedRationalMethodFactory::formulation() const {return QPixmap( QStringLiteral( ":/formulas/rationalUnitHydrograph.svg" ) );}

QString ReosTransferFunctionGeneralizedRationalMethodFactory::variablesDescription() const
{
  return QObject::tr( "Where:<br>"
                      "- \u0394t : the time step of the runoff<br>"
                      "- t<sub>c</sub> : the concentration time<br>"
                      "- A : the watershed area<br>"
                      "- r : the runoff intensity or the effective rainfall intensity during the time step in mm per unit time" );
}

ReosTransferFunction *ReosTransferFunctionLinearReservoirFactory::createTransferFunction( ReosWatershed *watershed ) const
{
  return new ReosTransferFunctionLinearReservoir( watershed );
}

ReosTransferFunction *ReosTransferFunctionLinearReservoirFactory::createTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed ) const
{
  return ReosTransferFunctionLinearReservoir::decode( element, watershed );
}

QString ReosTransferFunctionLinearReservoirFactory::presentationText() const
{
  return QObject::tr( "The Linear Reservoir Method is expressing the flow rate for the n<sup>th</sup> time step as below:" );
}

QPixmap ReosTransferFunctionLinearReservoirFactory::formulation() const
{
  return QPixmap( QStringLiteral( ":/formulas/linearReservoirHydrograph.svg" ) );
}

QString ReosTransferFunctionLinearReservoirFactory::variablesDescription() const
{
  return QObject::tr( "Where:<br>"
                      "- \u0394t : the time step of the runoff<br>"
                      "- t<sub>l</sub> : the lag time<br>"
                      "- A : the watershed area<br>"
                      "- r : the runoff intensity or the effective rainfall intensity during the time step in mm per unit time<br>"
                      "- Q<sub>0</sub> = 0" );
}



ReosTransferFunctionSCSUnitHydrograph::ReosTransferFunctionSCSUnitHydrograph( ReosWatershed *watershed ):
  ReosTransferFunction( watershed )
  , mPeakRateFactor( new ReosParameterDouble( tr( "Peak Factor" ), false, this ) )
  , mLagTime( new ReosParameterDuration( tr( "Lag time" ), false, this ) )
  , mUseConcentrationTime( new ReosParameterBoolean( tr( "Use concentration time for the lag time" ), false, this ) )
  , mFactorToLagTime( new ReosParameterDouble( tr( "Factor" ), false, this ) )
{
  mUseConcentrationTime->setValue( true );

  mPeakRateFactor->setValue( 484 );
  mFactorToLagTime->setValue( 0.6 );
  mLagTime->setValue( concentrationTime()->value() * mFactorToLagTime->value() );

  connect( mPeakRateFactor, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mLagTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mUseConcentrationTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mFactorToLagTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
}


ReosHydrograph *ReosTransferFunctionSCSUnitHydrograph::applyFunction( ReosRunoff *runoff, QObject *parent ) const
{
  if ( !runoff || !runoff->data() )
    return nullptr;

  ReosTimeSerieConstantInterval *runoffTimeSerie = runoff->data();
  QDateTime referenceTime = runoffTimeSerie->referenceTime()->value();
  ReosDuration timeStep = runoffTimeSerie->timeStep()->value();

  // here, we the time step has to be lesser than the peak time, check that and adjust the time step
  ReosDuration peakTime;
  int reduceTimeStepFactor = 1;
  ReosDuration originalTimeStep = timeStep;
  do
  {
    timeStep = originalTimeStep / reduceTimeStepFactor;
    if ( mUseConcentrationTime->value() )
      peakTime = timeStep / 2 + concentrationTime()->value() * mFactorToLagTime->value();
    else
      peakTime = timeStep / 2 + mLagTime->value();

    if ( peakTime <=  timeStep * 2 )
      reduceTimeStepFactor *= 2;
  }
  while ( peakTime <=  timeStep * 2 );

  Calculation calculation( runoffTimeSerie->constData(), reduceTimeStepFactor, timeStep, referenceTime, peakTime, mPeakRateFactor->value(), area()->value() );
  calculation.start();

  return calculation.hydrograph( parent );
}

ReosEncodedElement ReosTransferFunctionSCSUnitHydrograph::encode() const
{
  ReosEncodedElement element( type() );
  encodeBase( element );

  element.addEncodedData( QStringLiteral( "peak-factor" ), mPeakRateFactor->encode() );
  element.addEncodedData( QStringLiteral( "lag-time" ), mLagTime->encode() );
  element.addEncodedData( QStringLiteral( "use-concentration-time" ), mUseConcentrationTime->encode() );
  element.addEncodedData( QStringLiteral( "factor-to-lag-time" ), mFactorToLagTime->encode() );

  return element;
}

ReosTransferFunction *ReosTransferFunctionSCSUnitHydrograph::decode( const ReosEncodedElement &element, ReosWatershed *watershed )
{
  if ( element.description() != QStringLiteral( "transfer-function-scs-unit-hydrograph" ) )
    return nullptr;

  return new ReosTransferFunctionSCSUnitHydrograph( element, watershed );
}

ReosParameterDouble *ReosTransferFunctionSCSUnitHydrograph::peakRateFactor() const
{
  return mPeakRateFactor;
}

ReosParameterDouble *ReosTransferFunctionSCSUnitHydrograph::factorToLagTime() const
{
  return mFactorToLagTime;
}

ReosParameterBoolean *ReosTransferFunctionSCSUnitHydrograph::useConcentrationTime() const
{
  return mUseConcentrationTime;
}

ReosParameterDuration *ReosTransferFunctionSCSUnitHydrograph::lagTime() const
{
  return mLagTime;
}

ReosTransferFunctionCalculation *ReosTransferFunctionSCSUnitHydrograph::calculationProcess( ReosRunoff *runoff )
{
  if ( !runoff || !runoff->data() )
    return nullptr;

  ReosTimeSerieConstantInterval *runoffTimeSerie = runoff->data();
  QDateTime referenceTime = runoffTimeSerie->referenceTime()->value();
  ReosDuration timeStep = runoffTimeSerie->timeStep()->value();

  if ( mUseConcentrationTime->value() )
  {
    if ( !concentrationTime()->isValid() || !mFactorToLagTime->isValid() )
      return nullptr;
  }
  else
  {
    if ( !mLagTime->isValid() )
      return nullptr;
  }

  // here, we the time step has to be lesser than the peak time, check that and adjust the time step
  ReosDuration peakTime;
  int reduceTimeStepFactor = 1;
  ReosDuration originalTimeStep = timeStep;
  int counter = 0; //to prevent infinite loop
  do
  {
    timeStep = originalTimeStep / reduceTimeStepFactor;
    if ( mUseConcentrationTime->value() )
      peakTime = timeStep / 2 + concentrationTime()->value() * mFactorToLagTime->value();
    else
      peakTime = timeStep / 2 + mLagTime->value();

    if ( peakTime <=  timeStep * 2 )
      reduceTimeStepFactor *= 2;

    counter++;
  }
  while ( peakTime <=  timeStep * 2 && counter < 100 );

  return new Calculation( runoffTimeSerie->constData(), reduceTimeStepFactor, timeStep, referenceTime, peakTime, mPeakRateFactor->value(), area()->value() );
}

ReosTransferFunctionSCSUnitHydrograph::ReosTransferFunctionSCSUnitHydrograph( const ReosEncodedElement &element, ReosWatershed *watershed ):
  ReosTransferFunction( element, watershed )
{
  mPeakRateFactor = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "peak-factor" ) ), false, tr( "Peak Factor" ), this );
  mLagTime = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "lag-time" ) ), false, tr( "Lag time" ), this );
  mUseConcentrationTime = ReosParameterBoolean::decode( element.getEncodedData( QStringLiteral( "use-concentration-time" ) ), false, tr( "Use concentration time for the lag time" ), this );
  mFactorToLagTime = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "factor-to-lag-time" ) ), false, tr( "Factor" ), this );

  connect( mPeakRateFactor, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mLagTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mUseConcentrationTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mFactorToLagTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
}

int ReosTransferFunctionSCSUnitHydrograph::indexInTable( double peakRateFactor, bool &exact )
{
  exact = false;
  int rounded = int( peakRateFactor + 0.5 );

  for ( int i = 0; i < sTable.count(); ++i )
  {
    const UH_SCS_dimensionneless &current = sTable.at( i );
    exact = current.peakRateFactor == rounded;
    if ( rounded <= current.peakRateFactor )
      return i;
  }

  return sTable.count();
}

std::unique_ptr<ReosHydrograph> ReosTransferFunctionSCSUnitHydrograph::Calculation::createUnitHydrograph() const
{
  // here we use SI units whereas the peak factor is in US unit, and the runoff is in mm where as in litterature, t is in cm for SI
  double peakFlow = ( mPeakFactor / 484 ) * 0.208 / mPeakTime.valueHour() * mArea.valueKm2();

  UH_SCS_dimensionneless scs1;
  bool exact = false;
  int index = 0;
  if ( int( mPeakFactor + 0.5 ) == 484 ) //use the common dimensionless curve
  {
    exact = true;
    scs1 = {484, 0.1, {
        0, 0.03, 0.1, 0.19, 0.31, 0.47, 0.66, 0.82, 0.93, 0.99, 1, 0.99, 0.93, 0.86, 0.78, 0.68, 0.56, 0.46, 0.39, 0.33, 0.28, 0.2435, 0.207,
        0.177, 0.147, 0.127, 0.107, 0.092, 0.077, 0.066, 0.055, 0.0475, 0.04, 0.0345, 0.029, 0.025, 0.021, 0.018, 0.015, 0.013, 0.011, 0.0098,
        0.0086, 0.0074, 0.0062, 0.005, 0.004, 0.003, 0.002, 0.001, 0
      }
    };
  }
  else
  {
    index = indexInTable( mPeakFactor, exact );
    if ( index == sTable.count() ) //greater than the greater in the table --> take the greater in the table
    {
      index = sTable.count() - 1;
      exact = true;
    }
    if ( index == 0 ) //lesser than the lesser in the table --> take the lesser in the table
      exact = true;

    scs1 = sTable.at( index );
  }

  std::unique_ptr<ReosHydrograph> hydrograph1 = std::make_unique<ReosHydrograph>();
  hydrograph1->referenceTime()->setValue( mReferenceTime );

  double aDimStep1 = scs1.dimensionlessTimeStep;
  for ( int i = 0; i < scs1.dimensionlessRate.count(); i++ )
    hydrograph1->setValue( mPeakTime * i * aDimStep1, scs1.dimensionlessRate.at( i ) );

  if ( exact )
  {
    std::unique_ptr<ReosHydrograph> hydrograph = std::make_unique<ReosHydrograph>();
    hydrograph->referenceTime()->setValue( mReferenceTime );
    hydrograph->addOther( hydrograph1.get(), peakFlow, false );

    return hydrograph;
  }

  std::unique_ptr<ReosHydrograph> hydrograph2 = std::make_unique<ReosHydrograph>();
  hydrograph2->referenceTime()->setValue( mReferenceTime );

  const UH_SCS_dimensionneless &scs2 = sTable.at( index - 1 );
  double aDimStep2 = scs2.dimensionlessTimeStep;
  for ( int i = 0; i < scs2.dimensionlessRate.count(); i++ )
    hydrograph2->setValue( mPeakTime * i * aDimStep2, scs2.dimensionlessRate.at( i ) );

  double f = ( mPeakFactor - scs2.peakRateFactor ) / ( scs1.peakRateFactor - scs2.peakRateFactor );

  std::unique_ptr<ReosHydrograph> hydrograph = std::make_unique<ReosHydrograph>();
  hydrograph->referenceTime()->setValue( mReferenceTime );

  hydrograph->addOther( hydrograph1.get(), f * peakFlow, false );
  hydrograph->addOther( hydrograph2.get(), ( 1 - f )*peakFlow, false );

  return hydrograph;
}


ReosTransferFunction *ReosTransferFunctionSCSUnitHydrographFactory::createTransferFunction( ReosWatershed *watershed ) const
{
  return new ReosTransferFunctionSCSUnitHydrograph( watershed );
}

ReosTransferFunction *ReosTransferFunctionSCSUnitHydrographFactory::createTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed ) const
{
  return ReosTransferFunctionSCSUnitHydrograph::decode( element, watershed );
}

QString ReosTransferFunctionSCSUnitHydrographFactory::presentationText() const
{
  return QObject::tr( "The SCS Unit Hydrograph defines a Unit Hydrograph with predefined hydrograph shapes depending on a peak factor.<br>This peak factor links the peak flow rate and the peak time as below:" );
}

QPixmap ReosTransferFunctionSCSUnitHydrographFactory::formulation() const
{
  return QPixmap( QStringLiteral( ":/formulas/SCSUnitHydrograph.svg" ) );
}

QString ReosTransferFunctionSCSUnitHydrographFactory::variablesDescription() const
{
  return QObject::tr( "Where:<br>"
                      "- Q<sub>p</sub> : the peak flow rate for an effective rainfall height or a runoff of 1 cm<br>"
                      "- T<sub>p</sub> : the time of the peak in hours<br>"
                      "- A : the watershed area in km<sup>2</sup><br>"
                      "- P<sub>f</sub> : the peak factor between 100 and 600 with a typical value of 484<br>"
                      " Peak factor with high value is for watershed with quick reaction (mountain), and lower values are for slower watersheds (plain)." );
}




QVector<ReosTransferFunctionSCSUnitHydrograph::UH_SCS_dimensionneless> ReosTransferFunctionSCSUnitHydrograph::sTable =
{
  {
    100, 0.2, {
      0.0, 0.8142, 0.9228, 0.9722, 0.9941, 1.0, 0.9955, 0.984, 0.9675, 0.9475, 0.925, 0.9007, 0.8753, 0.849, 0.8223, 0.7954,
      0.7685, 0.7417, 0.7153, 0.6893, 0.6637, 0.6387, 0.6143, 0.5905, 0.5674, 0.5449, 0.5231, 0.5019, 0.4815, 0.4618, 0.4427,
      0.4243, 0.4065, 0.3894, 0.3729, 0.3571, 0.3418, 0.3272, 0.3131, 0.2996, 0.2866, 0.2741, 0.2621, 0.2506, 0.2396, 0.229,
      0.2189, 0.2092, 0.1999, 0.191, 0.1825, 0.1743, 0.1665, 0.159, 0.1519, 0.145, 0.1385, 0.1322, 0.1262, 0.1205, 0.115,
      0.1098, 0.1048, 0.1, 0.0954, 0.091, 0.0869, 0.0829, 0.0791, 0.0754, 0.072, 0.0686, 0.0655, 0.0624, 0.0596, 0.0568,
      0.0542, 0.0517, 0.0493, 0.047, 0.0448, 0.0427, 0.0407, 0.0388, 0.037, 0.0353, 0.0336, 0.0321, 0.0306, 0.0291, 0.0278,
      0.0265, 0.0252, 0.024, 0.0229, 0.0218, 0.0208, 0.0198, 0.0189, 0.018, 0.0172, 0.0164, 0.0156, 0.0148, 0.0141, 0.0135,
      0.0128, 0.0122, 0.0117, 0.0111, 0.0106, 0.0101, 0.0096, 0.0091, 0.0087, 0.0083, 0.0079, 0.0075, 0.0072, 0.0068, 0.0065,
      0.0062, 0.0059, 0.0056, 0.0054, 0.0051, 0.0049, 0.0046, 0.0044, 0.0042, 0.004, 0.0038, 0.0036, 0.0035, 0.0033, 0.0031,
      0.003, 0.0028, 0.0027, 0.0026, 0.0025, 0.0023, 0.0022, 0.0021, 0.002, 0.0019, 0.0018, 0.0017, 0.0017, 0.0016, 0.0015, 0.0
    }
  },
  {
    150, 0.2, {
      0.0, 0.6869, 0.8635, 0.9499, 0.9893, 1.0, 0.9918, 0.971, 0.9415, 0.9062, 0.8673, 0.8262, 0.784, 0.7415, 0.6994, 0.6582,
      0.6181, 0.5794, 0.5422, 0.5067, 0.4729, 0.4409, 0.4106, 0.382, 0.3551, 0.3298, 0.3061, 0.2839, 0.2631, 0.2438, 0.2257,
      0.2088, 0.1931, 0.1786, 0.165, 0.1524, 0.1407, 0.1299, 0.1199, 0.1106, 0.102, 0.094, 0.0866, 0.0798, 0.0735, 0.0677,
      0.0623, 0.0574, 0.0528, 0.0486, 0.0447, 0.0411, 0.0378, 0.0348, 0.032, 0.0294, 0.027, 0.0248, 0.0228, 0.0209, 0.0192,
      0.0177, 0.0162, 0.0149, 0.0137, 0.0126, 0.0115, 0.0106, 0.0097, 0.0089, 0.0082, 0.0075, 0.0069, 0.0063, 0.0058, 0.0053,
      0.0049, 0.0045, 0.0041, 0.0037, 0.0034, 0.0031, 0.0029, 0.0026, 0.0024, 0.0022, 0.002, 0.0019, 0.0017, 0.0016, 0.0014,
      0.0013, 0.0012, 0.0011, 0.001, 0.0009, 0.0008, 0.0008, 0.0007, 0.0007, 0.0006, 0.0005, 0.0005, 0.0005, 0.0004, 0.0004,
      0.0004, 0.0003, 0.0003, 0.0003, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001,
      0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0
    }
  },
  {
    200, 0.2, {
      0.0, 0.5489, 0.7911, 0.9212, 0.983, 1.0, 0.987, 0.954, 0.9082, 0.8545, 0.7966, 0.7372, 0.6779, 0.6203, 0.565, 0.5128,
      0.4638, 0.4183, 0.3763, 0.3377, 0.3025, 0.2704, 0.2413, 0.2151, 0.1914, 0.1701, 0.151, 0.1339, 0.1186, 0.105, 0.0928,
      0.082, 0.0724, 0.0638, 0.0563, 0.0496, 0.0437, 0.0384, 0.0338, 0.0297, 0.0261, 0.0229, 0.0201, 0.0176, 0.0155, 0.0136,
      0.0119, 0.0104, 0.0091, 0.008, 0.007, 0.0061, 0.0054, 0.0047, 0.0041, 0.0036, 0.0031, 0.0027, 0.0024, 0.0021, 0.0018,
      0.0016, 0.0014, 0.0012, 0.0011, 0.0009, 0.0008, 0.0007, 0.0006, 0.0005, 0.0005, 0.0004, 0.0004, 0.0003, 0.0003, 0.0002,
      0.0002, 0.0002, 0.0002, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0
    }
  },
  {
    250, 0.2, {
      0.0, 0.4142, 0.7086, 0.8863, 0.9751, 1.0, 0.9809, 0.9332, 0.868, 0.7937, 0.7159, 0.6388, 0.5648, 0.4957, 0.4322, 0.3747,
      0.3233, 0.2778, 0.2378, 0.2028, 0.1725, 0.1463, 0.1238, 0.1045, 0.088, 0.074, 0.0621, 0.0521, 0.0436, 0.0364, 0.0304,
      0.0253, 0.0211, 0.0175, 0.0146, 0.0121, 0.01, 0.0083, 0.0069, 0.0057, 0.0047, 0.0039, 0.0032, 0.0027, 0.0022, 0.0018,
      0.0015, 0.0012, 0.001, 0.0008, 0.0007, 0.0006, 0.0005, 0.0004, 0.0003, 0.0003, 0.0002, 0.0002, 0.0001, 0.0001, 0.0001,
      0.0001, 0.0001, 0.0001, 0.0
    }
  },

  {
    300, 0.2, {
      0.0, 0.2943, 0.6201, 0.8458, 0.9656, 1.0, 0.9736, 0.9085, 0.8217, 0.7257, 0.629, 0.5369, 0.4527, 0.3776, 0.3122, 0.2561,
      0.2087, 0.1691, 0.1363, 0.1093, 0.0873, 0.0695, 0.0551, 0.0436, 0.0343, 0.027, 0.0212, 0.0166, 0.0129, 0.0101, 0.0078,
      0.0061, 0.0047, 0.0037, 0.0028, 0.0022, 0.0017, 0.0013, 0.001, 0.0008, 0.0006, 0.0005, 0.0003, 0.0003, 0.0002, 0.0002,
      0.0001, 0.0001, 0.0001, 0.0001, 0.0
    }
  },

  {
    350, 0.2, {
      0.0, 0.197, 0.53, 0.8006, 0.9546, 1.0, 0.9651, 0.8803, 0.7703, 0.6532, 0.5402, 0.4378, 0.349, 0.2743, 0.2131, 0.1638,
      0.1248, 0.0944, 0.0708, 0.0529, 0.0392, 0.0289, 0.0213, 0.0156, 0.0114, 0.0082, 0.006, 0.0043, 0.0031, 0.0022, 0.0016,
      0.0011, 0.0008, 0.0006, 0.0004, 0.0003, 0.0002, 0.0001, 0.0001, 0.0001, 0.0001, 0.0
    }
  },

  {
    400, 0.1, {
      0.0, 0.027, 0.1244, 0.2732, 0.4429, 0.6081, 0.7517, 0.8642, 0.9421, 0.9863, 1.0, 0.988, 0.9555, 0.9076, 0.8491, 0.7839,
      0.7155, 0.6465, 0.579, 0.5144, 0.4538, 0.3977, 0.3465, 0.3004, 0.2591, 0.2224, 0.1902, 0.162, 0.1376, 0.1164, 0.0982,
      0.0826, 0.0693, 0.0579, 0.0484, 0.0403, 0.0335, 0.0278, 0.023, 0.019, 0.0157, 0.0129, 0.0106, 0.0087, 0.0072, 0.0059,
      0.0048, 0.0039, 0.0032, 0.0026, 0.0021, 0.0017, 0.0014, 0.0011, 0.0009, 0.0007, 0.0006, 0.0005, 0.0004, 0.0003, 0.0003,
      0.0002, 0.0002, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0
    }
  },
  {
    450, 0.1, {
      0.0, 0.011, 0.0739, 0.1975, 0.3614, 0.5371, 0.7, 0.8333, 0.9282, 0.9829, 1.0, 0.985, 0.9447, 0.8859, 0.8151, 0.7377,
      0.6581, 0.5798, 0.5051, 0.4357, 0.3725, 0.3159, 0.266, 0.2224, 0.1849, 0.1528, 0.1257, 0.1029, 0.0838, 0.068, 0.055,
      0.0443, 0.0356, 0.0285, 0.0227, 0.0181, 0.0143, 0.0114, 0.009, 0.0071, 0.0056, 0.0044, 0.0034, 0.0027, 0.0021, 0.0016,
      0.0013, 0.001, 0.0008, 0.0006, 0.0005, 0.0004, 0.0003, 0.0002, 0.0002, 0.0001, 0.0001, 0.0001, 0.0001, 0.0
    }
  },
  {
    500, 0.1, {
      0.0, 0.004, 0.0414, 0.1376, 0.2881, 0.4677, 0.6466, 0.8001, 0.913, 0.9791, 1.0, 0.9817, 0.9328, 0.8623, 0.7788,
      0.6893, 0.5996, 0.5135, 0.4338, 0.3621, 0.2989, 0.2444, 0.198, 0.1591, 0.1269, 0.1006, 0.0792, 0.062, 0.0482, 0.0374,
      0.0288, 0.0221, 0.0169, 0.0129, 0.0098, 0.0074, 0.0056, 0.0042, 0.0031, 0.0023, 0.0017, 0.0013, 0.001, 0.0007, 0.0005,
      0.0004, 0.0003, 0.0002, 0.0002, 0.0001, 0.0001, 0.0001, 0.0
    }
  },

  {
    550, 0.1, {
      0.0, 0.0013, 0.0218, 0.0923, 0.2242, 0.4012, 0.5922, 0.7649, 0.8964, 0.975, 1.0, 0.9781, 0.9198, 0.837, 0.7405,
      0.6396, 0.5408, 0.449, 0.3666, 0.2951, 0.2344, 0.184, 0.1429, 0.1099, 0.0837, 0.0633, 0.0475, 0.0354, 0.0262,
      0.0193, 0.0141, 0.0103, 0.0074, 0.0054, 0.0038, 0.0027, 0.002, 0.0014, 0.001, 0.0007, 0.0005, 0.0003, 0.0002,
      0.0002, 0.0001, 0.0001, 0.0001, 0.0
    }
  },

  {
    600, 0.1, {
      0.0, 0.0004, 0.0108, 0.0596, 0.1703, 0.3392, 0.5378, 0.7282, 0.8785, 0.9704, 1.0, 0.9741, 0.9058, 0.8101, 0.7008,
      0.5891, 0.4831, 0.3875, 0.3049, 0.2358, 0.1795, 0.1348, 0.0999, 0.0732, 0.0531, 0.0381, 0.0271, 0.0191, 0.0134,
      0.0093, 0.0064, 0.0044, 0.003, 0.002, 0.0014, 0.0009, 0.0006, 0.0004, 0.0003, 0.0002, 0.0001, 0.0001, 0.0001, 0.0
    }
  }
};

ReosTransferFunctionSCSUnitHydrograph::Calculation::Calculation( const QVector<double> runoffData, int reduceTimeStepFactor, const ReosDuration &timeStep, const QDateTime &referenceTime, const ReosDuration &peakTime, double peakFactor, const ReosArea &area ):
  mRunoffData( runoffData )
  , mReduceTimeStepFactor( reduceTimeStepFactor )
  , mTimeStep( timeStep )
  , mReferenceTime( referenceTime )
  , mPeakTime( peakTime )
  , mPeakFactor( peakFactor )
  , mArea( area )
{
}

void ReosTransferFunctionSCSUnitHydrograph::Calculation::start()
{
  mHydrograph = std::make_unique<ReosHydrograph>();

  std::unique_ptr<ReosHydrograph> unitHydrograph = createUnitHydrograph();

  mHydrograph->referenceTime()->setValue( mReferenceTime );
  int stepCount = mRunoffData.count();
  setMaxProgression( stepCount );
  for ( int i = 0; i < stepCount; ++i )
  {
    ReosDuration relativeTime = mTimeStep * i;
    unitHydrograph->referenceTime()->setValue( mReferenceTime.addMSecs( relativeTime.valueMilliSecond() ) );
    mHydrograph->addOther( unitHydrograph.get(), mRunoffData.at( i / mReduceTimeStepFactor ) / mReduceTimeStepFactor, false );
    if ( isStop() )
    {
      mHydrograph.reset();
      break;
    }
    setCurrentProgression( i );
  }


}


ReosTransferFunctionLinearReservoir::Calculation::Calculation( const QVector<double> runoffData, ReosDuration lagTime, const ReosArea &area, const ReosDuration &timeStep, const QDateTime &referenceTime ):
  mRunoffData( runoffData )
  , mLagTime( lagTime )
  , mArea( area )
  , mTimeStep( timeStep )
  , mReferenceTime( referenceTime )
{}

void ReosTransferFunctionLinearReservoir::Calculation::start()
{
  mHydrograph = std::make_unique<ReosHydrograph>();

  double adt = mTimeStep / mLagTime;
  double wsArea = mArea.valueM2();
  double dt = mTimeStep.valueSecond();
  double qPrev = 0;

  mHydrograph->referenceTime()->setValue( mReferenceTime );
  mHydrograph->setValue( ReosDuration(), 0 );
  setMaxProgression( mRunoffData.count() );
  for ( int i = 0; i < mRunoffData.count(); ++i )
  {
    double intensity = mRunoffData.at( i ) / dt;
    double q = qPrev * exp( -adt ) + intensity * ( 1 - exp( -adt ) ) / 1000 * wsArea;
    mHydrograph->setValue( mTimeStep * ( i + 1 ), q );
    qPrev = q;

    if ( isStop() )
    {
      mHydrograph.reset();
      break;
    }
    setCurrentProgression( i );
  }

  if ( !isStop() )
  {
    double lastQ = qPrev;
    int i = mRunoffData.count() + 1;
    while ( qPrev != 0 && qPrev > lastQ / 100 )
    {
      mHydrograph->setValue( mTimeStep * ( i ),  qPrev * exp( -adt ) );
      qPrev = qPrev * exp( -adt );
      ++i;
      if ( isStop() )
      {
        mHydrograph.reset();
        break;
      }
    }
  }
}

ReosHydrograph *ReosTransferFunctionCalculation::hydrograph( QObject *parent )
{
  if ( mHydrograph )
    mHydrograph->setParent( parent );
  return mHydrograph.release();
}

ReosTransferFunctionGeneralizedRationalMethod::Calculation::Calculation( const QVector<double> runoffData,
    const ReosDuration &concentrationTime,
    const ReosArea &area,
    const ReosDuration &timeStep,
    const QDateTime &referenceTime ):
  mRunoffData( runoffData )
  , mConcentrationTime( concentrationTime )
  , mArea( area )
  , mTimeStep( timeStep )
  , mReferenceTime( referenceTime )
{}

void ReosTransferFunctionGeneralizedRationalMethod::Calculation::start()
{
  mHydrograph = std::make_unique<ReosHydrograph>();
  ReosDuration::Unit timeUnit = ReosDuration::second;
  //! Construct the unit hydrograph
  double peakRatio = ( mArea.valueM2() / ( 1000 * mTimeStep.valueUnit( timeUnit ) ) ) * std::min( 1.0, mTimeStep / mConcentrationTime );
  ReosHydrograph unitHydrograph;
  ReosDuration d1 = mTimeStep;
  ReosDuration d2 = mConcentrationTime;
  ReosDuration d3 = mTimeStep + mConcentrationTime;
  unitHydrograph.setValue( ReosDuration(), 0 );
  unitHydrograph.setValue( d1, peakRatio );
  unitHydrograph.setValue( d2, peakRatio );
  unitHydrograph.setValue( d3, 0 );


  unitHydrograph.referenceTime()->setValue( mReferenceTime );
  mHydrograph->referenceTime()->setValue( mReferenceTime );
  setMaxProgression( mRunoffData.count() );
  for ( int i = 0; i < mRunoffData.count(); ++i )
  {
    unitHydrograph.referenceTime()->setValue( mReferenceTime.addMSecs( mTimeStep.valueMilliSecond()*i ) );
    mHydrograph->addOther( &unitHydrograph, mRunoffData.at( i ) );
    if ( isStop() )
    {
      mHydrograph.reset();
      break;
    }
    setCurrentProgression( i );
  }
}

ReosTransferFunctionNashUnitHydrograph::ReosTransferFunctionNashUnitHydrograph( ReosWatershed *watershed ):
  ReosTransferFunction( watershed )
  , mKParam( new ReosParameterDuration( tr( "K parameter" ), false, this ) )
  , mNParam( new ReosParameterInteger( tr( "n parameter" ), false, this ) )
  , mUseConcentrationTime( new ReosParameterBoolean( tr( "Use concentration time for K parameter (K=tc/n)" ), false, this ) )
{
  mUseConcentrationTime->setValue( true );

  mNParam->setValue( 3 );
  if ( watershed->concentrationTime()->isValid() )
    mKParam->setValue( watershed->concentrationTime()->value() / mNParam->value() );
  else
    mKParam->setValue( ReosDuration( 10, ReosDuration::minute ) );

  connect( mKParam, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mNParam, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mUseConcentrationTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
}

ReosHydrograph *ReosTransferFunctionNashUnitHydrograph::applyFunction( ReosRunoff *runoff, QObject *parent ) const
{
  if ( !runoff || !runoff->data() )
    return nullptr;

  ReosTimeSerieConstantInterval *runoffTimeSerie = runoff->data();

  ReosDuration K;
  if ( mUseConcentrationTime->value() )
    K = concentrationTime()->value() / mNParam->value();
  else
    K = mKParam->value();

  Calculation calculation( runoffTimeSerie->constData(),
                           runoffTimeSerie->timeStep()->value(),
                           runoffTimeSerie->referenceTime()->value(),
                           K,
                           mNParam->value(),
                           area()->value() );
  calculation.start();

  return calculation.hydrograph( parent );
}

ReosEncodedElement ReosTransferFunctionNashUnitHydrograph::encode() const
{
  ReosEncodedElement element( type() );
  encodeBase( element );

  element.addEncodedData( QStringLiteral( "K-parameter" ), mKParam->encode() );
  element.addEncodedData( QStringLiteral( "n-parameter" ), mNParam->encode() );
  element.addEncodedData( QStringLiteral( "use-concentration-time" ), mUseConcentrationTime->encode() );

  return element;
}

ReosTransferFunction *ReosTransferFunctionNashUnitHydrograph::decode( const ReosEncodedElement &element, ReosWatershed *watershed )
{
  return new ReosTransferFunctionNashUnitHydrograph( element, watershed );
}

ReosParameterDuration *ReosTransferFunctionNashUnitHydrograph::KParam() const
{
  return mKParam;
}

ReosParameterInteger *ReosTransferFunctionNashUnitHydrograph::nParam() const
{
  return mNParam;
}

ReosParameterBoolean *ReosTransferFunctionNashUnitHydrograph::useConcentrationTime() const
{
  return mUseConcentrationTime;
}


ReosTransferFunctionCalculation *ReosTransferFunctionNashUnitHydrograph::calculationProcess( ReosRunoff *runoff )
{
  if ( !runoff || !runoff->data() )
    return nullptr;

  ReosTimeSerieConstantInterval *runoffTimeSerie = runoff->data();
  QDateTime referenceTime = runoffTimeSerie->referenceTime()->value();

  ReosDuration K;
  if ( mUseConcentrationTime->value() )
    K = concentrationTime()->value() / mNParam->value();
  else
    K = mKParam->value();

  return new Calculation( runoffTimeSerie->constData(), runoffTimeSerie->timeStep()->value(), referenceTime, K, mNParam->value(), area()->value() );
}

ReosTransferFunctionNashUnitHydrograph::ReosTransferFunctionNashUnitHydrograph( const ReosEncodedElement &element, ReosWatershed *watershed ):
  ReosTransferFunction( element, watershed )
{
  mKParam = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "K-parameter" ) ), false, tr( "K parameter" ), this );
  mNParam = ReosParameterInteger::decode( element.getEncodedData( QStringLiteral( "n-parameter" ) ), false, tr( "n parameter" ), this );
  mUseConcentrationTime = ReosParameterBoolean::decode( element.getEncodedData( QStringLiteral( "use-concentration-time" ) ), false, tr( "Use concentration time for K parameter (K=tc/n)" ), this );

  connect( mKParam, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mNParam, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
  connect( mUseConcentrationTime, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
}

ReosTransferFunctionNashUnitHydrograph::Calculation::Calculation( const QVector<double> runoffData,
    const ReosDuration &timeStep,
    const QDateTime &referenceTime,
    const ReosDuration KParam,
    int nParam,
    const ReosArea &area ):
  mRunoffData( runoffData )
  , mKParam( KParam )
  , mNParam( nParam )
  , mReferenceTime( referenceTime )
  , mArea( area )
  , mTimeStep( timeStep )
{}

void ReosTransferFunctionNashUnitHydrograph::Calculation::start()
{
  ReosDuration effectiveTimeStep = mTimeStep;
  int timeStepReductionFactor = 1;
  while ( effectiveTimeStep > mKParam / 4 )  //reduce the effective time step to have a smooth hydrograph
  {
    timeStepReductionFactor *= 2;
    effectiveTimeStep = mTimeStep / timeStepReductionFactor;
  }

  std::unique_ptr<ReosHydrograph> unitHydrograph = createUnitHydrograph( effectiveTimeStep );

  mHydrograph = std::make_unique<ReosHydrograph>();


  mHydrograph->referenceTime()->setValue( mReferenceTime );
  int stepCount = mRunoffData.count() * timeStepReductionFactor;
  setMaxProgression( stepCount );
  for ( int i = 0; i < stepCount; ++i )
  {
    ReosDuration relativeTime = effectiveTimeStep * i;
    unitHydrograph->referenceTime()->setValue( mReferenceTime.addMSecs( relativeTime.valueMilliSecond() ) );
    mHydrograph->addOther( unitHydrograph.get(), mRunoffData.at( i / timeStepReductionFactor ) / timeStepReductionFactor );
    if ( isStop() )
    {
      mHydrograph.reset();
      break;
    }
    setCurrentProgression( i );
  }

}

std::unique_ptr<ReosHydrograph> ReosTransferFunctionNashUnitHydrograph::Calculation::createUnitHydrograph( const ReosDuration &timeStep ) const
{
  std::unique_ptr<ReosHydrograph> hydrograph = std::make_unique<ReosHydrograph>();

  hydrograph->referenceTime()->setValue( mReferenceTime );
  double Q = -1;
  double peak = -1;
  ReosDuration t;
  int factorial = 1;
  for ( int i = 1; i <  mNParam ; ++i )
    factorial *= i;
  double firstTerm = 1 / ( mKParam.valueSecond() * factorial );
  do
  {
    double secondTerme = pow( ( t / mKParam ), mNParam - 1 );
    double thirdTerme = std::exp( -( t / mKParam ) );
    Q = firstTerm * secondTerme * thirdTerme * mArea.valueInUnit( ReosArea::m2 ) / 1000;
    if ( Q > peak )
      peak = Q;
    hydrograph->setValue( t, Q );
    t = t + timeStep;
  }
  while ( ( Q == 0  || Q > 0.005  * peak ) && ! isStop() );

  return hydrograph;
}

ReosTransferFunction *ReosTransferFunctionNashUnitHydrographFactory::createTransferFunction( ReosWatershed *watershed ) const
{
  return new ReosTransferFunctionNashUnitHydrograph( watershed );
}

ReosTransferFunction *ReosTransferFunctionNashUnitHydrographFactory::createTransferFunction( const ReosEncodedElement &element, ReosWatershed *watershed ) const
{
  return ReosTransferFunctionNashUnitHydrograph::decode( element, watershed );
}

QString ReosTransferFunctionNashUnitHydrographFactory::presentationText() const
{
  return QObject::tr( "The Nash Unit Hydrograph is a conceptual model represented by an effective rainfall that paths through n different reservoirs "
                      "with the same storage coefficient K that gives reservoirs the propriety of linearity. "
                      "For a rainfall with a depth of 1 mm and a duration tending toward zero, the specific flow rate is given by the following relation:" );
}

QPixmap ReosTransferFunctionNashUnitHydrographFactory::formulation() const
{
  return QPixmap( QStringLiteral( ":/formulas/NashUnitHydrograph.svg" ) );
}

QString ReosTransferFunctionNashUnitHydrographFactory::variablesDescription() const
{
  return QObject::tr( "Where:<br>"
                      "- t : the time from rainfall<br>"
                      "- n : the count of reservoirs <br>"
                      "- K : the storage coefficient in the same unit that t<br>"
                      "- Q : the flow rate (mm per time unit)" );
}
