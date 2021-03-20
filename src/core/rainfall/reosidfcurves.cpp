/***************************************************************************
  reosidfcurves.cpp - ReosIdfCurves

 ---------------------
 begin                : 2.2.2021
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
#include "reosidfcurves.h"

#include <QBrush>

ReosIdfFormulaRegistery *ReosIdfFormulaRegistery::sIdfRegistery = nullptr;

ReosIntensityDurationInterval::ReosIntensityDurationInterval( const ReosDuration &start, const ReosDuration &end, QObject *parent ):
  QObject( parent )
  , mStartDuration( new ReosParameterDuration( tr( "start" ), false, this ) ), mEndDuration( new ReosParameterDuration( tr( "end" ), false, this ) )
{
  mStartDuration->setValue( start );
  mEndDuration->setValue( end );

  connect( mStartDuration, &ReosParameter::valueChanged, this, &ReosIntensityDurationInterval::changed );
  connect( mEndDuration, &ReosParameter::valueChanged, this, &ReosIntensityDurationInterval::changed );
}

ReosIntensityDurationInterval::ReosIntensityDurationInterval( QObject *parent ): QObject( parent ) {}

bool ReosIntensityDurationInterval::isInInterval( const ReosDuration &duration ) const
{
  return duration >= mStartDuration->value() && duration <= mEndDuration->value();
}

ReosIdfParameters *ReosIntensityDurationInterval::parameters( const QString &formulaName ) const
{
  return mParameters.value( formulaName, nullptr );
}

bool ReosIntensityDurationInterval::operator<( const ReosIntensityDurationInterval &other ) const
{
  return mStartDuration->value() < other.mStartDuration->value() &&
         mEndDuration->value() <= other.mStartDuration->value();
}

bool ReosIntensityDurationInterval::operator>( const ReosIntensityDurationInterval &other ) const
{
  return mStartDuration->value() >= other.mEndDuration->value() &&
         mEndDuration->value() > other.mEndDuration->value();
}

bool ReosIntensityDurationInterval::intersect( const ReosIntensityDurationInterval &other ) const
{
  return !( *this < other || *this > other );

}

ReosDuration ReosIntensityDurationInterval::startDuration() const
{
  if ( mStartDuration && mStartDuration->isValid() )
    return mStartDuration->value();
  else
    return ReosDuration();
}

ReosDuration ReosIntensityDurationInterval::endDuration() const
{
  if ( mEndDuration && mEndDuration->isValid() )
    return mEndDuration->value();
  else
    return ReosDuration();
}

void ReosIntensityDurationInterval::addParameters( ReosIdfParameters *parameters )
{
  mParameters[parameters->formulaName] = parameters;
  connect( parameters, &ReosIdfParameters::changed, this, &ReosIntensityDurationInterval::changed );
}

ReosEncodedElement ReosIntensityDurationInterval::encode() const
{
  ReosEncodedElement element( QStringLiteral( "intensity-duration-interval" ) );

  element.addEncodedData( QStringLiteral( "start-duration" ), mStartDuration->encode() );
  element.addEncodedData( QStringLiteral( "end-duration" ), mEndDuration->encode() );

  QList<ReosEncodedElement> encodedParameters;

  for ( const QString &key : mParameters.keys() )
    encodedParameters.append( mParameters.value( key )->encode() );

  element.addListEncodedData( QStringLiteral( "parameters" ), encodedParameters );

  return element;
}

ReosIntensityDurationInterval *ReosIntensityDurationInterval::decode( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "intensity-duration-interval" ) )
    return nullptr;

  std::unique_ptr<ReosIntensityDurationInterval> ret = std::make_unique<ReosIntensityDurationInterval>( parent );
  ret->mStartDuration = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "start-duration" ) ), false, ret.get() );
  ret->mEndDuration = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "end-duration" ) ), false, ret.get() );
  connect( ret->mStartDuration, &ReosParameter::valueChanged, ret.get(), &ReosIntensityDurationInterval::changed );
  connect( ret->mEndDuration, &ReosParameter::valueChanged, ret.get(), &ReosIntensityDurationInterval::changed );

  QList<ReosEncodedElement> encodedParameters = element.getListEncodedData( QStringLiteral( "parameters" ) );

  for ( const ReosEncodedElement &encodedParam : qAsConst( encodedParameters ) )
    ReosIdfParameters::decode( encodedParam, ret.get() );



  return ret.release();
}

ReosIdfFormula *ReosIdfFormulaRegistery::formula( const QString &name )
{
  auto it = mFormulas.find( name );
  if ( it != mFormulas.end() )
    return it->second.get();
  else
    return nullptr;
}

void ReosIdfFormulaRegistery::registerFormula( ReosIdfFormula *formula )
{
  mFormulas[formula->name()] = std::unique_ptr<ReosIdfFormula>( formula ); //issue with MSVC
}

void ReosIdfFormulaRegistery::instantiate( ReosModule *parentModule )
{
  if ( !sIdfRegistery )
    sIdfRegistery = new ReosIdfFormulaRegistery( parentModule );
}

ReosIdfFormulaRegistery *ReosIdfFormulaRegistery::instance()
{
  if ( !sIdfRegistery )
    sIdfRegistery = new ReosIdfFormulaRegistery();

  return sIdfRegistery;
}

bool ReosIdfFormulaRegistery::isInstanciate() {return sIdfRegistery != nullptr;}

QStringList ReosIdfFormulaRegistery::formulasList() const
{
  QStringList ret;
  for ( const auto &f : mFormulas )
    ret.append( f.first );

  return ret;
}

ReosIdfFormulaRegistery::ReosIdfFormulaRegistery( ReosModule *parent ): ReosModule( parent )
{

}

QString ReosIdfFormulaMontana::name() const {return QStringLiteral( "Montana" );}

double ReosIdfFormulaMontana::height( const ReosDuration &duration, ReosIdfParameters *parameters ) const
{
  if ( parameters->parametersCount() != 2 )
    return -1;

  double a = parameters->parameter( 0 )->value();
  double b = parameters->parameter( 1 )->value();
  double t = duration.valueUnit( parameters->parameterTimeUnit );

  return a * pow( t, 1 - b );
}

QStringList ReosIdfFormulaMontana::parametersNames() const
{
  QStringList param;
  param.append( QObject::tr( "a" ) );
  param.append( QObject::tr( "b" ) );

  return param;
}

ReosIntensityDurationCurve::ReosIntensityDurationCurve( const ReosDuration &returnPeriod, QObject *parent ):
  ReosDataObject( parent )
  , mReturnPeriod( new ReosParameterDuration( tr( "Return period" ), this ) )
{
  mReturnPeriod->setValue( returnPeriod );
  connect( mReturnPeriod, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
}

ReosIntensityDurationCurve::ReosIntensityDurationCurve( QObject *parent ):
  ReosDataObject( parent )
{}

ReosParameterDuration *ReosIntensityDurationCurve::returnPeriod() const {return mReturnPeriod;}

void ReosIntensityDurationCurve::setCurrentFormula( const QString &formulaName )
{
  mCurrentFormulaName = formulaName;
}

void ReosIntensityDurationCurve::setupFormula( ReosIdfFormulaRegistery *registery )
{
  mCurrentFormula = registery->formula( mCurrentFormulaName );
  emit dataChanged();
}

double ReosIntensityDurationCurve::height( const ReosDuration &duration, bool interpolationAllowed ) const
{
  if ( !mCurrentFormula )
    return -1;

  if ( mIntensityDurationIntervals.isEmpty() )
    return -1;

  const ReosIntensityDurationInterval *inter = interval( duration );

  if ( inter )
  {
    ReosIdfParameters *parameters = inter->parameters( mCurrentFormulaName );

    if ( !parameters )
      return -1;

    return mCurrentFormula->height( duration, parameters );
  }

  if ( !interpolationAllowed )
    return -1;

  ReosIntensityDurationInterval *interBefore = nullptr;
  ReosIntensityDurationInterval *interAfter = nullptr;
  for ( ReosIntensityDurationInterval *i : mIntensityDurationIntervals )
  {
    if ( i->endDuration() < duration )
      interBefore = i;
    if ( i->startDuration() > duration && !interAfter )
      interAfter = i;

    if ( interAfter && interBefore )
      break;
  }

  double returnValue = 0;
  int c = 0;
  if ( interBefore )
  {
    ReosIdfParameters *parametersBefore = interBefore->parameters( mCurrentFormulaName );
    returnValue += mCurrentFormula->height( duration, parametersBefore );
    c++;
  }

  if ( interAfter )
  {
    ReosIdfParameters *parametersAfter = interAfter->parameters( mCurrentFormulaName );
    returnValue += mCurrentFormula->height( duration, parametersAfter );
    c++;
  }

  if ( c > 0 )
    returnValue /= c;

  return returnValue;
}

bool ReosIntensityDurationCurve::addInterval( const ReosDuration &start, const ReosDuration &end )
{
  int pos = 0;
  std::unique_ptr<ReosIntensityDurationInterval> newInterval =
    std::make_unique<ReosIntensityDurationInterval>( start, end, this );

  for ( ; pos < mIntensityDurationIntervals.count(); ++pos )
  {
    if ( newInterval->intersect( *mIntensityDurationIntervals.at( pos ) ) )
      return false;

    if ( ( *newInterval ) < ( *mIntensityDurationIntervals.at( pos ) ) )
      break;
  }

  connect( newInterval.get(), &ReosIntensityDurationInterval::changed, this, &ReosDataObject::dataChanged );

  emit intervalWillBeAdded( pos );
  mIntensityDurationIntervals.insert( pos, newInterval.release() );
  emit intervalAdded();
  emit dataChanged();
  return true;
}

bool ReosIntensityDurationCurve::setIntervalValue( int i, const ReosDuration &start, const ReosDuration &end )
{
  if ( i >= mIntensityDurationIntervals.count() )
    return false;

  std::unique_ptr<ReosIntensityDurationInterval> newInterval =
    std::make_unique<ReosIntensityDurationInterval>( start, end, this );

  if ( i > 0 && newInterval->intersect( *mIntensityDurationIntervals.at( i - 1 ) ) )
    return false;

  if ( i < mIntensityDurationIntervals.count() - 1  && newInterval->intersect( *mIntensityDurationIntervals.at( i + 1 ) ) )
    return false;


  mIntensityDurationIntervals.at( i )->start()->setValue( start );
  mIntensityDurationIntervals.at( i )->end()->setValue( end );
  emit dataChanged();
  return true;

}

void ReosIntensityDurationCurve::removeInterval( int i )
{
  emit intervalWillBeRemoved( i );
  mIntensityDurationIntervals.at( i )->deleteLater();
  mIntensityDurationIntervals.erase( mIntensityDurationIntervals.begin() + i );
  emit intervalRemoved();
  emit dataChanged();
}

QPair<ReosDuration, ReosDuration> ReosIntensityDurationCurve::timeInterval( int i ) const
{
  if ( i < 0 || i >= mIntensityDurationIntervals.count() )
    return QPair<ReosDuration, ReosDuration>();

  return {mIntensityDurationIntervals.at( i )->startDuration(), mIntensityDurationIntervals.at( i )->endDuration() };
}

double ReosIntensityDurationCurve::firstHeight( int intervalIndex ) const
{
  if ( intervalIndex < 0 || intervalIndex >= mIntensityDurationIntervals.count() )
    return - 1.0;
  ReosIntensityDurationInterval *inter = mIntensityDurationIntervals.at( intervalIndex );
  ReosIdfParameters *parameters = inter->parameters( mCurrentFormulaName );

  if ( !parameters )
    return -1;

  return mCurrentFormula->height( inter->startDuration(), parameters );
}

double ReosIntensityDurationCurve::lastHeight( int intervalIndex ) const
{
  if ( intervalIndex < 0 || intervalIndex >= mIntensityDurationIntervals.count() )
    return - 1.0;
  ReosIntensityDurationInterval *inter = mIntensityDurationIntervals.at( intervalIndex );
  ReosIdfParameters *parameters = inter->parameters( mCurrentFormulaName );

  if ( !parameters )
    return -1;

  return mCurrentFormula->height( inter->endDuration(), parameters );
}

double ReosIntensityDurationCurve::intensity( const ReosDuration &duration, ReosDuration::Unit unit ) const
{
  double h = height( duration );

  if ( h >= 0 )
    return h / duration.valueUnit( unit );
  else
    return -1.0;
}

double ReosIntensityDurationCurve::firstIntensity( int intervalIndex, ReosDuration::Unit unit ) const
{
  double h = firstHeight( intervalIndex );

  if ( h >= 0 )
  {
    ReosDuration d = timeInterval( intervalIndex ).first;
    return h / d.valueUnit( unit );
  }
  else
    return -1.0;
}

double ReosIntensityDurationCurve::lastIntensity( int intervalIndex, ReosDuration::Unit unit ) const
{
  double h = lastHeight( intervalIndex );

  if ( h >= 0 )
  {
    ReosDuration d = timeInterval( intervalIndex ).second;
    return h / d.valueUnit( unit );
  }
  else
    return -1.0;
}

ReosIdfParameters *ReosIntensityDurationCurve::createParameters( int i, ReosIdfFormula *formula )
{
  if ( i < 0 || i > mIntensityDurationIntervals.count() )
    return nullptr;

  return formula->createParameters( mIntensityDurationIntervals.at( i ) );
}

ReosIdfParameters *ReosIntensityDurationCurve::currentParameters( int i )
{
  if ( !mCurrentFormula )
    return nullptr;

  ReosIdfParameters *param = mIntensityDurationIntervals.at( i )->parameters( mCurrentFormulaName );
  if ( param )
    return param;
  else
    return createParameters( i, mCurrentFormula );

  return mIntensityDurationIntervals.at( i )->parameters( mCurrentFormulaName );
}

bool ReosIntensityDurationCurve::intervalIsValid( int i )
{
  return currentParameters( i ) && currentParameters( i )->isValid();
}

QRectF ReosIntensityDurationCurve::extent( ReosDuration::Unit timeUnit ) const
{
  if ( mIntensityDurationIntervals.isEmpty() )
    return QRectF();

  double xMin =  mIntensityDurationIntervals.first()->startDuration().valueUnit( timeUnit );
  double xMax =  mIntensityDurationIntervals.last()->endDuration().valueUnit( timeUnit );

  double yMin = std::numeric_limits<double>::max();
  double yMax = -std::numeric_limits<double>::max();
  for ( int i = 0; i < intervalCount(); ++i )
  {
    double y1 = firstIntensity( i );
    double y2 = lastIntensity( i );

    if ( yMin > y1 )
      yMin = y1;
    if ( yMin > y2 )
      yMin = y2;
    if ( yMax < y1 )
      yMax = y1;
    if ( yMax < y2 )
      yMax = y2;
  }
  return QRectF( QPointF( xMin, yMin ), QPointF( xMax, yMax ) );
}

ReosEncodedElement ReosIntensityDurationCurve::encode() const
{
  ReosEncodedElement element( QStringLiteral( "intensity-duration-curve" ) );

  QList<ReosEncodedElement> encodedIntervals;
  for ( ReosIntensityDurationInterval *inter : mIntensityDurationIntervals )
    encodedIntervals.append( inter->encode() );
  element.addListEncodedData( QStringLiteral( "intervals" ), encodedIntervals );
  element.addEncodedData( QStringLiteral( "return-period" ), mReturnPeriod->encode() );
  element.addData( QStringLiteral( "current-formula" ), mCurrentFormulaName );

  return element;
}

ReosIntensityDurationCurve *ReosIntensityDurationCurve::decode( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "intensity-duration-curve" ) )
    return nullptr;

  std::unique_ptr<ReosIntensityDurationCurve> ret = std::make_unique<ReosIntensityDurationCurve>( parent );
  element.getData( QStringLiteral( "current-formula" ), ret->mCurrentFormulaName );

  QList<ReosEncodedElement> encodedIntervals = element.getListEncodedData( QStringLiteral( "intervals" ) );

  if ( ret->mReturnPeriod )
    ret->mReturnPeriod->deleteLater();

  ret->mReturnPeriod = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "return-period" ) ), false, ret.get() );
  connect( ret->mReturnPeriod, &ReosParameter::valueChanged, ret.get(), &ReosDataObject::dataChanged );

  for ( const ReosEncodedElement &elem : qAsConst( encodedIntervals ) )
  {
    ReosIntensityDurationInterval *inter = ReosIntensityDurationInterval::decode( elem, ret.get() );
    if ( inter )
    {
      ret->mIntensityDurationIntervals.append( inter );
      connect( inter, &ReosIntensityDurationInterval::changed, ret.get(), &ReosDataObject::dataChanged );
    }
  }
  return ret.release();
}

const ReosIntensityDurationInterval *ReosIntensityDurationCurve::interval( const ReosDuration &duration ) const
{
  if ( mLastDurationPos >= mIntensityDurationIntervals.count() )
    mLastDurationPos = 0;

  if ( mIntensityDurationIntervals[mLastDurationPos]->isInInterval( duration ) )
    return mIntensityDurationIntervals[mLastDurationPos];

  // try with the next one if exists
  if ( mLastDurationPos < mIntensityDurationIntervals.count() - 1 )
  {
    mLastDurationPos++;
    if ( mIntensityDurationIntervals[mLastDurationPos]->isInInterval( duration ) )
      return mIntensityDurationIntervals[mLastDurationPos];
  }

  // still nothing, parse all intervals
  for ( mLastDurationPos = 0; mLastDurationPos < mIntensityDurationIntervals.count(); ++mLastDurationPos )
  {
    if ( mIntensityDurationIntervals[mLastDurationPos]->isInInterval( duration ) )
      return mIntensityDurationIntervals[mLastDurationPos];
  }

  // still nothing...
  return nullptr;
}

ReosIdfParameters::ReosIdfParameters( ReosIntensityDurationInterval *interval, const QString &formulaName, const QStringList parameterNames ):
  QObject( interval )
  , formulaName( formulaName )
{
  mParameters.resize( parameterNames.size() );
  parameterTimeUnit = ReosDuration::minute;
  interval->addParameters( this );
  connect( this, &ReosIdfParameters::changed, interval, &ReosIntensityDurationInterval::changed );

  for ( int i = 0; i < parameterNames.count(); ++i )
  {
    mParameters[i] = new ReosParameterDouble( parameterNames.at( i ), this );
    connect( mParameters[i], &ReosParameter::valueChanged, this, &ReosIdfParameters::changed );
  }
}

ReosEncodedElement ReosIdfParameters::encode() const
{
  ReosEncodedElement element( QStringLiteral( "idf-parameters" ) );

  element.addData( QStringLiteral( "formulaName" ), formulaName );
  element.addEncodedData( QStringLiteral( "parameters-time-unit" ), ReosDuration( 0, parameterTimeUnit ).encode() );

  QList<ReosEncodedElement> encodedParameters;
  for ( ReosParameterDouble *param : mParameters )
  {
    if ( param )
      encodedParameters.append( param->encode() );
  }

  element.addListEncodedData( QStringLiteral( "parameters" ), encodedParameters );

  return element;
}

void ReosIdfParameters::decode( const ReosEncodedElement &element, ReosIntensityDurationInterval *interval )
{
  if ( element.description() != QStringLiteral( "idf-parameters" ) )
    return;

  QString formulaName;
  element.getData( QStringLiteral( "formulaName" ), formulaName );
  ReosIdfParameters *ret = new ReosIdfParameters( interval, formulaName, QStringList() );

  ret->parameterTimeUnit = ReosDuration::decode( element.getEncodedData( "parameters-time-unit" ) ).unit();
  QList<ReosEncodedElement> encodedParameters = element.getListEncodedData( QStringLiteral( "parameters" ) );
  for ( const ReosEncodedElement &elem : qAsConst( encodedParameters ) )
  {
    ret->mParameters.append( ReosParameterDouble::decode( elem, false, ret ) );
    connect( ret->mParameters.last(), &ReosParameter::valueChanged, ret, &ReosIdfParameters::changed );
  }
}

int ReosIdfParameters::parametersCount()
{
  return mParameters.count();
}

ReosParameterDouble *ReosIdfParameters::parameter( int i )
{
  return mParameters.at( i );
}

bool ReosIdfParameters::isValid() const
{
  bool valid = !mParameters.isEmpty();
  for ( ReosParameterDouble *p : qAsConst( mParameters ) )
    valid &= p->isValid();

  return valid;
}

ReosIdfFormula::~ReosIdfFormula() = default;

ReosIdfParameters *ReosIdfFormula::createParameters( ReosIntensityDurationInterval *interval ) const
{
  return new ReosIdfParameters( interval, name(), parametersNames() );
}

QString ReosIdfFormulaSherman::name() const {return QStringLiteral( "Sherman" );}

double ReosIdfFormulaSherman::height( const ReosDuration &duration, ReosIdfParameters *parameters ) const
{
  if ( parameters->parametersCount() != 3 )
    return -1;

  double a = parameters->parameter( 0 )->value();
  double c = parameters->parameter( 1 )->value();
  double n = parameters->parameter( 2 )->value();
  double t = duration.valueUnit( parameters->parameterTimeUnit );

  return t * a / ( std::pow( ( t + c ), n ) );
}

QStringList ReosIdfFormulaSherman::parametersNames() const
{
  QStringList param;
  param.append( QObject::tr( "a" ) );
  param.append( QObject::tr( "c" ) );
  param.append( QObject::tr( "n" ) );

  return param;
}

ReosIntensityDurationCurveTableModel::ReosIntensityDurationCurveTableModel( ReosIntensityDurationCurve *curve, QObject *parent ):
  QAbstractTableModel( parent )
  , mCurve( curve )
{
  mCurve->setupFormula( ReosIdfFormulaRegistery::instance() );
  connect( mCurve, &ReosIntensityDurationCurve::intervalWillBeAdded, this, &ReosIntensityDurationCurveTableModel::onIntervalWillBeAdded );
  connect( mCurve, &ReosIntensityDurationCurve::intervalAdded, this, &ReosIntensityDurationCurveTableModel::onIntervalAdded );
  connect( mCurve, &ReosIntensityDurationCurve::intervalWillBeRemoved, this, &ReosIntensityDurationCurveTableModel::onIntervalWillBeRemoved );
  connect( mCurve, &ReosIntensityDurationCurve::intervalRemoved, this, &ReosIntensityDurationCurveTableModel::onIntervaleRemoved );
}

QModelIndex ReosIntensityDurationCurveTableModel::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

int ReosIntensityDurationCurveTableModel::rowCount( const QModelIndex & ) const
{
  return mCurve->intervalCount() + 1;
}

int ReosIntensityDurationCurveTableModel::columnCount( const QModelIndex & ) const
{
  ReosIdfFormula *formula = ReosIdfFormulaRegistery::instance()->formula( mCurve->currentFormula() );
  if ( formula )
    return formula->parametersNames().count();
  else
    return 0;
}

QVariant ReosIntensityDurationCurveTableModel::data( const QModelIndex &index, int role ) const
{
  if ( index.row() > mCurve->intervalCount() )
    return QVariant();

  if ( index.row() == mCurve->intervalCount() )
  {
    if ( role == Qt::BackgroundRole )
    {
      QBrush brush( Qt::lightGray );
      return brush;
    }

    return QVariant();
  }

  if ( role == Qt::DisplayRole || role == Qt::EditRole )
  {
    ReosIdfParameters *param = mCurve->currentParameters( index.row() );
    if ( !param )
      return QVariant();
    int c = index.column();
    if ( c >= param->parametersCount() )
      return QVariant();

    return param->parameter( c )->toString();
  }
  return QVariant();
}

bool ReosIntensityDurationCurveTableModel::setData( const QModelIndex &index, const QVariant &value, int role )
{
  if ( !mCurve )
    return false;
  if ( index.row() >= mCurve->intervalCount() )
    return false;

  if ( role == Qt::EditRole )
  {
    ReosIdfParameters *param = mCurve->currentParameters( index.row() );
    if ( !param )
      return false;
    int c = index.column();
    if ( c >= param->parametersCount() )
      return false;

    QString str = value.toString();
    return param->parameter( c )->setValueWithString( str );
  }

  return false;
}

Qt::ItemFlags ReosIntensityDurationCurveTableModel::flags( const QModelIndex &index ) const
{
  return QAbstractTableModel::flags( index ) | Qt::ItemIsEditable;
}

QVariant ReosIntensityDurationCurveTableModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
  if ( orientation == Qt::Vertical )
  {
    if ( role == Qt::DisplayRole && section < mCurve->intervalCount() )
    {
      return mCurve->timeInterval( section ).first.toString( 0 ) + QStringLiteral( " - " ) + mCurve->timeInterval( section ).second.toString( 0 );
    }
    if ( role == Qt::DecorationRole && section == mCurve->intervalCount() )
    {
      return QPixmap( QStringLiteral( ":/images/add.svg" ) );
    }

    return QVariant();
  }

  const QStringList parameterNames = ReosIdfFormulaRegistery::instance()->formula( mCurve->currentFormula() )->parametersNames();
  if ( role == Qt::DisplayRole )
    return parameterNames.at( section );

  return QVariant();
}

ReosIntensityDurationCurve *ReosIntensityDurationCurveTableModel::curve() const
{
  return mCurve;
}

void ReosIntensityDurationCurveTableModel::setCurrentFormula( const QString &formulaName )
{
  beginResetModel();
  mCurve->setCurrentFormula( formulaName );
  mCurve->setupFormula( ReosIdfFormulaRegistery::instance() );
  endResetModel();
}

void ReosIntensityDurationCurveTableModel::onIntervalWillBeAdded( int pos )
{
  beginInsertRows( QModelIndex(), pos, pos );
}

void ReosIntensityDurationCurveTableModel::onIntervalAdded()
{
  endInsertRows();
}

void ReosIntensityDurationCurveTableModel::onIntervalWillBeRemoved( int pos )
{
  beginRemoveRows( QModelIndex(), pos, pos );
}

void ReosIntensityDurationCurveTableModel::onIntervaleRemoved()
{
  endRemoveRows();
}

void ReosIntensityDurationFrequencyCurves::addCurve( ReosIntensityDurationCurve *curve, QString name )
{
  mCurves.append( curve );
  mNames.append( name );
}

int ReosIntensityDurationFrequencyCurves::curvesCount()
{
  return mCurves.count();
}

ReosIntensityDurationCurve *ReosIntensityDurationFrequencyCurves::curve( int i )
{
  if ( i < 0 || i >= mCurves.count() )
    return nullptr;

  if ( mCurves.at( i ).isNull() )
    return nullptr;

  return mCurves.at( i );
}

QString ReosIntensityDurationFrequencyCurves::name( int i )
{
  if ( i < 0 || i >= mNames.count() )
    return QString();

  return mNames.at( i );
}

QRectF ReosIntensityDurationFrequencyCurves::fullExtent() const
{
  QRectF ret;
  for ( int i = 0; i < mCurves.count(); ++i )
  {
    if ( mCurves.at( i ).isNull() )
      continue;

    if ( ret.isEmpty() )
      ret = mCurves.at( i )->extent( mTimeUnit );
    else
      ret = ret.united( mCurves.at( i )->extent( mTimeUnit ) );
  }

  return ret;
}
