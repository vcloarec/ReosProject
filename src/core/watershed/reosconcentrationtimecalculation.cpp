/***************************************************************************
  reosconcentrationtimecalculation.cpp - ReosConcentrationTimeCalculation

 ---------------------
 begin                : 13.2.2021
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
#include "reosconcentrationtimecalculation.h"


ReosDuration ReosConcentrationTimeCalculation::concentrationTime( const ReosConcentrationTimeFormula::Parameters &parameters )
{
  ReosConcentrationTimeFormulasRegistery *registery = ReosConcentrationTimeFormulasRegistery::instance();

  if ( !registery )
  {
    return ReosDuration();
  }

  if ( mUsedMethod == UserChoosenFormula )
  {
    ReosConcentrationTimeFormula *formula = registery->formula( mUserChoosenFormula );

    if ( formula && formula->canBeCalculated( parameters ) )
    {
      ReosDuration time = formula->concentrationTime( parameters );
      time.setAdaptedUnit();
      return time;
    }
    else
      return ReosDuration();
  }

  // calculate result for each active and valid formula
  QVector<ReosDuration> results;

  for ( const QString &formulaName : qAsConst( mActiveFormulas ) )
  {
    ReosConcentrationTimeFormula *formula = registery->formula( formulaName );
    if ( formula && formula->canBeCalculated( parameters ) )
    {
      ReosDuration time = formula->concentrationTime( parameters );
      time.setAdaptedUnit();
      results.append( time );
    }
  }

  if ( results.isEmpty() )
    return ReosDuration();

  switch ( mUsedMethod )
  {
    case ReosConcentrationTimeCalculation::Maximum:
    {
      ReosDuration max = results.at( 0 );
      for ( const ReosDuration &d : qAsConst( results ) )
      {
        if ( max < d )
          max = d;
      }
      return max;
    }
    break;
    case ReosConcentrationTimeCalculation::Minimum:
    {
      ReosDuration min = results.at( 0 );
      for ( const ReosDuration &d : qAsConst( results ) )
      {
        if ( min > d )
          min = d;
      }
      return min;
    }
    break;
    case ReosConcentrationTimeCalculation::Average:
    {
      ReosDuration sum;
      for ( const ReosDuration &d : qAsConst( results ) )
      {
        sum = sum + d;
      }
      sum.setUnit( results.first().unit() );
      return sum / results.count();
    }
    break;
    case ReosConcentrationTimeCalculation::UserChoosenFormula:
      //! already treated
      break;
  }

  return ReosDuration();
}

void ReosConcentrationTimeCalculation::setActiveFormula( const QStringList &formulaNames )
{
  mActiveFormulas = formulaNames;
}

QStringList ReosConcentrationTimeCalculation::activeFormulas() const
{
  return mActiveFormulas;
}

ReosConcentrationTimeCalculation::UsedMethod ReosConcentrationTimeCalculation::usedMethod() const
{
  return mUsedMethod;
}

void ReosConcentrationTimeCalculation::setUsedMethod( const UsedMethod &returnMethod )
{
  mUsedMethod = returnMethod;
}

QString ReosConcentrationTimeCalculation::userChoosenFormula() const
{
  return mUserChoosenFormula;
}

void ReosConcentrationTimeCalculation::setUserChoosenFormula( const QString &userDefinedFormula )
{
  mUserChoosenFormula = userDefinedFormula;
}

ReosEncodedElement ReosConcentrationTimeCalculation::encode() const
{
  ReosEncodedElement element( QStringLiteral( "concentration-time-calculation" ) );

  element.addData( QStringLiteral( "active-formulas" ), mActiveFormulas );
  element.addData( QStringLiteral( "used-method" ), mUsedMethod );
  element.addData( QStringLiteral( "used-choosen-formula" ), mUserChoosenFormula );
  int ac = mAlreadyCalculated ? 1 : 0;
  element.addData( QStringLiteral( "already-calculated" ), ac );

  return element;
}

ReosConcentrationTimeCalculation ReosConcentrationTimeCalculation::decode( const ReosEncodedElement &element )
{
  if ( element.description() != QStringLiteral( "concentration-time-calculation" ) )
    return ReosConcentrationTimeCalculation();

  ReosConcentrationTimeCalculation ret;

  element.getData( QStringLiteral( "active-formulas" ), ret.mActiveFormulas );
  int um;
  if ( element.getData( QStringLiteral( "used-method" ), um ) )
    ret.mUsedMethod = static_cast<UsedMethod>( um );
  element.getData( QStringLiteral( "used-choosen-formula" ), ret.mUserChoosenFormula );
  int ac = 0;
  element.getData( QStringLiteral( "already-calculated" ), ac );
  ret.mAlreadyCalculated = ( ac == 1 );

  return ret;
}

bool ReosConcentrationTimeCalculation::alreadyCalculated() const
{
  return mAlreadyCalculated;
}

void ReosConcentrationTimeCalculation::setAlreadyCalculated( bool alreadyCalculated )
{
  mAlreadyCalculated |= alreadyCalculated;
}

ReosConcentrationTimeFormula::~ReosConcentrationTimeFormula() = default;

ReosConcentrationTimeFormulasRegistery *ReosConcentrationTimeFormulasRegistery::sInstance = nullptr;

ReosConcentrationTimeFormulasRegistery::ReosConcentrationTimeFormulasRegistery()
{
}

ReosConcentrationTimeFormula *ReosConcentrationTimeFormulasRegistery::formula( const QString &name ) const
{
  std::map<QString, std::unique_ptr<ReosConcentrationTimeFormula>>::const_iterator it = mFormulas.find( name );
  if ( it != mFormulas.end() )
    return it->second.get();
  else
    return nullptr;
}

QStringList ReosConcentrationTimeFormulasRegistery::formulasList() const
{
  QStringList keys;
  for ( const auto &it : mFormulas )
    keys.append( it.first );

  return keys;
}

int ReosConcentrationTimeFormulasRegistery::formulasCount() const
{
  return mFormulas.size();
}

bool ReosConcentrationTimeFormulasRegistery::isInstantiate()
{
  return sInstance != nullptr;
}

ReosConcentrationTimeFormulasRegistery *ReosConcentrationTimeFormulasRegistery::instance()
{
  if ( !sInstance )
    sInstance = new ReosConcentrationTimeFormulasRegistery();

  return sInstance;
}

void ReosConcentrationTimeFormulasRegistery::registerFormulas( ReosConcentrationTimeFormula *formula )
{
  mFormulas[formula->name()] = std::unique_ptr<ReosConcentrationTimeFormula>( formula );
}

ReosDuration ReosConcentrationTimeFormulaKirpich::concentrationTime( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double S = parameters.slope;
  double L = parameters.length;
  if ( L > 0 && S > 0 )
  {
    double valueInMinutes = 0.0195 * pow( L, 0.77 ) * pow( S, -0.385 );
    return ReosDuration( valueInMinutes, ReosDuration::minute );
  }
  return ReosDuration();
}

bool ReosConcentrationTimeFormulaKirpich::isInValidityDomain( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double S = parameters.slope;
  double A = parameters.area.valueKm2();
  return ( S >= 0.03 ) && ( S <= 0.1 ) && ( A >= 0.004 ) && ( A <= 0.453 );
}

bool ReosConcentrationTimeFormulaKirpich::canBeCalculated( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double S = parameters.slope;
  double L = parameters.length;
  return L > 0 && S > 0 ;

}

ReosDuration ReosConcentrationTimeFormulaPassini::concentrationTime( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double S = parameters.slope;
  double A = parameters.area.valueKm2();
  double L = parameters.length / 1000;
  if ( A > 0 && L > 0 && S > 0 )
  {
    double valueInHour = 0.108 * pow( A * L, 0.333 ) * pow( S, -0.5 );
    return ReosDuration( valueInHour, ReosDuration::hour );
  }

  return ReosDuration();
}

bool ReosConcentrationTimeFormulaPassini::isInValidityDomain( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double S = parameters.slope;
  double A = parameters.area.valueKm2();
  double L = parameters.length;
  return ( A > 0 && L > 0 && S > 0 );

}

bool ReosConcentrationTimeFormulaPassini::canBeCalculated( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double S = parameters.slope;
  double A = parameters.area.valueInUnit();
  double L = parameters.length;
  return ( A > 0 && L > 0 && S > 0 );
}

ReosDuration ReosConcentrationTimeFormulaVentura::concentrationTime( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double A = parameters.area.valueKm2();
  double L = parameters.length;
  double H = parameters.drop;
  if ( A > 0 && L > 0 && H > 0 )
  {
    double valueInHour = 0.127 * sqrt( A ) * sqrt( L ) / sqrt( H );
    return ReosDuration( valueInHour, ReosDuration::hour );
  }

  return ReosDuration();
}

bool ReosConcentrationTimeFormulaVentura::isInValidityDomain( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double A = parameters.area.valueKm2();
  double L = parameters.length;
  double H = parameters.drop;
  return ( A > 0 && L > 0 && H > 0 );
}

bool ReosConcentrationTimeFormulaVentura::canBeCalculated( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double A = parameters.area.valueInUnit();
  double L = parameters.length;
  double H = parameters.drop;
  return ( A > 0 && L > 0 && H > 0 );
}

ReosDuration ReosConcentrationTimeFormulaTurazza::concentrationTime( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double S = parameters.slope;
  double A = parameters.area.valueKm2();
  double L = parameters.length / 1000;
  if ( A > 0 && L > 0 && S > 0 )
  {
    double valueInHour = 0.108 * pow( A * L, 0.3333333 ) / sqrt( S );
    return ReosDuration( valueInHour, ReosDuration::hour );
  }

  return ReosDuration();
}

bool ReosConcentrationTimeFormulaTurazza::isInValidityDomain( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double S = parameters.slope;
  double A = parameters.area.valueKm2();
  double L = parameters.length / 1000;
  return ( A > 0 && L > 0 && S > 0 );
}

bool ReosConcentrationTimeFormulaTurazza::canBeCalculated( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double S = parameters.slope;
  double A = parameters.area.valueInUnit();
  double L = parameters.length;
  return ( A > 0 && L > 0 && S > 0 );
}

ReosDuration ReosConcentrationTimeFormulaVenTeShow::concentrationTime( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double S = parameters.slope;
  double L = parameters.length;
  if ( L > 0 && S > 0 )
  {
    double valueInHours = 0.1602 * pow( L / 1000, 0.64 ) * pow( S, -0.32 );
    return ReosDuration( valueInHours, ReosDuration::hour );
  }

  return ReosDuration();
}

bool ReosConcentrationTimeFormulaVenTeShow::isInValidityDomain( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double S = parameters.slope;
  double A = parameters.area.valueKm2();
  return ( S >= 0.0051 ) && ( S <= 0.09 ) && ( A >= 0.01 ) && ( A <= 18.5 );
}

bool ReosConcentrationTimeFormulaVenTeShow::canBeCalculated( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double S = parameters.slope;
  double L = parameters.length;
  return ( L > 0 && S > 0 );
}

ReosDuration ReosConcentrationTimeFormulaJohnstone::concentrationTime( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double S = parameters.slope;
  double L = parameters.length;
  if ( L > 0 && S > 0 )
  {
    double valueInHours = 0.4623 * pow( L / 1000, 0.5 ) * pow( S, -0.25 );
    return ReosDuration( valueInHours, ReosDuration::hour );
  }
  return ReosDuration();
}

bool ReosConcentrationTimeFormulaJohnstone::isInValidityDomain( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double A = parameters.area.valueKm2();
  return ( A >= 64.8 ) && ( A <= 4206.1 );
}

bool ReosConcentrationTimeFormulaJohnstone::canBeCalculated( const ReosConcentrationTimeFormula::Parameters &parameters ) const
{
  double S = parameters.slope;
  double L = parameters.length;
  return ( L > 0 && S > 0 );
}

ReosConcentrationTimeFormulasModel::ReosConcentrationTimeFormulasModel( ReosConcentrationTimeFormulasRegistery *registery, QObject *parent ):
  QAbstractTableModel( parent )
  , mRegistery( registery )
{}

QModelIndex ReosConcentrationTimeFormulasModel::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosConcentrationTimeFormulasModel::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosConcentrationTimeFormulasModel::rowCount( const QModelIndex & ) const
{
  if ( !mRegistery )
    return 0;

  return mRegistery->formulasCount();
}

int ReosConcentrationTimeFormulasModel::columnCount( const QModelIndex & ) const
{
  return 2;
}

bool ReosConcentrationTimeFormulasModel::setData( const QModelIndex &index, const QVariant &value, int role )
{
  if ( index.row() >= mRegistery->formulasCount() )
    return false;

  if ( role == Qt::CheckStateRole && index.column() == 0 )
  {
    const QString formulaName = mRegistery->formulasList().at( index.row() );
    if ( value == Qt::CheckState::Checked )
    {
      if ( !mActiveFormulas.contains( formulaName ) )
      {
        mActiveFormulas.append( formulaName );
        emit activeFormulasChanged();
        return true;
      }
    }
    else if ( value == Qt::CheckState::Unchecked )
    {
      if ( mActiveFormulas.contains( formulaName ) )
      {
        mActiveFormulas.removeOne( formulaName );
        emit activeFormulasChanged();
        return true;
      }
    }
  }

  return false;
}

QVariant ReosConcentrationTimeFormulasModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();
  if ( !mRegistery )
    return QVariant();

  if ( index.row() >= mRegistery->formulasCount() )
    return QVariant();

  QString formulaName = mRegistery->formulasList().at( index.row() );

  switch ( role )
  {
    case Qt::DisplayRole:
      if ( index.column() == 0 )
      {
        return mRegistery->formulasList().at( index.row() );
      }

      if ( index.column() == 1 )
      {
        ReosConcentrationTimeFormula *formula = mRegistery->formula( formulaName );
        ReosDuration time = formula->concentrationTime( mParameters );
        if ( time != ReosDuration() )
        {
          time.setAdaptedUnit();
          return time.toString( mCurrentTimeUnit, 2 );
        }
        else
          return QString( '-' );
      }
      break;
    case Qt::ForegroundRole:
      if ( index.column() == 0 &&
           !mRegistery->formula( formulaName )->isInValidityDomain( mParameters ) )
        return QColor( Qt::red );
      else
        return QColor( Qt::black );
      break;
    case Qt::BackgroundRole:
      if ( formulaName == mChoosenFormula && mHighLightChoosenFormula )
        return QColor( 100, 200, 150 );
      break;
    case Qt::CheckStateRole:

      if ( index.column() != 0 )
        return QVariant();

      if ( mActiveFormulas.contains( formulaName ) )
        return Qt::CheckState::Checked;
      else
        return Qt::CheckState::Unchecked;
      break;
    default:
      return QVariant();

  }
  return QVariant();

}

Qt::ItemFlags ReosConcentrationTimeFormulasModel::flags( const QModelIndex &index ) const
{
  if ( index.column() == 0 )
    return QAbstractItemModel::flags( index ) | Qt::ItemIsUserCheckable;
  else
    return QAbstractItemModel::flags( index );
}

void ReosConcentrationTimeFormulasModel::setParameters( ReosConcentrationTimeFormula::Parameters parameters )
{
  mParameters = parameters;
  emit dataChanged( index( 0, 0, QModelIndex() ), index( rowCount( QModelIndex() ), 1, QModelIndex() ) );
}

void ReosConcentrationTimeFormulasModel::setActiveFormulas( const QStringList &formulas )
{
  mActiveFormulas = formulas;
  emit dataChanged( index( 0, 0, QModelIndex() ), index( rowCount( QModelIndex() ), 1, QModelIndex() ) );
}


QStringList ReosConcentrationTimeFormulasModel::activeFormulas() const
{
  return mActiveFormulas;
}

void ReosConcentrationTimeFormulasModel::setChoosenFormula( const QModelIndex &ind )
{
  QStringList formulasList = mRegistery->formulasList();

  if ( ind.isValid() && ind.row() < formulasList.count() )
    mChoosenFormula = formulasList.at( ind.row() );
  else
    mChoosenFormula = QString();

  emit dataChanged( index( 0, 0, QModelIndex() ), index( rowCount( QModelIndex() ), 1, QModelIndex() ) );
}

void ReosConcentrationTimeFormulasModel::setChoosenFormula( const QString &formula )
{
  mChoosenFormula = formula;
}

QString ReosConcentrationTimeFormulasModel::choosenFormula() const
{
  return mChoosenFormula;
}

void ReosConcentrationTimeFormulasModel::highlightChoosenFormula( bool b )
{
  mHighLightChoosenFormula = b;
  emit dataChanged( index( 0, 0, QModelIndex() ), index( rowCount( QModelIndex() ), 1, QModelIndex() ) );
}

ReosConcentrationTimeFormula::Parameters ReosConcentrationTimeFormulasModel::parameters() const
{
  return mParameters;
}

QString ReosConcentrationTimeFormulasModel::textData() const
{
  QStringList formulaList = mRegistery->formulasList();
  QString ret;
  for ( const QString &formulaName : qAsConst( formulaList ) )
  {
    if ( mActiveFormulas.contains( formulaName ) )
    {
      ReosConcentrationTimeFormula *formula = mRegistery->formula( formulaName );
      if ( formula )
      {
        ret.append( formulaName );
        ret.append( QStringLiteral( "\t" ) );
        ret.append( formula->concentrationTime( mParameters ).toString( mCurrentTimeUnit, 2 ) );
        ret.append( QStringLiteral( "\n" ) );
      }
    }
  }

  return ret;
}

void ReosConcentrationTimeFormulasModel::setCurrentTimeUnit( const ReosDuration::Unit &currentTimeUnit )
{
  mCurrentTimeUnit = currentTimeUnit;
  emit dataChanged( index( 0, 0, QModelIndex() ), index( rowCount( QModelIndex() ), 1, QModelIndex() ) );
}

