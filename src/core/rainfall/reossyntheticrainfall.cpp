/***************************************************************************
  reossyntheticrainfall.cpp - ReosSyntheticRainfall

 ---------------------
 begin                : 10.2.2021
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
#include "reossyntheticrainfall.h"

ReosChicagoRainfall::ReosChicagoRainfall( QObject *parent ): ReosUniqueIdfCurveSyntheticRainfall( parent )
{
  connectParameters();
  mCenterCoefficient->setValueWithString( QStringLiteral( "0.5" ) );
  mTotalDuration->setValue( ReosDuration( 60, ReosDuration::minute ) );
}

ReosUniqueIdfCurveSyntheticRainfall::ReosUniqueIdfCurveSyntheticRainfall( QObject *parent ): ReosSerieRainfall( parent )
  , mTotalDuration( new ReosParameterDuration( tr( "Total Duration" ), false, this ) )
  , mCenterCoefficient( new ReosParameterDouble( tr( "Eccentricity" ), false, this ) )
{

}

ReosUniqueIdfCurveSyntheticRainfall::ReosUniqueIdfCurveSyntheticRainfall( const ReosEncodedElement &element, QObject *parent ): ReosSerieRainfall( element, parent )
{
  mTotalDuration = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "total-duration" ) ), false, tr( "Total Duration" ), this );
  mCenterCoefficient = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "eccentry-coefficient" ) ), false, tr( "Eccentricity" ), this );
}


ReosParameterDuration *ReosUniqueIdfCurveSyntheticRainfall::totalDuration()
{
  return mTotalDuration;
}

ReosParameterDouble *ReosUniqueIdfCurveSyntheticRainfall::centerCoefficient()
{
  return mCenterCoefficient;
}


ReosIntensityDurationCurve *ReosUniqueIdfCurveSyntheticRainfall::intensityDurationCurve() const
{
  return mIntensityDurationCurve;
}

void ReosUniqueIdfCurveSyntheticRainfall::setIntensityDurationCurve( ReosIntensityDurationCurve *intensityDurationCurve, const QString &intensityDurationUid )
{
  mIntensityDurationCurve = intensityDurationCurve;
  if ( !intensityDurationUid.isEmpty() )
  {
    mIntensityDurationUid = intensityDurationUid;
    emit newIntensityDuration( intensityDurationUid );
  }
  updateRainfall();
}

void ReosUniqueIdfCurveSyntheticRainfall::setIntensityDurationUid( const QString &uid )
{
  mIntensityDurationUid = uid;
}

void ReosChicagoRainfall::updateRainfall()
{
  if ( mIntensityDurationCurve.isNull() )
    return;

  mValues.clear();

  const ReosDuration ts = timeStep()->value();
  const ReosDuration totalDuration = mTotalDuration->value();
  double eccentricityCoef = mCenterCoefficient->value();
  mValues.append( mIntensityDurationCurve->height( ts, true ) );
  double cumulativeHeight = mValues.last();
  ReosDuration duration = ts;

  unsigned nbLeft = 0;
  unsigned nbRight = 0;

  while ( duration < totalDuration )
  {
    ReosDuration leftDuration = duration * eccentricityCoef;
    ReosDuration rightDuration = duration * ( 1 - eccentricityCoef );
    unsigned newNbLeft = leftDuration.numberOfFullyContainedIntervals( ts );
    unsigned newNbRigth = rightDuration.numberOfFullyContainedIntervals( ts );

    unsigned dnL = newNbLeft - nbLeft;
    unsigned dnR = newNbRigth - nbRight;

    ReosDuration correctedDuration = ts * int( newNbLeft + newNbRigth + 1 );

    double wantedHeight = mIntensityDurationCurve->height( correctedDuration, true );

    double difH = wantedHeight - cumulativeHeight;

    if ( ( dnL + dnR ) != 0 )
    {
      double hInc = difH / ( dnL + dnR );
      double hIncEffectiv = 0;

      if ( dnL != 0 )
      {
        if ( hInc <= mValues.first() )
        {
          mValues.prepend( hInc );
          hIncEffectiv += hInc;
        }
        else
        {
          double hIncEf = mValues.first();
          mValues.prepend( hIncEf );
          hIncEffectiv += hIncEf;
        }
      }

      if ( dnR != 0 )
      {
        if ( hInc <= mValues.last() )
        {
          mValues.append( hInc );
          hIncEffectiv += hInc;
        }
        else
        {
          double hIncEf = mValues.last();
          mValues.append( hIncEf );
          hIncEffectiv += hIncEf;
        }
      }
      cumulativeHeight = cumulativeHeight + hIncEffectiv;
    }

    nbLeft = newNbLeft;
    nbRight = newNbRigth;
    duration = duration + ts;
  }

  //Need correction if values count not corresponding to total duration
  if ( ( ts * mValues.count()  < totalDuration ) )
  {
    double hWanted = mIntensityDurationCurve->height( totalDuration, true );
    double hInc = hWanted - cumulativeHeight;
    mValues.append( hInc );
  }

  emit dataChanged();
}

ReosChicagoRainfall::ReosChicagoRainfall( const ReosEncodedElement &element, QObject *parent ): ReosUniqueIdfCurveSyntheticRainfall( element, parent )
{
  connectParameters();
}

void ReosUniqueIdfCurveSyntheticRainfall::connectParameters()
{
  connect( timeStep(), &ReosParameter::valueChanged, this, &ReosUniqueIdfCurveSyntheticRainfall::updateRainfall );
  connect( referenceTime(), &ReosParameter::valueChanged, this, &ReosUniqueIdfCurveSyntheticRainfall::updateRainfall );
  connect( mTotalDuration, &ReosParameter::valueChanged, this, &ReosUniqueIdfCurveSyntheticRainfall::updateRainfall );
  connect( mCenterCoefficient, &ReosParameter::valueChanged, this, &ReosUniqueIdfCurveSyntheticRainfall::updateRainfall );
}

QString ReosUniqueIdfCurveSyntheticRainfall::intensityDurationUid() const
{
  return mIntensityDurationUid;
}

void ReosUniqueIdfCurveSyntheticRainfall::encodeBase( ReosEncodedElement &element ) const
{
  element.addEncodedData( QStringLiteral( "total-duration" ), mTotalDuration->encode() );
  element.addEncodedData( QStringLiteral( "eccentry-coefficient" ), mCenterCoefficient->encode() );
}

ReosEncodedElement ReosChicagoRainfall::encode() const
{
  ReosEncodedElement element = ReosTimeSerieConstantInterval::encode( QStringLiteral( "chicago-rainfall-data" ) );
  encodeBase( element );
  return element;
}

ReosChicagoRainfall *ReosChicagoRainfall::decode( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "chicago-rainfall-data" ) )
    return nullptr;

  return new ReosChicagoRainfall( element, parent );
}

ReosParameterDuration *ReosDoubleTriangleRainfall::totalDuration()
{
  return mTotalDuration;
}

ReosParameterDuration *ReosDoubleTriangleRainfall::intenseDuration()
{
  return mIntenseDuration;
}

ReosParameterDouble *ReosDoubleTriangleRainfall::centerCoefficient()
{
  return mCenterCoefficient;
}

ReosIntensityDurationCurve *ReosDoubleTriangleRainfall::intensityDurationCurveIntensePeriod() const
{
  if ( mIntensityDurationCurveIntense.isNull() )
    return nullptr;
  return mIntensityDurationCurveIntense;
}

ReosIntensityDurationCurve *ReosDoubleTriangleRainfall::intensityDurationCurveTotal() const
{
  if ( mIntensityDurationCurveTotal.isNull() )
    return nullptr;
  return mIntensityDurationCurveTotal;
}

void ReosDoubleTriangleRainfall::setIntensityDurationCurve( ReosIntensityDurationCurve *intensityDurationCurveIntense,
    ReosIntensityDurationCurve *intensityDurationCurveTotal,
    const QString &intensityDurationUniqueIdIntense,
    const QString &intensityDurationUniqueIdTotal )
{
  mIntensityDurationCurveIntense = intensityDurationCurveIntense;
  mIntensityDurationCurveTotal = intensityDurationCurveTotal;
  if ( !intensityDurationUniqueIdIntense.isEmpty() && !intensityDurationUniqueIdTotal.isEmpty() )
  {
    mIntensityDurationUniqueIdIntense = intensityDurationUniqueIdIntense;
    mIntensityDurationUniqueIdTotal = intensityDurationUniqueIdTotal;
    emit newIntensityDuration( mIntensityDurationUniqueIdIntense, mIntensityDurationUniqueIdTotal );
  }
  updateRainfall();
}

void ReosDoubleTriangleRainfall::setIntensityDurationUniqueId( const QString &intenseUid, const QString &totalUid )
{
  mIntensityDurationUniqueIdIntense = intenseUid;
  mIntensityDurationUniqueIdTotal = totalUid;
}

QString ReosDoubleTriangleRainfall::intensityDurationUniqueIdIntense() const
{
  return mIntensityDurationUniqueIdIntense;
}

QString ReosDoubleTriangleRainfall::intensityDurationUniqueIdTotal() const
{
  return mIntensityDurationUniqueIdTotal;
}

ReosDoubleTriangleRainfall *ReosDoubleTriangleRainfall::decode( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "double-triangle-rainfall-data" ) )
    return nullptr;

  return new ReosDoubleTriangleRainfall( element, parent );
}

void ReosDoubleTriangleRainfall::updateRainfall()
{
  if ( mIntensityDurationCurveIntense.isNull() || mIntensityDurationCurveTotal.isNull() )
    return;

  mValues.clear();

  ReosDuration ts = timeStep()->value();
  ReosDuration totalDuration = mTotalDuration->value();
  ReosDuration intenseDuration = mIntenseDuration->value();
  double eccentricity = mCenterCoefficient->value();

  unsigned nbLowInterLeft = ( ( totalDuration - intenseDuration ) * eccentricity ).numberOfFullyContainedIntervals( ts );
  unsigned nbLowInterRight = ( ( totalDuration - intenseDuration ) * ( 1 - eccentricity ) ).numberOfFullyContainedIntervals( ts );
  unsigned nbInterIntense = intenseDuration.numberOfFullyContainedIntervals( ts );

  ReosDuration correctedIntenseDuration = ts * int( nbInterIntense );
  ReosDuration correctedTotalDuration = ts * int( nbLowInterRight + nbLowInterLeft );


  double intenseHeight = mIntensityDurationCurveIntense->height( correctedIntenseDuration, true );
  double lowHeight = mIntensityDurationCurveTotal->height( correctedIntenseDuration + correctedTotalDuration, true ) - intenseHeight;
  if ( lowHeight < 0 )
    lowHeight = 0;

  double intensitePicPeuIntense = lowHeight / ( correctedTotalDuration ).valueHour() * 2;
  double intensitePicIntense = ( intenseHeight - intensitePicPeuIntense * ( correctedIntenseDuration ).valueHour() ) / correctedIntenseDuration.valueHour() * 2 + intensitePicPeuIntense;


  for ( unsigned i = 1; i <= nbLowInterLeft; ++i )
  {
    double intensite = ( i - 0.5 ) * intensitePicPeuIntense / ( nbLowInterLeft );
    mValues.push_back( intensite * ts.valueHour() );
  }

  unsigned nbIntenseLateral = nbInterIntense / 2;

  for ( unsigned i = 1; i <= nbIntenseLateral; ++i )
  {
    double intensite = ( ts * ( i - 0.5 ) ).valueHour() * ( intensitePicIntense - intensitePicPeuIntense ) / ( correctedIntenseDuration / 2 ).valueHour() + intensitePicPeuIntense;
    mValues.push_back( intensite * ts.valueHour() );
  }

  //si presence intervalle central
  if ( nbInterIntense - 2 * nbIntenseLateral > 0 )
  {
    double intensiteLateral = ( intensitePicIntense - intensitePicPeuIntense ) * double( nbIntenseLateral ) / ( double( nbInterIntense ) / 2 ) + intensitePicPeuIntense;
    double intensiteCentral = ( intensitePicIntense + intensiteLateral ) / 2;
    mValues.push_back( intensiteCentral * ts.valueHour() );
  }

  for ( unsigned i = 1; i <= nbIntenseLateral; ++i )
  {
    double intensite = ( ts * ( nbIntenseLateral - i + 0.5 ) ).valueHour() * ( intensitePicIntense - intensitePicPeuIntense ) / ( correctedIntenseDuration / 2 ).valueHour() + intensitePicPeuIntense;
    mValues.push_back( intensite * ts.valueHour() );
  }

  for ( unsigned i = 1; i <= nbLowInterRight; ++i )
  {
    double intensite = ( nbLowInterRight - i + 0.5 ) * intensitePicPeuIntense / ( nbLowInterRight );
    mValues.push_back( intensite * ts.valueHour() );
  }

  emit dataChanged();

}

ReosEncodedElement ReosDoubleTriangleRainfall::encode() const
{
  ReosEncodedElement element = ReosTimeSerieConstantInterval::encode( QStringLiteral( "double-triangle-rainfall-data" ) );

  element.addEncodedData( QStringLiteral( "intense-duration" ), mIntenseDuration->encode() );
  element.addEncodedData( QStringLiteral( "total-duration" ), mTotalDuration->encode() );
  element.addEncodedData( QStringLiteral( "eccentry-coefficient" ), mCenterCoefficient->encode() );

  return element;
}

ReosDoubleTriangleRainfall::ReosDoubleTriangleRainfall( const ReosEncodedElement &element, QObject *parent ):
  ReosSerieRainfall( element, parent )
{
  mTotalDuration = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "total-duration" ) ), false, tr( "Total Duration" ), this );
  mIntenseDuration = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "intense-duration" ) ), false, tr( "Intense Duration" ), this );
  mCenterCoefficient = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "eccentry-coefficient" ) ), false, tr( "Eccentricity" ), this );
  connectParameters();
}

void ReosDoubleTriangleRainfall::connectParameters()
{
  connect( timeStep(), &ReosParameter::valueChanged, this, &ReosDoubleTriangleRainfall::updateRainfall );
  connect( referenceTime(), &ReosParameter::valueChanged, this, &ReosDoubleTriangleRainfall::updateRainfall );
  connect( mIntenseDuration, &ReosParameter::valueChanged, this, &ReosDoubleTriangleRainfall::updateRainfall );
  connect( mTotalDuration, &ReosParameter::valueChanged, this, &ReosDoubleTriangleRainfall::updateRainfall );
  connect( mCenterCoefficient, &ReosParameter::valueChanged, this, &ReosDoubleTriangleRainfall::updateRainfall );
}

ReosDoubleTriangleRainfall::ReosDoubleTriangleRainfall( QObject *parent ) :
  ReosSerieRainfall( parent )
  , mIntenseDuration( new ReosParameterDuration( tr( "Intense Duration" ), false, this ) )
  , mTotalDuration( new ReosParameterDuration( tr( "Total Duration" ), false, this ) )
  , mCenterCoefficient( new ReosParameterDouble( tr( "Eccentricity" ), false, this ) )
{
  mCenterCoefficient->setValueWithString( QStringLiteral( "0.5" ) );
  mTotalDuration->setValue( ReosDuration( 60, ReosDuration::minute ) );
  mIntenseDuration->setValue( ReosDuration( 15, ReosDuration::minute ) );
  connectParameters();
}

ReosSerieRainfall::ReosSerieRainfall( QObject *parent ): ReosTimeSerieConstantInterval( parent )
{
  setUpdata();
}

ReosEncodedElement ReosSerieRainfall::encode() const
{
  return ReosTimeSerieConstantInterval::encode( QStringLiteral( "serie-rainfall-data" ) );
}

ReosSerieRainfall *ReosSerieRainfall::decode( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "serie-rainfall-data" ) )
    return nullptr;

  return new ReosSerieRainfall( element, parent );
}

ReosSerieRainfall::ReosSerieRainfall( const ReosEncodedElement &element, QObject *parent ):
  ReosTimeSerieConstantInterval( element, parent )
{
  setUpdata();
}

void ReosSerieRainfall::setUpdata()
{
  setValueUnit( tr( "mm" ) );
  setValueModeName( ReosTimeSerieConstantInterval::Value, tr( "Height per time step" ) );
  setValueModeName( ReosTimeSerieConstantInterval::Cumulative, tr( "Total height" ) );
  setValueModeName( ReosTimeSerieConstantInterval::Intensity, tr( "Rainfall intensity" ) );
  setValueModeColor( ReosTimeSerieConstantInterval::Value, QColor( 0, 0, 200, 200 ) );
  setValueModeColor( ReosTimeSerieConstantInterval::Intensity, QColor( 50, 100, 255, 200 ) );
  setValueModeColor( ReosTimeSerieConstantInterval::Cumulative, QColor( 255, 50, 0 ) );
}

ReosAlternatingBlockRainfall::ReosAlternatingBlockRainfall( QObject *parent ): ReosUniqueIdfCurveSyntheticRainfall( parent )
{
  connectParameters();
  mCenterCoefficient->setValueWithString( QStringLiteral( "0.5" ) );
  mTotalDuration->setValue( ReosDuration( 60, ReosDuration::minute ) );
}

ReosEncodedElement ReosAlternatingBlockRainfall::encode() const
{
  ReosEncodedElement element = ReosTimeSerieConstantInterval::encode( QStringLiteral( "alternating-block-data" ) );
  encodeBase( element );
  return element;
}

ReosAlternatingBlockRainfall *ReosAlternatingBlockRainfall::decode( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "alternating-block-data" ) )
    return nullptr;

  return new ReosAlternatingBlockRainfall( element, parent );
}

void ReosAlternatingBlockRainfall::updateRainfall()
{
  if ( mIntensityDurationCurve.isNull() )
    return;

  mValues.clear();

  if ( !timeStep()->isValid() || !totalDuration()->isValid() || totalDuration()->value() < timeStep()->value() )
    return;

  const ReosDuration ts = timeStep()->value();
  const ReosDuration totalDuration = mTotalDuration->value();
  double eccentricityCoef = mCenterCoefficient->value();
  int intervalCount = totalDuration.numberOfFullyContainedIntervals( ts );
  eccentricityCoef = std::clamp( eccentricityCoef, 0.0, 1.0 );

  int peakInterval = ( intervalCount * eccentricityCoef ) ;
  if ( intervalCount > 0 )
    peakInterval = std::min( peakInterval, intervalCount - 1 );
  mValues.resize( intervalCount );

  mValues[peakInterval] = mIntensityDurationCurve->height( ts, true );
  double previousHeight = mValues[peakInterval];
  double cumulHeight = previousHeight;

  ReosDuration cumulDuration = ts;
  int side = eccentricityCoef < 0.5 ? 1 : -1;

  int filledIntervalled = 1;
  int ib = peakInterval;
  int ia = peakInterval;
  while ( filledIntervalled < intervalCount )
  {
    int pos;
    if ( ( side < 0 && ib > 0 ) || ia >= ( intervalCount - 1 ) )
    {
      ib--;
      pos = ib;
    }
    else
    {
      ia++;
      pos = ia;
    }

    filledIntervalled++;
    cumulDuration = ts * ( filledIntervalled );
    double theoricalCumulHeight = mIntensityDurationCurve->height( cumulDuration, true );
    double incrementalHeight = theoricalCumulHeight - cumulHeight;
    incrementalHeight = std::clamp( incrementalHeight, 0.0, previousHeight );

    mValues[pos] = incrementalHeight;

    previousHeight = incrementalHeight;
    cumulHeight += incrementalHeight;
    side = side * -1;
  }

  emit dataChanged();
}

ReosAlternatingBlockRainfall::ReosAlternatingBlockRainfall( const ReosEncodedElement &element, QObject *parent ):
  ReosUniqueIdfCurveSyntheticRainfall( element, parent )
{
  connectParameters();
}
