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

ReosUniqueIdfCurveSyntheticRainfall::ReosUniqueIdfCurveSyntheticRainfall( QObject *parent ): ReosSeriesRainfall( parent )
  , mTotalDuration( new ReosParameterDuration( tr( "Total Duration" ), false, this ) )
  , mCenterCoefficient( new ReosParameterDouble( tr( "Eccentricity" ), false, this ) )
{

}

ReosUniqueIdfCurveSyntheticRainfall::ReosUniqueIdfCurveSyntheticRainfall( const ReosEncodedElement &element, QObject *parent ): ReosSeriesRainfall( element, parent )
{
  mTotalDuration = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "total-duration" ) ), false, tr( "Total Duration" ), this );
  mCenterCoefficient = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "eccentry-coefficient" ) ), false, tr( "Eccentricity" ), this );
}


ReosParameterDuration *ReosUniqueIdfCurveSyntheticRainfall::totalDuration() const
{
  return mTotalDuration;
}

ReosParameterDouble *ReosUniqueIdfCurveSyntheticRainfall::centerCoefficient() const
{
  return mCenterCoefficient;
}


ReosIntensityDurationCurve *ReosUniqueIdfCurveSyntheticRainfall::intensityDurationCurve() const
{
  return mIntensityDurationCurve;
}

void ReosUniqueIdfCurveSyntheticRainfall::setIntensityDurationCurve( ReosIntensityDurationCurve *intensityDurationCurve, const QString &intensityDurationUid )
{
  if ( !mIntensityDurationCurve.isNull() )
    deregisterUpstreamData( mIntensityDurationCurve );

  mIntensityDurationCurve = intensityDurationCurve;

  registerUpstreamData( mIntensityDurationCurve );
  setObsolete();

  if ( !intensityDurationUid.isEmpty() )
  {
    mIntensityDurationUid = intensityDurationUid;
    emit newIntensityDuration( intensityDurationUid );
  }
}

void ReosUniqueIdfCurveSyntheticRainfall::setIntensityDurationUid( const QString &uid )
{
  mIntensityDurationUid = uid;
}

void ReosChicagoRainfall::updateRainfall() const
{
  if ( mIntensityDurationCurve.isNull() )
  {
    setActualized();
    emit dataChanged();
    return;
  }

  ReosTimeSerieConstantTimeStepProvider *data = constantTimeStepDataProvider();

  data->clear();

  const ReosDuration ts = timeStepParameter()->value();
  if ( ReosDuration() == ts )
  {
    setActualized();
    emit dataChanged();
    return;
  }
  const ReosDuration totalDuration = mTotalDuration->value();
  double eccentricityCoef = mCenterCoefficient->value();
  data->appendValue( mIntensityDurationCurve->height( ts, true ) );
  double cumulativeHeight = data->lastValue();
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

    double difH = std::max( 0.0, wantedHeight - cumulativeHeight );

    if ( ( dnL + dnR ) != 0 )
    {
      double hInc = difH / ( dnL + dnR );
      double hIncEffectiv = 0;

      if ( dnL != 0 )
      {
        if ( hInc <= data->firstValue() )
        {
          data->prependValue( hInc );
          hIncEffectiv += hInc;
        }
        else
        {
          double hIncEf = data->firstValue();
          data->prependValue( hIncEf );
          hIncEffectiv += hIncEf;
        }
      }

      if ( dnR != 0 )
      {
        if ( hInc <= data->lastValue() )
        {
          data->appendValue( hInc );
          hIncEffectiv += hInc;
        }
        else
        {
          double hIncEf = data->lastValue();
          data->appendValue( hIncEf );
          hIncEffectiv += hIncEf;
        }
      }
      cumulativeHeight = cumulativeHeight + hIncEffectiv;
    }

    nbLeft = newNbLeft;
    nbRight = newNbRigth;
    duration = duration + ts;
  }

  //Need correction if values count not corresponding to total duration (but we can't have the last value greater than the before last value
  if ( ( ts * data->valueCount()  < totalDuration ) )
  {
    double hWanted = mIntensityDurationCurve->height( totalDuration, true );
    double hInc = hWanted - cumulativeHeight;
    if ( data->valueCount() > 0 && hInc > data->lastValue() )
      hInc = data->lastValue();
    data->appendValue( hInc );
  }

  setActualized();
  emit dataChanged();
}

ReosChicagoRainfall::ReosChicagoRainfall( const ReosEncodedElement &element, QObject *parent ): ReosUniqueIdfCurveSyntheticRainfall( element, parent )
{
  connectParameters();
}

void ReosUniqueIdfCurveSyntheticRainfall::connectParameters()
{
  connect( timeStepParameter(), &ReosParameter::valueChanged, this, &ReosUniqueIdfCurveSyntheticRainfall::setObsolete );
  connect( mTotalDuration, &ReosParameter::valueChanged, this, &ReosUniqueIdfCurveSyntheticRainfall::setObsolete );
  connect( mCenterCoefficient, &ReosParameter::valueChanged, this, &ReosUniqueIdfCurveSyntheticRainfall::setObsolete );

  connect( mTotalDuration, &ReosParameter::valueChanged, this, &ReosUniqueIdfCurveSyntheticRainfall::dataChanged );
  connect( mCenterCoefficient, &ReosParameter::valueChanged, this, &ReosUniqueIdfCurveSyntheticRainfall::dataChanged );
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

void ReosUniqueIdfCurveSyntheticRainfall::updateData() const
{
  if ( isObsolete() )
    updateRainfall();
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
  if ( !mIntensityDurationCurveIntense.isNull() )
    deregisterUpstreamData( mIntensityDurationCurveIntense );

  if ( !mIntensityDurationCurveTotal.isNull() )
    deregisterUpstreamData( mIntensityDurationCurveTotal );

  mIntensityDurationCurveIntense = intensityDurationCurveIntense;
  mIntensityDurationCurveTotal = intensityDurationCurveTotal;

  registerUpstreamData( mIntensityDurationCurveIntense );
  registerUpstreamData( mIntensityDurationCurveTotal );

  setObsolete();

  if ( !intensityDurationUniqueIdIntense.isEmpty() && !intensityDurationUniqueIdTotal.isEmpty() )
  {
    mIntensityDurationUniqueIdIntense = intensityDurationUniqueIdIntense;
    mIntensityDurationUniqueIdTotal = intensityDurationUniqueIdTotal;
    emit newIntensityDuration( mIntensityDurationUniqueIdIntense, mIntensityDurationUniqueIdTotal );
  }
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

void ReosDoubleTriangleRainfall::updateRainfall() const
{
  if ( mIntensityDurationCurveIntense.isNull() || mIntensityDurationCurveTotal.isNull() )
    return;

  ReosTimeSerieConstantTimeStepProvider *data = constantTimeStepDataProvider();

  data->clear();

  ReosDuration ts = timeStepParameter()->value();
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
    data->appendValue( intensite * ts.valueHour() );
  }

  unsigned nbIntenseLateral = nbInterIntense / 2;

  for ( unsigned i = 1; i <= nbIntenseLateral; ++i )
  {
    double intensite = ( ts * ( i - 0.5 ) ).valueHour() * ( intensitePicIntense - intensitePicPeuIntense ) / ( correctedIntenseDuration / 2 ).valueHour() + intensitePicPeuIntense;
    data->appendValue( intensite * ts.valueHour() );
  }

  //si presence intervalle central
  if ( nbInterIntense - 2 * nbIntenseLateral > 0 )
  {
    double intensiteLateral = ( intensitePicIntense - intensitePicPeuIntense ) * double( nbIntenseLateral ) / ( double( nbInterIntense ) / 2 ) + intensitePicPeuIntense;
    double intensiteCentral = ( intensitePicIntense + intensiteLateral ) / 2;
    data->appendValue( intensiteCentral * ts.valueHour() );
  }

  for ( unsigned i = 1; i <= nbIntenseLateral; ++i )
  {
    double intensite = ( ts * ( nbIntenseLateral - i + 0.5 ) ).valueHour() * ( intensitePicIntense - intensitePicPeuIntense ) / ( correctedIntenseDuration / 2 ).valueHour() + intensitePicPeuIntense;
    data->appendValue( intensite * ts.valueHour() );
  }

  for ( unsigned i = 1; i <= nbLowInterRight; ++i )
  {
    double intensite = ( nbLowInterRight - i + 0.5 ) * intensitePicPeuIntense / ( nbLowInterRight );
    data->appendValue( intensite * ts.valueHour() );
  }

  setActualized();
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
  ReosSeriesRainfall( element, parent )
{
  mTotalDuration = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "total-duration" ) ), false, tr( "Total Duration" ), this );
  mIntenseDuration = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "intense-duration" ) ), false, tr( "Intense Duration" ), this );
  mCenterCoefficient = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "eccentry-coefficient" ) ), false, tr( "Eccentricity" ), this );
  connectParameters();
}

void ReosDoubleTriangleRainfall::updateData() const
{
  if ( isObsolete() )
    updateRainfall();
}

void ReosDoubleTriangleRainfall::connectParameters()
{
  connect( timeStepParameter(), &ReosParameter::valueChanged, this, &ReosDoubleTriangleRainfall::setObsolete );
  connect( mIntenseDuration, &ReosParameter::valueChanged, this, &ReosDoubleTriangleRainfall::setObsolete );
  connect( mTotalDuration, &ReosParameter::valueChanged, this, &ReosDoubleTriangleRainfall::setObsolete );
  connect( mCenterCoefficient, &ReosParameter::valueChanged, this, &ReosDoubleTriangleRainfall::setObsolete );

  connect( mIntenseDuration, &ReosParameter::valueChanged, this, &ReosDoubleTriangleRainfall::dataChanged );
  connect( mTotalDuration, &ReosParameter::valueChanged, this, &ReosDoubleTriangleRainfall::dataChanged );
  connect( mCenterCoefficient, &ReosParameter::valueChanged, this, &ReosDoubleTriangleRainfall::dataChanged );
}

ReosDoubleTriangleRainfall::ReosDoubleTriangleRainfall( QObject *parent ) :
  ReosSeriesRainfall( parent )
  , mIntenseDuration( new ReosParameterDuration( tr( "Intense Duration" ), false, this ) )
  , mTotalDuration( new ReosParameterDuration( tr( "Total Duration" ), false, this ) )
  , mCenterCoefficient( new ReosParameterDouble( tr( "Eccentricity" ), false, this ) )
{
  mCenterCoefficient->setValueWithString( QStringLiteral( "0.5" ) );
  mTotalDuration->setValue( ReosDuration( 60, ReosDuration::minute ) );
  mIntenseDuration->setValue( ReosDuration( 15, ReosDuration::minute ) );
  setObsolete();
  connectParameters();
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

void ReosAlternatingBlockRainfall::updateRainfall() const
{
  if ( mIntensityDurationCurve.isNull() )
    return;

  ReosTimeSerieConstantTimeStepProvider *data = constantTimeStepDataProvider();

  data->clear();

  if ( !timeStepParameter()->isValid() || !totalDuration()->isValid() || totalDuration()->value() < timeStepParameter()->value() )
  {
    setActualized();
    emit dataChanged();
    return;
  }

  const ReosDuration ts = timeStepParameter()->value();
  const ReosDuration totalDuration = mTotalDuration->value();
  double eccentricityCoef = mCenterCoefficient->value();
  int intervalCount = totalDuration.numberOfFullyContainedIntervals( ts );
  if ( intervalCount == 0 )
  {
    setActualized();
    emit dataChanged();
    return;
  }


  eccentricityCoef = std::clamp( eccentricityCoef, 0.0, 1.0 );

  int peakInterval = ( intervalCount * eccentricityCoef ) ;
  peakInterval = std::min( peakInterval, intervalCount - 1 );
  data->resize( intervalCount );

  data->setValue( peakInterval, mIntensityDurationCurve->height( ts, true ) );
  double previousHeight = data->value( peakInterval );
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

    data->setValue( pos, incrementalHeight );

    previousHeight = incrementalHeight;
    cumulHeight += incrementalHeight;
    side = side * -1;
  }

  setActualized();
  emit dataChanged();
}

ReosAlternatingBlockRainfall::ReosAlternatingBlockRainfall( const ReosEncodedElement &element, QObject *parent ):
  ReosUniqueIdfCurveSyntheticRainfall( element, parent )
{
  connectParameters();
}
