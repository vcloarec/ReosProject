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

ReosChicagoRainfall::ReosChicagoRainfall( QObject *parent ): ReosTimeSerieConstantInterval( parent ),
  mTotalDuration( new ReosParameterDuration( tr( "Total Duration" ), false, this ) )
  , mCenterCoefficient( new ReosParameterDouble( tr( "Eccentricity" ), false, this ) )
{
  mCenterCoefficient->setValueWithString( QStringLiteral( "0.5" ) );
}

ReosParameterDuration *ReosChicagoRainfall::totalDuration()
{
  return mTotalDuration;
}

ReosParameterDouble *ReosChicagoRainfall::centerCoefficient()
{
  return mCenterCoefficient;
}


ReosIntensityDurationCurve *ReosChicagoRainfall::intensityDurationCurve() const
{
  return mIntensityDurationCurve;
}

void ReosChicagoRainfall::setIntensityDurationCurve( ReosIntensityDurationCurve *intensityDurationCurve, const QString &intensityDurationUri )
{
  mIntensityDurationCurve = intensityDurationCurve;
  if ( !intensityDurationUri.isEmpty() )
  {
    mIntensityDurationUri = intensityDurationUri;
    emit newIntensityDuration( intensityDurationUri );
  }
  updateRainfall();
}

void ReosChicagoRainfall::setIntensityDurationUri( const QString &uri )
{
  mIntensityDurationUri = uri;
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

ReosChicagoRainfall::ReosChicagoRainfall( const ReosEncodedElement &element, QObject *parent ): ReosTimeSerieConstantInterval( element, parent )
{
  mTotalDuration = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "total-duration" ) ), false, this );
  mCenterCoefficient = ReosParameterDouble::decode( element.getEncodedData( QStringLiteral( "eccentry-coefficient" ) ), false, this );
  connectParameters();
}

void ReosChicagoRainfall::connectParameters()
{
  connect( timeStep(), &ReosParameter::valueChanged, this, &ReosChicagoRainfall::updateRainfall );
  connect( referenceTime(), &ReosParameter::valueChanged, this, &ReosChicagoRainfall::updateRainfall );
  connect( mTotalDuration, &ReosParameter::valueChanged, this, &ReosChicagoRainfall::updateRainfall );
  connect( mCenterCoefficient, &ReosParameter::valueChanged, this, &ReosChicagoRainfall::updateRainfall );
}

QString ReosChicagoRainfall::intensityDurationUri() const
{
  return mIntensityDurationUri;
}

ReosEncodedElement ReosChicagoRainfall::encode() const
{
  ReosEncodedElement element = ReosTimeSerieConstantInterval::encode( QStringLiteral( "chicago-rainfall-data" ) );

  element.addEncodedData( QStringLiteral( "total-duration" ), mTotalDuration->encode() );
  element.addEncodedData( QStringLiteral( "eccentry-coefficient" ), mCenterCoefficient->encode() );

  return element;
}

ReosChicagoRainfall *ReosChicagoRainfall::decode( const ReosEncodedElement &element, QObject *parent )
{
  if ( element.description() != QStringLiteral( "chicago-rainfall-data" ) )
    return nullptr;

  return new ReosChicagoRainfall( element, parent );
}
