/***************************************************************************
  reosmuskingumclassicrouting.cpp - ReosMuskingumClassicRouting

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
#include "reosmuskingumclassicrouting.h"
#include "reoshydrograph.h"

ReosMuskingumClassicRouting::ReosMuskingumClassicRouting( ReosHydrographRouting *parent ) :
  ReosHydrographRoutingMethod( parent )
  , mKParameter( new ReosParameterDuration( tr( "K" ), false, this ) )
  , mXParameter( new ReosParameterDouble( tr( "x" ), false, this ) )
{
  mKParameter->setValue( ReosDuration( 1.0, ReosDuration::hour ) );
  mXParameter->setValue( 0.2 );
}

void ReosMuskingumClassicRouting::calculateOutputHydrograph( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosCalculationContext & )
{
  ReosDuration K = mKParameter->value();
  double x = mXParameter->value();

  if ( !inputHydrograph || inputHydrograph->valueCount() < 3 )
    return;

  int inputCount = inputHydrograph->valueCount();
  int i = 0;

  if ( inputCount < 2 ) //need at least two values
    return;

  QDateTime refTime = inputHydrograph->referenceTime()->value();
  ReosDuration t = inputHydrograph->relativeTimeAt( 0 );
  outputHydrograph->setValue( t, 0 );
  ReosDuration lastTimeStep;
  double lastValue = 0;
  while ( i < ( inputCount - 1 ) || outputHydrograph->valueAtTime( t ) > lastValue / 100 )
  {
    ReosDuration timeStep;
    if ( i < inputCount - 1 )
      timeStep = ReosDuration( inputHydrograph->timeAt( i ).msecsTo( inputHydrograph->timeAt( i + 1 ) ), ReosDuration::millisecond );
    else
      timeStep = lastTimeStep;
    int internIteration = 1;
    if ( timeStep > K )
    {
      while ( timeStep > K )
      {
        timeStep = timeStep / 2;
        internIteration = internIteration * 2;
      }
    }
    lastTimeStep = timeStep;

    ReosDuration denom = ( K * 2 * ( 1 - x ) + timeStep );
    double C1 = ( timeStep - K * 2 * x ) / denom;
    double C2 = ( timeStep + K * 2 * x ) / denom;
    double C3 = ( K * 2 * ( 1 - x ) - timeStep ) / denom;

    for ( int it = 0; it < internIteration; ++it )
    {
      outputHydrograph->setValue( t + timeStep,
                                  C1 * inputHydrograph->valueAtTime( t ) +
                                  C2 * inputHydrograph->valueAtTime( t + timeStep ) +
                                  C3 * outputHydrograph->valueAtTime( t ) );
      t = t + timeStep;
    }
    ++i;

    if ( i == ( inputCount - 1 ) )
      lastValue = outputHydrograph->valueAtTime( t - timeStep );
  }
}

ReosParameterDuration *ReosMuskingumClassicRouting::kParameter() const
{
  return mKParameter;
}

ReosParameterDouble *ReosMuskingumClassicRouting::xParameter() const
{
  return mXParameter;
}
