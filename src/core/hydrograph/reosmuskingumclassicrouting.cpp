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

ReosMuskingumClassicRouting::ReosMuskingumClassicRouting( QObject *parent ) :
  ReosHydrographTransfer( parent )
  , mKParameter( new ReosParameterDuration( tr( "K" ), false, this ) )
  , mXParameter( new ReosParameterDouble( tr( "x" ), false, this ) )
{}

ReosHydrograph *ReosMuskingumClassicRouting::outputHydrograph( const ReosCalculationContext &context ) const
{
  calculate( context );
  return mResultHydrograph;
}

void ReosMuskingumClassicRouting::calculate( const ReosCalculationContext &context ) const
{
  ReosDuration K = mKParameter->value();
  double x = mXParameter->value();
  ReosHydrograph *inputHydrograph = inputHydrographSource()->outputHydrograph( context );

  if ( !inputHydrograph || inputHydrograph->valueCount() < 3 )
    return;

  mResultHydrograph->clear();

  int inputCount = inputHydrograph->valueCount();
  int i = 0;

  QDateTime refTime = inputHydrograph->referenceTime()->value();
  ReosDuration t = inputHydrograph->relativeTimeAt( 0 );
  mResultHydrograph->setValue( t, 0 );
  while ( i <= inputCount )
  {
    ReosDuration timeStep = ReosDuration( inputHydrograph->timeAt( i ).msecsTo( inputHydrograph->timeAt( i + 1 ) ), ReosDuration::millisecond );
    int internIteration = 1;
    if ( timeStep > K )
    {
      while ( timeStep > K )
      {
        timeStep = timeStep / 2;
        internIteration = internIteration * 2;
      }
    }

    ReosDuration denom = ( K * 2 * ( 1 - x ) + timeStep );
    double C1 = ( timeStep - K * 2 * x ) / denom;
    double C2 = ( timeStep + K * 2 * x ) / denom;
    double C3 = ( K * 2 * ( 1 - x ) - timeStep ) / denom;

    for ( int it = 0; it < internIteration; ++it )
    {
      mResultHydrograph->setValue( t + timeStep,
                                   C1 * inputHydrograph->valueAtTime( t ) +
                                   C2 * inputHydrograph->valueAtTime( t + timeStep ) +
                                   C3 * mResultHydrograph->valueAtTime( t ) );
      t = t + timeStep;
    }
  }
}
