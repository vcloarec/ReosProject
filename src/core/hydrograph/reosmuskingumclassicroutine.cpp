/***************************************************************************
  reosmuskingumclassicroutine.cpp - ReosMuskingumClassicRoutine

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
#include "reosmuskingumclassicroutine.h"
#include "reoshydrograph.h"

ReosMuskingumClassicRoutine::ReosMuskingumClassicRoutine( ReosHydrographRoutingLink *parent ) :
  ReosHydrographRoutingMethod( parent )
  , mKParameter( new ReosParameterDuration( tr( "K" ), false, this ) )
  , mXParameter( new ReosParameterDouble( tr( "x" ), false, this ) )
{
  mKParameter->setValue( ReosDuration( 1.0, ReosDuration::hour ) );
  mXParameter->setValue( 0.2 );

  connect( mKParameter, &ReosParameter::valueChanged, this, &ReosMuskingumClassicRoutine::dataChanged );
  connect( mXParameter, &ReosParameter::valueChanged, this, &ReosMuskingumClassicRoutine::dataChanged );
}

ReosMuskingumClassicRoutine::ReosMuskingumClassicRoutine( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *parent ):
  ReosHydrographRoutingMethod( parent )
  , mKParameter( ReosParameterDuration::decode( encodedElement.getEncodedData( QStringLiteral( "K-parameter" ) ), false, tr( "K" ), this ) )
  , mXParameter( ReosParameterDouble::decode( encodedElement.getEncodedData( QStringLiteral( "X-parameter" ) ), false, tr( "x" ), this ) )
{
  connect( mKParameter, &ReosParameter::valueChanged, this, &ReosMuskingumClassicRoutine::dataChanged );
  connect( mXParameter, &ReosParameter::valueChanged, this, &ReosMuskingumClassicRoutine::dataChanged );
}

void ReosMuskingumClassicRoutine::calculateOutputHydrograph( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosCalculationContext & )
{
#ifndef _NDEBUG
  QElapsedTimer timer;
  timer.start();
#endif

  calculate( inputHydrograph, outputHydrograph, mKParameter->value(), mXParameter->value() );

#ifndef _NDEBUG
  qDebug() << staticType() << " calculation spend "
           << timer.elapsed() << "ms to obtain hydrograph with "
           << outputHydrograph->valueCount() << "values from " << inputHydrograph->valueCount() << "values";
#endif
}

ReosHydrographCalculation *ReosMuskingumClassicRoutine::calculationProcess( ReosHydrograph *inputHydrograph, const ReosCalculationContext &context )
{
  return new Calculation( inputHydrograph, mKParameter->value(), mXParameter->value() );
}

ReosParameterDuration *ReosMuskingumClassicRoutine::kParameter() const
{
  return mKParameter;
}

ReosParameterDouble *ReosMuskingumClassicRoutine::xParameter() const
{
  return mXParameter;
}

ReosEncodedElement ReosMuskingumClassicRoutine::encode() const
{
  ReosEncodedElement element( type() );

  element.addEncodedData( QStringLiteral( "K-parameter" ), mKParameter->encode() );
  element.addEncodedData( QStringLiteral( "X-parameter" ), mXParameter->encode() );
  return element;
}

void ReosMuskingumClassicRoutine::calculate( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosDuration &K, double x, ReosProcess *process )
{
  if ( !inputHydrograph || inputHydrograph->valueCount() < 3 )
    return;

  std::unique_ptr<ReosHydrograph> tempHyd = std::make_unique<ReosHydrograph>();
  tempHyd->setReferenceTime( inputHydrograph->referenceTime() );

  int inputCount = inputHydrograph->valueCount();
  int i = 0;

  if ( inputCount < 2 ) //need at least two values
    return;

  QDateTime refTime = inputHydrograph->referenceTime();
  ReosDuration t = inputHydrograph->relativeTimeAt( 0 );
  tempHyd->setValue( t, 0 );
  ReosDuration lastTimeStep;
  double lastValue = 0;
  while ( i < ( inputCount - 1 ) || tempHyd->valueAtTime( t ) > lastValue / 100 )
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
      tempHyd->setValue( t + timeStep,
                         C1 * inputHydrograph->valueAtTime( t ) +
                         C2 * inputHydrograph->valueAtTime( t + timeStep ) +
                         C3 * tempHyd->valueAtTime( t ) );
      t = t + timeStep;
    }
    ++i;

    if ( i == ( inputCount - 1 ) )
      lastValue = tempHyd->valueAtTime( t - timeStep );
  }

  outputHydrograph->copyFrom( tempHyd.get() );
}

ReosHydrographRoutingMethod *ReosMuskingumClassicRoutineFactory::createRoutingMethod( ReosHydrographRoutingLink *routingLink ) const
{return new ReosMuskingumClassicRoutine( routingLink );}

ReosHydrographRoutingMethod *ReosMuskingumClassicRoutineFactory::createRoutingMethod( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *routingLink ) const
{
  if ( encodedElement.description() != ReosMuskingumClassicRoutine::staticType() )
    return nullptr;

  return new ReosMuskingumClassicRoutine( encodedElement, routingLink );
}

QString ReosMuskingumClassicRoutineFactory::type() const
{
  return ReosMuskingumClassicRoutine::staticType();
}
