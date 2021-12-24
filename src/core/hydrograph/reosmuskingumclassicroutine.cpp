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

#ifndef _NDEBUG
#include <QElapsedTimer>
#endif

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
  if ( !inputHydrograph )
    return;

  ReosModule::Message message;

  if ( inputHydrograph->valueCount() < 3 ) //need at least two values
  {
    if ( process )
    {
      message.type = ReosModule::Error;
      message.addText( tr( "Muskingum routing method need at least to value for input hydograph" ) );
    }
    return;
  }

  if ( x > 0.5 )
  {
    if ( process )
    {
      message.type = ReosModule::Error;
      message.addText( tr( "X parameter for Muskingum routing has to be less than 0.5" ) );
      process->notify( message );
    }
    return;
  }

  if ( x == 0.5 )
  {
    if ( process )
    {
      message.type = ReosModule::Warning;
      message.addText( tr( "X parameter for Muskingum routing equal 0.5, there will be no attenuation, consider using Lag routing method instead" ) );
    }
  }

  std::unique_ptr<ReosHydrograph> tempHyd = std::make_unique<ReosHydrograph>();
  tempHyd->setReferenceTime( inputHydrograph->referenceTime() );

  int inputCount = inputHydrograph->valueCount();
  int i = 0;

  if ( process )
    process->setCurrentProgression( 0 );

  if ( process )
    process->setMaxProgression( inputCount );
  int progressStep = std::max( inputCount / 100, 5 );

  QDateTime refTime = inputHydrograph->referenceTime();
  ReosDuration t = inputHydrograph->relativeTimeAt( 0 );
  tempHyd->setValue( t, 0 );
  ReosDuration lastTimeStep;
  double lastValue = 0;

  ReosDuration upperTimeStepBound = K * 2 * ( 1 - x ) ;
  ReosDuration lowerTimeStepBound = K * 2 * ( x ) ;

  bool tooSmallTimeStep = false;

  while ( i < ( inputCount - 1 ) || tempHyd->valueAtTime( t ) > lastValue / 100 )
  {
    ReosDuration timeStep;
    if ( i < inputCount - 1 )
      timeStep = ReosDuration( inputHydrograph->timeAt( i ).msecsTo( inputHydrograph->timeAt( i + 1 ) ), ReosDuration::millisecond );
    else
      timeStep = lastTimeStep;
    int internIteration = 1;
    if ( timeStep > upperTimeStepBound )
    {
      while ( timeStep > upperTimeStepBound )
      {
        timeStep = timeStep / 2;
        internIteration = internIteration * 2;
      }
    }
    else if ( timeStep < lowerTimeStepBound )
      tooSmallTimeStep = true;

    lastTimeStep = timeStep;

    ReosDuration denom = ( K * 2 * ( 1 - x ) + timeStep );
    double C1 = ( timeStep - K * 2 * x ) / denom;
    double C2 = ( timeStep + K * 2 * x ) / denom;
    double C3 = ( K * 2 * ( 1 - x ) - timeStep ) / denom;

    for ( int it = 0; it < internIteration; ++it )
    {
      tempHyd->setValue( t + timeStep,
                         C1 * inputHydrograph->valueAtTime( t + timeStep ) +
                         C2 * inputHydrograph->valueAtTime( t ) +
                         C3 * tempHyd->valueAtTime( t ) );
      t = t + timeStep;

      if ( process )
      {
        if ( process->isStop() )
          return;
      }
    }
    ++i;

    if ( i == ( inputCount - 1 ) )
      lastValue = tempHyd->valueAtTime( t - timeStep );

    if ( process )
    {
      if ( i % progressStep == 0 )
      {
        process->setCurrentProgression( i );
      }

      if ( process->isStop() )
        return;
    }
  }

  if ( process )
  {
    if ( tooSmallTimeStep )
    {
      message.type = ReosModule::Warning;
      message.addText( tr( "The time step of the input hydrograph is too small considering the parameter of the Muskingum routing method" ) );
    }
  }

  if ( process && !message.text.isEmpty() )
    process->notify( message );

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

QString ReosMuskingumClassicRoutineFactory::htmlDescription() const
{
  QString htmlText = QLatin1String( "<html>\n<body>\n" );
  htmlText += QLatin1String( "<table class=\"list-view\">\n" );
  htmlText += QLatin1String( "<h1>" ) + displayName() + QLatin1String( "</h1>\n<hr>\n" );
  htmlText += QLatin1String( "The Muskingum routing method expresses the output flow of reach depending on the input flow and two parameters K and x. "
                             "This method has the following formulation:" );
  htmlText += QLatin1String( "<br>" );
  htmlText += QLatin1String( "<br>" );
  htmlText += QLatin1String( "<img src = " ) + QLatin1String( ":/formulas/MuskingumRouting.svg" ) + QLatin1String( "/>" );
  htmlText += QLatin1String( "<br>" );
  htmlText += QLatin1String( "&Delta;t is the time step of the input hydrograph. The parameter K can be considered as the travel time through the reach; "
                             "x, dimensionless, is a parameter that expresses the attenuation of the hydrograph. As the terms C1, C2 and C3 must be non-negative, "
                             "K and x have to be chosen carefully depending on the time step of the input hydrograph, and must verify the two following conditions:"
                             "<ul>"
                             "<li>2.K.(1-x) > &Delta;t</li>"
                             "<li>&Delta;t > 2.K.x</li>"
                             "</ul>"
                             "Lekan ensures the first condition by reducing the time step of the input hydrograph if needed. "
                             "For the second one, to avoid arbitrarly distorting the input hydrograph, nothing is done except "
                             "a warning to the user to change either the parameters, the time step, or the method."
                             "<br>"
                           );

  return htmlText;
}
