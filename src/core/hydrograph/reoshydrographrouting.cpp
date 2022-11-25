/***************************************************************************
  reoshydrographtransfer.cpp - ReosHydrographTransfer

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
#include "reoshydrographrouting.h"
#include "reoshydrograph.h"
#include "reosstyleregistery.h"
#include "reoshydraulicscheme.h"


ReosHydrographRoutingMethodFactories *ReosHydrographRoutingMethodFactories::sInstance = nullptr;

ReosHydrographRoutingLink::ReosHydrographRoutingLink( ReosHydraulicNetwork *parent ):
  ReosHydraulicLink( parent )
{
  mRoutingMethods.insert( ReosHydrographRoutingMethodDirect::staticType(), new ReosHydrographRoutingMethodDirect( this ) );
  mCurrentRoutingMethod = ReosHydrographRoutingMethodDirect::staticType();

  connect( mRoutingMethods.value( mCurrentRoutingMethod ), &ReosDataObject::dataChanged, this, &ReosHydrographRoutingLink::calculateRouting );

  init();
}

ReosHydrographRoutingLink::ReosHydrographRoutingLink( ReosHydrographSource *hydrographSource, ReosHydrographNode *destination, ReosHydraulicNetwork *parent ):
  ReosHydrographRoutingLink( parent )
{
  setInputHydrographSource( hydrographSource );
  setDestination( destination );
}


ReosHydrographRoutingLink::ReosHydrographRoutingLink( ReosHydrographSource *hydrographSource,
    ReosHydrographNode *destination,
    const ReosEncodedElement &encodedElement,
    ReosHydraulicNetwork *parent )
  : ReosHydraulicLink( encodedElement, parent )
{
  init();

  setInputHydrographSource( hydrographSource );
  setDestination( destination );

  const QList<ReosEncodedElement> encodedRoutings = encodedElement.getListEncodedData( QStringLiteral( "routing-methods" ) );
  for ( const ReosEncodedElement &encodedRouting : encodedRoutings )
  {
    std::unique_ptr<ReosHydrographRoutingMethod> meth( ReosHydrographRoutingMethodFactories::instance()->createRoutingMethod( encodedRouting, this ) );
    if ( meth )
    {
      QString type = meth->type();
      connect( meth.get(), &ReosDataObject::dataChanged, this, &ReosHydrographRoutingLink::calculateRouting );
      mRoutingMethods.insert( type, meth.release() );
    }
  }

  // before v 2.3, this data are stored in the element encoded data, so try to retrieve them here
  if ( encodedElement.hasEncodedData( QStringLiteral( "current-routing-methode" ) ) )
    encodedElement.getData( QStringLiteral( "current-routing-methode" ), mCurrentRoutingMethod );
}

void ReosHydrographRoutingLink::init()
{
  mOutputHydrograph = new ReosHydrograph( this );
  mOutputHydrograph->setColor( ReosStyleRegistery::instance()->curveColor() );

  mOutputHydrograph->setName( tr( "Output of %1" ).arg( elementName()->value() ) );
  connect( elementName(), &ReosParameterString::valueChanged, mOutputHydrograph, [this]
  {
    mOutputHydrograph->setName( tr( "Output of %1" ).arg( elementName()->value() ) );
  } );
}

bool ReosHydrographRoutingLink::setCurrentRoutingMethod( const QString &routingType )
{
  if ( mCurrentRoutingMethod == routingType )
    return true;

  emit dirtied();

  if ( mRoutingMethods.contains( routingType ) )
  {
    mCurrentRoutingMethod = routingType;
    setObsolete();
    calculateRouting();
    return true;
  }

  ReosHydrographRoutingMethod *method = nullptr;
  if ( ReosHydrographRoutingMethodFactories::isInstantiate() )
    method = ReosHydrographRoutingMethodFactories::instance()->createRoutingMethod( routingType, this );

  if ( method )
  {
    mRoutingMethods.insert( routingType, method );
    connect( method, &ReosDataObject::dataChanged, this, &ReosHydrographRoutingLink::calculateRouting );
    connect( method, &ReosDataObject::dataChanged, this, &ReosHydrographRoutingLink::dirtied );
    registerUpstreamData( method );
    mCurrentRoutingMethod = routingType;
    setObsolete();
    calculateRouting();
    return true;
  }

  return false;
}

ReosHydrographRoutingMethod *ReosHydrographRoutingLink::currentRoutingMethod() const
{
  auto it = mRoutingMethods.find( mCurrentRoutingMethod );
  if ( it != mRoutingMethods.end() )
    return it.value();

  return nullptr;
}

void ReosHydrographRoutingLink::setInputHydrographSource( ReosHydrographSource *hydrographSource )
{
  if ( !mNode_1.isNull() )
  {
    disconnect( mNode_1, &ReosHydraulicNetworkElement::calculationIsUpdated, this, &ReosHydrographRoutingLink::onSourceUpdated );
  }
  attachOnSide1( hydrographSource );

  if ( hydrographSource )
  {
    connect( hydrographSource, &ReosHydraulicNetworkElement::calculationIsUpdated, this, &ReosHydrographRoutingLink::onSourceUpdated );
  }
}

ReosHydrographSource *ReosHydrographRoutingLink::inputHydrographSource() const
{
  if ( mNode_1.isNull() )
    return nullptr;
  else
    return qobject_cast<ReosHydrographSource *>( mNode_1 );
}

ReosHydrographNode *ReosHydrographRoutingLink::destinationNode() const
{
  if ( mNode_2.isNull() )
    return nullptr;
  else
    return qobject_cast<ReosHydrographNode *>( mNode_2 );
}

void ReosHydrographRoutingLink::setDestination( ReosHydrographNode *destination )
{
  if ( destinationNode() )
  {
    disconnect( this, &ReosHydraulicNetworkElement::calculationIsUpdated, destinationNode(), &ReosHydrographNode::onUpstreamRoutingUpdated );
  }

  attachOnSide2( destination );

  if ( destination )
  {
    connect( this, &ReosHydraulicNetworkElement::calculationIsUpdated, destinationNode(), &ReosHydrographNode::onUpstreamRoutingUpdated );
  }
}

ReosHydrograph *ReosHydrographRoutingLink::inputHydrograph() const
{
  return inputHydrographSource()->outputHydrograph();
}

ReosHydrograph *ReosHydrographRoutingLink::outputHydrograph() const
{
  return mOutputHydrograph;
}

bool ReosHydrographRoutingLink::calculationInProgress() const {return mCalculationIsInProgress;}

int ReosHydrographRoutingLink::calculationMaxProgression() const
{
  if ( !mCalculationIsInProgress )
    return 100;

  if ( !mCalculation )
    return 0;

  return mCalculation->maxProgression();
}

int ReosHydrographRoutingLink::calculationProgression() const
{
  if ( !mCalculationIsInProgress )
    return 100;

  if ( !mCalculation )
    return 0;

  return mCalculation->currentProgression();
}

void ReosHydrographRoutingLink::saveConfiguration( ReosHydraulicScheme *scheme ) const
{
  ReosEncodedElement encodedElement = scheme->restoreElementConfig( id() );

  encodedElement.addData( QStringLiteral( "current-method" ), mCurrentRoutingMethod );

  scheme->saveElementConfig( id(), encodedElement );

  for ( ReosHydrographRoutingMethod *met : mRoutingMethods )
    met->saveConfiguration( scheme );
}

void ReosHydrographRoutingLink::restoreConfiguration( ReosHydraulicScheme *scheme )
{
  ReosEncodedElement encodedElement = scheme->restoreElementConfig( id() );

  encodedElement.getData( QStringLiteral( "current-method" ), mCurrentRoutingMethod );

  for ( ReosHydrographRoutingMethod *met : std::as_const( mRoutingMethods ) )
    met->restoreConfiguration( scheme );

  emit dataChanged();
}

ReosTimeWindow ReosHydrographRoutingLink::timeWindow() const
{
  if ( mOutputHydrograph )
    return mOutputHydrograph->timeExtent();

  return ReosTimeWindow();
}

void ReosHydrographRoutingLink::updateCalculationContext( const ReosCalculationContext &context )
{
  bool upstreamWillBeUpdated = false;

  if ( inputHydrographSource() )
    upstreamWillBeUpdated = inputHydrographSource()->updateCalculationContextFromDownstream( context, this );

  if ( upstreamWillBeUpdated && mCalculation )
  {
    mCalculation->stop( true );
    mCalculation = nullptr;
  }

  if ( !upstreamWillBeUpdated && isObsolete() )
  {
    calculateRouting();
    upstreamWillBeUpdated = true;
  }

  mCalculationIsInProgress |= upstreamWillBeUpdated;

  if ( upstreamWillBeUpdated )
    emit calculationStart();

  if ( destinationNode() )
    destinationNode()->updateCalculationContextFromUpstream( context, this, upstreamWillBeUpdated );
}

void ReosHydrographRoutingLink::updateCalculationContextFromUpstream( const ReosCalculationContext &context, bool upstreamWillChange )
{
  if ( upstreamWillChange )
    mOutputHydrograph->setHydrographObsolete();
  else if ( isObsolete() )
  {
    calculateRouting();
    upstreamWillChange = true;
  }

  destinationNode()->updateCalculationContextFromUpstream( context, this, upstreamWillChange );
}

bool ReosHydrographRoutingLink::updateCalculationContextFromDownstream( const ReosCalculationContext &context )
{
  bool upstreamWillChange = inputHydrographSource()->updateCalculationContextFromDownstream( context, this );

  if ( upstreamWillChange )
    mOutputHydrograph->setHydrographObsolete();
  else if ( isObsolete() )
  {
    calculateRouting();
    upstreamWillChange = true;
  }

  return upstreamWillChange;
}


void ReosHydrographRoutingLink::calculateRouting()
{
  if ( ! inputHydrographSource() )
    return;
  ReosHydrographRoutingMethod *method = mRoutingMethods.value( mCurrentRoutingMethod, nullptr );
  if ( method )
  {
    ReosCalculationContext context;
//    method->calculateOutputHydrograph( inputHydrographSource()->outputHydrograph(), mOutputHydrograph, context );
#ifndef _NDEBUG
    qDebug() << "calculation of link: " << elementName()->value();
#endif

    emit calculationStart();
    mCalculationIsInProgress = true;

    if ( mCalculation )
      mCalculation->stop( true );

    ReosHydrographCalculation *calculation = method->calculationProcess( inputHydrographSource()->outputHydrograph(), context );
    mCalculation = calculation;
    connect( calculation, &ReosProcess::finished, this, [this, calculation]
    {
      if ( mCalculation == calculation )
      {
        notify( calculation->message() );
        mCalculation = nullptr;
        mCalculationIsInProgress = false;

        if ( calculation->isSuccessful() )
        {
          mOutputHydrograph->copyFrom( calculation->hydrograph() );
          calculationUpdated();
        }
      }
      emit timeWindowChanged();
      calculation->deleteLater();
    } );

    calculation->startOnOtherThread();
  }
}


void ReosHydrographRoutingLink::onSourceUpdated()
{
  calculateRouting();
}

void ReosHydrographRoutingLink::encodeData( ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const
{
  QList<ReosEncodedElement> encodedRoutings;
  for ( ReosHydrographRoutingMethod *routing : mRoutingMethods )
    encodedRoutings.append( routing->encode() );

  element.addListEncodedData( QStringLiteral( "routing-methods" ), encodedRoutings );

  element.addData( QStringLiteral( "current-routing-methode" ), mCurrentRoutingMethod );

  ReosHydraulicLink::encodeData( element, context );
}

ReosHydrographRoutingLink *ReosHydrographRoutingLink::decode( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context )
{
  if ( encodedElement.description() != ReosHydrographRoutingLink::staticType() )
    return nullptr;

  const QPair<QString, QString> nodesId = ReosHydraulicLink::decodeNodesId( encodedElement );

  ReosHydrographSource *source = qobject_cast<ReosHydrographSource *>( context.network()->getElement( nodesId.first ) );
  ReosHydrographNode *destination = qobject_cast<ReosHydrographNode *>( context.network()->getElement( nodesId.second ) );

  if ( !source || !destination )
    return nullptr;

  std::unique_ptr<ReosHydrographRoutingLink> ret( new ReosHydrographRoutingLink( source, destination, encodedElement, context.network() ) );

  return ret.release();
}


ReosHydrographRoutingMethod::ReosHydrographRoutingMethod( ReosHydrographRoutingLink *routingLink ): ReosDataObject( routingLink ) {}

ReosHydrographRoutingMethodDirect::ReosHydrographRoutingMethodDirect( ReosHydrographRoutingLink *routingLink ) : ReosHydrographRoutingMethod( routingLink )
{

}

ReosHydrographRoutingMethodDirect::ReosHydrographRoutingMethodDirect( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *parent )
  : ReosHydrographRoutingMethodDirect( parent )
{
  ReosDataObject::decode( encodedElement );
}

void ReosHydrographRoutingMethodDirect::calculateOutputHydrograph( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosCalculationContext & )
{
  if ( !inputHydrograph )
    return;
  outputHydrograph->clear();
  outputHydrograph->copyFrom( inputHydrograph );
}

ReosHydrographCalculation *ReosHydrographRoutingMethodDirect::calculationProcess( ReosHydrograph *inputHydrograph, const ReosCalculationContext &context )
{
  return new Calculation( inputHydrograph );
}

ReosEncodedElement ReosHydrographRoutingMethodDirect::encode() const
{
  ReosEncodedElement element( type() );
  ReosDataObject::encode( element );
  return element;
}

ReosHydrographRoutingMethodFactories::~ReosHydrographRoutingMethodFactories()
{
  sInstance = nullptr;
}

void ReosHydrographRoutingMethodFactories::instantiate( ReosModule *parent )
{
  if ( !sInstance )
    sInstance = new ReosHydrographRoutingMethodFactories( parent );
}

bool ReosHydrographRoutingMethodFactories::isInstantiate()
{
  return sInstance != nullptr;
}

ReosHydrographRoutingMethodFactories *ReosHydrographRoutingMethodFactories::instance()
{
  if ( !sInstance )
    sInstance = new ReosHydrographRoutingMethodFactories();

  return sInstance;
}

void ReosHydrographRoutingMethodFactories::addFactory( ReosHydrographRoutingMethodFactory *factory )
{
  mFactories.emplace( factory->type(), factory );
}

ReosHydrographRoutingMethod *ReosHydrographRoutingMethodFactories::createRoutingMethod( const QString &type, ReosHydrographRoutingLink *link )
{
  auto it = mFactories.find( type );
  if ( it != mFactories.end() )
    return it->second->createRoutingMethod( link );

  return nullptr;
}

ReosHydrographRoutingMethod *ReosHydrographRoutingMethodFactories::createRoutingMethod( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *link )
{
  auto it = mFactories.find( encodedElement.description() );
  if ( it != mFactories.end() )
    return it->second->createRoutingMethod( encodedElement, link );

  return nullptr;
}

QString ReosHydrographRoutingMethodFactories::displayName( const QString &type ) const
{
  auto it = mFactories.find( type );
  if ( it != mFactories.end() )
    return it->second->displayName();

  return QString();
}

QStringList ReosHydrographRoutingMethodFactories::methodTypes() const
{
  QStringList ret;
  for ( const auto &it : mFactories )
    ret.append( it.first );

  return ret;
}

QString ReosHydrographRoutingMethodFactories::htmlDescription( const QString &type )
{
  auto it = mFactories.find( type );
  if ( it != mFactories.end() )
    return it->second->htmlDescription();

  return QString();
}

ReosHydrographRoutingMethodFactories::ReosHydrographRoutingMethodFactories( ReosModule *parent ): ReosModule( parent )
{
  addFactory( new ReosHydrographRoutingMethodDirectFactory );
  addFactory( new ReosHydrographRoutingMethodMuskingumFactory );
  addFactory( new ReosHydrographRoutingMethodLagFactory );
}

ReosHydraulicNetworkElement *ReosHydrographRoutingLinkFactory::decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const
{
  return ReosHydrographRoutingLink::decode( encodedElement, context );
}

ReosHydrographRoutingMethodDirect::Calculation::Calculation( ReosHydrograph *inputHydrograph )
{
  mInputHydrograph = std::make_unique<ReosHydrograph>();
  mInputHydrograph->copyFrom( inputHydrograph );
}

void ReosHydrographRoutingMethodDirect::Calculation::start()
{
  mHydrograph.reset( new ReosHydrograph );
  mHydrograph->copyFrom( mInputHydrograph.get() );
  mIsSuccessful = true;
}

ReosHydrographRoutingMethod *ReosHydrographRoutingMethodDirectFactory::createRoutingMethod( ReosHydrographRoutingLink *routingLink ) const
{return new ReosHydrographRoutingMethodDirect( routingLink );}

ReosHydrographRoutingMethod *ReosHydrographRoutingMethodDirectFactory::createRoutingMethod( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *routingLink ) const
{
  return new ReosHydrographRoutingMethodDirect( encodedElement, routingLink );
}

QString ReosHydrographRoutingMethodDirectFactory::type() const
{return ReosHydrographRoutingMethodDirect::staticType();}

QString ReosHydrographRoutingMethodDirectFactory::htmlDescription() const
{
  QString htmlText = QLatin1String( "<html>\n<body>\n" );
  htmlText += QLatin1String( "<table class=\"list-view\">\n" );
  htmlText += QLatin1String( "<h1>" ) + displayName() + QLatin1String( "</h1>\n<hr>\n" );
  htmlText += QObject::tr( "This routing simply copy the input hydrograph to output without any change" );

  return htmlText;
}

#ifndef _NDEBUG
#include <QElapsedTimer>
#endif

ReosHydrographRoutingMethodMuskingum::ReosHydrographRoutingMethodMuskingum( ReosHydrographRoutingLink *parent ) :
  ReosHydrographRoutingMethod( parent )
  , mKParameter( new ReosParameterDuration( tr( "K" ), false, this ) )
  , mXParameter( new ReosParameterDouble( tr( "x" ), false, this ) )
{
  mKParameter->setValue( ReosDuration( 1.0, ReosDuration::hour ) );
  mXParameter->setValue( 0.2 );

  connect( mKParameter, &ReosParameter::valueChanged, this, &ReosHydrographRoutingMethodMuskingum::dataChanged );
  connect( mXParameter, &ReosParameter::valueChanged, this, &ReosHydrographRoutingMethodMuskingum::dataChanged );
}

ReosHydrographRoutingMethodMuskingum::ReosHydrographRoutingMethodMuskingum( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *parent ):
  ReosHydrographRoutingMethod( parent )
  , mKParameter( ReosParameterDuration::decode( encodedElement.getEncodedData( QStringLiteral( "K-parameter" ) ), false, tr( "K" ), this ) )
  , mXParameter( ReosParameterDouble::decode( encodedElement.getEncodedData( QStringLiteral( "X-parameter" ) ), false, tr( "x" ), this ) )
{
  ReosDataObject::decode( encodedElement );
  connect( mKParameter, &ReosParameter::valueChanged, this, &ReosHydrographRoutingMethodMuskingum::dataChanged );
  connect( mXParameter, &ReosParameter::valueChanged, this, &ReosHydrographRoutingMethodMuskingum::dataChanged );
}

void ReosHydrographRoutingMethodMuskingum::calculateOutputHydrograph( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosCalculationContext & )
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

ReosHydrographCalculation *ReosHydrographRoutingMethodMuskingum::calculationProcess( ReosHydrograph *inputHydrograph, const ReosCalculationContext &context )
{
  return new Calculation( inputHydrograph, mKParameter->value(), mXParameter->value() );
}

QString ReosHydrographRoutingMethodMuskingum::type() const {return staticType();}

QString ReosHydrographRoutingMethodMuskingum::staticType() {return ReosHydrographRoutingMethod::staticType() + QString( ':' ) + QStringLiteral( "muskingum" );}

ReosParameterDuration *ReosHydrographRoutingMethodMuskingum::kParameter() const
{
  return mKParameter;
}

ReosParameterDouble *ReosHydrographRoutingMethodMuskingum::xParameter() const
{
  return mXParameter;
}

ReosEncodedElement ReosHydrographRoutingMethodMuskingum::encode() const
{
  ReosEncodedElement element( type() );

  element.addEncodedData( QStringLiteral( "K-parameter" ), mKParameter->encode() );
  element.addEncodedData( QStringLiteral( "X-parameter" ), mXParameter->encode() );

  ReosDataObject::encode( element );

  return element;
}

void ReosHydrographRoutingMethodMuskingum::saveConfiguration( ReosHydraulicScheme *scheme ) const
{
  ReosDuration k = mKParameter->value();
  double X = mXParameter->value();

  ReosEncodedElement encoded = scheme->restoreElementConfig( id() );
  encoded.addEncodedData( QStringLiteral( "k-parameter" ), k.encode() );
  encoded.addData( QStringLiteral( "X-parameter" ), X );

  scheme->saveElementConfig( id(), encoded );
}

void ReosHydrographRoutingMethodMuskingum::restoreConfiguration( ReosHydraulicScheme *scheme )
{
  const ReosEncodedElement encoded = scheme->restoreElementConfig( id() );

  if ( encoded.hasEncodedData( QStringLiteral( "k-parameter" ) ) )
    mKParameter->setValue( ReosDuration::decode( encoded.getEncodedData( QStringLiteral( "k-parameter" ) ) ) );

  double X = mXParameter->value();
  encoded.getData( QStringLiteral( "X-parameter" ), X );
  mXParameter->setValue( X );
}

void ReosHydrographRoutingMethodMuskingum::calculate( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosDuration &K, double x, ReosProcess *process )
{
  if ( !inputHydrograph )
    return;

  ReosModule::Message message;

  if ( inputHydrograph->valueCount() < 3 ) //need at least two values
  {
    if ( process )
    {
      message.type = ReosModule::Error;
      message.addText( tr( "Muskingum routing method need more than two value for input hydograph" ) );
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

ReosHydrographRoutingMethod *ReosHydrographRoutingMethodMuskingumFactory::createRoutingMethod( ReosHydrographRoutingLink *routingLink ) const
{return new ReosHydrographRoutingMethodMuskingum( routingLink );}

ReosHydrographRoutingMethod *ReosHydrographRoutingMethodMuskingumFactory::createRoutingMethod( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *routingLink ) const
{
  if ( encodedElement.description() != ReosHydrographRoutingMethodMuskingum::staticType() )
    return nullptr;

  return new ReosHydrographRoutingMethodMuskingum( encodedElement, routingLink );
}

QString ReosHydrographRoutingMethodMuskingumFactory::type() const
{
  return ReosHydrographRoutingMethodMuskingum::staticType();
}

QString ReosHydrographRoutingMethodMuskingumFactory::htmlDescription() const
{
  QString htmlText = QLatin1String( "<html>\n<body>\n" );
  htmlText += QLatin1String( "<table class=\"list-view\">\n" );
  htmlText += QLatin1String( "<h1>" ) + displayName() + QLatin1String( "</h1>\n<hr>\n" );
  htmlText += QObject::tr( "The Muskingum routing method expresses the output flow of reach depending on the input flow and two parameters K and x. "
                           "This method has the following formulation:" );
  htmlText += QLatin1String( "<br>" );
  htmlText += QLatin1String( "<br>" );
  htmlText += QLatin1String( "<img src = " ) + QLatin1String( ":/formulas/MuskingumRouting.svg" ) + QLatin1String( "/>" );
  htmlText += QLatin1String( "<br>" );
  htmlText += QObject::tr( "&Delta;t is the time step of the input hydrograph. The parameter K can be considered as the travel time through the reach; "
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

ReosHydrographRoutingMethodMuskingum::Calculation::Calculation( ReosHydrograph *inputHydrograph, const ReosDuration &K, double X )
  : mK( K )
  , mX( X )
{
  mInputHydrograph = std::make_unique<ReosHydrograph>();
  mInputHydrograph->copyFrom( inputHydrograph );
  mHydrograph = std::make_unique<ReosHydrograph>();
}

void ReosHydrographRoutingMethodMuskingum::Calculation::start()
{
  calculate( mInputHydrograph.get(), mHydrograph.get(), mK, mX, this );
  if ( !isStop() )
    mIsSuccessful = true;
}

ReosHydrographRoutingMethodLag::ReosHydrographRoutingMethodLag( ReosHydrographRoutingLink *parent ) :
  ReosHydrographRoutingMethod( parent )
  , mLagParameter( new ReosParameterDuration( tr( "Lag" ), false, this ) )
{
  mLagParameter->setValue( ReosDuration( 1.0, ReosDuration::hour ) );

  connect( mLagParameter, &ReosParameter::valueChanged, this, &ReosHydrographRoutingMethodMuskingum::dataChanged );
}

ReosHydrographRoutingMethodLag::ReosHydrographRoutingMethodLag( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *parent ):
  ReosHydrographRoutingMethod( parent )
  , mLagParameter( ReosParameterDuration::decode( encodedElement.getEncodedData( QStringLiteral( "lag-parameter" ) ), false, tr( "Lag" ), this ) )
{
  ReosDataObject::decode( encodedElement );
  connect( mLagParameter, &ReosParameter::valueChanged, this, &ReosHydrographRoutingMethodMuskingum::dataChanged );
}

void ReosHydrographRoutingMethodLag::calculateOutputHydrograph( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosCalculationContext & )
{
  calculate( inputHydrograph, outputHydrograph, mLagParameter->value() );
}

ReosHydrographCalculation *ReosHydrographRoutingMethodLag::calculationProcess( ReosHydrograph *inputHydrograph, const ReosCalculationContext & )
{
  return new Calculation( inputHydrograph, mLagParameter->value() );
}

QString ReosHydrographRoutingMethodLag::type() const {return staticType();}

QString ReosHydrographRoutingMethodLag::staticType() {return ReosHydrographRoutingMethod::staticType() + QString( ':' ) + QStringLiteral( "lag" );}

ReosParameterDuration *ReosHydrographRoutingMethodLag::lagParameter() const
{
  return mLagParameter;
}

ReosEncodedElement ReosHydrographRoutingMethodLag::encode() const
{
  ReosEncodedElement element( type() );

  element.addEncodedData( QStringLiteral( "lag-parameter" ), mLagParameter->encode() );

  ReosDataObject::encode( element );
  return element;
}

void ReosHydrographRoutingMethodLag::saveConfiguration( ReosHydraulicScheme *scheme ) const
{
  ReosDuration lag = mLagParameter->value();

  ReosEncodedElement encoded = scheme->restoreElementConfig( id() );
  encoded.addEncodedData( QStringLiteral( "lag-parameter" ), lag.encode() );

  scheme->saveElementConfig( id(), encoded );
}

void ReosHydrographRoutingMethodLag::restoreConfiguration( ReosHydraulicScheme *scheme )
{
  const ReosEncodedElement encoded = scheme->restoreElementConfig( id() );

  if ( encoded.hasEncodedData( QStringLiteral( "lag-parameter" ) ) )
    mLagParameter->setValue( ReosDuration::decode( encoded.getEncodedData( QStringLiteral( "lag-parameter" ) ) ) );
}

void ReosHydrographRoutingMethodLag::calculate( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosDuration &lag, ReosProcess * )
{
  if ( !inputHydrograph )
    return;

  outputHydrograph->copyFrom( inputHydrograph );
  outputHydrograph->setReferenceTime( inputHydrograph->referenceTime().addMSecs( lag.valueMilliSecond() ) );
  if ( inputHydrograph->valueCount() > 0 )
    outputHydrograph->setValue( inputHydrograph->timeAt( 0 ), inputHydrograph->valueAt( 0 ) );
}

ReosHydrographRoutingMethodLag::Calculation::Calculation( ReosHydrograph *inputHydrograph, const ReosDuration &lag ):
  mLag( lag )
{
  mInputHydrograph = std::make_unique<ReosHydrograph>();
  mInputHydrograph->copyFrom( inputHydrograph );
  mHydrograph = std::make_unique<ReosHydrograph>();
}

void ReosHydrographRoutingMethodLag::Calculation::start()
{
  calculate( mInputHydrograph.get(), mHydrograph.get(), mLag, this );
  if ( !isStop() )
    mIsSuccessful = true;
}

ReosHydrographRoutingMethod *ReosHydrographRoutingMethodLagFactory::createRoutingMethod( ReosHydrographRoutingLink *routingLink ) const
{
  return new ReosHydrographRoutingMethodLag( routingLink );
}

ReosHydrographRoutingMethod *ReosHydrographRoutingMethodLagFactory::createRoutingMethod( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *routingLink ) const
{
  return new ReosHydrographRoutingMethodLag( encodedElement, routingLink );
}

QString ReosHydrographRoutingMethodLagFactory::type() const
{
  return ReosHydrographRoutingMethodLag::staticType();
}

QString ReosHydrographRoutingMethodLagFactory::htmlDescription() const
{
  QString htmlText = QLatin1String( "<html>\n<body>\n" );
  htmlText += QLatin1String( "<table class=\"list-view\">\n" );
  htmlText += QLatin1String( "<h1>" ) + displayName() + QLatin1String( "</h1>\n<hr>\n" );
  htmlText += QObject::tr( "The Lag routing method apply a duration offset on the hydrograph:"
                           "<br>"
                           "Output ( t + lag ) = Input( t )" );
  return htmlText;
}
