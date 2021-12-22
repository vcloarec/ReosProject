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
#include "reoshydrographtransfer.h"
#include "reoshydrograph.h"
#include "reosmuskingumclassicroutine.h"
#include "reosstyleregistery.h"


ReosHydrographRoutingMethodFactories *ReosHydrographRoutingMethodFactories::sInstance = nullptr;

ReosHydrographRoutingLink::ReosHydrographRoutingLink( ReosHydraulicNetwork *parent ):
  ReosHydraulicLink( parent )
{
  mRoutingMethods.insert( ReosDirectHydrographRouting::staticType(), new ReosDirectHydrographRouting( this ) );
  mCurrentRoutingMethod = ReosDirectHydrographRouting::staticType();

  connect( mRoutingMethods.value( mCurrentRoutingMethod ), &ReosDataObject::dataChanged, this, &ReosHydrographRoutingLink::calculateRouting );

  init();
}

ReosHydrographRoutingLink::ReosHydrographRoutingLink( ReosHydrographSource *hydrographSource, ReosHydrographNode *destination, ReosHydraulicNetwork *parent ):
  ReosHydrographRoutingLink( parent )
{
  setInputHydrographSource( hydrographSource );
  setHydrographDestination( destination );
}


ReosHydrographRoutingLink::ReosHydrographRoutingLink( ReosHydrographSource *hydrographSource,
    ReosHydrographNode *destination,
    const ReosEncodedElement &encodedElement,
    ReosHydraulicNetwork *parent )
  : ReosHydraulicLink( encodedElement, parent )
{
  init();

  setInputHydrographSource( hydrographSource );
  setHydrographDestination( destination );

  const QList<ReosEncodedElement> encodedRoutines = encodedElement.getListEncodedData( QStringLiteral( "routines-method" ) );
  for ( const ReosEncodedElement &encodedRoutine : encodedRoutines )
  {
    std::unique_ptr<ReosHydrographRoutingMethod> meth( ReosHydrographRoutingMethodFactories::instance()->createRoutingMethod( encodedRoutine, this ) );
    if ( meth )
    {
      QString type = meth->type();
      connect( meth.get(), &ReosDataObject::dataChanged, this, &ReosHydrographRoutingLink::calculateRouting );
      mRoutingMethods.insert( type, meth.release() );
    }
  }

  encodedElement.getData( QStringLiteral( "current-routine-methode" ), mCurrentRoutingMethod );
}

void ReosHydrographRoutingLink::init()
{
  mOutputHydrograph = new ReosHydrograph( this );
  mOutputHydrograph->setColor( ReosStyleRegistery::instance()->curveColor() );

  mOutputHydrograph->setName( tr( "Output of %1" ).arg( name()->value() ) );
  connect( name(), &ReosParameterString::valueChanged, mOutputHydrograph, [this]
  {
    mOutputHydrograph->setName( tr( "Output of %1" ).arg( name()->value() ) );
  } );
}

bool ReosHydrographRoutingLink::setCurrentRoutingMethod( const QString &routingType )
{
  if ( mCurrentRoutingMethod == routingType )
    return true;

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

void ReosHydrographRoutingLink::setHydrographDestination( ReosHydrographNode *destination )
{
  if ( destinationNode() )
  {
    disconnect( this, &ReosHydraulicNetworkElement::calculationIsUpdated, destinationNode(), &ReosHydrographNode::onUpstreamRoutineUpdated );
  }

  attachOnSide2( destination );

  if ( destination )
  {
    connect( this, &ReosHydraulicNetworkElement::calculationIsUpdated, destinationNode(), &ReosHydrographNode::onUpstreamRoutineUpdated );
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
    qDebug() << "calculation of link: " << name()->value();
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
        mCalculation = nullptr;
        if ( calculation->isSuccessful() )
        {
          mOutputHydrograph->copyFrom( calculation->hydrograph() );
          calculationUpdated();
        }
        mCalculationIsInProgress = false;
      }
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
  QList<ReosEncodedElement> encodedRoutines;
  for ( ReosHydrographRoutingMethod *routine : mRoutingMethods )
    encodedRoutines.append( routine->encode() );

  element.addListEncodedData( QStringLiteral( "routines-method" ), encodedRoutines );

  element.addData( QStringLiteral( "current-routine-methode" ), mCurrentRoutingMethod );

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

ReosDirectHydrographRouting::ReosDirectHydrographRouting( ReosHydrographRoutingLink *routingLink ) : ReosHydrographRoutingMethod( routingLink )
{

}

void ReosDirectHydrographRouting::calculateOutputHydrograph( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosCalculationContext & )
{
  if ( !inputHydrograph )
    return;
  outputHydrograph->clear();
  outputHydrograph->copyFrom( inputHydrograph );
}

ReosHydrographCalculation *ReosDirectHydrographRouting::calculationProcess( ReosHydrograph *inputHydrograph, const ReosCalculationContext &context )
{
  return new Calculation( inputHydrograph );
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

ReosHydrographRoutingMethodFactories::ReosHydrographRoutingMethodFactories( ReosModule *parent ): ReosModule( parent )
{
  addFactory( new ReosDirectHydrographRoutingFactory );
  addFactory( new ReosMuskingumClassicRoutineFactory );
}

ReosHydraulicNetworkElement *ReosHydrographRoutingLinkFactory::decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const
{
  return ReosHydrographRoutingLink::decode( encodedElement, context );
}

ReosDirectHydrographRouting::Calculation::Calculation( ReosHydrograph *inputHydrograph )
{
  mInputHydrograph = std::make_unique<ReosHydrograph>();
  mInputHydrograph->copyFrom( inputHydrograph );
}

void ReosDirectHydrographRouting::Calculation::start()
{
  mHydrograph.reset( new ReosHydrograph );
  mHydrograph->copyFrom( mInputHydrograph.get() );
  mIsSuccessful = true;
}
