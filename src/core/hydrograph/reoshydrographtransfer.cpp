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


ReosHydrographRoutingMethodFactories *ReosHydrographRoutingMethodFactories::sInstance = nullptr;

ReosHydrographRoutineLink::ReosHydrographRoutineLink( ReosHydraulicNetwork *parent ):
  ReosHydraulicLink( parent )
{
  mRoutineMethods.insert( ReosDirectHydrographRoutine::staticType(), new ReosDirectHydrographRoutine( this ) );
  mCurrentRoutingMethod = ReosDirectHydrographRoutine::staticType();

  connect( mRoutineMethods.value( mCurrentRoutingMethod ), &ReosDataObject::dataChanged, this, &ReosHydrographRoutineLink::calculateRoutine );

  mOutputHydrograph = new ReosHydrograph( this );
  mOutputHydrograph->setColor( Qt::blue );
}

ReosHydrographRoutineLink::ReosHydrographRoutineLink( ReosHydrographSource *hydrographSource, ReosHydrographNode *destination, ReosHydraulicNetwork *parent ):
  ReosHydrographRoutineLink( parent )
{
  setInputHydrographSource( hydrographSource );
  setHydrographDestination( destination );
}


ReosHydrographRoutineLink::ReosHydrographRoutineLink( ReosHydrographSource *hydrographSource,
    ReosHydrographNode *destination,
    const ReosEncodedElement &encodedElement,
    ReosHydraulicNetwork *parent )
  : ReosHydraulicLink( encodedElement, parent )
{
  mOutputHydrograph = new ReosHydrograph( this );
  mOutputHydrograph->setColor( Qt::blue );

  setInputHydrographSource( hydrographSource );
  setHydrographDestination( destination );

  const QList<ReosEncodedElement> encodedRoutines = encodedElement.getListEncodedData( QStringLiteral( "routines-method" ) );
  for ( const ReosEncodedElement &encodedRoutine : encodedRoutines )
  {
    std::unique_ptr<ReosHydrographRoutineMethod> meth( ReosHydrographRoutingMethodFactories::instance()->createRoutingMethod( encodedRoutine, this ) );
    if ( meth )
    {
      QString type = meth->type();
      connect( meth.get(), &ReosDataObject::dataChanged, this, &ReosHydrographRoutineLink::calculateRoutine );
      mRoutineMethods.insert( type, meth.release() );
    }
  }

  encodedElement.getData( QStringLiteral( "current-routine-methode" ), mCurrentRoutingMethod );

}

bool ReosHydrographRoutineLink::setCurrentRoutingMethod( const QString &routingType )
{
  if ( mCurrentRoutingMethod == routingType )
    return true;

  if ( mRoutineMethods.contains( routingType ) )
  {
    mCurrentRoutingMethod = routingType;
    setObsolete();
    calculateRoutine();
    return true;
  }

  ReosHydrographRoutineMethod *method = nullptr;
  if ( ReosHydrographRoutingMethodFactories::isInstantiate() )
    method = ReosHydrographRoutingMethodFactories::instance()->createRoutingMethod( routingType, this );

  if ( method )
  {
    mRoutineMethods.insert( routingType, method );
    connect( method, &ReosDataObject::dataChanged, this, &ReosHydrographRoutineLink::calculateRoutine );
    registerUpstreamData( method );
    mCurrentRoutingMethod = routingType;
    setObsolete();
    calculateRoutine();
    return true;
  }

  return false;
}

ReosHydrographRoutineMethod *ReosHydrographRoutineLink::currentRoutingMethod() const
{
  auto it = mRoutineMethods.find( mCurrentRoutingMethod );
  if ( it != mRoutineMethods.end() )
    return it.value();

  return nullptr;
}

void ReosHydrographRoutineLink::setInputHydrographSource( ReosHydrographSource *hydrographSource )
{
  if ( !mNode_1.isNull() )
  {
    disconnect( mNode_1, &ReosHydraulicNetworkElement::calculationIsUpdated, this, &ReosHydrographRoutineLink::onSourceUpdated );
  }
  attachOnSide1( hydrographSource );

  if ( hydrographSource )
  {
    connect( hydrographSource, &ReosHydraulicNetworkElement::calculationIsUpdated, this, &ReosHydrographRoutineLink::onSourceUpdated );
  }
}

ReosHydrographSource *ReosHydrographRoutineLink::inputHydrographSource() const
{
  if ( mNode_1.isNull() )
    return nullptr;
  else
    return qobject_cast<ReosHydrographSource *>( mNode_1 );
}

ReosHydrographNode *ReosHydrographRoutineLink::destinationNode() const
{
  if ( mNode_2.isNull() )
    return nullptr;
  else
    return qobject_cast<ReosHydrographNode *>( mNode_2 );
}

void ReosHydrographRoutineLink::setHydrographDestination( ReosHydrographNode *destination )
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

ReosHydrograph *ReosHydrographRoutineLink::inputHydrograph() const
{
  return inputHydrographSource()->outputHydrograph();
}

ReosHydrograph *ReosHydrographRoutineLink::outputHydrograph() const
{
  return mOutputHydrograph;
}

void ReosHydrographRoutineLink::updateCalculationContext( const ReosCalculationContext &context )
{
  bool upstreamWillBeUpdated = false;

  if ( inputHydrographSource() )
    upstreamWillBeUpdated = inputHydrographSource()->updateCalculationContextFromDownstream( context, this );

  if ( !upstreamWillBeUpdated && isObsolete() )
  {
    calculateRoutine();
    upstreamWillBeUpdated = true;
  }

  if ( destinationNode() )
    destinationNode()->updateCalculationContextFromUpstream( context, this, upstreamWillBeUpdated );
}

void ReosHydrographRoutineLink::updateCalculationContextFromUpstream( const ReosCalculationContext &context, bool upstreamWillChange )
{
  if ( upstreamWillChange )
    mOutputHydrograph->setHydrographObsolete();
  else if ( isObsolete() )
  {
    calculateRoutine();
    upstreamWillChange = true;
  }

  destinationNode()->updateCalculationContextFromUpstream( context, this, upstreamWillChange );
}

bool ReosHydrographRoutineLink::updateCalculationContextFromDownstream( const ReosCalculationContext &context )
{
  bool upstreamWillChange = inputHydrographSource()->updateCalculationContextFromDownstream( context, this );

  if ( upstreamWillChange )
    mOutputHydrograph->setHydrographObsolete();
  else if ( isObsolete() )
  {
    calculateRoutine();
    upstreamWillChange = true;
  }

  return upstreamWillChange;
}


void ReosHydrographRoutineLink::calculateRoutine()
{
  if ( ! inputHydrographSource() )
    return;
  ReosHydrographRoutineMethod *method = mRoutineMethods.value( mCurrentRoutingMethod, nullptr );
  if ( method )
  {
    ReosCalculationContext context;
//    method->calculateOutputHydrograph( inputHydrographSource()->outputHydrograph(), mOutputHydrograph, context );
#ifndef _NDEBUG
    qDebug() << "calculation of link: " << name()->value();
#endif

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
      }
      calculation->deleteLater();
    } );

    calculation->startOnOtherThread();
  }
}


void ReosHydrographRoutineLink::onSourceUpdated()
{
  calculateRoutine();
}

void ReosHydrographRoutineLink::encodeData( ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const
{
  QList<ReosEncodedElement> encodedRoutines;
  for ( ReosHydrographRoutineMethod *routine : mRoutineMethods )
    encodedRoutines.append( routine->encode() );

  element.addListEncodedData( QStringLiteral( "routines-method" ), encodedRoutines );

  element.addData( QStringLiteral( "current-routine-methode" ), mCurrentRoutingMethod );

  ReosHydraulicLink::encodeData( element, context );
}

ReosHydrographRoutineLink *ReosHydrographRoutineLink::decode( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context )
{
  if ( encodedElement.description() != ReosHydrographRoutineLink::staticType() )
    return nullptr;

  const QPair<QString, QString> nodesId = ReosHydraulicLink::decodeNodesId( encodedElement );

  ReosHydrographSource *source = qobject_cast<ReosHydrographSource *>( context.network()->getElement( nodesId.first ) );
  ReosHydrographNode *destination = qobject_cast<ReosHydrographNode *>( context.network()->getElement( nodesId.second ) );

  if ( !source || !destination )
    return nullptr;

  std::unique_ptr<ReosHydrographRoutineLink> ret( new ReosHydrographRoutineLink( source, destination, encodedElement, context.network() ) );

  return ret.release();
}


ReosHydrographRoutineMethod::ReosHydrographRoutineMethod( ReosHydrographRoutineLink *routingLink ): ReosDataObject( routingLink ) {}

ReosDirectHydrographRoutine::ReosDirectHydrographRoutine( ReosHydrographRoutineLink *routingLink ) : ReosHydrographRoutineMethod( routingLink )
{

}

void ReosDirectHydrographRoutine::calculateOutputHydrograph( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosCalculationContext & )
{
  if ( !inputHydrograph )
    return;
  outputHydrograph->clear();
  outputHydrograph->copyFrom( inputHydrograph );
}

ReosHydrographCalculation *ReosDirectHydrographRoutine::calculationProcess( ReosHydrograph *inputHydrograph, const ReosCalculationContext &context )
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

ReosHydrographRoutineMethod *ReosHydrographRoutingMethodFactories::createRoutingMethod( const QString &type, ReosHydrographRoutineLink *link )
{
  auto it = mFactories.find( type );
  if ( it != mFactories.end() )
    return it->second->createRoutingMethod( link );

  return nullptr;
}

ReosHydrographRoutineMethod *ReosHydrographRoutingMethodFactories::createRoutingMethod( const ReosEncodedElement &encodedElement, ReosHydrographRoutineLink *link )
{
  auto it = mFactories.find( encodedElement.description() );
  if ( it != mFactories.end() )
    return it->second->createRoutingMethod( encodedElement, link );

  return nullptr;
}

ReosHydrographRoutingMethodFactories::ReosHydrographRoutingMethodFactories( ReosModule *parent ): ReosModule( parent )
{
  addFactory( new ReosMuskingumClassicRoutineFactory );
}

ReosHydraulicNetworkElement *ReosHydrographRoutineLinkFactory::decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const
{
  return ReosHydrographRoutineLink::decode( encodedElement, context );
}
