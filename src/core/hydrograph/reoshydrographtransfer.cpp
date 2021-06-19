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
#include "reosmuskingumclassicrouting.h"


ReosHydrographRoutingMethodFactories *ReosHydrographRoutingMethodFactories::sInstance = nullptr;

ReosHydrographRouting::ReosHydrographRouting( ReosHydraulicNetwork *parent ):
  ReosHydraulicLink( parent )
{
  mRoutingMethods.insert( ReosDirectHydrographRouting::typeString(), new ReosDirectHydrographRouting( this ) );
  mCurrentRoutingMethod = ReosDirectHydrographRouting::typeString();
  mOutputHydrograph = new ReosHydrograph( this );
  mOutputHydrograph->setColor( Qt::blue );
}

ReosHydrographRouting::ReosHydrographRouting( ReosHydrographSource *hydrographSource, ReosHydrographNode *destination, ReosHydraulicNetwork *parent ):
  ReosHydrographRouting( parent )
{
  setInputHydrographSource( hydrographSource );
  setHydrographDestination( destination );
}

bool ReosHydrographRouting::setCurrentRoutingMethod( const QString &routingType )
{
  if ( mCurrentRoutingMethod == routingType )
    return true;

  if ( mRoutingMethods.contains( routingType ) )
  {
    mCurrentRoutingMethod = routingType;
    return true;
  }

  ReosHydrographRoutingMethod *method = nullptr;
  if ( ReosHydrographRoutingMethodFactories::isInstantiate() )
    method = ReosHydrographRoutingMethodFactories::instance()->createRoutingMethod( routingType, this );

  if ( method )
  {
    mRoutingMethods.insert( routingType, method );
    mCurrentRoutingMethod = routingType;
    return true;
  }

  return false;
}

ReosHydrographRoutingMethod *ReosHydrographRouting::currentRoutingMethod() const
{
  auto it = mRoutingMethods.find( mCurrentRoutingMethod );
  if ( it != mRoutingMethods.end() )
    return it.value();

  return nullptr;
}

void ReosHydrographRouting::setInputHydrographSource( ReosHydrographSource *hydrographSource )
{
  if ( !mNode_1.isNull() )
  {
    disconnect( mNode_1, &ReosHydraulicNetworkElement::calculationIsUpdated, this, &ReosHydrographRouting::onSourceUpdated );
  }
  attachOnSide1( hydrographSource );

  if ( hydrographSource )
    connect( hydrographSource, &ReosHydraulicNetworkElement::calculationIsUpdated, this, &ReosHydrographRouting::onSourceUpdated );
}

ReosHydrographSource *ReosHydrographRouting::inputHydrographSource() const
{
  if ( mNode_1.isNull() )
    return nullptr;
  else
    return qobject_cast<ReosHydrographSource *>( mNode_1 );
}

ReosHydrographNode *ReosHydrographRouting::destinationNode() const
{
  if ( mNode_2.isNull() )
    return nullptr;
  else
    return qobject_cast<ReosHydrographNode *>( mNode_2 );
}

void ReosHydrographRouting::setHydrographDestination( ReosHydrographNode *destination )
{
  if ( destinationNode() )
    disconnect( this, &ReosHydraulicNetworkElement::calculationIsUpdated, destinationNode(), &ReosHydrographNode::onUpstreamRoutingUpdated );

  attachOnSide2( destination );
  if ( destination )
    connect( this, &ReosHydraulicNetworkElement::calculationIsUpdated, destinationNode(), &ReosHydrographNode::onUpstreamRoutingUpdated );
}

ReosHydrograph *ReosHydrographRouting::outputHydrograph() const
{
  return mOutputHydrograph;
}

void ReosHydrographRouting::updateCalculation( const ReosCalculationContext &context )
{
  mSourceUpdated = false;
  mOutputHydrograph->clear();
  inputHydrographSource()->updateCalculation( context );

  if ( mSourceUpdated )
  {
    ReosHydrographRoutingMethod *method = mRoutingMethods.value( mCurrentRoutingMethod, nullptr );
    if ( method )
      method->calculateOutputHydrograph( inputHydrographSource()->outputHydrograph(), mOutputHydrograph, context );

    calculationUpdated();
  }
}

void ReosHydrographRouting::onSourceUpdated()
{
  mSourceUpdated = true;
}


ReosHydrographRoutingMethod::ReosHydrographRoutingMethod( ReosHydrographRouting *routingLink ): ReosDataObject( routingLink ) {}

ReosDirectHydrographRouting::ReosDirectHydrographRouting( ReosHydrographRouting *routingLink ) : ReosHydrographRoutingMethod( routingLink )
{

}

void ReosDirectHydrographRouting::calculateOutputHydrograph( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosCalculationContext & )
{
  if ( !inputHydrograph )
    return;
  outputHydrograph->clear();
  outputHydrograph->referenceTime()->setValue( inputHydrograph->referenceTime()->value() );

  for ( int i = 0; i < inputHydrograph->valueCount(); ++i )
    outputHydrograph->setValue( inputHydrograph->relativeTimeAt( i ), inputHydrograph->valueAt( i ) );
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

ReosHydrographRoutingMethod *ReosHydrographRoutingMethodFactories::createRoutingMethod( const QString &type, ReosHydrographRouting *link )
{
  auto it = mFactories.find( type );
  if ( it != mFactories.end() )
    return it->second->createRoutingMethod( link );

  return nullptr;
}

ReosHydrographRoutingMethodFactories::ReosHydrographRoutingMethodFactories( ReosModule *parent ): ReosModule( parent )
{
  addFactory( new ReosMuskingumClassicRoutingFactory );
}
