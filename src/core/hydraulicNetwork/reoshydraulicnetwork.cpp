/***************************************************************************
  reoshydraulicnetwork.cpp - ReosHydraulicNetwork

 ---------------------
 begin                : 20.5.2021
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
#include "reoshydraulicnetwork.h"
#include "reoshydraulicnode.h"
#include "reoshydrauliclink.h"
#include "reoshydrographtransfer.h"
#include <QUuid>

ReosHydraulicNetworkElement::ReosHydraulicNetworkElement( ReosHydraulicNetwork *parent ):
  ReosDataObject( parent )
  , mNetWork( parent )
  , mNameParameter( new ReosParameterString( tr( "Name" ), false, this ) )
{
  mUid = QUuid::createUuid().toString();
  mConstantTimeStepInTable = new ReosParameterDuration( tr( "Constant time step" ) );
  mConstantTimeStepInTable->setValue( ReosDuration( 5, ReosDuration::minute ) );
  mUseConstantTimeStepInTable = new ReosParameterBoolean( tr( "Use constant time step" ) );
}

ReosHydraulicNetworkElement::ReosHydraulicNetworkElement( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent )
  : ReosDataObject( parent )
  , mNetWork( parent )
  , mNameParameter( ReosParameterString::decode( encodedElement.getEncodedData( QStringLiteral( "name-parameter" ) ), false, tr( "Name" ), this ) )
{
  encodedElement.getData( QStringLiteral( "UID" ), mUid );

  mConstantTimeStepInTable = ReosParameterDuration::decode( encodedElement.getEncodedData( QStringLiteral( "constant-time-step-in-table" ) ),
                             false,
                             tr( "Constant time step" ),
                             this );

  if ( !mConstantTimeStepInTable->isValid() )
    mConstantTimeStepInTable->setValue( ReosDuration( 5, ReosDuration::minute ) );

  mUseConstantTimeStepInTable = ReosParameterBoolean::decode( encodedElement.getEncodedData( QStringLiteral( "use-constant-time-step-in-table" ) ),
                                false,
                                tr( "Use constant time step" ),
                                this );
  if ( !mUseConstantTimeStepInTable->isValid() )
    mUseConstantTimeStepInTable->setValue( false );
}


ReosHydraulicNetworkElement::~ReosHydraulicNetworkElement()
{
}

QString ReosHydraulicNetworkElement::id() const
{
  return type() + QString( ':' ) + mUid;
}

void ReosHydraulicNetworkElement::destroy()
{
  if ( !mNetWork.isNull() )
    mNetWork->mElements.remove( id() );
  deleteLater();
}

void ReosHydraulicNetworkElement::positionChanged()
{
  if ( mNetWork )
    mNetWork->elemPositionChangedPrivate( this );
}

ReosParameterString *ReosHydraulicNetworkElement::name() const
{
  return mNameParameter;
}

ReosParameterDuration *ReosHydraulicNetworkElement::constantTimeStepInTable() const
{
  return mConstantTimeStepInTable;
}

ReosParameterBoolean *ReosHydraulicNetworkElement::useConstantTimeStepInTable() const
{
  return mUseConstantTimeStepInTable;
}

ReosEncodedElement ReosHydraulicNetworkElement::encode( const ReosHydraulicNetworkContext &context ) const
{
  ReosEncodedElement element( type() );
  element.addData( QStringLiteral( "UID" ), mUid );
  element.addEncodedData( QStringLiteral( "name-parameter" ), mNameParameter->encode() );
  element.addEncodedData( QStringLiteral( "constant-time-step-in-table" ), mConstantTimeStepInTable->encode() );
  element.addEncodedData( QStringLiteral( "use-constant-time-step-in-table" ), mUseConstantTimeStepInTable->encode() );

  encodeData( element, context );

  return element;
}


ReosHydraulicNetwork::ReosHydraulicNetwork( ReosModule *parent, ReosWatershedModule *watershedModule )
  : ReosModule( parent )
  , mWatershedModule( watershedModule )
{
  ReosHydrographRoutingMethodFactories::instantiate( this );

  mElementFactories.emplace( ReosHydrographNodeWatershed::staticType(), new ReosHydrographNodeWatershedFactory );
  mElementFactories.emplace( ReosHydrographJunction::staticType(), new ReosHydrographJunctionFactory );

  mElementFactories.emplace( ReosHydrographRoutingLink::staticType(), new ReosHydrographRoutingLinkFactory );
}

QList<ReosHydraulicNetworkElement *> ReosHydraulicNetwork::getElements( const QString &type ) const
{
  QList<ReosHydraulicNetworkElement *> elems;
  for ( ReosHydraulicNetworkElement *elem : mElements )
    if ( elem->type() == type )
      elems.append( elem );

  return elems;
}

ReosHydraulicNetworkElement *ReosHydraulicNetwork::getElement( const QString &elemId ) const
{
  if ( mElements.contains( elemId ) )
    return mElements.value( elemId );
  else
    return nullptr;
}

ReosHydraulicNetworkElement *ReosHydraulicNetwork::addElement( ReosHydraulicNetworkElement *elem )
{
  mElements.insert( elem->id(), elem );
  if ( !elem->name()->isValid() )
  {
    int index = mElementIndexesCounter.value( elem->type(), 0 ) + 1;
    mElementIndexesCounter[ elem->type()] = index;
    elem->name()->setValue( ( elem->defaultDisplayName() + QStringLiteral( " %1" ) ).arg( index ) );
  }
  emit elementAdded( elem );

  return elem;
}

void ReosHydraulicNetwork::removeElement( ReosHydraulicNetworkElement *elem )
{
  emit elementRemoved( elem );
  ReosHydraulicNode *node = qobject_cast<ReosHydraulicNode *>( elem );
  if ( node )
    for ( ReosHydraulicLink *link : node->links() )
      removeElement( link );

  elem->destroy();
}

void ReosHydraulicNetwork::decode( const ReosEncodedElement &element )
{
  if ( element.description() != QStringLiteral( "hydraulic-network" ) )
    return;

  element.getData( QStringLiteral( "elements-counter" ), mElementIndexesCounter );

  QList<ReosEncodedElement> encodedElements = element.getListEncodedData( QStringLiteral( "hydraulic-element" ) );

  for ( const ReosEncodedElement &encodedElement : encodedElements )
  {
    if ( encodedElement.description().contains( ReosHydraulicNode::staticType() ) )
    {
      addEncodedElement( encodedElement );
    }
  }

  for ( const ReosEncodedElement &encodedElement : encodedElements )
  {
    if ( encodedElement.description().contains( ReosHydraulicLink::staticType() ) )
    {
      addEncodedElement( encodedElement );
    }
  }

}

ReosEncodedElement ReosHydraulicNetwork::encode() const
{
  ReosEncodedElement element( QStringLiteral( "hydraulic-network" ) );

  QList<ReosEncodedElement> encodedElements;


  for ( ReosHydraulicNetworkElement *elem : mElements )
  {
    encodedElements.append( elem->encode( context() ) );
  }

  element.addListEncodedData( QStringLiteral( "hydraulic-element" ), encodedElements );
  element.addData( QStringLiteral( "elements-counter" ), mElementIndexesCounter );

  return element;
}

void ReosHydraulicNetwork::elemPositionChangedPrivate( ReosHydraulicNetworkElement *elem )
{
  emit elementPositionHasChanged( elem );
}

void ReosHydraulicNetwork::addEncodedElement( const ReosEncodedElement &element )
{
  auto it = mElementFactories.find( element.description() );
  if ( it == mElementFactories.end() )
    return;

  ReosHydraulicNetworkElement *elem = it->second->decodeElement( element, context() );
  if ( elem )
    addElement( elem );
}

ReosWatershedModule *ReosHydraulicNetworkContext::watershedModule() const
{
  return mWatershedModule;
}

ReosHydraulicNetwork *ReosHydraulicNetworkContext::network() const
{
  return mNetwork;
}

ReosHydraulicNetworkElementFactory::ReosHydraulicNetworkElementFactory()
{

}

ReosHydraulicNetworkElementFactory::~ReosHydraulicNetworkElementFactory() {}
