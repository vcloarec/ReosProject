/***************************************************************************
  reoshydraulicstructureboundarycondition.cpp - ReosHydraulicStructureBoundaryCondition

 ---------------------
 begin                : 23.3.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reoshydraulicstructureboundarycondition.h"

#include "reoshydraulicstructure2d.h"
#include "reoshydrographrouting.h"

ReosHydraulicStructureBoundaryCondition::ReosHydraulicStructureBoundaryCondition( ReosHydraulicStructure2D *hydStructure, const QString &boundaryConditionId, const ReosHydraulicNetworkContext &context )
  : ReosHydrographJunction( QPointF(), context.network() )
  , mContext( context )
  , mStructure( hydStructure )
  , mBoundaryConditionId( boundaryConditionId )
{
  elementName()->setValue( mStructure->geometryStructure()->value( mBoundaryConditionId ).toString() );
  connect( hydStructure->geometryStructure(), &ReosPolylinesStructure::classesChanged, this, &ReosHydraulicStructureBoundaryCondition::onBoundaryClassesChange );
  connect( elementName(), &ReosParameter::valueChanged, this, &ReosHydraulicStructureBoundaryCondition::onParameterNameChange );
}


ReosHydraulicStructureBoundaryCondition::ReosHydraulicStructureBoundaryCondition(
  const ReosEncodedElement &encodedElement,
  ReosHydraulicNetwork *parent )
  : ReosHydrographJunction( encodedElement, parent )
  , mContext( parent->context() )
{
  encodedElement.getData( QStringLiteral( "boundary-condition-id" ), mBoundaryConditionId );
}

void ReosHydraulicStructureBoundaryCondition::encodeData( ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const
{
  encodedElement.addData( QStringLiteral( "boundary-condition-id" ), mBoundaryConditionId );

  ReosHydrographJunction::encodeData( encodedElement, context );
}

ReosHydraulicStructureBoundaryCondition *ReosHydraulicStructureBoundaryCondition::decode( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context )
{
  return new ReosHydraulicStructureBoundaryCondition( encodedElement, context.network() );
}

QPointF ReosHydraulicStructureBoundaryCondition::position( const QString &destinationCrs ) const
{
  if ( !mStructure.isNull() )
    return mStructure->geometryStructure()->boundaryConditionCenter( mBoundaryConditionId, destinationCrs );

  return QPointF();
}

bool ReosHydraulicStructureBoundaryCondition::isAutoSelectable() const
{
  return false;
}

bool ReosHydraulicStructureBoundaryCondition::canAcceptLink( const QString &linkId, int positionInLink )
{
  if ( !linkId.contains( ReosHydrographRoutingLink::staticType() ) )
    return false;

  if ( !linksBySide1().empty() )
    return false;

  if ( !linksBySide2().empty() )
  {
    return positionInLink == 1;
  }

  return true;
}

void ReosHydraulicStructureBoundaryCondition::updateCalculationContextFromUpstream(
  const ReosCalculationContext &context,
  ReosHydrographRoutingLink *upstreamLink,
  bool upstreamWillChange )
{
  switch ( conditionType() )
  {
    case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
      ReosHydrographJunction::updateCalculationContextFromUpstream( context, upstreamLink, upstreamWillChange );
      mStructure->updateCalculationContextFromUpstream( context, this, upstreamWillChange );
      break;
    case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
      break;
  }
}

QString ReosHydraulicStructureBoundaryCondition::boundaryConditionId() const
{
  return mBoundaryConditionId;
}

void ReosHydraulicStructureBoundaryCondition::attachStructure( ReosHydraulicStructure2D *structure )
{
  mStructure = structure;
}

ReosHydraulicStructureBoundaryCondition::Type ReosHydraulicStructureBoundaryCondition::conditionType() const
{
  if ( mLinksBySide1.isEmpty() && !linksBySide2().isEmpty() )
    return Type::InputFlow;

  return Type::OutputLevel;
}

//void ReosHydraulicStructureBoundaryCondition::updateCalculationContext( const ReosCalculationContext &context )
//{

//}

void ReosHydraulicStructureBoundaryCondition::onBoundaryClassesChange()
{
  disconnect( elementName(), &ReosParameter::valueChanged, this, &ReosHydraulicStructureBoundaryCondition::onParameterNameChange );
  elementName()->setValue( mStructure->geometryStructure()->value( mBoundaryConditionId ).toString() );
  connect( elementName(), &ReosParameter::valueChanged, this, &ReosHydraulicStructureBoundaryCondition::onParameterNameChange );
}

void ReosHydraulicStructureBoundaryCondition::onParameterNameChange()
{
  disconnect( mStructure->geometryStructure(), &ReosPolylinesStructure::classesChanged, this, &ReosHydraulicStructureBoundaryCondition::onBoundaryClassesChange );
  mStructure->geometryStructure()->changeClassValue( mBoundaryConditionId, elementName()->value() );
  connect( mStructure->geometryStructure(), &ReosPolylinesStructure::classesChanged, this, &ReosHydraulicStructureBoundaryCondition::onBoundaryClassesChange );
}

ReosHydraulicNetworkElement *ReosHydraulicStructureBoundaryConditionFactory::decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const
{
  if ( encodedElement.description() != ReosHydraulicStructureBoundaryCondition::staticType() )
    return nullptr;

  return ReosHydraulicStructureBoundaryCondition::decode( encodedElement, context );
}
