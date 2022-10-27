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
#include "reostimeseriesgroup.h"
#include "reoshydraulicscheme.h"

ReosHydraulicStructureBoundaryCondition::ReosHydraulicStructureBoundaryCondition(
  ReosHydraulicStructure2D *hydStructure,
  const QString &boundaryConditionId,
  const ReosHydraulicNetworkContext &context )
  : ReosHydrographJunction( QPointF(), context.network() )
  , mContext( context )
  , mBoundaryConditionId( boundaryConditionId )
{
  init();
  attachStructure( hydStructure );
}

ReosHydraulicStructureBoundaryCondition::ReosHydraulicStructureBoundaryCondition(
  ReosHydraulicStructure2D *hydStructure,
  const QString &boundaryConditionId,
  const ReosSpatialPosition &position,
  const ReosHydraulicNetworkContext &context )
  : ReosHydraulicStructureBoundaryCondition( hydStructure, boundaryConditionId, context )
{
  mPositionOnStructure = false;
  ReosHydrographJunction::setPosition( position );
}


ReosHydraulicStructureBoundaryCondition::ReosHydraulicStructureBoundaryCondition(
  const ReosEncodedElement &encodedElement,
  ReosHydraulicNetwork *parent )
  : ReosHydrographJunction( encodedElement, parent )
  , mContext( parent->context() )
{
  init();
  encodedElement.getData( QStringLiteral( "boundary-condition-id" ), mBoundaryConditionId );
  mWaterLevelSeriesGroup->decode( encodedElement.getEncodedData( QStringLiteral( "water-level-series" ) ), mContext.encodeContext() );
}


void ReosHydraulicStructureBoundaryCondition::init()
{
  mWaterLevelSeriesGroup = new ReosTimeSeriesVariableTimeStepGroup( this );
  mIsWaterLevelConstant = new ReosParameterBoolean( tr( "Constant level" ), false, this );
  mConstantWaterLevel = new ReosParameterDouble( tr( "Constant level value" ), false, this );
  mIsWaterLevelConstant->setValue( true );
  mConstantWaterLevel->setValue( 0 );

  connect( elementName(), &ReosParameter::valueChanged, this, &ReosHydraulicStructureBoundaryCondition::onParameterNameChange );

  connect( mWaterLevelSeriesGroup, &ReosDataObject::dataChanged, this, &ReosHydraulicStructureBoundaryCondition::dataChanged );
  connect( mIsWaterLevelConstant, &ReosParameter::valueChanged, this, &ReosHydraulicStructureBoundaryCondition::dataChanged );
  connect( mConstantWaterLevel, &ReosParameter::valueChanged, this, &ReosHydraulicStructureBoundaryCondition::dataChanged );

  connect( mWaterLevelSeriesGroup, &ReosDataObject::dataChanged, this, &ReosHydraulicStructureBoundaryCondition::dirtied );
  connect( mWaterLevelSeriesGroup, &ReosTimeSeriesVariableTimeStepGroup::serieChanged, this, &ReosHydraulicStructureBoundaryCondition::dirtied );
  connect( mIsWaterLevelConstant, &ReosParameter::valueChanged, this, &ReosHydraulicStructureBoundaryCondition::dirtied );
  connect( mConstantWaterLevel, &ReosParameter::valueChanged, this, &ReosHydraulicStructureBoundaryCondition::dirtied );
}

void ReosHydraulicStructureBoundaryCondition::syncHydrographName()
{
  mOutputHydrograph->setName( outputPrefixName() + QStringLiteral( " %1" ).arg( elementName()->value() ) );
}

void ReosHydraulicStructureBoundaryCondition::loadHydrographResult( const ReosCalculationContext &calculationContext )
{
  mOutputHydrograph->clear();
}

void ReosHydraulicStructureBoundaryCondition::encodeData( ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const
{
  encodedElement.addData( QStringLiteral( "boundary-condition-id" ), mBoundaryConditionId );
  encodedElement.addEncodedData( QStringLiteral( "water-level-series" ), mWaterLevelSeriesGroup->encode( context.encodeContext() ) );
  ReosHydrographJunction::encodeData( encodedElement, context );
}

ReosHydraulicStructureBoundaryCondition *ReosHydraulicStructureBoundaryCondition::decode( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context )
{
  return new ReosHydraulicStructureBoundaryCondition( encodedElement, context.network() );
}

QPointF ReosHydraulicStructureBoundaryCondition::position( const QString &destinationCrs ) const
{
  if ( mPositionOnStructure )
  {
    if ( !mStructure.isNull() )
      return mStructure->geometryStructure()->boundaryConditionCenter( mBoundaryConditionId, destinationCrs );
  }

  return ReosHydrographJunction::position( destinationCrs );
}

ReosSpatialPosition ReosHydraulicStructureBoundaryCondition::spatialPosition() const
{
  if ( !mStructure.isNull() )
    return ReosSpatialPosition( mStructure->geometryStructure()->boundaryConditionCenter( mBoundaryConditionId, QString() ),
                                mStructure->geometryStructure()->crs() );

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
    case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
      break;
  }
}

void ReosHydraulicStructureBoundaryCondition::saveConfiguration( ReosHydraulicScheme *scheme ) const
{
  ReosEncodedElement encodedElement = scheme->restoreElementConfig( id() );

  encodedElement.addData( QStringLiteral( "default-condition-type" ), static_cast<int>( mDefaultConditionType ) );
  encodedElement.addData( QStringLiteral( "is-water-elevation-constant" ), mIsWaterLevelConstant->value() ? 1 : 0 );
  encodedElement.addData( QStringLiteral( "constant-water-elevation" ), mConstantWaterLevel->value() );

  QString waterlevelSeriesId;
  if ( mWaterLevelSeriesIndex >= 0 && mWaterLevelSeriesIndex < mWaterLevelSeriesGroup->timeSeriesCount() )
    waterlevelSeriesId = mWaterLevelSeriesGroup->timeSeries( mWaterLevelSeriesIndex )->id();
  encodedElement.addData( QStringLiteral( "water-level-series-id" ), waterlevelSeriesId );

  scheme->saveElementConfig( id(), encodedElement );

  ReosHydrographJunction::saveConfiguration( scheme );

}

void ReosHydraulicStructureBoundaryCondition::restoreConfiguration( ReosHydraulicScheme *scheme )
{
  ReosEncodedElement encodedElement = scheme->restoreElementConfig( id() );

  int defaultCond = 0;
  int isLevelConstant = 0;
  double constantLevel = 0;
  QString waterlevelSeriesId;

  encodedElement.getData( QStringLiteral( "default-condition-type" ), defaultCond );
  encodedElement.getData( QStringLiteral( "is-water-elevation-constant" ), isLevelConstant );
  encodedElement.getData( QStringLiteral( "constant-water-elevation" ), constantLevel );
  encodedElement.getData( QStringLiteral( "water-level-series-id" ), waterlevelSeriesId );

  mDefaultConditionType = static_cast<ReosHydraulicStructureBoundaryCondition::Type>( defaultCond );
  mIsWaterLevelConstant->setValue( isLevelConstant == 1 );
  mConstantWaterLevel->setValue( constantLevel );
  mWaterLevelSeriesIndex = -1;
  for ( int i = 0; i < mWaterLevelSeriesGroup->timeSeriesCount(); ++i )
    if ( mWaterLevelSeriesGroup->timeSeries( i )->id() == waterlevelSeriesId )
    {
      mWaterLevelSeriesIndex = i;
      break;
    }

  ReosHydrographJunction::restoreConfiguration( scheme );

  syncHydrographName();
}

QString ReosHydraulicStructureBoundaryCondition::outputPrefixName() const
{
  switch ( conditionType() )
  {
    case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
      break;
    case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
      return tr( "Input flow from" );
      break;
    case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
      return tr( "Output flow at" );
      break;
  }

  return QString();
}

QString ReosHydraulicStructureBoundaryCondition::boundaryConditionId() const
{
  return mBoundaryConditionId;
}

void ReosHydraulicStructureBoundaryCondition::attachStructure( ReosHydraulicStructure2D *structure )
{
  mStructure = structure;
  connect( mStructure->geometryStructure(), &ReosPolylinesStructure::classesChanged, this, &ReosHydraulicStructureBoundaryCondition::onBoundaryClassesChange );
  connect( mStructure->geometryStructure(), &ReosPolylinesStructure::geometryChanged, this, &ReosHydraulicNetworkElement::positionChanged );
  elementName()->blockSignals( true );
  elementName()->setValue( mStructure->geometryStructure()->value( mBoundaryConditionId ).toString() );
  elementName()->blockSignals( false );
}

ReosHydraulicStructureBoundaryCondition::Type ReosHydraulicStructureBoundaryCondition::conditionType() const
{
  switch ( connectionState() )
  {
    case ReosHydraulicStructureBoundaryCondition::ConnectionState::NotConnected:
      return mDefaultConditionType;
      break;
    case ReosHydraulicStructureBoundaryCondition::ConnectionState::ConnectedToUpstreamLink:
      return Type::OutputLevel;
      break;
    case ReosHydraulicStructureBoundaryCondition::ConnectionState::ConnectedToDownstreamLink:
      return Type::InputFlow;
      break;
  }

  return mDefaultConditionType;
}

ReosHydraulicStructureBoundaryCondition::ConnectionState ReosHydraulicStructureBoundaryCondition::connectionState() const
{
  if ( !linksBySide2().isEmpty() )
    return ConnectionState::ConnectedToDownstreamLink;

  if ( !mLinksBySide1.isEmpty() )
    return ConnectionState::ConnectedToUpstreamLink;

  return ConnectionState::NotConnected;
}

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

ReosHydraulicStructure2D *ReosHydraulicStructureBoundaryCondition::structure() const
{
  return mStructure;
}

void ReosHydraulicStructureBoundaryCondition::setWaterLevelSeriesIndex( int waterLevelSeriesIndex )
{
  mWaterLevelSeriesIndex = waterLevelSeriesIndex;
  emit dirtied();
  emit dataChanged();
}

ReosTimeSerieVariableTimeStep *ReosHydraulicStructureBoundaryCondition::waterLevelSeries() const
{
  return mWaterLevelSeriesGroup->timeSeries( mWaterLevelSeriesIndex );
}

int ReosHydraulicStructureBoundaryCondition::waterLevelSeriesIndex() const
{
  return mWaterLevelSeriesIndex;
}

ReosTimeSeriesVariableTimeStepGroup *ReosHydraulicStructureBoundaryCondition::waterLevelSeriesGroup() const
{
  return mWaterLevelSeriesGroup;
}

ReosParameterDouble *ReosHydraulicStructureBoundaryCondition::constantWaterElevation() const
{
  return mConstantWaterLevel;
}

ReosParameterBoolean *ReosHydraulicStructureBoundaryCondition::isWaterLevelConstant() const
{
  return mIsWaterLevelConstant;
}

ReosHydraulicStructureBoundaryCondition::Type ReosHydraulicStructureBoundaryCondition::defaultConditionType() const
{
  return mDefaultConditionType;
}

void ReosHydraulicStructureBoundaryCondition::setDefaultConditionType( const ReosHydraulicStructureBoundaryCondition::Type &defaultConditionType )
{
  if ( mDefaultConditionType == defaultConditionType )
    return;

  mDefaultConditionType = defaultConditionType;
  emit dirtied();

  switch ( mDefaultConditionType )
  {
    case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
      setCurrentInternalHydrograph( nullptr );
      break;
    case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
      updateInternalHydrograph();
      break;
    case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
      setCurrentInternalHydrograph( nullptr );
      break;
  }

  syncHydrographName();
  emit dataChanged();
}

ReosHydraulicNetworkElement *ReosHydraulicStructureBoundaryConditionFactory::decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const
{
  if ( encodedElement.description() != ReosHydraulicStructureBoundaryCondition::staticType() )
    return nullptr;

  return ReosHydraulicStructureBoundaryCondition::decode( encodedElement, context );
}
