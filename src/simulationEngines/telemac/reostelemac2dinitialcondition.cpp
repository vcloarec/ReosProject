/***************************************************************************
  reostelemac2dinitialcondition.cpp - ReosTelemac2DInitialCondition

 ---------------------
 begin                : 28.4.2022
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
#include "reostelemac2dinitialcondition.h"


#include "reosparameter.h"
#include "reoshydraulicscheme.h"

ReosTelemac2DInitialCondition::ReosTelemac2DInitialCondition( QObject *parent )
  : ReosDataObject( parent )
{
}

ReosTelemac2DInitialCondition::ReosTelemac2DInitialCondition( const ReosEncodedElement &element, QObject *parent )
  : ReosDataObject( parent )
{
  ReosDataObject::decode( element );
}

ReosParameterDouble *ReosTelemac2DInitialConstantWaterLevel::initialWaterLevel() const
{
  return mInitialWaterLevel;
}

void ReosTelemac2DInitialConstantWaterLevel::saveConfiguration( ReosHydraulicScheme *scheme ) const
{
  ReosEncodedElement elem = scheme->restoreElementConfig( id() );
  elem.addData( QStringLiteral( "initial-water-level" ), mInitialWaterLevel->value() );

  scheme->saveElementConfig( id(), elem );
}

void ReosTelemac2DInitialConstantWaterLevel::restoreConfiguration( ReosHydraulicScheme *scheme )
{
  ReosEncodedElement elem = scheme->restoreElementConfig( id() );
  double iwl = 0;
  elem.getData( QStringLiteral( "initial-water-level" ), iwl );

  mInitialWaterLevel->setValue( iwl );
}

ReosTelemac2DInitialConstantWaterLevel::ReosTelemac2DInitialConstantWaterLevel( QObject *parent )
  : ReosTelemac2DInitialCondition( parent )
{
  mInitialWaterLevel = new ReosParameterDouble( tr( "Initial water level" ), false, this );
  mInitialWaterLevel->setValue( 4 );
}

ReosTelemac2DInitialConstantWaterLevel::ReosTelemac2DInitialConstantWaterLevel( const ReosEncodedElement &element, QObject *parent )
  : ReosTelemac2DInitialCondition( element, parent )
{
  mInitialWaterLevel = new ReosParameterDouble( tr( "Initial water level" ), false, this );
  ReosDataObject::decode( element );
}

ReosEncodedElement ReosTelemac2DInitialConstantWaterLevel::encode() const
{
  ReosEncodedElement element( QStringLiteral( "telemac-2d-initial-condition-constant-water-level" ) );
  ReosDataObject::encode( element );
  return element;
}

ReosTelemac2DInitialConditionFromSimulation::ReosTelemac2DInitialConditionFromSimulation( QObject *parent )
  : ReosTelemac2DInitialCondition( parent )
{
}

ReosTelemac2DInitialConditionFromSimulation::ReosTelemac2DInitialConditionFromSimulation( const ReosEncodedElement &element, QObject *parent )
  : ReosTelemac2DInitialCondition( parent )
{
  ReosDataObject::decode( element );
}

ReosEncodedElement ReosTelemac2DInitialConditionFromSimulation::encode() const
{
  ReosEncodedElement element( QStringLiteral( "telemac-2d-initial-condition-from-simulation" ) );
  ReosDataObject::encode( element );
  return element;
}

void ReosTelemac2DInitialConditionFromSimulation::saveConfiguration( ReosHydraulicScheme *scheme ) const
{
  ReosEncodedElement element = scheme->restoreElementConfig( id() );

  element.addData( QStringLiteral( "scheme-id" ), mOtherSchemeId );
  element.addData( QStringLiteral( "time-step-index" ), mTimeStepIndex );

  scheme->saveElementConfig( id(), element );
}

void ReosTelemac2DInitialConditionFromSimulation::restoreConfiguration( ReosHydraulicScheme *scheme )
{
  ReosEncodedElement element = scheme->restoreElementConfig( id() );

  element.getData( QStringLiteral( "scheme-id" ), mOtherSchemeId );
  element.getData( QStringLiteral( "time-step-index" ), mTimeStepIndex );
}

QString ReosTelemac2DInitialConditionFromSimulation::otherSchemeId() const
{
  return mOtherSchemeId;
}

void ReosTelemac2DInitialConditionFromSimulation::setOtherSchemeId( const QString &otherSchemeId )
{
  mOtherSchemeId = otherSchemeId;
}

int ReosTelemac2DInitialConditionFromSimulation::timeStepIndex() const
{
  return mTimeStepIndex;
}

void ReosTelemac2DInitialConditionFromSimulation::setTimeStepIndex( int timeStepIndex )
{
  mTimeStepIndex = timeStepIndex;
}
