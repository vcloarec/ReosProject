/***************************************************************************
  reossimulationinitialcondition.cpp - ReosSimulationInitialCondition

 ---------------------
 begin                : 29.3.2022
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
#include "reossimulationinitialcondition.h"

#include "reosparameter.h"

ReosSimulationInitialConditions::ReosSimulationInitialConditions( QObject *parent )
  : ReosDataObject( parent )
{
  mInitialWaterLevel = new ReosParameterDouble( tr( "Initial water level" ), false, this );
  mInitialWaterLevel->setValue( 4 );
}

ReosSimulationInitialConditions::ReosSimulationInitialConditions( const ReosEncodedElement &element, QObject *parent )
  : ReosDataObject( parent )
{
  mInitialWaterLevel = ReosParameterDouble::decode( element.getEncodedData(
                         QStringLiteral( "initial-water-level" ) ),
                       false, tr( "Initial water level" ), this );
}

ReosEncodedElement ReosSimulationInitialConditions::encode() const
{
  ReosEncodedElement element( QStringLiteral( "intital-conditions" ) );
  element.addEncodedData( QStringLiteral( "initial-water-level" ), mInitialWaterLevel->encode() );

  return element;
}

ReosParameterDouble *ReosSimulationInitialConditions::initialWaterLevel() const
{
  return mInitialWaterLevel;
}
