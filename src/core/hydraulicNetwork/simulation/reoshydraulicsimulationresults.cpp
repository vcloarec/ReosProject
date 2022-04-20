/***************************************************************************
  reoshydraulicsimulationresults.cpp - ReosHydraulicSimulationResults

 ---------------------
 begin                : 1.4.2022
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
#include "reoshydraulicsimulationresults.h"

#include "reoshydraulicsimulation.h"

ReosHydraulicSimulationResults::ReosHydraulicSimulationResults( const ReosHydraulicSimulation *simulation, QObject *parent )
  : ReosMeshDatasetSource( parent )
{
  mSimulationId = simulation->id();
}

QString ReosHydraulicSimulationResults::groupId( int groupIndex ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return QString();

  return mSimulationId + ':' + groupName( groupIndex );
}

QString ReosHydraulicSimulationResults::groupName( int groupIndex ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return QString();

  DatasetType dt = datasetType( groupIndex );

  switch ( dt )
  {
    case ReosHydraulicSimulationResults::DatasetType::None:
      return QStringLiteral( "(None)" );
    case ReosHydraulicSimulationResults::DatasetType::WaterLevel:
      return tr( "Water level" );
      break;
    case ReosHydraulicSimulationResults::DatasetType::WaterDepth:
      return tr( "Water depth" );
      break;
    case ReosHydraulicSimulationResults::DatasetType::Velocity:
      return tr( "Velocity" );
      break;
  }
}

bool ReosHydraulicSimulationResults::groupIsScalar( int groupIndex ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return true;

  DatasetType dt = datasetType( groupIndex );

  switch ( dt )
  {
    case ReosHydraulicSimulationResults::DatasetType::WaterLevel:
      return true;
      break;
    case ReosHydraulicSimulationResults::DatasetType::WaterDepth:
      return true;
      break;
    case ReosHydraulicSimulationResults::DatasetType::Velocity:
      return false;
      break;
  }
}
