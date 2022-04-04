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

ReosHydraulicSimulationResults::ReosHydraulicSimulationResults( QObject *parent ): ReosMeshDatasetSource( parent )
{

}

QString ReosHydraulicSimulationResults::groupName( int groupIndex ) const
{
  DatasetType dt = datasetType( groupIndex );

  switch ( dt )
  {
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
