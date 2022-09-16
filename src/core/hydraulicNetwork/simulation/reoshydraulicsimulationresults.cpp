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
#include "reosmesh.h"

ReosHydraulicSimulationResults::ReosHydraulicSimulationResults( const ReosHydraulicSimulation *simulation, QObject *parent )
  : ReosMeshDatasetSource( parent )
{
  mSimulationId = simulation->id();
}

QString ReosHydraulicSimulationResults::groupId( ReosHydraulicSimulationResults::DatasetType type )
{
  return groupId( groupIndex( type ) );
}

QString ReosHydraulicSimulationResults::groupId( int groupIndex ) const
{
  DatasetType dt = datasetType( groupIndex );

  switch ( dt )
  {
    case ReosHydraulicSimulationResults::DatasetType::None:
      return QStringLiteral( "none" );
    case ReosHydraulicSimulationResults::DatasetType::WaterLevel:
      return tr( "water-level" );
      break;
    case ReosHydraulicSimulationResults::DatasetType::WaterDepth:
      return tr( "water-depth" );
      break;
    case ReosHydraulicSimulationResults::DatasetType::Velocity:
      return tr( "velocity" );
      break;
  }

  return QString();
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

  return QString();
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

  return false;
}

double ReosHydraulicSimulationResults::interpolateResultOnMesh(
  ReosMesh *mesh,
  const ReosSpatialPosition &position,
  const QDateTime &time,
  ReosHydraulicSimulationResults::DatasetType dataType )
{
  int grInd = groupIndex( dataType );
  int dsInd = datasetIndexClosestBeforeTime( grInd, time );

  return mesh->interpolateDatasetValueOnPoint( this, position, grInd, dsInd );
}

QVector<double> ReosHydraulicSimulationResults::resultValues( ReosHydraulicSimulationResults::DatasetType datasetType, int index ) const
{
  int gi = groupIndex( datasetType );
  return datasetValues( gi, index );
}

