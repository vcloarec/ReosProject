/***************************************************************************
  reoshecrassimulation.cpp - ReosHecRasSimulation

 ---------------------
 begin                : 06.10.2022
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

#include "reoshecrassimulation.h"
#include "reoscore.h"

#include <QFileInfo>

REOSEXTERN ReosSimulationEngineFactory *engineSimulationFactory()
{
  return new ReosHecRasSimulationEngineFactory();
}

void ReosHecRasSimulationEngineFactory::initializeSettings()
{
}

ReosHecRasStructureImporter::ReosHecRasStructureImporter( const QString &file )
  : ReosStructureImporter()
{
  QFileInfo fileInfo( file );
  mIsValid = fileInfo.exists();
  if ( mIsValid )
  {
    mProject.reset( new ReosHecRasProject( file ) );
    mIsValid = mProject->GeometriesCount() > 0;
    if ( mIsValid )
    {
      QStringList geoms = mProject->geometryIds();
      Q_ASSERT( !geoms.isEmpty() );
      mIsValid = mProject->geometry( geoms.at( 0 ) ).area2dCount() > 0;
    }
  }
}

ReosHecRasStructureImporter::~ReosHecRasStructureImporter() = default;


ReosHydraulicStructure2D::Structure2DCapabilities ReosHecRasStructureImporter::capabilities() const
{
  return 0;
}

QString ReosHecRasStructureImporter::crs() const
{
  return QString();
}

QPolygonF ReosHecRasStructureImporter::domain() const
{
  if ( mIsValid )
  {
    return mProject->currentGeometry().area2d( 0 ).surface;
  }

  return QPolygonF();
}

ReosMeshResolutionController *ReosHecRasStructureImporter::resolutionController( ReosHydraulicStructure2D *structure ) const
{
  return new ReosMeshResolutionController( structure, crs() );
}

QStringList ReosHecRasStructureImporter::boundaryConditionsIds() const
{
  QStringList ret;
  if ( mIsValid )
  {
    const QList<ReosHecRasGeometry::BoundaryCondition> bcs = mProject->currentGeometry().allBoundariesConditions();
    for ( const ReosHecRasGeometry::BoundaryCondition &bc : bcs )
    {
      ret.append( bc.name );
    }
  }

  return ret;
}

QStringList ReosHecRasStructureImporter::boundaryConditionsNames() const
{
  return boundaryConditionsIds();
}

QList<QPointF> ReosHecRasStructureImporter::boundaryConditionMiddlePoint() const
{
  QList<QPointF> ret;
  if ( mIsValid )
  {
    const QList<ReosHecRasGeometry::BoundaryCondition> bcs = mProject->currentGeometry().allBoundariesConditions();
    for ( const ReosHecRasGeometry::BoundaryCondition &bc : bcs )
    {
      ret.append( bc.middlePosition );
    }
  }

  return ret;
}
